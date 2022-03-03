import io
import unittest

from dataclasses import dataclass

ZOA_LEN_MASK = 0x3F
ZOA_JOIN = 0x80
ZOA_ARR = 0x40
ZOA_DATA = 0x00

class Eof(Exception): pass

def isbytes(v): return isinstance(v, (bytes, bytearray))

@dataclass
class Zoa(object):
  data: bytearray
  arr: list["Zoa"]

  @classmethod
  def from_bytes(cls, value):
    if isbytes(value): return cls.new_data(value)
    out = []
    for v in value:
      out.append(Zoa.from_bytes(v))
    return cls.new_arr(out)

  def to_py(self):
    if self.data is not None: return bytes(self.data)
    if self.arr is None: raise ValueError(self)
    out = []
    for v in self.arr:
      out.append(v.to_py())
    return out

  @classmethod
  def new_arr(cls, value=None):
    return cls(data=None, arr=value if value is not None else [])

  @classmethod
  def new_data(cls, value=None):
    return cls(data=value if value is not None else bytearray(), arr=None)

  def serialize(self, bw=None):
    bw = bw if bw is not None else io.BytesIO()
    if self.data: write_data(bw, self.data)
    else:         write_arr(bw, self.arr)
    bw.seek(0)
    return bw

  def extend(self, value):
    if isbytes(value):
      if self.data is None: raise ValueError("invalid extend")
      self.data.extend(value)
    else:
      if self.arr is None: raise ValueError("invalid extend")
      self.arr.append(value)

  def get(self, value):
    if self.data is not None:
      return self.data
    return self.arr

def int_from_bytes(b: bytes):
  return int.from_bytes(b, 'big')

def write_byte(bw: io.BytesIO, v: int):
  return bw.write(v.to_bytes(1, 'big'))

def write_data(bw: io.BytesIO, data: bytes):
  if len(data) == 0:
    bw.write(b'\0') # No join bit, arr bit, or length
    return

  # write any join blocks
  i = 0
  while len(data) - i > 63:
    write_byte(bw, ZOA_JOIN | 63)
    bw.write(data[i:i+63])
    i += 63
  write_byte(bw, len(data) - i) # note: not joined
  bw.write(data[i:])

def write_arr(bw: io.BytesIO, arr: list[Zoa]):
  i = 0
  while True:
    remaining = len(arr) - i
    join = ZOA_JOIN if remaining > 63 else 0
    write_byte(bw, ZOA_ARR | join | min(63, remaining))

    j = 0
    while True:
      if i == len(arr): return
      if j >= 63: break
      v = arr[i]
      if v.data  is not None: write_data(bw, v.data)
      elif v.arr is not None: write_arr(bw, v.arr)
      else: raise ValueError(v)

      j += 1
      i += 1

def readexact(br: io.BytesIO, to: bytearray, length: int):
  while length:
    got = br.read(length)
    if not got: raise Eof()
    length -= len(got)
    to.extend(got)
    if not length: break


def from_zoab(br: io.BytesIO, joinTo:Zoa = None):
  out = None
  join = 0

  prev_ty = 1
  while True:
    meta = int_from_bytes(br.read(1))
    ty = ZOA_ARR & meta
    if join:
      if ty != prev_ty: raise ValueError("join different types")
    else:
      if ZOA_ARR & ty:  out = Zoa.new_arr()
      else:             out = Zoa.new_data()
    length = ZOA_LEN_MASK & meta

    if ty: # is arr
      for _ in range(length):
        out.arr.append(from_zoab(br))
    else:  # is data
      readexact(br, out.data, length)

    join = ZOA_JOIN & meta
    if not join:
      return out
    prev_ty = ty
