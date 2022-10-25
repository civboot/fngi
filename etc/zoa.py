"""
Zoa single file library (https://github.com/civboot/zoa)

Zoa is part of the civboot.org project and is released to the public domain
or licensed MIT under your discression. Modify this file in any way you wish.
Contributions are welcome.

Version: 0.0.2
"""
import ast
import io
import unittest
import dataclasses

from collections import OrderedDict as odict
from collections.abc import Hashable
from enum import Enum
from typing import Any, Dict, List, Tuple, Iterable
from dataclasses import dataclass

################################################################################
# Utilities and Constants

ZOA_LEN_MASK = 0x3F
ZOA_JOIN = 0x80
ZOA_ARR = 0x40
ZOA_DATA = 0x00

class Eof(Exception): pass

def isbytes(v): return isinstance(v, (bytes, bytearray))

def reprData(data: bytes):
  out = bytearray()
  if len(data) % 2 != 0: out.extend(b'  '); e = 2
  else:                                     e = 0

  for i, b in enumerate(data):
    out.extend(b'%.2X' % b)
    if e == 2 and i != len(data) - 1: out.extend(b'_'); e = 1
    else:                                               e += 1
  return out.decode('utf-8')

def reprArr(arr: list):
  out = []
  for v in arr:
    out.append(repr(v))
  return '[' + ', '.join(out) + ']'

def extendWithInt(b: bytearray, i: int):
  if i == 0: b.append(0); return
  out = []
  while i != 0:
    out.append(i % 0x100)
    i = i >> 8
  out.reverse()
  b.extend(out)

def asciiInt(a: int) -> int:
  if ord('0') <= a <= ord('9'): return a - ord('0')
  if ord('a') <= a <= ord('f'): return a - ord('a')
  if ord('A') <= a <= ord('F'): return a - ord('F')
  return None

################################################################################
# ZoaRaw: representing raw zoa (Data and Arr)

@dataclass
class ZoaRaw:
  data: bytearray
  arr: list["ZoaRaw"]

  @classmethod
  def frPy(cls, value):
    if isbytes(value): return cls.new_data(value)
    out = []
    for v in value:
      out.append(ZoaRaw.frPy(v))
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

  def __repr__(self):
    if self.data is not None: return reprData(self.data)
    else:                     return reprArr(self.arr)

################################################################################
# ZoaRaw Parser

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

def write_arr(bw: io.BytesIO, arr: list[ZoaRaw]):
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

def from_zoab(br: io.BytesIO, joinTo:ZoaRaw = None):
  out = None
  join = 0

  prev_ty = 1
  while True:
    meta = int_from_bytes(br.read(1))
    ty = ZOA_ARR & meta
    if join:
      if ty != prev_ty: raise ValueError("join different types")
    else:
      if ZOA_ARR & ty:  out = ZoaRaw.new_arr()
      else:             out = ZoaRaw.new_data()
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

################################################################################
# Native Types (zty)

@dataclass
class Undefined:
  """Undefined type."""
  name: str

def updateUndefined(prevTy, newTyName, newTy):
  if not isinstance(prevTy, Undefined): return prevTy
  if newTy == prevTy:                    return prevTy
  if newTyName == prevTy.name:           return newTy
  else:                                  return prevTy

def intBytesLen(v: int) -> int:
  if v <= 0xFF:       return 1
  if v <= 0xFFFF:     return 2
  if v <= 0xFFFFFF:   return 3
  if v <= 0xFFFFFFFF: return 4
  raise ValueError(f"Int too large: {v}")

class Int(int):
  name = 'Int'

  @classmethod
  def frPy(cls, *args, **kwargs): return cls(*args, **kwargs)

  @classmethod
  def frZ(cls, raw: ZoaRaw) -> int:
    if raw.arr:
      assert 1 == len(raw.arr)
      return -Int.from_bytes(raw.arr[0].data, byteorder='big')
    return Int.from_bytes(raw.data, byteorder='big')

  def toZ(self) -> ZoaRaw:
    length = intBytesLen(abs(self))
    v = abs(self).to_bytes(length, byteorder='big')
    z = ZoaRaw.new_data(v)
    if self >= 0: return z
    return ZoaRaw.new_arr([z])

  def toPy(self) -> 'Int': return self

  @classmethod
  def parse(cls, p: "Parser") -> "Int":
    bracket = p.peek() == b'{'
    if bracket: p.need('{')
    t = p.singleData()
    i = ast.literal_eval(t.decode('utf-8'))
    if not isinstance(i, int): p.error("Not an int: " + t)
    if bracket: p.need('}')
    return cls(i)

class Data(bytes):
  name = 'Data'

  @classmethod
  def frPy(cls, *args, **kwargs): return cls(*args, **kwargs)
  @classmethod
  def frZ(cls, raw: ZoaRaw) -> "Data": return cls(raw.data)
  def toZ(self) -> ZoaRaw: return ZoaRaw.new_data(self)
  def toPy(self) -> 'Data': return self
  def __repr__(self): return reprData(self)

  @classmethod
  def parse(cls, p: "Parser") -> "Data":
    """Data must be an integer or any hexadecimal numbers inside {...}"""
    t = p.token()
    if not t: p.error("Unexpected EOF")
    p.i -= len(t)
    if p.buf[p.i] == ord('|'):
      return cls(Str.parse(p).encode('utf-8'))

    out = bytearray()
    if t != b'{':
      extendWithInt(out, Int.parse(p))
      return cls(out)
    p.need('{')
    while True:
      p.skipWhitespace()
      if p.i >= len(p.buf):      p.error("Unexpected EOF waiting for '}'")
      if p.buf[p.i] == ord('}'): break
      if p.i + 1 >= len(p.buf):  p.error("Expecting two characters")
      a = asciiInt(p.buf[p.i]); b = asciiInt(p.buf[p.i + 1])
      if a is None or b is None:
        p.error(f"Expecting two hex numbers: {p.buf[p.i:p.i+2]}")
      out.append((a << 4) + b);
      p.i += 2
    p.need('}')
    return cls(out)


STR_ESC_LIT = {ord(c) for c in ('\\', '|', ' ')}

class Str(str):
  name = 'Str'

  @classmethod
  def frPy(cls, *args, **kwargs): return cls(*args, **kwargs)
  @classmethod
  def frZ(cls, raw: ZoaRaw) -> "Str": return cls(raw.data.decode('utf-8'))
  def toZ(self) -> ZoaRaw: return ZoaRaw.new_data(self.encode('utf-8'))
  def toPy(self) -> 'Str': return self

  @classmethod
  def parse(cls, p: "Parser") -> "Str":
    r"""Strings must be any characters without whitespace or inside {...}.

    There is also escape logic (i.e. `\n`, `\t`, `\ `)
    """
    t = p.token(); p.i -= len(t)
    if not t.startswith(b'|'):
      return cls(p.singleNonWhitespace().decode('utf-8'))
    out = bytearray()
    escaped = False
    p.i += 1;
    while True:
      if p.i >= len(p.buf): p.error("Unexpected EOF")
      c = p.buf[p.i]; p.i += 1
      if escaped:
        escaped = False
        if c in STR_ESC_LIT: out.append(c)
        elif c == ord('\n'): p.skipWhitespace(skipNewlines=False)
        elif c == ord('n'): out.append(ord('\n'))
        elif c == ord('t'): out.append(ord('\t'))
        else: p.error("Unrecognized escaped char: " + chr(c))
      elif c == ord('\\'): escaped = True
      elif c == ord('|'): break
      else:
        out.append(c)
        if c == ord('\n'): p.skipWhitespace(skipNewlines=False)
    return cls(out.decode('utf-8'))


################################################################################
# Container Types (zty)
# Note: the full type information and methods are created by the Parser.

class ArrBase(list):
  @classmethod
  def frPy(cls, l: Iterable[Any]): return cls([cls._ty.frPy(i) for i in l])
  @classmethod
  def frZ(cls, raw: ZoaRaw): return cls(cls._ty.frZ(z) for z in raw.arr)
  def toZ(self) -> ZoaRaw: return ZoaRaw.new_arr([v.toZ() for v in self])
  def toPy(self) -> list: return [v.toPy() for v in self]
  def __repr__(self): return reprArr(self)

  @classmethod
  def parse(cls, p: "Parser"):
    out = []
    p.need('{')
    while p.peek() != b'}':
      out.append(cls._ty.parse(p))
      p.sugar(',')
    p.need('}')
    return cls(out)

  @classmethod
  def _define(cls, name, ty):
    cls._ty = updateUndefined(cls._ty, name, ty)

class MapBase(odict):
  @classmethod
  def frPy(cls, l: Iterable[Any]):
    if isinstance(l, dict): l = l.items()
    return cls(odict((cls._kty.frPy(k), cls._vty.frPy(v)) for k, v in l))

  @classmethod
  def frZ(cls, raw: ZoaRaw):
    if len(raw.arr) % 2 != 0: raise ValueError(f"length not even: {raw}")
    arr = iter(raw.arr)
    out = odict()
    while True:
      try:
        key = cls._kty.frZ(next(arr))
        out[key] = cls._vty.frZ(next(arr))
      except StopIteration: break
    return cls(out)

  def toZ(self) -> ZoaRaw:
    def flatten():
      for key, value in self.items():
        yield key.toZ(); yield value.toZ()
    return ZoaRaw.new_arr(list(flatten()))

  def toPy(self) -> odict: return odict((k.toPy(), v.toPy()) for k, v in self.items())
  def __repr__(self): return repr(self.toPy())

  @classmethod
  def parse(cls, p: "Parser"):
    out = odict()
    p.need('{')
    while p.peek() != b'}':
      k      = cls._kty.parse(p);
      p.need('=')
      out[k] = cls._vty.parse(p); p.sugar(',')
    p.need('}')
    return cls(out)

  @classmethod
  def _define(cls, name, ty):
    cls._vty = updateUndefined(cls._vty, name, ty)
    cls._kty = updateUndefined(cls._kty, name, ty)

@dataclass
class StructField:
  ty: Any
  zid: int = None
  default: Any = None

  def _define(self, name, ty):
    self.ty = updateUndefined(self.ty, name, ty)

@dataclass(init=False)
class StructBase:
  @classmethod
  def frZ(cls, z: ZoaRaw):
    args = []
    posArgs = Int.frZ(z.arr[0]) # number of positional args
    fields = iter(cls._fields.items())
    for pos in range(posArgs):
      _name, f = next(fields)
      assert f.zid is None
      args.append(f.ty.frZ(z.arr[1 + pos]))
    kwargs = {}
    byId = {f.zid: (name, f.ty) for name, f in cls._fields.items()}
    for z in z.arr[1+posArgs:]:
      name, ty = byId[Int.frZ(zi[0])]
      kwargs[name] = ty.frZ(zi[1])
    return cls(*args, **kwargs)

  def toZ(self) -> ZoaRaw:
    # find how many positional args exist
    posArgs = 0; posArgsDone = False
    for name, f in self._fields.items():
      if f.zid is None: # positional arg
        if getattr(self, name.decode('utf-8')) is None: posArgsDone = True
        elif posArgsDone: raise ValueError(
          f"{name} has value after previous positional arg wasn't specified")
        else: posArgs += 1

    out = [Int(posArgs).toZ()] # starts with number of positional arguments
    for name, f in self._fields.items():
      if f.zid is None: out.append(getattr(self, name.decode('utf-8')).toZ())
      else: out.append(ZoaRaw.new_arr([f.zid, self.get(name).toZ()]))
    return ZoaRaw.new_arr(out)

  def toPy(self) -> dict:
    out = {}
    for name, f in self._fields.items():
      name = name.decode('utf-8')
      out[name] = getattr(self, name).toPy()
    return out

  @classmethod
  def parse(cls, p: "Parser"):
    kwargs = {}
    p.need('{')
    while p.peek() != b'}':
      name = Str.parse(p); p.need('=')
      if name in kwargs: p.error(f"Specified {name} twice")
      field = cls._fields[name.encode('utf-8')]
      kwargs[name] = field.ty.parse(p); p.sugar(',')
    p.need('}')
    return cls(**kwargs)

  @classmethod
  def _define(cls, name, ty):
    for f in cls._fields.values():
      f._define(name, ty)

@dataclass
class EnumVar:
  ty: Any

  def _define(self, name, ty):
    self.ty = updateUndefined(self.ty, name, ty)

@dataclass(init=False)
class EnumBase:
  @classmethod
  def frZ(cls, z: ZoaRaw) -> 'EnumBase':
    variant = Int.frZ(z.arr[0])
    name, var = cls._variants[variant]
    return cls(**{name.decode('utf-8'): var.ty.frZ(z.arr[1])})

  def toZ(self) -> ZoaRaw:
    variant, value = None, None
    for i, (n, v) in enumerate(self._variants):
      ty = v.ty
      v = getattr(self, n.decode('utf-8'))
      if v:
        if variant is not None: raise ValueError(
          f"Multiple variants set: {self._variants[variant]} and {(n, ty)}")
        variant, value = i, v
    if variant is None: raise ValueError("No variant set")
    return ZoaRaw.new_arr([Int(variant).toZ(), value.toZ()])

  def toPy(self) -> Enum: return self

  @classmethod
  def parse(cls, p: "Parser"):
    nameB = p.token(); name = nameB.decode('utf-8')
    for n, variant in cls._variants:
      if n == nameB: break
    else: p.error(f"Could not find variant: {name}")
    kwargs = {}; kwargs[name] = variant.ty.parse(p)
    return cls(**kwargs)

  @classmethod
  def _define(cls, name, ty):
    for _n, v in cls._variants:
      v._define(name, ty)

@dataclass
class BmVar: # Bitmap Variant
  var: int  # the value for this variant, i.e. 0b10
  msk: int  # the mask for this variant,  i.e. 0b11

  def _getVariantClosure(varSelf):
    def closure(bitmapSelf):
      return varSelf.msk & bitmapSelf.value
    return closure

  def _setVariantClosure(varSelf):
    def closure(bitmapSelf, var=None):
      if var is None: var = varSelf.var
      if var != 0 and var != varSelf.msk & var:
        raise ValueError(
          f'Attempt to set invalid. var={hex(var)} msk={hex(varSelf.msk)}')
      bitmapSelf.value = ((~varSelf.msk) & bitmapSelf.value) | var
    return closure

  def _togVariantClosure(varSelf):
    def closure(bitmapSelf):
      if not varSelf.var: raise TypeError("Toggle not allowed on value=0")
      v = varSelf.msk & bitmapSelf.value
      if v == varSelf.var: bitmapSelf.value &= ~varSelf.msk  # clear
      elif v == 0:         bitmapSelf.value |= varSelf.var   # set
      else: raise ValueError(
        f"Attempted toggle on multi-var mask at a different var: {v}")
    return closure

  def _isVariantClosure(varSelf):
    def closure(bitmapSelf):
      return varSelf.msk & bitmapSelf.value == varSelf.var
    return closure

@dataclass
class BitmapBase:
  value: int = 0

  @classmethod
  def frZ(cls, z: ZoaRaw) -> 'BitmapBase': return cls(int(Int.frZ(z)))
  def toZ(self) -> ZoaRaw: return Int(self.value).toZ()
  def toPy(self) -> 'BitmapBase': return self

################################################################################
# Dyn

class DynType(Enum):
  Empty = 0
  Str   = 1
  Data  = 2
  Int   = 3
  Num   = 4
  Path  = 5

  ArrDyn   = 0x20
  ArrStr   = 0x21
  ArrData  = 0x22
  ArrInt   = 0x23

  MapStr    = 0x41
  MapData   = 0x42
  MapStrStr = 0x43

dynFrZMethod = {}  # note: updated later

@dataclass
class Dyn:
  value: Any
  ty: DynType
  name = "Dyn"

  @classmethod
  def _none(cls): return cls(value=None, ty=DynType.Empty)
  @classmethod
  def _str(cls, v): return cls(value=v, ty=DynType.Str)
  @classmethod
  def _data(cls, v): return cls(value=v, ty=DynType.Data)
  @classmethod
  def _int(cls, v): return cls(value=v, ty=DynType.Int)
  @classmethod
  def _arrInt(cls, v): return cls(value=v, ty=DynType.ArrInt)
  @classmethod
  def _arrData(cls, v): return cls(value=v, ty=DynType.ArrData)
  @classmethod
  def _arrDyn(cls, v): return cls(value=v, ty=DynType.ArrDyn)

  @classmethod
  def frPy(cls, arg):
    if(isinstance(arg, cls)): return arg
    if(isinstance(arg, str)): return cls._str(Str(arg))
    if(isinstance(arg, bytes)): return cls._data(Data(arg))
    if(isinstance(arg, int)): return cls._int(Int(arg))
    raise TypeError(arg)

  @classmethod
  def frPyArrInt(cls, arr): return cls._arrInt(ArrInt.frPy(arr))
  @classmethod
  def frPyArrData(cls, arr): return cls._arrData(ArrData.frPy(arr))
  @classmethod
  def frPyArrDyn(cls, arr): return _frPyArrDyn(cls, arr)

  @classmethod
  def frZ(cls, raw: ZoaRaw) -> 'Dyn':
    if not len(raw.arr): return cls._none()
    if len(raw.arr) != 2: raise TypeError(raw)
    ty = DynType(Int.frZ(raw.arr[0]))
    return cls(value=dynFrZMethod[ty](raw.arr[1]), ty=ty)

  def toZ(self) -> ZoaRaw:
    return ZoaRaw.new_arr([
      Int(self.ty.value).toZ(),
      self.value.toZ(),
    ])

  def toPy(self) -> Any: return self.value.toPy()

  def __repr__(self):    return repr(self.value)

# Regster final dyn conversion
ArrDyn   = type('ArrDyn', (ArrBase,),  {'_ty': Dyn,  'name': 'ArrDyn'})
dynFrZMethod[DynType.ArrDyn] = ArrDyn.frZ

ArrStr  = type('ArrStr', (ArrBase,),  {'_ty': Str,  'name': 'ArrStr'})
ArrData = type('ArrData', (ArrBase,), {'_ty': Data, 'name': 'ArrData'})
ArrInt  = type('ArrInt', (ArrBase,),  {'_ty': Int,  'name': 'ArrInt'})

MapStrDyn  = type('MapStrDyn', (MapBase,),  {'_vty': Str, '_kty': Dyn, 'name': 'MapStrDyn'})
MapDataDyn = type('MapDataDyn', (MapBase,), {'_vty': Data,'_kty': Dyn, 'name': 'MapDataDyn'})
MapStrStr  = type('MapStrStr', (MapBase,),  {'_vty': Str, '_kty': Str, 'name': 'MapStrStr'})
dynFrZMethod.update({
  DynType.Str: Str.frZ,
  DynType.Data: Data.frZ,
  DynType.Int: Int.frZ,
  DynType.ArrStr: ArrStr.frZ,
  DynType.ArrData: ArrData.frZ,
  DynType.ArrInt: ArrInt.frZ,
  DynType.MapStr: MapStrDyn.frZ,
  DynType.MapData: MapDataDyn.frZ,
  DynType.MapStrStr: MapStrStr.frZ,
})

def _frPyArrDyn(cls, arr): return cls._arrDyn(ArrDyn.frPy(arr))

################################################################################
# Env: this contains all native and user-defined types found during parsing.

def modname(mod, name): return mod + '.' + name if mod else name

class TyEnv:
  def __init__(self):
    self.tys = {
      b'Str': Str,
      b'Data': Data,
      b'Int': Int,
      b'Dyn': Dyn,
      b'ArrDyn': ArrDyn,
      b'ArrStr': ArrStr,
      b'ArrData': ArrData,
      b'ArrInt': ArrInt,
      b'MapStrDyn': MapStrDyn,
      b'MapDataDyn': MapDataDyn,
      b'MapStrStr': MapStrStr,
    }
    self.vals = {}

  def arr(self, ty: Any) -> ArrBase:
    """Create or get generic array type."""
    name = f'Arr[{ty.name}]'
    existing = self.tys.get(name)
    if existing: return existing
    arrTy = type(name, (ArrBase,), {'_ty': ty, 'name': name})
    self.tys[name] = arrTy
    return arrTy

  def map(self, kty: Any, vty: Any) -> MapBase:
    """Create or get generic map type."""
    name = f'Map[{kty.name},{vty.name}]'
    existing = self.tys.get(name)
    if existing: return existing
    mapTy = type(name, (MapBase,), {'_kty': kty, '_vty': vty, 'name': name})
    self.tys[name] = mapTy
    return mapTy

  def undefined(self, name):
    existing = self.tys.get(name)
    if existing: raise ValueError(f"Declaring already defined type: {name}")
    if existing: return existing
    ty = Undefined(name)
    self.tys[name] = ty
    return ty

  def struct(self, mod: bytes, name: bytes, fields: Dict[bytes, StructField]):
    mn = modname(mod, name)
    undefined = self.tys.get(mn)
    if isinstance(undefined, Undefined): pass
    elif mn in self.tys: raise KeyError(f"Modname {mn} already exists")
    dfields = []
    for n, f in fields.items():
      n = n.decode('utf-8')
      if f.default is None:
        item = [n, f.ty]
      else:
        item = [n, f.ty, dataclasses.field(default_factory=lambda: f.default)]
      dfields.append(item)

    ty = dataclasses.make_dataclass(
      name.decode('utf-8'),
      dfields,
      bases=(StructBase,),
    )
    ty.name = mn
    ty._fields = fields
    return self._register(mn, ty, undefined)

  def enum(self, mod: bytes, name: bytes, variants: List[Tuple[bytes, Any]]):
    mn = modname(mod, name)
    undefined = self.tys.get(mn)
    if isinstance(undefined, Undefined): pass
    elif mn in self.tys: raise KeyError(f"Modname {mn} already exists")
    ty = dataclasses.make_dataclass(
      name.decode('utf-8'),
      [
        (n.decode('utf-8'), ty, dataclasses.field(default=None))
        for (n, ty) in variants
      ],
      bases=(EnumBase,),
    )
    ty.name = mn
    ty._variants = variants
    return self._register(mn, ty, undefined)

  def bitmap(self, mod: bytes, name: bytes, variants: List[Tuple[bytes, BmVar]]):
    mn = modname(mod, name)
    if mn in self.tys: raise KeyError(f"Modname {mn} already exists")
    methods = {'name': mn, '_variants': variants}
    for n, var in variants:
      n = n.decode('utf-8')
      methods['get_' + n] = var._getVariantClosure()
      methods['set_' + n] = var._setVariantClosure()
      methods['is_' + n] = var._isVariantClosure()
      methods['tog_' + n] = var._togVariantClosure()
    ty = type(name.decode('utf-8'), (BitmapBase,), methods)
    self.tys[mn] = ty
    return ty

  def _register(self, name, ty, undefined):
    self.tys[name] = ty
    if undefined: self._define(name, ty)
    return ty

  def _define(self, name, ty):
    for v in self.tys.values():
      if hasattr(v, '_define'):
        v._define(name, ty)

################################################################################
# Parser: parses tokens to create types

SINGLES = {ord(c) for c in ['%', '\\', '$', '|', '(', ')', '[', ']']}

class TG(Enum): # Token Group
  T_NUM = 0
  T_HEX = 1
  T_ALPHA = 2
  T_SINGLE = 3
  T_SYMBOL = 4
  T_WHITE = 5

  @classmethod
  def fromChr(cls, c: int):
    if(c <= ord(' ')):                  return cls.T_WHITE;
    if(ord('0') <= c and c <= ord('9')): return cls.T_NUM;
    if(ord('_') == c)                  : return cls.T_NUM;
    if(ord('a') <= c and c <= ord('f')): return cls.T_HEX;
    if(ord('A') <= c and c <= ord('F')): return cls.T_HEX;
    if(ord('g') <= c and c <= ord('z')): return cls.T_ALPHA;
    if(ord('G') <= c and c <= ord('Z')): return cls.T_ALPHA;
    if(ord('.') == c)                  : return cls.T_ALPHA;
    if(c in SINGLES)                   : return cls.T_SINGLE;
    return cls.T_SYMBOL

def coaleseTG(group: TG) -> TG:
  """Convert all alphanumerics to T_ALPHA."""
  if group.value <= TG.T_ALPHA.value:
    return TG.T_ALPHA
  return group

class ParseError(RuntimeError):
  def __init__(self, line, msg): return super().__init__(f'line {line}: {msg}')

@dataclass
class Parser:
  buf: bytearray
  mod: bytes = None
  i: int = 0
  line: int = 1
  env: TyEnv = dataclasses.field(default_factory=TyEnv)

  def error(self, msg): raise ParseError(self.line, msg)

  def skipWhitespace(self, skipNewlines=True):
    while self.i < len(self.buf) and TG.fromChr(self.buf[self.i]) is TG.T_WHITE:
      if skipNewlines and self.buf[self.i] == ord('\n'):
        self.line += 1
      self.i += 1

  def _token(self) -> bytes:
    self.skipWhitespace()
    if self.i == len(self.buf): return
    starti = self.i
    group = coaleseTG(TG.fromChr(self.buf[self.i]))
    self.i += 1
    if group is TG.T_SINGLE:
      return self.buf[starti:self.i]
    while self.i < len(self.buf):
      if coaleseTG(TG.fromChr(self.buf[self.i])) is not group:
        return self.buf[starti:self.i]
      self.i += 1
    return self.buf[starti: self.i]

  def token(self, allowEof=False):
    if not allowEof and self.i >= len(self.buf): self.error("Unexpected EOF")
    while self.i < len(self.buf):
      t = self._token()
      if t == b'\\': self.parseComment()
      else: return t

  def peek(self, allowEof=True) -> bytes:
    starti = self.i
    out = self.token(allowEof=allowEof)
    self.i = starti
    return out

  def need(self, s):
    if self.token() != s.encode('utf-8'): self.error(f"Expected |{s}|")

  def singleData(self):
    t = self.token()
    if t != '{': return t
    t = self.token()
    self.need('}')
    return t

  def singleNonWhitespace(self) -> bytearray:
    self.skipWhitespace()
    if self.i == len(self.buf): self.error("Unexpected EOF")
    out = bytearray()
    while self.i < len(self.buf) and self.buf[self.i] > ord(' '):
      out.append(self.buf[self.i])
      self.i += 1
    return out

  def sugar(self, s):
    if self.peek() == s.encode('utf-8'): self.token() # consume token

  def _blockComment(self):
    while self.i < len(self.buf):
      if self.buf[self.i:self.i+1] == b'\\(':
        self.i += 2
        self._blockComment()
      if self.buf[self.i] == ord(')'):
        return
      self.i += 1

  def parseComment(self):
    if self.buf[self.i] == ord('('): # block comment
      self.i += 1
      self._blockComment()
    elif self.buf[self.i] == ord(' '): # comment till EOL
      while self.i < len(self.buf) and self.buf[self.i] != ord('\n'):
        self.i += 1
    else: # ignore token
      self._token()

  def parseArr(self) -> ArrBase:
    self.need('['); ty = self.parseTy(); self.need(']')
    return self.env.arr(ty)

  def parseMap(self) -> MapBase:
    self.need('['); kty = self.parseTy();
    self.need(','); vty = self.parseTy();
    self.need(']')
    if not isinstance(kty, Hashable): raise TypeError(f'Key {kty.name} is not hashable')
    return self.env.map(kty, vty)

  def parseTy(self) -> Any:
    name = self.token()
    if name == b'Arr': return self.parseArr()
    if name == b'Map': return self.parseMap()
    return self.env.tys[name]

  def parseField(self) -> StructField:
    name = self.token(); self.need(':')
    # TODO: handle zid case
    ty = self.parseTy()
    if self.peek() == b'=':
      self.need('='); default = ty.parse(self)
    else: default = None
    return (name, StructField(ty=ty, default=default))

  def _parseStruct(self) -> (str, List[StructField]):
    name = self.token()
    fields = odict()
    self.need('[')
    while True:
      p = self.peek()
      if p == b']':
        self.need(']')
        break
      k, v = self.parseField()
      if k in fields: raise ValueError(f'field {k} listed twice')
      fields[k] = v
      self.sugar(';')
    return name, fields

  def parseInt(self) -> int:
    t = self.token()
    if t.startswith(b'0b'): return int(t[2:], 2)
    if t.startswith(b'0x'): return int(t[2:], 16)
    return int(t, 10)

  def parseDeclare(self) -> Undefined:
    return self.env.undefined(self.token())

  def parseStruct(self) -> StructBase:
    name, fields = self._parseStruct()
    return self.env.struct(self.mod, name, fields)

  def parseEnum(self) -> EnumBase:
    name, fields = self._parseStruct()
    # TODO: handle zid
    return self.env.enum(self.mod, name, [(n, EnumVar(f.ty)) for (n, f) in fields.items()])

  def parseBitmap(self) -> BitmapBase:
    name = self.token()
    variants = []
    self.need('[')
    while True:
      p = self.peek()
      if p == b']':
        self.need(']')
        break
      vname = self.token()
      var = self.parseInt()
      i = self.i
      # If next token is int, it is the msk
      try: msk = self.parseInt()
      except ValueError: # else, make it like a peek
        self.i = i
        msk = var
      self.sugar(';')
      variants.append((vname, BmVar(var, msk)))
    return self.env.bitmap(self.mod, name, variants)

  def parseConst(self):
    name = self.token(); self.need(':');
    if name in self.env.vals: self.error(f"const {name} already defined")
    tyName = self.token(); self.need('=')
    self.env.vals[name] = self.env.tys[tyName].parse(self)

  def parse(self):
    while self.i < len(self.buf):
      token = self.token()
      if not token: break
      elif token == b'declare': self.parseDeclare()
      elif token == b'struct':  self.parseStruct()
      elif token == b'enum':    self.parseEnum()
      elif token == b'bitmap':  self.parseBitmap()
      elif token == b'const':   self.parseConst()
