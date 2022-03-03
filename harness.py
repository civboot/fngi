#!/usr/bin/python3
"""
For why this exists see harness.md
"""


import re
import os
import collections
import sys
import enum
import pprint
from typing import Dict, Tuple, List, Deque
from dataclasses import dataclass

import zoa
from zoa import readexact

ZOAB_LOG  = 0x03

SZ_MASK   = 0x30
NOSZ_MASK = 0xCF

def wantStr(b):
  try: return b.decode('utf-8')
  except ValueError: return b

def nice(value):
  if isinstance(value, bytes):
    return wantStr(value)
  if isinstance(value, int):
    return f"0x{value:X}"


# Fngi log level byte
class Lvl(enum.Enum):
  SILENT    = 0x00
  SYS       = 0x80
  USER      = 0x20
  TRACE     = 0x1F
  DEBUG     = 0x0F
  INFO      = 0x07
  WARN      = 0x03
  CRIT      = 0x01

LOG_DICT = 0x01
LOG_ERR  = 0x02
LOG_FILE = 0x03

class InstrClass(enum.Enum):
  C_OP   = 0x00
  C_SLIT = 0x40
  C_JMP =  0x80
  C_MEM =  0xC0

FIELD_ERR_CODE = 0x01
FIELD_MSG = 0x02

# https://docs.python.org/3/library/sys.html#sys.stdin
IO_STREAM = sys.stdin.buffer

def integerBE(data):
  if len(data) == 1:
    return data[0]
  if len(data) == 2:
    return (data[0] << 8) + data[1]
  if len(data) == 4:
    return (data[0] << 24) + (data[1] << 16) + (data[2] << 8) + data[3]
  raise ValueError(data)

def integerLE(data, l=None):
  if l is None: l = len(data)
  if l == 1:
    return data[0]
  if l == 2:
    return data[0] + (data[1] << 8)
  if l == 4:
    return data[0] + (data[1] << 8) + (data[2] << 16) + (data[3] << 24)
  raise ValueError(data)


@dataclass
class FngiEnv:
  """A Map of the Fngi env from trace logs."""
  dicts: dict
  codes: dict
  file: str = None
  pos: int = None

@dataclass
class WorkingStack:
  size: int
  data: List[int] # sometimes only partial

  @classmethod
  def from_zoa(cls, zoaArr):
    size = integerBE(zoaArr[0].data)
    data = [integerBE(v.data) for v in zoaArr[1:]]
    assert size >= len(data)
    return cls(size=size, data=data)

class Event:
  pass

@dataclass
class BasicEvent(Event):
  lvl: Lvl
  payload: List

@dataclass
class DictEvent(Event):
  isConst: bool
  key: bytes
  value: int
  buf: int
  offset: int
  heap: int
  isConstant: bool
  isLocal: bool

  @classmethod
  def from_(cls, z):
    return cls(
      isConst=bool(z[0][1]), # after lvl
      key=z[1],
      value=integerBE(z[2]),
      buf=integerBE(z[3]),
      offset=integerBE(z[4]),
      heap=integerBE(z[5]),
      isConstant=bool(integerBE(z[6])),
      isLocal=bool(integerBE(z[7])))

  def meta(self):
    return self.value >> 24

  def ref(self):
    return 0xFFFFFF & self.value

  def __repr__(self):
    if self.isConstant:
      return f"DictConstant[{nice(self.value)}]"
    return f"DictFn[{nice(self.meta())} @{nice(self.ref())}]"


ERR_DATA_NONE  = 0x00
ERR_DATA_INT1  = 0x01
ERR_DATA_DATA1 = 0x02
ERR_DATA_INT2  = 0x03
ERR_DATA_DATA2 = 0x04

def errData(z):
  print(z)
  if not z: return z
  ty = ord(z[0])
  if ty == ERR_DATA_INT1: return (integerBE(z[1]),)
  if ty == ERR_DATA_INT2: return (integerBE(z[1]), integerBE(z[2]))
  return z[1:]


@dataclass
class ErrEvent(Event):
  errCode: int
  errName: str
  isCaught: bool
  ep: int
  lineNo: int
  data: list
  callStkLen: int
  callStk: bytes
  localsStk: bytes


  @classmethod
  def from_(cls, z, env):
    code = integerBE(z[1])
    isCaught = bool(integerBE(z[2]))
    callStk, localsStk = None, None
    if isCaught: callStkLen = integerBE(z[6])
    else:
      callStk, localsStk = z[6]
      callStkLen = len(callStk) // 4
    return cls(
      errCode=code,
      errName=env.codes.get(code, "Unknown Err"),
      isCaught=isCaught,
      ep=integerBE(z[3]),
      lineNo=integerBE(z[4]),
      data=errData(z[5]),
      callStkLen=callStkLen,
      callStk=callStk,
      localsStk=localsStk,
    )

@dataclass
class FileEvent(Event):
  file: str

  @classmethod
  def from_(cls, z):
    return cls(file=z[1])

RETZ = 0x01
RET  = 0x02
XL   = 0x84,
XW   = 0x85,
XSL  = 0x86,
XSW  = 0x87,

CALLSTACK_INSTRS = {XL, XW, XSL, XSW}

def waitForStart(io):
  buf = bytearray()
  while True:
    buf.clear()
    readexact(io, buf, 1)
    if buf[0] == 0x80:
      buf.clear(); readexact(io, buf, 1)
      if buf[0] == ZOAB_LOG:
        return
      print(f"??? Unknown byte after 0x80: {nice(buf[0])}")
    else:
      c = 0
      try: chr(buf[0])
      except ValueError: pass
      print(f"??? Unknown byte: {nice(buf[0])} {c}")

SP_REGEX = re.compile(
  r'^#(?P<value>[\w_]+)\s+=(?P<name>E_.+)$',
  re.MULTILINE)

def findErrorCodes():
  codes = {}
  for root, _, files in os.walk(".", topdown=False):
    for fname in files:
      if fname.endswith('.sp'): regex = SP_REGEX
      elif fname.endswith('.fn'): continue # TODO: not implemented yet
      else: continue

      with open(os.path.join(root, fname), 'r') as f:
        text = f.read()

      for m in re.finditer(regex, text):

        codes[int(m.group('value'), 16)] = m.group('name')

  return codes

def sysEvent(env, z):
  control = z[0]
  sysTy = Lvl.TRACE.value & control[0]
  if sysTy == LOG_DICT:
    return DictEvent.from_(z)
  if sysTy == LOG_ERR:
    return ErrEvent.from_(z, env)
  if sysTy == LOG_FILE:
    return FileEvent.from_(z)

def inputs_stream(env, io):
  while True:
    waitForStart(io)
    z = zoa.from_zoab(io).to_py()
    control = z[0]
    if Lvl.SYS.value & control[0]:
      yield sysEvent(env, z)
      continue

    lvl = Lvl(control[0])
    payload = list(z[1:])

    yield BasicEvent(lvl=lvl, payload=payload)


@dataclass
class CallStkItem:
  ep: int
  fn: DictEvent
  fnNext: DictEvent
  localData: bytes

def orderFns(dicts):
  fns = []
  for d in dicts.values():
    for f in d.values():
      if f.isLocal or f.isConstant: continue
      fns.append(f)

  fns.sort(key=DictEvent.ref)
  return fns

def findFnLesser(fns, addr):
  # A few notes:
  # - The ep that gets put on the call stack is the one where execution
  #   will CONTINUE.
  # - Therefore if the addr=fn.ref() then the previous function called
  #   something and bleeds into the next function on return
  #   (this may be intentional fall-through or they expect failure).
  if addr == 0: return None
  if fns[0].ref() > addr: return None
  if fns[-1].ref() < addr: return len(fns) - 1
  i = 0
  while i < len(fns):
    if fns[i].ref() >= addr:
      return i - 1
    i += 1

def getFnAndNext(fns, fnI):
  if fnI is None: return None, None
  fn, fnNext = fns[fnI], None
  if fnI < len(fns) - 1: fnNext = fns[fnI + 1]
  return fn, fnNext

def extractCallStack(fns, ev):
  out = []
  cs_i = 0
  ls_i = 0
  while cs_i < len(ev.callStk):
    lszRef = integerLE(ev.callStk[cs_i:cs_i+4])
    lszBytes = (lszRef >> 24) * 4
    ep = 0xFFFFFF & lszRef
    lsData = ev.localsStk[ls_i:ls_i+lszBytes]
    fnI = findFnLesser(fns, ep)
    fn, fnNext = getFnAndNext(fns, fnI)
    out.append(CallStkItem(ep=ep, fn=fn, fnNext=fnNext, localData=lsData))

    cs_i += 4
    ls_i += lszBytes
  return out

def printCallStack(callStk):
  for i, item in enumerate(callStk):
    ep, key, ref = nice(item.ep), "BASE", 0
    if item.fn:
      key, ref = nice(item.fn.key), nice(item.fn.ref())
    loc = "in"
    if ref:
      loc = f"{item.ep - item.fn.ref():>5} bytes"
      if item.fnNext:
        p = (item.ep - item.fn.ref()) / (item.fnNext.ref() - item.fn.ref())
        p = int(100 * p)
        loc = loc + f" ({p:>3}%) into"
      else: loc += "        into"

    if i == 0: print("    Failure      ", end="")
    else:      print("    + Called from", end="")
    print(f" @{ep:<10} {loc} {key} (defined @{ref})")

def harness(env):
  print("Starting harness")
  for i, ev in enumerate(inputs_stream(env, IO_STREAM)):
    if isinstance(ev, DictEvent):
      env.dicts[ev.buf][ev.key] = ev
      continue
    if isinstance(ev, ErrEvent):
      file = wantStr(env.file)

      if ev.isCaught: print(" -- Caught Error", end="")
      else:           print(" !! ERROR", end="")

      print(f" (file: {file} [{ev.lineNo}])", end="")
      print(f" [{nice(ev.errCode)}] \"{ev.errName}\"  ")
      if len(ev.data) == 1:
        print("  V:", nice(ev.data[0]))
      if len(ev.data) == 2:
        print("  ERROR VALUES")
        print("    A:", nice(ev.data[0]))
        print("    B:", nice(ev.data[1]))

      if   not ev.callStkLen: pass
      else:
        print(f"  Call Stack (depth={ev.callStkLen})")
        if not ev.callStk: continue
        fns = orderFns(env.dicts)
        callStk = extractCallStack(fns, ev)
        lastFnI = findFnLesser(fns, ev.ep)
        fn, fnNext = getFnAndNext(fns, lastFnI)
        lastCs = CallStkItem(ep=ev.ep, fn=fn, fnNext=fnNext, localData=b'')
        callStk.insert(0, lastCs)

        printCallStack(callStk)

      continue
    if isinstance(ev, FileEvent):
      env.file = ev.file
      if ev.file != b'RAW_STR': print(f"Compiling file: {ev.file}")
      continue

    print(f"{ev.lvl}[{i:>8d}]: ", end='')
    payload = ev.payload
    if len(payload) == 1: payload = payload[0]
    pprint.pprint(payload)


if __name__ == '__main__':
  env = FngiEnv(
    dicts = collections.defaultdict(dict),
    codes = findErrorCodes())
  try:
    harness(env)
  except zoa.Eof:
    print("Program terminated")
  # pprint.pprint(env.dicts)
  # for key, value in env.codes.items():
  #   print(f"  {nice(key)}: {value}")
