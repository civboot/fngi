#!/usr/bin/python3

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

  @classmethod
  def from_(cls, z):
    return cls(
      isConst=bool(z[0][1]), # after lvl
      key=z[1],
      value=integerBE(z[2]),
      buf=integerBE(z[3]),
      offset=integerBE(z[4]),
      heap=integerBE(z[5]))

@dataclass
class ErrEvent(Event):
  errCode: int
  errName: str
  isCaught: bool
  lineNo: int

  @classmethod
  def from_(cls, z, env):
    code = integerBE(z[1])
    return cls(
      errCode=code,
      errName=env.codes.get(code, "Unknown Err"),
      isCaught=bool(integerBE(z[2])),
      lineNo=integerBE(z[3])
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
      print(f"??? Unknown byte after 0x80: {hex(buf[0])}")
    else:
      c = 0
      try: chr(buf[0])
      except ValueError: pass
      print(f"??? Unknown byte: {hex(buf[0])} {c}")

SP_REGEX = re.compile(
  r'^#(?P<value>[\w_]+)\s+=(?P<name>E_[\w_]+)',
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

def harness(env):
  print("Starting harness")
  for i, ev in enumerate(inputs_stream(env, IO_STREAM)):
    if isinstance(ev, DictEvent):
      env.dicts[ev.buf][ev.key] = ev
      continue
    if isinstance(ev, ErrEvent):
      print(f"ERROR {ev.errName}({hex(ev.errCode)})  ", end="")
      file = env.file
      try: file = file.decode('utf-8')
      except ValueError: pass
      print(file, f"[{ev.lineNo}]")
      continue
    if isinstance(ev, FileEvent):
      env.file = ev.file
      if ev.file != b'RAW STR': print(f"File changed: {ev.file}")
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
  #   print(f"  {hex(key)}: {value}")
