#!/usr/bin/python3

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
  LOG_SILENT    = 0x00
  LOG_USER      = 0x80
  LOG_SYS       = 0x20
  LOG_TRACE     = 0x1F
  LOG_DEBUG     = 0x0F
  LOG_INFO      = 0x07
  LOG_WARN      = 0x03
  LOG_CRIT      = 0x01

TRACE_INSTR = 0x00
TRACE_FILE = 0x01
TRACE_FILE_POS = 0x02
TRACE_ERR = 0x03
TRACE_TY = 0x04

class InstrClass(enum.Enum):
  C_OP   = 0x00
  C_SLIT = 0x40
  C_JMP =  0x80
  C_MEM =  0xC0

FIELD_ERR_CODE = 0x01
FIELD_MSG = 0x02

# https://docs.python.org/3/library/sys.html#sys.stdin
IO_STREAM = sys.stdin.buffer

def intergerBE(data):
  if len(data) == 1:
    return data[0]
  if len(data) == 2:
    return data[0] << 8 + data[1]
  if len(data) == 4:
    return data[0] << 24 + data[1] << 16 + data[2] << 8 + data[3]
  raise ValueError(data)

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
class FileEvent(Event):
  file: bytes

@dataclass
class FilePosEvent(Event):
  pos: int

@dataclass
class ErrEvent(Event):
  isCaught: bool
  err: int
  callStkDepth: int

RETZ = 0x01
RET  = 0x02
XL   = 0x84,
XW   = 0x85,
XSL  = 0x86,
XSW  = 0x87,

CALLSTACK_INSTRS = {XL, XW, XSL, XSW}

@dataclass
class OpEvent(Event):
  instr: int
  ws: WorkingStack

@dataclass
class JmpEvent(Event):
  instr: int
  loc: int
  ws: WorkingStack

  def szI(self):
    return SZ_MASK & self.instr

@dataclass
class OtherEvent(Event):
  lvl: int
  msg: str
  err: int
  payload: dict

def inputs_stream(io):
  buf = bytearray()
  while True:
    buf.clear()
    readexact(io, buf, 2)
    assert buf[0] == 0x80, f"Unknown control: {hex(buf[0])}"
    assert buf[1] == 0x00, f"Unknown stream type for logging."
    z = zoa.from_zoab(io)
    assert z.arr, "Log is not an array."
    control = z.arr[0].data  # control value

    lvl = control[0]
    if Lvl.LOG_SYS.value & lvl and Lvl.LOG_TRACE.value & lvl == Lvl.LOG_TRACE.value:
      trace = control[1]
      if trace == TRACE_FILE: yield FileEvent(z.arr[1].data)
      if trace == TRACE_FILE_POS: yield FilePosEvent(integerBE(z.arr[1].data))
      if trace == TRACE_ERR: yield ErrEvent(
        isCaught=control[2],
        err=integerBE(z.arr[1].data),
        callStkDepth=integerBE(z.arr[2].data))
      assert trace == TRACE_INSTR, f"Unknown trace: {trace}"

      instr = control[2]
      instrClass = 0xC0 & instr
      if instrClass == InstrClass.C_OP.value:
        return OpEvent(instr=instr, ws=WorkingStack.from_zoa(z.arr[1]))
      elif instrClass == InstrClass.C_SLIT.value:
        literal = integerBE(z.arr[1].data)
        assert False, "not impl"
      elif instrClass == InstrClass.C_JMP.value:
        yield JmpEvent(
          instr=instr,
          loc=integerBE(z.arr[1].data),
          ws=WorkingStack.from_zoa(z.arr[2]))
      elif instrClass == InstrClass.C_MEM.value:
        memLoc = integerBE(z.arr[1].data)
        memValue = integerBE(z.arr[2].data)
        assert False, "not impl"
      else: assert False, "Unknown instr class"

    assert not Lvl.LOG_SYS.value & lvl, "Unknown system log"
    assert Lvl.LOG_USER.value & lvl, "Not a user log"
    lvl = Lvl(LOG_TRACE.value & lvl)
    msg = None
    errCode = None
    payload = {}

    for i, item in enumerate(z.arr[1:]):
      i += 1
      assert len(item) > 1, f"index={i} has only 1 item"
      key = item[0]
      assert key.data, f"index={i} key is not data"
      if len(key.data) == 1:
        if key.data[0] == FIELD_ERR_CODE:
          errCode = integerBE(item[1].data)
        elif key.data[0] == FIELD_MSG:
          msg = integerBE(item[1].data)
        else:
          assert False, f"index={i} unknown single-length key: {key.data[0]}"
      else:
        payload[key.data] = [v.to_py() for v in item[1:]]

    yield OtherEvent(lvl=lvl, msg=msg, err=err, payload=payload)


@dataclass
class Ty:
  pass

@dataclass
class FngiEnv:
  """A Map of the Fngi env from trace logs."""
  file: str = None
  pos: int = None
  callstack: List[Event] = list
  events: Deque[Event] = collections.deque


def harness():
  env = FngiEnv()
  for event in inputs_stream(IO_STREAM):
    if isinstance(event, OpEvent):
      if event.instr == RET:
        env.callstack.pop()
      if event.instr == RETZ and len(event.ws) > 0 and event.ws[0] == 0:
        env.callstack.pop()
    elif isinstance(event, JmpEvent) and NOSZ_MASK & event.instr in CALLSTACK_INSTRS:
      env.callstack.push(event)
    elif isinstance(event, FileEvent):
      env.file = event.file
      env.pos = None
    elif isinstance(event, FilePosEvent):
      env.pos = event.pos
    elif isinstance(event, ErrEvent):
      del env.callstack[event.callStkDepth:]

    env.events.pushright(event)
    if len(env.events) > 5000:
      env.events.popleft()


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


def inputs_stream2(io):
  while True:
    waitForStart(io)
    z = zoa.from_zoab(io)
    yield z.to_py()

def harness2():
  print("Starting harness")
  for i, event in enumerate(inputs_stream2(IO_STREAM)):
    print(f"LOG[{i:>8d}]: ", end='')
    pprint.pprint(event)

if __name__ == '__main__':
  try:
    harness2()
  except zoa.Eof:
    print("Program terminated")
