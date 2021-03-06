#!/usr/bin/python3
"""
See ./harness.md for full documentation.
"""

import signal
import threading
import select
import re
import os
import collections
import sys
import enum
import pprint
import time
from typing import Dict, Tuple, List, Deque
from dataclasses import dataclass
import subprocess
import argparse

DONE = False
def sigint(signal, frame): global DONE; DONE = True
signal.signal(signal.SIGINT, sigint)

parser = argparse.ArgumentParser(description='Fngi linux harness.')
parser.add_argument('--test', action='store_true', help="Run tests")
parser.add_argument('--valgrind', action='store_true', help="Run with valgrind")
parser.add_argument('--log', default='LOG_USER', help="The log level, i.e. LOG_INFO.")
parser.add_argument('--syslog', default='LOG_COMPILER', help="The log level, i.e. LOG_INFO.")
parser.add_argument('--pdb', action='store_true', help="Enter debug on failure")

import zoa
from zoa import readexact
PWD = os.path.dirname(__file__)
with open(os.path.join(PWD, 'harness/types.ty'), 'rb') as f:
  _parser = zoa.Parser(f.read())
_parser.parse()
tys = _parser.env.tys
Lvl = tys[b'Lvl'];           Meta = tys[b'Meta']
Instr = tys[b'Instr'];
WorkingStk = tys[b'WorkingStk']
LogEvent = tys[b'LogEvent']; FileEvent = tys[b'FileEvent']; 
DictEvent = tys[b'DictEvent']
ErrEvent = tys[b'ErrEvent']; JmpEvent = tys[b'JmpEvent']
RetEvent = tys[b'RetEvent']; Event = tys[b'Event']

ERRORS = []

class CheckDoneReader:
  """A reader that can be killed."""
  def __init__(self, fd):
    self.fd = fd

  def read(self, n, orNewline=False):
    out = bytearray()
    while n:
      (rdRdy, wrRdy, xRdy) = select.select([self.fd.fileno()], [], [], 0.2)
      assert not wrRdy
      assert not xRdy
      if rdRdy:
        b = os.read(self.fd.fileno(), n)
        if not b: break  # EOF
        out.extend(b)
        n -= len(b)
      elif DONE:
        # Read from bytestream until there is nothing, then respect DONE
        break # DONE looks like EOF
      if orNewline and b'\n' in out: break
    return out

def wantStr(b):
  try: return b.decode('utf-8')
  except ValueError: return b

def nice(value):
  if isinstance(value, bytes):
    return wantStr(value)
  if isinstance(value, int):
    return hex(value)


def integerBE(data): return int.from_bytes(data, 'big')
def integerLE(data, l=None): return int.from_bytes(data, 'little')
LVL = {0x1F: 'trace', 0x17: 'debug', 0x13: 'info', 0x11: 'warn', 0x10: 'crit'}

@dataclass
class FnEntry:
  d: DictEvent
  loffsets: Dict[int, DictEvent]

@dataclass
class FngiEnv:
  """A Map of the Fngi env from trace logs."""
  dicts: dict
  fnLookup: dict
  glbls: dict
  codes: dict
  uncaughtErr: "ErrEvent" = None
  file: str = None
  pos: int = None
  currentFn: "DictEvent" = None
  instrLookup: dict = None

def isCallstk(instr: Instr):
  return instr.is_xl() or instr.is_xw() or instr.is_xsl() or instr.is_xsw()

SP_REGEX = re.compile(
  r'#(?P<value>[\w_]+)\s+#([\w_]+)=(?P<name>.+)',
  re.MULTILINE)

def findGlobals():
  glbls = {}

  for root, _, files in os.walk(".", topdown=False):
    for fname in files:
      if fname.endswith('.sp'): regex = SP_REGEX
      elif fname.endswith('.fn'): continue # TODO: not implemented yet
      else: continue

      with open(os.path.join(root, fname), 'r') as f:
        text = f.read()

      for m in re.finditer(regex, text):
        glbls[m.group('name')] = int(m.group('value'), 16)
  return glbls

def findErrorCodes(glbls):
  return {value: name for (name, value) in glbls.items()
          if name.startswith('E_')}

def orderFns(dicts):
  fns = []
  for d in dicts.values():
    for f in d.values():
      if isinstance(f, FnEntry): f = f.d
      if f.meta.is_fn(): fns.append(f)

  fns.sort(key=lambda f: f.ref)
  return fns

def findFnLesser(fns, addr):
  # A few notes:
  # - The ep that gets put on the call stack is the one where execution
  #   will CONTINUE.
  # - Therefore if the addr=fn.ref then the previous function called
  #   something and bleeds into the next function on return
  #   (this may be intentional fall-through or they expect failure).
  if addr == 0: return None
  if not fns: return None
  if fns[0].value > addr: return None
  if fns[-1].value < addr: return len(fns) - 1
  i = 0
  while i < len(fns):
    if fns[i].value >= addr:
      return i - 1
    i += 1

def getFnAndNext(fns, fnI):
  if fnI is None: return None, None
  fn, fnNext = fns[fnI], None
  if fnI < len(fns) - 1: fnNext = fns[fnI + 1]
  return fn, fnNext

@dataclass
class CallStkItem:
  ep: int
  fn: DictEvent
  fnNext: DictEvent
  localData: bytes

def extractCallStack(fns, ev):
  out = []
  i, ls_i = 0, 0
  while i < len(ev.localsStkSz):
    ep = integerLE(ev.callStk[i*4:(i*4)+4])
    lSz = ev.localsStkSz[i] * 4
    lsData = ev.localsStk[ls_i:ls_i + lSz]
    fnI = findFnLesser(fns, ep)
    fn, fnNext = getFnAndNext(fns, fnI)
    out.append(CallStkItem(ep=ep, fn=fn, fnNext=fnNext, localData=lsData))
    i += 1
    ls_i += lSz
  return out

def printCallStack(callStk):
  for i, item in enumerate(callStk):
    ep, key, ref = nice(item.ep), "BASE", 0
    if item.fn:
      key, ref = nice(item.fn.key), nice(item.fn.value)
    loc = "in"
    if ref:
      loc = f"{item.ep - item.fn.value:>5} bytes"
      if item.fnNext:
        p = (item.ep - item.fn.value) / (item.fnNext.value - item.fn.value)
        p = int(100 * p)
        loc = loc + f" ({p:>3}%) into"
      else: loc += "        into"

    if i == 0: print("    Failure      ", end="")
    else:      print("    + Called from", end="")
    print(f" @{ep:<10} {loc} {key} (defined @{ref})")

def waitForStart(io):
  buf = bytearray()
  while True:
    buf.clear()
    readexact(io, buf, 1)
    if buf[0] == zoa.ZOA_JOIN:
      buf.clear(); readexact(io, buf, 1)
      if buf[0] == 0x03: return
      print(f"??? Unknown byte after 0x80: {chr(buf[0])} ({nice(buf[0])})")
    else:
      c = 0
      try: chr(buf[0])
      except ValueError: pass
      print(f"??? Unknown byte: {chr(buf[0])} ({nice(buf[0])})")

def handleDictEvent(env, ev):
  """Dict events need to do two things:
  - if the event is a function, it updates the fnLookup and the currentFn
  - if the event is a local it updates the current function's local offsets.
  """
  value = ev # the value to add to ev.dicts
  if not ev.meta.is_const():
    if ev.isLocal and env.currentFn:
      env.currentFn.loffsets[ev.value] = ev
    elif ev.meta.is_fn():
      # We store a FnEntry instead of a raw event.
      # This allows us to track the local values as we recieve them.
      value = FnEntry(ev, {})
      env.currentFn = value
      env.fnLookup[ev.ref] = nice(ev.key)

  env.dicts[ev.ref][ev.key] = value

def handleErrEvent(env, ev):
  file = wantStr(env.file.name)
  name = env.codes.get(ev.code, "Unknown Err"),

  if ev.isCaught: print(" -- Caught Error", end="")
  else:
    print(" !! ERROR", end="")
    env.uncaughtErr = ev
  print(f" (file: {file} [{ev.lineNo}])", end="")
  print(f" [{nice(ev.code)}] \"{name}\"  ")
  if len(ev.data) == 1:
    print("  V:", nice(ev.data[0]))
  if len(ev.data) == 2:
    print("  ERROR VALUES")
    print("    A:", nice(ev.data[0]))
    print("    B:", nice(ev.data[1]))

  fns = orderFns(env.dicts)
  callStk = extractCallStack(fns, ev)
  lastFnI = findFnLesser(fns, ev.ep)
  fn, fnNext = getFnAndNext(fns, lastFnI)
  lastCs = CallStkItem(ep=ev.ep, fn=fn, fnNext=fnNext, localData=b'')
  callStk.insert(0, lastCs)
  printCallStack(callStk)


def harness(env, io):
  try:
    return _harness(env, io)
  except zoa.Eof: pass
  except Exception as e:
    ERRORS.append(e)
    raise

def inputs_stream(env, io):
  while True:
    waitForStart(io)
    z = zoa.from_zoab(io)
    if not z.arr: continue
    yield Event.frZ(z)

def _harness(env, io):
  """Harness for spor.

  Note: if using io=stdi, use stdin.buffer
    https://docs.python.org/3/library/sys.html#sys.stdin
  """

  for i, ev in enumerate(inputs_stream(env, io)):
    if ev is None: print(f"ev is none at i={i}");
    elif ev.log:
      if ev.log.lvl.is_silent(): print(nice(ev.log.msg), end="")
      else: print(f"{LVL[ev.log.lvl.value]}: {nice(ev.log.msg)}")
    elif ev.dict: handleDictEvent(env, ev.dict);
    elif ev.err: handleErrEvent(env, ev.err);
    elif ev.file: env.file = ev.file
    else: raise ValueError(ev)

def readPassLoop(inp, toOut):
  try:
    try: _readPassLoop(inp, toOut)
    except ValueError as e:
      if str(e) == 'I/O operation on closed file': pass
      else: raise
  except Exception as e:
    ERRORS.append(e)
    raise

def _readPassLoop(inp, toOut):
  """Reads from input and passes directly to output but is killable.

  Yes, this is really necessary to allow it to be killable. Yes, this complexity
  is all from python/linux/whatever.
  """
  writeBuf = bytearray()
  eof = False

  while not DONE:
    time.sleep(0.5)
    (rdRdy, wrRdy, xRdy) = select.select([inp.fileno()], [toOut.fileno()], [], 0.2)
    if rdRdy:
      b = os.read(inp.fileno(), 1024)
      print("[py]  Read:", b)
      if b: writeBuf.extend(b)
      else: eof = True

    if wrRdy and writeBuf:
      print("[py] Wrote:", writeBuf)
      written = os.write(toOut.fileno(), writeBuf)
      toOut.flush()
      writeBuf = writeBuf[written:]

    if eof and not writeBuf:
      return

def main(args):
  global DONE

  glbls = findGlobals()
  env = FngiEnv(
    dicts = collections.defaultdict(dict),
    fnLookup = {},
    glbls = glbls,
    codes = findErrorCodes(glbls),
    instrLookup = {
      value: key.split()[0] for key, value in glbls.items()
      if key.split()[0] in {
          'JMPL', 'JMPW', 'JZL', 'JTBL', 'XL', 'XW', 'XSL', 'XSW',
          'RET', 'RETZ'}
    },
  )

  usrLogLvl = '00'
  if args.log != 'LOG_SILENT':
    usrLogLvl = env.glbls[args.log]
    assert usrLogLvl <= 0xFF
    usrLogLvl = '{:02X}'.format(usrLogLvl)
  sysLogLvl = '00'
  if args.syslog != 'LOG_SILENT':
    sysLogLvl = env.glbls[args.syslog]
    assert sysLogLvl <= 0xFF
    sysLogLvl = '{:02X}'.format(sysLogLvl)

  cargs = [
    "./spor",
    "1" if args.test else "0",
    sysLogLvl,
    usrLogLvl,
  ]

  if args.valgrind: cargs.insert(0, 'valgrind')

  print("Running args:", cargs)
  p = subprocess.Popen(
      args=cargs,
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE,
      stderr=sys.stderr)

  procOut = CheckDoneReader(p.stdout)
  # harnessTh = threading.Thread(target=harness, args=(env, procOut))
  readTh = threading.Thread(target=readPassLoop, args=(sys.stdin, p.stdin))
  try:
    # harnessTh.start()
    readTh.start()
    harness(env, procOut)
    # while p.returncode is None and harnessTh.is_alive():
    #   time.sleep(0.2)
    #   p.poll()
  finally:
    # time.sleep(0.5) # Allow some time for buffers to flush
    DONE = True
    p.poll()

  if p.returncode:
    print("!! Got error rc:", p.returncode)
    sys.exit(p.returncode)
  if env.uncaughtErr:
    print("!!", env.uncaughtErr)

  if ERRORS:
    print("!! Got errors:", ERRORS)
    raise ERRORS[0]

if __name__ == '__main__':
  args = parser.parse_args()
  try:
    main(args)
  except Exception:
    if not args.pdb: raise
    import pdb, traceback
    extype, value, tb = sys.exc_info()
    traceback.print_exc()
    pdb.post_mortem(tb)
