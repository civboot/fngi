##########################
# Fngi Make script
# This python script not only build fngi, but also:
# - Generates source code to avoid boilerplate
# - Runs tests

import argparse
import collections
import enum
import os
import pathlib
import pprint
import re
import sys
import select
import signal
import subprocess
import time
import threading

from typing import Dict, Tuple, List, Deque
from dataclasses import dataclass
from itertools import chain

import zoa
from zoa import readexact

##########################
# ****************************
# * Utility Functions

def write(path, contents):
  with open(path, 'w') as f: f.write(contents)

def gcc_run(args):
  subprocess.run(["gcc", "-o", "bin/run"] + args, check=True)
  time.sleep(0.3) # For gcc error logging
  subprocess.run(["bin/run"])
  os.remove("bin/run")


# ****************************
# * Generating Source Code
# This repository has a fair amount of duplicated code between different
# libraries. The source of truth for that code changes depending on the context.
# In general:
# - Contants are kept in kernel/constants.sp
# - Error codes are kept in kernel/errors.sp
# - Global variable offsets are kept in kernel/kernel.h, generated by etc/gen.c


c_header = '''/** @file {filename}
 * DO NOT EDIT MANUALLY! THIS FILE WAS GENERATED BY etc/make.py
 *
 * @brief {brief}
 */
#ifndef {defname}
#define {defname}
{include}

{content}

#endif // {defname}
'''

fngi_types = '''
typedef uint8_t              U1;
typedef uint16_t             U2;
typedef uint32_t             U4;
typedef uint32_t             UA;
typedef int8_t               I1;
typedef int16_t              I2;
typedef int32_t              I4;
typedef uint32_t             Ref;
'''

CONST_SP_REGEX = re.compile( r'#(?P<value>[\w_]+)\s+#0=(?P<name>.+)', re.MULTILINE)

def defname(filename): return '__' + filename.upper().replace('.', '_').replace('/', '_')

def find_constants(path):
  if path.endswith('.sp'): regex = CONST_SP_REGEX
  else: assert False, path
  with open(path, 'r') as f:
    text = f.read()
  for m in re.finditer(regex, text):
    yield (m.group('name'), int(m.group('value'), 16))

def all_constants():
  return chain(
      find_constants('kernel/constants.sp'),
      find_constants('kernel/errors.sp'))

def constants_h():
  constants = '\n'.join(
    f'#define {name.split()[0]:<20} 0x{value:X}' for name, value
    in all_constants()
  )

  filename = 'kernel/constants.h'
  write(
    filename,
    c_header.format(
      filename=filename,
      brief = "Contains common types in fngi",
      defname=defname(filename),
      include="",
      content=constants),
  )

def gen_c(): gcc_run(["etc/gen.c"])


# ****************************
# * Building the Kernel

def build():
  pathlib.Path("bin/").mkdir(exist_ok=True)
  constants_h()
  gen_c()
  subprocess.run(["gcc", "-o", "bin/boot", "linux/kernel.c"], check=True)
  time.sleep(0.3) # For gcc error logging


# ****************************
# * Acting as Kernel Harness
# The core kernel (kernel.c) does handle very much debugging. Instead, it sends
# relevant data to the "harness" or build tool which keeps track of state to
# print out relevant debugging information.

DONE = False
def sigint(signal, frame): global DONE; DONE = True
signal.signal(signal.SIGINT, sigint)

parser = argparse.ArgumentParser(description='Fngi linux make script and harness.')
parser.add_argument('--build', action='store_true', help="Build kernel")
parser.add_argument('--test', action='store_true', help="Run tests")
parser.add_argument('--valgrind', action='store_true', help="Run with valgrind")
parser.add_argument('--log', default='LOG_USER', help="The log level, i.e. LOG_INFO.")
parser.add_argument('--syslog', default='LOG_COMPILER', help="The log level, i.e. LOG_INFO.")
parser.add_argument('--pdb', action='store_true', help="Enter debug on failure")

PWD = os.path.dirname(__file__)
with open(os.path.join(PWD, '../kernel/types.ty'), 'rb') as f:
  _parser = zoa.Parser(f.read())
_parser.parse()

tys = _parser.env.tys
Lvl = tys[b'Lvl']; Meta = tys[b'Meta']; Instr = tys[b'Instr']
LogEvent = tys[b'LogEvent'];  FileEvent = tys[b'FileEvent'];
DictEvent = tys[b'DictEvent']
ErrEvent = tys[b'ErrEvent'];  JmpEvent = tys[b'JmpEvent']
RetEvent = tys[b'RetEvent'];  Event = tys[b'Event']

ERRORS = []

class CheckDoneReader:
  """A reader that can be killed."""
  def __init__(self, fd):
    self.fd = fd

  def read(self, n, orNewline=False):
    out = bytearray()
    while n > 0:
      (rdRdy, wrRdy, xRdy) = select.select([self.fd.fileno()], [], [], 0.2)
      assert not wrRdy
      assert not xRdy
      if rdRdy:
        b = os.read(self.fd.fileno(), n)
        if not b: break  # EOF
        out.extend(b); n -= len(b)
      elif DONE:
        break  # DONE looks like EOF
      if orNewline and b'\n' in out: break
    return out

def wantStr(b):
  try: return b.decode('utf-8')
  except ValueError: return b

def nice(value):
  if isinstance(value, bytes): return wantStr(value)
  if isinstance(value, int):   return hex(value)

def integerBE(data): return int.from_bytes(data, 'big')
def integerLE(data, l=None): return int.from_bytes(data, 'little')
LVL = {0x1F: 'trace', 0x17: 'debug', 0x13: 'info', 0x11: 'warn', 0x10: 'crit'}

@dataclass
class FnEntry:
  ev: DictEvent
  loffsets: Dict[int, DictEvent]

@dataclass
class FngiEnv:
  """A Map of the Fngi env from trace logs."""
  dicts: dict
  fns: dict
  glbls: dict
  codes: dict
  uncaughtErr: "ErrEvent" = None
  file: str = None
  pos: int = None
  currentFn: DictEvent = None
  instrLookup: dict = None

def isCallstk(instr: Instr):
  return instr.is_xsw() or instr.is_xlw() or instr.is_xll() or instr.is_xsl()

def findErrorCodes(glbls):
  return {value: name for (name, value) in glbls.items()
          if name.startswith('E_')}

def orderFns(dicts):
  fns = []
  for f in dicts.values():
    if isinstance(f, FnEntry): f = f.ev
    if f.m.is_fn(): fns.append(f)

  fns.sort(key=lambda f: f.v)
  return fns

def findFnLesser(fns, addr):
  # A few notes:
  # - The ep that gets put on the call stack is the one where execution
  #   will CONTINUE.
  # - Therefore if the addr=fn.ref then the previous function called
  #   something and bleeds into the next function on return
  #   (this may be intentional fall-through or they expect failure).
  if not fns or not addr: return None
  if fns[0].v > addr: return None
  if fns[-1].v < addr: return len(fns) - 1
  i = 0
  while i < len(fns):
    if fns[i].v >= addr:
      return i - 1
    i += 1

def getFnAndNext(fns: List[DictEvent], fnI: int) -> (DictEvent, DictEvent):
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
  while i < len(ev.csz):
    ep, lsz = ev.cs[i], ev.csz[i]
    lsData = ev.ls[ls_i:ls_i + lsz]
    fnI = findFnLesser(fns, ep)
    fn, fnNext = getFnAndNext(fns, fnI)
    out.append(CallStkItem(ep=ep, fn=fn, fnNext=fnNext, localData=lsData))
    i += 1;  ls_i += lsz
  return out

def printCallStack(callStk):
  for i, item in enumerate(callStk):
    ep, key, ref = nice(item.ep), "BASE", 0
    if item.fn:
      key, ref = nice(item.fn.key), nice(item.fn.v)
    loc = "in"
    if ref:
      loc = f"{item.ep - item.fn.v:>5} bytes"
      if item.fnNext:
        p = (item.ep - item.fn.v) / (item.fnNext.v - item.fn.v)
        loc = loc + f" ({100 * p:>.3}%) into"
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
  - if the event is a function, it updates the currentFn
  - if the event is a local it updates the current function's local offsets.
  """
  value = ev  # the value to add to ev.dicts
  if not ev.m.is_const():
    if ev.m.is_local() and env.currentFn:
      env.currentFn.loffsets[ev.v] = ev
    elif ev.m.is_fn():
      # We store a FnEntry instead of a raw event.
      # This allows us to track the local values as we recieve them.
      value = FnEntry(ev, {})
      env.currentFn = value
      env.fns[ev.v] = value
  env.dicts[ev.ref] = value


def handleErrEvent(env, ev):
  file = wantStr(env.file.name)
  name = env.codes.get(ev.code, "Unknown Err"),
  env.uncaughtErr = ev
  print(" !! ERROR", end="")
  print(f" (file: {file} [{ev.line}])", end="")
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

  # print("\nAll fns:")
  # for fn in fns: print(f"  {fn.v:04X}: {nice(fn.key)}")

def handleExecuteEvent(env, ev):
  out = ["+ " * ev.depth]

  name = env.fns[ev.jloc].ev.key if ev.jloc in env.fns else '???'
  out.append(f"[{ev.instr:<2X} @{ev.jloc:<4X} {name}")
  print(''.join(out))

def inputs_stream(env, io):
  while True:
    waitForStart(io)
    z = zoa.from_zoab(io)
    if not z.arr: continue
    yield Event.frZ(z)

def _eventReader(env, io):
  """Harness for spor.

  Note: if using io=stdin, use stdin.buffer instead, see:
    https://docs.python.org/3/library/sys.html#sys.stdin
  """

  for i, ev in enumerate(inputs_stream(env, io)):
    if ev is None: print(f"ev is none at i={i}");
    elif ev.log:
      if ev.log.lvl.is_silent(): print(nice(ev.log.msg), end="")
      else: print(f"{LVL[ev.log.lvl.value]}: {nice(ev.log.msg)}")
    elif ev.dict: handleDictEvent(env, ev.dict);
    elif ev.err: handleErrEvent(env, ev.err);
    elif ev.jmp: handleExecuteEvent(env, ev.jmp);
    elif ev.file: env.file = ev.file
    else: raise ValueError(ev)

def eventReader(env, io):
  try:
    return _eventReader(env, io)
  except zoa.Eof: pass
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

def readPassLoop(inp, toOut):
  try:
    try: _readPassLoop(inp, toOut)
    except ValueError as e:
      if str(e) == 'I/O operation on closed file': pass
      else: raise
  except Exception as e:
    ERRORS.append(e)
    raise


def getLogHex(env, lvl: str) -> str:
  if lvl == 'LOG_SILENT': return '00'
  out = env.glbls[lvl]
  assert out <= 0xFF
  return '{:02X}'.format(out)


def main(args):
  global DONE

  glbls = dict(all_constants())
  env = FngiEnv(
      dicts = {},
    fns = {},
    glbls = glbls,
    codes = findErrorCodes(glbls),
    instrLookup = {
      value: key.split()[0] for key, value in glbls.items()
      if key.split()[0] in {
          'JMPL', 'JMPW', 'JZL', 'JTBL', 'XL', 'XW', 'XSL', 'XSW',
          'RET', 'RETZ'}
    },
  )

  if args.build: build()

  cargs = [
    "./bin/boot",
    "1" if args.test else "0",
    getLogHex(env, args.syslog),
    getLogHex(env, args.log),
  ]

  if args.valgrind: cargs.insert(0, 'valgrind')

  print("Running args:", cargs)
  p = subprocess.Popen(
      args=cargs,
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE,
      stderr=sys.stderr)

  procOut = CheckDoneReader(p.stdout)
  readTh = threading.Thread(target=readPassLoop, args=(sys.stdin, p.stdin))
  try:
    readTh.start()
    eventReader(env, procOut)
  finally:
    DONE = True
    p.poll()

  if p.returncode:
    print("!! Got error rc:", p.returncode)
    sys.exit(p.returncode)
  if env.uncaughtErr:
    print("!!", env.uncaughtErr)
  if ERRORS:
    if len(ERRORS) > 1:
      print("!! Got additional errors:", ERRORS[1:])
    raise ERRORS[0]

# ****************************
# * Main

if __name__ == '__main__':
  args = parser.parse_args()
  try:
    main(args)
  except Exception as e:
    if not args.pdb: raise
    import pdb, traceback
    extype, value, tb = sys.exc_info()
    traceback.print_exc()
    pdb.post_mortem(tb)

