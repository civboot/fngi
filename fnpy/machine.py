# This execution engine is written from scratch but inspired by "A Talk Near
# the Future of Python" presented by David Beazley. You can watch the whole
# talk here: https://www.youtube.com/watch?v=r-A78RgMhZU
#
# In it we define a Machine that interacts with our Env class, specifically the
# memory inside it. This allows us to define and execute functions.

from pdb import set_trace as dbg
from .wasm_constants import *
from .types import Env, ENV
from ctypes import sizeof

STACK_TYPES = {U32, I32, U64, I64, F32, F64}
WASM_TYPES = {U8, I8, U16, I16}
WASM_TYPES.update(STACK_TYPES)

def checkTy(value: any, ty: DataTy):
    if type(value) is not ty: raise TypeError(f"{value} is not of type {ty}")

def assertTyEq(v1: DataTy, v2: DataTy):
    assert type(v1) == type(v2)
    assert v1.value == v2.value

def needAlign(size: int) -> int:
    """Return padding bytes needed to align a value of size."""
    if size % 4 == 0:
        return 0
    return 4 - (size % 4)

def formatArgs(args):
    out = []
    for a in args:
        if isinstance(a, int): out.append(hex(a))
        elif isinstance(a, (tuple, list)): out.append("(block)")
        else: out.append("???")
    return "Args: " + ' '.join(out)


def run(env: Env, code):
    ds = env.ds
    lenCode = len(code)
    index = 0
    while index < lenCode:
        instr = code[index]
        brLevel = None
        # wi stands for "webassembly instr"
        if isinstance(instr, int):
            wi = instr
            args = ()
        else:
            wi, *args = instr
        print("\nSTART ", wasmName[wi], formatArgs(args), '', ds.debugStr())
        subroutine = wasmSubroutines.get(wi, None)
        if subroutine: subroutine(env, args)

        elif wi == Wbr: brLevel = args[0]
        elif wi == Wbr_if and ds.popv(U32):
            brLevel = args[0]
        elif wi == Wblock:
            brLevel = run(env, args[0])
            if brLevel == 0:
                # br has been handled by getting here
                brLevel = None
            elif brLevel > 0: return brLevel - 1
        elif wi == Wloop:
            while True:
                brLevel = run(env, args[0])
                if brLevel is None: break # ended block w/out br
                if brLevel > 0: return brLevel - 1
                # else loop again

        elif wi == Wi32.add:
            ds.push(I32(ds.popv(I32) + ds.popv(I32)))
        elif wi == Wi32.mul:
            ds.push(I32(ds.popv(I32) * ds.popv(I32)))

        elif wi == Wi32.le_s:
            ds.push(U32(ds.popv(I32) <= ds.popv(I32)))
        elif wi == Wi32.gt_s:
            ds.push(U32(ds.popv(I32) > ds.popv(I32)))

        print("END   ", wasmName[wi], ds.debugStr())

        if brLevel is not None: return brLevel
        index += 1


def testRunSimple():
    env = ENV.copyForTest()
    run(env, [
        (Wi32.const, 10),
        (Wi32.const, 11),
        Wi32.add,
        (Wi32.const, 2),
        Wi32.mul,
    ])
    assert 42 == env.ds.popv(I32)

def testRunLoop():
    env = ENV.copyForTest()
    vPtr = env.heap.grow(4)
    run(env, [
        # @v = 0;
        (Wi32.const, vPtr),
        (Wi32.const, 0),
        Wi32.store,
        # while True:
        #  out += 1
        #  if 10 > out: continue
        (Wloop, [
            (Wi32.const, vPtr),
            (Wi32.const, vPtr),
            Wi32.load,
            (Wi32.const, 1), Wi32.add,
            Wi32.store,
            (Wi32.const, vPtr), Wi32.load,
            (Wi32.const, 10),
            Wi32.gt_s,
            (Wbr_if, 0), # continue if true
        ]),
        (Wi32.const, vPtr),
        Wi32.load,
    ])
    assert 10 == env.ds.popv(I32)
