# This execution engine is written from scratch but inspired by "A Talk Near
# the Future of Python" presented by David Beazley. You can watch the whole
# talk here: https://www.youtube.com/watch?v=r-A78RgMhZU
#
# In it we define a Machine that interacts with our Env class, specifically the
# memory inside it. This allows us to define and execute functions.

from .imports import *
from .struct import Void, FnStructTy, Fn
from .env import Env, ENV
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

def fnInit(env: Env, fn: Fn):
    """Before executing a fn's code:
    - The locals space (which includes inputs) must be reserved on the return
      stack.
    - The inputs must be stored in the correct locals indexes from the data
      stack.
    """
    rs = env.returnStack
    ds = env.ds
    fnSt = fn.struct

    if fnSt.size: rs.grow(fnSt)
    # wasmInp = fnSt.fieldMap['wasmInp'].ty.tys
    # for i in reversed(range(len(wasmInp))):
    #     ty = wasmInp[i]
    #     value = ds.pop(ty)
    #     rs.setWasmLocal(i, value)

def fnTeardown(env: Env, fn: Fn):
    """After a function has finished executing:
    - The locals space must be un-reserved.
    """
    if fn.struct.size: env.returnStack.shrink(fn.struct)

def run(env: Env, code: List[any]):
    pass


# def testRunSimple():
#     env = ENV.copyForTest()
#     runWasm(env, [
#         (w.i32.const, 10),
#         (w.i32.const, 11),
#         w.i32.add,
#         (w.i32.const, 2),
#         w.i32.mul,
#     ])
#     assert 42 == env.ds.popv(I32)
# 
# def testRunLoop():
#     env = ENV.copyForTest()
#     vPtr = env.heap.grow(4)
#     runWasm(env, [
#         # @v = 0;
#         (w.i32.const, vPtr),
#         (w.i32.const, 0),
#         w.i32.store,
#         # while True:
#         #  out += 1
#         #  if 10 > out: continue
#         (w.loop, [
#             (w.i32.const, vPtr),
#             (w.i32.const, vPtr),
#             w.i32.load,
#             (w.i32.const, 1), w.i32.add,
#             w.i32.store,
#             (w.i32.const, vPtr), w.i32.load,
#             (w.i32.const, 10),
#             w.i32.lt_s,
#             (w.br_if, 0), # continue if true
#         ]),
#         (w.i32.const, vPtr),
#         w.i32.load,
#     ])
#     result = env.ds.popv(I32)
#     assert 10 == result
