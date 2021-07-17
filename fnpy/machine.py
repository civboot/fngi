# This execution engine is written from scratch but inspired by "A Talk Near
# the Future of Python" presented by David Beazley. You can watch the whole
# talk here: https://www.youtube.com/watch?v=r-A78RgMhZU
#
# In it we define a Machine that interacts with our Env class, specifically the
# memory inside it. This allows us to define and execute functions.

from .wasm_constants import *
from .types import Env, ENV


def run(env: Env, code):
    ds = env.dataStack
    lenCode = len(code)
    index = 0
    while index < lenCode:
        instr = code[index]
        flowControl = None
        # wi stands for "webassembly instr"
        if isinstance(instr, int):
            wi = instr
            args = ()
        else:
            wi, *args = instr
        print(wasmName[wi], ds.debugStr())
        if wi == Wi32.const:
            ds.push(I32(args[0]))
        elif wi == Wi64.const:
            ds.push(I64(args[0]))

        elif wi == Wbr: flowControl = args[0]
        elif wi == Wbr_if and ds.popv(U32):
            flowControl = args[0]
        elif wi in {Wblock, Wloop}: flowControl = run(env, args[0])

        elif wi == Wi32.add:
            ds.push(I32(ds.popv(I32) + ds.popv(I32)))
        elif wi == Wi32.mul:
            ds.push(I32(ds.popv(I32) * ds.popv(I32)))

        elif wi == Wi32.le_s:
            ds.push(Bool(ds.popv(I32) <= ds.popv(I32)))

        elif wi in {Wi32.load, Wi32.store}:
            offset, align = 0, 0
            if not isinstance(instr, int):
                _, offset, align = instr
            assert align == 0, "TODO: not implemented"
            value = None
            if wi == Wi32.store: value = ds.pop(I32)
            ptr = ds.popv(U32) + offset

            if wi == Wi32.load: ds.push(env.memory.get(ptr, I32))
            elif wi == Wi32.store: env.memory.set(ptr, value)

        if flowControl is None:
            index += 1
            continue

        if flowControl > 0:
            # If the flow control is at a larger level, subtract one and let
            # next level handle it.
            flowControl -= 1
            return flowControl
        assert flowControl == 0

        if wi == Wloop:
            # repeat current instruction without incrementing index
            # (loop block)
            continue
        if wi == Wblock:
            # Break out of the current block
            return


def testRunSimple():
    env = ENV.copyForTest()
    run(env, [
        (Wi32.const, 10),
        (Wi32.const, 11),
        Wi32.add,
        (Wi32.const, 2),
        Wi32.mul,
    ])
    assert 42 == env.dataStack.popv(I32)

def testRunLoop():
    env = ENV.copyForTest()
    vPtr = env.heap.grow(4)
    run(env, [
        # @v = 0;
        (Wi32.const, vPtr),
        (Wi32.const, 0),
        Wi32.store,
        (Wloop, [           # while out <= 10: out += 1
            (Wi32.const, vPtr),
            (Wi32.const, vPtr),
            Wi32.load,
            (Wi32.const, 1), Wi32.add,
            Wi32.store,
            (Wi32.const, vPtr), Wi32.load,
            (Wi32.const, 10), Wi32.le_s,
            (Wbr_if, 0), # continue if true
        ]),
    ])
    assert 10 == env.dataStack.popv(I32)
