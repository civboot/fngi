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
    for instr in code:
        if type(instr) not in (tuple, list):
            wi = instr
        else:
            wi = instr[0]
        if wi == Wi32.const:
            ds.push(I32(instr[1]))
        elif wi == Wi32.add:
            ds.push(I32(ds.pop(I32).value + ds.pop(I32).value))
        elif wi == Wi32.mul:
            ds.push(I32(ds.pop(I32).value * ds.pop(I32).value))

def testRunSimple():
    env = ENV.copyForTest()
    run(env, [
        (Wi32.const, 10),
        (Wi32.const, 11),
        Wi32.add,
        (Wi32.const, 2),
        Wi32.mul,
    ])
    assert 42 == env.dataStack.pop(I32).value
