from .imports import *
from .mem import Mem
from .stack import Stk

KiB = 2**10
WS_SIZE = 1 * KiB
RS_SIZE = 256

@dataclass
class FuEnv(object):
    ws: Stk
    rs: Stk
    rsSc: Stk # parallel sector tracker for rs
    rsWU: Stk # parallel Ws Update tracker for rs

    mem: Mem
    ls: Stk

    cp: int  # seCtor Ptr
    ep: int  # Execution Ptr

    @property
    def lp(self): return self.ls.m.sp  # Locals Ptr

def _returnZero(_): return 0

def createRegStk(size):
    return Stk(MStk(size), Mem(size, minPtr=0), getStart=_returnZero)

def createEnv(
        memSize=4096,
        wsSize=WS_SIZE,
        rsSize=RS_SIZE,
    ) -> Env:
    """Create an Env."""
    ws = createRegStk(wsSize)
    rs = createRegStk(rsSize)
    rsSc = createRegStk(rsSize)
    rsWU = createRegStk(rsSize)

    mem = Memory(memSize)
    heap = Heap(mem, MHeap.new(memSize), None)
    ls = Stk(MStk(memSize), mem, getStart=heap.getHeap)
    heap.getEnd = ls.getSp

    return Env(
        memory=mem,
        ds=dataStack,
        returnStack=returnStack,
        heap=heap,
        codeHeap=None, ba=None, arena=None,
        fns=[],
        tys={}, refs={},
    )
