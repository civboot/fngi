from .imports import *
from .mem import Mem
from .stack import Stk, MStk
from .heap import Heap, MHeap

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
    heap: Heap
    ls: Stk

    cp: int = 0  # seCtor Ptr
    ep: int = None  # Execution Ptr

    @property
    def lp(self): return self.ls.m.sp  # Locals Ptr

def _returnZero(_): return 0

def createRegStk(size):
    return Stk(MStk(size), Mem(size, minPtr=0), getStart=_returnZero)

def createEnv(
        memSize=4096,
        wsSize=WS_SIZE,
        rsSize=RS_SIZE,
    ) -> FuEnv:
    """Create an Env."""
    ws = createRegStk(wsSize)
    rs = createRegStk(rsSize)
    rsSc = createRegStk(rsSize)
    rsWU = createRegStk(rsSize)

    mem = Mem(memSize)
    heap = Heap(MHeap.new(1), mem, None)
    ls = Stk(MStk.new(memSize), mem, getStart=heap.getHeap)
    heap.getEnd = ls.getSp

    return FuEnv(
        ws=ws, rs=rs, rsSc=rsSc, rsWU=rsWU,
        mem=mem, heap=heap, ls=ls,
    )

def testEnv():
    env = createEnv()
