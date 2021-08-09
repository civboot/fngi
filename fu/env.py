from .imports import *
from .mem import Mem
from .stack import Stk, MStk
from .heap import Heap, MHeap

KiB = 2**10
TEST_MEM_SIZE = 4 * KiB
WS_SIZE = 1 * KiB
RS_SIZE = 256

@dataclass
class FuEnv(object):
    ws: Stk  # working stack
    rs: Stk  # return stack
    rsSc: Stk # parallel sector tracker for rs
    rsWU: Stk # parallel Ws Update tracker for rs

    mem: Mem
    heap: Heap
    ls: Stk # local stack

    cp: int = 0  # seCtor Ptr
    ep: int = None  # Execution Ptr

    @property
    def lp(self): return self.ls.m.sp  # Locals Ptr

    def pushr(self, addr: APtr, wsUpdate: int):
        addrR = CPtr(addr.value & MASK_16)
        cp = CPtr(addr.value >> 16)
        u = U16(wsUpdate)

        self.rs.push(addrR)
        self.rsSc.push(cp)
        self.rsWU.push(u)

    def popr(self) -> Tuple[CPtr, CPtr, int]:
        # Make sure the data is all there.
        addrR = self.rs.fetch(0, CPtr)
        cp = self.rsSc.fetch(0, CPtr)
        u = self.rsWU.fetch(0, U16)

        # Update the stacks.
        self.rs.pop(CPtr); self.rsSc.pop(CPtr); self.rsWU.pop(U16)

        return (addrR, cp, u.value)


def _returnZero(): return 0

def createRegStk(size):
    return Stk(MStk.new(size), Mem(size, minPtr=0), getStart=_returnZero)

def createEnv(
        memSize=TEST_MEM_SIZE,
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
    env.ws.push(I32(0x00442211))
    env.ws.push(U8(42))
    assert 42 == env.ws.popv(U8)
    assert 0x00442211 == env.ws.popv(I32)

    env.pushr(APtr(0x10023004), 0x10)
    addrR, cp, u = env.popr()
    assert 0x1002 == cp.value
    assert 0x3004 == addrR.value
    assert 0x10 == u

    lp = env.lp
    assert lp == TEST_MEM_SIZE

    env.ls.push(U32(42))
    assert lp - 4 == env.lp

    env.heap.push(U32(42))
    assert env.heap.m.heap == 8

    assert env.lp == env.heap.getEnd()
    assert env.ls.getStart() == env.heap.m.heap
