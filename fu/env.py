from .imports import *
from .mem import Mem
from .stack import Stk, MStk
from .heap import Heap, MHeap

KiB = 2**10
TEST_MEM_SIZE = 4 * KiB
WS_SIZE = 1 * KiB
RS_SIZE = KiB // 2

class EPOutOfBounds(IndexError): pass

@dataclass
class FuEnv(object):
    ws: Stk  # working stack
    rs: Stk  # return stack
    rsWU: Stk # parallel Ws Update tracker for rs

    mem: Mem
    heap: Heap
    ls: Stk # local stack

    ep: int = None  # Execution Ptr

    ptrTy = U16

    @property
    def cp(self): return self.ep >> 16  # seCtor Ptr

    @property
    def lp(self): return self.ls.m.sp  # Locals Ptr

    def aPtr(self, ptr: Primitive) -> APtr:
        """Convert a ptr to an absolute pointer."""
        ty = type(ptr)
        if ty == APtr: return ptr
        elif ty == CPtr: return APtr(self.cp + ptr.value)
        else: raise TypeError(ptr)

    def pushr(self, aPtr: APtr, wsGrow: int):
        assert type(aPtr) == APtr
        u = U16(wsGrow)

        self.rs.push(aPtr)
        self.rsWU.push(u)

    def popr(self) -> Tuple[CPtr, CPtr, int]:
        # return cp, cPtr, wsGrow
        # Make sure the data is all there.
        aPtr = self.rs.fetch(0, APtr)
        wsGrow = self.rsWU.fetch(0, U16)

        # Update the stacks.
        self.rs.pop(APtr); self.rsWU.pop(U16)

        return (aPtr, wsGrow.value)

    def setEp(self, newEp: int):
        if not (1 <= newEp < len(self.mem)):
            raise EPOutOfBounds("ep out of bounds")
        self.ep = newEp

    def popImm(self):
        """Pop an immediate value from the execution pointer."""
        ep = self.ep
        out = self.mem.fetchv(ep, U16)
        self.setEp(ep + 2)
        return out

    def fetchCp(self, ptrOffset: int, ty: Primitive):
        """Fetch from the sector pointer offset."""
        return self.mem.fetchv(self.cp + ptrOffset, ty)

    def storeCp(self, ptrOffset: int, value: Primitive):
        """Fetch from the sector pointer offset."""
        return self.mem.store(self.cp + ptrOffset, value)

    def fetchLp(self, ptrOffset: int, ty: Primitive):
        """Fetch from the sector pointer offset."""
        return self.mem.fetchv(self.lp + ptrOffset, ty)

    def storeLp(self, ptrOffset: int, value: Primitive):
        """Fetch from the sector pointer offset."""
        return self.mem.store(self.lp + ptrOffset, value)


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
    rsWU = createRegStk(rsSize)

    mem = Mem(memSize)
    heap = Heap(MHeap.new(1), mem, None)
    ls = Stk(MStk.new(memSize), mem, getStart=heap.getHeap)
    heap.getEnd = ls.getSp

    return FuEnv(
        ws=ws, rs=rs, rsWU=rsWU,
        mem=mem, heap=heap, ls=ls,
    )

def testEnv():
    env = createEnv()
    env.ws.push(I32(0x00442211))
    env.ws.push(U8(42))
    assert 42 == env.ws.popv(U8)
    assert 0x00442211 == env.ws.popv(I32)

    env.pushr(APtr(0x10023004), 0x10)
    aPtr, u = env.popr()
    assert 0x10023004 == aPtr.value
    assert 0x10 == u

    lp = env.lp
    assert lp == TEST_MEM_SIZE

    env.ls.push(U32(42))
    assert lp - 4 == env.lp

    env.heap.push(U32(42))
    assert env.heap.m.heap == 8

    assert env.lp == env.heap.getEnd()
    assert env.ls.getStart() == env.heap.m.heap
