from .imports import *
from .struct import Ty, needAlign
from .mem import Mem

class OutOfBoundsError(IndexError): pass

class MHeap(ctypes.Structure):
    """A heap as represented in memory."""
    _fields_ = [
        ('start', Ptr), # the start of heap's memory
        ('heap', Ptr),  # the heap pointer
    ]

    @classmethod
    def new(cls, start):
        return cls(start, start)

def _check(m: MHeap, end, heap, size):
    if heap + size >= end:
        raise OutOfBoundsError(f"{hex(heap)}+{hex(size)} >= {hex(end)}")
    if heap < m.start:
        raise OutOfBoundsError(f"{hex(heap)} < {hex(m.start)}")


class Heap(object):
    """The heap grows up and you can manually shrink it."""
    def __init__(self, mheap: MHeap, mem: Mem, getEnd):
        self.m = mheap
        self.mem = mem
        self.getEnd = getEnd # stack pointer grows down, modifying the end.

    def getHeap(self): # used in locals stack implementation
        return self.m.heap

    def grow(self, size, align=True):
        """Grow the heap.

        Return the beginning of the (aligned) grown region."""
        out = self.m.heap
        if align:
            out = out + needAlign(out, size)
        heap = out + size
        _check(self.m, self.getEnd(), heap, size)
        self.m.heap = heap
        return out

    def shrink(self, size):
        heap = self.m.heap - size
        _check(self.m, self.getEnd(), heap, 0)
        self.m.heap = heap

    def push(self, value: Primitive, align=True) -> Primitive:
        """Push a Primitive then return it's mutable reference inside memory"""
        ptr = self.grow(ctypes.sizeof(value), align)
        return self.mem.storeFetch(ptr, value)


def testHeap():
    size = 0x100
    mem = Mem(size)
    heap = Heap(MHeap.new(1), mem, getEnd=lambda: size)
    assert 1 == heap.m.heap
    assert 1 == heap.getHeap()

    result = heap.grow(4, align=True)
    assert 4 == result
    assert 8 == heap.m.heap

    heap.shrink(2)
    assert 6 == heap.m.heap

    try:
        heap.shrink(0x20)
        assert False
    except OutOfBoundsError: pass
    assert 6 == heap.m.heap

    try:
        heap.grow(0x100)
        assert False
    except OutOfBoundsError: pass
    assert 6 == heap.m.heap
