from .imports import *
from .struct import Ty, tyOf, needAlign

class NullPtrError(IndexError): pass
class AlignmentError(IndexError): pass

class Mem(object):
    """Access to raw byte memory."""

    def __init__(self, size, minPtr=1):
        self.minPtr = minPtr
        self.data = ctypes.create_string_buffer(size)
        self.data[0:2] = b'\xDE\xAD' # address 0 is "0xDEAD"

    def ptrTo(self, value: Primitive) -> int:
        """Get the index of the value within self.data.

        The definition of Ptr is this index."""
        return ctypes.addressof(value) - ctypes.addressof(self.data)

    def fetch(self, ptr: int, ty: Primitive, copy=False):
        ty = tyOf(ty)
        self.checkRange(ptr + ctypes.sizeof(ty))
        return ty.from_buffer(self.data, ptr)

    def fetchv(self, ptr: int, ty: Primitive):
        return self.fetch(ptr, ty).value

    def fetchCopy(self, ptr: int, ty: Primitive):
        ty = tyOf(ty)
        self.checkRange(ptr + ctypes.sizeof(ty))
        return ty.from_buffer_copy(self.data, ptr)

    def store(self, ptr, value: Primitive):
        size = ctypes.sizeof(value)
        self.checkRange(ptr, size)
        self.data[ptr:ptr + size] = bytes(value)

    def storeFetch(self, ptr, value: Primitive) -> Primitive:
        self.store(ptr, value)
        return self.fetch(ptr, type(value))

    def checkRange(self, ptr: int, size: int = 0):
        if ptr < self.minPtr or (ptr + size) > len(self.data):
            msg = f"ptr={ptr} minPtr={self.minPtr}, memorySize={len(self.data)}"
            raise NullPtrError(msg)
        if needAlign(ptr, size):
            raise AlignmentError(f"{ptr} misaligned to {size}")


def testMem():
    mem = Mem(0x1000)
    try:
        mem.store(0, U32(42))
        assert False
    except NullPtrError: pass
    try:
        mem.store(5, U32(42))
        assert False
    except AlignmentError: pass

    mem.store(4, U32(42))
    assert 42 == mem.fetchv(4, U32)

    # Ctypes ".value" also update memory in place.
    a = mem.storeFetch(0x100, U32(1))
    assert mem.ptrTo(a) == 0x100
    assert 1 == mem.fetchv(0x100, U32)
    a.value = 32
    assert 32 == mem.fetchv(0x100, U32)

    # fetchCopy cancels this behavior
    b = mem.fetchCopy(0x100, U32)
    a.value = 42
    assert 42 == mem.fetchv(0x100, U32), "a changed"
    assert 32 == b.value, "b didn't change"
    b.value = 99
    assert 42 == a.value, "a didn't change"
    assert 99 == b.value, "b changed"


