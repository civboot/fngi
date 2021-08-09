from .imports import *
from .struct import Ty
from .mem import Mem

class StkUnderflowError(IndexError): pass
class StkOverflowError(IndexError): pass
class StkPrimitiveError(ValueError): pass

# Compute the index into a list for a stack representation
def _si(i): return -i - 1

class Stack(list):
    """
    A pure python stack. It's really just a list where the indexes are
    reversed. This means:
    - pushing puts the value at s[0], moving the other values up.
    - popping gets the value at s[0] and removes it.
    - printing shows the stack in the correct order.
    """
    def __init__(self, data=()):
        if data: data = reversed(data)
        super().__init__(data)

    def __repr__(self):
        return f"Stk{list(self)}"

    def __iter__(self):
        return super().__reversed__()

    def __reversed__(self):
        return super().__iter__()

    def _getslice(self, sl):
        assert not sl.step, "not supported"
        # reverse sl and make them negative
        start = None if sl.start is None else _si(sl.start)
        stop = None if sl.stop is None else _si(sl.stop)
        return slice(start, stop, -1)

    def __getitem__(self, index):
        try:
            if isinstance(index, slice):
                return super().__getitem__(self._getslice(index))
            else:
                return super().__getitem__(_si(index))
        except IndexError as e:
            raise IndexError(str(e))

    def __delitem__(self, index):
        return super().__delitem__(_si(index))

    def __setitem__(self, index, value):
        try:
            if isinstance(index, slice):
                return super().__setitem__(self._getslice(index), value)
            else:
                return super().__setitem__(_si(index), value)
        except IndexError as e:
            raise IndexError(str(e))

    def insert(self, index, value):
        return super().insert(_si(index), value)

    def push(self, value):
        return super().append(value)

    def assertEq(self, expectedList):
        assert expectedList == list(self)


def testStack():
    s = Stack(range(10))
    assert 0 == s.pop()
    s.push(0)
    assert 0 == s[0]
    assert 2 == s[2]
    assert 9 == s[-1]
    assert [0,1,2] == s[:3]
    assert [3,4] == s[3:5]
    del s[3]
    s.assertEq([0,1,2,4,5,6,7,8,9])


class MStk(ctypes.Structure):
    """A fu stack as represented in memory."""
    _fields_ = [
        ('end', APtr),   # the end of stack's memory
        ('sp', APtr),  # the stack pointer
    ]

    @classmethod
    def new(cls, end):
        return cls(end, end)

def _check(m, start, sp, offset, size, ty=None, tys=None):
    if sp + offset + size > m.end:
        raise StkUnderflowError(f"{hex(sp)}+{hex(offset)}+{hex(size)} >= {hex(m.end)}")
    if sp < start:
        raise StkUnderflowError(f"{hex(sp)} < {hex(start)}")
    if ty and ctypes.sizeof(ty) != ctypes.sizeof(tys[0]):
        raise TypeError(f"{ty} != {tys[0]}")


class Stk(object):
    """A fu stack."""
    def __init__(self, mstk: MStk, mem: Mem, getStart=None):
        self.m = mstk
        self.mem = mem
        self.tys = Stack()
        self.getStart = getStart  # heap grows up, modifying the start.

    def getSp(self): return self.m.sp  # used in env.heap implementation

    def fetch(self, offset: int, ty: Primitive):
        size = ctypes.sizeof(ty)
        _check(self.m, self.getStart(), self.m.sp, offset, size)
        return self.mem.fetch(self.m.sp + offset, ty)

    def fetchv(self, offset: int, ty: Primitive):
        return self.fetch(offset, ty).value

    def store(self, offset: int, value: Primitive):
        size = ctypes.sizeof(ty)
        _check(self.m, self.getStart(), self.m.sp, offset, size)
        self.mem.store(offset, value)

    def pop(self, ty: Primitive):
        size = ctypes.sizeof(ty)
        _check(self.m, self.getStart(), self.m.sp, 0, size, ty, self.tys)
        out = self.mem.fetch(self.m.sp, ty)
        self.m.sp += size
        self.tys.pop()
        return out

    def popv(self, ty: Primitive): return self.pop(ty).value

    def push(self, value: Primitive):
        size = ctypes.sizeof(value)
        sp = self.m.sp - size
        _check(self.m, self.getStart(), sp, 0, size)
        self.mem.store(sp, value)
        self.m.sp = sp
        self.tys.push(type(value))

    def shrink(self, st: Ty):
        size = st.size + needAlign(st)
        sp = self.m.sp + size
        _check(self.m, self.getStart(), sp, 0, size, st, self.tys)
        self.m.sp = sp
        return sp

    def grow(self, st: Ty):
        size = st.size + needAlign(st)
        sp = self.m.sp - size
        _check(self.m, self.getStart(), sp, 0, size, st, self.tys)
        self.m.sp = sp
        return sp

    def __len__(self):
        return self.m.end - self.m.sp

    def __repr__(self):
        offset = 0
        values = []
        for ty in self.tys:
            values.append(hex(self.fetchv(offset, ty)))
            offset += ctypes.sizeof(ty)
        return f'Stk{values}'

    def clearMemory(self):
        self.m.sp = self.m.end
        self.tys = Stack()


def testStk():
    size = 0x100
    mem = Mem(size)
    stk = Stk(MStk.new(size), mem, getStart=lambda: 1)

    try: stk.pop(U8); assert False
    except StkUnderflowError: pass

    stk.push(U8(3))
    stk.tys.assertEq([U8])

    assert 3 == stk.fetchv(0, U8)
    assert 3 == stk.popv(U8)


