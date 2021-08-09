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

