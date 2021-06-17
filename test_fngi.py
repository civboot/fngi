
import unittest
from fngi import *

def testNeedAlign():
    assert 0 == needAlign(0)
    assert 3 == needAlign(1)
    assert 2 == needAlign(2)
    assert 1 == needAlign(3)
    assert 0 == needAlign(4)

class TestStack(unittest.TestCase):
    def testPushPopI16(self):
        s = Stack(16)
        s.push(I16(0x7008), align=False)
        assert len(s) == 2
        assert s.pop(I16, align=False).v == 0x7008

    def testPushPopI16Align(self):
        s = Stack(16)
        s.push(I16(0x7008), align=True)
        assert len(s) == 4
        assert s.pop(I16, align=True).v == 0x7008

    def testPushPopI32(self):
        s = Stack(16)
        s.push(I32(0x4200FF))
        assert s.pop(I32).v == 0x4200FF
