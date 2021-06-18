
import unittest
from fngi import *


class TestStack(unittest.TestCase):
    def newStack(self):
        return Stack(bytearray(16), 0, 16)


    def testPushPopI16(self):
        s = self.newStack()
        s.push(I16(0x7008), align=False)
        assert len(s) == 2
        assert s.pop(I16, align=False).v == 0x7008

    def testPushPopI16Align(self):
        s = self.newStack()
        s.push(I16(0x7008), align=True)
        assert len(s) == 4
        assert s.pop(I16, align=True).v == 0x7008

    def testPushPopI32(self):
        s = self.newStack()
        s.push(I32(0x4200FF))
        assert s.pop(I32).v == 0x4200FF
