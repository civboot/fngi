
import unittest
from fngi import *

class TestMoreStack(TestStack):
    def testGetSetI16(self):
        s = self.newStack()
        s.push(I16(0x7008))
        s.push(I16(0x3322))
        assert s.get(4, I16).value == 0x7008
        s.set(4, I16(0x1133))
        assert s.get(4, I16).value == 0x1133
        assert s.get(0, I16).value == 0x3322

    def testDirtyStack(self):
        """Test what happens if you have a dirty stack and pop the wrong type
        (don't do this).
        """
        m = Memory(16)
        m.set(4, U32(0x11112222))
        v = m.get(4, U32)
        m.set(4, U16(0xF00F))
        expected = 0xF00F2222 if BIG_ENDIAN else 0x1111F00F
        assert expected == v.value
