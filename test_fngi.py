
import unittest
import fngi

class TestStack(unittest.TestCase):
    def testPushPop(self):
        s = fngi.Stack(16)
        s.pushValue('I', 0x4200FF)
        s.pushValue('H', 0x8008)
        assert len(s) == 6
        assert s.popValue('H') == (0x8008,)
        assert s.popValue('I') == (0x4200FF,)

    def testPushPopTy(self):
        s = fngi.Stack(16)
        s.pushTy(fngi.u8, 0x8)
        assert len(s) == 4
        s.pushTy(fngi.u32, 0x4200FF)
        assert s.popTy(fngi.u32) == (0x4200FF,)
        assert s.popTy(fngi.u8) == (0x8,)
