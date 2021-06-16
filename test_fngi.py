
import unittest
import fngi

class TestStack(unittest.TestCase):
    def testPushPop(self):
        s = fngi.Stack(16)
        s.pushValue('I', 0x4200FF)
        s.pushValue('H', 0x8008)
        assert s.popValue('H') == (0x8008,)
        assert s.popValue('I') == (0x4200FF,)
