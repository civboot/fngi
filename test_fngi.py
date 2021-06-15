
import unittest
import fngi

class TestStack(unittest.TestCase):
    def test_push_pop(self):
        s = fngi.Stack(16)
        s.push_value('I', 0x4200FF)
        s.push_value('H', 0x8008)
        assert s.pop_value('H') == 0x8008
        assert s.pop_value('I') == 0x4200FF
