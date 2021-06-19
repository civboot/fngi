# python -i notes/try_ctypes.py
from ctypes import *

class Foo(Structure):
    _fields_ = [
        ('b0', c_byte),
        ('b1', c_byte),
        ('s0', c_short),
        ('i0', c_long),
    ]

# Get offset:
# Foo.b0.offset
# Foo.s0.offset

f = Foo(1, 0x42, 0xF00F, 0x12345678)

# get bytearray: bytearray(f)
# update bytearray: b = bytearray(32); b[10:10+sizeof(f)] = f

class StackData(Structure):
    def __new__(cls, tys):
        print("tys:", tys)
        cls._fields_ = [("f" + str(i), t) for (i, t) in enumerate(tys)]
        return cls

    def __init__(self, **kwargs):
        super().__init__()
        print("kwargs:", kwargs)

s = StackData([c_byte, c_byte, c_short, c_long])
