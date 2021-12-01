# python -i notes/try_ctypes.py
from ctypes import *

buf = create_string_buffer(100)

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

f = Foo.from_buffer(buf)
heap = sizeof(Foo)

f.b0 = 1
f.b1 = 0x42
f.s0 = 0xF00F
f.i0 = 0x12345678

print(buf[0:heap].hex())

# get bytearray: bytearray(f)
# update buf: buf[10:10+sizeof(f)] = f

Array_U16_12 = c_uint16 * 12
arrStart = heap
arr = Array_U16_12.from_buffer(buf, heap)
heap += sizeof(Array_U16_12)

arr[0] = 0xFA01
arr[1] = 0xFA02
arr[2] = 0xFA03

print(buf[arrStart:heap].hex())
# Same as
print(buf[addressof(arr) - addressof(buf): heap].hex())
