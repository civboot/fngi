# These are imported by every module
from pdb import set_trace as dbg

from typing import Any
from typing import ByteString
from typing import Callable
from typing import List
from typing import Tuple
from typing import Dict
from typing import Union
from typing import List
import math

from collections import OrderedDict
from dataclasses import dataclass
from enum import Enum, auto as enumVal
import copy
import ctypes
import operator
import sys

BIG_ENDIAN = sys.byteorder != 'little'

# This is the only way to get the base class for ctypes (_CData) which is
# technically private.
Primitive = ctypes.c_uint8.__bases__[0].__bases__[0]

from ctypes import c_bool as Bool
from ctypes import c_uint8 as U8
from ctypes import c_uint16 as U16
from ctypes import c_uint32 as U32
from ctypes import c_uint64 as U64
from ctypes import c_int8 as I8
from ctypes import c_int16 as I16
from ctypes import c_int32 as I32
from ctypes import c_int64 as I64
from ctypes import c_float as F32
from ctypes import c_double as F64

MASK_8 = 0xFFFF
MASK_16 = 0xFFFFFFFF

class Trap(Exception): pass

INSTR_WIDTH = 2
MAX_U16 = 2**16

# Absolute Pointers
ASz = U32  # usize
ASIZE = ctypes.sizeof(ASz)
APtr = ASz  # Absolute pointer
Ptr = APtr

def splitAPtr(aPtr: APtr) -> (int, int):
    """Split an absolute pointer into section and CPtr"""
    assert type(aPtr) == APtr
    cp = CPtr(addr.value >> 16)
    cPtr = CPtr(addr.value & MASK_16)
    return cp, cPtr

# seCtor pointers
CSz = U16
CSIZE = ctypes.sizeof(CSz)
CPtr = CSz

class BetterEnum(Enum):
    @classmethod
    def fromStr(cls, name):
        out = getattr(cls, name)
        if not isinstance(out, cls): raise KeyError(name)
        return out
