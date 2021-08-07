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
from ctypes import sizeof

from collections import OrderedDict
from dataclasses import dataclass
import copy
import ctypes
import operator
import sys

BIG_ENDIAN = sys.byteorder != 'little'

# This is the only way to get the base class for ctypes (_CData) which is
# technically private.
DataTy = ctypes.c_uint8.__bases__[0].__bases__[0]

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

USIZE = 2 # 2 byte ptr width
USize = U16
Ptr = U16
