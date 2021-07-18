# Imported by wasm.py
from collections import OrderedDict
from dataclasses import dataclass
import copy
import ctypes
from ctypes import c_bool as Bool

# This is the only way to get the base class for ctypes (_CData) which is
# technically private.
DataTy = ctypes.c_uint8.__bases__[0].__bases__[0]


def _loadStorePtr(ds: "Stack", args: any):
    """Used for load/store wasm instructions."""
    offset, align = 0, 0
    if args:
        offset, align = args
    ptr = ds.popv(U32) + offset
    if align and ptr % align != 0:
        ptr += align - (ptr % align)
    return ptr

def WloadValue(env: "Env", args: tuple, ty: DataTy):
    ptr = _loadStorePtr(env.ds, args)
    value = env.memory.get(ptr, ty)
    env.ds.push(value)

def WstoreValue(env: "Env", args: tuple, ty: DataTy):
    value = env.ds.pop(I32)
    ptr = _loadStorePtr(env.ds, args)
    env.memory.set(ptr, value)

