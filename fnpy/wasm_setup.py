# Imported by wasm.py
from collections import OrderedDict
from dataclasses import dataclass
import copy
import ctypes
from ctypes import c_bool as Bool
import operator

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

def _loadValue(env: "Env", args: tuple, ty: DataTy):
    ptr = _loadStorePtr(env.ds, args)
    value = env.memory.get(ptr, ty)
    env.ds.push(value)

def _storeValue(env: "Env", args: tuple, ty: DataTy):
    value = env.ds.pop(I32)
    ptr = _loadStorePtr(env.ds, args)
    env.memory.set(ptr, value)

def _doOp(env: "Env", args: tuple, ty: DataTy, operation):
    right = ds.popv(ty)
    left = ds.popv(ty)
    ds.push(ty(operation(left, right)))

# Used for classifying things into "modules"
class _Namespace: pass

# BINARY_INT = [
#     'add', 'sub', 'mul', 'div_s', 'div_u',
#     'rem_s', 'rem_u',
#     'and_', 'or_', 'xor',
#     'shl', 'shr_s', 'shr_u',
#     'rotl', 'rotr',
#     'eq', 'ne',
#     'lt_s', 'lt_u',
#     'gt_s', 'gt_u',
#     'le_s', 'le_u',
#     'ge_s', 'ge_u',
# ]

class UnreachableError(Exception): pass

def _raiseUunreachable(*args):
    raise UnreachableError()

def NI(*args):
    raise NotImplementedError()

def _select(e, a):
    question = e.ds.popv()
    v1 = e.ds.popv()
    v2 = e.ds.popv()
    e.ds.push(v1 if question else v2, U32)

_wasmOps = {
    'unreachable': _raiseUnreachable(),
    'nop': lambda e,a: None,
    # 'block': lambda e,a: n,
    # 'loop': lambda e,a: n,
    # 'if': lambda e,a: n,
    # 'else': lambda e,a: n,
    # 'end': lambda e,a: n,
    # 'br': lambda e,a: n,
    # 'br_if': lambda e,a: n,
    # 'br_table': lambda e,a: n,
    # 'return': lambda e,a: n,
    # 'call': lambda e,a: n,
    # 'call_indirect': lambda e,a: n,
    'drop': NI(),
    'select': _select,
    'local.get': lambda e,a: NI(),
    'local.set': lambda e,a: NI(),
    'local.tee': lambda e,a: NI(),
    'global.get': lambda e,a: NI(),
    'global.set': lambda e,a: NI(),
    'i32.load': lambda e,a: NI(),
    'i64.load': lambda e,a: NI(),
    'f32.load': lambda e,a: NI(),
    'f64.load': lambda e,a: NI(),
    'i32.load8_s': lambda e,a: NI(),
    'i32.load8_u': lambda e,a: NI(),
    'i32.load16_s': lambda e,a: NI(),
    'i32.load16_u': lambda e,a: NI(),
    'i64.load8_s': lambda e,a: NI(),
    'i64.load8_u': lambda e,a: NI(),
    'i64.load16_s': lambda e,a: NI(),
    'i64.load16_u': lambda e,a: NI(),
    'i64.load32_s': lambda e,a: n,
    'i64.load32_u': lambda e,a: NI(),
    'i32.store': lambda e,a: NI(),
    'i64.store': lambda e,a: NI(),
    'f32.store': lambda e,a: NI(),
    'f64.store': lambda e,a: NI(),
    'i32.store8': lambda e,a: NI(),
    'i32.store16': lambda e,a: NI(),
    'i64.store8': lambda e,a: NI(),
    'i64.store16': lambda e,a: NI(),
    'i64.store32': lambda e,a: NI(),
    'memory.size': lambda e,a: NI(),
    'memory.grow': lambda e,a: NI(),
    'i32.const': lambda e,a: NI(),
    'i64.const': lambda e,a: NI(),
    'f32.const': lambda e,a: NI(),
    'f64.const': lambda e,a: NI(),
    'i32.eqz': lambda e,a: NI(),
    'i32.eq': lambda e,a: NI(),
    'i32.ne': lambda e,a: NI(),
    'i32.lt_s': lambda e,a: NI(),
    'i32.lt_u': lambda e,a: NI(),
    'i32.gt_s': lambda e,a: NI(),
    'i32.gt_u': lambda e,a: NI(),
    'i32.le_s': lambda e,a: NI(),
    'i32.le_u': lambda e,a: NI(),
    'i32.ge_s': lambda e,a: NI(),
    'i32.ge_u': lambda e,a: NI(),
    'i64.eqz': lambda e,a: NI(),
    'i64.eq': lambda e,a: NI(),
    'i64.ne': lambda e,a: NI(),
    'i64.lt_s': lambda e,a: NI(),
    'i64.lt_u': lambda e,a: NI(),
    'i64.gt_s': lambda e,a: NI(),
    'i64.gt_u': lambda e,a: NI(),
    'i64.le_s': lambda e,a: NI(),
    'i64.le_u': lambda e,a: NI(),
    'i64.ge_s': lambda e,a: NI(),
    'i64.ge_u': lambda e,a: NI(),
    'f32.eq': lambda e,a: NI(),
    'f32.ne': lambda e,a: NI(),
    'f32.lt': lambda e,a: NI(),
    'f32.gt': lambda e,a: NI(),
    'f32.le': lambda e,a: NI(),
    'f32.ge': lambda e,a: NI(),
    'f64.eq': lambda e,a: NI(),
    'f64.ne': lambda e,a: NI(),
    'f64.lt': lambda e,a: NI(),
    'f64.gt': lambda e,a: NI(),
    'f64.le': lambda e,a: NI(),
    'f64.ge': lambda e,a: NI(),
    'i32.clz': lambda e,a: NI(),
    'i32.ctz': lambda e,a: NI(),
    'i32.popcnt': lambda e,a: NI(),
    'i32.add': lambda e,a: NI(),
    'i32.sub': lambda e,a: NI(),
    'i32.mul': lambda e,a: NI(),
    'i32.div_s': lambda e,a: NI(),
    'i32.div_u': lambda e,a: NI(),
    'i32.rem_s': lambda e,a: NI(),
    'i32.rem_u': lambda e,a: NI(),
    'i32.and_': lambda e,a: NI(),
    'i32.or_': lambda e,a: NI(),
    'i32.xor': lambda e,a: NI(),
    'i32.shl': lambda e,a: NI(),
    'i32.shr_s': lambda e,a: NI(),
    'i32.shr_u': lambda e,a: NI(),
    'i32.rotl': lambda e,a: NI(),
    'i32.rotr': lambda e,a: NI(),
    'i64.clz': lambda e,a: NI(),
    'i64.ctz': lambda e,a: NI(),
    'i64.popcnt': lambda e,a: NI(),
    'i64.add': lambda e,a: NI(),
    'i64.sub': lambda e,a: NI(),
    'i64.mul': lambda e,a: NI(),
    'i64.div_s': lambda e,a: NI(),
    'i64.div_u': lambda e,a: NI(),
    'i64.rem_s': lambda e,a: NI(),
    'i64.rem_u': lambda e,a: NI(),
    'i64.and_': lambda e,a: NI(),
    'i64.or_': lambda e,a: NI(),
    'i64.xor': lambda e,a: NI(),
    'i64.shl': lambda e,a: NI(),
    'i64.shr_s': lambda e,a: NI(),
    'i64.shr_u': lambda e,a: NI(),
    'i64.rotl': lambda e,a: NI(),
    'i64.rotr': lambda e,a: NI(),
    'f32.abs': lambda e,a: NI(),
    'f32.neg': lambda e,a: NI(),
    'f32.ceil': lambda e,a: NI(),
    'f32.floor': lambda e,a: NI(),
    'f32.trunc': lambda e,a: NI(),
    'f32.nearest': lambda e,a: NI(),
    'f32.sqrt': lambda e,a: NI(),
    'f32.add': lambda e,a: NI(),
    'f32.sub': lambda e,a: NI(),
    'f32.mul': lambda e,a: NI(),
    'f32.div': lambda e,a: NI(),
    'f32.min': lambda e,a: NI(),
    'f32.max': lambda e,a: NI(),
    'f32.copysign': lambda e,a: NI(),
    'f64.abs': lambda e,a: NI(),
    'f64.neg': lambda e,a: NI(),
    'f64.ceil': lambda e,a: NI(),
    'f64.floor': lambda e,a: NI(),
    'f64.trunc': lambda e,a: NI(),
    'f64.nearest': lambda e,a: NI(),
    'f64.sqrt': lambda e,a: NI(),
    'f64.add': lambda e,a: NI(),
    'f64.sub': lambda e,a: NI(),
    'f64.mul': lambda e,a: NI(),
    'f64.div': lambda e,a: NI(),
    'f64.min': lambda e,a: NI(),
    'f64.max': lambda e,a: NI(),
    'f64.copysign': lambda e,a: NI(),
    'i32.wrap_i64': lambda e,a: NI(),
    'i32.trunc_f32_s': lambda e,a: NI(),
    'i32.trunc_f32_u': lambda e,a: NI(),
    'i32.trunc_f64_s': lambda e,a: NI(),
    'i32.trunc_f64_u': lambda e,a: NI(),
    'i64.extend_i32_s': lambda e,a: NI(),
    'i64.extend_i32_u': lambda e,a: NI(),
    'i64.trunc_f32_s': lambda e,a: NI(),
    'i64.trunc_f32_u': lambda e,a: NI(),
    'i64.trunc_f64_s': lambda e,a: NI(),
    'i64.trunc_f64_u': lambda e,a: NI(),
    'f32.convert_i32_s': lambda e,a: NI(),
    'f32.convert_i32_u': lambda e,a: NI(),
    'f32.convert_i64_s': lambda e,a: NI(),
    'f32.convert_i64_u': lambda e,a: NI(),
    'f32.demote_f64': lambda e,a: NI(),
    'f64.convert_i32_s': lambda e,a: NI(),
    'f64.convert_i32_u': lambda e,a: NI(),
    'f64.convert_i64_s': lambda e,a: NI(),
    'f64.convert_i64_u': lambda e,a: NI(),
    'f64.promote_f32': lambda e,a: NI(),
    'i32.reinterpret_f32': lambda e,a: NI(),
    'i64.reinterpret_f64': lambda e,a: NI(),
    'f32.reinterpret_i32': lambda e,a: NI(),
    'f64.reinterpret_i64': lambda e,a: NI(),
}
