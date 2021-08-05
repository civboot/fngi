"""
Reference: https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md
"""
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

from .wasm_constants import *

WASM_PAGE = 0x10000 # 2^16 bytes

class WasmError(Exception):
    @classmethod
    def raise_(cls): raise cls()

class UnreachableError(WasmError): pass
class Trap(WasmError): pass

class Fn(object):
    def __init__(self, name: str, inputs: any, outputs: any):
        self._name, self.inputs, self.outputs = name, inputs, outputs

    def name(self):
        return self._name

def instrStr(instr):
    if isinstance(instr, int):
        intr = (instr,)
    instr = (wasmName[instr[0]],) + tuple(instr[1:])
    return str(instr)

class WasmFn(Fn):
    """A webassembly function."""
    def __init__(self,
            name: str,
            inputs: List[DataTy],
            outputs: List[DataTy],
            locals_: List[DataTy],
            code: any):
        super().__init__(name, inputs, outputs)
        self.locals = locals_
        self.code = code

        self.trueLocals = inputs + locals_
        for l in self.trueLocals: assert l in {I32, I64, F32, F64}

        self.offsets = self._calcOffsets(self.trueLocals)
        self.rstackSize = sum(map(sizeof, self.trueLocals))

    @staticmethod
    def _calcOffsets(trueLocals):
        """Returns an array that converts an index to a memory offset for local
        variables.

        See: self.offsets
        """
        offsets = []
        offset = 0
        for l in trueLocals:
            offsets.append(offset)
            offset += sizeof(l)
        return offsets

    def lget(self, env: "Env", index: int) -> DataTy:
        """Used to get a local value index."""
        return env.ds.get(self.offsets[index], self.trueLocals[index])

    def lset(self, env: "Env", index: int, value: DataTy):
        """Used to set a local value index."""
        assert sizeof(value) == sizeof(self.trueLocals[index])
        env.ds.set(self.offsets[index], value)

    def debugStr(self):
        return (
            f"name: {self.name()}"
            + "\nCode:\n" + '\n'.join(map(instrStr, self.code))
        )

def div_s(left, right):
    """
    The div_s instruction returns the signed quotient of its operands,
    interpreted as signed. The quotient is silently rounded to the nearest
    integer toward zero.
    """
    result = left / right
    if result > 0: return math.floor(result)
    return math.ceil(result)

def rem_s(left, right):
    """
    The rem_s instruction returns the signed remainder from a division of its
    operand values interpreted as signed, with the result having the same sign
    as the first operand (the dividend).
    """
    if right == 0: raise Trap()
    sign = -1 if left < 0 else 1
    return sign * (abs(left) % abs(right))

def shrI32(left, right):
    return left >> (right % 32)

def shlI32(left, right):
    right = right % 32
    sign = -1 if left < 0 else 1
    return sign * (abs(left) << right)

def popcnt(v):
    return bin(v).count('1')

def clzU32(v):  # hacky impl
    if v == 0: return 32
    return 32 - (len(bin(v)) - 2)

def cntzU32(v): # hacky impl
    if v == 0: return 32
    count = 0
    bin_ = bin(v)
    for c in reversed(bin_):
        if c != '0': return count
        count += 1

def shr_sI32(left, right):
    right = right % 32
    out = abs(left) >> right
    if left < 0:
        if out == 0: return -1
        return -1 * out
    return out

def _localGet(env, args):
    value = env.returnStack.getWasmLocal(args[0])
    env.ds.push(value)

def _localSet(env, args):
    value = env.ds.drop()
    env.returnStack.setWasmLocal(args[0], value)

def _localTee(env, args):
    value = env.ds.drop()
    env.ds.push(value)
    env.returnStack.setWasmLocal(args[0], value)

def NI(*args):
    raise NotImplementedError()

def _loadStorePtr(ds: "Stack", args: any):
    """Used for load/store wasm instructions."""
    offset, align = 0, 0
    if args:
        offset, align = args
    ptr = ds.popv(U32) + offset
    if align and ptr % align != 0:
        ptr += align - (ptr % align)
    return ptr

def _loadValue(ty: DataTy):
    def impl(env, args):
        ptr = _loadStorePtr(env.ds, args)
        value = env.memory.get(ptr, ty)
        env.ds.push(value)
    return impl

def _storeValue(ty: DataTy):
    def impl(env, args):
        value = env.ds.pop(ty)
        ptr = _loadStorePtr(env.ds, args)
        env.memory.set(ptr, value)
    return impl

def _storeTrunc(ty: DataTy, trunc):
    def impl(env, args):
        value = trunc(env.ds.pop(ty))
        ptr = _loadStorePtr(env.ds, args)
        env.memory.set(ptr, value)
    return impl

def _doUnary(ty: DataTy, operation):
    def impl(env, args):
        v = env.ds.popv(ty)
        env.ds.push(ty(operation(v)))
    return impl

def _doBinary(ty: DataTy, operation):
    def impl(env, args):
        right = env.ds.popv(ty)
        left = env.ds.popv(ty)
        env.ds.push(ty(operation(left, right)))
    return impl

def _doBinMultiTy(leftTy, rightTy, outTy, operation):
    def impl(env, args):
        right = env.ds.popv(rightTy)
        left = env.ds.popv(leftTy)
        env.ds.push(outTy(operation(left, right)))
    return impl

def _memsize(env, _args):
    pages = len(env.memory.data) // 0x10000
    env.ds.push(I32(pages))

# TODO: this is only for little endian
def _1bytes(v): return U8(bytes(v)[:1])
def _2bytes(v): return U16(bytes(v)[:2])
def _4bytes(v): return U32(bytes(v)[:4])

def _bitand(l, r): return l & r
def _bitor(l, r): return l | r
def _bitxor(l,r): return l ^ r
def _inverse(l): return ~l


def rotl(width, a, n):
    "Rotate a, n times to the left"
    if n > 0:
        mask = (1 << width) - 1
        a, n = a & mask, n % width
        return (((a << n) & mask)      # bottom shifted up and masked
                | (a >> (width - n)))  # Top moved down
    elif n == 0: return a
    else: return rotr(width, a, -n)

def rotr(width, a, n):
    "Rotate a, n times to the right"
    if n > 0:
        mask = (1 << width) - 1
        a, n = a & mask, n % width
        return ((a >> n)    # top moved down
                | ((a & ((1 << n) - 1))   # Bottom masked...
                   << (width - n)))  # ... then moved up
    elif n == 0: return a
    else: return rotl(width, a, -n)

def rotlI32(a, n):
    return rotl(32, a, n)

def rotrI32(a, n):
    return rotr(32, a, n)

wasmSubroutines = {
  w.unreachable: UnreachableError.raise_,
  w.nop: lambda e,a: None,

  # Note: Implemented in machine.py directly
  w.block: None,
  w.loop: None,
  w.if_: None,
  w.else_: None,
  w.end: None,
  w.br: None,
  w.br_if: None,
  w.br_table: None,
  w.return_: None,
  w.call: None,
  w.call_indirect: None,

  w.drop: lambda e,a: e.ds.drop(),
  w.select: lambda e,a: e.ds.select(),
  w.local.get: _localGet,
  w.local.set: _localSet,
  w.local.tee: _localTee,
  w.global_.get: NI,
  w.global_.set: NI,
  w.i32.load: _loadValue(I32),
  w.i64.load: _loadValue(I64),
  w.f32.load: _loadValue(F32),
  w.f64.load: _loadValue(F64),
  w.i32.load8_s: _loadValue(I8),
  w.i32.load8_u: _loadValue(U8),
  w.i32.load16_s: _loadValue(I16),
  w.i32.load16_u: _loadValue(U16),
  w.i64.load8_s: _loadValue(I8),
  w.i64.load8_u: _loadValue(U8),
  w.i64.load16_s: _loadValue(I16),
  w.i64.load16_u: _loadValue(U16),
  w.i64.load32_s: _loadValue(I32),
  w.i64.load32_u: _loadValue(U32),
  w.i32.store: _storeValue(I32),
  w.i64.store: _storeValue(I64),
  w.f32.store: _storeValue(F32),
  w.f64.store: _storeValue(F64),
  w.i32.store8: _storeTrunc(I32, _1bytes),
  w.i32.store16: _storeTrunc(I32, _2bytes),
  w.i64.store8: _storeTrunc(I64, _1bytes),
  w.i64.store16: _storeTrunc(I64, _2bytes),
  w.i64.store32: _storeTrunc(I64, _4bytes),
  w.memory.size: _memsize,
  w.memory.grow: lambda e,a: env.heap.grow(a[0] * WASM_PAGE),
  w.i32.const: lambda e,a: e.ds.push(I32(a[0])),
  w.i64.const: lambda e,a: e.ds.push(I64(a[0])),
  w.f32.const: lambda e,a: e.ds.push(F32(a[0])),
  w.f64.const: lambda e,a: e.ds.push(F64(a[0])),
  w.i32.eqz: _doUnary(I32, operator.not_),
  w.i32.eq: _doBinary(I32, operator.eq),
  w.i32.ne: _doBinary(I32, operator.ne),
  w.i32.lt_s: _doBinary(I32, operator.lt),
  w.i32.lt_u: _doBinMultiTy(U32, U32, I32, operator.lt),
  w.i32.gt_s: _doBinary(I32, operator.gt),
  w.i32.gt_u: _doBinMultiTy(U32, U32, I32, operator.gt),
  w.i32.le_s: _doBinary(I32, operator.le),
  w.i32.le_u: _doBinMultiTy(U32, U32, I32, operator.le),
  w.i32.ge_s: _doBinary(I32, operator.ge),
  w.i32.ge_u: _doBinMultiTy(U32, U32, I32, operator.ge),
  w.i64.eqz: _doUnary(I64, operator.not_),
  w.i64.eq: _doBinary(I64, operator.eq),
  w.i64.ne: _doBinary(I64, operator.ne),
  w.i64.lt_s: _doBinary(I64, operator.lt),
  w.i64.lt_u: _doBinMultiTy(U32, U32, I32, operator.lt),
  w.i64.gt_s: _doBinary(I64, operator.gt),
  w.i64.gt_u: _doBinMultiTy(U32, U32, I32, operator.gt),
  w.i64.le_s: _doBinary(I64, operator.le),
  w.i64.le_u: _doBinMultiTy(U32, U32, I32, operator.le),
  w.i64.ge_s: _doBinary(I64, operator.ge),
  w.i64.ge_u: _doBinMultiTy(U32, U32, I32, operator.ge),
  w.f32.eq: _doBinary(F32, operator.eq),
  w.f32.ne: _doBinary(F32, operator.ne),
  w.f32.lt: _doBinary(F32, operator.lt),
  w.f32.gt: _doBinary(F32, operator.gt),
  w.f32.le: _doBinary(F32, operator.le),
  w.f32.ge: _doBinary(F32, operator.ge),
  w.f64.eq: _doBinary(F64, operator.eq),
  w.f64.ne: _doBinary(F64, operator.ne),
  w.f64.lt: _doBinary(F64, operator.le),
  w.f64.gt: _doBinary(F64, operator.gt),
  w.f64.le: _doBinary(F64, operator.le),
  w.f64.ge: _doBinary(F64, operator.ge),
  w.i32.clz: _doUnary(U32, clzU32),
  w.i32.ctz: _doUnary(U32, cntzU32),
  w.i32.popcnt: _doUnary(U32, popcnt),
  w.i32.add: _doBinary(I32, operator.add),
  w.i32.sub: _doBinary(I32, operator.sub),
  w.i32.mul: _doBinary(I32, operator.mul),
  w.i32.div_s: _doBinary(I32, div_s),
  w.i32.div_u: _doBinary(U32, operator.floordiv),
  w.i32.rem_s: _doBinary(I32, rem_s),
  w.i32.rem_u: _doBinary(U32, operator.mod),
  w.i32.and_: _doBinary(I32, _bitand),
  w.i32.or_: _doBinary(I32, _bitor),
  w.i32.xor: _doBinary(I32, _bitxor),
  w.i32.shl: _doBinary(I32, shlI32),
  w.i32.shr_s: _doBinary(I32, shr_sI32),
  w.i32.shr_u: _doBinary(U32, shrI32),
  w.i32.rotl: _doBinMultiTy(U32, U32, I32, rotlI32),
  w.i32.rotr: _doBinMultiTy(U32, U32, I32, rotrI32),
  w.i64.clz: _doUnary(NI, NI),
  w.i64.ctz: _doUnary(NI, NI),
  w.i64.popcnt: NI,
  w.i64.add: NI,
  w.i64.sub: NI,
  w.i64.mul: NI,
  w.i64.div_s: NI,
  w.i64.div_u: NI,
  w.i64.rem_s: NI,
  w.i64.rem_u: NI,
  w.i64.and_: NI,
  w.i64.or_: NI,
  w.i64.xor: NI,
  w.i64.shl: NI,
  w.i64.shr_s: NI,
  w.i64.shr_u: NI,
  w.i64.rotl: NI,
  w.i64.rotr: NI,
  w.f32.abs: NI,
  w.f32.neg: NI,
  w.f32.ceil: NI,
  w.f32.floor: NI,
  w.f32.trunc: NI,
  w.f32.nearest: NI,
  w.f32.sqrt: NI,
  w.f32.add: NI,
  w.f32.sub: NI,
  w.f32.mul: NI,
  w.f32.div: NI,
  w.f32.min: NI,
  w.f32.max: NI,
  w.f32.copysign: NI,
  w.f64.abs: NI,
  w.f64.neg: NI,
  w.f64.ceil: NI,
  w.f64.floor: NI,
  w.f64.trunc: NI,
  w.f64.nearest: NI,
  w.f64.sqrt: NI,
  w.f64.add: NI,
  w.f64.sub: NI,
  w.f64.mul: NI,
  w.f64.div: NI,
  w.f64.min: NI,
  w.f64.max: NI,
  w.f64.copysign: NI,
  w.i32.wrap_i64: NI,
  w.i32.trunc_f32_s: NI,
  w.i32.trunc_f32_u: NI,
  w.i32.trunc_f64_s: NI,
  w.i32.trunc_f64_u: NI,
  w.i64.extend_i32_s: NI,
  w.i64.extend_i32_u: NI,
  w.i64.trunc_f32_s: NI,
  w.i64.trunc_f32_u: NI,
  w.i64.trunc_f64_s: NI,
  w.i64.trunc_f64_u: NI,
  w.f32.convert_i32_s: NI,
  w.f32.convert_i32_u: NI,
  w.f32.convert_i64_s: NI,
  w.f32.convert_i64_u: NI,
  w.f32.demote_f64: NI,
  w.f64.convert_i32_s: NI,
  w.f64.convert_i32_u: NI,
  w.f64.convert_i64_s: NI,
  w.f64.convert_i64_u: NI,
  w.f64.promote_f32: NI,
  w.i32.reinterpret_f32: NI,
  w.i64.reinterpret_f64: NI,
  w.f32.reinterpret_i32: NI,
  w.f64.reinterpret_i64: NI,
}
