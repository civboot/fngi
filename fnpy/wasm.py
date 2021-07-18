from wasm_constants import *

class UnreachableError(Exception): pass

def _raiseUunreachable(*args):
    raise UnreachableError()

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
        value = env.ds.pop(I32)
        ptr = _loadStorePtr(env.ds, args)
        env.memory.set(ptr, value)
    return impl

def _doUnary(ty: DataTy, operation):
    def impl(env, args):
        v = ds.popv(ty)
        ds.push(ty(operation(v)))
    return impl

def _doBinary(ty: DataTy, operation):
    def impl(env, args):
        ds = env.ds
        right = ds.popv(ty)
        left = ds.popv(ty)
        ds.push(ty(operation(left, right)))
    return impl

def _doBinCnv(ty, operation, preConvTy):
    """Do binary with pre-conversion"""
    def impl(env, args):
        ds = env.ds
        right = preConvTy(ds.popv(ty))
        left = preConvTy(ds.popv(ty))
        ds.push(ty(operation(left, right)))
    return impl

def _select(e, a):
    question = e.ds.popv()
    v1 = e.ds.popv()
    v2 = e.ds.popv()
    e.ds.push(v1 if question else v2, U32)

# TODO: this is only for little endian
def _1bytes(v): return U8(bytes(v)[:1])
def _2bytes(v): return U16(bytes(v)[:2])
def _4bytes(v): return U32(bytes(v)[:4])

wasmSubroutine = {
  Wunreachable: _raiseUnreachable(),
  Wnop: lambda e,a: None,
  # Wblock: NI(),
  # Wloop: NI(),
  # Wif: NI(),
  # Welse: NI(),
  # Wend: NI(),
  # Wbr: NI(),
  # Wbr_if: NI(),
  # Wbr_table: NI(),
  # Wreturn: NI(),
  # Wcall: NI(),
  # Wcall_indirect: NI(),
  Wdrop: NI(),
  Wselect: _select,
  Wlocal.get: NI(),
  Wlocal.set: NI(),
  Wlocal.tee: NI(),
  Wglobal.get: NI(),
  Wglobal.set: NI(),
  Wi32.load: _loadValue(I32),
  Wi64.load: _loadValue(I64),
  Wf32.load: _loadValue(F32),
  Wf64.load: _loadValue(F64),
  Wi32.load8_s: _loadValue(I8),
  Wi32.load8_u: _loadValue(U8),
  Wi32.load16_s: _loadValue(I16),
  Wi32.load16_u: _loadValue(U16),
  Wi64.load8_s: _loadValue(I8),
  Wi64.load8_u: _loadValue(U8),
  Wi64.load16_s: _loadValue(I16),
  Wi64.load16_u: _loadValue(U16),
  Wi64.load32_s: _loadValue(I32),
  Wi64.load32_u: _loadValue(U32),
  Wi32.store: _storeValue(I32),
  Wi64.store: _storeValue(I64),
  Wf32.store: _storeValue(F32),
  Wf64.store: _storeValue(F64),
  Wi32.store8: _storeValue(_1bytes),
  Wi32.store16: _storeValue(_2bytes),
  Wi64.store8: _storeValue(_1bytes),
  Wi64.store16: _storeValue(_2bytes),
  Wi64.store32: _storeValue(_4bytes),
  Wmemory.size: lambda e,a: len(env.memory.data) // 0x10000,
  Wmemory.grow: lambda e,a: env.heap.grow(a[0] * 0x10000),
  Wi32.const: lambda e,a: e.ds.push(I32(a[0])),
  Wi64.const: lambda e,a: e.ds.push(I64(a[0])),
  Wf32.const: lambda e,a: e.ds.push(F32(a[0])),
  Wf64.const: lambda e,a: e.ds.push(F64(a[0])),
  Wi32.eqz: _doUnary(I32, operator.not_),
  Wi32.eq: _doBinary(I32, operator.eq),
  Wi32.ne: _doBinary(I32, operator.ne),
  Wi32.lt_s: _doBinary(I32, operator.lt),
  Wi32.lt_u: _doBinCnv(I32, operator.lt, U32),
  Wi32.gt_s: _doBinary(I32, operator.gt),
  Wi32.gt_u: _doBinCnv(I32, operator.gt, U32),
  Wi32.le_s: _doBinary(I32, operator.le),
  Wi32.le_u: _doBinCnv(I32, operator.le, U32),
  Wi32.ge_s: _doBinary(I32, operator.ge),
  Wi32.ge_u: _doBinCnv(I32, operator.ge, U32),
  Wi64.eqz: _doUnary(I64, operator.not_),
  Wi64.eq: _doBinary(I64, operator.eq),
  Wi64.ne: _doBinary(I64, operator.ne),
  Wi64.lt_s: _doBinary(I64, operator.lt),
  Wi64.lt_u: _doBinCnv(I64, operator.lt, U64),
  Wi64.gt_s: _doBinary(I64, operator.gt),
  Wi64.gt_u: _doBinCnv(I64, operator.gt, U64),
  Wi64.le_s: _doBinary(I64, operator.le),
  Wi64.le_u: _doBinCnv(I64, operator.le, U64),
  Wi64.ge_s: _doBinary(I64, operator.ge),
  Wi64.ge_u: _doBinCnv(I64, operator.ge, U64),
  Wf32.eq: _doBinary(NI, NI),
  Wf32.ne: _doBinary(NI, NI),
  Wf32.lt: _doBinary(NI, NI),
  Wf32.gt: _doBinary(NI, NI),
  Wf32.le: _doBinary(NI, NI),
  Wf32.ge: _doBinary(NI, NI),
  Wf64.eq: _doBinary(NI, NI),
  Wf64.ne: _doBinary(NI, NI),
  Wf64.lt: _doBinary(NI, NI),
  Wf64.gt: _doBinary(NI, NI),
  Wf64.le: _doBinary(NI, NI),
  Wf64.ge: _doBinary(NI, NI),
  Wi32.clz: _doBinary(NI, NI),
  Wi32.ctz: _doBinary(NI, NI),
  Wi32.popcnt: _doBinary(NI, NI),
  Wi32.add: _doBinary(NI, NI),
  Wi32.sub: _doBinary(NI, NI),
  Wi32.mul: _doBinary(NI, NI),
  Wi32.div_s: _doBinary(NI, NI),
  Wi32.div_u: _doBinary(NI, NI),
  Wi32.rem_s: _doBinary(NI, NI),
  Wi32.rem_u: _doBinary(NI, NI),
  Wi32.and_: _doBinary(NI, NI),
  Wi32.or_: _doBinary(NI, NI),
  Wi32.xor: _doBinary(NI, NI),
  Wi32.shl: _doBinary(NI, NI),
  Wi32.shr_s: _doBinary(NI, NI),
  Wi32.shr_u: _doBinary(NI, NI),
  Wi32.rotl: _doBinary(NI, NI),
  Wi32.rotr: _doBinary(NI, NI),
  Wi64.clz: _doBinary(NI, NI),
  Wi64.ctz: _doBinary(NI, NI),
  Wi64.popcnt: _doBinary(NI, NI),
  Wi64.add: _doBinary(NI, NI),
  Wi64.sub: _doBinary(NI, NI),
  Wi64.mul: _doBinary(NI, NI),
  Wi64.div_s: _doBinary(NI, NI),
  Wi64.div_u: _doBinary(NI, NI),
  Wi64.rem_s: _doBinary(NI, NI),
  Wi64.rem_u: _doBinary(NI, NI),
  Wi64.and_: _doBinary(NI, NI),
  Wi64.or_: _doBinary(NI, NI),
  Wi64.xor: _doBinary(NI, NI),
  Wi64.shl: _doBinary(NI, NI),
  Wi64.shr_s: _doBinary(NI, NI),
  Wi64.shr_u: _doBinary(NI, NI),
  Wi64.rotl: _doBinary(NI, NI),
  Wi64.rotr: _doBinary(NI, NI),
  Wf32.abs: _doBinary(NI, NI),
  Wf32.neg: _doBinary(NI, NI),
  Wf32.ceil: _doBinary(NI, NI),
  Wf32.floor: _doBinary(NI, NI),
  Wf32.trunc: _doBinary(NI, NI),
  Wf32.nearest: _doBinary(NI, NI),
  Wf32.sqrt: _doBinary(NI, NI),
  Wf32.add: _doBinary(NI, NI),
  Wf32.sub: _doBinary(NI, NI),
  Wf32.mul: _doBinary(NI, NI),
  Wf32.div: _doBinary(NI, NI),
  Wf32.min: _doBinary(NI, NI),
  Wf32.max: _doBinary(NI, NI),
  Wf32.copysign: _doBinary(NI, NI),
  Wf64.abs: _doBinary(NI, NI),
  Wf64.neg: _doBinary(NI, NI),
  Wf64.ceil: _doBinary(NI, NI),
  Wf64.floor: _doBinary(NI, NI),
  Wf64.trunc: _doBinary(NI, NI),
  Wf64.nearest: _doBinary(NI, NI),
  Wf64.sqrt: _doBinary(NI, NI),
  Wf64.add: _doBinary(NI, NI),
  Wf64.sub: _doBinary(NI, NI),
  Wf64.mul: _doBinary(NI, NI),
  Wf64.div: _doBinary(NI, NI),
  Wf64.min: _doBinary(NI, NI),
  Wf64.max: _doBinary(NI, NI),
  Wf64.copysign: _doBinary(NI, NI),
  Wi32.wrap_i64: _doBinary(NI, NI),
  Wi32.trunc_f32_s: _doBinary(NI, NI),
  Wi32.trunc_f32_u: _doBinary(NI, NI),
  Wi32.trunc_f64_s: _doBinary(NI, NI),
  Wi32.trunc_f64_u: _doBinary(NI, NI),
  Wi64.extend_i32_s: _doBinary(NI, NI),
  Wi64.extend_i32_u: _doBinary(NI, NI),
  Wi64.trunc_f32_s: _doBinary(NI, NI),
  Wi64.trunc_f32_u: _doBinary(NI, NI),
  Wi64.trunc_f64_s: _doBinary(NI, NI),
  Wi64.trunc_f64_u: _doBinary(NI, NI),
  Wf32.convert_i32_s: _doBinary(NI, NI),
  Wf32.convert_i32_u: _doBinary(NI, NI),
  Wf32.convert_i64_s: _doBinary(NI, NI),
  Wf32.convert_i64_u: _doBinary(NI, NI),
  Wf32.demote_f64: _doBinary(NI, NI),
  Wf64.convert_i32_s: _doBinary(NI, NI),
  Wf64.convert_i32_u: _doBinary(NI, NI),
  Wf64.convert_i64_s: _doBinary(NI, NI),
  Wf64.convert_i64_u: _doBinary(NI, NI),
  Wf64.promote_f32: _doBinary(NI, NI),
  Wi32.reinterpret_f32: _doBinary(NI, NI),
  Wi64.reinterpret_f64: _doBinary(NI, NI),
  Wf32.reinterpret_i32: _doBinary(NI, NI),
  Wf64.reinterpret_i64: _doBinary(NI, NI),
}
