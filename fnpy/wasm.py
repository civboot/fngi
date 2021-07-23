from .wasm_constants import *

class UnreachableError(Exception): pass

def _raiseUnreachable(*args):
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

def _memsize(env, _args):
    pages = len(env.memory.data) // 0x10000
    env.dataStack.push(I32(pages))

# TODO: this is only for little endian
def _1bytes(v): return U8(bytes(v)[:1])
def _2bytes(v): return U16(bytes(v)[:2])
def _4bytes(v): return U32(bytes(v)[:4])

def _bitand(l, r): return l & r
def _bitor(l, r): return l | r
def _bitxor(l,r): return l ^ r
def _inverse(l): return ~l

def rotr(ty, a, n):
    "Rotate a, n times to the right"
    width = sizeof(ty)
    if n > 0:
        mask = (1 << width) - 1
        a, n = a & mask, n % width
        return ((a >> n)    # top moved down
                | ((a & ((1 << n) - 1))   # Bottom masked...
                   << (width - n)))  # ... then moved up
    elif n == 0: return a
    else: return rotl(ty, a, -n)
 
def rotl(ty, a, n):
    "Rotate a, n times to the left"
    width = sizeof(ty)
    if n > 0:
        mask = (1 << width) - 1
        a, n = a & mask, n % width
        return (((a << n) & mask)      # bottom shifted up and masked
                | (a >> (width - n)))  # Top moved down
    elif n == 0: return a
    else: return rotr(width, a, -n)

wasmSubroutines = {
  w.unreachable: _raiseUnreachable,
  w.nop: lambda e,a: None,
  # w.block: NI,
  # w.loop: NI,
  # w.if_: NI,
  # w.else_: NI,
  # w.end: NI,
  # w.br: NI,
  # w.br_if: NI,
  # w.br_table: NI,
  # w.return_: NI,
  # w.call: NI,
  # w.call_indirect: NI,
  w.drop: lambda e,a: e.dataStack.drop(),
  w.select: lambda e,a: e.dataStack.select(),
  w.local.get: NI,
  w.local.set: NI,
  w.local.tee: NI,
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
  w.memory.grow: lambda e,a: env.heap.grow(a[0] * 0x10000),
  w.i32.const: lambda e,a: e.ds.push(I32(a[0])),
  w.i64.const: lambda e,a: e.ds.push(I64(a[0])),
  w.f32.const: lambda e,a: e.ds.push(F32(a[0])),
  w.f64.const: lambda e,a: e.ds.push(F64(a[0])),
  w.i32.eqz: _doUnary(I32, operator.not_),
  w.i32.eq: _doBinary(I32, operator.eq),
  w.i32.ne: _doBinary(I32, operator.ne),
  w.i32.lt_s: _doBinary(I32, operator.lt),
  w.i32.lt_u: _doBinCnv(I32, operator.lt, U32),
  w.i32.gt_s: _doBinary(I32, operator.gt),
  w.i32.gt_u: _doBinCnv(I32, operator.gt, U32),
  w.i32.le_s: _doBinary(I32, operator.le),
  w.i32.le_u: _doBinCnv(I32, operator.le, U32),
  w.i32.ge_s: _doBinary(I32, operator.ge),
  w.i32.ge_u: _doBinCnv(I32, operator.ge, U32),
  w.i64.eqz: _doUnary(I64, operator.not_),
  w.i64.eq: _doBinary(I64, operator.eq),
  w.i64.ne: _doBinary(I64, operator.ne),
  w.i64.lt_s: _doBinary(I64, operator.lt),
  w.i64.lt_u: _doBinCnv(I64, operator.lt, U64),
  w.i64.gt_s: _doBinary(I64, operator.gt),
  w.i64.gt_u: _doBinCnv(I64, operator.gt, U64),
  w.i64.le_s: _doBinary(I64, operator.le),
  w.i64.le_u: _doBinCnv(I64, operator.le, U64),
  w.i64.ge_s: _doBinary(I64, operator.ge),
  w.i64.ge_u: _doBinCnv(I64, operator.ge, U64),
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
  w.i32.clz: _doUnary(NI, NI),
  w.i32.ctz: _doUnary(NI, NI),
  w.i32.popcnt: _doBinary(NI, NI),
  w.i32.add: _doBinary(I32, operator.add),
  w.i32.sub: _doBinary(I32, operator.sub),
  w.i32.mul: _doBinary(I32, operator.mul),
  w.i32.div_s: _doBinary(I32, operator.floordiv),
  w.i32.div_u: _doBinary(I32, operator.floordiv),
  w.i32.rem_s: _doBinary(I32, operator.mod),
  w.i32.rem_u: _doBinary(I32, operator.mod),
  w.i32.and_: _doBinary(I32, _bitand),
  w.i32.or_: _doBinary(I32, _bitor),
  w.i32.xor: _doBinary(I32, _bitxor),
  w.i32.shl: _doBinary(I32, operator.lshift),
  w.i32.shr_s: _doBinary(I32, operator.rshift),
  w.i32.shr_u: _doBinary(I32, operator.rshift),
  w.i32.rotl: _doBinary(I32, rotl),
  w.i32.rotr: _doBinary(I32, rotr),
  w.i64.clz: _doUnary(NI, NI),
  w.i64.ctz: _doUnary(NI, NI),
  w.i64.popcnt: _doBinary(NI, NI),
  w.i64.add: _doBinary(NI, NI),
  w.i64.sub: _doBinary(NI, NI),
  w.i64.mul: _doBinary(NI, NI),
  w.i64.div_s: _doBinary(NI, NI),
  w.i64.div_u: _doBinary(NI, NI),
  w.i64.rem_s: _doBinary(NI, NI),
  w.i64.rem_u: _doBinary(NI, NI),
  w.i64.and_: _doBinary(NI, NI),
  w.i64.or_: _doBinary(NI, NI),
  w.i64.xor: _doBinary(NI, NI),
  w.i64.shl: _doBinary(NI, NI),
  w.i64.shr_s: _doBinary(NI, NI),
  w.i64.shr_u: _doBinary(NI, NI),
  w.i64.rotl: _doBinary(NI, NI),
  w.i64.rotr: _doBinary(NI, NI),
  w.f32.abs: _doBinary(NI, NI),
  w.f32.neg: _doBinary(NI, NI),
  w.f32.ceil: _doBinary(NI, NI),
  w.f32.floor: _doBinary(NI, NI),
  w.f32.trunc: _doBinary(NI, NI),
  w.f32.nearest: _doBinary(NI, NI),
  w.f32.sqrt: _doBinary(NI, NI),
  w.f32.add: _doBinary(NI, NI),
  w.f32.sub: _doBinary(NI, NI),
  w.f32.mul: _doBinary(NI, NI),
  w.f32.div: _doBinary(NI, NI),
  w.f32.min: _doBinary(NI, NI),
  w.f32.max: _doBinary(NI, NI),
  w.f32.copysign: _doBinary(NI, NI),
  w.f64.abs: _doBinary(NI, NI),
  w.f64.neg: _doBinary(NI, NI),
  w.f64.ceil: _doBinary(NI, NI),
  w.f64.floor: _doBinary(NI, NI),
  w.f64.trunc: _doBinary(NI, NI),
  w.f64.nearest: _doBinary(NI, NI),
  w.f64.sqrt: _doBinary(NI, NI),
  w.f64.add: _doBinary(NI, NI),
  w.f64.sub: _doBinary(NI, NI),
  w.f64.mul: _doBinary(NI, NI),
  w.f64.div: _doBinary(NI, NI),
  w.f64.min: _doBinary(NI, NI),
  w.f64.max: _doBinary(NI, NI),
  w.f64.copysign: _doBinary(NI, NI),
  w.i32.wrap_i64: _doBinary(NI, NI),
  w.i32.trunc_f32_s: _doBinary(NI, NI),
  w.i32.trunc_f32_u: _doBinary(NI, NI),
  w.i32.trunc_f64_s: _doBinary(NI, NI),
  w.i32.trunc_f64_u: _doBinary(NI, NI),
  w.i64.extend_i32_s: _doBinary(NI, NI),
  w.i64.extend_i32_u: _doBinary(NI, NI),
  w.i64.trunc_f32_s: _doBinary(NI, NI),
  w.i64.trunc_f32_u: _doBinary(NI, NI),
  w.i64.trunc_f64_s: _doBinary(NI, NI),
  w.i64.trunc_f64_u: _doBinary(NI, NI),
  w.f32.convert_i32_s: _doBinary(NI, NI),
  w.f32.convert_i32_u: _doBinary(NI, NI),
  w.f32.convert_i64_s: _doBinary(NI, NI),
  w.f32.convert_i64_u: _doBinary(NI, NI),
  w.f32.demote_f64: _doBinary(NI, NI),
  w.f64.convert_i32_s: _doBinary(NI, NI),
  w.f64.convert_i32_u: _doBinary(NI, NI),
  w.f64.convert_i64_s: _doBinary(NI, NI),
  w.f64.convert_i64_u: _doBinary(NI, NI),
  w.f64.promote_f32: _doBinary(NI, NI),
  w.i32.reinterpret_f32: _doBinary(NI, NI),
  w.i64.reinterpret_f64: _doBinary(NI, NI),
  w.f32.reinterpret_i32: _doBinary(NI, NI),
  w.f64.reinterpret_i64: _doBinary(NI, NI),
}
