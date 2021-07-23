# The following *Index values are copied directly from
# https://www.w3.org/TR/wasm-core-1/#a6-index-of-types
#
# This file generates fnpy/wasm_constants.py

import collections

typesIndex = '''
Category	Constructor	Binary Opcode
Type index	x	(positive number as s32 or u32)
Value type	i32	0x7F (-1 as s7)
Value type	i64	0x7E (-2 as s7)
Value type	f32	0x7D (-3 as s7)
Value type	f64	0x7C (-4 as s7)
(reserved)		0x7B .. 0x71
Element type	funcref	0x70 (-16 as s7)
(reserved)		0x6F .. 0x61
Function type	[valtype ∗ ]→[valtype ∗ ]	0x60 (-32 as s7)
(reserved)		0x5F .. 0x41
Result type	[ϵ]	0x40 (-64 as s7)
Table type	limits elemtype	(none)
Memory type	limits	(none)
Global type	mut valtype	(none)
'''


instructionsIndex = '''
Instruction	Binary Opcode	Type	Validation	Execution
unreachable	0x00	[t 1 ∗ ​	]→[t 2 ∗ ​	]	validation	execution
nop	0x01	[]→[]	validation	execution
block [t ?  ]	0x02	[]→[t ∗ ]	validation	execution
loop [t ?  ]	0x03	[]→[t ∗ ]	validation	execution
if [t ?  ]	0x04	[i32]→[t ∗ ]	validation	execution
else	0x05			
(reserved)	0x06			
(reserved)	0x07			
(reserved)	0x08			
(reserved)	0x09			
(reserved)	0x0A			
end	0x0B			
br l	0x0C	[t 1 ∗ ​	t ?  ]→[t 2 ∗ ​	]	validation	execution
br_if l	0x0D	[t ?  i32]→[t ?  ]	validation	execution
br_table l ∗ l	0x0E	[t 1 ∗ ​	t ?  i32]→[t 2 ∗ ​	]	validation	execution
return	0x0F	[t 1 ∗ ​	t ?  ]→[t 2 ∗ ​	]	validation	execution
call x	0x10	[t 1 ∗ ​	]→[t 2 ∗ ​	]	validation	execution
call_indirect x	0x11	[t 1 ∗ ​	i32]→[t 2 ∗ ​	]	validation	execution
(reserved)	0x12			
(reserved)	0x13			
(reserved)	0x14			
(reserved)	0x15			
(reserved)	0x16			
(reserved)	0x17			
(reserved)	0x18			
(reserved)	0x19			
drop	0x1A	[t]→[]	validation	execution
select	0x1B	[t t i32]→[t]	validation	execution
(reserved)	0x1C			
(reserved)	0x1D			
(reserved)	0x1E			
(reserved)	0x1F			
local.get x	0x20	[]→[t]	validation	execution
local.set x	0x21	[t]→[]	validation	execution
local.tee x	0x22	[t]→[t]	validation	execution
global.get x	0x23	[]→[t]	validation	execution
global.set x	0x24	[t]→[]	validation	execution
(reserved)	0x25			
(reserved)	0x26			
(reserved)	0x27			
i32.load memarg	0x28	[i32]→[i32]	validation	execution
i64.load memarg	0x29	[i32]→[i64]	validation	execution
f32.load memarg	0x2A	[i32]→[f32]	validation	execution
f64.load memarg	0x2B	[i32]→[f64]	validation	execution
i32.load8_s memarg	0x2C	[i32]→[i32]	validation	execution
i32.load8_u memarg	0x2D	[i32]→[i32]	validation	execution
i32.load16_s memarg	0x2E	[i32]→[i32]	validation	execution
i32.load16_u memarg	0x2F	[i32]→[i32]	validation	execution
i64.load8_s memarg	0x30	[i32]→[i64]	validation	execution
i64.load8_u memarg	0x31	[i32]→[i64]	validation	execution
i64.load16_s memarg	0x32	[i32]→[i64]	validation	execution
i64.load16_u memarg	0x33	[i32]→[i64]	validation	execution
i64.load32_s memarg	0x34	[i32]→[i64]	validation	execution
i64.load32_u memarg	0x35	[i32]→[i64]	validation	execution
i32.store memarg	0x36	[i32 i32]→[]	validation	execution
i64.store memarg	0x37	[i32 i64]→[]	validation	execution
f32.store memarg	0x38	[i32 f32]→[]	validation	execution
f64.store memarg	0x39	[i32 f64]→[]	validation	execution
i32.store8 memarg	0x3A	[i32 i32]→[]	validation	execution
i32.store16 memarg	0x3B	[i32 i32]→[]	validation	execution
i64.store8 memarg	0x3C	[i32 i64]→[]	validation	execution
i64.store16 memarg	0x3D	[i32 i64]→[]	validation	execution
i64.store32 memarg	0x3E	[i32 i64]→[]	validation	execution
memory.size	0x3F	[]→[i32]	validation	execution
memory.grow	0x40	[i32]→[i32]	validation	execution
i32.const i32	0x41	[]→[i32]	validation	execution
i64.const i64	0x42	[]→[i64]	validation	execution
f32.const f32	0x43	[]→[f32]	validation	execution
f64.const f64	0x44	[]→[f64]	validation	execution
i32.eqz	0x45	[i32]→[i32]	validation	execution, operator
i32.eq	0x46	[i32 i32]→[i32]	validation	execution, operator
i32.ne	0x47	[i32 i32]→[i32]	validation	execution, operator
i32.lt_s	0x48	[i32 i32]→[i32]	validation	execution, operator
i32.lt_u	0x49	[i32 i32]→[i32]	validation	execution, operator
i32.gt_s	0x4A	[i32 i32]→[i32]	validation	execution, operator
i32.gt_u	0x4B	[i32 i32]→[i32]	validation	execution, operator
i32.le_s	0x4C	[i32 i32]→[i32]	validation	execution, operator
i32.le_u	0x4D	[i32 i32]→[i32]	validation	execution, operator
i32.ge_s	0x4E	[i32 i32]→[i32]	validation	execution, operator
i32.ge_u	0x4F	[i32 i32]→[i32]	validation	execution, operator
i64.eqz	0x50	[i64]→[i32]	validation	execution, operator
i64.eq	0x51	[i64 i64]→[i32]	validation	execution, operator
i64.ne	0x52	[i64 i64]→[i32]	validation	execution, operator
i64.lt_s	0x53	[i64 i64]→[i32]	validation	execution, operator
i64.lt_u	0x54	[i64 i64]→[i32]	validation	execution, operator
i64.gt_s	0x55	[i64 i64]→[i32]	validation	execution, operator
i64.gt_u	0x56	[i64 i64]→[i32]	validation	execution, operator
i64.le_s	0x57	[i64 i64]→[i32]	validation	execution, operator
i64.le_u	0x58	[i64 i64]→[i32]	validation	execution, operator
i64.ge_s	0x59	[i64 i64]→[i32]	validation	execution, operator
i64.ge_u	0x5A	[i64 i64]→[i32]	validation	execution, operator
f32.eq	0x5B	[f32 f32]→[i32]	validation	execution, operator
f32.ne	0x5C	[f32 f32]→[i32]	validation	execution, operator
f32.lt	0x5D	[f32 f32]→[i32]	validation	execution, operator
f32.gt	0x5E	[f32 f32]→[i32]	validation	execution, operator
f32.le	0x5F	[f32 f32]→[i32]	validation	execution, operator
f32.ge	0x60	[f32 f32]→[i32]	validation	execution, operator
f64.eq	0x61	[f64 f64]→[i32]	validation	execution, operator
f64.ne	0x62	[f64 f64]→[i32]	validation	execution, operator
f64.lt	0x63	[f64 f64]→[i32]	validation	execution, operator
f64.gt	0x64	[f64 f64]→[i32]	validation	execution, operator
f64.le	0x65	[f64 f64]→[i32]	validation	execution, operator
f64.ge	0x66	[f64 f64]→[i32]	validation	execution, operator
i32.clz	0x67	[i32]→[i32]	validation	execution, operator
i32.ctz	0x68	[i32]→[i32]	validation	execution, operator
i32.popcnt	0x69	[i32]→[i32]	validation	execution, operator
i32.add	0x6A	[i32 i32]→[i32]	validation	execution, operator
i32.sub	0x6B	[i32 i32]→[i32]	validation	execution, operator
i32.mul	0x6C	[i32 i32]→[i32]	validation	execution, operator
i32.div_s	0x6D	[i32 i32]→[i32]	validation	execution, operator
i32.div_u	0x6E	[i32 i32]→[i32]	validation	execution, operator
i32.rem_s	0x6F	[i32 i32]→[i32]	validation	execution, operator
i32.rem_u	0x70	[i32 i32]→[i32]	validation	execution, operator
i32.and	0x71	[i32 i32]→[i32]	validation	execution, operator
i32.or	0x72	[i32 i32]→[i32]	validation	execution, operator
i32.xor	0x73	[i32 i32]→[i32]	validation	execution, operator
i32.shl	0x74	[i32 i32]→[i32]	validation	execution, operator
i32.shr_s	0x75	[i32 i32]→[i32]	validation	execution, operator
i32.shr_u	0x76	[i32 i32]→[i32]	validation	execution, operator
i32.rotl	0x77	[i32 i32]→[i32]	validation	execution, operator
i32.rotr	0x78	[i32 i32]→[i32]	validation	execution, operator
i64.clz	0x79	[i64]→[i64]	validation	execution, operator
i64.ctz	0x7A	[i64]→[i64]	validation	execution, operator
i64.popcnt	0x7B	[i64]→[i64]	validation	execution, operator
i64.add	0x7C	[i64 i64]→[i64]	validation	execution, operator
i64.sub	0x7D	[i64 i64]→[i64]	validation	execution, operator
i64.mul	0x7E	[i64 i64]→[i64]	validation	execution, operator
i64.div_s	0x7F	[i64 i64]→[i64]	validation	execution, operator
i64.div_u	0x80	[i64 i64]→[i64]	validation	execution, operator
i64.rem_s	0x81	[i64 i64]→[i64]	validation	execution, operator
i64.rem_u	0x82	[i64 i64]→[i64]	validation	execution, operator
i64.and	0x83	[i64 i64]→[i64]	validation	execution, operator
i64.or	0x84	[i64 i64]→[i64]	validation	execution, operator
i64.xor	0x85	[i64 i64]→[i64]	validation	execution, operator
i64.shl	0x86	[i64 i64]→[i64]	validation	execution, operator
i64.shr_s	0x87	[i64 i64]→[i64]	validation	execution, operator
i64.shr_u	0x88	[i64 i64]→[i64]	validation	execution, operator
i64.rotl	0x89	[i64 i64]→[i64]	validation	execution, operator
i64.rotr	0x8A	[i64 i64]→[i64]	validation	execution, operator
f32.abs	0x8B	[f32]→[f32]	validation	execution, operator
f32.neg	0x8C	[f32]→[f32]	validation	execution, operator
f32.ceil	0x8D	[f32]→[f32]	validation	execution, operator
f32.floor	0x8E	[f32]→[f32]	validation	execution, operator
f32.trunc	0x8F	[f32]→[f32]	validation	execution, operator
f32.nearest	0x90	[f32]→[f32]	validation	execution, operator
f32.sqrt	0x91	[f32]→[f32]	validation	execution, operator
f32.add	0x92	[f32 f32]→[f32]	validation	execution, operator
f32.sub	0x93	[f32 f32]→[f32]	validation	execution, operator
f32.mul	0x94	[f32 f32]→[f32]	validation	execution, operator
f32.div	0x95	[f32 f32]→[f32]	validation	execution, operator
f32.min	0x96	[f32 f32]→[f32]	validation	execution, operator
f32.max	0x97	[f32 f32]→[f32]	validation	execution, operator
f32.copysign	0x98	[f32 f32]→[f32]	validation	execution, operator
f64.abs	0x99	[f64]→[f64]	validation	execution, operator
f64.neg	0x9A	[f64]→[f64]	validation	execution, operator
f64.ceil	0x9B	[f64]→[f64]	validation	execution, operator
f64.floor	0x9C	[f64]→[f64]	validation	execution, operator
f64.trunc	0x9D	[f64]→[f64]	validation	execution, operator
f64.nearest	0x9E	[f64]→[f64]	validation	execution, operator
f64.sqrt	0x9F	[f64]→[f64]	validation	execution, operator
f64.add	0xA0	[f64 f64]→[f64]	validation	execution, operator
f64.sub	0xA1	[f64 f64]→[f64]	validation	execution, operator
f64.mul	0xA2	[f64 f64]→[f64]	validation	execution, operator
f64.div	0xA3	[f64 f64]→[f64]	validation	execution, operator
f64.min	0xA4	[f64 f64]→[f64]	validation	execution, operator
f64.max	0xA5	[f64 f64]→[f64]	validation	execution, operator
f64.copysign	0xA6	[f64 f64]→[f64]	validation	execution, operator
i32.wrap_i64	0xA7	[i64]→[i32]	validation	execution, operator
i32.trunc_f32_s	0xA8	[f32]→[i32]	validation	execution, operator
i32.trunc_f32_u	0xA9	[f32]→[i32]	validation	execution, operator
i32.trunc_f64_s	0xAA	[f64]→[i32]	validation	execution, operator
i32.trunc_f64_u	0xAB	[f64]→[i32]	validation	execution, operator
i64.extend_i32_s	0xAC	[i32]→[i64]	validation	execution, operator
i64.extend_i32_u	0xAD	[i32]→[i64]	validation	execution, operator
i64.trunc_f32_s	0xAE	[f32]→[i64]	validation	execution, operator
i64.trunc_f32_u	0xAF	[f32]→[i64]	validation	execution, operator
i64.trunc_f64_s	0xB0	[f64]→[i64]	validation	execution, operator
i64.trunc_f64_u	0xB1	[f64]→[i64]	validation	execution, operator
f32.convert_i32_s	0xB2	[i32]→[f32]	validation	execution, operator
f32.convert_i32_u	0xB3	[i32]→[f32]	validation	execution, operator
f32.convert_i64_s	0xB4	[i64]→[f32]	validation	execution, operator
f32.convert_i64_u	0xB5	[i64]→[f32]	validation	execution, operator
f32.demote_f64	0xB6	[f64]→[f32]	validation	execution, operator
f64.convert_i32_s	0xB7	[i32]→[f64]	validation	execution, operator
f64.convert_i32_u	0xB8	[i32]→[f64]	validation	execution, operator
f64.convert_i64_s	0xB9	[i64]→[f64]	validation	execution, operator
f64.convert_i64_u	0xBA	[i64]→[f64]	validation	execution, operator
f64.promote_f32	0xBB	[f32]→[f64]	validation	execution, operator
i32.reinterpret_f32	0xBC	[f32]→[i32]	validation	execution, operator
i64.reinterpret_f64	0xBD	[f64]→[i64]	validation	execution, operator
f32.reinterpret_i32	0xBE	[i32]→[f32]	validation	execution, operator
f64.reinterpret_i64	0xBF	[i64]→[f64]	validation	execution, operator
i32.extend8_s	0xC0	[i32]→[i32]	validation	execution, operator
i32.extend16_s	0xC1	[i32]→[i32]	validation	execution, operator
i64.extend8_s	0xC2	[i64]→[i64]	validation	execution, operator
i64.extend16_s	0xC3	[i64]→[i64]	validation	execution, operator
i64.extend32_s	0xC4	[i64]→[i64]	validation	execution, operator
ref.null t	0xD0	[]→[t]	validation	execution
ref.is_null	0xD1	[t]→[i32]	validation	execution
ref.func x	0xD2	[]→[funcref]	validation	execution
0xFB i32.trunc_sat_f32_s	0xFC	0x00	[f32]→[i32]	validation execution, operator
i32.trunc_sat_f32_u	0xFC	0x01	[f32]→[i32]	validation execution, operator
i32.trunc_sat_f64_s	0xFC	0x02	[f64]→[i32]	validation execution, operator
i32.trunc_sat_f64_u	0xFC	0x03	[f64]→[i32]	validation execution, operator
i64.trunc_sat_f32_s	0xFC	0x04	[f32]→[i64]	validation execution, operator
i64.trunc_sat_f32_u	0xFC	0x05	[f32]→[i64]	validation execution, operator
i64.trunc_sat_f64_s	0xFC	0x06	[f64]→[i64]	validation execution, operator
i64.trunc_sat_f64_u	0xFC	0x07	[f64]→[i64]	validation execution, operator
memory.init x	0xFC	0x08	[i32	i32 i32]→[] validation execution
data.drop x	0xFC	0x09	[]→[]	validation execution
memory.copy	0xFC	0x0A	[i32	i32 i32]→[] validation execution
memory.fill	0xFC	0x0B	[i32	i32 i32]→[] validation execution
table.init x y	0xFC	0x0C	[i32	i32 i32]→[] validation execution
elem.drop x	0xFC	0x0D	[]→[]	validation execution
table.copy x y	0xFC	0x0E	[i32	i32 i32]→[] validation execution 
table.grow x	0xFC	0x0F	[t	i32]→[] validation execution 
table.size x	0xFC	0x10	[]→[i32]	validation execution
table.fill x	0xFC	0x11	[i32	t i32]→[] validation execution
'''

def parseIndex(index: str, getName):
    lines = iter(index.split('\n'))
    next(lines) # discard empty line
    header = next(lines).split('\t')
    binaryIndex = header.index('Binary Opcode')

    out = collections.OrderedDict()
    for line in lines:
        row = line.split('\t')
        if len(row) <= 1: continue
        name = getName(row)
        valueStr = row[binaryIndex]
        if valueStr == '(none)': continue

        if valueStr == '(positive number as s32 or u32)':
            valueStr = '0x10ad' # leetspeak for "LOAD", "10" looks like "lO"
        else:
            valueStr = valueStr.split()[0]
        value = int(valueStr, 16)
        if not name or name == '(reserved)': continue
        assert name not in out
        out[name] = value

    return out

def getTypesName(row):
    category = row[0]
    constructor = row[1]
    if not constructor: return ''
    elif category == '(reserved)': return ''
    elif category == 'Type index': return 'idx'
    elif category == 'Value type': return constructor
    else: return category.split()[0].lower()

wasmTypes = parseIndex(typesIndex, getTypesName)
wasmInstructions = parseIndex(
    instructionsIndex, lambda r: r[0].split()[0])

PY_FILE_START = r'''
# This file is automatically generated using tools/generate_wasm_constants.py
# Don't edit it manually.
# Constants are copy+pasted directly from the WebAssembly spec located at:
# https://www.w3.org/TR/wasm-core-1/#a6-index-of-types
from collections import OrderedDict
from dataclasses import dataclass
import copy
import ctypes
import operator

# This is the only way to get the base class for ctypes (_CData) which is
# technically private.
DataTy = ctypes.c_uint8.__bases__[0].__bases__[0]

# Used for classifying things into "modules"
class _Namespace: pass

# The webassembly namespace
w = _Namespace()

from ctypes import c_bool as Bool
'''

WASM_SUBROUTINE_INTRO = r'''
# Wasm subroutines operate on the Env (e) object given the args (a) from the
# wasm instr.
#
# None of these involve break/loop/etc as those all happen in machine.run
'''

LOAD_KEY_TEMPLATE = '{ns}.load{post}'.format
LOAD_VAL_TEMPLATE = 'lambda e,a: _loadValue(e, a, {ty})'.format

STORE_KEY_TEMPLATE = '{ns}.store{post}'.format
STORE_VAL_TEMPLATE = 'lambda e,a: _storeValue(e, a, {ty})'.format

BINARY_FLOAT = [
    'add', 'sub', 'mul', 'div',
    'eq', 'ne', 'lt', 'gt', 'le', 'ge',
]


BINARY_INT = [
    'add', 'sub', 'mul', 'div_s', 'div_u',
    'rem_s', 'rem_u',
    'and_', 'or_', 'xor',
    'shl', 'shr_s', 'shr_u',
    'rotl', 'rotr',
    'eq', 'ne',
    'lt_s', 'lt_u',
    'gt_s', 'gt_u',
    'le_s', 'le_u',
    'ge_s', 'ge_u',
]

INSTR_INT = [
  'eqz',
  'clz',
  'ctz',
]

INSTR_FLOAT = [
  'nearest',
  'sqrt',
  'convert_i32_s',
  'convert_i32_u',
  'convert_i64_s',
  'convert_i64_u',
]

INSTR_SPECIAL = [ # only one
  'f32.demote_f64',
  'i32.wrap_i64',
  'f64.promote_f32',
  'i64.extend_i32_s',
  'i64.extend_i32_u',
  'i32.reinterpret_f32',
  'i64.reinterpret_f64',
  'f32.reinterpret_i32',
  'f64.reinterpret_i64',
]


def _subLoadStore(subs, ns, ty):
    subs[LOAD_KEY_TEMPLATE(ns=ns, post='')] = LOAD_VAL_TEMPLATE(ty=ty)
    subs[STORE_KEY_TEMPLATE(ns=ns, post='')] = STORE_VAL_TEMPLATE(ty=ty)
    if ty in ('F32', 'F64'): return
    numBits = int(ty[1:])

    for bits in 8, 16, 32:
        if numBits <= bits: break
        for post in ('_s', '_u'):
            bitTy = ('I' if post == '_s' else 'U') + str(bits)
            post = str(bits) + post
            subs[LOAD_KEY_TEMPLATE(ns=ns, post=post)] = LOAD_VAL_TEMPLATE(ty=bitTy)
            subs[STORE_KEY_TEMPLATE(ns=ns, post=post)] = STORE_VAL_TEMPLATE(ty=bitTy)


BINARY_KEY_TEMPLATE = '{ns}.{binary}'.format

def _subBinary(subs, ns, ty):
    if ty[0] == 'F': binarys = BINARY_FLOAT
    else: binarys = BINARY_INT
    for b in binarys:
        key = BINARY_KEY_TEMPLATE(ns, binary)


def writeWasmSubroutines(f):
    subs = {}
    for ty in nativeTys:
        ns = ty.lower()
        subs[f'{ns}.const'] = f'lambda e,a: e.ds.push({ty}(a[0]))'
        _subLoadStore(subs, ns, ty)

    f.write(WASM_SUBROUTINE_INTRO)
    f.write('wasmSubroutines = {\n')
    for key in wasmInstructions:
        if key not in subs: continue
        value = subs[key]
        f.write(f'  w.{key}: {value},\n')
    f.write('}\n')


nativeIntTys = ['I32', 'I64']
nativeFloatTys = ['F32', 'F64']
nativeTys = nativeIntTys + nativeFloatTys

if __name__ == '__main__':
    import sys
    fpath = 'fnpy/wasm_constants.py'
    print("Generating", fpath)
    f = open(fpath, 'w')
    f.write(PY_FILE_START[1:])
    for ctype, fnty in [('uint', 'U'), ('int', 'I')]:
        for size in [8, 16, 32, 64]:
            f.write(f"from ctypes import c_{ctype}{size} as {fnty}{size}\n")
    for ctype, fnty in [('float', 'F32'), ('double', 'F64')]:
        f.write(f"from ctypes import c_{ctype} as {fnty}\n")
    f.write('\n')

    for ns in ('types', 'local', 'global_', 'i32', 'i64', 'f32', 'f64', 'memory'):
        f.write(f'w.{ns} = _Namespace()\n')

    def keywordReplace(s):
        out = s.split('.')
        fix = {'and', 'or', 'if', 'else', 'return', 'global', }
        for i in range(len(out)):
            if out[i] in fix:
                out[i] = out[i] + '_'

        return '.'.join(out)

    writeKeyVal = lambda item: f.write(
        f"{keywordReplace('w.' + item[0])} = {hex(item[1])}\n")

    f.write("\n# A.6 Index of Types\n")
    for key, value in wasmTypes.items():
        key = 'types.' + key
        writeKeyVal((key, value))

    f.write("\n# A.7 Index of Instructions\n")
    for item in wasmInstructions.items():
        writeKeyVal(item)

    f.write("\n# Name Lookup Dict (for testing)\n")
    f.write("wasmName = {\n")
    for item in wasmInstructions.items():
        f.write(f"  {keywordReplace('w.' + item[0])}: '{item[0]}',\n")
    f.write('}\n')

    f.write("\n# Code Lookup Dict (for testing)\n")
    f.write("wasmCode = {\n")
    for item in wasmInstructions.items():
        f.write(f"  '{item[0]}': {keywordReplace('w.' + item[0])},\n")
    f.write('}\n')

    # writeWasmSubroutines(f)
    # allIntTys = ['U8', 'U16', 'U32', 'U64', 'I8', 'I16'] + nativeIntTys
