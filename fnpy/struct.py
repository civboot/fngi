from dataclasses import dataclass
from .wasm import *
import inspect

@dataclass
class RefTy:
    ty: Union['Ty', Fn]

def fsizeof(ty: 'Ty'):
    if isinstance(ty, DataTy) or (
            inspect.isclass(ty) and issubclass(ty, DataTy)):
        return sizeof(ty)
    elif isinstance(ty, StructTy):
        return ty.size
    elif isinstance(ty, RefTy):
        return sizeof(Ptr)
    else:
        raise ValueError(f"Has no size: {ty}")

def alignField(offset: int, ty: "Ty"):
    """Calculate correct offset for dataTy."""
    size = fsizeof(ty)
    if size == 0: return offset
    alignment = min(4, size)
    if alignment == 3: alignment = 4

    mod = offset % alignment
    if mod == 0: return offset
    return offset + (alignment - mod)

def calcOffsetsAndSize(fields: List["Ty"]):
    offset = 0
    offsets = []
    for ty in fields:
        offset = alignField(offset, ty)
        offsets.append(offset)
        offset += fsizeof(ty)
    return offsets, offset

@dataclass
class Field:
    name: str
    ty: "Ty"
    index: int
    offset: int

def assertAllStkTypes(tys):
    for ty in tys:
        assert ty in {I32, I64, F32, F64}

class StructTy:
    """Create a struct data type from core types."""

    def __init__(self, fields: List[Tuple[str, DataTy]], isStk=False):
        self.isStk = isStk
        self.names = list(map(operator.itemgetter(0), fields))
        self.tys = list(map(operator.itemgetter(1), fields))
        self.offsets, self.size = calcOffsetsAndSize(self.tys)
        self.fields = {}
        for i in range(len(self.names)):
            name = self.names[i]
            field = Field(
                name=name,
                ty=self.tys[i],
                index=i,
                offset=self.offsets[i],
            )
            key = name
            if key is None:
                assert isStk
                key = i
            self.fields[key] = field

        if isStk: assertAllStkTypes(self.tys)

    def field(self, key: str):
        """Return the field given a '.' separated key.

        For use with nested structs.
        """
        key = _prepareKey(key)
        st = self
        field = None
        try:
            for k in key:
                field = st.fields[k]
                st = field.ty
        except KeyError as e:
            raise KeyError(f"{key}: {e}")
        return field

    def offset(self, key: str):
        """Return the offset of a field given a '.' separated key.

        The offset is calculated with respect to self (the "base" struct).
        """
        key = _prepareKey(key)
        st = self
        offset = 0
        try:
            for k in key:
                field = st.fields[k]
                offset += field.offset
                st = field.ty
        except KeyError as e:
            raise KeyError(f"{key}: {e}")
        return offset

    def ty(self, key: str):
        return self.field(key).ty

    def __getitem__(self, item):
        return self.fields[item]


def _prepareSubKey(k):
    if isinstance(k, int): return k
    k = k.strip()
    try: k = int(k)
    except ValueError: pass
    return k

def _prepareKey(key):
    if isinstance(key, str):
        key = key.split('.')
    elif isinstance(key, int):
        key = [key]
    return list(map(_prepareSubKey, key))


def testStruct():
    aFields = [
        ('a1', U32),
        ('a2', U8),
        ('a3', U16),
        ('a4', U16),
        ('a5', U64),
        ('a6', U8),
    ]
    a = StructTy(aFields)
    assert 0 == a['a1'].offset
    assert 4 == a['a2'].offset
    assert 6 == a['a3'].offset
    assert 8 == a['a4'].offset
    assert 12 == a['a5'].offset
    assert 20 == a['a6'].offset
    assert 21 == a.size

    bFields = [
        ('u8', U8),
        ('a', a),
        ('u32', U32),
    ]

    b = StructTy(bFields)
    assert 0 == b['u8'].offset
    assert 4 == b['a'].offset
    assert 4 == b.offset('a.a1')
    assert 8 == b.offset('a.a2')
    assert 28 == b['u32'].offset
    assert 32 == b.size

    class C_A(ctypes.Structure):
        _fields_ = [
            ('a1', U32),
            ('a2', U8),
            ('a3', U16),
            ('a4', U16),
            # Note: a5 must be split into two U32's. Python's ctypes is running
            # in 64bit mode which has a maxalign of 8 bytes
            ('a5', U32),
            ('a5_', U32),
            ('a6', U8),
        ]

    assert C_A.a1.offset == a['a1'].offset
    assert C_A.a2.offset == a['a2'].offset
    assert C_A.a3.offset == a['a3'].offset
    assert C_A.a4.offset == a['a4'].offset
    assert C_A.a5.offset == a['a5'].offset
    assert C_A.a6.offset == a['a6'].offset

    class C_B(ctypes.Structure):
        _fields_ = [
            ('u8', U8),
            ('a', C_A),
            ('u32', U32),
        ]

    assert C_B.u8.offset == b['u8'].offset
    assert C_B.a.offset == b['a'].offset
    assert C_B.u32.offset == b['u32'].offset


Void = StructTy([])


class FnStructTy(StructTy):
    """A struct specifically for keeping track of function data. Has fields:
    - wasmTrueLocals: the wasm inputs+locals which are accessed by index.
    - inp: (non stk) input type to the fn.
    - &ret: (non stk) return type from the fn.
    - locals: (non wasm) local variables in the function.

    All of the above types are themselves structs.
    """
    def __init__(
            self,
            wasmTrueLocals: List[DataTy],
            inp: StructTy,
            ret: RefTy,
            locals_: StructTy):
        wasmTrueLocals = StructTy(
            fields=[(None, ty) for ty in wasmTrueLocals],
            isStk=True)
        fields = [
            ('wasmTrueLocals', wasmTrueLocals),
            ('inp', inp),
            ('ret', ret),
            ('locals', locals_),
        ]
        return super().__init__(fields)

Ty = Union[DataTy, StructTy, RefTy]
