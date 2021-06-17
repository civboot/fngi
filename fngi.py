import abc
import sys
import struct
import ctypes

from typing import Any
from typing import ByteString
from typing import Callable
from typing import List
from typing import Tuple
from typing import Dict

def needAlign(size: int) -> int:
    if size % 4 == 0:
        return 0

    return 4 - (size % 4)

class Ty(object):
    """The base type that all fngi types derive from.

    Notice that when instantiated, all types will automatically register with
    the global TYS dictionary.
    """
    def __init__(self):
        super().__init__()

    def name(self):
        return self.__class__.__name__

TYS: Dict[str, Ty] = {} # Global registry


class DataTy(ctypes.Structure, Ty):
    """A type for data represented in memory.

    Reading/Writing a DataType (dt) to a bytearray (ba):
    - reading => dt = MyDataType.from_buffer(ba)
    - writing => ba[start:start+dt.size()] = dt
    """
    @classmethod
    def size(cls):
        return ctypes.sizeof(cls)

    @classmethod
    def fieldOffset(cls, field: str):
        return getattr(cls, field).offset

    @classmethod
    def fieldSize(cls, field: str):
        return getattr(cls, field).size

# Fungi uses 32 bits for its pointers.
ctype_ptr = ctypes.c_uint32

def createCoreTy(name, cty):
    """Creates a core data type and registers it.

    This is soley to reduce boilerplace. It is the same as:

        class {{name}}(DataTy): _fields_ =  [('v', {{cty}})]
        TYS["{{name}}"] = {{name}}
    """
    nativeTy = type(name, tuple([DataTy]), {'_fields_': [('v', cty)]})
    TYS[name] = nativeTy
    return nativeTy


I8 = createCoreTy("I8",  ctypes.c_int8)
U8 = createCoreTy("U8",  ctypes.c_uint8)
I16 = createCoreTy("I16", ctypes.c_int16)
U16 = createCoreTy("U16", ctypes.c_uint16)
I32 = createCoreTy("I32", ctypes.c_int32)
U32 = createCoreTy("U32", ctypes.c_uint32)
I64 = createCoreTy("I64", ctypes.c_int64)
U64 = createCoreTy("U64", ctypes.c_uint64)
Ptr = createCoreTy("Ptr", ctype_ptr) # an address in memory, no type
Ref = createCoreTy("Ref", ctype_ptr) # an address in memory to a type


class FnTy(Ty):
    def __init__(self, name: str, inputs: List[Ty], outputs: List[Ty]):
        super().__init__()
        self.name, self.inputs, self.outputs = name, inputs, outputs


class RefTy(DataTy):
    def __init__(self, ref: Ty):
        self.ref = ref


class Fn(abc.ABC):
    """A function has a type and starts execution at an index.

    The runtime will appropriately execute the body within the function by
    incrementing the index, except for control structures which will have
    other behavior.

    Notice that all created Fns will automatically register with the global FNS
    registry.
    """
    def __init__(self, name: str, ty: FnTy):
        self.name, self.ty = name, ty
        FNS[name] = self

    @abc.abstractmethod
    def exec(self):
        raise TypeError("Called exec on a non-native fn")

    @abc.abstractmethod
    def fnIndex(self):
        raise TypeError("Called fnIndex on a non-user fn")

FNS: Dict[str, Fn] = {} # Global Registry


class NativeFn(Fn):
    """A native function. Must implement the call method."""

    def fnIndex(self):
        raise TypeError("Called fnIndex on a non-user fn")


class UserFn(Fn):
    def __init__(self, name: str, ty: FnTy, fnIndex: int):
        super().__init__(name, ty)
        self._fnIndex = fnIndex
        FN_INDEX_LOOKUP[fnIndex] = self

    def fnIndex(self):
        return self._fnIndex


def bytesReplace(b: bytearray, value: int, start: int, end: int):
    """Replace bytes in range with value"""
    for i in range(start, end):
        b[i] = value


class Stack(object):
    """The core stack type for storing data of various kinds.

    This is the memory that interpreted fngi programs access. It is a simply a
    bytearray, with fngi structures being C-like. The stacks are also used for
    interfacing between python and fngi programs.
    """
    def __init__(self, initialSize):
        self.data = bytearray(initialSize)
        self.sp = len(self.data)

    def checkRange(self, index, size):
        if index < 0 or index + size > len(self.data):
            raise IndexError("index={} size={} stack_size={}".format(index, size, len(self.data)))

    # Set / Push
    def set(self, index: int, value: ByteString):
        size = value.size()
        self.checkRange(self, index, size)
        self.data[index:index + size] = value

    def push(self, value: ByteString, align=True):
        size = value.size()
        if align: 
            size += needAlign(size)
        self.checkRange(self.sp - size, size)
        self.sp -= size
        # Note: DON'T use self.sp+size for the second slice value, as it may
        # shorten the bytearray.
        self.data[self.sp:self.sp + value.size()] = value

    # Get / Pop

    def get(self, index, size) -> bytes:
        self.checkRange(index, size)
        return self.data[index:index+size]

    def popSize(self, size) -> bytes:
        self.checkRange(self.sp, size)
        out = self.get(self.sp, size)
        self.sp += size
        return out

    def pop(self, ty: DataTy, align=False) -> DataTy:
        size = ty.size()
        if align:
            size += needAlign(size)
        return ty.from_buffer(self.popSize(size))

    def __len__(self):
        return len(self.data) - self.sp

    def __sizeof__(self):
        return len(self)

    def __repr__(self):
        return "STACK<{}/{}>".format(len(self), len(self.data))


##############################################################################
# The Global Environment and Execution Loop
#
# These are presented first so that it is clear how simple things are. Experienced
# programmers may notice the similarities to the execution model of Forth, which
# is intentional, because fngi will eventually be implemented in Forth. This (python)
# implementation is for prototyping and to provide a reference/learning manual for the
# stage0 fngi language. It will be kept up-to-date with the stage0 forth compiler.
#
# For the execution model, there are two types of Fn (function), NativeFn which
# are implemented in python and UserFn which are simply a range of indexes
# inside of the EXECS global variable; which are run by execLoop.
#
# Although functions have types (i.e. inputs/outputs), the types are only
# checked at compile time. At execution time, functions pop values off of the
# DATA_STACK for their parameters and push values on the DATA_STACK for their
# results. They also use the RET_STACK and BSP for keeping track of local
# variables.

# Global Environment
RUNNING = False # must stay true for execLoop to keep running.

STACK_SIZE = 10 * 2**20 # 10 MiB
# Runtime
DATA_STACK = Stack(32) # data stack is intentionally limited
BSP = 0
RET_STACK = Stack(STACK_SIZE)

# Compiletime
FN_INDEX_LOOKUP: Dict[int, Fn] = {}
EXECS: List[Fn]
FN_INDEX = 0

TYPE_STACK = Stack(STACK_SIZE)
DEFER_STACK = Stack(STACK_SIZE)
LOCALS: DataTy = None

def execLoop():
    """Yup, this is the entire exec loop."""
    while RUNNING:
        fn = EXECS[FN_INDEX]
        if isinstance(fn, NativeFn):
            # If native, run the function
            FN_INDEX += 1
            fn.call()
        else:
            # If not native, store the next index on the call stack
            callStack.pushValue(u32, FN_INDEX + 1)
            # and "run" the function by changing the FN_INDEX to be it
            FN_INDEX = fn.fnIndex()

# Native Functions
#
# Now we can define native functions by instantiating a FnTy and
# subclassing from NativeFn. Since a large majority of NativeFn's can
# be defined as a single python function with some inputs/outputs we're going
# to make a python decorator to reduce boilerplate.

def nativeFn(
    inputs: List[Ty],
    outputs: List[Ty]
    ):

    def wrapper(pythonDef):
        name = pythonDef.__name__
        fnTy = FnTy(name, inputs, outputs)
        # Create a subclass of NativeFn the hard way
        nativeFn = type(name, tuple([NativeFn]), {"call": pythonDef})

        # register the ty and fn
        TYS[name] = fnTy
        FNS[name] = nativeFn

    return wrapper

@nativeFn([], [])
def Ret():
    FN_INDEX = RET_STACK[BSP - 4]


@nativeFn([U32, U32], [U32])
def AddU32():
    sum = DATA_STACK.popTySlot(u32) + DATA_STACK.popTySlot(u32)
    DATA_STACK.pushTySlot(u32, sum)


def registerConstant(ty):
    constantFnTy = FnTy(ty.name() + "ConstantTy", [], [ty])

    def fnInit(self, value: Any):
        NativeFn.__init__(self, self.name(), constantFnTy)
        self.value = value

    def fnExec(self):
        DATA_STACK.pushTySlot(self.ty.outputs[0], self.value)

    constantFn = type(
        ty.name() + 'Constant',
        tuple([NativeFn]),
        {"__init__": fnInit, "exec": fnExec})

# Parser

## Tokens
INVALID = '!invalid!'
LINE_COMMENT = '//'
STR = '"'

CALL = '$'
SINGLE_MACRO = '!'
DOUBLE_MACRO = '!!'

COLON = ':'
SEMICOLON = ';'
EQUAL = '='

BLOCK_OPEN = '('
BLOCK_CLOSE = ')'
UNPARSED_OPEN = '#('
UNPARSED_CLOSED = '#)'

BRACKET_OPEN = '['
BRACKET_CLOSE = ']'
DATA_OPEN = '{'
DATA_CLOSE = '}'

NAME = 'name'
FUNC = 'func'
RETURN = 'return'
LET = 'let'
EOF = 'EOF'

def gi(arr, index, default=None):
    """Get the index in an array or the default if out of bounds."""
    if index >= len(arr):
        return default
    return arr[index]

def isWhitespace(text, index):
    return ord(text[index]) <= ord(' ')

def isSymbol(text, index):
    return (ord('!') <= ord(text[index]) <= ord('/') or
            ord(':') <= ord(text[index]) <= ord('@') or
            ord('[') <= ord(text[index]) <= ord('`') or
            ord('{') <= ord(text[index]) <= ord('~'))

def isNameChar(text, index):
    return not isWhitespace(text, index) and not isSymbol(text, index)

def getToken(text, index):
    """Parse a single token from the text

    Returns: (startIndex, endIndex, token)
    """
    while isWhitespace(text, index): # skip whitespace
        index += 1
        if index >= len(text):
            break

    startIndex = index

    if len(text) <= index:
        return startIndex, index, EOF

    if text[index] == '/':
        if gi(text, index + 1) == '/':
            index += 2
            while gi(text, index, '\n') != '\n':
                index += 1
            return startIndex, index, LINE_COMMENT
        return startIndex, index + 1, INVALID
    elif text[index] == '$': return startIndex, index+1, CALL
    elif text[index] == '!':
        if gi(text, index+1) == '!':
            return startIndex, index+2, DOUBLE_MACRO
        return startIndex, index+1, SINGLE_MACRO

    elif text[index] == '"':
        index += 1
        while True:
            if gi(text, index, '"') == '"': break
            if gi(text, index) == '\\' and gi(text, index + 1) == '"':
                index += 1  # will skip both
            index += 1
        index += 1
        return startIndex, index, STR

    elif text[index] == ':': return startIndex, index + 1, COLON
    elif text[index] == ';': return startIndex, index + 1, SEMICOLON
    elif text[index] == '=': return startIndex, index + 1, EQUAL

    elif text[index] == '(': return startIndex, index + 1, BLOCK_OPEN
    elif text[index] == ')': return startIndex, index + 1, BLOCK_CLOSE

    elif text[index] == '[': return startIndex, index + 1, BRACKET_OPEN
    elif text[index] == ']': return startIndex, index + 1, BRACKET_CLOSE

    elif text[index] == '{': return startIndex, index + 1, DATA_OPEN
    elif text[index] == '}': return startIndex, index + 1, DATA_CLOSE

    elif isSymbol(text, index): return startIndex, index + 1, INVALID

    while isNameChar(text, index):
        index += 1

    # name or keyword
    name = text[startIndex:index]
    token = NAME
    if name == FUNC: token = FUNC
    elif name == LET: token = LET
    elif name == RETURN: token = RETURN

    return startIndex, index, token

def parseFile(text, index):
    pass

def parseFn(text, index):
    pass

def parseWord(text, index):
    pass

def printTokens(text, index=0):
    while index <= len(text):
        startIndex, endIndex, token = getToken(text, index)
        print("{}: {}".format(token, text[startIndex:endIndex]))
        index = endIndex
        if token == 'EOF':
            return

if __name__ == '__main__':
    printTokens(open(sys.argv[1]).read())
