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
        TYS[self.name()] = self

    def name(self):
        return self.__class__.__name__


class DataTy(ctypes.Structure, Ty):
    """A type for data represented in memory."""

class I8(DataTy): _fields_ = [('v', ctypes.c_int8)]
class U8(DataTy): _fields_ = [('v', ctypes.c_uint8)]
class I16(DataTy): _fields_ = [('v', ctypes.c_int16)]
class U16(DataTy): _fields_ = [('v', ctypes.c_uint16)]
class I32(DataTy): _fields_ = [('v', ctypes.c_int32)]
class U32(DataTy): _fields_ = [('v', ctypes.c_uint32)]
class I64(DataTy): _fields_ = [('v', ctypes.c_int64)]
class U64(DataTy): _fields_ = [('v', ctypes.c_uint64)]

class Variable(object):
    def __init__(self, name: str, ty: Ty):
        self.name = name
        self.ty = ty

class StructTy(DataTy):
    def __init__(self, fields: List[Variable]):
        super().__init__()
        self.fields = fields

        self._fieldLookup: Map[str, int] = {
            f.name: i for (f, i) in enumerate(fields)
        }

    def format(self) -> str:
        return "".join(f.ty.format() for f in self.fields)


class FnTy(Ty):
    def __init__(self, name: str, inputs: List[Ty], outputs: List[Ty]):
        super().__init__()
        self.name, self.inputs, self.outputs = name, inputs, outputs


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
        size = ctypes.sizeof(value)
        self.checkRange(self, index, size)
        self.data[index:index + size] = value

    def push(self, value: ByteString, align=True):
        size = ctypes.sizeof(value)
        if align: 
            size += needAlign(size)
        self.checkRange(self.sp - size, size)
        self.sp -= size
        # Note: DON'T use self.sp+size for the second slice value, as it may
        # shorten the bytearray.
        self.data[self.sp:self.sp + ctypes.sizeof(value)] = value

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
        size = ctypes.sizeof(ty)
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
# Although functions have types, the types are only checked at compile time.
# At execution time, functions pop values off of the DATA_STACK for their
# parameters and push values on the DATA_STACK for their results. They also use
# the RET_STACK and BSP for keeping track of local variables.

# Global Environment
RUNNING = False # must stay true for execLoop to keep running.


STACK_SIZE = 10 * 2**20 # 10 MiB
# Runtime
DATA_STACK = Stack(32) # data stack is intentionally limited
BSP = 0
RET_STACK = Stack(STACK_SIZE)

# Compiletime
TYS: Dict[str, Ty] = {}
FNS: Dict[str, Fn] = {}
FN_INDEX_LOOKUP: Dict[int, Fn] = {}
EXECS: List[Fn]
FN_INDEX = 0

TYPE_STACK = Stack(STACK_SIZE)
DEFER_STACK = Stack(STACK_SIZE)
LOCALS: StructTy = None

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
# subclassing from NativeFn. Since a large majority of NativeFn's are
# simply functions which we have to define inputs and outputs for,
# we're going to make a python decorator.

def nativeFn(
    inputs: List[Ty],
    outputs: List[Ty]
    ):

    def wrapper(pythonDef):
        name = pythonDef.__name__
        fnTy = FnTy(name, inputs, outputs)
        # Create the NativeFn class the hard way
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
