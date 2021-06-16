import abc
import sys
import struct

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


class Ty(abc.ABC):
    """The base type that all fngi types derive from."""


class DataTy(Ty):
    """A type for data represented in memory."""
    @abc.abstractmethod
    def format(self) -> str:
        """The data format for use in a struct."""

    def size(self) -> int:
        """Absolute size when packed."""
        return struct.calcsize(self.format())

    def slotFormat(self) -> str:
        """The data format for use on the stack (alighment 4)."""
        needAlign = needAlign(self.size())
        return self._format + ('' if needAlign == 0 else str(needAlign) + 'x')

    def slotSize(self) -> int:
        """Size when pushed onto the stack (aligment 4)"""
        return struct.calcsize(self.slotFormat())


class CoreTy(DataTy):
    def __init__(self, format):
        self._format = format

    def format(self) -> str:
        return self._format


i8 = CoreTy('b')
u8 = CoreTy('B')
i16 = CoreTy('h')
u16 = CoreTy('H')
i32 = CoreTy('l')
u32 = CoreTy('L')
i64 = CoreTy('q')
u64 = CoreTy('Q')
ptr = u32

class Field(object):
    def __init__(self, name: str, ty: Ty):
        self.name = name
        self.ty = ty


class StructTy(DataTy):
    def __init__(self, name: str, fields: List[Field]):
        super().__init__()
        self.name, self.fields = name, fields

    def format(self) -> str:
        return "".join(f.ty.format() for f in self.fields)


class FnTy(Ty):
    def __init__(self, name: str, inputs: List[Ty], outputs: List[Ty]):
        self.name, self.inputs, self.outputs = name, inputs, outputs


class Fn(abc.ABC):
    """A function has a type and starts execution at an index.

    The runtime will appropriately execute the body within the function by
    incrementing the index, except for control structures which will have
    other behavior.
    """
    def __init__(self, name: str, ty: FnTy):
        self.name, self.ty = name, ty

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

    def fnIndex(self):
        return self._fnIndex


def bytesReplace(b: bytearray, value: int, start: int, end: int):
    """Replace bytes in range with value"""
    for i in range(start, end):
        b[i] = value


class Stack(object):
    """The core stack type for storing data of various kinds.

    This is the memory that interpreted fngi programs access. It is a simply bytearray,
    with fngi structures being C-like.
    """
    def __init__(self, initialSize):
        self.data = bytearray(initialSize)
        self.sp = len(self.data)

    # Push / Set

    def set(self, index: int, value: ByteString):
        self.data[index:index + len(value)] = value

    def pushTy(self, ty: DataTy, value):
        self.pushValue(ty.format(), value)

    def pushValue(self, format: str, value: Any):
        self.push(struct.pack(format, value))

    def push(self, value: ByteString):
        self.sp -= len(value)
        self.set(self.sp, value)

    # Pop / Get

    def get(self, index, size) -> bytes:
        if index + size > len(self.data):
            raise IndexError("{} / {}".format(index + size, len(self.data)))
        return self.data[index:index+size]

    def getValue(self, format, index: int) -> Tuple[Any]:
        return struct.unpack(format, self.get(index, struct.calcsize(format)))

    def popTy(self, ty: DataTy) -> Tuple[Any]:
        return self.popValue(self, ty.format())

    def popValue(self, format: str) -> Tuple[Any]:
        out = self.getValue(format, self.sp)
        self.sp += struct.calcsize(format)
        return out

    def __len__(self):
        return len(self.data) - self.sp

    def __repr__(self):
        return "STACK<{}/{}>[{}]".format(len(self), len(self.data), self.data[self.sp:].hex())


##############################################################################
# The Global Environment and Execution Loop,
#
# These are presented first so that it is clear how simple things are. Experienced
# programmers may notice the similarities to the execution model of Forth, which
# is intentional, because fngi will eventually be implemented in Forth.
#
# For the execution model, there are two types of Fn (function), NativeFn which
# are implemented in python and UserFn which are simply a range of indexes
# inside of the EXECS global variable.
#
# Although functions have types, the types are only checked at compile time.
# At execution time, functions pop values off of the DATA_STACK for their
# parameters and push values on the DATA_STACK for their results. They also use
# the RET_STACK and BSP for keeping track of local variables.

# Global Environment
TYS: Dict[str, Ty] = {}
FNS: Dict[str, Fn] = {}
FN_INDEX_LOOKUP: Dict[int, Fn] = {}
EXECS: List[Fn]
FN_INDEX = 0

stackSize = 10 * 2**20 # 10 MiB

# Runtime
DATA_STACK = Stack(32) # data stack is intentionally limited
BSP = 0
RET_STACK = Stack(stackSize)

# Compiletime
TYPE_STACK = Stack(stackSize)
DEFER_STACK = Stack(stackSize)

def execLoop():
    """Yup, this is the entire exec loop."""
    while True:
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


@nativeFn([u32, u32], [u32])
def AddU32():
    sum = DATA_STACK.popTy(u32) + DATA_STACK.popTy(u32)
    DATA_STACK.pushTy(u32, sum)

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
