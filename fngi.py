import sys
import struct

from typing import Any
from typing import ByteString
from typing import List
from typing import Dict

# Tokens
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

class Stack(object):
    def __init__(self, initialSize):
        self.data = bytearray(initialSize)
        self.sp = len(self.data)

    def push(self, value: ByteString):
        self.sp -= len(value)
        self.data[self.sp:self.sp + len(value)] = value

    def push_value(self, format: str, value: Any):
        self.push(struct.pack(format, value))

    def pop_value(self, format: str):
        vsize = struct.calcsize(format)
        if self.sp + vsize > len(self.data):
            raise IndexError(vsize)
        out = struct.unpack(format, self.data[self.sp:self.sp + vsize])
        self.sp += vsize
        return out[0]

    def __len__(self):
        return len(self.data) - self.sp

    def __repr__(self):
        return "STACK<{}/{}>[{}]".format(len(self), len(self.data), self.data[self.sp:].hex())

class Ty(object):
    """The base type that all fngi types derive from."""
    pass

class CoreTy(Ty):
    def __init__(self, format):
        self.format = format

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

class StructTy(Ty):
    def __init__(self, name: str, fields: List[Field]):
        self.name = name
        self.fields = fields

class FunctionTy(Ty):
    def __init__(self, 
            name:str,
            inputs: List[Ty],
            outputs: List[Ty],
            execIndex: int):
        self.name = name
        self.inputs = inputs
        self.outputs = outputs
        self.execIndex = execIndex

# Global Environment
execArray = []
tys: Dict[str, Ty] = {}
functions: Dict[str, FunctionTy]  = {}

stackSize = 10 * 2**20 # 10 MiB
typeStack = Stack(stackSize)
deferStack = Stack(stackSize)
localsStack = Stack(stackSize)
dataStack = Stack(stackSize)
callStack = Stack(stackSize)


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
