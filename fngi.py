import abc
import sys
import struct
import ctypes
import copy

from typing import Any
from typing import ByteString
from typing import Callable
from typing import List
from typing import Tuple
from typing import Dict

# Most of our code is going to have some basic tests inline.  Tests can be run
# by installing pytest and running it.

def needAlign(size: int) -> int:
    """Return padding bytes needed to align a value of size."""
    if size % 4 == 0:
        return 0
    return 4 - (size % 4)

def testNeedAlign():
    assert 0 == needAlign(0)
    assert 3 == needAlign(1)
    assert 2 == needAlign(2)
    assert 1 == needAlign(3)
    assert 0 == needAlign(4)

# Types in the compiler are represented by a class. We sometimes use
# uninstantiated classes to represent types, mostly because for
# structs that is the only way python permits using ctypes.Structure subclasses.
# For other types we typically use instantiated classes.
#
# Types are registered with the global ENV.tys dictionary for lookup during
# compilation. For the stage0 fngi compiler we are building here, there are no
# namespaces or modules for us to worry about.
#
# There are two ways to pass data in/out of functions: as data stack values
# or as a structure. We will get into this more later, but the difference
# between these is that data stack values are passed direcly on the
# Env.dataStack (what most Native functions use) whereas struct values
# are done entirely on env.returnStack (i.e. where local variables are stored).
#
# The goal with this is to more closely resemble real assembly or a lower-level
# language, whether that be RISC/CISC registers in assembly or stack-based
# assembly like wasm or an underlying Forth language/cpu.

class Ty(object):
    """The base type that all fngi types derive from.
    """
    def __init__(self):
        super().__init__()

    @classmethod
    def name(cls):
        return cls.__name__


class DataTy(ctypes.Structure, Ty):
    """A type for data represented in memory.

    Reading/Writing a DataTy (dt) to a bytearray (ba):
    - reading => dt = MyDataTy.from_buffer(ba)
    - writing => ba[start:start+dt.size()] = dt

    Python's ctypes.Structure class allows us to get C-compliant structures
    "out of the box" with minimal effort.
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


class NativeTy(DataTy):
    """A native type (i.e. u8, i32, ptr, etc).

    These are Structures with a single c_type field named `v`

    These are the only types allowed on the data stack.
    """


class StackData(DataTy):
    """A list of values that are input/output on the stack.
    """
    def __init__(self, vals: List[NativeTy]):
        self.vals = vals


class UserStruct(DataTy):
    """A user-defined data struct.

    This can be a named struct or "anonymous" function input/output.
    """


class Fn(Ty):
    """The base Fn type.

    New function types are created by instantiating a subclass of Fn.
    """

    def __init__(self, name: str, inputs: DataTy, outputs: DataTy):
        super().__init__()
        self._name, self.inputs, self.outputs = name, inputs, outputs

    def name(self):
        return self._name


# We will define more types soon. But first let's define how our data is
# stored.
#
# Most data in fngi is passed on either the dataStack, retStack or heap within
# the ENV instance. Locals are kept inside of the retStack.
#
# The dataStack is NOT within global memory, and therefore cannot have a
# pointer into it. This is because on some platforms it is stored in registers
# instead of memory.
#
# The other data regions (retStack, heap, typeStack, etc) are all slices of the
# global memory region (ENV.heap.memory).

class Heap(object):
    """The heap can only grow and shrink. Alloc/free is implemented in fngi."""
    def __init__(self, memory):
        self.memory = memory
        self.heap = 0

    def grow(self, size):
        """Grow the heap, return the beginning of the grown region."""
        out = self.heap
        self.heap += size
        return self.heap

    def shrink(self, size):
        self.heap -= size


class Stack(object):
    """The core stack type for storing data of various kinds.

    This is the memory that interpreted fngi programs access. It is a simply a
    bytearray, with fngi structures being C-like. The stacks are also used for
    interfacing between python and fngi programs.
    """
    def __init__(self, memory, start, size):
        self.memory = memory
        self.start = start
        self.end = self.start + size
        self.total_size = size
        self.sp = self.end

    def checkRange(self, index, size):
        if index < 0 or index + size > self.total_size:
            raise IndexError("index={} size={} stack_size={}".format(
                index, size, self.total_size))

    # Set / Push
    def set(self, index: int, value: ByteString):
        size = value.size()
        self.checkRange(self, index, size)
        self.memory[start + index:start + index + size] = value

    def push(self, value: ByteString, align=True):
        size = value.size()
        if align: 
            size += needAlign(size)
        self.checkRange(self.sp - size, size)
        self.sp -= size
        # Note: DON'T use self.sp+size for the second slice value, as it may
        # shorten the bytearray.
        self.memory[self.sp:self.sp + value.size()] = value

    # Get / Pop

    def get(self, index, size) -> bytes:
        self.checkRange(index, size)
        return self.memory[self.start + index:self.start + index + size]

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
        return self.end - self.sp

    def __repr__(self):
        return "STACK<{}/{}>".format(len(self), self.total_size)

# We now come to the "global" environment. This contains all the data which
# functions (native or user, we'll get to that) and the compiler itself mutate
# while compiling and executing fngi code. As we'll see, fngi will be
# simultaniously executing while compiling, as we will write a large amount of
# even the stage0 compiler in the fngi language itself.

class Env(object):
    """Contains all the mutable state for a fngi compiler and interpreter to run.

    All NativeFn's call methods take an Env instance, which they mutate. While
    there is a global ENV variable to ease in the registration of new types and
    native functions, tests will copy the ENV before mutating it so they do not
    affect the global Env.
    """
    def __init__(
            self,
            heap: Heap,
            dataStack: Stack,
            retStack: Stack,
            typeStack: Stack,
            deferStack: Stack,
            callSpace: List[Fn],
            fnLocals: DataTy,
            fnIndexLookup: Dict[int, Fn],
            tys: Dict[str, Ty],
            ):
        self.heap = heap
        self.dataStack, self.retStack = dataStack, retStack
        self.typeStack, self.deferStack = typeStack, deferStack
        self.callSpace = callSpace
        self.fnLocals = fnLocals
        self.fnIndexLookup = fnIndexLookup

        self.tys = tys

        self.running = False
        self.fnIndex = 0
        self.bsp = 0

    def copyForTest(self):
        """Copy the env for the test. Sets fnIndex=0 and running=True."""
        out = copy.deepcopy(self)
        out.fnIndex = 0
        out.running = True
        return out


MiB = 2**20 # 1 Mebibyte
DATA_STACK_SIZE = 4 * 32 # Data Stack stores up to 32 usize elements
STACK_SIZE = MiB // 2 # 1/2 MiB stacks
MEMORY = bytearray(5 * MiB) # 5 MiB Global Memory
HEAP = Heap(MEMORY)

ENV = Env(
    heap=HEAP,
    dataStack=Stack(bytearray(DATA_STACK_SIZE), 0, DATA_STACK_SIZE),
    retStack=Stack(MEMORY, HEAP.grow(STACK_SIZE), STACK_SIZE),
    typeStack=Stack(MEMORY, HEAP.grow(STACK_SIZE), STACK_SIZE),
    deferStack=Stack(MEMORY, HEAP.grow(STACK_SIZE), STACK_SIZE),
    callSpace=[],
    fnLocals=None,
    fnIndexLookup={},
    tys={},
)


def createNativeTy(name, cty):
    """Creates a core data type and registers it.

    This is soley to reduce boilerplace. It is the same as:

        class {{name}}(DataTy): _fields_ =  [('v', {{cty}})]
        ENV.tys["{{name}}"] = {{name}}
    """
    nativeTy = type(name, tuple([NativeTy]), {'_fields_': [('v', cty)]})
    ENV.tys[name] = nativeTy
    return nativeTy


ctype_ptr = ctypes.c_uint32 # fngi uses 32 bits for its pointers.
I8 = createNativeTy("I8",  ctypes.c_int8)
U8 = createNativeTy("U8",  ctypes.c_uint8)
I16 = createNativeTy("I16", ctypes.c_int16)
U16 = createNativeTy("U16", ctypes.c_uint16)
I32 = createNativeTy("I32", ctypes.c_int32)
U32 = createNativeTy("U32", ctypes.c_uint32)
I64 = createNativeTy("I64", ctypes.c_int64)
U64 = createNativeTy("U64", ctypes.c_uint64)
Ptr = createNativeTy("Ptr", ctype_ptr) # an address in memory, no type

class Ref(DataTy):
    """The Ref type has a "generic" refTy it is associated with."""
    _fields_ = [('v', ctype_ptr)]

    def __init__(self, v: int, refTy: Ty):
        self.refTy = refTy
        # Note: reftypes are not registered ty ENV.tys


##############################################################################
# Core function types and the fngi callLoop
#
# This is presented early so that it is clear how simple things are. Experienced
# programmers may notice the similarities to the execution model of Forth, which
# is intentional, because fngi will eventually be implemented in Forth. This (python)
# implementation is for prototyping and to provide a reference/learning manual for the
# stage0 fngi language. It will be kept up-to-date with the stage0 forth compiler.
#
# For the execution model, there are two types of Fn (function), NativeFn which
# are implemented in python and UserFn which are simply a range of indexes
# inside of the ENV.callSpace global variable; which are run by callLoop.
#
# Although functions have types (i.e. inputs/outputs), the types are only
# checked at compile time. At execution time, functions pop values off of the
# env.dataStack for their parameters and push values on the env.dataStack for their
# results. They also use the RET_STACK and BSP for keeping track of local
# variables.

class NativeFn(Fn):
    """A native function.

    Aka a function implemented in python which modifies the env.dataStack.
    """
    def __init__(
            self,
            name: str,
            inputs: DataTy,
            outputs: DataTy,
            call: Callable[[Env], None]):
        super().__init__(name, inputs, outputs)
        self._call = call

    def fnIndex(self):
        raise TypeError("Called fnIndex on a non-user fn")

    def call(self, env: Env):
        self._call(env)

    def __repr__(self):
        return "{}{}->{}".format(self.name(), self.inputs, self.outputs)


class UserFn(Fn):
    """A user-created function defined in fngi source code.

    Functions are compiled to the ENV.callSpace and therefore have an index.
    """
    def __init__(self, name: str, inputs: DataTy, outputs: DataTy, fnIndex: int):
        super().__init__(name, inputs, outputs)
        self._fnIndex = fnIndex

        # Handle registration
        ENV.tys[name] = self
        FN_INDEX_LOOKUP[fnIndex] = self

    def fnIndex(self):
        return self._fnIndex


def callLoop(env: Env):
    """Yup, this is the entire call loop."""
    while env.running:
        fn = env.callSpace[env.fnIndex]
        print(fn)
        if isinstance(fn, NativeFn):
            # If native, run the function
            env.fnIndex += 1
            fn.call(env)
        else:
            # If not native, store the next index on the call stack
            callStack.pushValue(u32, env.fnIndex + 1)
            # and "run" the function by changing the FN_INDEX to be it
            env.fnIndex = fn.fnIndex()


def quitCall(env):
    """Stop running the interpreter."""
    env.running = False
quit = NativeFn("quit", [], [], quitCall)
ENV.tys["quit"] = quit


def addU32Call(env):
    result = U32(env.dataStack.pop(U32).v + env.dataStack.pop(U32).v)
    env.dataStack.push(result)
addU32 = NativeFn("addU32", [U32, U32], [U32], addU32Call)
ENV.tys["addU32"] = addU32


def literal(value: DataTy):
    """Creates a literal.

    Note this is NOT registered with ENV.tys
    """
    ty = type(value)
    def literalCall(env: Env):
        env.dataStack.push(value)
    return NativeFn(ty.name() + 'Literal', [], [ty], literalCall)


def testAddLiterals():
    """Write and test an ultra-basic fngi "function" that just adds two
    literals and quits.
    """
    env = ENV.copyForTest()
    env.callSpace = [literal(U32(22)), literal(U32(20)), addU32, quit]
    callLoop(env)
    assert 42 == env.dataStack.pop(U32).v

# Now that we have a basic execution engine, let's flush out some of the language.
#
# We will use decorators and other python tricks extensively to avoid boilerplate,
# so it's worth learning them if you don't already know them.
#
# The first thing we have to define is how we will be calling functions. fngi
# has local variables which are stored on the return stack, so we will use
# c-style calling conventions, mutating the sp (stack pointer) and bsp (base
# stack pointer) as we call and return from functions. We will also be
# passing values to functions on the returnStack.
# TODO: fix
#


def nativeFn(inputs: List[Ty], outputs: List[Ty], name=None):
    """Takes some types and a python defined function and converts to an
    instantiated NativeFn.
    """
    def wrapper(pyDef):
        nonlocal name
        if name is None:
            name = pyDef.__name__

        nativeFn = NativeFn(name, inputs, outputs, pyDef)

        # Handle registration
        ENV.tys[name] = nativeFn
        return nativeFn
    return wrapper

@nativeFn([], [], "return")
def ret(env):
    pass
    # TODO: need to deal with bsp and sp handling, need to handle locals
    # correctly
    # env.fnIndex = env.retStack[env.bsp - 4]


# We will be writing core pieces of the language. Fns we already have:
# - call a native or user defined function and return from it.
# - put literal values on the stack.
#
# Fns we will be defining here:
# - get/set all core types at a pointer
# - branch to a different part of a fn
# - add, subtract and multiply all the core types

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
