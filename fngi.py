# Python implementation of fngi language.
#
# This implementation simultaniously defines a "bytecode spec" for the stage0
# compiler runtime. This will allow a future implementation (aka in Forth)
# to have a reference for testing and inspecting the runtime, including
# variables, function defintions, etc.
#
# This requires careful and extensive use of python's `ctypes` module,
# which provides c-like (and therefore fngi-like) datatypes for us to use.

import abc
import sys
import struct
import ctypes
import copy
import dataclasses
import enum
import io
import os
import inspect

from ctypes import sizeof

from typing import Any
from typing import ByteString
from typing import Callable
from typing import List
from typing import Tuple
from typing import Dict
from typing import Union

BIG_ENDIAN = sys.byteorder != 'little'
SEEK_RELATIVE = 1 # f.seek(offset, SEEK_RELATIVE) does a releative seek

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

# This is the only way to get ctypes._CData which is technically private.
DataTy = ctypes.c_uint8.__bases__[0].__bases__[0]

def fieldOffset(ty: DataTy, field: str):
    return getattr(getDataTy(ty), field).offset

def fieldSize(ty: DataTy, field: str):
    return getattr(getDataTy(cls), field).size



# DataTy is the base class of all types that can be represented in memory

Ptr = ctypes.c_uint32 # fngi uses 32 bits for its pointers.
Bool = ctypes.c_bool
U8 = ctypes.c_uint8
U16 = ctypes.c_uint16
U32 = ctypes.c_uint32
U64 = ctypes.c_uint64

I8 = ctypes.c_int8
I16 = ctypes.c_int16
I32 = ctypes.c_int32
I64 = ctypes.c_int64


class Fn(object):
    """The base Fn type.

    New function types are created by instantiating a subclass of Fn.
    """
    def __init__(self, name: str, inputs: DataTy, outputs: DataTy, fnPtr: int):
        super().__init__()
        self._name, self.inputs, self.outputs = name, inputs, outputs
        self._fnPtr = fnPtr

    def name(self):
        return self._name

    def fnPtr(self):
        return self._fnPtr

    def u32(self) -> Ptr:
        return Ptr(self.fnPtr())

class _Ref(object):
    """For defining Ty before defining Ref."""


# All (stage0) fngi types must be either a DataTy, Fn or Ref
Ty = Union[DataTy, Fn, _Ref]


class Ref(_Ref):
    def __init__(self, ty: Ty):
        self.ty = ty


def getDataTy(v: Any) -> DataTy:
    if isinstance(v, DataTy):
        return v.__class__
    elif inspect.isclass(v):
        if issubclass(v, DataTy):
            return v
        elif issubclass(v, Ref):
            return Ptr
    else:
        raise TypeError("Not representable in memory: {}".format(v))


# We will define more types later. But first let's define how our data is
# stored.
#
# Most data in fngi is passed on either the dataStack or is a pointer into
# global memory.
#
# The dataStack is NOT within global memory, and therefore cannot have a
# pointer into it. This is because on some platforms it is stored in registers
# (or a register stack) instead of memory.
#
# The other data regions (returnStack, heap, allocators, etc) are all slices of the
# global memory region (ENV.heap.memory).

MiB = 2**20
KiB = 2**10
DATA_STACK_SIZE = 8 * 4 # Data Stack stores up to 8 usize elements
CODE_HEAP_SIZE = 32 * KiB
BLOCKS_ALLOCATOR_SIZE = (5 * MiB) // 2
EXTRA_HEAP_SIZE = 1 * MiB
RETURN_STACK_SIZE = MiB // 2 # 1/2 MiB return stack

# 4KiB is the size of a "block" of memory, the maximum amount that
# can be allocatd without growing a heap.
BLOCK_SIZE = 2**12 
BLOCKS_TOTAL = BLOCKS_ALLOCATOR_SIZE // BLOCK_SIZE
BlocksArray = U16 * BLOCKS_TOTAL # How array types are created in ctypes


class Memory(object):
    def __init__(self, size):
        self.data = ctypes.create_string_buffer(size)

    def getPtrTo(self, value: DataTy) -> int:
        """Get the index of the value within self.data.

        The definition of Ptr is this index."""
        return ctypes.addressof(value) - ctypes.addressof(self.data)

    def get(self, ptr: int, ty: DataTy, copy=False):
        ty = getDataTy(ty)
        self.checkRange(ptr + sizeof(ty))
        return ty.from_buffer(self.data, ptr)

    def getCopy(self, ptr: int, ty: DataTy):
        ty = getDataTy(ty)
        self.checkRange(ptr + sizeof(ty))
        return ty.from_buffer_copy(self.data, ptr)


    def set(self, ptr, value: DataTy):
        size = sizeof(value)
        self.checkRange(ptr, size)
        self.data[ptr:ptr + size] = bytes(value)

    def getArray(self, ptr: int, ty: DataTy, length: int):
        arrayTy = ty * length
        return self.get(ptr, arrayTy)

    def setArray(self, ptr: int, values: List[DataTy]):
        if len(values) == 0:
            return

        ty = values[0].__class__
        arrayTy = ty * len(values)
        arrayValue = arrayTy(*values)
        self.set(ptr, arrayValue)

    def checkRange(self, ptr: int, size: int = 0):
        if ptr <= 0 or (ptr + size) > len(self.data):
            raise IndexError("ptr={} memorySize={}".format(
                ptr, len(self.data)))


class MHeap(ctypes.Structure):
    """A heap as represented in memory."""
    _fields_ = [
        ('start', Ptr), # the start of heap's memory
        ('end', Ptr),   # the end of heap's memory
        ('heap', Ptr),  # the heap pointer
    ]

    @classmethod
    def new(cls, start, end):
        return cls(start, end, start)


class Heap(object):
    """The heap grows up."""
    def __init__(self, memory, mheap):
        self.memory = memory
        self.mheap = mheap

    @property # property without name.setter is immutable access
    def start(self) -> int:
        return self.mheap.start

    @property
    def end(self) -> int:
        return self.mheap.end

    @property
    def heap(self) -> int:
        return self.mheap.heap

    @heap.setter
    def heap(self, val: int):
        self.mheap.heap = val

    def checkRange(self, ptr, size):
        if ptr < self.start or ptr + size >= self.end:
            raise IndexError(
                    "start={} end={} ptr={} size={}".format(self.start, self.end, ptr, size))

    def grow(self, size, align=True):
        """Grow the heap, return the beginning of the grown region."""
        size = size + (needAlign(size) if align else 0)
        self.checkRange(self.heap, size)
        out = self.heap
        self.heap += size
        return out

    def shrink(self, size, align=True):
        size = size + (needAlign(size) if align else 0)
        self.checkRange(self.heap - size, size)
        self.heap -= size

    def push(self, value: DataTy, align=True) -> DataTy:
        """Push a DataTy then return it's mutable reference inside memory"""
        ptr = self.grow(sizeof(value), align)
        self.memory.set(ptr, value)
        return self.memory.get(ptr, getDataTy(value))


class MStack(ctypes.Structure):
    """A stack as represented in memory."""
    _fields_ = [
        ('start', Ptr), # the start of stack's memory
        ('end', Ptr),   # the end of stack's memory
        ('sp', Ptr),  # the stack pointer
    ]

    @classmethod
    def new(cls, start, end):
        return cls(start, end, end)

class Stack(object):
    """Stack implementation, used for the data stack and the return stack.

    Stacks grow down, and typically are kept on alignment.
    """
    def __init__(self, memory: Memory, mstack: MStack):
        self.memory = memory
        self.mstack = mstack
        self.total_size = mstack.end - mstack.start

    @classmethod
    def forTest(cls, size: int):
        return cls(Memory(size), MStack.new(0, size))

    @property # property without name.setter is immutable access
    def start(self) -> int:
        return self.mstack.start

    @property
    def end(self) -> int:
        return self.mstack.end

    @property
    def sp(self) -> int:
        return self.mstack.sp

    @sp.setter
    def sp(self, val: int):
        self.mstack.sp = val

    def checkRange(self, index, size):
        if index < 0 or index + size > self.total_size:
            raise IndexError("index={} size={} stack_size={}".format(
                index, size, self.total_size))

    # Set / Push
    def set(self, index: int, value: DataTy):
        """Set a value at an offset from the sp."""
        self.checkRange(self.sp + index, sizeof(value))
        self.memory.set(self.sp + index, value)

    def push(self, value: DataTy):
        size = sizeof(value) + needAlign(sizeof(value))
        self.checkRange(self.sp - size, size)
        self.sp -= size
        self.memory.set(self.sp, U32(0)) # zero the memory first
        self.memory.set(self.sp, value)

    # Get / Pop

    def get(self, index, ty: DataTy) -> bytes:
        """Get a value at an offset from the sp."""
        ty = getDataTy(ty)
        self.checkRange(self.sp + index, sizeof(ty))
        return self.memory.get(self.sp + index, ty)

    def pop(self, ty: DataTy) -> DataTy:
        size = sizeof(ty) + needAlign(sizeof(ty))
        self.checkRange(self.sp, size)
        out = self.memory.getCopy(self.sp, ty)
        self.sp += size
        return out

    def __len__(self):
        return self.end - self.sp

    def __repr__(self):
        return "STACK<{}/{}>".format(len(self), self.total_size)


class TestStack(object):
    def newStack(self):
        return Stack.forTest(16)

    def testPushPopI16(self):
        s = self.newStack()
        s.push(I16(0x7008))
        assert len(s) == 4
        assert s.pop(I16).value == 0x7008

    def testPushPopI32(self):
        s = self.newStack()
        s.push(I32(0x4200FF))
        assert s.pop(I32).value == 0x4200FF


#########################################
# Memory Manager
#
# Our parser will be building an Abstract Syntax Tree, which is basically a
# tree consisting of unknown sized (aka allocated) nodes of various types. The
# AST can either be parsed from the source code OR generated dynamically by
# macros, so there will be a lot of need for non-static memory allocation.
#
# Trying to do this without a memory manager would be extremely annoying to say
# the least. Furthermore, once we have built the stage0 compiler we
# will want to compile the stage1 compiler, and for that we definitely need a
# memory manager as well.
#
# There are an extremely wide range of memory managers. One of the core
# problems with "simpler" memory managers with minimal hardware support has to
# do with memory fragmentation: when memory of various sizes is allocated and
# freed repeatedly you are left with "holes" of memory that cannot fit larger
# types.
#
# Modern systems with enough hardware and software support have MMUs (Memory
# Management Units) which allow virtually moving memory pages to make
# non-consecuitive pages appear consecutive to the program. However, fngi is
# designed to be able to not only _run_ on very minimal devices (i.e.
# microcontrollers) but also be able to _compile itself from source_ with
# almost no binary blob (just the binary assembly needed to bootstrap basic IO
# and the forth interpreter).
#
# > Note: Obviously the last statement is not true for this (prototype) python
# > implementation.
#
# We will avoid the issue of complicated memory management with a few
# restrictions on the programs supported by our stage0 compiler:
# - Memory can only be allocated in up to 4k (2**12) blocks
# - Memory size must be a power of 2, with a minimum size of 16 (2**4)
# - It is the program's/compiler's job (aka not the memory manager's job) to
#   track the size of it's pointers. The allocator does not know the size of a
#   pointer. This means that unlike the "standard C" function signature
#   of `free(ptr)` in stage0 fngi it is `free(ptrSize, ptr)`
# - Memory must be allocated from arenas. An arena is an object with alloc/free methods,
#   but unlike a "global allocator" the entire arena can be dropped. Since the max block
#   size is 4k, this makes sure that all "holes" are at least 4k in size and so there
#   will rarely be fragmentation below 4k in size.
# - There is no global allocator except for heap.grow(), heap.shrink() and a
#   a single global arena instance (which can be used to create child arenas).

BLOCK_USED = 0xFA7E # sentinel for last used block in a linked-list
BLOCK_FREE = 0xF4EE # sentinal for last free block in a linked-list
BLOCK_OOB = 0xFFFF # block allocator is Out Of Blocks


class U16IndexSll(ctypes.Structure):
    """Our allocators will make estensive use of an "indexed" singly linked
    list.

    Unlike normal SLL's, the "nextNode" item is an index into the array memory
    instead of a pointer into global memory.
    """
    _fields_ = [
        ('root', U16),
        ('arrayPtr', Ptr),
    ]

    def push(self, memory, i: int):
        # FROM: root -> a -> b -> ...
        #   TO: root -> i -> a -> b -> ...

        # Set: i -> a
        memory.set(memory.arrayPtr + i * sizeof(U16), U16(self.root))
        # Set: root -> i
        self.root = i

    def pop(self, memory) -> int:
        # FROM: root -> a -> b -> ...
        #   TO: root -> b -> ...
        # RETURN: ptr to a (value that was in root)
        out = self.root
        if out != BLOCK_FREE:
            return BLOCK_OOB

        self.root = memory.get(memory.arrayPtr, U16)
        return out


class MBlockAllocator(ctypes.Structure):
    _fields_ = [
        ('freeRootIndex', U16),
        ('blocksPtr', Ptr),
    ]

class BlockAllocator(ctypes.Structure):
    """A global allocator that allows allocating only 4k blocks.

    Use createBlockAllocator function to create an instance.

    We are designing this in a way we can easily emulate in forth. The
    allocator has a pointer to it's memory region (memory, start)
    as well as an array of singly-linked lists which point to indexes
    within the freeBlocks array.

    When a block is used, it's value is set to BLOCK_USED
    """
    def __init__(self, memory: Memory, mba: MBlockAllocator):
        self.memory = memory
        self.mba = mba

    @property
    def freeRootIndex(self) -> int:
        return self.mba.freeRoot

    @property
    def blocksPtr(self) -> BlocksArray:
        return self.mba.blocksPtr

    def selfPtr(self):
        return self.memory.getPtrTo(self.mba)

    def freeRootPtr(self):
        return self.selfPtr() + BlockAllocator.freeRoot.offset

    def blocksStart(self) -> int:
        return self.blocksPtr

    def blocksEnd(self) -> int:
        return self.blockStart() + BLOCKS_ALLOCATOR_SIZE

    def alloc(self) -> int:
        """Return the indeex to a free block, or 0."""
        out = self.freeRoot
        if out == BLOCK_FREE:
            return BLOCK_OOB

        self.freeRoot = self.blocks[self.freeRoot]
        self.blocks[out] = BLOCK_USED
        return out

    def free(self, block: int):
        newRoot = block
        self.checkPtr(self.getPtrTo(newRoot))
        self.blocks[newRoot] = self.freeRoot
        self.freeRoot = newRoot

    def checkPtr(self, ptr):
        ptrAlign = ptr % BLOCK_SIZE 
        if ptr < self.start or ptr > self.start + self.size or ptrAlign != 0:
            raise IndexError("ptr not managed: ptr={}, ptrAlign={} start={} end={}".format(
                ptr, ptrAlign, self.blocksStart(), self.blocksEnd()))

    def getPtrTo(self, block: int):
        ptr = self.start + (block * BLOCK_SIZE)
        self.checkPtr(ptr)
        return ptr

    def getIndex(self, ptr: int):
        self.checkPtr(ptr)
        return (ptr - self.start) % BLOCK_SIZE



def joinMem(ptr1: int, ptr2: int, size: int):
    """If mem can be joined, return the lowest pointer. Else return 0"""
    if ptr1 == 0 or ptr2 == 0:
        return 0

    if ptr2 < ptr1:
        ptr1, ptr2 = ptr2, ptr1

    if ptr1 + size == ptr2:
        return ptr1

    return 0


class Arena(object):
    """For the arena allocator we use a few tricks for storing free regions of
    memory. Please note that this allocator is designed to be a prototype for
    one written in forth wich can run on microcontrollers, so it uses extremely
    low level concepts (i.e. raw pointers stored inside of freed blocks)

    The arena allocator can allocate memory in powers of 2, from 16 (2**4)
    bytes up to 4kiB (2**12). It keeps track of free blocks of memory by using
    an array of linked lists, where each index in the array is 4+po2. Unlike
    the BlockAllocator, the pointers to the "next free node" is kept _within
    the allocateable memory itself_. This allows each arena allocator to have
    only ~64 bytes of memory overhead.

    The arena keeps track of all the 4KiB blocks it is using by using the
    BlockAllocator's blocks array. This is the exact same method that the block
    allocator itself uses to track it's free blocks (difference being the arena
    is tracking the allocated blocks). This allows all of the blocks the arena is using to
    be freed when the arena is dropped.

    The algorighm our arena allocator uses for small allocations is called a
    "buddy allocator".  Blocks are allocated by po2, if a free block is not
    available it is requested from the next-highest po2, which will ask from
    the next highest, etc. When a block is found, it will be split in half
    repeateldy (storing the unused half) until it is the correct size. When a
    block is freed, merging will be attempted on the next available free block.
    Both alloc and free run in O(1) time (approximately 10 to 100 "executions").

    We allow allocating 2^4 to 2^12 size blocks (8 sizes). 7 of these have a
    linked list, while size 2^12 uses a single index into the block allocator.
    """
    def __init__(self, parent, blockAllocator: BlockAllocator):
        self.parent = parent
        self.blockAllocator = blockAllocator
        self.blockRoot = BLOCK_USED
        self.po2Roots = { po2: 0 for po2 in range(4, 12) }

    def allocBlock(self) -> int:
        """Allocate a block, return the pointer to it."""
        index = self.blockAllocator.alloc()
        if index == BLOCK_OOB:
            return 0

        self.blockAllocator.blocks[index] = self.blockRoot
        self.blockRoot = index
        return self.blockAllocator.getPtrTo(index)

    def pushFreePo2(self, po2, ptr):
        if po2 == 12:
            blocks = self.blockAllocator.blocks
            index = self.blockAllocator.getIndex(ptr)

            # find the index in the LL and pop it
            if index == self.blockRoot:
                self.blockRoot = blocks[index]
            else:
                prev = self.blockRoot
                while blocks[prev] != index:
                    prev = blocks[prev]
                blocks[prev] = blocks[index]

            self.blockAllocator.free(index)
        else:
            memory = self.blockAllocator.memory
            oldRoot = self.po2Roots[po2]
            memory.set(oldRoot, Ptr(ptr))
            self.po2Roots[po2] = ptr

    def popFreePo2(self, po2):
        if po2 == 12:
            freeMem = self.allocBlock()
        else:
            freeMem = self.po2roots[po2]
            if freeMem != 0:
                memory = self.blockAllocator.memory
                newRoot = memory.get(freeMem, Ptr)
                self.po2roots[po2] = newRoot
        return freeMem

    def alloc(self, wantPo2: int):
        if wantPo2 > 12:
            raise ValueError("wantPo2=" + str(wantPo2))
        po2 = wantPo2
        freeMem = 0

        # Find a block of size greather than or equal to wantPo2
        while True:
            freeMem = self.popFreePo2(po2)
            if po2 == 12 or freeMem != 0:
                break
            po2 += 1

        if freeMem == 0:
            return 0

        while True:
            if po2 == wantPo2:
                return freeMem

            # split freeMem in half, storing the leftover region
            po2 -= 1
            extraMem = freeMem + (2**po2)
            self.pushFreePo2(po2, extraMem)

    def free(self, po2: int, ptr: int):
        while True:
            if po2 == 12:
                self.pushFreePo2(po2, ptr)
                break
            joinedMem = joinMem(ptr, self.po2Roots[po2], 2**po2)
            if joinedMem == 0:
                self.pushFreePo2(po2, ptr)
                break
            else:
                self.popFreePo2(po2) # remove it, we are joining it
                ptr = joinedMem
                # then try to join the next largest po2
                po2 += 1



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
            memory: Memory,
            dataStack: Stack,
            heap: Heap,
            codeHeap: Heap,
            ba: BlockAllocator,
            returnStack: Stack,
            fnPtrLookup: Dict[int, Fn],
            tys: Dict[str, Ty],
            refs: Dict[Ty, Ref],
            ):
        self.memory = memory
        self.dataStack = dataStack
        self.heap = heap
        self.codeHeap = codeHeap
        self.ba = ba
        self.returnStack = returnStack
        self.fnPtrLookup = fnPtrLookup

        self.tys = tys
        self.refs = refs

        self.running = False
        self.ep = 0 # execution pointer

    def copyForTest(self):
        """Copy the env for the test. Sets ep=heap.heap and running=True."""
        m = copy.deepcopy(self.memory)
        dataStack = copy.deepcopy(self.dataStack)

        heap = Heap(m, self.heap.mheap)
        codeHeap = Heap(m, self.codeHeap.mheap)
        ba = BlockAllocator(m, self.ba.mba)
        returnStack = Stack(m, self.returnStack.mstack)

        out = Env(
            memory=m,
            dataStack=dataStack,
            heap=heap,
            codeHeap=codeHeap,
            ba=ba,
            returnStack=returnStack,
            fnPtrLookup=copy.deepcopy(self.fnPtrLookup),
            tys=copy.deepcopy(self.tys),
            refs=copy.deepcopy(self.refs))

        out.running = True
        out.ep = self.heap.heap
        return out



# Memory layout:
# CODE_HEAP: location where "code" and native fn indexes go.
#   note: memory address 0 has something special written.
# BLOCK_ALLOCATOR: location for block allocator to use
# EXTRA_HEAP: some extra heap space to put global variables and data
#   such as the global allocators, user-defined globals, compiler state, etc.
# RETURN_STACK: the return stack
MEMORY_SIZE = (
    CODE_HEAP_SIZE
    + BLOCKS_ALLOCATOR_SIZE
    + EXTRA_HEAP_SIZE
    + RETURN_STACK_SIZE)


MEMORY = Memory(MEMORY_SIZE)
MEMORY.data[0:4] = b'\xA0\xDE\xFE\xC7' # address 0 is "A DEFECT"


# Note: return stack is "above" the heap
HEAP = Heap(MEMORY, MHeap.new(0, MEMORY_SIZE - RETURN_STACK_SIZE))
CODE_HEAP_MEM = 4 + HEAP.grow(CODE_HEAP_SIZE) # not ptr=0
BLOCK_ALLOCATOR_MEM = HEAP.grow(BLOCKS_ALLOCATOR_SIZE)
REAL_HEAP_START = HEAP.heap

RETURN_STACK_MEM = BLOCK_ALLOCATOR_MEM + EXTRA_HEAP_SIZE
# data_stack: not in main memory.


# Now that we have the _space_ for all of our memory regions, we need to
# actually keep the mutations to them synced within the main memory block.
#
# Why go to this trouble? This means that if we re-implement this language
# according to this binary spec, the binary output should match _exactly_
# at all points of execution. This will make testing and debugging much easier.
# For example, we can write tests that compile a fngi program using both python
# and forth compilers and assert they are identical. Also, this means we can
# write assembly dumping and debugging tools in python and use them on our
# Forth implementation.

# Data stack kept in separate memory region
DATA_STACK = Stack(
    Memory(DATA_STACK_SIZE + 4),
    MStack.new(4, DATA_STACK_SIZE))

# Note: Heap.push sets the memory and returns a mutable reference
HEAP.mheap = HEAP.push(HEAP.mheap)
CODE_HEAP = Heap(
    MEMORY,
    HEAP.push(MHeap.new(
        CODE_HEAP_MEM, CODE_HEAP_MEM + CODE_HEAP_SIZE)))

BLOCK_ALLOCATOR = BlockAllocator(
    MEMORY,
    HEAP.push(MBlockAllocator(0, BLOCK_ALLOCATOR_MEM)))

RETURN_STACK_MEM_END = RETURN_STACK_MEM + RETURN_STACK_SIZE
RETURN_STACK = Stack(
    MEMORY,
    HEAP.push(MStack.new(RETURN_STACK_MEM, RETURN_STACK_MEM_END)))

# Data stack kept in separate memory region
DATA_STACK = Stack(
    Memory(DATA_STACK_SIZE),
    MStack.new(0, DATA_STACK_SIZE))

ENV = Env(
    memory=MEMORY,
    dataStack=DATA_STACK,
    heap=HEAP,
    codeHeap=CODE_HEAP,
    ba=BLOCK_ALLOCATOR,
    returnStack=RETURN_STACK,
    fnPtrLookup={},
    tys={},
    refs={},
)


def registerFn(fn: Fn):
    ENV.tys[fn.name()] = fn
    ENV.fnPtrLookup[fn.fnPtr()] = fn
    return fn


ENV.tys.update({
    "U8": U8, "U16": U16, "U32": U32, "U64": U64,
    "I8": I8, "I16": I16, "I32": I32, "I64": I64,
    "Ptr": Ptr})


def createNativeRef(ty: Ty) -> Ref:
    refTy = Ref(ty)
    ENV.refs[ty] = refTy
    return refTy


RefI8 = createNativeRef(I8)
RefU8 = createNativeRef(U8)
RefI16 = createNativeRef(I16)
RefU16 = createNativeRef(U16)
RefI32 = createNativeRef(I32)
RefU32 = createNativeRef(U32)
RefI64 = createNativeRef(I64)
RefU64 = createNativeRef(U64)


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
# are implemented in python (and have a reference function pointer) and UserFn
# which are simply an array of function pointers inside of the ENV.memory.
# callLoop takes care of how to execute each kind of Fn.
#
# Although functions have types (i.e. inputs/outputs), the types are only
# checked at compile time. At execution time, functions pop values off of the
# env.dataStack for their parameters and push values on the env.dataStack for their
# results. They also may use the return stack for storing local variables and
# input/output of values.

class NativeFn(Fn):
    """A native function.

    Aka a function implemented in python which modifies the env.dataStack.
    """
    def __init__(
            self,
            name: str,
            inputs: DataTy,
            outputs: DataTy,
            call: Callable[[Env], None],
            fnPtr: int):

        super().__init__(name, inputs, outputs, fnPtr)
        self._call = call

    def call(self, env: Env):
        self._call(env)

    def __repr__(self):
        return "{}{}->{}".format(self.name(), self.inputs, self.outputs)


class UserFn(Fn):
    """A user-created function defined in fngi source code.

    Functions are compiled to the ENV.callSpace and therefore have an index.
    """
    def __init__(self, name: str, inputs: DataTy, outputs: DataTy, fnPtr: int):
        super().__init__(name, inputs, outputs, fnPtr)


def callLoop(env: Env):
    """Yup, this is the entire call loop."""
    while env.running:
        fnPtr = env.memory.get(env.ep, Ptr).value
        fn = env.fnPtrLookup[fnPtr]
        if isinstance(fn, NativeFn):
            # If native, run the function
            env.ep += 4
            fn.call(env)
        else:
            # If not native, store the next index on the call stack
            env.returnStack.pushValue(u32, env.ep + 1)
            # and "run" the function by changing the FN_INDEX to be it
            env.ep = fnPtr

# We want a way to define native fn's in as few lines as possible. We will use
# decorators and other python tricks extensively to avoid boilerplate, so it's
# worth learning them if you don't already know them.

def popData(env: Env, ty: Ty) -> int:
    if issubclass(ty, Ref):
        return env.dataStack.pop(Ptr)
    else:
        return env.dataStack.pop(ty)

def nativeFn(inputs: List[Ty], outputs: List[Ty], name=None, createRef=True):
    """Takes some types and a python defined function and converts to an
    instantiated NativeFn.
    """
    outputsPushOrder = outputs[::-1] # reverse

    def wrapper(pyDef):
        nonlocal name
        if name is None:
            name = pyDef.__name__

        def callDef(env):
            nonlocal name # available when debugging
            # pop stack items from left to right
            args = [env.dataStack.pop(ty).value for ty in inputs]
            # call the function with them in that order
            outStack = pyDef(env, *args)
            outStack = [] if outStack is None else outStack
            # reverse the output stack because that is the correct order for
            # pushing to the stack
            outStack.reverse()
            assert len(outStack) == len(outputs)
            # push outputs to the data stack
            for out, ty in zip(outStack, outputsPushOrder):
                env.dataStack.push(ty(out))

        fnPtr = ENV.codeHeap.grow(4)
        nativeFnInstance = NativeFn(name, inputs, outputs, callDef, fnPtr)

        # Handle registration
        registerFn(nativeFnInstance)
        if createRef:
            createNativeRef(nativeFnInstance)

        return nativeFnInstance
    return wrapper


@nativeFn([], [])
def quit(env):
    """Stop running the interpreter."""
    env.running = False


@nativeFn([U32, U32], [U32])
def addU32(env, a: int, b:int):
    return [a + b]


@nativeFn([], [U32])
def literalU32(env):
    """Creates a literal using the next 4 bytes."""
    out = env.memory.get(env.ep, U32).value
    env.ep += 4
    return [out]


def testCallLoop_addLiterals():
    """Write and test an ultra-basic fngi "function" that just adds two
    literals and quits.
    """
    env = ENV.copyForTest()
    env.memory.setArray(
        env.heap.heap,
        [
            literalU32.u32(), U32(22),
            literalU32.u32(), U32(20),
            addU32.u32(), quit.u32(),
        ])
    callLoop(env)
    result = env.dataStack.pop(U32).value
    assert 42 == result

# Now that we have a basic execution engine, let's flush out some of the language.
#
# Stage0 Fngi has only 4 core constructs:
# - native data types like U32, Ptr, and Ref
# - user-defined data types (structs and enums) which are composed of native
#   data types.
# - native and user-defined functions. In stage0 these can only push/pop native values
#   from the stack and define local variables.
# - native and user-defined macros which for stage0 call only popToken and
#   pushToken
#
# When calling a function, the compiler must insert the following. fnStackSize is known
# at compile time.
#
#   subStack(fnStackSize)
#   call fnPtr
#   addStack(fnStackSize)These are known 

@nativeFn([], [], name="return", createRef=False)
def ret(env):
    env.ep = env.returnStack.pop(Ptr)

def compare(a, b):
    """
    returns < 0 iff a < b
    returs  = 0 iff a == b
    returns > 0 iff a > b
    """
    if a < b:
        return -1
    elif a > b:
        return 1
    else:
        return 0

# We will be writing core pieces of the language. Fns we already have:
# - call a native or user defined function (see callLoop) and return from it.
# - put literal values on the stack.
#
# Fns we will be defining here:
# - add to the return sp and fetch/update from an offset of the rsp
# - add/subtract/multiply/divmod U32/I32
# - fetch/update U8/U32/I32
# - compare U8/I32/U32
# - branch to a different part of a fn
#
# We can write (and test) many programs using only these types, so those will
# be what we focus on. We will define the other types in a separate file for
# the reader's reference.

@nativeFn([I32], [])
def addRsp(env, offset):
    env.returnStack.sp += offset

@nativeFn([I32], [U8])
def fetchRspOffsetU8(env, offset):
    return [env.returnStack.get(offset, U8)]

@nativeFn([I32, U8], [])
def updateRspOffsetU8(env):
    env.returnStack.set(env.dataStack.pop(I32), env.dataStack.pop(U8))

# U8 Native Functions

@nativeFn([RefU8], [U8])
def fetchU8(env, ptr):
    return [envb.heap.get(ptr, U8)]

@nativeFn([RefU8, U8], [])
def updateU8(env, ptr, value):
    env.heap.set(ptr, U8(value))

@nativeFn([U8, U8], [U8])
def addU8(env, a, b):
    return [a + b]

@nativeFn([U8, U8], [U8])
def subU8(env, a, b):
    return [a - b]

@nativeFn([U8, U8], [U32])
def mulU8(env, a, b):
    return [a * b]

@nativeFn([U8, U8], [I32])
def compareU8(env, a, b):
    """a b -> compare."""
    return [compare(a, b)]

# U32 Native Functions

@nativeFn([RefU32], [U32])
def fetchU32(env, ptr):
    return [envb.heap.get(ptr, U32)]

@nativeFn([RefU32, U32], [])
def updateU32(env, ptr, value):
    env.heap.set(ptr, U32(value))

@nativeFn([U32, U32], [U32])
def addU32(env, a, b):
    return [a + b]

@nativeFn([U32, U32], [U32])
def subU32(env, a, b):
    return [a - b]

@nativeFn([U32, U32], [U64])
def mulU32(env, a, b):
    return [a * b]

@nativeFn([U32, U32], [U32, U32])
def divmodU32(env, a, b):
    """a b -> quotent remainder"""
    return [a / b, a % b]

@nativeFn([U32, U32], [I32])
def compareU32(env, a, b):
    """a b -> compare."""
    return [compare(a, b)]

# I32 Native Functions

@nativeFn([RefI32], [I32])
def fetchI32(env, ptr):
    return [envb.heap.get(ptr, I32)]

@nativeFn([RefI32, I32], [])
def updateI32(env, ptr, value):
    env.heap.set(ptr, I32(value))

@nativeFn([I32, I32], [I32])
def addI32(env, a, b):
    return [a + b]

@nativeFn([I32, I32], [I32])
def subI32(env, a, b):
    return [a - b]

@nativeFn([I32, I32], [I64])
def mulI32(env, a, b):
    return [a * b]

@nativeFn([I32, I32], [I32, I32])
def divmodI32(env, a, b):
    """a b -> quotent remainder"""
    return [a / b, a % b]

@nativeFn([I32, I32], [I32])
def compareI32(env, a, b):
    """a b -> compare."""
    return [compare(a, b)]


#########################################
# Parser
#
# With the above we should have enough to compile the stage0 language, except
# for macros which we will get to later.
#
# First we are going to build the parser, which fairly self-explanatory. We parse
# and emit tokens.

## Tokens
class TokenVariant(enum.Enum):
    # TODO: use solid integer values
    EOF = enum.auto()

    # Multi char symbols
    SINGLE_MACRO = enum.auto() # !
    DOUBLE_MACRO = enum.auto() # !!
    ARROW = enum.auto() # ->

    # Single char symbols
    SEMICOLON = enum.auto() # ;
    EQUAL = enum.auto() # =
    REF = enum.auto() # &
    DEREF = enum.auto() # @
    BLOCK_OPEN = enum.auto() # (
    BLOCK_CLOSE = enum.auto() # )
    DATA_OPEN = enum.auto() # {
    DATA_CLOSE = enum.auto() # }
    TYPE_OPEN = enum.auto() # [
    TYPE_CLOSE = enum.auto() # ]

    # Keywords
    FN = enum.auto() # fn
    LET = enum.auto() # let
    RETURN = enum.auto() # return

    # Contains Direct Data
    INVALID = enum.auto()
    NAME = enum.auto()
    LINE_COMMENT = enum.auto() # //
    ESCAPED_STR = enum.auto() # \"


@dataclasses.dataclass
class Token(object):
    variant: TokenVariant
    string: str = None
    lines: int = 0

EOF = Token(TokenVariant.EOF)

SINGLE_MACRO = Token(TokenVariant.SINGLE_MACRO)
DOUBLE_MACRO = Token(TokenVariant.DOUBLE_MACRO)
ARROW = Token(TokenVariant.ARROW)

SEMICOLON = Token(TokenVariant.SEMICOLON)
EQUAL = Token(TokenVariant.EQUAL)
REF = Token(TokenVariant.REF)
DEREF = Token(TokenVariant.DEREF)
BLOCK_OPEN = Token(TokenVariant.BLOCK_OPEN)
BLOCK_CLOSE = Token(TokenVariant.BLOCK_CLOSE)
DATA_OPEN = Token(TokenVariant.DATA_OPEN)
DATA_CLOSE = Token(TokenVariant.DATA_CLOSE)
TYPE_OPEN = Token(TokenVariant.TYPE_OPEN)
TYPE_CLOSE = Token(TokenVariant.TYPE_CLOSE)

FN = Token(TokenVariant.FN)
LET = Token(TokenVariant.LET)
RETURN = Token(TokenVariant.RETURN)


def isWhitespace(c: str):
    if c == '': return False
    return ord(c) <= ord(' ')

def isSymbol(c: str):
    return (ord('!') <= ord(c) <= ord('/') or
            ord(':') <= ord(c) <= ord('@') or
            ord('[') <= ord(c) <= ord('`') or
            ord('{') <= ord(c) <= ord('~'))

def isNameChar(c: str):
    return not isWhitespace(c) and not isSymbol(c)


class TokenStream(object):
    def __init__(self, fo: io.TextIOWrapper):
        self.fo = fo
        self.fileSize = os.stat(fo.fileno()).st_size
        self.tokenIndex = 0
        self.lineno = 0

    def nextToken(self):
        self.fo.seek(self.tokenIndex)
        nextToken(self.fo, self.fileSize)
        self.tokenIndex = self.fo.tell()

    def checkForLine(self, c: str):
        if c == '\n':
            self.lineno += 1
        return c

    def get(self, start: int, size: int):
        self.fo.seek(start)
        return self.fo.read(size)

    def read1(self):
        return self.fo.read(1)

    def seekBack1(self, c: str):
        if c != b'': 
            self.fo.seek(self.fo.tell() - 1)

    def nextToken(self) -> Token:
        """Parse a single token from the file."""
        f = self.fo
        c = self.checkForLine(self.read1())
        while isWhitespace(c): # skip whitespace
            c = self.checkForLine(self.read1())
            if c == b'': break

        if c == b'':
            return EOF

        elif c == b'/':
            c = self.read1()
            if c != b'/':
                self.seekBack1(c)
                return Token(TokenVariant.INVALID, b'/')

            if c == b'/':
                out = []
                while c != b'' and c != b'\n':
                    c = self.read1()
                    out.append(c)
                self.checkForLine(c)
                return Token(TokenVariant.LINE_COMMENT, b''.join(out), 1)

        elif c == b'!':
            c = self.read1()
            if c == b'!':
                return DOUBLE_MACRO
            self.seekBack1(c)
            return SINGLE_MACRO

        elif c == b'-':
            c = self.read1()
            if c == b'>':
                return ARROW
            self.seekBack1(c)
            return Token(TokenVariant.INVALID, b'-')

        elif c == b'\\': # Escaped Str
            out = [b'\\']
            c = self.read1()
            out.append(c)
            if c != b'"':
                lines = c == b'\n'
                self.lineno += lines
                return Token(TokenVariant.INVALID, b''.join(out), lines)

            out = []
            startingLineno = self.lineno
            while True:
                c = self.checkForLine(self.read1())
                out.append(c)
                if c == b'\\':
                    c = self.checkForLine(self.read1())
                    out.append(c)
                    if c == b'"':
                        break

            return Token(
                TokenVariant.ESCAPED_STR,
                b''.join(out),
                lines=self.lineno - startingLineno)

        elif c == b';': return SEMICOLON
        elif c == b'=': return EQUAL
        elif c == b'&': return REF
        elif c == b'@': return DEREF
        elif c == b'(': return BLOCK_OPEN
        elif c == b')': return BLOCK_CLOSE
        elif c == b'{': return DATA_OPEN
        elif c == b'}': return DATA_CLOSE
        elif c == b'[': return TYPE_OPEN
        elif c == b']': return TYPE_CLOSE
        elif isSymbol(c): return Token(TokenVariant.INVALID, c)

        name = [c]
        while True:
            c = self.read1()
            if not isNameChar(c):
                self.seekBack1(c)
                break
            name.append(c)

        name = b''.join(name)
        if name == b'fn': return FN
        if name == b'let': return LET
        if name == b'return': return RETURN

        return Token(TokenVariant.NAME, name)

    def getAllTokens(self) -> List[Token]:
        """Used in testing."""
        out = []
        while True:
            token = self.nextToken()
            out.append(token)
            if token is EOF:
                return out

    def getAllVariants(self) -> List[TokenVariant]:
        return [t.variant for t in self.getAllTokens()]

def testTokenStream():
    ts = TokenStream(open("test_data/hello_world.fn", 'rb', buffering=0))
    result = ts.getAllVariants()
    tv = TokenVariant
    expected = [
        # let [&CStr] HELLO 
        tv.LET, tv.TYPE_OPEN, tv.REF, tv.NAME, tv.TYPE_CLOSE, tv.NAME,
        # = \"Hello world!\"; 
        tv.EQUAL, tv.ESCAPED_STR, tv.SEMICOLON,
        # fn main{}
        tv.FN, tv.NAME, tv.DATA_OPEN, tv.DATA_CLOSE,
        # -> {} (
        tv.ARROW, tv.DATA_OPEN, tv.DATA_CLOSE, tv.BLOCK_OPEN,
        # fdWriteString {
        tv.NAME, tv.DATA_OPEN,
        # fd = 1; // stdout
        tv.NAME, tv.EQUAL, tv.NAME, tv.SEMICOLON, tv.LINE_COMMENT,
        # str = HELLO;
        tv.NAME, tv.EQUAL, tv.NAME, tv.SEMICOLON,
        # }; )
        tv.DATA_CLOSE, tv.SEMICOLON, tv.BLOCK_CLOSE,
        tv.EOF,
    ]
    assert expected == result


if __name__ == '__main__':
    def printTokens(fo):
        ts = TokenStream(fo)
        token = None
        while token is not EOF:
            token = ts.nextToken()
            print(token)

    printTokens(open(sys.argv[1], 'rb', buffering=0))
