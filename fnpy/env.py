# The fnpy environment such as stack, memory, native functions, etc.

from pdb import set_trace as dbg

import abc
import sys
import struct
import ctypes
import copy
import inspect

from ctypes import sizeof

from .wasm import *

# Compute the index into a list for a stack representation
def _si(i): return -i - 1

class PyStack(list):
    """
    A pure python stack. It's really just a list where the indexes are
    reversed. This means:
    - pushing puts the value at s[0], moving the other values up.
    - popping gets the value at s[0] and removes it.
    - printing shows the stack in the correct order.
    """
    def __init__(self, data=None):
        if data: data = reversed(data)
        super().__init__(data)

    def __repr__(self):
        return f"Stk{list(reversed(self))}"

    def _getslice(self, sl):
        assert not sl.step, "not supported"
        # reverse sl and make them negative
        start = None if sl.start is None else _si(sl.start)
        stop = None if sl.stop is None else _si(sl.stop)
        return slice(start, stop, -1)

    def __getitem__(self, index):
        if isinstance(index, slice):
            return super().__getitem__(self._getslice(index))
        else:
            return super().__getitem__(_si(index))

    def __delitem__(self, index):
        return super().__delitem__(_si(index))

    def __setitem__(self, index, value):
        if isinstance(index, slice):
            return super().__setitem__(self._getslice(index), value)
        else:
            return super().__setitem__(_si(index), value)

    def insert(self, index, value):
        return super().insert(_si(index), value)

    def push(self, value):
        return super().append(value)

    def assertEq(self, expectedList):
        assert expectedList == list(reversed(self))

def testPyStack():
    s = PyStack(range(10))
    assert 0 == s.pop()
    s.push(0)
    assert 0 == s[0]
    assert 2 == s[2]
    assert 9 == s[-1]
    assert [0,1,2] == s[:3]
    assert [3,4] == s[3:5]
    del s[3]
    s.assertEq([0,1,2,4,5,6,7,8,9])

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
# Env.ds (what most Native functions use) whereas struct values
# are done entirely on env.returnStack (i.e. where local variables are stored).
#
# The goal with this is to more closely resemble real assembly or a lower-level
# language, whether that be RISC/CISC registers in assembly or stack-based
# assembly like wasm or an underlying Forth language/cpu.

def fieldOffset(ty: DataTy, field: str):
    return getattr(getDataTy(ty), field).offset

def fieldSize(ty: DataTy, field: str):
    return getattr(getDataTy(cls), field).size


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
# Most data in fngi is passed in either the ds or is a pointer into
# global memory.
#
# The ds is NOT within global memory, and therefore cannot have a
# pointer into it. This is because on some platforms it is stored in registers
# (or a register stack) instead of memory.
#
# The other data regions (returnStack, heap, allocators, etc) are all slices of the
# global memory region (ENV.heap.memory).

KiB = 2**10
MiB = 2**20
DATA_STACK_SIZE = 8 * sizeof(Ptr)
CODE_HEAP_SIZE = 32 * KiB
BLOCKS_ALLOCATOR_SIZE = (5 * MiB) // 2
EXTRA_HEAP_SIZE = 1 * MiB
RETURN_STACK_SIZE = MiB // 2 # 1/2 MiB return stack

# 4KiB is the size of a "block" of memory, the maximum amount that
# can be allocatd without growing a heap.
BLOCK_PO2 = 12
BLOCK_SIZE = 2**BLOCK_PO2
BLOCKS_TOTAL = BLOCKS_ALLOCATOR_SIZE // BLOCK_SIZE
BLOCKS_INDEXES_SIZE = BLOCKS_TOTAL * sizeof(U16)
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

    def setGet(self, ptr, value: DataTy):
        self.set(ptr, value)
        self.get(ptr, value.__class__)

    def getArray(self, ptr: int, ty: DataTy, length: int):
        arrayTy = ty * length
        return self.get(ptr, arrayTy)

    def setArray(self, ptr: int, values: List[DataTy]):
        if len(values) == 0:
            return

        ty = getDataTy(values[0])
        arrayTy = ty * len(values)
        arrayValue = arrayTy(*values)
        self.set(ptr, arrayValue)

    def checkRange(self, ptr: int, size: int = 0):
        if ptr <= 0 or (ptr + size) > len(self.data):
            raise IndexError("ptr={} memorySize={}".format(
                ptr, len(self.data)))


class MManBase(object):
    """Base class for memory manager objects.

    These must define an `m` attribute which is the memory object that needs to
    be moved to the new memory region during copy.
    """

    def copyForTest(self, memory: Memory):
        mptr = self.memory.getPtrTo(self.m)
        return self.__class__(
            memory,
            memory.get(mptr, getDataTy(self.m)))


class MHeap(ctypes.Structure):
    """A heap as represented in memory."""
    _fields_ = [
        ('start', Ptr), # the start of heap's memory
        ('end', Ptr),   # the end of heap's memory
        ('heap', Ptr),  # the heap pointer
    ]

    @classmethod
    def new(cls, start, end):
        return cls(start, end, heap=start)


class Heap(MManBase):
    """The heap grows up."""
    def __init__(self, memory, mheap):
        self.memory = memory
        self.m = mheap

    def checkRange(self, ptr, size):
        if ptr < self.m.start or ptr + size >= self.m.end:
            raise IndexError(
                    "start={} end={} ptr={} size={}".format(self.m.start, self.m.end, ptr, size))

    def grow(self, size, align=True):
        """Grow the heap, return the beginning of the grown region."""
        size = size + (needAlign(size) if align else 0)
        self.checkRange(self.m.heap, size)
        out = self.m.heap
        self.m.heap += size
        return out

    def shrink(self, size, align=True):
        size = size + (needAlign(size) if align else 0)
        self.checkRange(self.m.heap - size, size)
        self.m.heap -= size

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


class FngiStack(MManBase):
    """Stack implementation, used for the data stack and the return stack.

    Stacks grow down, and typically are kept on alignment.
    """
    def __init__(self, memory: Memory, mstack: MStack):
        self.memory = memory
        self.m = mstack
        self.totalSize = mstack.end - mstack.start
        self.tys = [] # List of tys on the stack. Last value is "top" of the stack.

    def clearMemory(self):
        self.m.sp = self.m.end
        self.tys = []

    def checkRange(self, offset, size, useSp=None, requireSize=False):
        if not useSp: useSp = self.m.sp
        ptr = useSp + offset
        assert useSp % 4 == 0
        if not (useSp <= ptr < self.m.end):
            raise IndexError(
                f"offset={offset} (ptr={hex(ptr)}) does not fit"
                + f" inside sp={hex(useSp)} end={hex(self.m.end)}")
        if not (self.m.start <= useSp <= self.m.end):
            raise IndexError(
                f"sp={hex(useSp)} is not between"
                + f" [{hex(self.m.start)}, {hex(self.m.end)}")

        if requireSize and size != 4 and size != 8:
            raise ValueError("size must be 4 or 8 to push onto stack")

    # Set / Push
    def set(self, offset: int, value: DataTy):
        """Set a value at an offset from the sp.

        Note: this does NOT check that the type matches.
        """
        self.checkRange(offset, sizeof(value), requireSize=False)
        self.memory.set(self.m.sp + offset, value)

    def push(self, value: DataTy):
        assert isinstance(value, DataTy)
        size = sizeof(value)
        self.checkRange(0, size, self.m.sp - size)
        self.m.sp -= size
        self.memory.set(self.m.sp, value)
        self.tys.append(type(value))

    # Get / Pop

    def get(self, offset, ty: DataTy) -> bytes:
        """Get a value at an offset from the sp."""
        ty = getDataTy(ty)
        self.checkRange(offset, sizeof(ty), requireSize=False)
        return self.memory.get(self.m.sp + offset, ty)

    def pop(self, ty: DataTy) -> DataTy:
        assert issubclass(ty, DataTy)
        size = sizeof(ty)
        self.checkRange(0, size)
        if size == 8 and not self.trackSize[-1]:
            raise IndexError("Trying to pop wrong size from stack.")
        out = self.memory.getCopy(self.m.sp, ty)
        self.tys.pop()
        self.m.sp += size
        return out

    def popv(self, ty: DataTy) -> any:
        return self.pop(ty).value

    def drop(self) -> DataTy:
        """Drop a value of either 4 or 8 bytes (depending on what is next).

        This returns the value so it can be more easily used with select
        """
        ty = self.trackSize[-1]
        self.checkRange(0, sizeof(ty))
        return self.pop(ty)

    def select(self):
        """Select a value based on the top of the stack {check; v2; v1}
        if check do v1 eldo v2
        Yes, wasm is as confusing as possible.
        """
        if len(self.trackSize) < 3: raise IndexError("select requires len >= 3")
        if sizeof(self.trackSize[-2]) != sizeof(self.trackSize[-3]):
            raise IndexError(
                f"select trackSize could change ty size: {self.trackSize[-3:-1]}")
        check = self.drop()
        v2 = self.drop()
        v1 = self.drop()
        self.push(v1 if check.value else v2)

    def grow(self, tys: List[DataTy]):
        """Grow by the types, leaving data uninizilized.

        The tys must have index 0 be the top of the stack, index -1 the bottom.
        """
        size = sum(map(sizeof, tys))
        self.checkRange(0, size, useSp=self.sp - size, requireSize=False)
        self.trackSize.extend(reversed(tys))
        self.sp -= size

    def shrink(self, tys: List[DataTy]):
        """Shrink by the types, asserting correct sizes."""
        size = sum(map(sizeof, tys))
        self.checkRange(0, size, requireSize=False)
        for i in range(tys):
            if sizeof(tys[i]) != sizeof(self.tys[i]):
                raise ValueError(f"size {tys[i]} != {self.tys[i]}")
        del self.trackSize[-len(tys):]
        self.sp += size

    def debugStr(self):
        return 'Stack: {}'.format(
            ' '.join(hex(self.get(i, I32).value) for i in range(0, len(self), 4)))

    def __len__(self):
        return self.m.end - self.m.sp

    def __repr__(self):
        return "STACK<{}/{}>".format(len(self), self.totalSize)


#########################################
# Memory Manager
#
# Our parser will be building an Abstract Syntax Tree, which is basically a
# tree consisting of unknown sized (aka allocated) nodes of various types. The
# AST can either be parsed from the source code OR generated dynamically by
# macros, so there will be a lot of need for non-static memory allocation.
#
# Trying to do this without a memory manager would be extremely annoying to say
# the least. Furthermore, once we have built the bootstrapping compiler we will
# want to build out the rest of the language and have it compile itself, and
# for that we definitely need a memory manager.
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
# restrictions on the programs supported by our compiler and std library. A
# more "full featured" std library can be written for systems with more
# features.
# - Memory can only be allocated in up to 4k (2**12) byte blocks
# - Allocation size must be a power of 2, with a minimum size of 8 (2**3)
# - It is the program's/compiler's job (aka not the memory manager's job) to
#   track the size of it's pointers. The allocator does not know the size of a
#   pointer. This means that unlike the "standard C" function signature
#   of `free(ptr)` in fngi it is `free(ptrPo2, ptr)`
# - Multiple allocators are permitted, with arenas being preferred for most
#   cases. An arena is an object with alloc/free methods, but unlike a "global
#   allocator" the entire arena can be dropped. Since the max block size is 4k,
#   this makes sure that all "holes" are at least 4k in size and so there will
#   rarely be fragmentation below 4k in size.
# - There is no global allocator except for heap.grow(), heap.shrink() and a
#   a single global arena instance (which can be used to create child arenas).
#
# Note: tests for the allocators can be found in test_allocator.py

BLOCK_USED = 0x8000 # sentinel for last used block in a linked-list
BLOCK_FREE = 0xE4EE # sentinal for last free block in a linked-list
BLOCK_OOB = 0xFFFF # block allocator is Out Of Blocks


class MBlockAllocator(ctypes.Structure):
    _fields_ = [
        ('freeRootIndex', U16),
        ('blocksPtr', Ptr),
        ('memPtr', Ptr),
    ]

class BlockAllocator(MManBase):
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
        self.m = mba
        for i in range(0, BLOCKS_TOTAL - 1):
            self.setBlock(i, i+1)
        self.setBlock(BLOCKS_TOTAL - 1, BLOCK_FREE)
        self.blocksFree = BLOCKS_TOTAL

    def blocksAlloc(self):
        return BLOCKS_TOTAL - self.blocksFree

    def blocksEnd(self) -> int:
        return self.m.memPtr + BLOCKS_ALLOCATOR_SIZE

    def alloc(self, po2: int) -> int:
        if po2 != BLOCK_PO2: raise ValueError(po2)
        return self.blockToPtr(self.allocBlock())

    def allocBlock(self) -> int:
        """Return the index to a free block, or BLOCK_OOB."""
        # An "indexed linked list" pop
        # FROM: root -> a -> b -> ...
        #   TO: root -> b -> ...
        # RETURN: ptr to a (value that was in root)
        i = self.m.freeRootIndex
        if i == BLOCK_FREE:
            if self.blocksFree != 0:
                raise RuntimeError("Failed to detect OOM")
            return BLOCK_OOB

        self.m.freeRootIndex = self.getBlock(i)
        self.blocksFree -= 1
        return i

    def free(self, po2: int, ptr: int):
        if po2 != BLOCK_PO2: raise ValueError(po2)
        self.freeBlock(self.ptrToBlock(ptr))

    def freeBlock(self, i):
        if self.blocksFree >= BLOCKS_TOTAL:
            raise ValueError("invalid free")
        # An "indexed linked list" push
        # FROM: root -> a -> b -> ...
        #   TO: root -> i -> a -> b -> ...
        self.checkPtr(self.blockToPtr(i))
        # Set: i -> a
        self.setBlock(i, self.m.freeRootIndex)
        # Set: root -> i
        self.m.freeRootIndex = i
        self.blocksFree += 1

    def checkPtr(self, ptr):
        ptrAlign = ptr % BLOCK_SIZE 
        if (ptr < self.m.memPtr
                or ptr >= self.m.memPtr + BLOCKS_ALLOCATOR_SIZE
                or ptrAlign != 0):
            raise IndexError("ptr not managed: ptr={}, ptrAlign={} start={} end={}".format(
                ptr, ptrAlign, self.m.memPtr, self.blocksEnd()))

    def blockToPtr(self, block: int):
        if block == BLOCK_OOB:
            return 0
        ptr = self.m.memPtr + (block * BLOCK_SIZE)
        self.checkPtr(ptr)
        return ptr

    def ptrToBlock(self, ptr: int):
        self.checkPtr(ptr)
        return (ptr - self.m.memPtr) // BLOCK_SIZE

    def ptrToBlockInside(self, ptr: int):
        return self.ptrToBlock(
            self.blockToPtr((ptr - self.m.memPtr) // BLOCK_SIZE))

    def getBlock(self, i):
        """Get the value in the blocks array"""
        out = self.memory.get(self.m.blocksPtr + i * sizeof(U16), U16).value
        return out

    def setBlock(self, i, value):
        """Set the value in the blocks array"""
        assert i != value, "never valid to have the block point to itself"
        self.memory.set(self.m.blocksPtr + i * sizeof(U16), U16(value))

    def allBlockIndexes(self):
        return [self.getBlock(i) for i in range(BLOCKS_TOTAL)]


def joinMem(ptr1: int, ptr2: int, size: int):
    """If mem can be joined, return the lowest pointer. Else return 0"""
    if ptr1 == 0 or ptr2 == 0:
        return 0

    if ptr2 < ptr1:
        ptr1, ptr2 = ptr2, ptr1

    if ptr1 + size == ptr2:
        return ptr1

    return 0


def testJoinMem():
    assert 0 == joinMem(4, 16, 4)
    assert 4 == joinMem(4, 8, 4)
    assert 4 == joinMem(8, 4, 4)

    assert 102 == joinMem(104, 102, 2)
    assert 0x400 == joinMem(0x400, 0x500, 0x100)
    assert 0x400 == joinMem(0x500, 0x400, 0x100)
    assert 0 == joinMem(0x600, 0x400, 0x100)


def getPo2(size: int) -> int:
    if size <= 0:
        raise ValueError(size)

    x = 1
    exponent = 0
    while True:
        if x >= size:
            return exponent
        x = x * 2
        exponent += 1

# array of the 8 po2 roots (2**3, 2**4, ... 2**11)
ARENA_PO2_MIN = 3
MArenaPo2Roots = U32 * (BLOCK_PO2 - ARENA_PO2_MIN)

class MArena(ctypes.Structure):
    _fields_ = [
        ('baPtr', Ptr), # block allocator ptr
        ('blockRootIndex', U16), # root index for allocated blocks (inside baPtr's blocks)
        ('_align', U16), # alignment
        ('po2Roots', MArenaPo2Roots),
    ]

ARENA_STRUCT_PO2 = getPo2(sizeof(MArena))
assert 6 == ARENA_STRUCT_PO2 # MArena takes 40 bytes, 2**6 (=64) byte region


class Arena(object):
    """For the arena allocator we use a few tricks for storing free regions of
    memory. Please note that this allocator is designed to be a prototype of
    one written in forth wich can run on microcontrollers, so it uses extremely
    low level concepts (i.e. raw pointers stored inside of freed blocks)

    The arena allocator can allocate memory in powers of 2, from 8 (2**3) bytes
    up to 4k (2**12) bytes. It keeps track of free blocks of memory by using an
    array of linked lists, where each index in the array is 4+po2. Unlike the
    BlockAllocator, the pointers to the "next free node" is kept _within the
    allocateable memory itself_. This allows each arena allocator to have only
    ~64 bytes of memory overhead.

    The arena keeps track of all the 4KiB blocks it is using by keeping a LL in
    the BlockAllocator's blocks array. This is the exact same method that the
    block allocator itself uses to track it's free blocks (difference being the
    arena is tracking the allocated blocks instead of free ones). This allows
    all of the blocks the arena is using to be freed when the arena is dropped.

    The algorithm our arena allocator uses for small allocations is called a
    "buddy allocator", although this is a very peculiar variant. Blocks are
    allocated by po2 (power of 2). If a free block is not available it is
    requested from the next-highest po2, which will ask from the next highest,
    etc. When a block is found, it will be split in half repeatedly (storing
    the unused half) until it is the correct size. When a block is freed,
    merging will be attempted on the next available free block. Both alloc and
    free run in O(1) time (approximately 10 to 100 "executions" depending on
    how many blocks must be split/merged).

    We allow allocating 2^3 to 2^12 size blocks (9 sizes). 8 of these have a
    linked list, while size 2^12 uses a single index into the block allocator.

    The allocator is generally performant, although freeing blocks can
    potentially be slow (requires traversing the block LL to pop it). It is
    recommended to directly use the block allocator if you know your data will
    require a whole block.
    """
    def __init__(self,
            parent: 'Arena',
            memory: Memory,
            blockAllocator: BlockAllocator,
            marena: MArena):
        self.parent = parent
        self.memory = memory
        self.ba = blockAllocator
        self.marena = marena

    @classmethod
    def new(cls, parent: 'Arena', aId: int):
        """Allocate a new arena from the parent."""
        # Note: The aId (arena id) is a sentinel allowing investigation of the
        # block allocator in postmortems. Where it appears in the ba.blocks it
        # is known that it should be the last element of this arena's allocated
        # blocks linked-list.
        memory = parent.memory
        ba = parent.ba

        baPtr = memory.getPtrTo(ba.m)

        arenaPtr = parent.alloc(ARENA_STRUCT_PO2)
        memory.set(
            arenaPtr,
            MArena(
                baPtr=baPtr,
                blockRootIndex=U16(BLOCK_USED | aId).value,
                _align=0,
                po2Roots=MArenaPo2Roots(*[0 for _ in range(8)])))
        marena = memory.get(arenaPtr, MArena)
        return cls(parent, memory, ba, marena)

    def copyForTest(self, memory: Memory, ba: BlockAllocator):
        mptr = self.memory.getPtrTo(self.marena)
        return self.__class__(
            None,
            memory,
            ba,
            memory.get(mptr, getDataTy(self.marena)))

    def allocBlock(self) -> int:
        """Allocate a block, return the pointer to it."""
        bi = self.ba.allocBlock()
        if bi == BLOCK_OOB:
            return 0

        self.ba.setBlock(bi, self.marena.blockRootIndex)
        self.marena.blockRootIndex = bi
        return self.ba.blockToPtr(bi)

    def freeBlock(self, ptr) -> int:
        bindex = self.ba.ptrToBlock(ptr)
        # find the bindex in the LL and pop it
        if bindex == self.marena.blockRootIndex:
            # from: (root) bindex -> a -> b -> ...
            # to  : (root) a -> b -> ...
            self.marena.blockRootIndex = self.ba.getBlock(bindex)
        else:
            # from: root -> a -> ... t -> w -> bindex -> x
            #   to: root -> a -> ... t -> w -> x
            # Trying to find w.
            w = self.marena.blockRootIndex
            # w is not w until it points to bindex
            while True:
                wPointsTo = self.ba.getBlock(w)
                if wPointsTo == bindex:
                    break
                if wPointsTo & BLOCK_USED == BLOCK_USED:
                    raise ValueError('could not find block')
                w = wPointsTo
            self.ba.setBlock(w, self.ba.getBlock(wPointsTo))

        self.ba.freeBlock(bindex)

    def pushFreePo2(self, po2, ptr):
        oldRoot = self.getPo2Root(po2)
        self.memory.set(ptr, Ptr(oldRoot))
        self.setPo2Root(po2, ptr)

    def popFreePo2(self, po2):
        if po2 == BLOCK_PO2:
            return self.allocBlock()
        freeMem = self.getPo2Root(po2)
        if freeMem != 0:
            # Pop item from linked list
            self.setPo2Root(po2, self.memory.get(freeMem, Ptr))
        return freeMem

    def alloc(self, wantPo2: int):
        if wantPo2 > 12:
            raise ValueError("wantPo2=" + str(wantPo2))
        wantPo2 = self._realPo2(wantPo2)
        po2 = wantPo2
        freeMem = 0

        # Find a block of size greather than or equal to wantPo2
        while True:
            freeMem = self.popFreePo2(po2)
            if freeMem != 0 or po2 == BLOCK_PO2:
                break
            po2 += 1

        if freeMem == 0:
            return 0 # no memory found

        # Split the found block of memory until it is the right size
        while True:
            if po2 == wantPo2:
                return freeMem

            # split freeMem in half, storing the leftover region
            po2 -= 1
            extraMem = freeMem + (2**po2)
            self.pushFreePo2(po2, extraMem)

    def free(self, po2: int, ptr: int):
        po2 = self._realPo2(po2)
        while True:
            if po2 == BLOCK_PO2:
                return self.freeBlock(ptr)

            joinedMem = joinMem(ptr, self.getPo2Root(po2), 2**po2)
            if joinedMem == 0:
                return self.pushFreePo2(po2, ptr)
            else:
                self.popFreePo2(po2) # remove root, we are joining it
                ptr = joinedMem
                # then try to join the next largest po2
                po2 += 1

    def getPo2Root(self, po2):
        return self.marena.po2Roots[po2 - ARENA_PO2_MIN]

    def setPo2Root(self, po2, ptr):
        po2i = po2 - ARENA_PO2_MIN
        self.marena.po2Roots[po2i] = ptr

    @staticmethod
    def _realPo2(po2):
        return max(ARENA_PO2_MIN, po2)


# We now come to the "global" environment. This contains all the memory which
# functions (native or user, we'll get to that) and the compiler itself mutate
# while compiling and executing fngi code. As we'll see, fngi will be
# simultaniously executing while compiling, as we will write a large amount of
# even the stage0 compiler in the fngi language itself.
#
# Memory layout:
# CODE_HEAP: location where "code" and native fn indexes go.
#   note: memory address 0 has something special written.
# BLOCK_ALLOCATOR: location for block allocator to use. The arena allocator
#   uses blocks, so this also includes the arena allocator.
# EXTRA_HEAP: some extra heap space to put global variables and data such as
#   the global allocators, user-defined globals, compiler state, etc.
# RETURN_STACK: the return stack
MEMORY_SIZE = (
    CODE_HEAP_SIZE
    + BLOCKS_ALLOCATOR_SIZE
    + EXTRA_HEAP_SIZE
    + RETURN_STACK_SIZE)

MEMORY = Memory(MEMORY_SIZE)
MEMORY.data[0:4] = b'\xA0\xDE\xFE\xC7' # address 0 is "A DEFECT"

HEAP = Heap(MEMORY, MHeap.new(0, MEMORY_SIZE - RETURN_STACK_SIZE))
CODE_HEAP_MEM = 4 + HEAP.grow(CODE_HEAP_SIZE) # not ptr=0
BLOCK_ALLOCATOR_MEM = HEAP.grow(BLOCKS_ALLOCATOR_SIZE)
BLOCK_INDEXES_MEM = HEAP.grow(BLOCKS_INDEXES_SIZE)
REAL_HEAP_START = HEAP.m.heap
RETURN_STACK_MEM = REAL_HEAP_START + EXTRA_HEAP_SIZE


# Now that we have the _space_ for all of our memory regions, we need to
# actually keep the mutations to them synced within the main memory block.
#
# Why go to this trouble? This means that if we re-implement this language
# according to this binary spec, the binary output should match _exactly_
# at all points of execution. This will make testing and debugging much easier.
# For example, we can write tests that compile a fngi program using both python
# and forth compilers and assert they are identical. Also, this means we can
# write debugging tools in python and use them on our Forth implementation.

# Data stack kept in separate memory region
DATA_STACK = FngiStack(
    Memory(DATA_STACK_SIZE + 4),
    MStack.new(4, DATA_STACK_SIZE))

# Reserve a space for the HEAP.m data inside of MEMORY that automatically
# mutates when HEAP.m is mutated. Do the same for all other values.
HEAP.m = HEAP.push(HEAP.m)
CODE_HEAP = Heap(
    MEMORY,
    HEAP.push(MHeap.new(
        CODE_HEAP_MEM, CODE_HEAP_MEM + CODE_HEAP_SIZE)))

BLOCK_ALLOCATOR = BlockAllocator(
    MEMORY,
    HEAP.push(MBlockAllocator(0, BLOCK_INDEXES_MEM, BLOCK_ALLOCATOR_MEM)))

ARENA = Arena( # global arena
    None,
    MEMORY,
    BLOCK_ALLOCATOR,
    HEAP.push(MArena(
        baPtr=MEMORY.getPtrTo(BLOCK_ALLOCATOR.m),
        blockRootIndex=BLOCK_USED,
        _align=0,
        po2Roots=MArenaPo2Roots(*[0 for _ in range(8)]))))

RETURN_STACK_MEM_END = RETURN_STACK_MEM + RETURN_STACK_SIZE
RETURN_STACK = FngiStack(
    MEMORY,
    HEAP.push(MStack.new(RETURN_STACK_MEM, RETURN_STACK_MEM_END)))

# Data stack kept in separate memory region
DATA_STACK = FngiStack(
    Memory(DATA_STACK_SIZE),
    MStack.new(0, DATA_STACK_SIZE))


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
            ds: FngiStack,
            returnStack: FngiStack,
            heap: Heap,
            codeHeap: Heap,
            ba: BlockAllocator,
            arena: Arena,
            fns: List[Fn],
            tys: Dict[str, Ty],
            refs: Dict[Ty, Ref],
            ):
        self.memory = memory
        self.ds = ds
        self.heap = heap
        self.codeHeap = codeHeap
        self.ba = ba
        self.arena = arena
        self.returnStack = returnStack
        self.fns = fns
        self.fnIndexes = None

        self.tys = tys
        self.refs = refs

        self.executingFn = None
        self.running = False
        self.rbp = 0 # rstack base ptr
        self.ep = 0 # execution pointer

    def indexFns(self):
        fnIndexes = {}
        for index, fn in enumerate(self.fns):
            if fn.name is None: continue
            if fn.name in fnIndexes: raise KeyError(f"Multiple fns named {fn.name}")
            fnIndexes[fn.name()] = index
        self.fnIndexes = fnIndexes

    def copyForTest(self):
        """Copy the env for the test. Sets ep=heap.heap and running=True."""
        m = copy.deepcopy(self.memory)
        ds = copy.deepcopy(self.ds)

        heap = self.heap.copyForTest(m)
        codeHeap = self.codeHeap.copyForTest(m)
        ba = self.ba.copyForTest(m)
        arena = self.arena.copyForTest(m, ba)
        returnStack = self.returnStack.copyForTest(m)

        out = Env(
            memory=m,
            ds=ds,
            returnStack=returnStack,
            heap=heap,
            codeHeap=codeHeap,
            ba=ba,
            arena=arena,
            fns=copy.deepcopy(self.fns),
            tys=copy.deepcopy(self.tys),
            refs=copy.deepcopy(self.refs))

        out.running = True
        out.ep = self.heap.m.heap
        return out

    def clearMemory(self):
        self.memory.data[:] = ctypes.create_string_buffer(len(self.memory.data))
        self.ds.clearMemory()
        if self.returnStack: self.returnStack.clearMemory()

def createWasmEnv(memoryPages=1) -> Env:
    """Create a clean wasm environment for testing wasm."""
    memSize = memoryPages * WASM_PAGE
    mem = Memory(memSize)
    # Unlike fngi, we use a comparibly large data stack.
    dataStack = FngiStack(
        Memory(WASM_PAGE),
        MStack.new(4, WASM_PAGE)
    )
    # Unlike fngi, the return stack must be a separate memory region because
    # some tests will have specific pointers within main memory.
    # Note: the returnStack is used for inputs and locals, which in wasm
    # cannot traditionally have pointers to them.
    returnStack = FngiStack(
        Memory(WASM_PAGE),
        MStack.new(4, WASM_PAGE)
    )

    return Env(
        memory=mem,
        ds=dataStack,
        returnStack=returnStack,
        heap=None, codeHeap=None, ba=None, arena=None,
        fns=[],
        tys=None, refs=None,
    )

ENV = Env(
    memory=MEMORY,
    ds=DATA_STACK,
    returnStack=RETURN_STACK,
    heap=HEAP,
    codeHeap=CODE_HEAP,
    ba=BLOCK_ALLOCATOR,
    arena=ARENA,
    fns=[],
    tys={},
    refs={},
)

def testMemoryLayout():
    _testMemoryLayout(ENV)

def _testMemoryLayout(env: Env):
    reservedSpace = CODE_HEAP_SIZE + BLOCKS_ALLOCATOR_SIZE + BLOCKS_INDEXES_SIZE
    assert reservedSpace == env.memory.getPtrTo(env.heap.m)
    assert reservedSpace == REAL_HEAP_START

    # Assert current heap location
    assert 0 == env.heap.m.start
    expected = reservedSpace
    expected += (
        2 * sizeof(MHeap) 
        + sizeof(MBlockAllocator) 
        + sizeof(MStack)
        + sizeof(MArena)
    )
    assert expected == env.heap.m.heap

    # Block allocator
    assert CODE_HEAP_SIZE == env.ba.m.memPtr
    assert CODE_HEAP_SIZE + BLOCKS_ALLOCATOR_SIZE == env.ba.m.blocksPtr
    assert 0 == env.ba.m.freeRootIndex
    expected = reservedSpace + 2 * sizeof(MHeap)
    assert expected == env.memory.getPtrTo(env.ba.m)

def testEnvCopy():
    env = ENV.copyForTest()
    assert env.memory is not ENV.memory
    assert env.memory.data is not ENV.memory.data
    assert bytes(env.memory.data) == bytes(ENV.memory.data)
    assert env.ds.m is not ENV.ds.m
    assert env.heap.m is not ENV.heap.m
    assert env.ba.m is not ENV.ba.m
    assert env.arena.marena is not ENV.arena.marena
    assert env.returnStack.m is not ENV.returnStack.m

    _testMemoryLayout(env)


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
