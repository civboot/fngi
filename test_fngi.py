
import unittest
import random
from fnpy.types import BIG_ENDIAN
from fnpy.types import Memory, Heap, Stack, BlockAllocator
from fnpy.types import MHeap, MStack, MBlockAllocator as Mba
from fnpy.types import MEMORY_SIZE, BLOCKS_ALLOCATOR_SIZE
from fnpy.types import BLOCK_SIZE, BLOCKS_TOTAL
from fnpy.types import BLOCK_FREE, BLOCK_USED
from fnpy.types import BLOCK_FREE, BLOCK_USED, BLOCK_OOB
from fnpy.types import ARENA_PO2_MIN, BLOCK_PO2
from fnpy.types import Ptr, U8, I8, U16, I16, U32, I32
from fnpy.types import ENV
from fnpy.types import getPo2
from ctypes import sizeof

# update/modify for randomized memory tests
MEM_SEED = "(in the arena) Are you not entertained?"
MEM_LOOPS = int(1e3)

class TestStack(unittest.TestCase):
    def newStack(self):
        size = 16
        return Stack(Memory(size), MStack.new(0, size))

    def testPushPopI16(self):
        s = self.newStack()
        s.push(I16(0x7008))
        assert len(s) == 4
        assert s.pop(I16).value == 0x7008

    def testPushPopI32(self):
        s = self.newStack()
        s.push(I32(0x4200FF))
        assert s.pop(I32).value == 0x4200FF

    def testDirtyStack(self):
        """Test what happens if you have a dirty stack and pop the wrong type
        (don't do this).
        """
        m = Memory(16)
        m.set(4, U32(0x11112222))
        v = m.get(4, U32)
        m.set(4, U16(0xF00F))
        expected = 0xF00F2222 if BIG_ENDIAN else 0x1111F00F
        assert expected == v.value


class BTracker(object):
    """An object to make it easier to track and assert the block allocator.

    Allocations happen through the block tracker, which tracks them and can
    assert on the state.
    """
    def __init__(self, ba):
        self.ba = ba
        self.numFree = BLOCKS_TOTAL
        self.freeBlocks = [True] * BLOCKS_TOTAL

    def getBlock(self, i):
        return self.ba.getBlock(i)

    def setBlock(self, i, v):
        return self.ba.setBlock(i, v)

    def allocBlock(self) -> int:
        noneFree = (self.numFree == 0)
        bindex = self.ba.allocBlock()
        if bindex == BLOCK_OOB:
            assert noneFree
        else:
            assert not noneFree
            assert self.freeBlocks[bindex]
            self.numFree -= 1
            self.freeBlocks[bindex] = False
        return bindex

    def freeBlock(self, bindex):
        assert not self.freeBlocks[bindex]
        self.ba.freeBlock(bindex)
        self.numFree += 1
        self.freeBlocks[bindex] = True

    def getFree(self):
        """Return all free indexes."""
        blocki = self.ba.m.freeRootIndex

        out = set()
        # Just walk the linked list, returning all values
        while blocki != BLOCK_FREE:
            assert blocki not in out
            out.add(blocki)
            blocki = self.ba.getBlock(blocki)

        return out


class TestBlockAllocator(unittest.TestCase):
    def setUp(self):
        self.mem = Memory(MEMORY_SIZE)
        self.heap = Heap(self.mem, MHeap.new(0, MEMORY_SIZE))
        self.heap.grow(BLOCK_SIZE) # don't use first block (especially address 0)
        ba_mem = self.heap.grow(BLOCKS_ALLOCATOR_SIZE)
        self.heap_mem = self.heap.m.heap
        self.heap.m = self.heap.push(self.heap.m)
        self.ba = BlockAllocator(
            self.mem,
            self.heap.push(Mba(0, ba_mem)))

    def testHeapLocation(self):
        expectedHeap = self.heap_mem + sizeof(MHeap) + sizeof(Mba)
        assert expectedHeap == self.heap.m.heap

        # Walking through MHeap data inside memory, asserting that it mutated
        start = self.mem.get(self.heap_mem, Ptr).value
        assert 0 == start

        end = self.mem.get(self.heap_mem + 4, Ptr).value
        assert MEMORY_SIZE == end

        heap = self.mem.get(self.heap_mem + 8, Ptr).value
        assert expectedHeap == heap

    def testBlockAlloc_two(self):
        assert 0 == self.ba.m.freeRootIndex # free root index starts at 0
        assert 1 == self.ba.getBlock(0) # nextFree is 1
        first = self.ba.allocBlock()
        assert 0 == first

        assert 1 == self.ba.m.freeRootIndex
        assert 2 == self.ba.getBlock(1) # nextFree is 2
        second = self.ba.allocBlock()
        assert 1 == second

        assert 2 == self.ba.m.freeRootIndex

    def testRandomLoop(self):
        """Randomly allocate and free blocks."""
        random.seed(MEM_SEED)
        bt = BTracker(self.ba)
        allocated = set()

        allocThreshold = 7
        for _ in range(0, MEM_LOOPS):
            if random.randint(0, 10) < allocThreshold:
                # allocate branch
                noneFree = (bt.numFree == 0)
                bi = bt.allocBlock()
                if noneFree:
                    assert BLOCK_OOB == bi
                    # out of blocks, start freeing more
                    allocThreshold -= random.randint(0, 3)
                    allocThreshold = min(1, allocThreshold)
                else:
                    assert bi not in allocated
                    allocated.add(bi)
            else:
                # free branch
                if len(allocated) == 0:
                    # cannot free, start allocating more
                    allocThreshold += random.randint(0, 3)
                    allocThreshold = max(allocThreshold, 9)
                else:
                    bi = random.choice(tuple(allocated))
                    allocated.discard(bi)
                    bt.freeBlock(bi)

            if random.randint(0, 10) >= 10:
                bt.getFree() & allocated == set()



class ATracker(object):
    """Arena allocator tracker."""
    def __init__(self, arena):
        self.arena = arena
        self.po2Allocated = {i: set() for i in range(0, BLOCK_PO2 + 1)}
        self.allAllocated = []

    def ptrInAllocatedBlocks(self, ptr):
        ba = self.arena.ba
        blocki = self.arena.marena.blockRootIndex

        while True:
            assert blocki != BLOCK_FREE, "somehow followed free LL"
            bPtr = ba.blockToPtr(blocki)
            if bPtr <= ptr < bPtr + BLOCK_SIZE:
                return True
            if blocki == BLOCK_USED:
                break
            blocki = ba.getBlock(blocki)
        return False

    def checkArena(self):
        marena = self.arena.marena
        ba = self.arena.ba

        for po2i, ptr in enumerate(self.getPo2Roots()):
            while ptr != 0:
                assert self.ptrInAllocatedBlocks(ptr), ptr
                ptr = ba.memory.get(ptr, Ptr).value

    def assertNoOverlap(self):
        i = 0
        while i < len(self.allAllocated) - 1:
            allocPtr, allocSize = self.allAllocated[i]
            nextallocPtr, _ = self.allAllocated[i+1]
            assert allocPtr + allocSize <= nextAllocPtr

    def alloc(self, po2) -> int:
        ptr = self.arena.alloc(po2)
        self.checkArena()
        if ptr == 0:
            return

        assert ptr not in self.po2Allocated[po2]
        index = (ptr, 2**max(ARENA_PO2_MIN, po2))

        # assert the pointer doesn't fall into any allocated blocks
        ba = self.arena.ba
        for allocPtr, allocSize in self.allAllocated:
            assert not (allocPtr <= ptr < (allocPtr + allocSize))

        self.allAllocated.append(index)
        self.allAllocated.sort(key=lambda a: a[0])
        self.po2Allocated[po2].add(ptr)

        return ptr

    def free(self, po2, ptr) -> int:
        index = (ptr, 2**max(ARENA_PO2_MIN, po2))
        assert index in self.allAllocated
        assert ptr in self.po2Allocated[po2]
        self.arena.free(po2, ptr)
        self.checkArena()

        self.allAllocated.remove(index)
        self.po2Allocated[po2].discard(ptr)

    def getPo2Roots(self):
        ma = self.arena.marena
        return [ma.po2Roots[i] for i in range(0, BLOCK_PO2 - ARENA_PO2_MIN)]


class TestArena(unittest.TestCase):
    def setUp(self):
        self.env = ENV.copyForTest()
        self.ma = self.env.arena.marena
        self.at = ATracker(self.env.arena)

    def assertRootsEmpty(self):
        roots = self.at.getPo2Roots()
        assert [0] * 9 == roots

    def testInit(self):
        self.assertRootsEmpty()

    def testAllocBlock(self):
        arena = self.env.arena
        at = self.at
        ptr = at.alloc(12)

        self.assertRootsEmpty()

        blocki = self.env.ba.ptrToBlock(ptr)
        assert 0 == blocki
        assert arena.marena.blockRootIndex == blocki
        assert self.env.ba.getBlock(blocki) == BLOCK_USED
        at.free(12, ptr)
        assert self.env.ba.getBlock(blocki) == 1

    def testAllocSmall(self):
        ptr = self.at.alloc(3)
        assert ptr != 0
        roots = self.at.getPo2Roots()

        self.at.assertNoOverlap()
        blocki = self.env.ba.ptrToBlock(ptr)
        assert (
            [blocki] * 9
            == [self.env.ba.ptrToBlockInside(p) for p in roots]
        )
        self.at.free(3, ptr)

    def testRandomLoop(self):
        random.seed(MEM_SEED)
        sizeMin = 1
        sizeMax = 2**12
        a = ATracker(self.env.arena)
        allocated = []

        totalBytesAllocated = 0
        totalBytesFreed = 0

        allocThreshold = 7
        maxPo2 = 0
        for allocatingTry in range(0, MEM_LOOPS):
            size = random.randint(sizeMin, sizeMax)
            po2 = BLOCK_PO2 - getPo2(size)
            maxPo2 = max(maxPo2, po2)
            if random.randint(0, 10) < allocThreshold:
                # allocate branch
                ptr = a.alloc(po2)
                if ptr == 0:
                    # out of mem, start freeing more
                    allocThreshold -= random.randint(0, 3)
                    allocThreshold = max(1, allocThreshold)
                else:
                    allocated.append((po2, ptr))
                    totalBytesAllocated += 2**po2
            else:
                # free branch
                if allocated:
                    i = random.randint(0, len(allocated) - 1)
                    po2, ptr = allocated[i]
                    a.free(po2, ptr)
                    del allocated[i]
                    totalBytesFreed += 2**po2
                else:
                    # cannot free, start allocating more
                    allocThreshold += random.randint(0, 3)
                    allocThreshold = min(allocThreshold, 9)

        assert maxPo2 == 12

        print("Total Allocated:", totalBytesAllocated, "  Total Freed:", totalBytesFreed)

