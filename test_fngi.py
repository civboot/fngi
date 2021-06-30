
import unittest
import random
from fngi import BIG_ENDIAN
from fngi import Memory, Heap, Stack, BlockAllocator
from fngi import MHeap, MBlockAllocator as Mba
from fngi import MEMORY_SIZE, BLOCKS_ALLOCATOR_SIZE
from fngi import BLOCK_SIZE, BLOCKS_TOTAL
from fngi import BLOCK_FREE, BLOCK_USED
from fngi import BLOCK_FREE, BLOCK_USED, BLOCK_OOB
from fngi import ARENA_PO2_MIN, ARENA_PO2_MAX
from fngi import Ptr, U8, I8, U16, I16, U32, I32
from fngi import ENV
from fngi import getPo2
from ctypes import sizeof

class TestStack(unittest.TestCase):
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

    def testGetSetI16(self):
        s = self.newStack()
        s.push(I16(0x7008))
        s.push(I16(0x3322))
        assert s.get(4, I16).value == 0x7008
        s.set(4, I16(0x1133))
        assert s.get(4, I16).value == 0x1133
        assert s.get(0, I16).value == 0x3322

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

    def alloc(self) -> int:
        noneFree = (self.numFree == 0)
        bindex = self.ba.alloc()
        if bindex == BLOCK_OOB:
            assert noneFree
        else:
            assert not noneFree
            assert self.freeBlocks[bindex]
            self.numFree -= 1
            self.freeBlocks[bindex] = False
        return bindex

    def free(self, bindex):
        assert not self.freeBlocks[bindex]
        self.ba.free(bindex)
        self.numFree += 1
        self.freeBlocks[bindex] = True

    def getFree(self):
        """Return all free indexes."""
        blocksPtr = self.ba.mba.blocksPtr
        blocki = self.ba.mba.freeRootIndex

        out = set()
        # Just walk the linked list, returning all values
        while blocki != BLOCK_FREE:
            assert blocki not in out
            out.add(blocki)
            blocki = self.ba.memory.get(
                blocksPtr + blocki * sizeof(U16),
                U16).value

        return out


class TestBlockAllocator(unittest.TestCase):
    def setUp(self):
        self.mem = Memory(MEMORY_SIZE)
        self.heap = Heap(self.mem, MHeap.new(0, MEMORY_SIZE))
        self.heap.grow(BLOCK_SIZE) # don't use first block (especially address 0)
        ba_mem = self.heap.grow(BLOCKS_ALLOCATOR_SIZE)
        self.heap_mem = self.heap.heap
        self.heap.mheap = self.heap.push(self.heap.mheap)
        self.ba = BlockAllocator(
            self.mem,
            self.heap.push(Mba(0, ba_mem)))

    def testHeapLocation(self):
        expectedHeap = self.heap_mem + sizeof(MHeap) + sizeof(Mba)
        assert expectedHeap == self.heap.heap

        # Walking through MHeap data inside memory, asserting that it mutated
        start = self.mem.get(self.heap_mem, Ptr).value
        assert 0 == start

        end = self.mem.get(self.heap_mem + 4, Ptr).value
        assert MEMORY_SIZE == end

        heap = self.mem.get(self.heap_mem + 8, Ptr).value
        assert expectedHeap == heap

    def testBlockAlloc_two(self):
        assert 0 == self.ba.mba.freeRootIndex # free root index starts at 0
        assert 1 == self.ba.getBlock(0) # nextFree is 1
        first = self.ba.alloc()
        assert 0 == first

        assert 1 == self.ba.mba.freeRootIndex
        assert 2 == self.ba.getBlock(1) # nextFree is 2
        second = self.ba.alloc()
        assert 1 == second

        assert 2 == self.ba.mba.freeRootIndex

    def testRandomLoop(self):
        """Randomly allocate and free blocks."""
        random.seed(b"blocks are fun")
        bt = BTracker(self.ba)
        allocated = set()

        allocThreshold = 7
        for _ in range(0, 1000):
            if random.randint(0, 10) < allocThreshold:
                # allocate branch
                noneFree = (bt.numFree == 0)
                bi = bt.alloc()
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
                    bt.free(bi)

            if random.randint(0, 10) >= 10:
                bt.getFree() & allocated == set()


class ATracker(object):
    """Arena allocator tracker."""
    def __init__(self, arena):
        self.arena = arena
        self.po2Allocated = {i: [] for i in range(1, ARENA_PO2_MAX + 1)}
        self.allAllocated = []

    def ptrInAllocatedBlocks(self, ptr):
        ba = self.arena.ba
        blocksPtr = ba.mba.blocksPtr
        blocki = self.arena.marena.blockRootIndex

        out = []
        while blocki != BLOCK_USED:
            bPtr = ba.blockToPtr(blocki)
            if bPtr <= ptr < bPtr + BLOCK_SIZE:
                return True
            blocki = ba.getBlock(blocki)
        return False

    def checkArena(self):
        marena = self.arena.marena
        ba = self.arena.ba

        for po2i, ptr in enumerate(self.getPo2Roots()):
            history = []
            while ptr != 0:
                history.append(ptr)
                assert self.ptrInAllocatedBlocks(ptr), ptr
                ptr = ba.memory.get(ptr, Ptr).value

    def assertNoOverlap(self):
        i = 0
        while i < len(self.allAllocated) - 1:
            allocPtr, allocSize = self.allAllocated[i]
            nextallocPtr, _ = self.allAllocated[i+1]
            assert allocPtr + allocSize <= nextAllocPtr

    def alloc(self, po2) -> int:
        print("Allocating", po2)
        ptr = self.arena.alloc(po2)
        print("Allocated", po2, "->", ptr)
        self.checkArena()
        if ptr == 0:
            return

        assert ptr not in self.po2Allocated[po2]
        index = (ptr, 2**min(ARENA_PO2_MIN, po2))

        # assert the pointer doesn't fall into any allocated blocks
        for allocPtr, allocSize in self.allAllocated:
            assert not (allocPtr <= ptr < allocPtr + allocSize)

        self.allAllocated.append(index)
        self.allAllocated.sort(key=lambda a: a[0])
        self.po2Allocated[po2].append(ptr)

        return ptr

    def free(self, po2, ptr) -> int:
        print("Freeing", po2, ptr)
        index = (ptr, 2**min(ARENA_PO2_MIN, po2))
        assert index in self.allAllocated
        assert ptr in self.po2Allocated[po2]
        self.arena.free(po2, ptr)
        self.checkArena()

        self.allAllocated.remove(index)
        self.po2Allocated[po2].remove(ptr)

    def getPo2Roots(self):
        ma = self.arena.marena
        return [ma.po2Roots[i] for i in range(0, ARENA_PO2_MAX - ARENA_PO2_MIN)]


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
        random.seed(b"are you not entertained?")
        sizeMin = 1
        sizeMax = 2**12
        a = ATracker(self.env.arena)
        allocated = set()

        allocThreshold = 7
        for allocatingTry in range(0, 1000):
            print(allocatingTry)
            size = random.randint(sizeMin, sizeMax)
            po2 = getPo2(size)
            if random.randint(0, 10) < allocThreshold:
                # allocate branch
                ptr = a.alloc(po2)
                if ptr == 0:
                    # out of mem, start freeing more
                    allocThreshold -= random.randint(0, 3)
                    allocThreshold = max(1, allocThreshold)
                else:
                    allocated.add(ptr)
            else:
                # free branch
                if allocated:
                    ptr = random.choice(tuple(allocated))
                    a.free(po2, ptr)
                    allocated.discard(ptr)
                else:
                    # cannot free, start allocating more
                    allocThreshold += random.randint(0, 3)
                    allocThreshold = min(allocThreshold, 9)

