import unittest
import random

from fngi import Memory, Heap, BlockAllocator
from fngi import MHeap, MBlockAllocator as Mba
from fngi import MEMORY_SIZE, BLOCKS_ALLOCATOR_SIZE
from fngi import BLOCK_SIZE, BLOCKS_TOTAL
from fngi import BLOCK_FREE, BLOCK_USED
from fngi import BLOCK_FREE, BLOCK_USED, BLOCK_OOB
from fngi import Ptr
from ctypes import sizeof

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
        assert 0 == self.ba.freeRootIndex # free root index starts at 0
        assert 1 == self.ba._getBlock(0) # nextFree is 1
        first = self.ba.alloc()
        assert 0 == first

        assert 1 == self.ba.freeRootIndex
        assert 2 == self.ba._getBlock(1) # nextFree is 2
        second = self.ba.alloc()
        assert 1 == second

        assert 2 == self.ba.freeRootIndex

    def testBlockAlloc(self):
        """Randomly allocate and free blocks."""
        random.seed(b"fun times")
        bt = BTracker(self.ba)
        allocated = []

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
                else:
                    allocated.append(bi)
            else:
                # free branch
                if len(allocated) == 0:
                    # cannot free, start allocating more
                    allocThreshold += random.randint(0, 3)
                else:
                    ai = random.randint(0, len(allocated) - 1)
                    bi = allocated.pop(ai)
                    bt.free(bi)




class BTracker(object):
    """An object to make it easier to track and assert on the block allocator.

    Allocations happen through the block tracker, which tracks them and can
    assert on the state.
    """
    def __init__(self, mba):
        self.mba = mba
        self.numFree = BLOCKS_TOTAL
        self.freeBlocks = [True] * BLOCKS_TOTAL

    def alloc(self) -> int:
        noneFree = (self.numFree == 0)
        bindex = self.mba.alloc()
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
        self.mba.free(bindex)
        self.numFree += 1
        self.freeBlocks[bindex] = True

    def getFree(self):
        """Return all free indexes."""
        blocksPtr = self.mba.blocksPtr
        blocki = self.mba.freeRootIndex

        out = []
        # Just walk the linked list, returning all values
        while blocki != BLOCK_FREE:
            out.append(blocki)
            blocki = self.memory.get(
                blocksPtr + blocki * sizeof(U16),
                U16).value

        return out
