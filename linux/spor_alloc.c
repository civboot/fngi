// Author: Garrett Berg (github.com/vitiral/spor_alloc)
// Attribution appreciated but not required, this is community commons software.

#include <stdio.h>
#include <assert.h>
#include "spor_alloc.h"

void BA_init(BlockAllocator* ba) {
  if(ba->cap == 0) return;
  assert(ba->cap < BLOCK_END);
  ba->rooti = 0;
  uint8_t i;
  uint8_t previ = BLOCK_END;
  for (i = 0; i < ba->cap; i += 1) {
    ba->nodes[i].previ = previ;
    ba->nodes[i].nexti = i + 1;
    previ = i;
  }
  ba->nodes[i - 1].nexti = BLOCK_END;
}

// Allocate a block, updating BlockAllocator and client's root indexes.
//
// Go from:
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To (returning block 'd'):
//   clientRoot -> d -> c -> b -> a
//   baRoot     -> e -> f
block_t* BA_alloc(BlockAllocator* ba, uint8_t* clientRooti) {
  uint8_t di = ba->rooti; // index of "d"
  if(di == BLOCK_END) return NULL;

  BlockNode* d = &ba->nodes[di]; // node "d"
  ba->rooti = d->nexti;  // baRoot -> e
  if (d->nexti != BLOCK_END) {
    ba->nodes[d->nexti].previ = BLOCK_END; // baRoot <- e
  }

  assert(d->previ == BLOCK_END); // "d" is already root node
  d->nexti = *clientRooti; // d -> c
  if(*clientRooti != BLOCK_END) {
    ba->nodes[*clientRooti].previ = di;  // d <- c
  }
  *clientRooti = di; // clientRooti -> d
  return &ba->blocks[di]; // return block 'd'
}

// Free a block, updating BlockAllocator and client's root indexes.
//
// Go from (freeing c):
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To:
//   clientRoot -> b -> a
//   baRoot     -> c -> d -> e -> f
void BA_free(BlockAllocator* ba, uint8_t* clientRooti, block_t* b) {
  // Assert block is within blocks memory region
  assert(b >= ba->blocks);
  uint8_t ci = BA_index(ba, b);
  assert(ci < ba->cap);

  BlockNode* c = &ba->nodes[ci]; // node 'c'
  if(ci == *clientRooti) {
    assert(c->previ == BLOCK_END);
    *clientRooti = c->nexti; // clientRoot -> b
    if(c->nexti != BLOCK_END) {
      ba->nodes[c->nexti].previ = BLOCK_END; // clientRoot <- b
    }
  } else { // i.e. b -> c -> d  ===>  b -> d
    ba->nodes[c->previ].nexti = c->nexti;
    ba->nodes[c->nexti].previ = c->previ;
  }

  c->nexti                   = ba->rooti; // c -> d
  ba->nodes[ba->rooti].previ = ci;        // c <- d
  ba->rooti = ci;                         // baRoot -> c
  c->previ = BLOCK_END;                   // baRoot <- c
}

// Free all blocks owned by the client.
//
// Go from:
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To:
//   clientRoot -> END
//   baRoot     -> a -> b -> c -> d -> e -> f
void BA_freeAll(BlockAllocator* ba, uint8_t* clientRooti) {
  while(BLOCK_END != *clientRooti) {
    BA_free(ba, clientRooti, &ba->blocks[*clientRooti]);
  }
}

// Reserve space if too small. Return false if not enough space.
bool _BBA_reserveIfSmall(BlockBumpArena* bba, BBAReturn* out, uint16_t size) {
  if((bba->cap) < (bba->len) + size) {
    if(BLOCK_END != bba->rooti) {
      // Record any leftover memory
      out->leftover = (&bba->ba->blocks[bba->rooti]) + bba->len;
      out->leftoverSize = (bba->cap) - (bba->len);
    }
    if(NULL == BA_alloc(bba->ba, &bba->rooti)) return false;
    bba->len = 0;
    bba->cap = BLOCK_SIZE;
  }
  return true;
}

// Allocate "aligned" data from the top of the block.
//
// WARNING: It is the caller's job to ensure that size is suitably alligned to
// their system width.
BBAReturn BBA_alloc(BlockBumpArena* bba, uint16_t size) {
  BBAReturn out = {0};
  if(!_BBA_reserveIfSmall(bba, &out, size)) return out;
  bba->cap -= size;
  out.data = (&bba->ba->blocks[bba->rooti]) + bba->cap;
  return out;
}

// Allocate "unaligned" data from the bottom of the block.
BBAReturn BBA_allocUnaligned(BlockBumpArena* bba, uint16_t size) {
  BBAReturn out = {0};
  if(!_BBA_reserveIfSmall(bba, &out, size)) return out;
  out.data = (&bba->ba->blocks[bba->rooti]) + bba->len;
  bba->len += size;
  return out;
}
