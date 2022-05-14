// Author: Garrett Berg (github.com/vitiral/spor_alloc)
// Attribution appreciated but not required, this is community commons software.
// 

#ifndef __SPOR_ALLOC_H
#define __SPOR_ALLOC_H

#include <stdbool.h>
#include <stdint.h>

// BLOCK_END:
// - When used in previ: this is root node
// - When used in nexti: this is last node
// - When used in rooti: there are no nodes
#define BLOCK_END  0xFF

// Size of a block (4kiB)
#define BLOCK_PO2  12
#define BLOCK_SIZE (1<<BLOCK_PO2)

typedef uint8_t block_t[BLOCK_SIZE];
typedef struct { uint8_t previ; uint8_t nexti; } BlockNode;

// **********************
// ** BlockAllocator (BA) Architecture
//
// Below is the block allocator struct. It stores an array (length `cap`)
// of 4KiB blocks (block_t) and an array of the same size of BlockNodes.
//
// The BlockNode array is conceptually a doubly-linked list. `previ` and `nexti`
// can contain either an index into nodes (and also blocks) or the value
// BLOCK_END, which is documented above.
//
// BA_alloc and BA_free are how memory is allocated and freed, and it
// simultaniously maintains the client's index of allocated memory, allowing
// the client to drop all their allocated memory blocks if/when they are finished.

typedef struct {
  BlockNode* nodes;
  block_t*   blocks;
  uint8_t    cap;     // number of blocks
  uint8_t    rooti;   // root index
} BlockAllocator;

// **********************
// ** BlockBumpArena (BBA) Architecture
//
// Arena Allocator: allows dropping the entire allocator, freeing all memory it
//   owns.
//
// Bump Allocator: simply increments a "heap" like pointer and returns the
//   memory, with no mechanism to free it. It is by far the simplest and fastest
//   allocator architecture possible, with the downside that memory can never be
//   freed. Since this is an arena allocator, it adds some complexity but allows
//   a whole arena's worth of memory to be freed at once.

typedef struct {
  BlockAllocator* ba;
  uint8_t         rooti; // root owned block (currently allocating from)
  uint16_t        len;      // lower (unaligned) used bytes
  uint16_t        cap;      // upper (aligned) used bytes
} BlockBumpArena;

typedef struct {
  void*    data;            // data of the size requested (or NULL)
  void*    leftover;        // leftover data
  uint16_t leftoverSize;
} BBAReturn;

#define  BA_index(BA, BLOCK)   ( \
    ((uint8_t*) (BLOCK) - (uint8_t*) (BA)->blocks) \
    >> BLOCK_PO2)
void     BA_init     (BlockAllocator* a);
block_t* BA_alloc    (BlockAllocator* a, uint8_t* clientRooti);
void     BA_free     (BlockAllocator* a, uint8_t* clientRooti, block_t* b);
void     BA_freeAll  (BlockAllocator* a, uint8_t* clientRooti);

#define    BBA_new(BA) { .ba=BA, .rooti=BLOCK_END, .len=0, .cap=0 };
BBAReturn  BBA_alloc           (BlockBumpArena* bba, uint16_t size);
BBAReturn  BBA_allocUnaligned  (BlockBumpArena* bba, uint16_t size);
#define    BBA_drop(BBA)       BA_freeAll((BBA).ba, &(BBA).rooti)

// Convience macro to malloc new block allocator. Remember to free it!
#define BA_new(BA, NUM_BLOCKS)                                  \
    (BA).cap = NUM_BLOCKS;                                      \
    (BA).nodes = malloc((NUM_BLOCKS) << 1); /* NUM_BLOCKS*2 */  \
    (BA).blocks = malloc((NUM_BLOCKS) << BLOCK_PO2);            \
    BA_init(&(BA));

// Convienience macro to drop block allocator reserved using malloc.
#define BA_drop(BA)         \
    free((BA).nodes);       \
    free((BA).blocks);      \
    BA.cap = 0;             \
    BA.rooti = BLOCK_END;

#endif // __SPOR_ALLOC_H
