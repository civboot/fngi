// Spor binary kernel.
// This C file implements spor, see spor.md for documentation.

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <assert.h>
#include <setjmp.h>
#include <stdbool.h>

#include "constants.h"
#include "kernel.h"

#define eprint(str)                fprintf (stderr, str)
#define eprintf(format, args...)   fprintf (stderr, format, args)
#define UNREACH   if(1) { eprint("!!! unreachable\n"); assert(false); }
#define ASSERT_EQ(E, CODE) if(1) { \
  U4 r = CODE; \
  if((E) != r) eprintf("!!! Assertion failed: 0x%X == 0x%X\n", E, r); \
  assert((E) == r); }

// Access of spor-local memory

// ***********************
// ** Memory, Environment and Test Setup
U1*       mem;
Kern*     k;
Globals*  g;

#define asRef(PTR)      ((U1*)(PTR)  - mem)
#define asPtr(TY, REF)  ((TY*)((ssize_t)mem + (REF)))
#define WS              (g->ws)
#define CS              (g->cs)
#define CSZ             (g->csSz)

#define Stk_init(CAP, REF) (Stk) {.ref = REF, .sp = CAP, .cap = CAP}
void initEnv(U4 blocks) {
  mem = malloc(BLOCK_SIZE * blocks); assert(mem);
  k = asPtr(Kern, 0);
  g = asPtr(
      Globals,
      sizeof(Kern)
      + (WS_DEPTH + CS_DEPTH) * RSIZE + CS_DEPTH
      + (2 * blocks)
  );
  *k = (Kern)    {0};
  *g = (Globals) {0};
  WS  = Stk_init(WS_DEPTH * RSIZE, sizeof(Kern));
  CS  = Stk_init(CS_DEPTH * RSIZE, WS.ref + WS.cap);
  CSZ = Stk_init(CS_DEPTH        , CS.ref + CS.cap);
  k->ba = (BA) {
    .nodes = CSZ.ref + CSZ.cap, .blocks = BLOCK_SIZE,
    .rooti = BLOCK_END,         .cap = blocks - 1,
  };
  k->bba = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };
  g->gbuf = (Buf) {
    .ref = asRef(g), .len = sizeof(Globals), .cap = BLOCK_SIZE };
  g->t = (TokenState) { .ref = asRef(&g->buf0), .size = TOKEN_SIZE };
}

// Memory layout (all in first block):
// Kern | ws data | cs data | csSz data | bba.nodes | Globals...
#define NEW_ENV_BARE(BLOCKS) \
  jmp_buf local_err_jmp; \
  err_jmp = &local_err_jmp; \
  initEnv(BLOCKS);

#define ENV_CLEANUP()               \
    err_jmp = NULL;                 \
    free(mem)

#define BARE_TEST(NAME)  void NAME() {  \
  eprintf("## %s\n", #NAME);                                          \
  NEW_ENV_BARE(10);                                                   \

#define TEST_END \
  ENV_CLEANUP(); }


// ***********************
// * Error (Panic) Handling
// Spor/Fngi's errors are "panics", where a panic causes a longjmp to the error
// handler. Panics should be considered extraordinary events, and although
// fngi code can handle them with D_panic, it is not recommended to do so for
// any but the most low-level code.
//
// During testing however, panics are expectd. We therefore add functionality
// for any test to expect a panic, and macros to make it more convienient.

jmp_buf* err_jmp = NULL;
#define SET_ERR(E)  if(TRUE) { assert(E); g->err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!g->err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }
#define EXPECT_ERR(E, CALL)    if(setjmp(local_err_jmp)) \
  { ASSERT_EQ(E, g->err); } else { CALL; UNREACH; }

BARE_TEST(testSetErr)
  EXPECT_ERR(E_intern, SET_ERR(E_intern));
  EXPECT_ERR(E_intern, ASM_ASSERT(false, E_intern));
TEST_END

// ***********************
// * Stacks
// Stacks are the core data type for fngi and include the WS (working stack) and
// CS (call stack).
#define WS_POP()         Stk_pop(&g->ws)
#define WS_PUSH(V)       Stk_push(&g->ws, V)
#define ASSERT_WS(E)     ASSERT_EQ(E, WS_POP())

U4 Stk_pop(Stk* stk) {
  ASM_ASSERT(stk->sp + RSIZE <= stk->cap, E_stkUnd);
  U4 out = *asPtr(U4, stk->sp);
  stk->sp += RSIZE;
  return out;
}

void Stk_push(Stk* stk, U4 value) {
  ASM_ASSERT(stk->sp > 0, E_stkOvr);
  stk->sp -= RSIZE;
  *asPtr(U4, stk->sp) = value;
}

BARE_TEST(testStk)
  WS_PUSH(0xA);  ASSERT_WS(0xA);
  WS_PUSH(0x10); WS_PUSH(0x11);
  assert(g->ws.sp == g->ws.cap - 8);
  ASSERT_WS(0x11); ASSERT_WS(0x10);
TEST_END

// ***********************
// * Kernel Allocators: BlockAllocator (BA) and BlockBumpArena (BBA)
// fngi utilizes 4KiB blocks of memory for many things including storing both
// the code and dictionary of it's module system. The kernel allocators are
// extremely lightweight but permit for dropable modules and code without memory
// fragmentation.
//
// For more documentation see github.com/vitiral/spor_alloc

#define BA_block(BA, BI) ((BA).blocks + ((BI) << BLOCK_PO2))
#define BA_index(BA, BLOCK)   (((BLOCK) - (BA).blocks) >> BLOCK_PO2)
#define BBA_ba(bba) asPtr(BA, (bba).ba)

void BA_init(BA* ba) {
  if(ba->cap == 0) return;
  ASM_ASSERT(ba->cap < BLOCK_END, E_intern);
  BANode* nodes = asPtr(BANode, ba->nodes);
  ba->rooti = 0;
  uint8_t i;
  uint8_t previ = BLOCK_END;
  for (i = 0; i < ba->cap; i += 1) {
    nodes[i].previ = previ;
    nodes[i].nexti = i + 1;
    previ = i;
  }
  nodes[i - 1].nexti = BLOCK_END;
}

// Allocate a block, updating BlockAllocator and client's root indexes.
//
// Go from:
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To (returning block 'd'):
//   clientRoot -> d -> c -> b -> a
//   baRoot     -> e -> f
Ref BA_alloc(BA* ba, U1* clientRooti) {
  uint8_t di = ba->rooti; // index of "d"
  if(di == BLOCK_END) return 0;

  BANode* nodes = asPtr(BANode, ba->nodes);
  BANode* d = &nodes[di]; // node "d"
  ba->rooti = d->nexti;  // baRoot -> e
  if (d->nexti != BLOCK_END) {
    nodes[d->nexti].previ = BLOCK_END; // baRoot <- e
  }

  ASM_ASSERT(d->previ == BLOCK_END, E_intern); // "d" is already root node
  d->nexti = *clientRooti; // d -> c
  if(*clientRooti != BLOCK_END) {
    nodes[*clientRooti].previ = di;  // d <- c
  }
  *clientRooti = di; // clientRooti -> d
  return BA_block(*ba, di); // return block 'd'
}

// Free a block, updating BlockAllocator and client's root indexes.
//
// Go from (freeing c):
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To:
//   clientRoot -> b -> a
//   baRoot     -> c -> d -> e -> f
void BA_free(BA* ba, uint8_t* clientRooti, Ref b) {
  // Assert block is within blocks memory region
  ASM_ASSERT(b >= ba->blocks, E_oob);
  uint8_t ci = BA_index(*ba, b);
  ASM_ASSERT(ci < ba->cap, E_oob);

  BANode* nodes = asPtr(BANode, ba->nodes);
  BANode* c = &nodes[ci]; // node 'c'
  if(ci == *clientRooti) {
    ASM_ASSERT(c->previ == BLOCK_END, E_intern);
    *clientRooti = c->nexti; // clientRoot -> b
    if(c->nexti != BLOCK_END) {
      nodes[c->nexti].previ = BLOCK_END; // clientRoot <- b
    }
  } else { // i.e. b -> c -> d  ===>  b -> d
    nodes[c->previ].nexti = c->nexti;
    nodes[c->nexti].previ = c->previ;
  }

  c->nexti               = ba->rooti; // c -> d
  nodes[ba->rooti].previ = ci;        // c <- d
  ba->rooti              = ci;        // baRoot -> c
  c->previ               = BLOCK_END; // baRoot <- c
}

// Free all blocks owned by the client.
//
// Go from:
//   clientRoot -> c -> b -> a
//   baRoot     -> d -> e -> f
// To:
//   clientRoot -> END
//   baRoot     -> a -> b -> c -> d -> e -> f
void BA_freeAll(BA* ba, uint8_t* clientRooti) {
  while(BLOCK_END != *clientRooti) {
    BA_free(ba, clientRooti, BA_block(*ba, *clientRooti));
  }
}

BARE_TEST(testBANew)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);

  ASSERT_EQ(9, k->ba.cap);
  // Check start node: root <-> a
  ASSERT_EQ(BLOCK_END, nodes[0].previ);
  ASSERT_EQ(1        , nodes[0].nexti);

  // Check end node
  ASSERT_EQ(7        , nodes[8].previ);
  ASSERT_EQ(BLOCK_END, nodes[8].nexti);
TEST_END

BARE_TEST(testAllocFree)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  uint8_t crooti = BLOCK_END; // clientRoot

  Ref a = BA_alloc(&k->ba, &crooti);
  ASSERT_EQ(k->ba.blocks, a); // first block

  // // clientroot -> a
  ASSERT_EQ(0         , crooti);
  ASSERT_EQ(BLOCK_END , nodes[0].previ);
  ASSERT_EQ(BLOCK_END , nodes[0].nexti);

  // // baRoot -> b -> c
  ASSERT_EQ(1         , k->ba.rooti);
  ASSERT_EQ(BLOCK_END , nodes[1].previ);
  ASSERT_EQ(2         , nodes[1].nexti);

  BA_free(&k->ba, &crooti, a);
  ASSERT_EQ(BLOCK_END , crooti);
  ASSERT_EQ(BLOCK_END , nodes[0].previ);
  ASSERT_EQ(1         , nodes[0].nexti);
  ASSERT_EQ(0         , nodes[1].previ);
}

BARE_TEST(testAlloc2FreeFirst)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  uint8_t crooti = BLOCK_END; // clientRoot

  Ref a = BA_alloc(&k->ba, &crooti);
  Ref b = BA_alloc(&k->ba, &crooti); // clientRoot -> b -> a;  baRoot -> c -> d
  ASSERT_EQ(a , k->ba.blocks);          // first block
  ASSERT_EQ(b , BA_block(k->ba, 1)); // second block
  BA_free(&k->ba, &crooti, a); // clientroot -> b -> END;  baRoot -> a -> c -> d

  // clientroot -> b -> END
  ASSERT_EQ(1         , crooti);
  ASSERT_EQ(BLOCK_END , nodes[1].previ);
  ASSERT_EQ(BLOCK_END , nodes[1].nexti);

  // baRoot -> a -> c ...
  ASSERT_EQ(0         , k->ba.rooti);
  ASSERT_EQ(BLOCK_END , nodes[0].previ);
  ASSERT_EQ(2         , nodes[0].nexti);
}


// Reserve space if too small. Return false if not enough space.
bool _BBA_reserveIfSmall(BBA* bba, BBAReturn* out, U2 size) {
  if((bba->cap) < (bba->len) + size) {
    if(BLOCK_END != bba->rooti) {
      // Record any leftover memory
      out->leftover = BA_block(*BBA_ba(*bba), bba->rooti) + bba->len;
      out->leftoverSize = (bba->cap) - (bba->len);
    }
    if(0 == BA_alloc(BBA_ba(*bba), &bba->rooti)) return false;
    bba->len = 0;
    bba->cap = BLOCK_SIZE;
  }
  return true;
}

// Allocate "aligned" data from the top of the block.
//
// WARNING: It is the caller's job to ensure that size is suitably alligned to
// their system width.
BBAReturn BBA_alloc(BBA* bba, U2 size) {
  BBAReturn out = {0};
  if(!_BBA_reserveIfSmall(bba, &out, size)) return out;
  bba->cap -= size;
  out.data = BA_block(*BBA_ba(*bba), bba->rooti) + bba->cap;
  return out;
}

// Allocate "unaligned" data from the bottom of the block.
BBAReturn BBA_allocUnaligned(BBA* bba, uint16_t size) {
  BBAReturn out = {0};
  if(!_BBA_reserveIfSmall(bba, &out, size)) return out;
  out.data = BA_block(*BBA_ba(*bba), bba->rooti) + bba->len;
  bba->len += size;
  return out;
}

int main() {
  eprint("## main\n");
  testSetErr();
  testStk();
  testBANew();
  testAllocFree();
  testAlloc2FreeFirst();

  return 0;
}
