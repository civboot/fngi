// Spor binary kernel.
// This C file implements spor, see spor.md for documentation.
//
// # Table of Contents
// Search for these headings to find information
//
// * [1] Environment and Test Setup
// * [2] Error (Panic) Handling
// * [2.a] Stacks
// * [2.b] BlockAllocator (BA)
// * [2.c] BlockBumpArena (BBA)
// * [2.d] Binary Search Tree (BST)

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
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

// ***********************
// * [1] Environment and Test Setup
// This sets up the environment and error handling.
//
// In normal fngi code, errors should be returned. However, for critical
// failures code can and will "panic". In C, a panic causes a longjmp to the
// error handler. Panics should be considered extraordinary events, and although
// fngi code can handle them with D_panic, it is not recommended to do so for
// any but the most low-level code or for testing purposes.

jmp_buf* err_jmp = NULL;
#define SET_ERR(E)  if(TRUE) { assert(E); g->err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!g->err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }

U1*       mem;  // start of spor memory
Ref       memSize;
Kern*     k;    // kernel owned data structures
Globals*  g;    // global data structures (global base can move)

void* boundsCheck(U4 size, Ref r) {
  ASM_ASSERT(r, E_null); ASM_ASSERT(r + size <= memSize, E_oob);
  return (void*) ((ssize_t)mem + r);
}

#define asRef(PTR)      ((U1*)(PTR)  - mem)
#define asPtr(TY, REF)  ((TY*)boundsCheck(sizeof(TY), REF))
#define WS              (g->ws)
#define CS              (g->cs)
#define CSZ             (g->csSz)

#define Stk_init(CAP, REF) (Stk) {.ref = REF, .sp = CAP, .cap = CAP}
void initEnv(U4 blocks) {
  memSize = BLOCK_SIZE * blocks;
  mem = malloc(memSize); assert(mem);
  k = (Kern*) mem;
  g = asPtr(Globals,
            sizeof(Kern)
            + (WS_DEPTH + CS_DEPTH) * RSIZE + CS_DEPTH
            + (2 * blocks));
  *k = (Kern)    {0};
  *g = (Globals) {0};

  WS  = Stk_init(WS_DEPTH * RSIZE, sizeof(Kern));
  CS  = Stk_init(CS_DEPTH * RSIZE, WS.ref + WS.cap);
  CSZ = Stk_init(CS_DEPTH        , CS.ref + CS.cap);
  k->ba = (BA) {
    .nodes = CSZ.ref + CSZ.cap, .blocks = BLOCK_SIZE,
    .rooti = BLOCK_END,         .cap = blocks - 1,
  };
  k->bba    = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };
  k->bbaTmp = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };

  g->gbuf = (Buf) {
    .ref = asRef(g), .len = sizeof(Globals), .cap = BLOCK_SIZE };
  g->t = (TokenState) { .ref = asRef(&g->buf0), .size = TOKEN_SIZE };
  g->modBBA = asRef(&k->bba);
  g->tmpBBA = asRef(&k->bbaTmp);
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
  jmp_buf test_err_jmp; \
  eprintf("## %s\n", #NAME); \
  NEW_ENV_BARE(5); \
  if(setjmp(local_err_jmp)) { \
    eprintf("!! Error: %X\n", g->err); exit(1); } \
  else { /*Test code here */

#define TEST_END } ENV_CLEANUP(); }

#define EXPECT_ERR(E, CALL) \
  err_jmp = &test_err_jmp; if(setjmp(test_err_jmp)) \
  { ASSERT_EQ(E, g->err); err_jmp = &local_err_jmp; } \
  else { CALL; UNREACH; }
BARE_TEST(testSetErr)
  EXPECT_ERR(E_intern, SET_ERR(E_intern));
  EXPECT_ERR(E_intern, ASM_ASSERT(false, E_intern));
TEST_END

// ***********************
// * [2] Memory Managers
// Fngi has a few very simple data structures it uses for managing memory. All
// of these work successfully on low-level systems such as microcontrollers with
// zero overhead and no hardware support.
//
// * [2.a] Stacks
// Stacks are the core memory manager for operations (via the working stack
// (WS)) and for executing functions (via the call stack (CS)).
#define WS_POP()         Stk_pop(&g->ws)
#define WS_PUSH(V)       Stk_push(&g->ws, V)
#define ASSERT_WS(E)     ASSERT_EQ(E, WS_POP())

U4 Stk_pop(Stk* stk) {
  ASM_ASSERT(stk->sp + RSIZE <= stk->cap, E_stkUnd);
  U4 out = *((U4*) (mem + stk->sp));
  stk->sp += RSIZE;
  return out;
}

void Stk_push(Stk* stk, U4 value) {
  ASM_ASSERT(stk->sp > 0, E_stkOvr);
  stk->sp -= RSIZE;
  *((U4*) (mem + stk->sp)) = value;
}

BARE_TEST(testStk)
  WS_PUSH(0xA);  ASSERT_WS(0xA);
  WS_PUSH(0x10); WS_PUSH(0x11);
  assert(g->ws.sp == g->ws.cap - 8);
  ASSERT_WS(0x11); ASSERT_WS(0x10);
TEST_END

// ***********************
// * [2.b] BlockAllocator (BA)
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

  ASSERT_EQ(4, k->ba.cap);
  // Check start node: root <-> a
  ASSERT_EQ(BLOCK_END, nodes[0].previ);
  ASSERT_EQ(1        , nodes[0].nexti);

  // Check end node
  ASSERT_EQ(2        , nodes[3].previ);
  ASSERT_EQ(BLOCK_END, nodes[3].nexti);
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
TEST_END

BARE_TEST(testAlloc2FreeFirst)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  uint8_t crooti = BLOCK_END; // clientRoot

  Ref a = BA_alloc(&k->ba, &crooti);
  Ref b = BA_alloc(&k->ba, &crooti); // clientRoot -> b -> a;  baRoot -> c -> d
  ASSERT_EQ(a, k->ba.blocks);        // first block
  ASSERT_EQ(b, BA_block(k->ba, 1));  // second block
  BA_free(&k->ba, &crooti, a); // clientroot -> b -> END;  baRoot -> a -> c -> d

  // clientroot -> b -> END
  ASSERT_EQ(1         , crooti);
  ASSERT_EQ(BLOCK_END , nodes[1].previ);
  ASSERT_EQ(BLOCK_END , nodes[1].nexti);

  // baRoot -> a -> c ...
  ASSERT_EQ(0         , k->ba.rooti);
  ASSERT_EQ(BLOCK_END , nodes[0].previ);
  ASSERT_EQ(2         , nodes[0].nexti);
TEST_END


// ***********************
// * [2.c] BlockBumpArena (BBA)
// For storing code and dictionary entries which reference code, fngi uses a
// block bump arena. This "bumps" memory from the top or bottom of a 4k block,
// but does not allow freeing it. However, the entire arena can be dropped to
// recover all the memory without fragmentation.

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

BARE_TEST(testBBA)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  BBAReturn ret = BBA_alloc(&k->bba, 12);
  ASSERT_EQ(0                               , ret.leftoverSize);
  ASSERT_EQ(k->ba.blocks + BLOCK_SIZE - 12  , ret.data);

  ret = BBA_alloc(&k->bba, BLOCK_SIZE);
  ASSERT_EQ(BLOCK_SIZE - 12                 , ret.leftoverSize);
  ASSERT_EQ(k->ba.blocks                    , ret.leftover);
  ASSERT_EQ(BA_block(k->ba, 1)              , ret.data);

  ret = BBA_allocUnaligned(&k->bba, 13);
  ASSERT_EQ(0                               , ret.leftoverSize);
  ASSERT_EQ(BA_block(k->ba, 2)              , ret.data);

  ret = BBA_allocUnaligned(&k->bba, 25);
  ASSERT_EQ(0                               , ret.leftoverSize);
  ASSERT_EQ(BA_block(k->ba, 2) + 13         , ret.data);

  ret = BBA_allocUnaligned(&k->bba, BLOCK_SIZE - 20);
  ASSERT_EQ(BLOCK_SIZE - 13 - 25            , ret.leftoverSize);
  ASSERT_EQ(BA_block(k->ba, 3)              , ret.data);

  ret = BBA_alloc(&k->bba, BLOCK_SIZE);
  ASSERT_EQ(20                              , ret.leftoverSize);
  ASSERT_EQ(0                               , ret.data);
TEST_END

// ***********************
// * [2.d] Dict Binary Search Tree (Dict)
// The dictionary is a basic unbalancing binary search tree with keys of cdata
// (counted data).  It contains a U4 value and two meta bytes.
//
// The full meaning of the meta bytes are documented in kernel.sp. However, the
// highest bit of m0 is used for distinguishing red vs black.

#define asSlc(CDATA)  (Slc) { .ref = CDATA + 1, .len = *asPtr(U1, CDATA), }

I4 Slc_cmp(Slc l, Slc r) { // return -1 if l<r, 1 if l>r, 0 if eq
  U2 len = l.len;
  if(r.len < l.len) len = r.len;
  U1* lp = asPtr(U1, l.ref);
  U1* rp = asPtr(U1, r.ref);
  for(U2 i = 0; i < len; i += 1) {
    if(*lp < *rp) return -1;
    if(*lp > *rp) return 1;
    lp += 1, rp += 1;
  }
  if(l.len < r.len) return -1;
  if(l.len > r.len) return 1;
  return 0;
}

#define TEST_SLICES \
  Ref c_a = BLOCK_SIZE; \
  Ref c_b = c_a + 4; \
  Ref c_c = c_b + 5; \
  memmove(asPtr(U1, c_a), "\x04" "aaa", 4); \
  memmove(asPtr(U1, c_b), "\x05" "abbd", 5); \
  memmove(asPtr(U1, c_c), "\x04" "abc", 4); \
  Slc a = asSlc(c_a); \
  Slc b = asSlc(c_b); \
  Slc c = asSlc(c_c);

BARE_TEST(testSlc)
  TEST_SLICES

  ASSERT_EQ(4, a.len);
  ASSERT_EQ(5, b.len);
  assert(c_a + 1 == a.ref);

  ASSERT_EQ(0,  Slc_cmp(a, a));
  ASSERT_EQ(-1, Slc_cmp(a, b));
  ASSERT_EQ(-1, Slc_cmp(a, c));
  ASSERT_EQ(1,  Slc_cmp(b, a));
  ASSERT_EQ(1,  Slc_cmp(c, b));
TEST_END

// Find slice in BST, starting at node. Set result to node.
// returns 0 if node==NULL
// The return value is the result of `Slc_cmp(node.ckey, out.ckey)`
I4 Dict_find(DNode** node, Slc slc) {
  if(!*node) return 0;
  while(true) {
    I4 cmp = Slc_cmp(slc, asSlc((*node)->ckey));
    if(cmp == 0) return 0; // found exact match
    if(cmp < 0) {
      if((*node)->l)  *node = asPtr(DNode, (*node)->l); // search left
      else            return cmp; // not found
    } else /* cmp > 0 */ {
      if((*node)->r)  *node = asPtr(DNode, (*node)->r); // search right
      else            return cmp; // not found
    }
  }
}

// Add a node to the tree. WARNING: modifies *node.
void Dict_add(DNode** node, DNode* add) {
  if(!*node) {
    *node = add;
    return;
  }
  I4 cmp = Dict_find(node, asSlc(add->ckey));
  ASM_ASSERT(cmp, E_cKey);
  if(cmp < 0) (*node)->l = asRef(add);
  else        (*node)->r = asRef(add);
  add->l = 0, add->r = 0;
}

BARE_TEST(testDict)
  TEST_SLICES

  DNode* n_a = asPtr(DNode, BLOCK_SIZE + 0x100);
  DNode* n_b = &n_a[1];
  DNode* n_c = &n_a[2];

  DNode* node = NULL;
  Dict_find(&node, b);
  assert(NULL == node);

  *n_a = (DNode) { .ckey = c_a };
  *n_b = (DNode) { .ckey = c_b };
  *n_c = (DNode) { .ckey = c_c };

  node = n_b; ASSERT_EQ( 0, Dict_find(&node, b));    assert(n_b == node); // b found
  node = n_b; ASSERT_EQ(-1, Dict_find(&node, a));    assert(n_b == node); // not found
  node = n_b; ASSERT_EQ( 1, Dict_find(&node, c));    assert(n_b == node); // not found

  node = NULL; Dict_add(&node, n_b); assert(node == n_b);
  node = n_b; Dict_add(&node, n_a);
  node = n_b; ASSERT_EQ( 0, Dict_find(&node, b));    assert(n_b == node); // b found
  node = n_b; ASSERT_EQ( 0, Dict_find(&node, a));    assert(n_a == node); // a found
  node = n_b; ASSERT_EQ( 1, Dict_find(&node, c));    assert(n_b == node); // not found

  node = n_b; Dict_add(&node, n_c);
  node = n_b; ASSERT_EQ( 0, Dict_find(&node, b));    assert(n_b == node); // b found
  node = n_b; ASSERT_EQ( 0, Dict_find(&node, a));    assert(n_a == node); // a found
  node = n_b; ASSERT_EQ( 0, Dict_find(&node, c));    assert(n_c == node); // c found

  ASSERT_EQ(n_b->l, asRef(n_a));
  ASSERT_EQ(n_b->r, asRef(n_c));
TEST_END

int main() {
  eprint("## main\n");
  testSetErr();
  testStk();
  testBANew();
  testAllocFree();
  testAlloc2FreeFirst();
  testBBA();
  testSlc();
  testDict();

  return 0;
}
