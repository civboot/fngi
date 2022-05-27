// Spor binary kernel.
// This C file implements spor, see spor.md for documentation.
//
// # Table of Contents
// Search for these headings to find information
//
// * 1: Environment and Test Setup
// * 1.a: Panic Handling
// * 1.b: Environment Setup
//
// * 2: Memory Managers and Data Structures
// * 2.a: Stacks
// * 2.b: BlockAllocator (BA)
// * 2.c: BlockBumpArena (BBA)
// * 2.d: Slc (slice) Data Structure
// * 2.e: Dict Binary Search Tree
//
// * 3: Executing Instructions
// * 3.a: Utilities
// * 3.b: Functions
// * 3.c: Giant Switch Statement
//
// * 4: Scanner

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
#define R  return;
#define UNREACH   if(1) { eprint("!!! unreachable\n"); assert(false); }
#define ASSERT_EQ(E, CODE) if(1) { \
  U4 r = CODE; \
  if((E) != r) eprintf("!!! Assertion failed: 0x%X == 0x%X\n", E, r); \
  assert((E) == r); }

// ***********************
// * 1: Environment and Test Setup
// This sets up the environment and tests.


// *********
// * 1.a: Panic Handling
// In normal fngi code, errors should be returned. However, for critical
// failures code can and will "panic". In C, a panic causes a longjmp to the
// error handler. Panics should be considered extraordinary events, and although
// fngi code can handle them with D_panic, it is not recommended to do so for
// any but the most low-level code or for testing purposes.

jmp_buf* err_jmp = NULL;
#define SET_ERR(E)  if(TRUE) { assert(E); g->err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!g->err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }

// *********
// * 1.b: Environment Setup
// This provides helpful functions and macros for setting up the environment.
//
// The initial environment has a somewhat complicated memory layout. The basics
// are (1) everything is in 4k blocks, (2) everything except for the local stack
// data is in the first block. The layout of "everything" is below:
//
//   Kern | ws data | cs data | csSz data | bba.nodes | Globals ... room to grow
U1        *mem, *memEnd;  // start/end of spor memory
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
#define LS              (g->ls)
#define CS              (g->cs)
#define CSZ             (g->csSz)
#define LS_SP           (g->ls.ref + g->ls.sp)
#define CS_SP           (g->cs.ref + g->cs.sp)

#define Stk_init(CAP, REF) (Stk) {.ref = REF, .sp = CAP, .cap = CAP}
void initEnv(U4 blocks) {
  memSize = BLOCK_SIZE * blocks;
  mem = malloc(memSize); assert(mem); memEnd = mem + memSize;
  k = (Kern*) mem;
  g = asPtr(Globals,
            sizeof(Kern)
            + (WS_DEPTH + CS_DEPTH) * RSIZE + CS_DEPTH
            + (2 * blocks));
  *k = (Kern)    {0};
  *g = (Globals) {0};

  WS  = Stk_init(WS_DEPTH * RSIZE, sizeof(Kern));
  WS  = Stk_init(BLOCK_SIZE, BLOCK_SIZE);
  CS  = Stk_init(CS_DEPTH * RSIZE, WS.ref + WS.cap);
  CSZ = Stk_init(CS_DEPTH        , CS.ref + CS.cap);
  k->ba = (BA) {
    .nodes = CSZ.ref + CSZ.cap, .blocks = BLOCK_SIZE * 2,
    .rooti = BLOCK_END,         .cap = blocks - 2,
  };
  k->bba    = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };
  k->bbaTmp = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };

  g->gbuf = (Buf) {
    .ref = asRef(g), .len = sizeof(Globals), .cap = BLOCK_SIZE };
  g->t = (TokenState) { .ref = asRef(&g->buf0), .size = TOKEN_SIZE };
  g->modBBA = asRef(&k->bba);
  g->tmpBBA = asRef(&k->bbaTmp);
}

#define NEW_ENV_BARE(BLOCKS) \
  jmp_buf local_err_jmp; \
  err_jmp = &local_err_jmp; \
  initEnv(BLOCKS);

#define ENV_CLEANUP()               \
    err_jmp = NULL;                 \
    free(mem)

#define BARE_TEST(NAME, BLOCKS)  void NAME() {  \
  jmp_buf test_err_jmp; \
  eprintf("## %s\n", #NAME); \
  NEW_ENV_BARE(BLOCKS); \
  if(setjmp(local_err_jmp)) { \
    eprintf("!! Error: %X\n", g->err); exit(1); } \
  else { /*Test code here */

#define TEST_END   } ENV_CLEANUP(); }

#define EXPECT_ERR(E, CALL) \
  err_jmp = &test_err_jmp; if(setjmp(test_err_jmp)) \
  { ASSERT_EQ(E, g->err); err_jmp = &local_err_jmp; } \
  else { CALL; UNREACH; }
BARE_TEST(testSetErr, 2)
  EXPECT_ERR(E_intern, SET_ERR(E_intern));
  EXPECT_ERR(E_intern, ASM_ASSERT(false, E_intern));
TEST_END

// ***********************
// * 2: Memory Managers and Data Structures
// Fngi has a few very simple data structures it uses for managing memory. All
// of these work successfully on low-level systems such as microcontrollers with
// zero overhead and no hardware support.

// *********
// * 2.a: Stacks
// Stacks are the core memory manager for operations (via the working stack
// (WS)) and for executing functions (via the call stack (CS)).
#define Stk_len(S)       (((S).size - (S).sp) >> RPO2)
#define WS_POP()         Stk_pop(&WS)
#define WS_PUSH(V)       Stk_push(&WS, V)
#define CS_PUSH(V)       Stk_push(&CS, V)

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

// Return value of ASCII hex char (or 0xFF if not a hex character)
/*fn*/ U1 charToHex(U1 c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('A' <= c && c <= 'F') return 10 + c - 'A';
  if ('a' <= c && c <= 'f') return 10 + c - 'a';
  return 0xFF;
}

BARE_TEST(testStk, 2)
  WS_PUSH(0xA);  ASSERT_WS(0xA);
  WS_PUSH(0x10); WS_PUSH(0x11);
  assert(g->ws.sp == g->ws.cap - 8);
  ASSERT_WS(0x11); ASSERT_WS(0x10);
  ASSERT_EQ(0,   charToHex('0'));    ASSERT_EQ(9,   charToHex('9'));
  ASSERT_EQ(0xA, charToHex('A'));    ASSERT_EQ(0xF, charToHex('F'));
  ASSERT_EQ(0xA, charToHex('a'));    ASSERT_EQ(0xF, charToHex('f'));
  ASSERT_EQ(0xFF, charToHex('R'));
TEST_END

// *********
// * 2.b: BlockAllocator (BA)
// fngi utilizes 4KiB blocks of memory for many things including storing both
// the code and dictionary of it's module system. The kernel allocators are
// extremely lightweight but support dropable modules (and their code) without
// memory fragmentation.
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

BARE_TEST(testBANew, 6)
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

BARE_TEST(testAllocFree, 6)
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

BARE_TEST(testAlloc2FreeFirst, 6)
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


// *********
// * 2.c: BlockBumpArena (BBA)
// For storing code and dictionary entries which reference code, fngi uses a
// block bump arena. This "bumps" memory from the top (for aligned) or bottom of
// a 4k block, but does not allow freeing it. However, the entire arena can be
// dropped to recover all the memory without fragmentation.

bool _BBA_reserveIfSmall(BBA* bba, U2 size) {
  if((bba->cap) < (bba->len) + size) {
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
Ref BBA_alloc(BBA* bba, U2 size) {
  if(!_BBA_reserveIfSmall(bba, size)) return 0;
  bba->cap -= size;
  return BA_block(*BBA_ba(*bba), bba->rooti) + bba->cap;
}

// Allocate "unaligned" data from the bottom of the block.
Ref BBA_allocUnaligned(BBA* bba, uint16_t size) {
  if(!_BBA_reserveIfSmall(bba, size)) return 0;
  Ref out = BA_block(*BBA_ba(*bba), bba->rooti) + bba->len;
  bba->len += size;
  return out;
}

BARE_TEST(testBBA, 6)
  BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  ASSERT_EQ(k->ba.blocks + BLOCK_SIZE - 12  , BBA_alloc(&k->bba, 12));
  ASSERT_EQ(BA_block(k->ba, 1)              , BBA_alloc(&k->bba, BLOCK_SIZE));
  ASSERT_EQ(BA_block(k->ba, 2)              , BBA_allocUnaligned(&k->bba, 13));
  ASSERT_EQ(BA_block(k->ba, 2) + 13         , BBA_allocUnaligned(&k->bba, 25));
  ASSERT_EQ(BA_block(k->ba, 3)              , BBA_allocUnaligned(&k->bba, BLOCK_SIZE - 20));
  ASSERT_EQ(0                               , BBA_alloc(&k->bba, BLOCK_SIZE));
TEST_END

// *********
// * 2.d: Slc (slice) Data Structure
// One of the core types in fngi is the Slc (slice) and it's child the Buf
// (buffer). A Slc is simply a reference and a U2 len (length). A Buf adds on a
// capacity, allowing for the data to grow. Note that a reference to a Buf is
// also a valid reference to a Slc.
//
// The other core type is cdata, often just represented as "c". This is counted
// data where the first byte has the count (length).
#define cAsSlc(CDATA)  asSlc(CDATA + 1, *asPtr(U1, CDATA))

Slc asSlc(Ref ref, U2 len) {
  ASM_ASSERT(ref, E_null); ASM_ASSERT(ref + len < memSize, E_oob);
  return (Slc) {.ref = ref, .len = len};
}

I4 Slc_cmp(Slc l, Slc r) { // return -1 if l<r, 1 if l>r, 0 if eq
  U2 len; if(l.len < r.len) len = l.len;  else len = r.len;
  U1 *lp = mem + l.ref, *rp = mem + r.ref;
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
  Slc a = cAsSlc(c_a); \
  Slc b = cAsSlc(c_b); \
  Slc c = cAsSlc(c_c);

BARE_TEST(testSlc, 5)
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


// *********
// * 2.e: Dict Binary Search Tree
// The dictionary is a basic unbalanced binary search tree with keys of cdata.
// It contains a U4 value and some metadata necessary for distinguishing between
// the kinds of values (see kernel/constants.sp).

// Find slice in BST, starting at node. Set result to node.
// returns 0 if node==NULL
// The return value is the result of `Slc_cmp(node.ckey, out.ckey)`
I4 Dict_find(DNode** node, Slc slc) {
  if(!*node) return 0;
  while(true) {
    I4 cmp = Slc_cmp(slc, cAsSlc((*node)->ckey));
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
  I4 cmp = Dict_find(node, cAsSlc(add->ckey));
  ASM_ASSERT(cmp, E_cKey);
  if(cmp < 0) (*node)->l = asRef(add);
  else        (*node)->r = asRef(add);
  add->l = 0, add->r = 0;
}

BARE_TEST(testDict, 2)
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

// ***********************
// * 3: Executing Instructions
// Fngi's assembly is defined in kernel/constants.sp. These constants are
// auto-generated into constants.h, which are imported here.
//
// The VM executes instruction bytecode in the fngi memory space, utilizing
// the fngi globals like CS and WS.
#define sectorRef(R)  (/*join with ep's sector*/ (0xFFFF0000 & vm.ep) | r)
VM vm;

// *********
// * 3.a: Utilities
// There are a few utility functions necessary for executing instructions.

U4 ftBE(Ref ref, U2 size) { // fetch Big Endian
  U1* p = boundsCheck(size, ref);
  switch(size) {
    case 1: return *p;                  case 2: return (*p<<8) + *(p + 1);
    case 4: return (*p << 24) + (*(p + 1)<<16) + (*(p + 2)<<8) + *(p + 3);
    default: SET_ERR(E_sz);
  }
}

void srBE(Ref ref, U2 size, U4 value) { // store Big Endian
  U1* p = boundsCheck(size, ref);
  switch(size) {
    case 1: *p = value; break;
    case 2: *p = value>>8; *(p+1) = value; break;
    case 4: *p = value>>24; *(p+1) = value>>16; *(p+2) = value>>8; *(p+3) = value;
            break;
    default: SET_ERR(E_sz);
  }
}

U4 popLit(U1 size) { U4 out = ftBE(vm.ep, size); vm.ep += size; return out; }
U4 min(U4 a, U4 b) { if(a < b) return a; return b; }

BARE_TEST(testUtilities, 2)
  srBE(BLOCK_SIZE,      1, 0x01);
  srBE(BLOCK_SIZE + 1,  2, 0x2345);
  srBE(BLOCK_SIZE + 3,  4, 0x6789ABCD);
  ASSERT_EQ(0x01,         ftBE(BLOCK_SIZE, 1));
  ASSERT_EQ(0x2345,       ftBE(BLOCK_SIZE + 1, 2));
  ASSERT_EQ(0x6789ABCD,   ftBE(BLOCK_SIZE + 3, 4));
  vm.ep = BLOCK_SIZE;
  ASSERT_EQ(0x01,         popLit(1));
  ASSERT_EQ(0x2345,       popLit(2));
TEST_END

// *********
// * 3.b: Functions
// Functions can be either "small" (no locals) or "large" (has locals).

void xImpl(U1 growSz, Ref fn) { // base impl for XS and XL.
  CS_PUSH(vm.ep);
  g->csSz.sp -= 1; *(mem + g->csSz.ref + g->csSz.sp) = growSz; // push growSz onto csSz
  vm.ep = fn;
}

void xlImpl(Ref fn) { // impl for XL*
  // get amount to grow, must be multipled by APtr size .
  U1 growSz = *asPtr(U1, fn);
  ASM_ASSERT(growSz % RSIZE, E_align4);
  ASM_ASSERT(g->ls.sp >= growSz, E_stkOvr);
  g->ls.sp -= growSz; // grow locals stack
  xImpl(growSz, fn + 1);
}

// *********
// * 3.c: Giant Switch Statement
//
// Instructions (which are really just a single byte) are executed inside a
// giant switch statement. Most instructions modify the working stack and read
// literal values from the execution pointer. Some can also affect the execution
// pointerand the call stack.

void executeDV(U1 dv, bool isFetch); // Device execute, will be defined later.

inline static void executeInstr(Instr instr) {
  U4 l, r;
  switch ((U1)instr) {
    // Operation Cases
    case NOP: return;
    case RETZ: if(WS_POP()) return; // intentional fallthrough
    case RET:
      r = Stk_pop(&CS);
      l = *(mem + g->csSz.ref + g->csSz.sp); // size to shrink locals
      g->csSz.sp += 1;
      ASM_ASSERT(g->ls.sp + l <= g->ls.cap, E_stkUnd);
      g->ls.sp += l;
      vm.ep = r;
      return;
    case SWP: r = WS_POP(); l = WS_POP(); WS_PUSH(r); WS_PUSH(l); R
    case DRP : WS_POP(); R
    case OVR : r = WS_POP(); l = WS_POP(); WS_PUSH(l); WS_PUSH(r); WS_PUSH(l); R
    case DUP : r = WS_POP(); WS_PUSH(r); WS_PUSH(r);      R
    case DUPN: r = WS_POP(); WS_PUSH(r); WS_PUSH(0 == r); R
    case DVFT: executeDV(popLit(1), true); R
    case DVSR: executeDV(popLit(1), false); R
    case RGFT:
      r = popLit(1);
      if (R_LP & r) {
        return WS_PUSH(LS_SP + (0x7F & r)); // local stack pointer + offset
      } else {
        switch (r) {
          case R_EP: WS_PUSH(vm.ep); R
          case R_GB: WS_PUSH(g->gbuf.ref); R
          default: SET_ERR(E_cReg);
        }
      }
    case RGSR:
      r = popLit(1);
      if (R_LP & r) {
        g->ls.sp = WS_POP() + (0x7F & r) - g->ls.ref;
        return;
      } else {
        switch (0xC0 & r) {
          case R_EP: SET_ERR(E_cReg); // SR to EP not allowed
          case R_GB: g->gbuf.ref = WS_POP(); R
          default: SET_ERR(E_cReg);
        }
      }
    case INC : WS_PUSH(WS_POP() + 1); return;
    case INC2: WS_PUSH(WS_POP() + 2); return;
    case INC4: WS_PUSH(WS_POP() + 4); return;
    case DEC : WS_PUSH(WS_POP() - 1); return;
    case INV : WS_PUSH(~WS_POP()); return;
    case NEG : WS_PUSH(-WS_POP()); return;
    case NOT : WS_PUSH(0 == WS_POP()); return;
    case CI1 : WS_PUSH((I4) ((I1) WS_POP())); return;
    case CI2 : WS_PUSH((I4) ((I2) WS_POP())); return;

    case ADD : r = WS_POP(); WS_PUSH(WS_POP() + r); return;
    case SUB : r = WS_POP(); WS_PUSH(WS_POP() - r); return;
    case MOD : r = WS_POP(); WS_PUSH(WS_POP() % r); return;
    case SHL : r = WS_POP(); WS_PUSH(WS_POP() << r); return;
    case SHR : r = WS_POP(); WS_PUSH(WS_POP() >> r); return;
    case MSK : r = WS_POP(); WS_PUSH(WS_POP() & r); return;
    case JN  : r = WS_POP(); WS_PUSH(WS_POP() | r); return;
    case XOR : r = WS_POP(); WS_PUSH(WS_POP() ^ r); return;
    case AND : r = WS_POP(); WS_PUSH(WS_POP() && r); return;
    case OR  : r = WS_POP(); WS_PUSH(WS_POP() || r); return;
    case EQ  : r = WS_POP(); WS_PUSH(WS_POP() == r); return;
    case NEQ : r = WS_POP(); WS_PUSH(WS_POP() != r); return;
    case GE_U: r = WS_POP(); WS_PUSH(WS_POP() >= r); return;
    case LT_U: r = WS_POP(); WS_PUSH(WS_POP() < r); return;
    case GE_S: r = WS_POP(); WS_PUSH((I4)WS_POP() >= (I4) r); return;
    case LT_S: r = WS_POP(); WS_PUSH((I4)WS_POP() < (I4) r); return;
    case MUL  :r = WS_POP(); WS_PUSH(WS_POP() * r); return;
    case DIV_U:r = WS_POP(); WS_PUSH(WS_POP() / r); return;
    case DIV_S:
      r = WS_POP();
      ASM_ASSERT(r, E_divZero);
      WS_PUSH((I4) WS_POP() / (I4) r);
      return;

    // Mem Cases
    case SZ1 + FT: WS_PUSH(*asPtr(U1, WS_POP())); R
    case SZ2 + FT: WS_PUSH(*asPtr(U2, WS_POP())); R
    case SZ4 + FT: WS_PUSH(*asPtr(U4, WS_POP())); R

    case SZ1 + FTO: WS_PUSH(*asPtr(U1, WS_POP() + popLit(1))); R
    case SZ2 + FTO: WS_PUSH(*asPtr(U2, WS_POP() + popLit(1))); R
    case SZ4 + FTO: WS_PUSH(*asPtr(U4, WS_POP() + popLit(1))); R

    case SZ1 + FTLL: WS_PUSH(*asPtr(U1, LS_SP + popLit(1))); R
    case SZ2 + FTLL: WS_PUSH(*asPtr(U2, LS_SP + popLit(1))); R
    case SZ4 + FTLL: WS_PUSH(*asPtr(U4, LS_SP + popLit(1))); R

    case SZ1 + FTGL: WS_PUSH(*asPtr(U1, g->gbuf.ref + popLit(2))); R
    case SZ2 + FTGL: WS_PUSH(*asPtr(U2, g->gbuf.ref + popLit(2))); R
    case SZ4 + FTGL: WS_PUSH(*asPtr(U4, g->gbuf.ref + popLit(2))); R

    case SZ1 + SR: r = WS_POP(); *asPtr(U1, r) = WS_POP(); R
    case SZ2 + SR: r = WS_POP(); *asPtr(U2, r) = WS_POP(); R
    case SZ4 + SR: r = WS_POP(); *asPtr(U4, r) = WS_POP(); R

    case SZ1 + SRO: r = WS_POP(); *asPtr(U1, r + popLit(1)) = WS_POP(); R
    case SZ2 + SRO: r = WS_POP(); *asPtr(U2, r + popLit(1)) = WS_POP(); R
    case SZ4 + SRO: r = WS_POP(); *asPtr(U4, r + popLit(1)) = WS_POP(); R

    case SZ1 + SRLL: *asPtr(U1, LS_SP + popLit(1)) = WS_POP(); R
    case SZ2 + SRLL: *asPtr(U2, LS_SP + popLit(1)) = WS_POP(); R
    case SZ4 + SRLL: *asPtr(U4, LS_SP + popLit(1)) = WS_POP(); R

    case SZ1 + SRGL: *asPtr(U1, g->gbuf.ref + popLit(2)) = WS_POP(); R
    case SZ2 + SRGL: *asPtr(U2, g->gbuf.ref + popLit(2)) = WS_POP(); R
    case SZ4 + SRGL: *asPtr(U4, g->gbuf.ref + popLit(2)) = WS_POP(); R

    case SZ1 + LIT: WS_PUSH(popLit(1)); R
    case SZ2 + LIT: WS_PUSH(popLit(2)); R
    case SZ4 + LIT: WS_PUSH(popLit(4)); R

    // Jmp Cases
    case SZ1 + JMPL: r = popLit(1); vm.ep = vm.ep + (I1)r; R
    case SZ2 + JMPL: r = popLit(2); vm.ep = sectorRef(r); R
    case SZ4 + JMPL: r = popLit(4); vm.ep = r; R

    case SZ1 + JMPW:
    case SZ2 + JMPW:
    case SZ4 + JMPW: vm.ep = WS_POP();

    case SZ1 + JZL: r = popLit(1); if(!WS_POP()) { vm.ep = vm.ep + (I1)r; } R
    case SZ2 + JZL: r = popLit(2); if(!WS_POP()) { vm.ep = sectorRef(r); } R
    case SZ4 + JZL: r = popLit(4); if(!WS_POP()) { vm.ep = r; } R

    case SZ1 + JTBL:
    case SZ2 + JTBL:
    case SZ4 + JTBL: assert(FALSE); // TODO: not impl

    case SZ1 + XLL: xlImpl(vm.ep + (I1)popLit(1)); R;
    case SZ2 + XLL: xlImpl(sectorRef(popLit(2))); R;
    case SZ4 + XLL: xlImpl(popLit(4)); R;

    case SZ1 + XLW:
    case SZ2 + XLW:
    case SZ4 + XLW: xlImpl(WS_POP()); R;

    case SZ1 + XSL: xImpl(0, vm.ep + (I1)popLit(1)); R;
    case SZ2 + XSL: xImpl(0, sectorRef(popLit(2))); R;
    case SZ4 + XSL: xImpl(0, popLit(4)); R;

    case SZ1 + XSW:
    case SZ2 + XSW:
    case SZ4 + XSW: xImpl(0, WS_POP()); R;

    default: if(instr >= SLIT) return WS_PUSH(0x3F & instr);
             SET_ERR(E_cInstr);
  }
}

// ***********************
// * 4: Scanner
// The fngi scanner is used for both spor and fngi syntax. The entire compiler
// works by:
//   (1) scanning a single tokens.
//   (2) executing or compiling the current token.
//      (2.a) possibly in some special way by peeking at the next token.
//
// There are a few use-cases that must be supported:
//   (1) scanning tokens
//   (2) peeking at tokens
//   (3) reading raw characters performantly
//
// The basic architecture is to use a small buffer (~128 bytes) to buffer input
// from the operating system or hardware. We keep track of the currently scanned
// token. When it is not used, we shift bytes to the left to delete the current
// token.
//
// The advantage to this is simplicity. We do pay some cost for the performance
// of shifting bytes left after ever token, but that cost is small compared to
// serial IO.

#define TBUF  (mem + g->t.buf)
#define TLEN  (g->t.len)
#define TSIZE (g->t.size)

/*fn*/ U1 toTokenGroup(U1 c) {
  if(c <= ' ') return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '_') return T_ALPHA;
  if(c == '%' || c == '\\' || c == '$' || c == '|' ||
     c == '.' || c ==  '(' || c == ')') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

int main() {
  eprint("# main\n");
  testSetErr();
  testStk();
  testBANew();
  testAllocFree();
  testAlloc2FreeFirst();
  testBBA();
  testSlc();
  testDict();
  testUtilities();

  return 0;
}
