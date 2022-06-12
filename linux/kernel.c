// Spor binary kernel.
// This C file implements spor, see spor.md for documentation.
//
// # Table of Contents
// Search for these headings to find information
//
// * 1: Environment and Test Setup
//   * 1.a: Panic Handling
//   * 1.b: Environment Setup
//
// * 2: Memory Managers and Data Structures
//   * 2.a: Stacks
//   * 2.b: BlockAllocator (BA)
//   * 2.c: BlockBumpArena (BBA)
//   * 2.d: Slc (slice) Data Structure
//   * 2.e: Dict Binary Search Tree
//
// * 3: Executing Instructions
//   * 3.a: Utilities
//   * 3.b: Functions
//   * 3.c: Giant Switch Statement
//
// * 4: Source Code
//   * 4.a: Reading and Mocking
//   * 4.b: Scan
//
// * 5: Compiler
//   * 5.a: Utilities
//   * 5.b: Spor Token Functions
//   * 5.c: Spor Compiler
//   * 5.d: Compile Constants
//
// * 6: Device Operations (DV)

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <setjmp.h>
#include <stdbool.h>
#include <errno.h>

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


//   *******
//   * 1.a: Panic Handling
// In normal fngi code, errors should be returned. However, for critical
// failures code can and will "panic". In C, a panic causes a longjmp to the
// error handler. Panics should be considered extraordinary events, and although
// fngi code can handle them with D_panic, it is not recommended to do so for
// any but the most low-level code or for testing purposes.

jmp_buf* err_jmp = NULL;
bool expectingErr = false;
#define SET_ERR(E)  if(true) { assert(E); g->err = E; \
  if(!expectingErr) { eprintf("!! Hit Error, fn=%s [cline=%u]\n", __func__, __LINE__); } \
  longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!g->err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }

//   *******
//   * 1.b: Environment Setup
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
U4 line;

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
            + (WS_DEPTH + CS_DEPTH) * RSIZE + CS_DEPTH // stacks
            + (2 * blocks)); // nodes
  *k = (Kern)    {0};
  *g = (Globals) {0};

  LS  = Stk_init(BLOCK_SIZE      , BLOCK_SIZE);
  WS  = Stk_init(WS_DEPTH * RSIZE, sizeof(Kern));
  CS  = Stk_init(CS_DEPTH * RSIZE, WS.ref + WS.cap);
  CSZ = Stk_init(CS_DEPTH        , CS.ref + CS.cap);
  k->ba = (BA) {
    .blocks = BLOCK_SIZE * 2, .nodes = CSZ.ref + CSZ.cap,
    .rooti = BLOCK_END,       .cap = blocks - 2,
  };
  k->bba    = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };
  k->bbaTmp = (BBA) { .ba = asRef(&k->ba), .rooti=BLOCK_END };

  g->gbuf = (Buf) {
    .ref = asRef(g), .len = sizeof(Globals), .cap = BLOCK_SIZE };
  g->t = (TokenState) { .ref = asRef(&g->buf0), .size = TOKEN_SIZE };
  g->curBBA = asRef(&k->bba);
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
    eprintf("!! Error #%X (line %u)\n", g->err, line); exit(1); } \
  else { /*Test code here */

#define TEST_END   } ENV_CLEANUP(); }

#define EXPECT_ERR(E, CALL) \
  err_jmp = &test_err_jmp; expectingErr = true; if(setjmp(test_err_jmp)) \
  { expectingErr = false; ASSERT_EQ(E, g->err); err_jmp = &local_err_jmp; } \
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

//   *******
//   * 2.a: Stacks
// Stacks are the core memory manager for operations (via the working stack
// (WS)) and for executing functions (via the call stack (CS)).
#define Stk_len(S)       (((S).cap - (S).sp) / RSIZE)
#define WS_POP()         Stk_pop(&WS)
#define WS_PUSH(V)       Stk_push(&WS, V)
#define WS_PUSH2(A, B)   if(1) { Stk_push(&WS, A); Stk_push(&WS, B); }
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

//   *******
//   * 2.b: BlockAllocator (BA)
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
  if(ba->cap == 0) return; ASM_ASSERT(ba->cap < BLOCK_END, E_intern);
  BANode* nodes = asPtr(BANode, ba->nodes);
  ba->rooti = 0;
  U1 i, previ = BLOCK_END;
  for (i = 0; i < ba->cap; i += 1) {
    nodes[i].previ = previ; nodes[i].nexti = i + 1;
    previ = i;
  }
  nodes[i - 1].nexti = BLOCK_END;
}


// Allocate a block, updating BlockAllocator and client's root indexes.
//
// Go from:
//   baRoot     -> d -> e -> f
//   clientRoot -> c -> b -> a
// To (returning block 'd'):
//   baRoot     -> e -> f
//   clientRoot -> d -> c -> b -> a
Ref BA_alloc(BA* ba, U1* clientRooti) {
  uint8_t di = ba->rooti; // index of "d"
  if(di == BLOCK_END) return 0;

  BANode* nodes = asPtr(BANode, ba->nodes);
  BANode* d = &nodes[di]; // node "d"
  ba->rooti = d->nexti;  // baRoot -> e
  if (d->nexti != BLOCK_END) nodes[d->nexti].previ = BLOCK_END; // baRoot <- e

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


//   *******
//   * 2.c: BlockBumpArena (BBA)
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
  Ref out = BA_block(*BBA_ba(*bba), bba->rooti) + bba->cap;
  return out;
}

// Allocate "unaligned" data from the bottom of the block.
Ref BBA_allocUnaligned(BBA* bba, uint16_t size) {
  if(!_BBA_reserveIfSmall(bba, size)) return 0;
  Ref out = BA_block(*BBA_ba(*bba), bba->rooti) + bba->len;
  bba->len += size;
  return out;
}

BARE_TEST(testBBA, 6)   BA_init(&k->ba);
  BANode* nodes = asPtr(BANode, k->ba.nodes);
  ASSERT_EQ(k->ba.blocks + BLOCK_SIZE - 12  , BBA_alloc(&k->bba, 12));
  ASSERT_EQ(BA_block(k->ba, 1)              , BBA_alloc(&k->bba, BLOCK_SIZE));
  ASSERT_EQ(BA_block(k->ba, 2)              , BBA_allocUnaligned(&k->bba, 13));
  ASSERT_EQ(BA_block(k->ba, 2) + 13         , BBA_allocUnaligned(&k->bba, 25));
  ASSERT_EQ(BA_block(k->ba, 3)              , BBA_allocUnaligned(&k->bba, BLOCK_SIZE - 20));
  ASSERT_EQ(0                               , BBA_alloc(&k->bba, BLOCK_SIZE));
TEST_END

//   *******
//   * 2.d: Slc (slice) Data Structure
// One of the core types in fngi is the Slc (slice) and it's child the Buf
// (buffer). A Slc is simply a reference and a U2 len (length). A Buf adds on a
// capacity, allowing for the data to grow. Note that a reference to a Buf is
// also a valid reference to a Slc.
//
// The other core type is cdata, often just represented as "c". This is counted
// data where the first byte has the count (length).
#define cAsSlc(CDATA)  asSlc(CDATA + 1, *asPtr(U1, CDATA))
#define sAsTmpSlc(S)   mvAndSlc(S, strlen(S))

Slc asSlc(Ref ref, U2 len) {
  ASM_ASSERT(ref, E_null); ASM_ASSERT(ref + len < memSize, E_oob);
  return (Slc) {.ref = ref, .len = len};
}

Slc mvAndSlc(U1* buf, U2 len) {
  U1* gbuf = asPtr(U1, g->gbuf.ref + g->gbuf.len);
  memmove(gbuf, buf, len);
  return (Slc) { .ref = asRef(gbuf), .len = len };
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
  memmove(asPtr(U1, c_a), "\x03" "aaa", 4); \
  memmove(asPtr(U1, c_b), "\x04" "abbd", 5); \
  memmove(asPtr(U1, c_c), "\x03" "abc", 4); \
  Slc a = cAsSlc(c_a); \
  Slc b = cAsSlc(c_b); \
  Slc c = cAsSlc(c_c);

BARE_TEST(testSlc, 5)
  TEST_SLICES
  ASSERT_EQ(3, c.len); assert(c_c + 1 == c.ref);
  ASSERT_EQ('a', *asPtr(U1, c.ref));  ASSERT_EQ('c', *asPtr(U1, c.ref + 2));


  ASSERT_EQ(0,  Slc_cmp(a, a));
  ASSERT_EQ(-1, Slc_cmp(a, b));
  ASSERT_EQ(-1, Slc_cmp(a, c));
  ASSERT_EQ(1,  Slc_cmp(b, a));
  ASSERT_EQ(1,  Slc_cmp(c, b));

  Slc z = sAsTmpSlc("abc");  ASSERT_EQ(0, Slc_cmp(c, z));
TEST_END


//   *******
//   * 2.e: Dict Binary Search Tree
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

DNode* Dict_get(DNode* root, Slc slc) {
  ASM_ASSERT(!Dict_find(&root, slc), E_cNoKey);
  return root;
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

//   *******
//   * 3.a: Utilities
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
U4 max(U4 a, U4 b) { if(a < b) return b; return a; }

static inline void _memmove(Ref dst, Ref src, U2 len) {
  void* d = boundsCheck(len, dst); void* s = boundsCheck(len, src);
  memmove(d, s, len);
}

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

//   *******
//   * 3.b: Functions
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

//   *******
//   * 3.c: Giant Switch Statement
//
// Instructions (which are really just a single byte) are executed inside a
// giant switch statement. Most instructions modify the working stack and read
// literal values from the execution pointer. Some can also affect the execution
// pointerand the call stack.

static inline void executeDV(U1 dv);

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
    case DV: executeDV(popLit(1)); R
    case RG:
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
    case SZ4 + JTBL: assert(false); // TODO: not impl

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

void execute(U1 instr) {
  U2 startingLen = Stk_len(g->cs);
  while(true) {
    executeInstr(instr);
    if(Stk_len(g->cs) == startingLen) return;
    instr = popLit(1);
  }
}


// ***********************
// * 4: Source Code
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
//
// https://my.eng.utah.edu/~cs4400/file-descriptor.pdf

#include <fcntl.h>
#include <signal.h>

int F_handleErr(File* f, int res) {
  if(errno == EWOULDBLOCK) return res;
  if(res < 0) {
    f->code = F_Eio; g->syserr = errno; errno = 0;
  }
  return res;
}

void F_open(File* f) {
  U1 pathname[256];
  ASM_ASSERT(f->b.len < 255, E_io);
  memcpy(pathname, boundsCheck(f->b.len, f->b.ref), f->b.len);
  pathname[f->b.len] = 0;
  int fd = F_handleErr(f, open(pathname, O_NONBLOCK, O_RDWR));
  if(fd < 0) f->fid = -1;
  else { f->pos = 0; f->fid = fd; f->code = F_done; f->plc = 0; }
}

void F_read(File* f) {
  ASM_ASSERT(f->code == F_reading || f->code >= F_done, E_io);
  int len;
  if(!(F_INDEX & f->fid)) { // mocked file. TODO: add some randomness
    PlcBuf* p = asPtr(PlcBuf, f->fid);
    len = min(p->len - p->plc, f->b.cap - f->b.len);
    _memmove(f->b.ref, p->ref + p->plc, len); p->plc += len;
  } else {
    f->code = F_reading;
    len = F_handleErr(f, read(F_FD(*f), asPtr(U1, f->b.ref), f->b.cap - f->b.len));
    assert(len >= 0);
  }
  f->b.len += len; f->pos += len;
  if(f->b.len == f->b.cap) f->code = F_done;
  else if (0 == len) f->code = F_eof;
}

File* mockFile(U1* contents, U2 bufCap) {
  File* f = asPtr(File, BBA_alloc(&k->bba, sizeof(File)));
  f->b = (Buf) { .ref = BBA_allocUnaligned(&k->bba, bufCap), .cap = bufCap };

  PlcBuf* p = asPtr(PlcBuf, BBA_alloc(&k->bba, sizeof(PlcBuf)));
  U2 len = strlen(contents);
  U1* s = asPtr(U1, BBA_allocUnaligned(&k->bba, len));
  memmove(s, contents, len);
  *p = (PlcBuf) { .ref = asRef(s), .len = len, .cap = len };

  f->fid = asRef(p); f->code = F_done;
  return f;
}

void fileAssertions(File* f) {
  F_read(f); ASSERT_EQ(F_done, f->code);
  ASSERT_EQ(5, f->b.len);
  ASSERT_EQ(0, memcmp("Hi th", asPtr(U1, f->b.ref), 5));
  f->b.len = 0; F_read(f); assert(!memcmp("ere B", asPtr(U1, f->b.ref), 5));
  f->b.len = 0; F_read(f); assert(!memcmp("ob\nTh", asPtr(U1, f->b.ref), 5));
  f->b.len = 0; F_read(f); assert(!memcmp("is is", asPtr(U1, f->b.ref), 5));
  f->b.len = 0; F_read(f); assert(!memcmp(" Jane", asPtr(U1, f->b.ref), 5));
  f->b.len = 0; F_read(f); assert(!memcmp(".", asPtr(U1, f->b.ref), 1));
  ASSERT_EQ(F_done, f->code); F_read(f); ASSERT_EQ(F_eof, f->code);
}

BARE_TEST(testRead, 4)  BA_init(&k->ba);
  U1* s = "Hi there Bob\nThis is Jane."; 
  File* f = mockFile(s, 5); ASSERT_EQ(5, f->b.cap);
  fileAssertions(f);
TEST_END


// Read file blocking. Any errors result in a panic.
void readBlocking(File* f, U2 atLeast) {
  ASM_ASSERT(f->b.cap - f->b.len >= atLeast, E_intern);
  fd_set fdset; FD_ZERO(&fdset); FD_SET(f->fid, &fdset);
  U2 startLen = f->b.len;
  while(1) { F_read(f);
    if(f->code == F_eof || f->b.len - startLen >= atLeast) break;
    ASM_ASSERT(f->code < F_error, E_io);
    if(F_INDEX & f->fid) {
      assert(select(f->fid, &fdset, NULL, NULL, NULL));
      assert(FD_ISSET(f->fid, &fdset));
    }
  }
}

FILE* srcFile;
char* testSrcStr = NULL;
U2    testSrcStr_i = 0;

//   *******
//   * 4.a: Reading and Mocking
// We read using a function pointer so that tests can mock it out.

#define Tbuf  (mem + g->t.ref)
#define Tlen  (g->t.len)
#define Tsize (g->t.size)
#define Tslc  ((Slc) {.ref = g->t.ref, .len = Tlen})

U1 (*readAtLeast)(U1 num) = NULL; // Read num bytes into Tbuf

// Read at least num bytes from source file into Tbuf.
// If EOF is reached return the number of bytes read.
// If this would overflow Tbuf, return the number of bytes actually read.
U1 readSrcAtLeast(U1 num) {
  U4 out = 0;
  num = min(TOKEN_SIZE, num);
  assert(num);
  while (true) {
    ssize_t numRead = read(
      fileno(srcFile),                // filedes
      Tbuf + Tsize,                   // buf
      max(num, TOKEN_SIZE - Tsize));  // nbyte
    assert(!errno);
    assert(numRead >= 0);
    Tsize += numRead;
    out += numRead;
    if(!numRead) break; // EOF
    if(TOKEN_SIZE - Tsize == 0) break;
    if(numRead >= num) break;
  }
  assert(out <= 0xFF);
  // eprintf("??? readSrcAtLeast numRead=%u: ", out);
  // fwrite(Tbuf + Tsize - out, 1, out, stderr);
  // eprintf("\n ??? Token len=%u: %.*s", tokenLen, tokenLen, Tbuf);
  return out;
}

void readNewAtLeast(U1 num) {
  Tlen = 0;
  Tsize = 0;
  readAtLeast(num);
}

void shiftBuf() {
  // Shift buffer left from end of token
  if(Tlen == 0) return;
  U2 newStart = Tlen; U1 i = 0;
  while(Tlen < Tsize) {
    Tbuf[i] = Tbuf[Tlen];
    i += 1, Tlen += 1;
  }
  Tlen = 0, Tsize = Tsize - newStart;
}

// Function for "reading" from a string.
U1 testSrcStrReadAtLeast(U1 n) {
  U1 numRead = 0;
  while (Tsize < TOKEN_SIZE) {
    U1 c = testSrcStr[testSrcStr_i];
    if(c == 0) return numRead;
    Tbuf[Tsize] = c;
    Tsize += 1;
    testSrcStr_i += 1;
    numRead += 1;
  }
  return numRead;
}

// Function for reading from a file.
void setCompileFile(char* s) {
  // zoab_file(strlen(s), s);
  line = 1, readAtLeast = &readSrcAtLeast, srcFile = fopen(s, "rb");
  assert(srcFile > 0);
}

void setCompileStr(char* s) {
  // zoab_file(7, "RAW_STR");
  testSrcStr = s, testSrcStr_i = 0, line = 1;
  readAtLeast = &testSrcStrReadAtLeast;
}

//   *******
//   * 4.b: Scan
// The scanner reads the next token, storing at the beginning of Tbuf with
// length Tlen.

#define ASSERT_TOKEN(S)  if(1) { scan(); if(!Teq(S)) { \
  eprintf("! Token: %s == %.*s\n", S, Tlen, Tbuf); assert(false); } }
U1 Teq(U1* s) { U2 len = strlen(s);  if(len != Tlen) return false;
                return 0 == memcmp(s, Tbuf, len); }

U1 toTokenGroup(U1 c) {
  if(c <= ' ')             return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if(c == '_')             return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '%' || c == '\\' || c == '$' || c == '|' ||
     c == '.' || c ==  '(' || c == ')') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

void _scan() {
  // Skip whitespace
  while(true) {
    if(Tlen >= Tsize) { readNewAtLeast(1); }
    if(Tsize == 0) return;
    if(toTokenGroup(Tbuf[Tlen]) != T_WHITE) break;
    if(Tbuf[Tlen] == '\n') line += 1;
    Tlen += 1;
  }
  shiftBuf(); // Moves buffer to the left (Tlen=0)
  if(!Tsize) { readAtLeast(1); }
  assert(Tsize);

  U1 c = Tbuf[Tlen];
  g->t.group = toTokenGroup(c);
  if(g->t.group == T_SINGLE) {
    Tlen += 1; // SINGLE: always single-char token
    return;
  }

  // Parse token until the group changes.
  while (true) {
    if (Tlen >= Tsize) readAtLeast(1);
    if (Tlen >= Tsize) break;

    ASM_ASSERT(Tlen < TOKEN_SIZE, E_cTLen);
    c = Tbuf[Tlen];

    U1 tg = toTokenGroup(c);
    if (tg == g->t.group) {}
    else if ((g->t.group <= T_ALPHA) && (tg <= T_ALPHA)) {}
    else break;
    Tlen += 1;
  }
}

// void scan() { _scan(); }
void scan() { _scan(); }

BARE_TEST(testScan, 3)
  setCompileStr("hi there$==");
  ASSERT_TOKEN("hi"); ASSERT_TOKEN("there"); ASSERT_TOKEN("$"); ASSERT_TOKEN("==");

  setCompileStr("         lots     \n\n\n\n    of \n\n  empty            ");
  ASSERT_TOKEN("lots"); ASSERT_TOKEN("of"); ASSERT_TOKEN("empty");

  setCompileFile("kernel/constants.sp");
  ASSERT_TOKEN("\\"); ASSERT_TOKEN("Kernel"); ASSERT_TOKEN("Constants");
  ASSERT_TOKEN("\\");
  ASSERT_TOKEN("\\"); ASSERT_TOKEN("Note"); ASSERT_TOKEN(":"); ASSERT_TOKEN("this");
TEST_END

// ***********************
// * 5: Compiler
// We can finally begin writing the compiler. There are only a few characters
// that the native spor assembler can handle (see `compile()` function).
//
// We must first implement how to handle each case and then we can start
// compiling!

typedef struct { U1 sz; U1 instr; } Compiler;
Compiler compiler;

//   *******
//   * 5.a: Utilities

U1 charToSz(U1 c) {
  switch (c) { case '1': return 1; case '2': return 2; case '4': return 4;
               case 'R': return RSIZE; default: SET_ERR(E_sz); } }

Ref bump(U1 aligned, U4 size) {
  Ref ref;  BBA* bba = asPtr(BBA, g->curBBA);  U1 starti = bba->rooti;
  if(aligned) ref = BBA_alloc(bba, size);
  else        ref = BBA_allocUnaligned(bba, size);
  ASM_ASSERT(starti == bba->rooti, E_newBlock); ASM_ASSERT(ref, E_oom);
  return ref;
}

U1 scanInstr() { // scan a single instruction, combine with sz
  scan(); DNode* node = Dict_get(asPtr(DNode, k->dict), Tslc);
  U1 instr = node->v;
  if((0xC0 & instr == I_MEM) || (0xC0 & instr == I_JMP)) {
    switch (compiler.sz) {
      case 1: instr |= SZ1; break; case 2: instr |= SZ2; break;
      case 4: instr |= SZ4; break; default: SET_ERR(E_intern);
    }
  }
  return instr;
}

//   *******
//   * 5.b: Spor Token Functions
// Each of these handle a single "token" (really single character) case in the
// spore compiler.

#define SPOR_TEST(NAME, BLOCKS) \
  BARE_TEST(NAME, BLOCKS) \
  initSpor();

void initSpor() {
  vm = (VM) { .ep = 1 };
  compiler = (Compiler) { .sz = RSIZE, .instr = NOP };
  BA_init(&k->ba);
}

Ref newBlock() { // start a new block
  BBA* bba = asPtr(BBA, g->curBBA);
  Ref r = BA_alloc(asPtr(BA, bba->ba), &bba->rooti);
  ASM_ASSERT(r, E_oom); bba->len = 0; bba->cap = BLOCK_SIZE;
  return r;
}

void cDot() { // `.`, aka "set size"
  if(Tlen >= Tsize) readAtLeast(1);
  compiler.sz = charToSz(Tbuf[Tlen]);
  Tlen += 1;
}

void cForwardSlash() { // `\`, aka line comment
  while(true) {
    if (Tlen >= Tsize) readNewAtLeast(1);
    if (Tsize == 0 || Tbuf[Tlen] == '\n') break;
    Tlen += 1;
  }
}

void cHash() { // `#`, aka hex literal
   U4 v = 0; scan();
  for(U1 i = 0; i < Tlen; i += 1) {
    U1 c = Tbuf[i];
    if (c == '_') continue;
    ASM_ASSERT(toTokenGroup(c) <= T_HEX, E_cHex);
    v = (v << 4) + charToHex(c);
  }
  WS_PUSH(v); shiftBuf();
}

void cEqual() { // `=`, aka dict set
  U1 meta = WS_POP(); U4 value = WS_POP(); scan();
  Ref ckey = bump(false, Tlen + 1);
  *(mem + ckey) = Tlen;
  memmove(mem + ckey + 1, Tbuf, Tlen);

  DNode* add = (DNode*) (mem + bump(true, sizeof(DNode)));
  *add = (DNode) {.ckey = ckey, .v = value, .m1 = meta};

  DNode* root = NULL; if(k->dict) root = asPtr(DNode, k->dict);
  Dict_add(&root, add);
  if(!k->dict) k->dict = asRef(root);
}

void cAt() { // `@`, aka dict get
  scan();
  DNode* root = asPtr(DNode, k->dict);
  Slc t = Tslc;

  DNode* node = Dict_get(root, t);
  // DNode* node = Dict_get(asPtr(DNode, k->dict), Tslc);
  WS_PUSH(node->v);
}

void cComma() { // `,`, aka write heap
  U4 value = WS_POP();
  srBE(bump(/*aligned=*/ false, compiler.sz), compiler.sz, value);
}

void cPercent() { // `%`, aka compile instr
  srBE(bump(/*aligned=*/ false, 1), 1, scanInstr());
}

void cCarrot() { // `^`, aka execute instr
  U1 instr = scanInstr();
  vm.ep += 1;
  executeInstr(instr);
}

void cDollar() { // `$`, aka execute token
  scan(); DNode* n = Dict_get(asPtr(DNode, k->dict), Tslc);

  if(TY_FN_INLINE == (TY_FN_TY_MASK & n->m1)) {
    U1 len = *asPtr(U1, n->v);
    memmove(asPtr(U1, bump(false, len)), asPtr(U1, n->v + 1), len);
    return;
  }
  if(TY_FN_SYN == (TY_FN_TY_MASK & n->m1)) WS_PUSH(false); // pass asNow=false
  WS_PUSH(n->v);
  if(TY_FN_LARGE & n->m1) execute(SZ4 + XLW);
  else                    execute(SZ4 + XSW);
}

#define ASSERT_VALUE(K, V) ASSERT_EQ( \
  V, Dict_get(asPtr(DNode, k->dict), sAsTmpSlc(K))->v)

SPOR_TEST(testSporBasics, 4)  newBlock();
  setCompileStr(" 12 ");  cHash();   ASSERT_EQ(0x12, WS_POP());
  WS_PUSH2(0x42, 0); setCompileStr("mid");  cEqual(); ASSERT_VALUE("mid", 0x42);
  WS_PUSH2(0x44, 0); setCompileStr("aLeft"); cEqual(); ASSERT_VALUE("aLeft", 0x44);
  WS_PUSH2(0x88, 0); setCompileStr("zRight"); cEqual(); ASSERT_VALUE("zRight", 0x88);
TEST_END

//   *******
//   * 5.c: Spor Compiler

bool compile() {
  if(Tlen == 0) return true; Tlen = 1; // spor compiler only uses first character
  switch (Tbuf[0]) {
    case '.': cDot(); break;        case '\\': cForwardSlash(); break;
    case '#': cHash(); break;       case '=': cEqual(); break;
    case '@': cAt(); break;         case ',': cComma(); break;
    case '%': cPercent(); break;    case '^': cCarrot(); break;
    case '$': cDollar(); break;
    default: eprintf("!! Invalid ASM token: %.*s\n", Tlen, Tbuf);
             SET_ERR(E_cToken);
  }
  return false;
}

void compileLoop() { while(true) { scan(); if(compile()) break; } }


//   *******
//   * 5.d: Compile Constants

void compileFile(char* s) { setCompileFile(s); compileLoop(); ASSERT_NO_ERR(); }

void compileConstants() {
  WS_PUSH(newBlock());
  WS_PUSH(RSIZE); WS_PUSH(SZR);
  compileFile("kernel/constants.sp");
  newBlock(); compileFile("kernel/errors.sp");
}

SPOR_TEST(testConstants, 4)
  compileConstants();
  ASSERT_VALUE("JMPL", 0x80);    ASSERT_VALUE("XLW", 0x85);
  ASSERT_VALUE("E_io", 0xE010);  ASSERT_VALUE("E_unreach", 0xE003);
TEST_END

// ***********************
// * 6: Device Operations (DV)
// Besides instructions for the most basic actions, Device Operations (DV) are
// the primary mechanism that spor code communicates with hardware. The kernel
// defines some extremely basic device operations which are sufficient to both:
//   (1) bootstrap fngi on running spor assembly
//   (2) enable a usable computer operating system or general purpose
//       programming language.

static inline void executeDV(U1 dv) {
  switch (dv) {
    case D_assert: { U4 chk = WS_POP(); ASM_ASSERT(chk, WS_POP()); R }
    case D_catch: {
      U4 ep = vm.ep, cs_sp = CS.sp, ls_sp = LS.sp;
      jmp_buf* prev_err_jmp = err_jmp; // cache prev jmp location
      jmp_buf local_err_jmp; err_jmp = &local_err_jmp;
      if(setjmp(local_err_jmp)) { /* got error, handled below */ }
      else execute(SZ4 + XLW);
      // ALWAYS Reset ep, call, and local stack
      vm.ep = ep, CS.sp = cs_sp, CSZ.sp = cs_sp / 4, LS.sp = ls_sp;
      WS.sp = WS.cap;              // clear WS
      err_jmp = prev_err_jmp;      // restore prev jmp location
      WS_PUSH(g->err); g->err = 0; // push and clear error
      return;
    }
    case D_memset: {
        U2 len = WS_POP(); U1 value = WS_POP(); void* dst = boundsCheck(len, WS_POP());
        memset(dst, value, len);
        return;
    }
    case D_memcmp: {
        U2 len = WS_POP();
        void* r = boundsCheck(len, WS_POP()); void* l = boundsCheck(len, WS_POP());
        return WS_PUSH(memcmp(l, r, len));
    }
    case D_memmove: {
        U2 len = WS_POP(); Ref src = WS_POP(); return _memmove(WS_POP(), src, len);
    }
    case D_bump: {
    }
    default: SET_ERR(E_dv);
  }
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
  testRead();
  testScan();
  testSporBasics();
  testConstants();

  return 0;
}
