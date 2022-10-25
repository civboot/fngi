#include <string.h>
#include <assert.h>

#include "./civ.h"

/*extern*/ jmp_buf* err_jmp = NULL;
/*extern*/ U2 civErr        = 0;
/*extern*/ Civ civ          = (Civ) {};

// ####
// # Core methods

// requiredBumpUp
// The amount to add to achieve alignment.
Slot requiredBump(void* ptr, U2 alignment) {
  Slot out = alignment - ((Slot)ptr % alignment);
  return (out == alignment) ? 0 : out;
}

// The amount to subtract to achieve alignment.
Slot requiredBumpDown(void* ptr, U2 alignment) {
  return (Slot)ptr % alignment;
}

// ##
// # Big Endian (unaligned) Fetch/Store
U4 ftBE(U1* p, Slot size) { // fetch Big Endian
  switch(size) {
    case 1: return *p;                  case 2: return (*p<<8) + *(p + 1);
    case 4: return (*p << 24) + (*(p + 1)<<16) + (*(p + 2)<<8) + *(p + 3);
    default: SET_ERR("ftBE: invalid sz");
  }
}

void srBE(U1* p, Slot size, U4 value) { // store Big Endian
  switch(size) {
    case 1: *p = value; break;
    case 2: *p = value>>8; *(p+1) = value; break;
    case 4: *p = value>>24; *(p+1) = value>>16; *(p+2) = value>>8; *(p+3) = value;
            break;
    default: SET_ERR("srBE: invalid sz");
  }
}

// ##
// # min/max
#define MIN_DEF { if(a < b) return a; return b; }
U4  U4_min (U4  a, U4  b) MIN_DEF
Ref Ref_min(Ref a, Ref b) MIN_DEF

#define MAX_DEF { if(a < b) return a; return b; }
U4  U4_max (U4  a, U4  b) MAX_DEF
Ref Ref_max(Ref a, Ref b) MAX_DEF

// ##
// # Stk

Slot Stk_pop(Stk* stk) {
  U2 sp = stk->sp;
  ASSERT(sp + 1 <= stk->cap, "Stk underflow");
  stk->sp = sp + 1;
  return stk->dat[sp];
}

U1 Stk1_pop(Stk1* stk) {
  U2 sp = stk->sp;
  ASSERT(sp + 1 <= stk->cap, "Stk1 underflow");
  stk->sp = sp + 1;
  return stk->dat[sp];
}

void Stk_push(Stk* stk, U4 value) {
  ASSERT(stk->sp > 0, "Stk overflow");
  stk->sp -= 1;
  stk->dat[stk->sp] = value;
}

void Stk1_push(Stk1* stk, U1 value) {
  ASSERT(stk->sp > 0, "Stk1 overflow");
  stk->sp -= 1;
  stk->dat[stk->sp] = value;
}

// ##
// # Slc
Slc Slc_frNt(U1* s)     { return (Slc) { .dat = s,      .len = strlen(s) }; }
Slc Slc_frCStr(CStr* c) { return (Slc) { .dat = c->dat, .len = c->count  }; }

I4 Slc_cmp(Slc l, Slc r) { // return -1 if l<r, 1 if l>r, 0 if eq
  U2 len; if(l.len < r.len) len = l.len;  else len = r.len;
  U1 *lp = l.dat; U1 *rp = r.dat;
  for(U2 i = 0; i < len; i += 1) {
    if(*lp < *rp) return -1;
    if(*lp > *rp) return 1;
    lp += 1, rp += 1;
  }
  if(l.len < r.len) return -1;
  if(l.len > r.len) return 1;
  return 0;
}

// ##
// # Buf + PlcBuf
DEFINE_AS(Buf,    /*as*/Slc);
DEFINE_AS(PlcBuf, /*as*/Slc);
DEFINE_AS(PlcBuf, /*as*/Buf);

void Buf_ntCopy(Buf* b, U1* s) {
  b->len = strlen(s);
  ASSERT(b->cap >= b->len, "Buf_ntCopy: copy too large");
  memcpy(b->dat, s, b->len);
}

// ##
// # Sll
void Sll_add(Sll** root, Sll* node) {
  eprintf("Hi\n");
  eprintf("root=%X, node=%X\n", *root, node);
  Sll* next = (*root) ? (*root)->next : NULL;
  *root = node;
  node->next = next;
}

Sll* Sll_pop(Sll** root) {
  if(not *root) return NULL;
  Sll* out = *root;
  *root = (*root)->next;
  return out;
}

// #################################
// # BA: Block Allocator

Sll*  BANode_asSll(BANode* node) { return (Sll*) node; }
Sll** BANode_asSll2(BANode** node) { return (Sll**) node; }
Sll** BA_asSll2(BA* ba) { return BANode_asSll2(&ba->free); }

BANode* BA_alloc(BA* ba) {
  BANode* out = (BANode*) Sll_pop(BA_asSll2(ba));
  if(not out) return NULL;
  ba->len -= 1;
  return out;
}

void BA_free(BA* ba, BANode* node) {
  Sll_add(BA_asSll2(ba), BANode_asSll(node));
  ba->len += 1;
}

void BA_freeAll(BA* ba, BANode* nodes) {
  while(nodes) {
    BANode* next = nodes->next;
    BA_free(ba, nodes);
    nodes = next;
  }
}


// #################################
// # BBA: Block Bump Arena
DEFINE_AS(Arena,  /*as*/Resource);
#define BBA_block(BBA)  &(BBA)->ba->blocks[(BBA)->rooti]
#define BBA_bheap(BBA)  (U1*)((U1*)(BBA)->block + (BBA)->len)
#define BBA_theap(BBA)  (U1*)((U1*)(BBA)->block + (BBA)->cap)
#define BBA_cachedLen(BBA)  *(U2*)( (U1*)(BBA)->block + (BLOCK_SIZE - 4) )
#define BBA_cachedCap(BBA)  *(U2*)( (U1*)(BBA)->block + (BLOCK_SIZE - 2) )

BBA BBA_new(BA* ba) { return (BBA) { .ba = ba }; }

// Return true if there is a failure.
bool _allocBlockIfRequired(BBA* bba, Slot grow) {
  if((bba->cap) < (bba->len) + grow) {
    if(bba->blocks->block) { // cache len and cap in current block
      BBA_cachedLen(bba) = bba->len;
      BBA_cachedCap(bba) = bba->cap;
    }
    BANode* node = BA_alloc(bba->ba, &bba->rooti);
    if(not node) return true;
    node->next = bba->blocks;
    bba->blocks = node;
    bba->len = 0;
    bba->cap = BLOCK_SIZE - sizeof(Slot);
  }
  return false;
}

U1* BBA_alloc(BBA* bba, Slot sz, U2 alignment) {
  ASSERT(sz <= BLOCK_SIZE - 4, "allocation sz too large");
  if(1 == alignment) {
    // Grow up
    if(_allocBlockIfRequired(bba, sz)) return NULL;
    U1* out = BBA_bheap(bba);
    bba->len += sz;
    return out;
  }
  // Else grow down (aligned)
  if(_allocBlockIfRequired(bba, sz + requiredBumpDown(BBA_theap(bba), alignment)))
    return NULL;
  bba->cap -= sz + requiredBumpDown(BBA_theap(bba), alignment);
  return BBA_theap(bba);
}

void BBA_drop(BBA* bba) {
  BA_freeAll(bba->blocks);
  bba->len = 0; bba->cap = 0; bba->block = NULL;
}

void BBA_free(BBA* bba, void* data, Slot sz, U2 alignment) {
  ASSERT(bba->block, "Free empty BBA");
  if(1 == alignment) {
    ASSERT(bba->len >= sz, "unordered free");
    bba->len -= sz;
  } else {
    U1* theap = BBA_theap(bba);
    ASSERT(theap <= (U1*)data and theap > (U1*)data - alignment, "unordered free");
    bba->cap = (U2)((U1*)data - (U1*)bba->block) + sz;
  }

  if(bba->cap - bba->len == BLOCK_SIZE - 4) {
    BA_free(bba->ba, bba->blocks);
    if(BLOCK_END == bba->rooti) {
      bba->block = NULL;
      bba->cap = 0;
    } else {
      bba->block = BBA_block(bba);
      bba->len = BBA_cachedLen(bba);
      bba->cap = BBA_cachedCap(bba);
    }
  }
}

MArena mBBA = (MArena) {
  .drop           = Role_METHOD(BBA_drop),
  .alloc          = Role_METHOD(BBA_alloc, Slot, U2),
  .free           = Role_METHOD(BBA_free, void*, Slot, U2),
};

Arena BBA_asArena(BBA* bba) { return (Arena) { .m = &mBBA, .d = bba }; }

// #################################
// # Civ Global Environment

void schedule(Fiber* fb) {
  fb->next = civ.fb->next;
  civ.fb->next = fb;
  fb->prev = civ.fb;
}

void yield() { civ.fb = civ.fb->next; }

void kill(Fiber* fb) {
  eprintf("??? kill self=%X next=%X\n", civ.fb, civ.fb->next);
  Fiber* next = civ.fb->next;
  if(fb == next) { civ.fb = NULL; return; } // no more fibers
  next->prev = fb->prev;
  fb->prev->next = fb->next;
  if(civ.fb == fb) yield();
}

// #################################
// # Binary Search Tree

// Find slice in Bst, starting at `*node`. Set result to `*node`
// Else, the return value is the result of `Slc_cmp(node.ckey, slc)`
//
// This can be used like this:
//   Bst* node = NULL;
//   I4 cmp = Bst_find(&node, Slc_ntLit("myNode"));
//   // if   not node    : *node was null (Bst is empty)
//   // elif cmp == 0    : *node key == "myNode"
//   // elif cmp < 0     : *node key <  "myNode"
//   // else cmp > 0     : *node key >  "myNode"
I4 Bst_find(Bst** node, Slc slc) {
  if(!*node) return 0;
  while(true) {
    I4 cmp = Slc_cmp(slc, Slc_frCStr((*node)->key));
    if(cmp == 0) return 0; // found exact match
    if(cmp < 0) {
      if((*node)->l)  *node = (*node)->l; // search left
      else            return cmp; // not found
    } else /* cmp > 0 */ {
      if((*node)->r)  *node = (*node)->r; // search right
      else            return cmp; // not found
    }
  }
}

// Add a node to the tree, modifying *root if the node becomes root.
//
// Returns NULL if `add.key` does not exist in the tree. Else returns the
// existing node.
Bst* Bst_add(Bst** root, Bst* add) {
  if(!*root) { *root = add; return NULL; } // new root
  Bst* node = *root; // prevent modification to root
  I4 cmp = Bst_find(&node, Slc_frCStr(add->key));
  if(cmp == 0) return node;
  if(cmp < 0) node->l = add;
  else        node->r = add;
  add->l = 0, add->r = 0;
  return NULL;
}

// #################################
// # File
DEFINE_AS(RFile,  /*as*/Resource);
