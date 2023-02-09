// C Bootstrapping of fngi programming language.
//
// Table of Contents:
//
// ***********************
// * 1: Initialization
// * 2: Executing Instructions
// * 3: TyDb, the type database and validator
// * 4: Token scanner
// * 5: Compiler
//   * lit / compileLit
//   * Buffer (op1, op2)
//   * scan / scanTy
//   * srOffset
//   * ftOffset
//   * compileVar, compileDict, compleFn
//   * single: compile a single token + compileSrc
// * 6: Native Functions
//   * Misc
//   * Core syn functions
//   * fn and fn signature
//   * if/elif/else
//   * blk
//   * struct
//   * '.', '&', '@'
// * 7: Registering Functions and Tys
// * 8: Execution helpers

#include "./fngi.h"

#define R0        return 0;
#define Ty_fmt(TY)    CStr_fmt((TY)->bst.key)
#define Token_fmt()   Dat_fmt(k->g.token)

#define NL eprintf("\n")

/*extern*/ Kern* fngiK = NULL;

void N_dbgRs(Kern* k);
void dbgWs(Kern *k) {
  Stk* ws = WS; U2 cap = ws->cap; U2 len = cap - ws->sp;
  eprintf("{");
  for(U2 i = 1; i <= len; i++) {
    U4 v = ws->dat[cap - i];
    if(v <= 0xFFFF)   eprintf(" %+8X", v);
    else              eprintf(" %+4X_%X", v >> 16, 0xFFFF & v);
  }
  eprintf("}");
}


// ***********************
// * 1: Initialization
// Starting global types and kernel. Creating new types on allocator.

bool FnFiber_init(FnFiber* fb) {
  U4* dat = (U4*) BA_alloc(&civ.ba);
  if(not dat) return false;
  fb->ws   = Stk_init(dat, WS_DEPTH); dat += WS_DEPTH;
  fb->info = Stk_init(dat, RS_DEPTH); dat += RS_DEPTH;
  fb->rs   = Stk_init(dat, (BLOCK_SIZE / RSIZE) - WS_DEPTH - RS_DEPTH);
  return true;
}

void baseCompFn(Kern* k);
TyFn TyFn_baseCompFn = {
  .bst.key = (CStr*) ("\x0A" "baseCompFn"),
  .meta = TY_FN | TY_FN_NATIVE,
  .v = (S)baseCompFn,
};

void Kern_init(Kern* k, FnFiber* fb) {
  *k = (Kern) {
    .bbaCode = (BBA) { &civ.ba },
    .bbaDict = (BBA) { &civ.ba },
    .bbaRepl = (BBA) { &civ.ba },
    .fb = fb,
    .g = {
      .compFn = &TyFn_baseCompFn,
      .dictStk = Stk_init(k->g.dictBuf, DICT_DEPTH),
      .token = (Buf){.dat = k->g.tokenDat, .cap = 64},
      .bbaDict = &k->bbaDict,
      .tyDb = (TyDb) {
        .tyIs = Stk_init(k->g.tyDb.tyIsDat, TYDB_DEPTH),
        .done = Stk_init(k->g.tyDb.doneDat, TYDB_DEPTH),

      }
    }, 0
  };
  assert(0 == k->g.dictBuf[DICT_DEPTH - 1]);
  Stk_add(&k->g.dictStk, 0);
}

bool Kern_eof(Kern* k) { return Reader_eof(k->g.src); }

Slc tokenSlc(Kern* k) {
  scan(k); ASSERT(not Kern_eof(k), "got EOF after '.'");
  return *Buf_asSlc(&k->g.token);
}

CStr* tokenCStr(Kern* k) {
  scan(k);
  CStr* out = CStr_new(BBA_asArena(k->g.bbaDict), *Buf_asSlc(&k->g.token));
  tokenDrop(k);
  return out;
}

// Allocate, initialize and add to dict a new Ty from the information in Kern.
// This includes the token as the fn name.
//
// It is the caller's job to initialize .v
// It is also the caller's job to handle TyFn and TyDict values.
Ty* Ty_new(Kern* k, U2 meta, CStr* key) {
  if(not key) key = tokenCStr(k);
  ASSERT(key, "Ty_new key OOM");
  U1 sz = sizeof(Ty);
  switch (TY_MASK & meta) {
    case TY_VAR:  sz = sizeof(TyVar);  break;
    case TY_FN:   sz = sizeof(TyFn);   break;
    case TY_DICT: sz = sizeof(TyDict); break;
  }
  Ty* ty = (Ty*) BBA_alloc(k->g.bbaDict, sz, 4);
  eprintf("??? newty %X  %.*s\n", ty, Dat_fmt(*key));
  memset(ty, 0, sz);
  ty->bst = (Bst) { .key = key },
  ty->parent = (Ty*) k->g.curMod,
  ty->meta = meta,
  ty->line = k->g.tokenLine,
  ty->file = k->g.srcInfo,
  Kern_addTy(k, ty);
  return ty;
}

// ***********************
// * 2: Executing Instructions
// Fngi's assembly is defined in src/spor.zty. These constants are
// auto-generated into src/gen/spor.h
//
// The VM is compsed of these few instructions, which are designed to be:
// (1) extremely close to the memory. They all directly access memory or the
//     stack.
// (2) very compact for common operations like accessing the locals stack.


//   *******
//   * 2.a: Utilities
// There are a few utility functions necessary for executing instructions and
// compiling them in tests.

// U1* heap(BBA* bba, bool top) {
//   Block* b = bba->dat->block;
//   if(top) return (U1*)b + b->top;
//   return (U1*)b + b->bot;
// }

U4 popLit(Kern* k, U1 size) {
  U4 out = ftBE(cfb->ep, size);
  cfb->ep += size;
  return out;
}

// Make sure to check isFnNative first!
static inline void executeNative(Kern* k, TyFn* fn) {
  ((void(*)(Kern*)) fn->v)(k);
}

void xImpl(Kern* k, Ty* ty) {
  TyFn* fn = tyFn(ty);
  if(isFnNative(fn)) return executeNative(k, fn);
  ASSERT(RS->sp >= fn->lSlots + 1, "execute: return stack overflow");
  INFO_ADD((S)fn);
  RS_ADD((S)cfb->ep);
  cfb->ep = (U1*)ty->v;
  RS->sp -= fn->lSlots; // grow locals (and possibly defer)
}

TyFn catchTy = (TyFn) {
  .bst.key = (CStr*) ("\x0F" "__catchMarker__"),
  .meta = TY_FN,
  .v = 0,
};

void ret(Kern* k) {
  TyFn* ty = (TyFn*) INFO_POP();
  U1 lSlots = (ty == &catchTy) ? 0 : ty->lSlots;
  RS->sp += lSlots;
  cfb->ep = (U1*)RS_POP();
}

void jmpImpl(Kern* k, void* ty) {
  TyFn* fn = tyFn(ty);
  ASSERT(0 == fn->lSlots, "jmp to fn with locals");
  cfb->ep = (U1*)fn->v;
}

void slcImpl(Kern* k, U1 sz) {
  S len = popLit(k, sz);
  WS_ADD((S)cfb->ep); WS_ADD(len); // {dat, len}
  cfb->ep += len;
}

inline static U1 executeInstr(Kern* k, U1 instr) {
  // Slc name = instrName(instr);
  // eprintf("??? instr %0.u: %+10.*s: ", k->fb->ep, Dat_fmt(name)); dbgWs(k); NL;
  U4 l, r;
  switch ((U1)instr) {
    // Operation Cases
    case NOP: R0
    case RETZ: if(WS_POP()) { R0 } // intentional fallthrough
    case RET: return RET;
    case YLD: return YLD;
    case SWP: WS_POP2(l, r); WS_ADD2(r, l); R0
    case DRP : WS_POP(); R0
    case OVR : WS_POP2(l, r); WS_ADD3(l, r, l);          R0
    case DUP : r = WS_POP(); WS_ADD2(r, r);              R0
    case DUPN: r = WS_POP(); WS_ADD(r); WS_ADD(0 == r);  R0
    case LR: WS_ADD(RS_top(k) + popLit(k, 1)); R0

    case INC : WS_ADD(WS_POP() + 1); R0
    case INC2: WS_ADD(WS_POP() + 2); R0
    case INC4: WS_ADD(WS_POP() + 4); R0
    case DEC : WS_ADD(WS_POP() - 1); R0
    case INV : WS_ADD(~WS_POP()); R0
    case NEG : WS_ADD(-WS_POP()); R0
    case NOT : WS_ADD(0 == WS_POP()); R0
    case CI1 : WS_ADD((I4) ((I1) WS_POP())); R0
    case CI2 : WS_ADD((I4) ((I2) WS_POP())); R0

    case ADD : r = WS_POP(); WS_ADD(WS_POP() + r); R0
    case SUB : r = WS_POP(); WS_ADD(WS_POP() - r); R0
    case MOD : r = WS_POP(); WS_ADD(WS_POP() % r); R0
    case SHL : r = WS_POP(); WS_ADD(WS_POP() << r); R0
    case SHR : r = WS_POP(); WS_ADD(WS_POP() >> r); R0
    case MSK : r = WS_POP(); WS_ADD(WS_POP() & r); R0
    case JN  : r = WS_POP(); WS_ADD(WS_POP() | r); R0
    case XOR : r = WS_POP(); WS_ADD(WS_POP() ^ r); R0
    case AND : r = WS_POP(); WS_ADD(WS_POP() && r); R0
    case OR  : r = WS_POP(); WS_ADD(WS_POP() || r); R0
    case EQ  : r = WS_POP(); WS_ADD(WS_POP() == r); R0
    case NEQ : r = WS_POP(); WS_ADD(WS_POP() != r); R0
    case GE_U: r = WS_POP(); WS_ADD(WS_POP() >= r); R0
    case LT_U: r = WS_POP(); WS_ADD(WS_POP() < r); R0
    case GE_S: r = WS_POP(); WS_ADD(((I4)WS_POP()) >= ((I4)r)); R0
    case LT_S: r = WS_POP(); WS_ADD(((I4)WS_POP()) < ((I4)r)); R0
    case MUL  :r = WS_POP(); WS_ADD(WS_POP() * r); R0
    case DIV_U:r = WS_POP(); WS_ADD(WS_POP() / r); R0
    case DIV_S: WS_POP2(l, r); ASSERT(r, "Div zero");
                WS_ADD((I4)l / (I4)r);             R0

    // Mem Cases
    case SZ1 + FT: WS_ADD(*(U1*)WS_POP()); R0
    case SZ2 + FT: WS_ADD(*(U2*)WS_POP()); R0
    case SZ4 + FT: WS_ADD(*(U4*)WS_POP()); R0

    case SZ1 + FTBE: WS_ADD(ftBE((U1*)WS_POP(), 1)); R0
    case SZ2 + FTBE: WS_ADD(ftBE((U1*)WS_POP(), 2)); R0
    case SZ4 + FTBE: WS_ADD(ftBE((U1*)WS_POP(), 4)); R0

    case SZ1 + FTO: WS_ADD(*(U1*) (WS_POP() + popLit(k, 1))); R0
    case SZ2 + FTO: WS_ADD(*(U2*) (WS_POP() + popLit(k, 1))); R0
    case SZ4 + FTO: WS_ADD(*(U4*) (WS_POP() + popLit(k, 1))); R0

    case SZ1 + FTLL: WS_ADD(*(U1*) (RS_top(k) + popLit(k, 1))); R0
    case SZ2 + FTLL: WS_ADD(*(U2*) (RS_top(k) + popLit(k, 1))); R0
    case SZ4 + FTLL: WS_ADD(*(U4*) (RS_top(k) + popLit(k, 1))); R0

    case SZ1 + SR: WS_POP2(l, r); *(U1*)l = r; R0
    case SZ2 + SR: WS_POP2(l, r); *(U2*)l = r; R0
    case SZ4 + SR: WS_POP2(l, r); *(U4*)l = r; R0

    case SZ1 + SRBE: WS_POP2(l, r); srBE((U1*)l, 1, r); R0
    case SZ2 + SRBE: WS_POP2(l, r); srBE((U1*)l, 2, r); R0
    case SZ4 + SRBE: WS_POP2(l, r); srBE((U1*)l, 4, r); R0

    case SZ1 + SRO: WS_POP2(l, r); *(U1*)(l + popLit(k, 1)) = r; R0
    case SZ2 + SRO: WS_POP2(l, r); *(U2*)(l + popLit(k, 1)) = r; R0
    case SZ4 + SRO: WS_POP2(l, r); *(U4*)(l + popLit(k, 1)) = r; R0

    case SZ1 + SRLL: *(U1*) (RS_top(k) + popLit(k, 1)) = WS_POP(); R0
    case SZ2 + SRLL: *(U2*) (RS_top(k) + popLit(k, 1)) = WS_POP(); R0
    case SZ4 + SRLL: *(U4*) (RS_top(k) + popLit(k, 1)) = WS_POP(); R0

    case SZ1 + LIT: WS_ADD(popLit(k, 1)); R0
    case SZ2 + LIT: WS_ADD(popLit(k, 2)); R0
    case SZ4 + LIT: WS_ADD(popLit(k, 4)); R0

    // // Jmp Cases
    case LCL: // grow locals stack
      assert(false);
  //     r = (U1)WS_POP();
  //     ASSERT(CSZ.sp < CSZ.cap, "CSZ oob");
  //     l = CSZ.dat[CSZ.sp];
  //     ASSERT((U4)l + r < 0xFF, "Local SZ oob");
  //     CSZ.dat[CSZ.sp] = l + r;
  //     ASSERT((I4)CS.sp - r > 0, "Locals oob");
  //     CS.sp -= r;
  //     R0
    case XL:  xImpl(k, (Ty*)popLit(k, 4));    R0
    // case JW:  cfb->ep = (U1*)WS_POP();  R0;
    case XW:  xImpl(k, (Ty*)WS_POP());     R0;

    case SZ1 + JL: r = popLit(k, 1); cfb->ep +=  (I1)r - 1; R0
    case SZ2 + JL: r = popLit(k, 2); cfb->ep +=  (I2)r - 1; R0
    // case SZ4 + JL: r = popLit(k, 4); cfb->ep  = (U1*)r    ; R0

    case SZ1 + JLZ: r = popLit(k, 1); if(!WS_POP()) { cfb->ep += (I1)r - 1; } R0
    case SZ2 + JLZ: r = popLit(k, 2); if(!WS_POP()) { cfb->ep += (I2)r - 1; } R0
    // case SZ4 + JLZ: r = popLit(k, 4); if(!WS_POP()) { cfb->ep  = (U1*)r;    } R0

    case SZ1 + JTBL: assert(false);
    case SZ2 + JTBL: assert(false);
    case SZ4 + JTBL: assert(false); // TODO: not impl

    case SZ1 + SLIC: slcImpl(k, 1); R0;
    case SZ2 + SLIC: slcImpl(k, 2); R0;
    case SZ4 + SLIC: slcImpl(k, 4); R0;

    default: if(instr >= SLIT) { WS_ADD(0x3F & instr); R0 }
  }
  SET_ERR(SLC("Unknown instr"));
}

typedef struct { U2 i; U2 rs; bool found; } PanicHandler;
PanicHandler getPanicHandler(Kern* k) { // find the index of the panic handler
  eprintf("??? getting panic handler\n");
  Stk* info = &cfb->info;
  PanicHandler h = {.i = info->sp, .rs = RS->sp};
  while(h.i < info->cap) {
    TyFn* fn = tyFn((void*)info->dat[h.i]);
    if(&catchTy == fn) {
      eprintf("???   found!\n");
      h.found = true;
      return h;
    }
    h.i += 1; h.rs += fn->lSlots;
  }
  eprintf("???   not found!\n");
  return h;
}

bool catchPanic(Kern* k) { // handle a panic and return whether it is caught
  U2 rsDepth = Stk_len(RS);
  PanicHandler h = getPanicHandler(k);
  if(not h.found) return false;
  Stk_clear(WS); WS_ADD(rsDepth); // set WS to {rsDepth}
  cfb->info.sp = h.i;
  RS->sp = h.rs;
  ret(k); // ret from catchable fn
  return true;
}

void yield() {
  assert(false);
}

void executeLoop(Kern* k) { // execute fibers until all fibers are done.
  jmp_buf local_errJmp;
  jmp_buf* prev_errJmp = civ.fb->errJmp; civ.fb->errJmp = &local_errJmp;
  while(cfb) {
    if(setjmp(local_errJmp)) { // got panic
      eprintf("??? got panic\n");
      if(!catchPanic(k)) {
        civ.fb->errJmp = prev_errJmp;
        Slc path = CStr_asSlcMaybe(k->g.srcInfo->path);
        eprintf("!! Uncaught panic: %.*s[%u]\n", Dat_fmt(path), k->g.srcInfo->line);
        longjmp(*prev_errJmp, 1);
        assert(false);
      }
    }
    U1 res = executeInstr(k, popLit(k, 1));
    if(res) {
      if(YLD == res) yield();
      else /* RET */ {
        if(0 == Stk_len(RS)) {  // empty stack, fiber done
          cfb->ep = NULL;
          break;
        } else ret(k);
      }
    }
  }
  civ.fb->errJmp = prev_errJmp;
}

void executeFn(Kern* k, TyFn* fn) {
  if(isFnNative(fn)) return executeNative(k, fn);
  cfb->ep = (U1*)fn->v;
  executeLoop(k);
}

// ***********************
// * 3: TyDb, the type database and validator
// The type database is a stack of TyI Singly Linked Lists.
// Whenever a function is called, it's inputs are popped from the top SLL and
// the outputs are added.
// If statements/etc utilize split/merge operations.

TYIS(/*extern*/) // global TyI and TyIs, see fngi.h

static inline S szIToSz(U1 szI) {
  switch(SZ_MASK & szI) {
    case SZ1: return 1;
    case SZ2: return 2;
    case SZ4: return 4;
  }
  assert(false);
}

static inline S szToSzI(U1 sz) {
  switch (sz) {
    case 1: return SZ1;
    case 2: return SZ2;
    case 4: return SZ4;
  }
  assert(false);
}

S TyDict_size(TyDict* ty) {
  if(isDictNative(ty)) return szIToSz(ty->v);
  else                 return ty->sz;
}

Ty* TyDict_find(TyDict* dict, Slc s) {
  Bst* find = TyDict_bst(dict);
  I4 i = Bst_find(&find, s);
  if(i) return NULL;
  else  return (Ty*) find;
}

Ty* TyDict_scanTy(Kern* k, TyDict* dict) {
    scan(k);
    Ty* ty = TyDict_find(dict, tokenSlc(k));
    if(not ty) return NULL;
    tokenDrop(k); return ty;
}

TyVar* TyDict_field(TyDict* d, TyI* field) {
  TyVar* var = (TyVar*) TyDict_find(d, CStr_asSlc(field->name));
  assert(var && isTyVar((Ty*)var) && not isVarGlobal(var));
  return var;
}

S TyI_sz(TyI* tyI) {
  if(TyI_refs(tyI)) return RSIZE;
  ASSERT(isTyDict(tyI->ty), "TyI must have refs or be a dict.");
  return TyDict_size((TyDict*)tyI->ty);
}

bool TyI_eq(TyI* r, TyI* g) { // r=require g=given
  if(TyI_refs(r) != TyI_refs(g)) return false;
  if(r->ty == g->ty)             return true;
  if((&Ty_S == r->ty) or (&Ty_U4 == r->ty)) {
    if(&Ty_U1 == g->ty) return true;
    if(&Ty_U2 == g->ty) return true;
    if(&Ty_U4 == g->ty) return true;
    if(&Ty_S  == g->ty) return true;
  } else if ((&Ty_U2 == r->ty) and (&Ty_U1 == g->ty)) return true;
  else if ((&Ty_I4 == r->ty) or (&Ty_SI == r->ty)) {
    if(&Ty_I4 == g->ty) return true;
    if(&Ty_SI == g->ty) return true;
  }
  // TODO: for structs, allow references to structs where the given has a prefix
  // of the required.
  return false;
}

void TyI_print(TyI* tyI) {
  if(tyI->name) eprintf("%.*s", Dat_fmt(*tyI->name));
  else          eprintf("_");
  eprintf(":");
  for(U1 refs = TyI_refs(tyI), i = 0; i < refs; i++) eprintf("&");
  eprintf("%.*s", Dat_fmt(*tyI->ty->bst.key));
}

void TyI_printAll(TyI* tyI) {
  if(not tyI) return;
  TyI_printAll(tyI->next);
  eprintf(" "); TyI_print(tyI);
}

TyI* TyI_cloneNode(TyI* node, BBA* bba) {
  TyI* cloned = BBA_alloc(bba, sizeof(TyI), RSIZE); ASSERT(cloned, "tyClone OOM");
  *cloned = *node; cloned->next = NULL;
  return cloned;
}

// Sll_add((Sll**)TyDb_root(&k->g.tyDb), TyI_asSll(TyI_cloneNode(node, k->g.bbaTy)));
void TyI_cloneAddNode(BBA* bba, TyI** root, TyI* node) {
  Sll_add((Sll**)root, TyI_asSll(TyI_cloneNode(node, bba)));
}

// Clone from bottom to top, pushing onto root.
// The result will be in the same order.
// This is done so that freeing is done from top to bottom.
void  TyI_cloneAdd(BBA* bba, TyI** root, TyI* nodes) {
  if(not nodes) return;
  TyI_cloneAdd(bba, root, nodes->next);
  TyI_cloneAddNode(bba, root, nodes);
}

TyI* TyI_copy(BBA* bba, TyI* nodes) {
  TyI* root = NULL;
  TyI_cloneAdd(bba, &root, nodes);
  return root;
}

#define IS_UNTY  (C_UNTY & k->g.fnState)

void dbgTyIs(Kern* k) {
  if(IS_UNTY) return;
  eprintf("["); TyI_printAll(TyDb_top(&k->g.tyDb)); eprintf(" ]");
}

void TyDb_print(Kern* k) { TyI_printAll(TyDb_top(&k->g.tyDb)); }

void TyDb_pop(Kern* k) {
  Slc* err = BBA_free(
    k->g.bbaTy , Sll_pop(TyDb_rootSll(&k->g.tyDb)),
    sizeof(TyI), RSIZE);
  if(err) SET_ERR(*err);
}

void TyDb_free(Kern* k, TyI* stream) {
  while(stream) {
    stream = stream->next;
    TyDb_pop(k);
  }
}

void TyDb_swap(Kern* k) {
  Stk* s = &k->g.tyDb.tyIs;
  ASSERT(Stk_len(s) > 1, "swapping with len < 2");
  S cached = s->dat[s->sp];
  s->dat[s->sp] = s->dat[s->sp + 1];
  s->dat[s->sp + 1] = cached;
}

void TyDb_drop(Kern* k) {
  TyDb* db = &k->g.tyDb;
  TyDb_free(k, TyDb_top(db));
  Stk_pop(&db->tyIs);
  Stk_pop(&db->done);
}

void tyNotMatch(TyI* require, TyI* given) {
      eprintf("!!   Given  :"); TyI_printAll(given);
    eprintf("\n!!   Require:"); TyI_printAll(require); eprintf("\n");
}

// Check two type stacks.
void tyCheck(TyI* require_, TyI* given_, bool sameLen, Slc errCxt) {
  TyI* require = require_, *given = given_;
  S i = 0;
  while(require) {
    if(not given) {
      if(not ERR_EXPECTED) {
        eprintf("!! Detected stack underflow, need additional types:\n");
        tyNotMatch(require_, given_);
      }
      SET_ERR(errCxt);
    }
    if(not TyI_eq(require, given)) {
      if(not ERR_EXPECTED) {
        eprintf("!! Given %u Types don't match Require\n", i);
        tyNotMatch(require_, given_);
      }
      SET_ERR(errCxt);
    }
    given = given->next; require = require->next; i += 1;
  }
  if(sameLen and given) {
    if(not ERR_EXPECTED) {
      eprintf("!! Type lengths differ:\n"); tyNotMatch(require_, given_);
    }
    SET_ERR(errCxt);
  }
}

// Call a function or equivalent, checking and popping inp; then pushing the out.
void tyCall(Kern* k, TyI* inp, TyI* out) {
  if(IS_UNTY) return;
  if(TyDb_done(k)) {
    ASSERT(not inp and not out, "Code after guaranteed 'ret'");
  }
  TyI** root = TyDb_root(&k->g.tyDb);

  tyCheck(inp, *root, false, SLC("Type error during operation (i.e. function call, struct, etc)"));
  TyDb_free(k, inp);
  TyI_cloneAdd(k->g.bbaTy, root, out);
}

// Check function return type and possibly mark as done.
void tyRet(Kern* k, bool done) {
  if(IS_UNTY) return;
  TyDb* db = &k->g.tyDb;
  ASSERT(not TyDb_done(k), "Code after guaranteed 'ret'");
  tyCheck(tyFn(k->g.curTy)->out, TyDb_top(db), true, SLC("Type error during 'ret'"));
  TyDb_setDone(db, done);
}

// Clone the current snapshot and use clone for continued mutations.
void tyClone(Kern* k, U2 depth) {
  if(IS_UNTY) return;
  TyDb* db = &k->g.tyDb;
  TyI* stream = TyDb_index(db, depth);
  TyDb_new(db);
  if(stream) TyI_cloneAdd(k->g.bbaTy, TyDb_root(&k->g.tyDb), stream);
}

void tyMerge(Kern* k) {
  if(IS_UNTY) return;
  TyDb* db = &k->g.tyDb;
  ASSERT(Stk_len(&db->tyIs) > 1, "tyMerge with 1 or fewer snapshots");
  if(not TyDb_done(k)) {
    tyCheck(TyDb_index(db, 1), TyDb_top(db), true,
            SLC("Type error during merge (i.e. if/else)"));
  }
  TyDb_drop(k);
}

// ***********************
// * 4: Token scanner

U1 toTokenGroup(U1 c) {
  if(c <= ' ')             return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if(c == '_')             return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '$' || c == '|' || c == '.' || c == ':'  ||
     c == '(' || c == ')') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

void skipWhitespace(Kern* k, Reader f) {
  Ring* r = &Xr(f, asBase)->ring;
  while(true) {
    U1* c = Reader_get(f, 0);
    if(c == NULL) return;
    if(*c > ' ')  return;
    if(*c == '\n') k->g.srcInfo->line += 1;
    Ring_incHead(r, 1);
  }
}

// Guarantees a token is in Kern.g.token
void scanRaw(Kern* k) {
  Buf* b = &k->g.token;
  if(b->len) return; // does nothing if token wasn't cleared.
  Reader f = k->g.src;
  skipWhitespace(k, f);
  U1* c = Reader_get(f, 0); if(c == NULL) return;
  const U1 firstTg = toTokenGroup(*c);
  Buf_add(b, *c);
  if(T_SINGLE == firstTg) return;
  while(true) {
    c = Reader_get(f, b->len); if(c == NULL) return;
    U1 tg = toTokenGroup(*c);
    if (tg == firstTg) {}
    else if ((tg <= T_ALPHA) && (firstTg <= T_ALPHA)) {}
    else break;
    ASSERT(b->len < b->cap, "Token too long");
    _Buf_add(b, *c);
  }
}

// Scan a line into token
void scanLine(Kern* k) {
  Reader f = k->g.src; Buf* b = &k->g.token;
  for(U2 i = 0; true; i++) {
    U1* c = Reader_get(f, b->len);
    if((NULL == c) or ('\n' == *c)) return;
    ASSERT(b->len < b->cap, "Line too long");
    _Buf_add(b, *c);
  }
}

void tokenDrop(Kern* k) {
  Buf* b = &k->g.token;
  Ring_incHead(&Xr(k->g.src, asBase)->ring, b->len);
  Buf_clear(b);
}

static inline bool tokenEq(Kern* k, Slc s) {
  return Slc_eq(s, *Buf_asSlc(&k->g.token));
}

bool tokenPeek(Kern* k, Slc s) { scan(k); return tokenEq(k, s); }

// consume token if it equals slc
bool tokenConsume(Kern* k, Slc s) {
  scan(k); bool out = tokenEq(k, s);
  if(out) tokenDrop(k);
  return out;
}

#define PEEK(T)     tokenPeek(k, SLC(T))
#define CONSUME(T)  tokenConsume(k, SLC(T))
#define REQUIRE(T)  ASSERT(CONSUME(T), "Expected: '" T "'")

U1 cToU1(U1 c) {
  if('0' <= c && c <= '9') return c - '0';
  if('a' <= c && c <= 'f') return 10 + c - 'a';
  if('A' <= c && c <= 'F') return 10 + c - 'A';
  return 0xFF; // invalid
}

// Attempt to parse a number from the token
ParsedNumber parseU4(Slc t) {
  ParsedNumber p = {0};
  U1 base = 10, i = 0;
  if(t.len > 2) {
    Slc s = Slc_slc(&t, 0, 2);
    if(Slc_eq(SLC("0b"), s)) { base = 2;  i = 2; }
    if(Slc_eq(SLC("0x"), s)) { base = 16; i = 2; }
    if(Slc_eq(SLC("0c"), s)) { assert(false); } // character
  }
  for(;i < t.len; i++) {
    if('_' == t.dat[i]) continue;
    U1 c = cToU1(t.dat[i]);
    if(0xFF == c or c >= base) { p.isNum = false; return p; }
    p.isNum = true;
    p.v = (p.v * base) + c;
  }
  return p;
}

// ***********************
// * 5: Compiler

static inline void Kern_compFn(Kern* k) { executeFn(k, k->g.compFn); }

void N_memclr(Kern* k) { // mem:&U1, len:U4
  U4 len = WS_POP(); memset((void*)WS_POP(), 0, len);
}
TyFn TyFn_memclr = TyFn_native("\x06" "memclr", 0, (S)N_memclr, &TyIs_rU1_U4, TYI_VOID);

// ***********************
//   * lit / compileLit

void lit(Buf* b, U4 v) {
  if (v <= SLIT_MAX)    { Buf_add(b, SLIT | v); }
  else if (v <= 0xFF)   { Buf_add(b, SZ1 | LIT); Buf_add(b, v); }
  else if (v <= 0xFFFF) { Buf_add(b, SZ2 | LIT); Buf_addBE2(b, v); }
  else                  { Buf_add(b, SZ4 | LIT); Buf_addBE4(b, v); }
}

void compileLit(Kern* k, U4 v, bool asNow) {
  if(asNow) return WS_ADD(v);
  tyCall(k, NULL, &TyIs_S);
  lit(&k->g.code, v);
}

// ***********************
//   * Buffer (op1, op2)

void Buf_addSz(Buf* b, S v, U1 sz) {
  switch(SZ_MASK & v) {
    case SZ1: return Buf_add(b, v);
    case SZ2: return Buf_addBE2(b, v);
    case SZ4: return Buf_addBE4(b, v);
  }
  SET_ERR(SLC("Buf_addSz: sz"));
}

// sized operation with 1 byte literal (i.e. FTLL, SRLL, etc)
void op1(Buf* b, U1 op, U1 sz, S v) {
  Buf_add(b, op | (SZ_MASK & sz));
  Buf_add(b, v);
}

void op2(Buf* b, U1 op, U1 sz, S v) {
  Buf_add(b, op | (SZ_MASK & sz));
  Buf_addBE2(b, v);
}

// Compile an operation offset
void opOffset(Buf* b, U1 op, U1 sz, U2 offset) {
  assert(op != FT && op != SR);
  if(0 == offset) {
    if     (FTO == op) return Buf_add(b, FT | (SZ_MASK & sz));
    else if(SRO == op) return Buf_add(b, SR | (SZ_MASK & sz));
  }
  op1(b, op, sz, offset);
}

// Compile a call
void opCall(Kern* k, TyFn* fn) {
  Buf* b = &k->g.code; Buf_add(b, XL); Buf_addBE4(b, (S)fn);
}

// ***********************
//   * scan / scanTy

Ty* Kern_findTy(Kern* k, Slc t) {
  Ty* ty = NULL;
  Stk* dicts = &k->g.dictStk;
  for(U2 i = dicts->sp; i < dicts->cap; i++) {
    ty = (Ty*)dicts->dat[i];
    I4 res = Bst_find((Bst**)&ty, t);
    if((0 == res) && (ty != NULL)) return ty;
  }
  return NULL;
}
Ty* Kern_findToken(Kern* k) { return Kern_findTy(k, *Buf_asSlc(&k->g.token)); }

// Scan, immediately executing comment functions
void scan(Kern* k) {
  while(true) {
    scanRaw(k); Ty* ty = Kern_findToken(k);
    if(ty and isTyFn(ty) and isFnComment((TyFn*)ty)) {
      tokenDrop(k);
      executeFn(k, (TyFn*)ty);
    } else return;
  }
}

// Bst* Bst_add(Bst** root, Bst* add);
void Kern_addTy(Kern* k, Ty* ty) {
  ty->bst.l = NULL; ty->bst.r = NULL;
  Stk* dicts = &k->g.dictStk;
  ASSERT(dicts->sp < dicts->cap, "No dicts");
  Bst** root = (Bst**) &dicts->dat[dicts->sp];
  ty = (Ty*)Bst_add(root, (Bst*)ty);
  if(ty) {
    eprintf("!! Overwritten key: %.*s\n", Dat_fmt(*ty->bst.key));
    SET_ERR(SLC("key was overwritten"));
  }
}

Ty* scanTy(Kern* k) {
  scan(k);  Ty* ty = Kern_findToken(k);
  if(not ty) return NULL;
  tokenDrop(k);
  return ty;
}

typedef struct { Ty* ty; U1 refs; U1 derefs; } TySpec;
TySpec scanTySpec(Kern *k) {
  TySpec s = {0};
  while(true) {
    if     (CONSUME("&")) s.refs += 1;
    else if(CONSUME("@")) s.derefs += 1;
    else break;
  }
  ASSERT(not s.derefs,           "invalid deref on type spec");
  ASSERT(s.refs <= TY_REFS, "number of '&' must be <= 3");

  s.ty = scanTy(k);
  if(not s.ty) {
    eprintf("!! Type not found: %.*s\n", Dat_fmt(k->g.token));
    SET_ERR(SLC("Type not found"));
  }
  if(isTyDict(s.ty)) eprintf("");
  else if(isTyFn(s.ty)) ASSERT(s.refs > 0, "type spec on fn must be a reference");
  else SET_ERR(SLC("type spec must be a Dict or &Fn"))
  return s;
}

TyI* scanTyI(Kern* k) {
  TySpec s = scanTySpec(k);
  ASSERT(not s.derefs, "derefs not allowed in Ty");
  TyI* tyI = (TyI*) BBA_alloc(k->g.bbaDict, sizeof(TyI), RSIZE);
  ASSERT(tyI, "scanTyI: OOM");
  *tyI = (TyI) { .meta = s.refs, .ty = s.ty };
  return tyI;
}

// ***********************
//   * srOffset
// Handles not only storing offset, but also parsing '{' syntax.

typedef struct {
  U1 op;
  bool checkTy;
  bool clear;
  bool notParse; // parse=look for '{'
} SrOffset;
void srOffsetStruct(Kern* k, TyDict* d, U2 offset, SrOffset* st);

void srOffset(Kern* k, TyI* tyI, U2 offset, SrOffset* st) {
  if(not st->notParse) {
    if(CONSUME("{")) return srOffsetStruct(k, tyDict(tyI->ty), offset, st);
    Kern_compFn(k);
  }

  if(st->checkTy) tyCall(k, tyI, NULL);
  Buf* b = &k->g.code;
  if(TyI_refs(tyI)) return opOffset(b, st->op, SZR, offset);
  ASSERT(not isTyFn(tyI->ty), "invalid fn local"); assert(isTyDict(tyI->ty));
  TyDict* d = (TyDict*) tyI->ty;
  if(isDictNative(d)) return opOffset(b, st->op, /*szI*/d->v, offset);
  assert(isDictStruct(d));
  // store struct fields from stack
  SrOffset recSt = { .op = st->op, .notParse = true };
  for(TyI* field = d->fields; field; field = field->next) {
    srOffset(k, field, offset + TyDict_field(d, field)->v, &recSt);
  }
}

// Implementation of '{ ... }'
void srOffsetStruct(Kern* k, TyDict* d, U2 offset, SrOffset* st) {
  Buf* b = &k->g.code;
  // Clear memory first
  if(st->clear) {
    switch(st->op) {
      case SRLL: op1(b, LR, 0, offset); break;
      case SRO: Buf_add(b, DUP);        break;
      default: assert(false);
    }
    lit(b, d->sz);
    opCall(k, &TyFn_memclr);
  }
  st->clear = false;
  while(not CONSUME("}")) {
    scan(k); Ty* ty = Kern_findToken(k);
    if(ty and isTyFn(ty)) {
      ASSERT(isFnSyn((TyFn*)ty), "non-syn function in {...}");
      executeFn(k, (TyFn*)ty);
      continue;
    }
    ty = TyDict_scanTy(k, d); ASSERT(ty, "field not found");
    TyVar* field = tyVar(ty); REQUIRE("=");
    srOffset(k, field->tyI, offset + field->v, st);
  }
}

// ***********************
//   * ftOffset
// Handles not only fetching offset, but also parsing '.' syntax.

typedef struct { U1 op; bool noAddTy; bool findEqual; } FtOffset;

U1 ftOpToSr(U1 ftOp) {
  switch(ftOp) {
    case FTO: return SRO;
    case FTLL: return SRLL;
  }
  assert(false);
}

void ftOffsetStruct(Kern* k, TyDict* d, TyI* field, U2 offset, FtOffset* st);

void _tyFtOffsetRefs(Kern* k, TyI* tyI, FtOffset* st) {
  if(st->noAddTy) return;
  if(st->op == FTLL) return tyCall(k, NULL, tyI);
  assert(st->op == FTO); assert(not tyI->next);
  tyCall(k, NULL, tyI);
}

// Fetch an offset using st->op operation.
// This defines the basic syntax for `foo.bar = baz`
void ftOffset(Kern* k, TyI* tyI, U2 offset, FtOffset* st) {
  bool addTy = not st->noAddTy; Buf* b = &k->g.code;
  if(TyI_refs(tyI)) {
    _tyFtOffsetRefs(k, tyI, st);
    return opOffset(b, st->op, SZR, offset);
  }
  ASSERT(not isTyFn(tyI->ty), "invalid fn local");
  ASSERT(isTyDict(tyI->ty), "invalid non-dict fetch");
  TyDict* d = (TyDict*) tyI->ty;
  if(st->findEqual && CONSUME("=")) {
    SrOffset srSt = (SrOffset) {.op = ftOpToSr(st->op), .checkTy = true, .clear = true};
    return srOffset(k, tyI, offset, &srSt);
  }
  if(isDictNative(d)) {
    if(addTy) tyCall(k, NULL, tyI);
    return opOffset(b, st->op, d->v, offset);
  }
  ASSERT(isDictStruct(d), "unknown dict type");
  if(CONSUME(".")) {
    TyVar* field = (TyVar*) TyDict_find(d, tokenSlc(k));
    tokenDrop(k);
    assert(isTyVar((Ty*)field)); // TODO: only field implemented
    assert(not isVarGlobal(field));
    return ftOffset(k, field->tyI, offset + field->v, st);
  }
  if(addTy) tyCall(k, NULL, tyI);
  st->noAddTy = true;
  ftOffsetStruct(k, d, d->fields, offset, st);
}

void ftOffsetStruct(Kern* k, TyDict* d, TyI* field, U2 offset, FtOffset* st) {
  if(not field) return;
  ftOffsetStruct(k, d, field->next, offset, st);
  ftOffset(k, field, offset + TyDict_field(d, field)->v, st);
}


// ***********************
//   * compileTy (Var, Dict, Fn)

void compileVar(Kern* k, TyVar* v, bool asNow) {
  if(isVarGlobal(v)) assert(false);
  ASSERT(not asNow, "'$' used with local variable");
  if(CONSUME("=")) {
    SrOffset st = (SrOffset) {.op = SRLL, .checkTy = true };
    srOffset(k, v->tyI, /*offset=*/v->v, &st);
  } else {
    FtOffset st = (FtOffset) {.op = FTLL, .findEqual = true };
    ftOffset(k, v->tyI, /*offset=*/v->v, &st);
  }
}

void compileDict(Kern* k, TyDict* ty, bool asNow) {
  ASSERT(not asNow, "$compileDict not allowed");
  Kern_compFn(k); // compile next token
  if(IS_UNTY) return;
  TyI tyI = (TyI) {.ty = (Ty*) ty};
  TyI* top = TyDb_top(&k->g.tyDb);
  if(isDictNative(ty)) {
    if(TyI_refs(top)) {
      ASSERT(RSIZE == TyI_sz(top), "Cannot convert pointer to less than RSIZE");
    } else {
      ASSERT((isTyDict(top->ty) && isDictNative((TyDict*)top->ty)),
           "Cannot cast non-native as native.");
    }
    TyDb_pop(k); tyCall(k, NULL, &tyI);
  } else {
    assert(isDictStruct(ty));
    tyCall(k, ty->fields, &tyI);
  }
}

void compileFn(Kern* k, TyFn* fn) {
  tyCall(k, fn->inp, fn->out);
  Buf* b = &k->g.code;
  if(isFnInline(fn)) { // inline: len before function code.
    return Buf_extend(b, (Slc){(U1*)fn->v, .len=fn->len});
  }
  Buf_add(b, XL); Buf_addBE4(b, (U4)fn);
}

void single(Kern* k, bool asNow);
void baseCompFn(Kern* k) {
  if(Kern_eof(k)) return;
  single(k, false);
}


void checkName(Kern* k, bool check) {
  if(not check) {
    eprintf("!!! name not found token: `%.*s`\n", Dat_fmt(k->g.token));
    SET_ERR(SLC("name not found"));
  }
}

void compileTy(Kern* k, Ty* ty, bool asNow) {
  checkName(k, ty);
  if(isTyConst(ty)) return compileLit(k, ty->v, asNow);
  if(isTyVar(ty))   return compileVar(k, (TyVar*) ty, asNow);
  if(isTyDict(ty))  return compileDict(k, (TyDict*) ty, asNow);
  TyFn* fn = tyFn(ty);
  if(isFnSyn(fn)) {
    WS_ADD(asNow);
    return executeFn(k, fn);
  }
  Kern_compFn(k); // non-syn functions consume next token
  if(isFnNow(fn)) { ASSERT(asNow, "function must be called with '$'"); }
  if(asNow) executeFn(k, fn);
  else      compileFn(k, fn);
}

// ***********************
//   * single: compile a single token + compileSrc
void single(Kern* k, bool asNow) {
  scan(k); Slc t = *Buf_asSlc(&k->g.token);
  if(not t.len) return;
  ParsedNumber n = parseU4(t);
  if(n.isNum) {
    tokenDrop(k);
    return compileLit(k, n.v, asNow);
  }
  Ty* ty = scanTy(k); checkName(k, ty);
  compileTy(k, ty, asNow);
}

void compileSrc(Kern* k) {
  while(true) {
    Kern_compFn(k);
    if(Kern_eof(k)) break;
  }
}

// ***********************
// * 6: Native Functions
// These functions are implemented as native C code and can be executed by fngi.

// ***********************
//   * Misc

void N_noop(Kern* k) { WS_POP(); } // noop syn function
void N_notNow(Kern* k) { ASSERT(not WS_POP(), "cannot be executed with '$'"); }
void N_unty(Kern* k) {
  WS_POP(); // ignore asNow
  k->g.fnState |= C_UNTY;
  Kern_compFn(k);
  k->g.fnState &= ~C_UNTY;
}

void _N_ret(Kern* k) { tyRet(k, true); Buf_add(&k->g.code, RET); }
void N_ret(Kern* k)  { N_notNow(k); Kern_compFn(k); _N_ret(k); }
void N_tAssertEq(Kern* k) {
  WS_POP2(U4 l, U4 r);
  eprintf("??? N_tAssertEq l=%X r=%X\n", l, r);
  N_dbgRs(k);
  eprintf("??? N_tAssertEq end dbgRs\n");
  TASSERT_EQ(l, r);
}

void N_dbgRs(Kern* k) {
  eprintf("??? dbgRs k=%X  catchTy=%X\n", k, &catchTy);
  Stk* info = &cfb->info;
  Stk* rs = RS;
  U2 r = rs->sp;
  U1* ep = cfb->ep;
  for(U2 i = info->sp; i < info->cap; i++) {
    TyFn* fn = (TyFn*) info->dat[i];
    eprintf("???   i=%u cap=%u  fn=%X\n", i, info->cap, fn);
    eprintf("???   fn->v=%X\n", fn->v);
    eprintf("! fn %.*s (%u bytes in)\n", Ty_fmt(fn), ep - fn->v);

    U1 lSlots = (fn == &catchTy) ? 0 : fn->lSlots;
    r += lSlots;
    ep = (U1*) rs->dat[r];
    r += RSIZE;
  }
}


void N_destruct(Kern* k) {
  N_notNow(k);
  Kern_compFn(k);
  TyI* top = TyDb_top(&k->g.tyDb);
  TyDict* ty = (TyDict*) top->ty;
  ASSERT(isTyDict((Ty*)ty) && isDictStruct(ty), "destruct non-dict");
  ASSERT(not TyI_refs(top), "cannot destruct reference (yet?)");
  TyI tyI = (TyI){.ty = (Ty*)ty};
  tyCall(k, &tyI, ty->fields);
}

// ***********************
//   * Core syn functions

void _N_dollar(Kern* k);
TyFn _TyFn_dollar = TyFn_native("\x01" "$", TY_FN_SYN, (S)_N_dollar, TYI_VOID, TYI_VOID);
void _N_dollar(Kern* k) {
  if(Kern_eof(k)) return;
  single(k, true);
}

void N_dollar(Kern*k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_dollar;
  executeFn(k, &_TyFn_dollar);
  k->g.compFn = cfn;
}

void N_paren(Kern* k) {
  WS_POP(); // ignore asNow
  while(true) {
    if(CONSUME(")")) break;
    ASSERT(not Kern_eof(k), "expected ')' but reached EOF");
    Kern_compFn(k);
  }
}

void _N_fslash(Kern* k);
TyFn _TyFn_fslash = TyFn_native("\x07" "_fslash", 0, (S)_N_fslash, TYI_VOID, TYI_VOID);
void _N_fslash(Kern* k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_fslash;
  scanRaw(k);
  // Execute next token if it is '(', else drop it.
  if(tokenEq(k, SLC("("))) single(k, true);
  else                           tokenDrop(k);
  k->g.compFn = cfn;
}

void N_fslash(Kern* k) {
  U1* c = Reader_get(k->g.src, 0); ASSERT(c, "Comment: got EOF");
  if('\n' == *c) return; // immediate newline
  if(' ' == *c) { scanLine(k); return tokenDrop(k); }
  return _N_fslash(k);
}

void _fnMetaNext(Kern* k, U2 meta) {
  N_notNow(k);
  ASSERT(not (k->g.metaNext & TY_FN_TY_MASK),
         "fn can only be one main type: now, syn, inline, comment");
  k->g.metaNext |= meta;
}

// ***********************
//   * fn and fn signature

void N_now(Kern* k)     { _fnMetaNext(k,   TY_FN_NOW); }
void N_syn(Kern* k)     { _fnMetaNext(k,   TY_FN_SYN); }
void N_inline(Kern* k)  { _fnMetaNext(k,   TY_FN_INLINE); }
void N_comment(Kern* k) { _fnMetaNext(k,   TY_FN_COMMENT); }

#define SET_FN_STATE(STATE)  k->g.fnState = bitSet(k->g.fnState, STATE, C_FN_STATE)
#define IS_FN_STATE(STATE)   ((C_FN_STATE & k->g.fnState) == (STATE))

void synFnFound(Kern* k, Ty* ty) {
  ASSERT(isTyFn(ty) and isFnSyn((TyFn*)ty),
         "expect only syn functions in function signature");
  executeFn(k, (TyFn*)ty);
}

void varImpl(Kern* k, TyVar* var, S ptr) {
  if(ptr) var->v = ptr;
  else { // uses fn locals offset
    S sz = TyI_sz(var->tyI);
    var->v = align(k->g.fnLocals, alignment(sz));
    k->g.fnLocals = var->v + sz;
  }
}

// Used for both stk and out
void N_stk(Kern *k) {
  N_notNow(k);
  Sll** root;
  if(     IS_FN_STATE(FN_STATE_STK)) root = TyFn_inpRoot(tyFn(k->g.curTy));
  else if(IS_FN_STATE(FN_STATE_OUT)) root = TyFn_outRoot(tyFn(k->g.curTy));
  else { SET_ERR(SLC("stk used after local variable inputs or in body")); }
  CONSUME(":");
  Sll_add(root, TyI_asSll(scanTyI(k)));
}
TyFn TyFn_stk = TyFn_native("\x03" "stk", TY_FN_SYN, (S)N_stk, TYI_VOID, TYI_VOID);

CStr* scanDedupCStr(Kern* k) {
  Ty* found = scanTy(k);
  if(found) return found->bst.key;
  else      return tokenCStr(k);
}

TyVar* varPre(Kern* k) {
  CStr* key = scanDedupCStr(k);
  TyVar* var = (TyVar*) Ty_new(k, TY_VAR, key);
  REQUIRE(":");
  TyI* tyI = scanTyI(k);
  tyI->name = key;
  var->tyI = tyI;
  return var;
}

void N_inp(Kern* k) {
  N_notNow(k);
  ASSERT(IS_FN_STATE(FN_STATE_STK) or IS_FN_STATE(FN_STATE_INP)
         , "inp used after out");
  TyVar* var = varPre(k);
  SET_FN_STATE(FN_STATE_INP);
  TyI* tyI = TyI_cloneNode(var->tyI, k->g.bbaDict);
  Sll_add(TyFn_inpRoot(tyFn(k->g.curTy)), TyI_asSll(tyI));
  varImpl(k, var, /*ptr=*/0);
}
TyFn TyFn_inp = TyFn_native("\x03" "inp", TY_FN_SYN, (S)N_inp, TYI_VOID, TYI_VOID);

void N_var(Kern* k) {
  N_notNow(k); ASSERT(not IS_FN_STATE(FN_STATE_NO), "var used outside fn");
  TyVar* v = varPre(k);
  varImpl(k, v, /*ptr=*/0);
  if(CONSUME("=")) {
    SrOffset st = (SrOffset) {.op = SRLL, .checkTy = true };
    srOffset(k, v->tyI, /*offset=*/v->v, &st);
  }
}

void fnSignature(Kern* k, TyFn* fn, Buf* b/*code*/) {
  SET_FN_STATE(FN_STATE_STK);
  while(true) {
    if(CONSUME("do")) break;
    if(CONSUME("->")) SET_FN_STATE(FN_STATE_OUT);
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "expected 'do' but reached EOF");
    WS_ADD(/*asNow=*/ false); // all of these are syn functions
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)
       and not Slc_eq(SLC("&"), CStr_asSlc(ty->bst.key))) {
      tokenDrop(k); executeFn(k, (TyFn*)ty);
    } else if (IS_FN_STATE(FN_STATE_OUT)) N_stk(k);
    else                                  N_inp(k);
  }

  // Walk input and store locals and push stack types
  for(TyI* tyI = fn->inp; tyI; tyI = tyI->next) {
    TyVar* v = (TyVar*)Kern_findTy(k, CStr_asSlcMaybe(tyI->name));
    if(v and isTyVar((Ty*)v) and not isVarGlobal(v)) {
      SrOffset st = (SrOffset) { .op = SRLL, .checkTy = false, .notParse = true };
      srOffset(k, tyI, v->v, &st);
    }
    else if(/*isStk and*/ not IS_UNTY) {
      TyI_cloneAddNode(k->g.bbaTy, TyDb_root(&k->g.tyDb), tyI);
    }
  }
}

// fn NAME do (... code ...)
// future:
// fn ... types ... do ( ... code ... )
void N_fn(Kern* k) {
  N_notNow(k);
  U2 meta = k->g.metaNext;
  // Create TyFn based on NAME
  scan(k); TyFn* fn = (TyFn*) Ty_new(k, TY_FN | meta, NULL);
  tokenDrop(k);
  k->g.curTy = (Ty*) fn;

  Buf* code = &k->g.code;  Buf prevCode = *code;
  *code = Buf_new(BBA_asArena(&k->bbaCode), FN_ALLOC);
  ASSERT(code->dat, "Code OOM");

  const U2 db_startLen = Stk_len(&k->g.tyDb.done);
  TyDb_new(&k->g.tyDb); LOCAL_BBA(bbaTy); // type stack
  LOCAL_BBA(bbaDict); Stk_add(&k->g.dictStk, 0); // local variables

  fnSignature(k, fn, code);
  SET_FN_STATE(FN_STATE_BODY); Kern_compFn(k); // compile the fn body
  SET_FN_STATE(FN_STATE_NO);

  // Force a RET at the end, whether UNTY or not.
  if( (not IS_UNTY and not TyDb_done(k))
      or  (IS_UNTY and (RET != code->dat[code->len-1]))) _N_ret(k);

  // Free unused area of buffers
  ASSERT(not BBA_free(&k->bbaCode, code->dat + code->len, code->cap - code->len, 1),
         "N_fn free");

  fn->v = (S)code->dat; fn->len = code->len;
  fn->lSlots = align(k->g.fnLocals, RSIZE) / RSIZE;
  k->g.fnLocals = 0; k->g.metaNext = 0;
  *code = prevCode;
  fn->inp = TyI_copy(prev_bbaDict, fn->inp); // move out of bbaDict
  fn->out = TyI_copy(prev_bbaDict, fn->out);

  TyDb_drop(k); END_LOCAL_BBA(bbaDict); END_LOCAL_BBA(bbaTy);
  Stk_pop(&k->g.dictStk);
  ASSERT(db_startLen == Stk_len(&k->g.tyDb.done),
         "A type operation (i.e. if/while/etc) is incomplete in fn");
}

// ***********************
//   * if/elif/else
// Type checked if/elif/else flow.
//
// The basic rule is that the result type of all branches must be identical,
// Where branches that trigger tyRet are skipped/ignored. In essence this means
// that all branches must be identical to the first "full" (non done) branch,
// so each subsequent branch is simply merged into this first one.
//
// The algorithm ends up being fairly simple, and can be understood
// by looking at the state of TyDb at the various points. There are two
// possible states:
//
//   hadFull=true: there was a "full" if/elif/else branch that did not have a RET,
//   aka a branch happend and control continued.
//      TyDb -> stateToUse -> fullIfState -> outer
//
//   hadFull=false: there have been no "full" if/elif/else branches, all had RET
//   or some other escape.
//      TyDb -> stateToUse -> outer
//
// When hadFull, any block is immediately merged with fullIfState since they
// have to match. Otherwise, any block that does not have RET sets hadFull=true.

typedef struct { bool hadFull; bool hadElse; } IfState;

U2 _if(Buf* b) { // flow control only (no type checking)
  Buf_add(b, JLZ); Buf_add(b, 0);
  return b->len - 1;
}

void _endIf(Buf* b, U2 i) { // flow control only
  b->dat[i] = b->len - i;
}

U2 _else(Buf* b, U2 iIf) { // flow control only
  Buf_add(b, JL); // end of if has unconditional jmp to end of else
  Buf_add(b, 0);
  _endIf(b, iIf); // if jmps into else block
  return b->len - 1;
}

#define DBG_TYS(...) \
    if(not IS_UNTY) { eprintf(__VA_ARGS__); dbgTyIs(k); NL; }

IfState tyIf(Kern* k, IfState is) {
  if(IS_UNTY) return is;
  if(is.hadFull) {
    tyMerge(k);    // check stateToUse==fullIfState
    tyClone(k, 1); // clone outer state
  } else if (TyDb_done(k)) {
    tyMerge(k);    // drop stateToUse
    tyClone(k, 0); // clone outer state
  } else {
    tyClone(k, 1); // stateToUse becomes fullIfState. Clone outer as stateToUse
    is.hadFull = true;
  }
  return is;
}

IfState _N_if(Kern* k, IfState is) {
  Buf* b = &k->g.code;
  U2 i = _if(b);
  REQUIRE("do"); Kern_compFn(k);
  is = tyIf(k, is);

  if(CONSUME("elif")) {
    i = _else(b, i);
    Kern_compFn(k); tyCall(k, &TyIs_S, NULL);
    ASSERT(IS_UNTY or not TyDb_done(k), "Detected done in elif test");
    is = _N_if(k, is);
  } else if(CONSUME("else")) {
    i = _else(b, i);
    Kern_compFn(k); // else body
    is = tyIf(k, is);
    is.hadElse = true;
  }
  _endIf(b, i);
  return is;
}

void tyIfEnd(Kern* k, IfState is) {
  if(IS_UNTY) return;
  TyDb_drop(k); // stateToUse
  if(is.hadFull) {
    TyDb* db = &k->g.tyDb;
    // Swap the last two types
    TyI* cloned = NULL;
    TyI_cloneAdd(k->g.bbaDict, &cloned, TyDb_top(db));
    TyDb_drop(k); TyDb_drop(k);
    TyDb_new(db);
    TyI_cloneAdd(k->g.bbaTy, TyDb_root(db), cloned);
    assert(not Sll_free((Sll*)cloned, sizeof(TyI), BBA_asArena(k->g.bbaDict)));
    assert(not TyDb_done(k));
  } else if (is.hadElse) {
    TyDb_setDone(&k->g.tyDb, true);
  }
}

void N_if(Kern* k) {
  N_notNow(k);
  Kern_compFn(k); tyCall(k, &TyIs_S, NULL);
  ASSERT(IS_UNTY or not TyDb_done(k), "Detected done in if test");
  tyClone(k, 0);
  tyIfEnd(k, _N_if(k, (IfState){0}));
}

// ***********************
//   * blk
//
// Blk can be used to construct loop/while/etc. Each nesting is tied to
// a node in Globals.blk.
//
// The basic syntax is:
//
//   var x: U4 = block(
//     var i: U4 = checkThing();
//     if(i == 2) do continue; // looping
//     if(i == 3) do break 2;  // return value early
//     i // return value at end of loop.
//   );
//
// The type of the first break needs to be stored as the block type, and further
// breaks need to match this type.
//
// Continues need to reset the type back to the block beginning.
//
// Any data is allocated from bbaDict, which is the local dictionary who's
// lifetime is only the function definition.

static inline Sll** Blk_root(Kern* k) { return (Sll**) &k->g.blk; }

void tyCont(Kern* k) {
  if(IS_UNTY) return;
  Blk* blk = k->g.blk;
  TyDb* db = &k->g.tyDb;
  ASSERT(not TyDb_done(k), "Cont after guaranteed 'ret'");
  tyCheck(blk->startTyI, TyDb_top(db), /*sameLen*/true,
          SLC("Type error: cont not identical as block start."));
  TyDb_setDone(db, /*done*/true);
}

void N_cont(Kern* k) {
  N_notNow(k);
  tyCont(k);
  Buf* b = &k->g.code;
  Buf_add(b, SZ2 | JL);
  Buf_addBE2(b, k->g.blk->start - b->len - 1);
}

void tyBreak(Kern* k) {
  if(IS_UNTY) return;
  Blk* blk = k->g.blk;
  TyDb* db = &k->g.tyDb;
  ASSERT(not TyDb_done(k), "Break after guaranteed 'ret'");
  if(blk->endTyI) {
    tyCheck(blk->endTyI, TyDb_top(db), /*sameLen*/true,
            SLC("Type error: breaks not identical type."));
  } else {
    TyI_cloneAdd(k->g.bbaDict, &k->g.blk->endTyI, TyDb_top(db));
  }
  TyDb_setDone(db, /*done*/true);
}

void N_brk(Kern* k) {
  N_notNow(k); Kern_compFn(k);
  tyBreak(k);
  Buf* b = &k->g.code;

  Buf_add(b, SZ2 | JL); // unconditional jump to end of block
  Sll* br = BBA_alloc(k->g.bbaDict, sizeof(Sll), RSIZE); ASSERT(br, "brk OOM");
  Sll_add(&k->g.blk->breaks, br);  br->dat = b->len;
  Buf_addBE2(b, 0);
}

void N_blk(Kern* k) {
  N_notNow(k);
  Buf* b = &k->g.code;
  Blk* blk = BBA_alloc(k->g.bbaDict, sizeof(Blk), RSIZE);
  ASSERT(blk, "block OOM");
  *blk = (Blk) { .start = b->len };
  TyI_cloneAdd(k->g.bbaDict, &blk->startTyI, TyDb_top(&k->g.tyDb));
  Sll_add(Blk_root(k), Blk_asSll(blk));

  Kern_compFn(k); // compile code block
  if(not TyDb_done(k)) {
    tyBreak(k);
  }
  for(Sll* br = blk->breaks; br; br = br->next) {
    srBE2(b->dat + br->dat, b->len - br->dat - 1);
  }
}

// ***********************
//   * struct
//
// struct Foo [ a: U2; b: &U4 ]

void field(Kern* k, TyDict* st) {
  TyVar* var = varPre(k);
  S sz = TyI_sz(var->tyI);
  var->v = align(st->sz, alignment(sz));
  st->sz = var->v + sz;
  TyI* tyI = TyI_cloneNode(var->tyI, k->g.bbaDict);
  Sll_add(TyDict_fieldsRoot(st), TyI_asSll(tyI));
}

void N_struct(Kern* k) {
  N_notNow(k);
  TyDict* st = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_STRUCT, NULL);
  TyDict* prevMod = k->g.curMod;
  k->g.curMod = st;
  Stk_add(&k->g.dictStk, 0);
  REQUIRE("[");
  while(not CONSUME("]")) {
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "Expected ']', reached EOF");
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)) {
      tokenDrop(k); WS_ADD(/*asNow*/false); executeFn(k, (TyFn*)ty);
    } else field(k, st);
  }
  st->v = Stk_pop(&k->g.dictStk); // update "root" of struct
  k->g.curMod = prevMod;
}

// ***********************
//   * '.', '&', '@'

void N_dot(Kern* k) { // A reference is on the stack, get a (sub)field
  N_notNow(k);
  ASSERT(not IS_UNTY, "Cannot use '.' on stack references without type checking");
  TyI* top = TyDb_top(&k->g.tyDb);
  U2 refs = TyI_refs(top);
  ASSERT(refs, "invalid '.', the value on the stack is not a reference");
  TyDict* d = (TyDict*) top->ty; assert(d);
  ASSERT(isTyDict((Ty*)d), "invalid '.', the value on the stack is not a TyDict");
  ASSERT(not isDictNative(d), "invalid '.' on native type");

  Buf* b = &k->g.code;
  // Trim the refs to be only 1
  while(refs > 1) { Buf_add(b, FT | SZR); }
  TyDb_pop(k);

  Ty* ty = TyDict_scanTy(k, d); ASSERT(ty, "field not found");
  if(isTyVar(ty)) {
    TyVar* field = (TyVar*) ty;
    ASSERT(not isVarGlobal(field), "invalid '.', accessing constant through ref");
    FtOffset st = (FtOffset) {.op = FTO, .findEqual = true};
    ftOffset(k, field->tyI, field->v, &st);
    return;
  }
  // Call the method
  TyI this = (TyI) { .ty = (Ty*)d, .meta = /*refs*/1 };
  tyCall(k, NULL, &this);
  compileTy(k, ty, false);
}

// & on a nested reference type (i.e. a struct field)
void ampRef(Kern* k, TyI* tyI) {
  ASSERT(PEEK("."), "invalid &, non-variable/struct");
  Buf* b = &k->g.code;
  U2 offset = 0; U2 refs = TyI_refs(tyI);
  while(true) {
    if(CONSUME(".")) {
      while(refs > 1) {
        opOffset(b, FTO, SZR, offset); offset = 0; refs -= 1;
      }
      TyVar* var = tyVar(TyDict_find(tyDict(var->tyI->ty), tokenSlc(k)));
      assert(not isVarGlobal(var));
      tyI = var->tyI; offset += var->v;
    } else {
      ASSERT(TyI_refs(tyI) + 1 <= TY_REFS, "refs too large");
      if(offset) { lit(b, offset); Buf_add(b, ADD); }
      TyDb_drop(k);
      TyI out = (TyI) { .ty = tyI->ty, .meta = tyI->meta + 1 };
      return tyCall(k, NULL, &out);
    }
  }
}

// Struct A [ a: &S ]
// Struct B [ b: &A ]
// Struct C [ c: &B ]
// var s: S = 42;
// var a: A = A(&s) // reference to s
// var b: B = B(&a) // reference to a
// var c: C = C(&b)
//
// doThing(&b.a);   // pass reference of a through b (double ref)
// doThing(&c.b.a); // pass reference of a through c (still double ref)
//
// The last one requires a fetch
void N_amp(Kern* k) {
  N_notNow(k);
  scan(k); Ty* ty = Kern_findToken(k); ASSERT(ty, "not found");
  if(not isTyVar(ty)) {
    Kern_compFn(k);
    return ampRef(k, TyDb_top(&k->g.tyDb));
  }
  tokenDrop(k);
  TyVar* var = (TyVar*) ty;
  assert(not isVarGlobal(var)); // TODO
  U2 offset = var->v; TyI* tyI = var->tyI;
  while(not TyI_refs(tyI) and CONSUME(".")) {
    var = (TyVar*) TyDict_find(tyDict(var->tyI->ty), tokenSlc(k));
    assert(isTyVar((Ty*)var)); // TODO: support function
    offset += var->v; tyI = var->tyI;
  }
  Buf* b = &k->g.code;
  if(PEEK(".")) {
    assert(TyI_refs(tyI)); // should be guaranteed
    op1(b, FTLL, SZR, offset);
    return ampRef(k, tyI);
  }
  ASSERT(TyI_refs(tyI) + 1 <= TY_REFS, "refs too large");
  TyI out = (TyI) { .ty = tyI->ty, .meta = tyI->meta + 1 };
  tyCall(k, NULL, &out);
  op1(b, LR, 0, offset);
}

void N_at(Kern* k) {
  N_notNow(k);
  ASSERT(not IS_UNTY, "Cannot use '@' without type checking");
  Kern_compFn(k);
  // TyI* tyI = TyDb_top(&k->g.tyDb);
  // ASSERT(TyI_refs(tyI), "invalid '@', the value on the stack is not a reference");
  // FtOffset st = (FtOffset) { .op = FTO, .findEqual = true };
  // ftOffset(k, tyI, 0, &st);

  if(CONSUME("=")) {
    assert(false);
  }
  TyI* top = TyDb_top(&k->g.tyDb); U2 refs = TyI_refs(top);
  ASSERT(refs, "invalid '@', the value on the stack is not a reference");
  if(refs > 1) Buf_add(&k->g.code, FT | SZR);
  else if (not isTyDict(top->ty)) {
    SET_ERR(SLC("Cannot fetch non-dict type"));
  } else {
    TyDict* d = (TyDict*) top->ty;
    if(isDictNative(d)) Buf_add(&k->g.code, FT | (SZ_MASK & d->v));
    else assert(false);
  }

  TyI inp = (TyI) { .ty = (Ty*)top->ty, .meta = top->meta };
  TyI out = (TyI) { .ty = (Ty*)top->ty, .meta = top->meta - 1};
  tyCall(k, &inp, &out);
}

// ***********************
// * 7: Registering Functions

// Create a static TyFn and add it to the kern.
// NAMELEN: the fngi name's length; NAME: the fngi name
// META: the meta to use;  VAL: the native value to add
#define STATIC_FNTY(INP, OUT, NAMELEN, NAME, META, VAL) \
  CStr_ntVar(LINED(key), NAMELEN, NAME);\
  static TyFn LINED(Ty);                \
  LINED(Ty) = (TyFn) {                  \
    .bst.key = LINED(key),              \
    .meta =  TY_FN | META,              \
    .v = VAL,                           \
    .inp = INP, .out = OUT              \
  };                                    \
  Kern_addTy(k, (Ty*)&LINED(Ty));

#define STATIC_NATIVE(INP, OUT, NAMELEN, NAME, META, KFN) \
    STATIC_FNTY(INP, OUT, NAMELEN, NAME, TY_FN_NATIVE | (META), kFn(KFN))

#define STATIC_INLINE(INP, OUT, NAMELEN, NAME, META, ...)               \
  assert(sizeof((U1[]){__VA_ARGS__}) < 0xFF);                 \
  static U1 LINED(code)[] = {__VA_ARGS__ __VA_OPT__(,) RET};  \
  STATIC_FNTY(INP, OUT, NAMELEN, NAME, TY_FN_INLINE | (META), (S)LINED(code)); \
  LINED(Ty).len = sizeof(LINED(code)) - 1;

// For Ty that really do only take up Ty space (TY_DICT_NATIVE)
#define STATIC_TY(V, NAMELEN, NAME, META, VAL) \
  CStr_ntVar(LINED(key), NAMELEN, NAME);\
  V = (Ty) {                   \
    .bst.key = LINED(key),            \
    .meta = META,                     \
    .v = VAL,                         \
  };                                  \
  Kern_addTy(k, (Ty*)&V);

void Kern_fns(Kern* k) {
  // Native data types
  STATIC_TY(Ty_U1, "\x02", "U1",  TY_DICT | TY_DICT_NATIVE                   , SZ1);
  STATIC_TY(Ty_U2, "\x02", "U2",  TY_DICT | TY_DICT_NATIVE                   , SZ2);
  STATIC_TY(Ty_U4, "\x02", "U4",  TY_DICT | TY_DICT_NATIVE                   , SZ4);
  STATIC_TY(Ty_S , "\x01", "S",   TY_DICT | TY_DICT_NATIVE                   , SZR);
  STATIC_TY(Ty_I1, "\x02", "I1",  TY_DICT | TY_DICT_NATIVE | TY_NATIVE_SIGNED, SZ1);
  STATIC_TY(Ty_I2, "\x02", "I2",  TY_DICT | TY_DICT_NATIVE | TY_NATIVE_SIGNED, SZ2);
  STATIC_TY(Ty_I4, "\x02", "I4",  TY_DICT | TY_DICT_NATIVE | TY_NATIVE_SIGNED, SZ4);
  STATIC_TY(Ty_SI, "\x02", "SI",  TY_DICT | TY_DICT_NATIVE | TY_NATIVE_SIGNED, SZR);
  TASSERT_EMPTY();

  // Ty: S
  TyIs_S = (TyI) { .ty = &Ty_S };
  // Ty: S, s
  TyIs_SS = (TyI) { .next = &TyIs_S, .ty = &Ty_S };
  // Ty: S, s, S
  TyIs_SSS = (TyI) { .next = &TyIs_SS, .ty = &Ty_S };

  TyIs_U1 = (TyI) { .ty = &Ty_U1 };
  TyIs_U2 = (TyI) { .ty = &Ty_U2 };
  TyIs_U4 = (TyI) { .ty = &Ty_U4 };
  TyIs_U4x2 = (TyI) { .next = &TyIs_U4, .ty = &Ty_U4 };

  TyIs_rU1 = (TyI) { .ty = &Ty_U1, .meta = 1 };
  TyIs_rU2 = (TyI) { .ty = &Ty_U2, .meta = 1 };
  TyIs_rU4 = (TyI) { .ty = &Ty_U4, .meta = 1 };

  TyIs_rU1_U4 = (TyI) {.ty = &Ty_U4, .next = &TyIs_rU4};

  Kern_addTy(k, (Ty*) &TyFn_baseCompFn);
  Kern_addTy(k, (Ty*) &TyFn_stk);
  Kern_addTy(k, (Ty*) &TyFn_inp);
  Kern_addTy(k, (Ty*) &TyFn_memclr);

  // Pure noop syntax sugar
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "_"    , TY_FN_SYN       , N_noop);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", ";"    , TY_FN_SYN       , N_noop);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", ","    , TY_FN_SYN       , N_noop);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x02", "->"   , TY_FN_SYN       , N_noop);

  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x06", "notNow",  TY_FN_SYN, N_notNow);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x04", "unty",    TY_FN_SYN, N_unty);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "ret",     TY_FN_SYN, N_ret);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "$",       TY_FN_SYN, N_dollar);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "(",       TY_FN_SYN, N_paren);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "\\",      TY_FN_COMMENT, N_fslash);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "now",     TY_FN_SYN, N_now);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "syn",     TY_FN_SYN, N_syn);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x06", "inline",  TY_FN_SYN, N_inline);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x07", "comment", TY_FN_COMMENT, N_comment);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x02", "fn",      TY_FN_SYN, N_fn);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "var",     TY_FN_SYN, N_var);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x02", "if",      TY_FN_SYN, N_if);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x04", "cont",    TY_FN_SYN, N_cont);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "brk",     TY_FN_SYN, N_brk);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "blk",     TY_FN_SYN, N_blk);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x06", "struct",  TY_FN_SYN, N_struct);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", ".",       TY_FN_SYN, N_dot);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "&",       TY_FN_SYN, N_amp);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "@",       TY_FN_SYN, N_at);

  STATIC_NATIVE(&TyIs_SS, TYI_VOID, "\x09", "tAssertEq",  0, N_tAssertEq);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x05", "dbgRs"    ,  0, N_dbgRs);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x08", "destruct", TY_FN_SYN, N_destruct);

  // Stack operators. These are **not** PRE since they directly modify the stack.
  STATIC_INLINE(&TyIs_SS, &TyIs_SS,  "\x03", "swp"  , 0       , SWP   );
  STATIC_INLINE(&TyIs_S,  TYI_VOID,  "\x03", "drp"  , 0       , DRP   );
  STATIC_INLINE(&TyIs_SS, &TyIs_SSS, "\x03", "ovr"  , 0       , OVR   );
  STATIC_INLINE(&TyIs_S,  &TyIs_SS,  "\x03", "dup"  , 0       , DUP   );
  STATIC_INLINE(&TyIs_S,  &TyIs_SS,  "\x04", "dupn" , 0       , DUPN  );

  // Standard operators that use PRE syntax. Either "a <op> b" or simply "<op> b"
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "nop"  , 0, NOP  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "inc"  , 0, INC  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x04", "inc2" , 0, INC2 );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x04", "inc4" , 0, INC4 );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "dec"  , 0, DEC  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "inv"  , 0, INV  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "neg"  , 0, NEG  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "not"  , 0, NOT  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x05", "i1to4", 0, CI1  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x05", "i2to4", 0, CI2  );

  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "+"    , 0, ADD  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "-"    , 0, SUB  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "%"    , 0, MOD  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "shl"  , 0, SHL  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "shr"  , 0, SHR  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "msk"  , 0, MSK  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "jn"   , 0, JN   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "xor"  , 0, XOR  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "and"  , 0, AND  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "or"   , 0, OR   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "=="   , 0, EQ   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "!="   , 0, NEQ  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", ">="   , 0, GE_U );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "<"    , 0, LT_U );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x04", "ge_s" , 0, GE_S );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x04", "lt_s" , 0, LT_S );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "*"    , 0, MUL  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "/"    , 0, DIV_U);

  // ftN(addr): fetch a value of sz N from address.
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft1"  , 0, SZ1+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft2"  , 0, SZ2+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft4"  , 0, SZ4+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ftR"  , 0, SZR+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe1", 0, SZ1+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe2", 0, SZ2+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe4", 0, SZ4+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBeR", 0, SZR+FTBE );
  TASSERT_EMPTY();
}

// ***********************
// * 8: Execution helpers

void Kern_handleSig(Kern* k, int sig, struct sigcontext* ctx) {
  eprintf("??? Kern_handleSig k=%X sig=%u ctx=%X\n", k, sig, ctx);
  eprintf("??? Kern_handleSig infoTop=%X\n", Stk_top(&cfb->info));
  U2 cap = 100;
  // Trace t = (Trace) {
  //   .trace = (SBuf) { .dat = malloc(sizeof(S) * cap), .cap = cap },
  //   .ctx = ctx, .sig = sig
  // };
  // assert(t.trace.dat);
  void* m[100] = {0};
  eprintf("??? trace=%X trace[0]=%u\n", m, m[0]);
  S len = backtrace(m, 100);

  // Trace t = Trace_newSig(100, sig, ctx);
  eprintf("??? Kern_handleSig infoTop2=%X\n", Stk_top(&cfb->info));
  assert(false);
  // Trace_handleSig(sig, ctx);
  eprintf("! fngi return stack:\n");
  N_dbgRs(k);
  eprintf("! token=\"%.*s\" line=%u\n", Dat_fmt(k->g.token), k->g.tokenLine);
  exit(sig);
}

void fngiHandleSig(int sig, struct sigcontext ctx) {
  Kern_handleSig(fngiK, sig, &ctx); exit(sig);
}

void fngiErrPrinter() {
  Kern* k = fngiK;
  eprintf("??? fngiErrPrinter: %X\n", Stk_top(&cfb->info));
  Kern_handleSig(k, 0, NULL); }

void executeInstrs(Kern* k, U1* instrs) {
  cfb->ep = instrs;
  executeLoop(k);
}

CStr_ntLitUnchecked(replPath, "\x08", "/repl.fn");
FileInfo replInfo = {0};

U1* compileRepl(Kern* k, bool withRet) {
  replInfo.path = replPath;
  k->g.srcInfo = &replInfo;
  U1* body = (U1*) BBA_alloc(&k->bbaRepl, 256, 1);
  ASSERT(body, "compileRepl OOM");
  Buf* code = &k->g.code; *code = (Buf){.dat=body, .cap=256};
  compileSrc(k);
  if(withRet) Buf_add(code, RET);
  if(code->len < code->cap) {
    ASSERT(not BBA_free(
      &k->bbaRepl,
      /*dat*/code->dat + code->len,
      /*sz*/code->cap - code->len,
      /*alignment*/1),
      "compileRepl free");
  }
  *code = (Buf){0};
  return body;
}

void simpleRepl(Kern* k) {
  REPL_START
  size_t cap;  jmp_buf local_errJmp;  jmp_buf* prev_errJmp = civ.fb->errJmp;
  Ring_var(_r, 256);  BufFile f = BufFile_init(_r, (Buf){0});
  k->g.src = File_asReader(BufFile_asFile(&f));

  civ.fb->errJmp = &local_errJmp;
  eprintf(  "Simple REPL: type EXIT to exit\n");
  U2 rsSp = RS->sp; U2 infoSp = cfb->info.sp;
  while(true) {
    if(setjmp(local_errJmp)) { // got panic
      eprintf("!! Caught panic, WS: "); dbgWs(k); NL;
      RS->sp = rsSp; cfb->info.sp = infoSp;
      Ring_clear(&f.ring); Buf_clear(&k->g.token);
    }

    size_t len = getline((char**)&f.b.dat, &cap, stdin);
    ASSERT(len < 0xFFFF, "input too large");
    f.b.plc = 0; f.b.len = len; f.b.cap = len; f.code = File_DONE;
    eprintf("##### Input: %.*s", Dat_fmt(f.b));
    if(0 == strcmp("EXIT", f.b.dat)) break;
    if(len-1) executeInstrs(k, compileRepl(k, true));
    eprintf("> "); dbgWs(k);
    eprintf(" :"); TyI_printAll(TyDb_top(&k->g.tyDb)); NL;
  }
  civ.fb->errJmp = prev_errJmp;
  REPL_END
}
