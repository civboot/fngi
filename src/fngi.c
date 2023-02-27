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
#define TOKEN         Dat_fmt(k->g.token)

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

// DictStk

TyDict* DictStk_pop(DictStk* stk) {
  ASSERT(stk->sp < stk->cap, "DictStk underflow");
  return stk->dat[stk->sp ++];
}

TyDict* DictStk_top(DictStk* stk) {
  ASSERT(stk->sp < stk->cap, "DictStk_top OOB");
  return stk->dat[stk->sp];
}

void DictStk_add(DictStk* stk, TyDict* r) {
  ASSERT(stk->sp, "DictStk overflow");
  stk->dat[-- stk->sp] = r;
}


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
  .code = (U1*)baseCompFn,
};

void TyDb_init(TyDb* db) {
  db->tyIs = Stk_init(db->tyIsDat, TYDB_DEPTH);
  db->done = Stk_init(db->doneDat, TYDB_DEPTH);
}

void DictStk_reset(Kern* k) {
  k->g.dictStk.sp = k->g.dictStk.cap;
  DictStk_add(&k->g.dictStk, &k->g.rootDict);
}

void Kern_init(Kern* k, FnFiber* fb) {
  *k = (Kern) {
    .bbaCode = (BBA) { &civ.ba },
    .bbaDict = (BBA) { &civ.ba },
    .bbaRepl = (BBA) { &civ.ba },
    .fb = fb,
    .g = {
      .compFn = &TyFn_baseCompFn,
      .dictStk = (DictStk) { .dat = k->g.dictBuf, .sp = DICT_DEPTH, .cap = DICT_DEPTH },
      .token = (Buf){.dat = k->g.tokenDat, .cap = 64},
      .bbaDict = &k->bbaDict,
      .bbaTyImm = (BBA) { &civ.ba },
    },
  };
  TyDb_init(&k->g.tyDb); TyDb_init(&k->g.tyDbImm);
  k->g.tyDbImm.bba = &k->g.bbaTyImm;
  DictStk_reset(k);
}

bool Kern_eof(Kern* k) { return BaseFile_eof(SpReader_asBase(k, k->g.src)); }

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
  memset(ty, 0, sz);
  ty->bst = (CBst) { .key = key },
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
  ((void(*)(Kern*)) fn->code)(k);
}

void xImpl(Kern* k, Ty* ty) {
  TyFn* fn = tyFn(ty);
  if(isFnNative(fn)) return executeNative(k, fn);
  ASSERT(RS->sp >= fn->lSlots + 1, "execute: return stack overflow");
  INFO_ADD((S)fn);
  RS_ADD((S)cfb->ep);
  cfb->ep = fn->code;
  RS->sp -= fn->lSlots; // grow locals (and possibly defer)
}

TyFn catchTy = (TyFn) {
  .bst.key = (CStr*) ("\x0F" "__catchMarker__"),
  .meta = TY_FN,
  .code = NULL,
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
  cfb->ep = fn->code;
}

void slcImpl(Kern* k, U1 sz) {
  S len = popLit(k, sz);
  WS_ADD((S)cfb->ep); WS_ADD(len); // {dat, len}
  cfb->ep += len;
}

inline static U1 executeInstr(Kern* k, U1 instr) {
  Slc name = instrName(instr);
  eprintf("!!! instr %0.u: %+10.*s: ", k->fb->ep, Dat_fmt(name)); dbgWs(k); NL;
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
    case LR: WS_ADD(RS_topRef(k) + popLit(k, 2)); R0
    case GR: {
      TyVar* g = (TyVar*) popLit(k, 4);
      WS_ADD(g->v + popLit(k, 2));
      return 0;
    }

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

    case SZ1 + FTLL: WS_ADD(*(U1*) (RS_topRef(k) + popLit(k, 2))); R0
    case SZ2 + FTLL: WS_ADD(*(U2*) (RS_topRef(k) + popLit(k, 2))); R0
    case SZ4 + FTLL: WS_ADD(*(U4*) (RS_topRef(k) + popLit(k, 2))); R0

    case SZ1 + FTGL:
    case SZ2 + FTGL:
    case SZ4 + FTGL: {
      TyVar* g = (TyVar*) popLit(k, 4);
      WS_ADD(ftSzI((U1*)g->v + popLit(k, 2), SZ_MASK & instr));
      return 0;
    }

    case SZ1 + SRGL:
    case SZ2 + SRGL:
    case SZ4 + SRGL: {
      TyVar* g = (TyVar*) popLit(k, 4);
      srSzI((U1*)g->v + popLit(k, 2), SZ_MASK & instr, WS_POP());
      return 0;
    }

    case SZ1 + SR: WS_POP2(l, r); *(U1*)l = r; R0
    case SZ2 + SR: WS_POP2(l, r); *(U2*)l = r; R0
    case SZ4 + SR: WS_POP2(l, r); *(U4*)l = r; R0

    case SZ1 + SRBE: WS_POP2(l, r); srBE((U1*)l, 1, r); R0
    case SZ2 + SRBE: WS_POP2(l, r); srBE((U1*)l, 2, r); R0
    case SZ4 + SRBE: WS_POP2(l, r); srBE((U1*)l, 4, r); R0

    case SZ1 + SRO: WS_POP2(l, r); *(U1*)(l + popLit(k, 1)) = r; R0
    case SZ2 + SRO: WS_POP2(l, r); *(U2*)(l + popLit(k, 1)) = r; R0
    case SZ4 + SRO: WS_POP2(l, r); *(U4*)(l + popLit(k, 1)) = r; R0

    case SZ1 + SRLL: *(U1*) (RS_topRef(k) + popLit(k, 2)) = WS_POP(); R0
    case SZ2 + SRLL: *(U2*) (RS_topRef(k) + popLit(k, 2)) = WS_POP(); R0
    case SZ4 + SRLL: *(U4*) (RS_topRef(k) + popLit(k, 2)) = WS_POP(); R0

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
    case XL:  xImpl(k, (Ty*) popLit(k, 4));    R0
    case XLL: xImpl(k, (Ty*) (RS_topRef(k) + popLit(k, 2)));     R0;
    // The role must be in locals as {&MRole, &Data}
    case XRL: {
      S* role = (S*) (RS_topRef(k) + popLit(k, 2));
      WS_ADD((S)(role + 1)); // push &Data onto stack
      xImpl(k, (Ty*) role[popLit(k, 1)]);
      return 0;
    }

    case SZ1 + JL: r = popLit(k, 1); cfb->ep +=  (I1)r - 1; R0
    case SZ2 + JL: r = popLit(k, 2); cfb->ep +=  (I2)r - 2; R0
    // case SZ4 + JL: r = popLit(k, 4); cfb->ep  = (U1*)r    ; R0

    case SZ1 + JLZ: r = popLit(k, 1); if(!WS_POP()) { cfb->ep += (I1)r - 1; } R0
    case SZ2 + JLZ: r = popLit(k, 2); if(!WS_POP()) { cfb->ep += (I2)r - 2; } R0
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
  Stk* info = &cfb->info;
  PanicHandler h = {.i = info->sp, .rs = RS->sp};
  while(h.i < info->cap) {
    TyFn* fn = tyFn((void*)info->dat[h.i]);
    if(&catchTy == fn) {
      h.found = true;
      return h;
    }
    h.i += 1; h.rs += fn->lSlots;
  }
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
  // eprintf("!!! executeFn %.*s: ", Ty_fmt(fn)); dbgWs(k); NL;
  if(isFnNative(fn)) return executeNative(k, fn);
  cfb->ep = fn->code;
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
  ASSERT(not isDictMod(ty), "attempted size of TY_DICT_MOD");
  if(isDictNative(ty)) return szIToSz((S)ty->children);
  else                 return ty->sz;
}

Ty* TyDict_find(TyDict* dict, Slc s) {
  CBst* find = TyDict_bst(dict);
  I4 i = CBst_find(&find, s);
  if(i) return NULL;
  else  return (Ty*) find;
}

Ty* TyDict_scanTy(Kern* k, TyDict* dict) {
    scan(k); Ty* ty = TyDict_find(dict, tokenSlc(k));
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
  if(((Ty*)&Ty_S == r->ty) or ((Ty*)&Ty_U4 == r->ty)) {
    if((Ty*)&Ty_U1 == g->ty) return true;
    if((Ty*)&Ty_U2 == g->ty) return true;
    if((Ty*)&Ty_U4 == g->ty) return true;
    if((Ty*)&Ty_S  == g->ty) return true;
  } else if (((Ty*)&Ty_U2 == r->ty) and ((Ty*)&Ty_U1 == g->ty)) return true;
  else if (((Ty*)&Ty_I4 == r->ty) or ((Ty*)&Ty_SI == r->ty)) {
    if((Ty*)&Ty_I4 == g->ty) return true;
    if((Ty*)&Ty_SI == g->ty) return true;
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

void Kern_typed(Kern *k, bool typed) {
  if(typed) k->g.fnState &= ~C_UNTY;
  else      k->g.fnState |= C_UNTY;
}

#define IS_UNTY  (C_UNTY & k->g.fnState)

void dbgTyIs(Kern* k, TyDb* db) {
  if(IS_UNTY) return;
  eprintf("["); TyI_printAll(TyDb_top(db)); eprintf(" ]");
}

void TyDb_print(Kern* k, TyDb* db) { TyI_printAll(TyDb_top(db)); }

void TyDb_pop(Kern* k, TyDb* db) {
  Sll** root = TyDb_rootSll(db);
  Slc* err = BBA_free(db->bba, Sll_pop(root), sizeof(TyI), RSIZE);
  if(err) SET_ERR(*err);
}

void TyDb_free(Kern* k, TyDb* db, TyI* stream) {
  while(stream) {
    stream = stream->next;
    TyDb_pop(k, db);
  }
}

void TyDb_drop(Kern* k, TyDb* db) {
  TyDb_free(k, db, TyDb_top(db));
  Stk_pop(&db->tyIs);
  Stk_pop(&db->done);
}

// Drop the item below the top of stack while still preserving proper drop order.
// Note: This uses bbaDict to temporarily store the top value.
void TyDb_nip(Kern* k, TyDb* db) {
  TyI* top = NULL; TyI_cloneAdd(k->g.bbaDict, &top, TyDb_top(db));
  TyDb_drop(k, db); TyDb_drop(k, db); // drop both top and second
  TyDb_new(db); // re-add top
  TyI_cloneAdd(db->bba, TyDb_root(db), top);
  assert(not Sll_free((Sll*)top, sizeof(TyI), BBA_asArena(k->g.bbaDict)));
  assert(not TyDb_done(db));
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
        eprintf("!! Type stack underflow:\n");
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

void tyCall(Kern* k, TyDb* db, TyI* inp, TyI* out) {
  Slc err;
  if(db == &k->g.tyDbImm)    err = SLC("Type error at imm");
  else if (db == &k->g.tyDb) err = SLC("Type error at fn compilation");
  else                       err = SLC("Type error at UNKNOWN");
  if(IS_UNTY) return;
  if(TyDb_done(db)) ASSERT(not inp and not out, "Code after guaranteed 'ret'");
  TyI** root = TyDb_root(db);
  tyCheck(inp, *root, false, err);
  TyDb_free(k, db, inp);
  TyI_cloneAdd(db->bba, root, out);
}

// Check function return type and possibly mark as done.
void tyRet(Kern* k, TyDb* db, bool done) {
  if(IS_UNTY) return;
  ASSERT(not TyDb_done(db), "Code after guaranteed 'ret'");
  tyCheck(tyFn(k->g.curTy)->out, TyDb_top(db), true, SLC("Type error during 'ret'"));
  TyDb_setDone(db, done);
}

// Clone the current snapshot and use clone for continued mutations.
void tyClone(Kern* k, TyDb* db, U2 depth) {
  if(IS_UNTY) return;
  TyI* stream = TyDb_index(db, depth);
  TyDb_new(db);
  if(stream) TyI_cloneAdd(db->bba, TyDb_root(db), stream);
}

void tyMerge(Kern* k, TyDb* db) {
  if(IS_UNTY) return;
  ASSERT(Stk_len(&db->tyIs) > 1, "tyMerge with 1 or fewer snapshots");
  if(not TyDb_done(db)) {
    tyCheck(TyDb_index(db, 1), TyDb_top(db), true,
            SLC("Type error during merge (i.e. if/else)"));
  }
  TyDb_drop(k, db);
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
  if(c == '#' || c == '|' || c == '.' || c == ':'  ||
     c == '(' || c == ')') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

/*extern*/ MSpReader mSpReader_UFile = (MSpReader) {
  // TODO: actually implement
};

/*extern*/ MSpReader mSpReader_BufFile = (MSpReader) {
  // TODO: actually implement
};

BaseFile* SpReader_asBase(Kern* k, SpReader r) {
  if(r.m == &mSpReader_UFile)   return UFile_asBase((UFile*) r.d);
  if(r.m == &mSpReader_BufFile) return BufFile_asBase((BufFile*) r.d);
  assert(false); // not implemented
}

U1* SpReader_get(Kern* k, SpReader r, U2 i) {
  File f = (File) { .m = NULL, .d = r.d };
  if(r.m == &mSpReader_UFile)         f.m = UFile_mFile();
  else if (r.m == &mSpReader_BufFile) f.m = BufFile_mFile();
  if(f.m) return Reader_get(File_asReader(f), i);
  assert(false); // not implemented
  return (U1*)WS_POP();
}

void skipWhitespace(Kern* k, SpReader f) {
  Ring* r = &SpReader_asBase(k, f)->ring;
  while(true) {
    U1* c = SpReader_get(k, f, 0);
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
  SpReader f = k->g.src;
  skipWhitespace(k, f);
  U1* c = SpReader_get(k, f, 0); if(c == NULL) return;
  const U1 firstTg = toTokenGroup(*c);
  Buf_add(b, *c);
  if(T_SINGLE == firstTg) return;
  while(true) {
    c = SpReader_get(k, f, b->len); if(c == NULL) return;
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
  SpReader f = k->g.src; Buf* b = &k->g.token;
  for(U2 i = 0; true; i++) {
    U1* c = SpReader_get(k, f, b->len);
    if((NULL == c) or ('\n' == *c)) return;
    ASSERT(b->len < b->cap, "Line too long");
    _Buf_add(b, *c);
  }
}

void tokenDrop(Kern* k) {
  Buf* b = &k->g.token;
  Ring_incHead(&SpReader_asBase(k, k->g.src)->ring, b->len);
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
TyFn TyFn_memclr = TyFn_native("\x06" "memclr", 0, (U1*)N_memclr, &TyIs_rU1_U4, TYI_VOID);

// ***********************
//   * lit / compileLit

void lit(Buf* b, U4 v) {
  if (v <= SLIT_MAX)    { Buf_add(b, SLIT | v); }
  else if (v <= 0xFF)   { Buf_add(b, SZ1 | LIT); Buf_add(b, v); }
  else if (v <= 0xFFFF) { Buf_add(b, SZ2 | LIT); Buf_addBE2(b, v); }
  else                  { Buf_add(b, SZ4 | LIT); Buf_addBE4(b, v); }
}

void compileLit(Kern* k, U4 v, bool asImm) {
  tyCall(k, tyDb(k, asImm), NULL, &TyIs_S);
  if(asImm) {
    return WS_ADD(v);
  }
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

void op2(Buf* b, U1 op, U1 szI, S v) {
  Buf_add(b, op | (SZ_MASK & szI));
  Buf_addBE2(b, v);
}

void op42(Buf* b, U1 op, U1 szI, S v4, S v2) {
  Buf_add(b, op | (SZ_MASK & szI));
  Buf_addBE4(b, v4); Buf_addBE2(b, v2);
}

// Compile an operation offset
void opCompile(Buf* b, U1 op, U1 szI, U2 offset, TyVar* global) {
  assert(op != FT && op != SR);
  if(0 == offset) {
    if     (FTO == op) return Buf_add(b, FT | (SZ_MASK & szI));
    else if(SRO == op) return Buf_add(b, SR | (SZ_MASK & szI));
  }
  if      (FTO  == op || SRO  == op) op1(b, op, szI, offset);
  else if (FTLL == op || SRLL == op) op2(b, op, szI, offset);
  else if (LR   == op) op2(b, LR, 0, offset);
  else if (FTGL == op || SRGL == op) {
    assert(global);
    op42(b, op, szI, (S)global, offset);
  } else if (GR   == op) {
    assert(global);
    op42(b, GR, 0, (S)global, offset);
  }
  else assert(false);
}

void opImm(Kern* k, U1 op, U1 szI, U2 offset, TyVar* g) {
  switch(op) {
    case FTGL: WS_ADD(ftSzI((U1*)g->v     + offset, szI)); return;
    case SRGL:        srSzI((U1*)g->v     + offset, szI, WS_POP()); return;
    case FTO:  WS_ADD(ftSzI((U1*)WS_POP() + offset, szI)); return;
    case SRO: {
      S v = WS_POP();
      S addr = g ? g->v : WS_POP();
      return srSzI((U1*)addr + offset, szI, v);
    }
    case GR: WS_ADD(g->v); return;
  }
  eprintf("unknown op=0x%X '%.*s'\n", op, Dat_fmt(instrName(op)));
  assert(false);
}

// Perform an offset operation.
// If 'b' is provided, then the operation is compiled. Else it is executed
// immediately.
void opOffset(Kern* k, Buf* b, U1 op, U1 szI, U2 offset, TyVar* g) {
  if(b)  opCompile(b, op, szI, offset, g);
  else   opImm(    k, op, szI, offset, g);
}

// Compile a call
void opCall(Kern* k, TyFn* fn) {
  Buf* b = &k->g.code; Buf_add(b, XL); Buf_addBE4(b, (S)fn);
}

// ***********************
//   * scan / scanTy

Ty* Kern_findTy(Kern* k, Slc t) { // You probably want to use scanTy
  Ty* ty = NULL;
  DictStk* dicts = &k->g.dictStk;
  for(U2 i = dicts->sp; i < dicts->cap; i++) {
    ty = dicts->dat[i]->children;
    I4 res = CBst_find((CBst**)&ty, t);
    if((0 == res) && (ty != NULL)) return ty;
  }
  return NULL;
}

// You probably want to use scanTy
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

// CBst* CBst_add(CBst** root, CBst* add);
void Kern_addTy(Kern* k, Ty* ty) {
  ty->bst.l = NULL; ty->bst.r = NULL;
  DictStk* dicts = &k->g.dictStk;
  ASSERT(dicts->sp < dicts->cap, "No dicts");
  Ty** root = &DictStk_top(dicts)->children;
  ty = (Ty*)CBst_add((CBst**)root, (CBst*)ty);
  if(ty) {
    eprintf("!! Overwritten key: %.*s\n", Dat_fmt(*ty->bst.key));
    SET_ERR(SLC("key was overwritten"));
  }
}

Ty* scanTy(Kern* k) {
  scan(k); Ty* ty = Kern_findToken(k);
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
  bool asImm;
  TyVar* global;
} SrOffset;
void srOffsetStruct(Kern* k, TyDict* d, U2 offset, SrOffset* st);

void srOffset(Kern* k, TyI* tyI, U2 offset, SrOffset* st) {
  if(not st->notParse) {
    if(CONSUME("{")) return srOffsetStruct(k, tyDict(tyI->ty), offset, st);
    Kern_compFn(k);
  }

  Buf* b = st->asImm ? NULL : &k->g.code; TyDb* db = tyDb(k, st->asImm);
  if(st->checkTy) tyCall(k, db, tyI, NULL);
  if(TyI_refs(tyI)) return opOffset(k, b, st->op, SZR, offset, st->global);
  ASSERT(not isTyFn(tyI->ty), "invalid fn local"); assert(isTyDict(tyI->ty));
  TyDict* d = (TyDict*) tyI->ty;
  ASSERT(not isDictMod(d), "cannot store in mod");
  if(isDictNative(d)) return opOffset(k, b, st->op, /*szI*/(S)d->children, offset, st->global);
  assert(isDictStruct(d));
  SrOffset recSt = *st; // Note: we CANNOT modify 'st', since (unlike ftOffset)
                        // it may be re-used in next field.
  recSt.checkTy = false; recSt.clear = false; recSt.notParse = true;
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
      case SRLL: op2(b, LR, 0, offset); break;
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
      tokenDrop(k); WS_ADD(/*asImm*/false); executeFn(k, (TyFn*)ty);
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
//
// For example:
// struct A [ U4 v; method aDo self: &A, x: U4 -> U4 do ( self.v + x ) ]
// struct B [ U4 b; A a; method bDo(self: &B) do ( self.a.aDo(self.b) ) ]
//
// var b: B = (A 3, 4);
//            \ + compileVar(name="b")
//            \ + ftOffset(b.tyI, offset=0)
//            \   + ftOffset(a.tyI, offset=4)
//            \   + compileMethod
// tAssertEq(6, b.a.aDo(3)

typedef struct {
  U1 op; bool noAddTy; bool findEqual; bool asImm; TyVar* global;
} FtOffset;

U1 ftOpToSr(U1 ftOp) {
  switch(ftOp) {
    case FTO: return SRO;
    case FTLL: return SRLL;
    case FTGL: return SRGL;
  }
  assert(false);
}

U1 ftOpToRef(U1 ftOp) {
  switch(ftOp) {
    case FTLL: return LR;
    case FTGL: return GR;
  }
  assert(false);
}

void ftOffsetStruct(Kern* k, TyDict* d, TyI* field, U2 offset, FtOffset* st);

// Handle type operations of refs.
// In both cases (FTO and FTLL) we just put the tyI on the stack
// For FTO, the dot compiler already reduced the number of references.
void _tyFtOffsetRefs(Kern* k, TyDb* db, TyI* tyI, FtOffset* st) {
  if(st->noAddTy) return;
  if(st->op == FTLL) return tyCall(k, db, NULL, tyI);
  assert(st->op == FTO); assert(not tyI->next);
  tyCall(k, db, NULL, tyI);
}

void compileFn(Kern* k, TyFn* fn, bool asImm) {
  tyCall(k, tyDb(k, asImm), fn->inp, fn->out);
  if(asImm) return executeFn(k, fn);
  Buf* b = &k->g.code;
  if(isFnInline(fn)) return Buf_extend(b, (Slc){fn->code, .len=fn->len});
  Buf_add(b, XL); Buf_addBE4(b, (U4)fn);
}

// Just pushes &self onto the stack and calls the method.
void compileMethod(Kern* k, TyDict* d, TyFn* meth, U2 offset, FtOffset* st) {
  Buf* b = st->asImm ? NULL : &k->g.code; TyDb* db = tyDb(k, st->asImm);
  assert(not st->noAddTy); // invariant
  if(isFnMethod(meth)) {
    TyI self = { .ty = (Ty*)d, .meta = /*refs*/1 };
    tyCall(k, db, TYI_VOID, &self);
    opOffset(k, b, ftOpToRef(st->op), 0, offset, st->global);
  }
  Kern_compFn(k);
  compileFn(k, meth, st->asImm);
}

// Fetch an offset using st->op operation.
// This defines the basic syntax for `foo.bar = baz`
void ftOffset(Kern* k, TyI* tyI, U2 offset, FtOffset* st) {
  bool addTy = not st->noAddTy;
  Buf* b = st->asImm ? NULL : &k->g.code; TyDb* db = tyDb(k, st->asImm);
  if(TyI_refs(tyI)) {
    _tyFtOffsetRefs(k, db, tyI, st);
    return opOffset(k, b, st->op, SZR, offset, st->global);
  }
  ASSERT(not isTyFn(tyI->ty), "invalid fn local");
  ASSERT(isTyDict(tyI->ty), "invalid non-dict fetch");
  TyDict* d = (TyDict*) tyI->ty; ASSERT(not isDictMod(d), "cannot fetch in mod");
  if(st->findEqual && CONSUME("=")) {
    SrOffset srSt = (SrOffset) {
      .op = ftOpToSr(st->op),
      .checkTy = true, .clear = true, .asImm = st->asImm,
      .global = st->global,
    };
    return srOffset(k, tyI, offset, &srSt);
  }
  if(isDictNative(d)) {
    if(addTy) tyCall(k, db, NULL, tyI);
    return opOffset(k, b, st->op, (S)d->children, offset, st->global);
  }
  ASSERT(isDictStruct(d), "unknown dict type");
  if(CONSUME(".")) {
    Ty* f = TyDict_scanTy(k, d);
    ASSERT(f, "member not found");
    if(isTyVar(f)) {
      TyVar* field = (TyVar*)f;
      assert(not isVarGlobal(field));
      return ftOffset(k, field->tyI, offset + field->v, st);
    } else return compileMethod(k, d, tyFn(f), offset, st);
  }
  if(addTy) tyCall(k, db, NULL, tyI);
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

void compileVar(Kern* k, TyVar* v, bool asImm) {
  TyVar* global = isVarGlobal(v) ? v : NULL;
  if(asImm) ASSERT(global, "'imm#' used with local variable");
  if(CONSUME("=")) {
    SrOffset st = (SrOffset) {
      .op = global ? SRGL : SRLL, .checkTy = true, .asImm = asImm, .global = global
    };
    srOffset(k, v->tyI, /*offset=*/global ? 0 : v->v, &st);
  } else {
    FtOffset st = (FtOffset) {
      .op = global ? FTGL : FTLL, .findEqual = true, .asImm = asImm, .global = global};
    ftOffset(k, v->tyI, /*offset=*/global ? 0 : v->v, &st);
  }
}

// Return the next member through '.', or NULL if there is no '.'
Ty* nextDot(Kern* k, TyDict* d) {
  if(not CONSUME(".")) return NULL;
  Ty* ty = TyDict_scanTy(k, d); ASSERT(ty, "member not found");
  return ty;
}

void assertRefCast(TyI* top) {
  ASSERT(RSIZE == TyI_sz(top), "Cannot convert type to pointer unless sz=RSIZE");
}

void compileDict(Kern* k, TyDict* d, bool asImm) {
  assert(not isDictMod(d));
  Kern_compFn(k); // compile next token
  if(IS_UNTY) return;
  TyDb* db = tyDb(k, asImm);
  TyI tyI = (TyI) {.ty = (Ty*) d};
  TyI* top = TyDb_top(db);
  if(isDictNative(d)) {
    if(TyI_refs(top)) assertRefCast(top);
    else {
      ASSERT((isTyDict(top->ty) && isDictNative((TyDict*)top->ty)),
           "Cannot cast non-native as native.");
    }
    TyDb_pop(k, db); tyCall(k, db, NULL, &tyI);
  } else {
    assert(isDictStruct(d));
    tyCall(k, db, d->fields, &tyI);
  }
}

void single(Kern* k, bool asImm);
void baseCompFn(Kern* k) {
  if(Kern_eof(k)) return;
  single(k, false);
}


void checkName(Kern* k, bool check) {
  if(not check) {
    eprintf("!!! name not found token: '%.*s'\n", TOKEN);
    SET_ERR(SLC("name not found"));
  }
}

void compileTy(Kern* k, Ty* ty, bool asImm) {
  eprintf("!!! compileTy: %.*s\n", Ty_fmt(ty));
  checkName(k, ty);
  do {
    if(isTyVar(ty))   return compileVar(k, (TyVar*) ty, asImm);
    if(isTyDict(ty)) {
      TyDict* d = (TyDict*) ty;
      if(isDictMod(d)) {
        ty = nextDot(k, d); ASSERT(ty, "Generics not yet implemented");
        continue;
      }
      return compileDict(k, (TyDict*) ty, asImm);
    }
  } while(0);
  ASSERT(isTyFn(ty), "Unknown type meta"); TyFn* fn = (TyFn*)ty;
  if(isFnSyn(fn)) { WS_ADD(asImm); return executeFn(k, fn); }
  Kern_compFn(k); // non-syn functions consume next token
  if(isFnImm(fn)) { ASSERT(asImm, "fn must be called with 'imm#'"); }
  compileFn(k, fn, asImm);
}

// ***********************
//   * single: compile a single token + compileSrc
void single(Kern* k, bool asImm) {
  scan(k); Slc t = *Buf_asSlc(&k->g.token);
  eprintf("!!! single: asImm=%X t=%.*s\n", asImm, Dat_fmt(t));
  if(not t.len) return;
  ParsedNumber n = parseU4(t);
  if(n.isNum) {
    tokenDrop(k);
    return compileLit(k, n.v, asImm);
  }
  Ty* ty = scanTy(k);
  checkName(k, ty);
  compileTy(k, ty, asImm);
}

void compileSrc(Kern* k) {
  ASSERT(k->g.src.d, "compileSrc: src not set");
  TyDb* db = tyDb(k, true);
  TyDb_new(db);
  while(true) {
    Kern_compFn(k);
    if(Kern_eof(k)) break;
  }
  tyCheck(NULL, TyDb_top(db), true, SLC("Type error of asImm in src"));
  TyDb_drop(k, db);
}

// ***********************
// * 6: Native Functions
// These functions are implemented as native C code and can be executed by fngi.

// ***********************
//   * Misc

// Reserve space on k->g.code. Return the previous value, which should be
// replaced when done.
Buf Kern_reserveCode(Kern* k, S sz) {
  Buf* code = &k->g.code;  Buf prevCode = *code;
  *code = Buf_new(BBA_asArena(&k->bbaCode), FN_ALLOC);
  ASSERT(code->dat, "Code OOM");
  return prevCode;
}

void N_noop(Kern* k) { WS_POP(); } // noop syn function
void N_notImm(Kern* k) { ASSERT(not WS_POP(), "cannot be executed with 'imm#'"); }
void N_unty(Kern* k) {
  WS_POP(); // ignore asImm
  k->g.fnState |= C_UNTY;
  Kern_compFn(k);
  k->g.fnState &= ~C_UNTY;
}

void _N_ret(Kern* k) { tyRet(k, tyDb(k, false), true); Buf_add(&k->g.code, RET); }
void N_ret(Kern* k)  { N_notImm(k); Kern_compFn(k); _N_ret(k); }
void N_tAssertEq(Kern* k) { WS_POP2(U4 l, U4 r); TASSERT_EQ(l, r); }
void N_assertWsEmpty(Kern* k) {
  if(not Stk_len(WS)) return;
  eprintf("! Working stack not empty: "); dbgWs(k); NL;
  SET_ERR(SLC("Working stack not empty"));
}

void N_dbgRs(Kern* k) {
  Stk* info = &cfb->info;
  Stk* rs = RS;
  U2 r = rs->sp;
  U1* ep = cfb->ep;
  for(U2 i = info->sp; i < info->cap; i++) {
    TyFn* fn = (TyFn*) info->dat[i];
    U1 lSlots = (fn == &catchTy) ? 0 : fn->lSlots;
    eprintf("! - %.*s (%u bytes in)\n", Ty_fmt(fn), ep - fn->code);
    r += lSlots;
    ep = (U1*) rs->dat[r];
    r += RSIZE;
  }
}

void N_destruct(Kern* k) {
  N_notImm(k); TyDb* db = tyDb(k, false);
  Kern_compFn(k);
  TyI* top = TyDb_top(db);
  TyDict* ty = (TyDict*) top->ty;
  ASSERT(isTyDict((Ty*)ty) && isDictStruct(ty), "destruct non-dict");
  ASSERT(not TyI_refs(top), "cannot destruct reference (yet?)");
  TyI tyI = (TyI){.ty = (Ty*)ty};
  tyCall(k, db, &tyI, ty->fields);
}

// ***********************
//   * Core syn functions

void _N_imm(Kern* k);
TyFn _TyFn_imm = TyFn_native("\x04" "_imm", TY_FN_SYN, (U1*)_N_imm, TYI_VOID, TYI_VOID);
void _N_imm(Kern* k) { if(Kern_eof(k)) return;  single(k, true); }

// Compile a token (and all sub-tokens) with asImm=true
void compImm(Kern* k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_imm;
  executeFn(k, &_TyFn_imm);
  k->g.compFn = cfn;
}

void N_imm(Kern*k) {
  N_notImm(k); REQUIRE("#"); compImm(k);
}

void N_paren(Kern* k) {
  WS_POP(); // ignore asImm
  while(true) {
    if(CONSUME(")")) break;
    ASSERT(not Kern_eof(k), "expected ')' but reached EOF");
    Kern_compFn(k);
  }
}

void _N_fslash(Kern* k);
TyFn _TyFn_fslash = TyFn_native("\x07" "_fslash", 0, (U1*)_N_fslash, TYI_VOID, TYI_VOID);
void _N_fslash(Kern* k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_fslash;
  scanRaw(k);
  // Execute next token if it is '(', else drop it.
  if(tokenEq(k, SLC("("))) single(k, false);
  else                     tokenDrop(k);
  k->g.compFn = cfn;
}

void N_fslash(Kern* k) {
  U1* c = SpReader_get(k, k->g.src, 0); ASSERT(c, "Comment: got EOF");
  if('\n' == *c) return; // immediate newline
  if(' ' == *c) { scanLine(k); return tokenDrop(k); }
  return _N_fslash(k);
}

void _fnMetaNext(Kern* k, U2 meta) {
  N_notImm(k);
  ASSERT(not (k->g.metaNext & TY_FN_TY_MASK),
         "fn can only be one main type: imm, syn, inline, comment");
  k->g.metaNext |= meta;
}

TyDict* _locGet(Kern *k) {
  TyDict* next = (TyDict*) scanTy(k);
  ASSERT(next, "name not found");
  TyDict* d;
  do {
    d = next;
    ASSERT(isTyDict((Ty*) d), "Cannot use 'use' with non-dict");
    ASSERT(not isDictNative(d), "Cannot use 'use' with native dict");
    next = (TyDict*) nextDot(k, d);
  } while(next);
  return d;
}


void _loc(Kern* k, TyDict* d) {
  assert(not isDictNative(d));
  DictStk_add(&k->g.dictStk, d);
  Kern_compFn(k);
  DictStk_pop(&k->g.dictStk);
}

void N_mod(Kern* k) {
  N_notImm(k);
  TyDict* d = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_MOD, NULL);
  _loc(k, d);
}

void N_loc(Kern* k) {
  N_notImm(k); REQUIRE(":");
  _loc(k, _locGet(k));
}

void N_fileloc(Kern* k) { // loc that stays for whole file
  N_notImm(k); REQUIRE(":");
  TyDict* d = _locGet(k);
  DictStk_add(&k->g.dictStk, d);
}

// ***********************
//   * fn and fn signature

// fnTy#IMM to make next function immediate, etc.
void N_fnTy(Kern* k)   {
  N_notImm(k); REQUIRE("#");
  compImm(k); tyCall(k, tyDb(k, true), &TyIs_S, NULL);
  S meta = WS_POP();
  ASSERT(not (TY_FN_TY_MASK & k->g.metaNext),
         "fn can only be one main type: imm, syn, inline, comment");
  k->g.metaNext |= (TY_FN_TY_MASK & meta);
}

#define SET_FN_STATE(STATE)  k->g.fnState = bitSet(k->g.fnState, STATE, C_FN_STATE)
#define IS_FN_STATE(STATE)   ((C_FN_STATE & k->g.fnState) == (STATE))

void synFnFound(Kern* k, Ty* ty) {
  ASSERT(isTyFn(ty) and isFnSyn((TyFn*)ty),
         "expect only syn functions in function signature");
  executeFn(k, (TyFn*)ty);
}

void localImpl(Kern* k, TyVar* var) {
  S sz = TyI_sz(var->tyI);
  var->v = align(k->g.fnLocals, alignment(sz));
  k->g.fnLocals = var->v + sz;
}

// Used for both stk and out
void N_stk(Kern *k) {
  N_notImm(k);
  Sll** root;
  if(     IS_FN_STATE(FN_STATE_STK)) root = TyFn_inpRoot(tyFn(k->g.curTy));
  else if(IS_FN_STATE(FN_STATE_OUT)) root = TyFn_outRoot(tyFn(k->g.curTy));
  else { SET_ERR(SLC("stk used after local variable inputs or in body")); }
  CONSUME(":");
  Sll_add(root, TyI_asSll(scanTyI(k)));
}
TyFn TyFn_stk = TyFn_native("\x03" "stk", TY_FN_SYN, (U1*)N_stk, TYI_VOID, TYI_VOID);

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
  N_notImm(k);
  ASSERT(IS_FN_STATE(FN_STATE_STK) or IS_FN_STATE(FN_STATE_INP)
         , "inp used after out");
  TyVar* var = varPre(k);
  SET_FN_STATE(FN_STATE_INP);
  TyI* tyI = TyI_cloneNode(var->tyI, k->g.bbaDict);
  Sll_add(TyFn_inpRoot(tyFn(k->g.curTy)), TyI_asSll(tyI));
  localImpl(k, var);
}
TyFn TyFn_inp = TyFn_native("\x03" "inp", TY_FN_SYN, (U1*)N_inp, TYI_VOID, TYI_VOID);

void _varGlobal(Kern* k, TyVar* v) {
  v->meta |= TY_VAR_GLOBAL;
  S sz = TyI_sz(v->tyI);
  Buf prevCode = Kern_reserveCode(k, sz);
  v->v = (S) k->g.code.dat;
  k->g.code.len = sz; // mark as totally used.
  if(CONSUME("=")) {
    compImm(k);
    SrOffset st = (SrOffset) {.op = SRGL, .checkTy = true, .asImm = true, .global = v};
    srOffset(k, v->tyI, /*offset*/0, &st);
    v->meta |= TY_VAR_INIT;
  }
  k->g.code = prevCode;
}

void _varLocal(Kern* k, TyVar* v) {
  localImpl(k, v);
  if(CONSUME("=")) {
    SrOffset st = (SrOffset) {.op = SRLL, .checkTy = true };
    srOffset(k, v->tyI, /*offset=*/v->v, &st);
  }
}

void N_var(Kern* k) {
  N_notImm(k);
  TyVar* v = varPre(k);
  if(IS_FN_STATE(FN_STATE_NO))  _varGlobal(k, v);
  else                          _varLocal(k, v);
}

void fnSignature(Kern* k, TyFn* fn) {
  SET_FN_STATE(FN_STATE_STK);
  while(true) {
    if(CONSUME("do")) break;
    if(CONSUME("->")) SET_FN_STATE(FN_STATE_OUT);
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "expected 'do' but reached EOF");
    WS_ADD(/*asImm=*/ false); // all of these are syn functions
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)
       and not Slc_eq(SLC("&"), CStr_asSlc(ty->bst.key))) {
      tokenDrop(k); executeFn(k, (TyFn*)ty);
    } else if (IS_FN_STATE(FN_STATE_OUT)) N_stk(k);
    else                                  N_inp(k);
  }
}

// Walk input and store locals and push stack types
void fnInputs(Kern* k, TyFn* fn) {
  TyDb* db = tyDb(k, false);
  for(TyI* tyI = fn->inp; tyI; tyI = tyI->next) {
    TyVar* v = (TyVar*)Kern_findTy(k, CStr_asSlcMaybe(tyI->name));
    if(v and isTyVar((Ty*)v) and not isVarGlobal(v)) {
      SrOffset st = (SrOffset) { .op = SRLL, .checkTy = false, .notParse = true };
      srOffset(k, tyI, v->v, &st);
    }
    else if(/*isStk and*/ not IS_UNTY) {
      TyI_cloneAddNode(db->bba, TyDb_root(db), tyI);
    }
  }
}

// fn NAME do (... code ...)
// future:
// fn ... types ... do ( ... code ... )
void N_fn(Kern* k) {
  N_notImm(k);
  U2 meta = k->g.metaNext;
  // Create TyFn based on NAME
  scan(k); TyFn* fn = (TyFn*) Ty_new(k, TY_FN | meta, NULL);
  tokenDrop(k);
  Ty* prevTy = k->g.curTy; k->g.curTy = (Ty*) fn;

  Buf* code = &k->g.code;  Buf prevCode = Kern_reserveCode(k, FN_ALLOC);

  const U2 db_startLen = Stk_len(&k->g.tyDb.done);
  TyDb_new(&k->g.tyDb);
  LOCAL_TYDB_BBA(tyDb); TyDb* db = tyDb(k, false);

  DictStk_add(&k->g.dictStk, (TyDict*) fn); // local variables

  fnSignature(k, fn); fnInputs(k, fn);
  SET_FN_STATE(FN_STATE_BODY); Kern_compFn(k); // compile the fn body
  SET_FN_STATE(FN_STATE_NO);

  // Force a RET at the end, whether UNTY or not.
  if( (not IS_UNTY and not TyDb_done(db))
      or  (IS_UNTY and (RET != code->dat[code->len-1]))) _N_ret(k);

  // Free unused area of buffers
  ASSERT(not BBA_free(&k->bbaCode, code->dat + code->len, code->cap - code->len, 1),
         "N_fn free");

  fn->code = code->dat; fn->len = code->len;
  fn->lSlots = align(k->g.fnLocals, RSIZE) / RSIZE;
  k->g.fnLocals = 0; k->g.metaNext = 0;
  *code = prevCode;

  TyDb_drop(k, db);
  ASSERT(db_startLen == Stk_len(&k->g.tyDb.done),
         "A type operation (i.e. if/while/etc) is incomplete in fn");
  END_LOCAL_TYDB_BBA(tyDb);
  DictStk_pop(&k->g.dictStk);
  k->g.curTy = prevTy;
}


void N_meth(Kern* k) {
  TyDict* mod = k->g.curMod;
  ASSERT(isTyDict((Ty*)mod) && isDictStruct(mod), "'meth' can only be in struct");
  k->g.metaNext |= TY_FN_METHOD;
  N_fn(k);
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
  Buf_add(b, SZ2 | JLZ); Buf_addBE2(b, 0);
  return b->len - 2;
}

void _endIf(Buf* b, U2 i) { // flow control only
  srBE2(&b->dat[i], b->len - i);
}

U2 _else(Buf* b, U2 iIf) { // flow control only
  Buf_add(b, SZ2 | JL); // end of if has unconditional jmp to end of else
  Buf_addBE2(b, 0);
  _endIf(b, iIf); // if jmps into else block
  return b->len - 2;
}

#define DBG_TYS(...) \
    if(not IS_UNTY) { eprintf(__VA_ARGS__); dbgTyIs(k); NL; }

IfState tyIf(Kern* k, IfState is) {
  TyDb* db = tyDb(k, false);
  if(IS_UNTY) return is;
  if(is.hadFull) {
    tyMerge(k, db);    // check stateToUse==fullIfState
    tyClone(k, db, 1); // clone outer state
  } else if (TyDb_done(db)) {
    tyMerge(k, db);    // drop stateToUse
    tyClone(k, db, 0); // clone outer state
  } else {
    tyClone(k, db, 1); // stateToUse becomes fullIfState. Clone outer as stateToUse
    is.hadFull = true;
  }
  return is;
}

IfState _N_if(Kern* k, IfState is) {
  TyDb* db = tyDb(k, false);
  Buf* b = &k->g.code;
  U2 i = _if(b);
  REQUIRE("do"); Kern_compFn(k);
  is = tyIf(k, is);

  if(CONSUME("elif")) {
    i = _else(b, i);
    Kern_compFn(k); tyCall(k, db, &TyIs_S, NULL);
    ASSERT(IS_UNTY or not TyDb_done(db), "Detected done in elif test");
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
  TyDb* db = tyDb(k, false);
  TyDb_drop(k, db); // stateToUse
  if(is.hadFull)       TyDb_nip(k, db);
  else if (is.hadElse) TyDb_setDone(db, true);
}

void N_if(Kern* k) {
  N_notImm(k); TyDb* db = tyDb(k, false);
  Kern_compFn(k); tyCall(k, db, &TyIs_S, NULL);
  ASSERT(IS_UNTY or not TyDb_done(db), "Detected done in if test");
  tyClone(k, db, 0);
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

void tyCont(Kern* k, TyDb* db) {
  if(IS_UNTY) return;
  Blk* blk = k->g.blk;
  ASSERT(not TyDb_done(db), "Cont after guaranteed 'ret'");
  tyCheck(blk->startTyI, TyDb_top(db), /*sameLen*/true,
          SLC("Type error: cont not identical as block start."));
  TyDb_setDone(db, /*done*/true);
}

void N_cont(Kern* k) {
  N_notImm(k);
  tyCont(k, tyDb(k, false));
  Buf* b = &k->g.code;
  Buf_add(b, SZ2 | JL);
  Buf_addBE2(b, k->g.blk->start - b->len);
}

void tyBreak(Kern* k, TyDb* db) {
  if(IS_UNTY) return;
  Blk* blk = k->g.blk;
  ASSERT(not TyDb_done(db), "Break after guaranteed 'ret'");
  if(blk->endTyI) {
    tyCheck(blk->endTyI, TyDb_top(db), /*sameLen*/true,
            SLC("Type error: breaks not identical type."));
  } else {
    TyI_cloneAdd(k->g.bbaDict, &k->g.blk->endTyI, TyDb_top(db));
  }
  TyDb_setDone(db, /*done*/true);
}

void N_brk(Kern* k) {
  N_notImm(k); Kern_compFn(k);
  tyBreak(k, tyDb(k, false));
  Buf* b = &k->g.code;

  Buf_add(b, SZ2 | JL); // unconditional jump to end of block
  Sll* br = BBA_alloc(k->g.bbaDict, sizeof(Sll), RSIZE); ASSERT(br, "brk OOM");
  Sll_add(&k->g.blk->breaks, br);  br->dat = b->len;
  Buf_addBE2(b, 0);
}

void N_blk(Kern* k) {
  N_notImm(k); TyDb* db = tyDb(k, false);
  Buf* b = &k->g.code;
  Blk* blk = BBA_alloc(k->g.bbaDict, sizeof(Blk), RSIZE);
  ASSERT(blk, "block OOM");
  *blk = (Blk) { .start = b->len };
  TyI_cloneAdd(k->g.bbaDict, &blk->startTyI, TyDb_top(db));
  Sll_add(Blk_root(k), Blk_asSll(blk));

  Kern_compFn(k); // compile code block
  if(not TyDb_done(db)) tyBreak(k, db); // implicit break checks blk type
  for(Sll* br = blk->breaks; br; br = br->next) {
    srBE2(b->dat + br->dat, b->len - br->dat);
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
  N_notImm(k);
  TyDict* st = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_STRUCT, NULL);
  TyDict* prevMod = k->g.curMod;
  k->g.curMod = st;
  DictStk_add(&k->g.dictStk, st);
  REQUIRE("[");
  while(not CONSUME("]")) {
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "Expected ']', reached EOF");
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)) {
      tokenDrop(k); WS_ADD(/*asImm*/false); executeFn(k, (TyFn*)ty);
    } else field(k, st);
  }
  DictStk_pop(&k->g.dictStk);
  k->g.curMod = prevMod;
}

// ***********************
//   * '.', '&', '@'

void N_dot(Kern* k) { // A reference is on the stack, get a (sub)field
  N_notImm(k); TyDb* db = tyDb(k, false);
  ASSERT(not IS_UNTY, "Cannot use '.' on stack references without type checking");
  TyI* top = TyDb_top(db);
  U2 refs = TyI_refs(top);
  ASSERT(refs, "invalid '.', the value on the stack is not a reference");
  TyDict* d = (TyDict*) top->ty; assert(d);
  ASSERT(isTyDict((Ty*)d), "invalid '.', the value on the stack is not a TyDict");
  ASSERT(not isDictNative(d), "invalid '.' on native type");
  ASSERT(not isDictMod(d), "accessing mod through ref");

  Buf* b = &k->g.code;
  // Trim the refs to be only 1
  while(refs > 1) { Buf_add(b, FT | SZR); }
  TyDb_pop(k, db);
  Ty* ty = TyDict_scanTy(k, d); ASSERT(ty, "member not found");
  if(isTyVar(ty)) {
    TyVar* var = (TyVar*) ty;
    ASSERT(not isVarGlobal(var), "invalid '.', accessing constant through ref");
    FtOffset st = (FtOffset) {.op = FTO, .findEqual = true};
    ftOffset(k, var->tyI, var->v, &st);
    return;
  }
  // Call the method
  TyI this = (TyI) { .ty = (Ty*)d, .meta = /*refs*/1 };
  tyCall(k, db, NULL, &this); // put back on ty stack (from TyDb_pop above)
  compileTy(k, ty, false);
}

// & on a nested reference type (i.e. a struct field)
void ampRef(Kern* k, TyI* tyI) {
  ASSERT(PEEK("."), "invalid &, non-variable/struct");
  Buf* b = &k->g.code;
  U2 offset = 0; U2 refs = TyI_refs(tyI);
  TyDb* db = tyDb(k, false);
  while(true) {
    if(CONSUME(".")) {
      while(refs > 1) {
        opOffset(k, b, FTO, SZR, offset, NULL); offset = 0; refs -= 1;
      }
      TyVar* var = tyVar(TyDict_find(tyDict(var->tyI->ty), tokenSlc(k)));
      assert(not isVarGlobal(var));
      tyI = var->tyI; offset += var->v;
    } else {
      ASSERT(TyI_refs(tyI) + 1 <= TY_REFS, "refs too large");
      if(offset) { lit(b, offset); Buf_add(b, ADD); }
      TyDb_drop(k, db);
      TyI out = (TyI) { .ty = tyI->ty, .meta = tyI->meta + 1 };
      return tyCall(k, db, NULL, &out);
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
  N_notImm(k); TyDb* db = tyDb(k, false);
  scan(k); Ty* ty = Kern_findToken(k); ASSERT(ty, "not found");
  if(isTyVar(ty)) { }
  else if (isTyDict(ty)) {
    Kern_compFn(k);
    assertRefCast(TyDb_top(db));
    TyI to = { .ty = (ty), .meta = 1 };
    TyDb_pop(k, db); tyCall(k, db, NULL, &to);
    return;
  } else SET_ERR(SLC("'&' can only get ref of variable or typecast"));
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
    assert(TyI_refs(tyI));
    op2(b, FTLL, SZR, offset);
    return ampRef(k, tyI);
  }
  ASSERT(TyI_refs(tyI) + 1 <= TY_REFS, "refs too large");
  TyI out = (TyI) { .ty = tyI->ty, .meta = tyI->meta + 1 };
  tyCall(k, tyDb(k, false), NULL, &out);
  op2(b, LR, 0, offset);
}

void N_at(Kern* k) {
  N_notImm(k); TyDb* db = tyDb(k, false);
  ASSERT(not IS_UNTY, "Cannot use '@' without type checking");
  Kern_compFn(k);
  if(CONSUME("=")) {
    assert(false);
  }
  TyI* top = TyDb_top(db); U2 refs = TyI_refs(top);
  ASSERT(refs, "invalid '@', the value on the stack is not a reference");
  if(refs > 1) Buf_add(&k->g.code, FT | SZR);
  else if (not isTyDict(top->ty)) { SET_ERR(SLC("Cannot fetch non-dict type")); }
  else {
    TyDict* d = (TyDict*) top->ty;
    ASSERT(not isDictMod(d), "Cannot @mod");
    if(isDictNative(d)) Buf_add(&k->g.code, FT | (SZ_MASK & (S)d->children));
    else assert(false);
  }

  TyI inp = (TyI) { .ty = (Ty*)top->ty, .meta = top->meta };
  TyI out = (TyI) { .ty = (Ty*)top->ty, .meta = top->meta - 1};
  tyCall(k, tyDb(k, false), &inp, &out);
}

void N_ptrAddRaw(Kern* k) {
  S sz = WS_POP(); WS_POP3(S ptr, S i, S bound);
  ASSERT(i < bound, "ptrAdd OOB");
  WS_ADD(ptr + (i * sz));
}
TyFn TyFn_ptrAddRaw = {
  .bst.key = (CStr*) ("\x09" "ptrAddRaw"), .meta = TY_FN | TY_FN_NATIVE,
  .code = (U1*)N_ptrAddRaw, .inp = &TyIs_Unsafe, .out = &TyIs_Unsafe
};

void N_ptrAdd(Kern* k) {
  bool asImm = WS_POP(); TyDb* db = tyDb(k, asImm);
  ASSERT(not IS_UNTY, "ptrAdd requires type checking");
  Kern_compFn(k);
  TyI* tyI = TyDb_top(db);
  if(not tyI || not tyI->next || not tyI->next->next) {
    // invalid type
    tyCall(k, db, &TyIs_rAnySS, &TyIs_rAny); assert(false);
  }
  TyI* ptrTy = tyI->next->next;
  ASSERT(TyI_refs(ptrTy), "ptrAdd requires the ptr to be a reference");
  TyI inp[3]; inp[0] = (TyI) {.ty = ptrTy->ty, .meta = ptrTy->meta };
              inp[1] = (TyI) {.ty = (Ty*)&Ty_S, .next = &inp[0]};
              inp[2] = (TyI) {.ty = (Ty*)&Ty_S, .next = &inp[1]};
  tyCall(k, db, &inp[2], &inp[0]);
  TyI derefTyI = {.ty = ptrTy->ty, .meta = ptrTy->meta - 1};
  lit(&k->g.code, TyI_sz(&derefTyI));
  Kern_typed(k, false);
  compileFn(k, &TyFn_ptrAddRaw, asImm);
  Kern_typed(k, true);
}

// ***********************
//   * Export to fngi

// setFnTy myFn: U4 -> U2 do;
void N_setFnTy(Kern* k) {
  N_notImm(k);
  TyFn* fn = tyFn(scanTy(k)); CONSUME(":");
  ASSERT(fn->inp == &TyIs_UNSET && fn->out == &TyIs_UNSET, "fn types are not unset");
  fnSignature(k, fn);
}

void N_single(Kern* k) { single(k, WS_POP()); }
void N_compileTy(Kern* k) {
  WS_POP2(S ty, bool asImm); compileTy(k, (Ty*)ty, asImm);
}
void N_compileLit(Kern* k) {
  WS_POP2(U4 v, bool asImm); compileLit(k, v, asImm);
}
void N_findTy(Kern* k) {
  WS_POP2(S dat, S len);
  WS_ADD((S)Kern_findTy(k, (Slc){.dat = (U1*)dat, .len = len}));
}

// ***********************
// * 7: Registering Functions

#define ADD_ANY_VAR(VAR, TY, NAMELEN, NAME, META, VNAME, V) \
  CStr_ntVar(LINED(key), NAMELEN, NAME);\
  VAR = (TY) {          \
    .bst.key = LINED(key),              \
    .meta =  META,                      \
    .VNAME = V,                         \
  };                                    \
  Kern_addTy(k, (Ty*)&VAR);

#define ADD_ANY_CREATE(TY, NAMELEN, NAME, META, VNAME, V) \
  static TY LINED(ty);                  \
  ADD_ANY_VAR(LINED(ty), TY, NAMELEN, NAME, META, VNAME, V)

#define ADD_TY_NATIVE(VAR, NAMELEN, NAME, META, VAL) \
  ADD_ANY_VAR(VAR, TyDict, NAMELEN, NAME, TY_DICT | TY_DICT_NATIVE | META, children, VAL)

#define ADD_FN(NAMELEN, NAME, META, CODE, INP, OUT) \
  ADD_ANY_CREATE(TyFn, NAMELEN, NAME, TY_FN | TY_FN_NATIVE | (META), code, (U1*)CODE); \
  LINED(ty).inp = INP; LINED(ty).out = OUT;

#define ADD_INLINE_FN(NAMELEN, NAME, META, INP, OUT, ...)     \
  assert(sizeof((U1[]){__VA_ARGS__}) < 0xFF);                 \
  static U1 LINED(code)[] = {__VA_ARGS__ __VA_OPT__(,) RET};  \
  ADD_FN(NAMELEN, NAME, TY_FN_INLINE | (META), (S)LINED(code), INP, OUT); \
  LINED(ty).len = sizeof(LINED(code)) - 1;

void Kern_fns(Kern* k) {
  // Native data types
  ADD_TY_NATIVE(Ty_UNSET, "\x08", "Ty_UNSET",  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_Any,   "\x03", "Any"     ,  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_Unsafe,"\x06", "Unsafe"  ,  0      , (Ty*)(SZR + 1));

  TyIs_UNSET  = (TyI) { .ty = (Ty*)&Ty_UNSET  };
  TyIs_Unsafe = (TyI) { .ty = (Ty*)&Ty_Unsafe };
  TyIs_rAny   = (TyI) { .ty = (Ty*)&Ty_Any, .meta = 1 };
  TyIs_rAnyS  = (TyI) { .ty = (Ty*)&Ty_S, .next = &TyIs_rAny };
  TyIs_rAnySS = (TyI) { .ty = (Ty*)&Ty_S, .next = &TyIs_rAnyS };

  ADD_TY_NATIVE(Ty_U1, "\x02", "U1",  0               , (Ty*)SZ1);
  ADD_TY_NATIVE(Ty_U2, "\x02", "U2",  0               , (Ty*)SZ2);
  ADD_TY_NATIVE(Ty_U4, "\x02", "U4",  0               , (Ty*)SZ4);
  ADD_TY_NATIVE(Ty_S , "\x01", "S",   0               , (Ty*)SZR);
  ADD_TY_NATIVE(Ty_I1, "\x02", "I1",  TY_NATIVE_SIGNED, (Ty*)SZ1);
  ADD_TY_NATIVE(Ty_I2, "\x02", "I2",  TY_NATIVE_SIGNED, (Ty*)SZ2);
  ADD_TY_NATIVE(Ty_I4, "\x02", "I4",  TY_NATIVE_SIGNED, (Ty*)SZ4);
  ADD_TY_NATIVE(Ty_SI, "\x02", "SI",  TY_NATIVE_SIGNED, (Ty*)SZR);
  TASSERT_EMPTY();

  // Ty: S
  TyIs_S = (TyI) { .ty = (Ty*)&Ty_S };
  // Ty: S, s
  TyIs_SS = (TyI) { .next = &TyIs_S, .ty = (Ty*)&Ty_S };
  // Ty: S, s, S
  TyIs_SSS = (TyI) { .next = &TyIs_SS, .ty = (Ty*)&Ty_S };

  TyIs_U1 = (TyI) { .ty = (Ty*)&Ty_U1 };
  TyIs_U2 = (TyI) { .ty = (Ty*)&Ty_U2 };
  TyIs_U4 = (TyI) { .ty = (Ty*)&Ty_U4 };
  TyIs_U4x2 = (TyI) { .next = &TyIs_U4, .ty = (Ty*)&Ty_U4 };

  TyIs_rU1 = (TyI) { .ty = (Ty*)&Ty_U1, .meta = 1 };
  TyIs_rU2 = (TyI) { .ty = (Ty*)&Ty_U2, .meta = 1 };
  TyIs_rU4 = (TyI) { .ty = (Ty*)&Ty_U4, .meta = 1 };

  TyIs_rU1_U4 = (TyI) {.ty = (Ty*)&Ty_U4, .next = &TyIs_rU4};

  Kern_addTy(k, (Ty*) &TyFn_baseCompFn);
  Kern_addTy(k, (Ty*) &TyFn_stk);
  Kern_addTy(k, (Ty*) &TyFn_inp);
  Kern_addTy(k, (Ty*) &TyFn_memclr);

  ADD_FN("\x01", "\\"           , TY_FN_COMMENT   , N_fslash   , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "_"            , TY_FN_SYN       , N_noop     , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", ";"            , TY_FN_SYN       , N_noop     , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", ","            , TY_FN_SYN       , N_noop     , TYI_VOID, TYI_VOID);
  ADD_FN("\x02", "->"           , TY_FN_SYN       , N_noop     , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "notImm"       , TY_FN_SYN       , N_notImm   , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "unty"         , TY_FN_SYN       , N_unty     , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "ret"          , TY_FN_SYN       , N_ret      , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "imm"          , TY_FN_SYN       , N_imm      , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "("            , TY_FN_SYN       , N_paren    , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "mod"          , TY_FN_SYN       , N_mod      , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "loc"          , TY_FN_SYN       , N_loc      , TYI_VOID, TYI_VOID);
  ADD_FN("\x07", "fileloc"      , TY_FN_SYN       , N_fileloc  , TYI_VOID, TYI_VOID);
  ADD_FN("\x02", "fn"           , TY_FN_SYN       , N_fn       , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "meth"         , TY_FN_SYN       , N_meth     , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "fnTy"         , TY_FN_SYN       , N_fnTy     , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "var"          , TY_FN_SYN       , N_var      , TYI_VOID, TYI_VOID);
  ADD_FN("\x02", "if"           , TY_FN_SYN       , N_if       , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "cont"         , TY_FN_SYN       , N_cont     , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "brk"          , TY_FN_SYN       , N_brk      , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "blk"          , TY_FN_SYN       , N_blk      , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "struct"       , TY_FN_SYN       , N_struct   , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "."            , TY_FN_SYN       , N_dot      , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "&"            , TY_FN_SYN       , N_amp      , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "@"            , TY_FN_SYN       , N_at       , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "ptrAdd"       , TY_FN_SYN       , N_ptrAdd   , TYI_VOID, TYI_VOID);
  ADD_FN("\x08", "destruct"     , TY_FN_SYN       , N_destruct , TYI_VOID, TYI_VOID);
  ADD_FN("\x05", "dbgRs"        , 0               , N_dbgRs    , TYI_VOID, TYI_VOID);
  ADD_FN("\x09", "tAssertEq"    , 0               , N_tAssertEq, &TyIs_SS, TYI_VOID);
  ADD_FN("\x0D", "assertWsEmpty", 0               , N_assertWsEmpty, TYI_VOID, TYI_VOID);
  // FIXME: getting weird error
  ADD_FN("\x07", "setFnTy"      , TY_FN_SYN       , N_setFnTy  , TYI_VOID, TYI_VOID);

  // Stack operators. These are **not** PRE since they directly modify the stack.
  ADD_INLINE_FN("\x03", "swp"  , 0       , &TyIs_SS, &TyIs_SS,   SWP   );
  ADD_INLINE_FN("\x03", "drp"  , 0       , &TyIs_S,  TYI_VOID,   DRP   );
  ADD_INLINE_FN("\x03", "ovr"  , 0       , &TyIs_SS, &TyIs_SSS,  OVR   );
  ADD_INLINE_FN("\x03", "dup"  , 0       , &TyIs_S,  &TyIs_SS,   DUP   );
  ADD_INLINE_FN("\x04", "dupn" , 0       , &TyIs_S,  &TyIs_SS,   DUPN  );

  // Standard operators that use PRE syntax. Either "a <op> b" or simply "<op> b"
  ADD_INLINE_FN("\x03", "nop"  , 0       , &TyIs_S,  &TyIs_S, NOP     );
  ADD_INLINE_FN("\x03", "inc"  , 0       , &TyIs_S,  &TyIs_S, INC     );
  ADD_INLINE_FN("\x04", "inc2" , 0       , &TyIs_S,  &TyIs_S, INC2    );
  ADD_INLINE_FN("\x04", "inc4" , 0       , &TyIs_S,  &TyIs_S, INC4    );
  ADD_INLINE_FN("\x03", "dec"  , 0       , &TyIs_S,  &TyIs_S, DEC     );
  ADD_INLINE_FN("\x03", "inv"  , 0       , &TyIs_S,  &TyIs_S, INV     );
  ADD_INLINE_FN("\x03", "neg"  , 0       , &TyIs_S,  &TyIs_S, NEG     );
  ADD_INLINE_FN("\x03", "not"  , 0       , &TyIs_S,  &TyIs_S, NOT     );
  ADD_INLINE_FN("\x05", "i1to4", 0       , &TyIs_S,  &TyIs_S, CI1     );
  ADD_INLINE_FN("\x05", "i2to4", 0       , &TyIs_S,  &TyIs_S, CI2     );
  ADD_INLINE_FN("\x01", "+"    , 0       , &TyIs_SS, &TyIs_S, ADD     );
  ADD_INLINE_FN("\x01", "-"    , 0       , &TyIs_SS, &TyIs_S, SUB     );
  ADD_INLINE_FN("\x01", "%"    , 0       , &TyIs_SS, &TyIs_S, MOD     );
  ADD_INLINE_FN("\x03", "shl"  , 0       , &TyIs_SS, &TyIs_S, SHL     );
  ADD_INLINE_FN("\x03", "shr"  , 0       , &TyIs_SS, &TyIs_S, SHR     );
  ADD_INLINE_FN("\x03", "msk"  , 0       , &TyIs_SS, &TyIs_S, MSK     );
  ADD_INLINE_FN("\x02", "jn"   , 0       , &TyIs_SS, &TyIs_S, JN      );
  ADD_INLINE_FN("\x03", "xor"  , 0       , &TyIs_SS, &TyIs_S, XOR     );
  ADD_INLINE_FN("\x03", "and"  , 0       , &TyIs_SS, &TyIs_S, AND     );
  ADD_INLINE_FN("\x02", "or"   , 0       , &TyIs_SS, &TyIs_S, OR      );
  ADD_INLINE_FN("\x02", "=="   , 0       , &TyIs_SS, &TyIs_S, EQ      );
  ADD_INLINE_FN("\x02", "!="   , 0       , &TyIs_SS, &TyIs_S, NEQ     );
  ADD_INLINE_FN("\x02", ">="   , 0       , &TyIs_SS, &TyIs_S, GE_U    );
  ADD_INLINE_FN("\x01", "<"    , 0       , &TyIs_SS, &TyIs_S, LT_U    );
  ADD_INLINE_FN("\x04", "ge_s" , 0       , &TyIs_SS, &TyIs_S, GE_S    );
  ADD_INLINE_FN("\x04", "lt_s" , 0       , &TyIs_SS, &TyIs_S, LT_S    );
  ADD_INLINE_FN("\x01", "*"    , 0       , &TyIs_SS, &TyIs_S, MUL     );
  ADD_INLINE_FN("\x01", "/"    , 0       , &TyIs_SS, &TyIs_S, DIV_U   );

  ADD_INLINE_FN("\x03", "ft1"  , 0       , &TyIs_S, &TyIs_S , SZ1+FT  );
  ADD_INLINE_FN("\x03", "ft2"  , 0       , &TyIs_S, &TyIs_S , SZ2+FT  );
  ADD_INLINE_FN("\x03", "ft4"  , 0       , &TyIs_S, &TyIs_S , SZ4+FT  );
  ADD_INLINE_FN("\x03", "ftR"  , 0       , &TyIs_S, &TyIs_S , SZR+FT  );
  ADD_INLINE_FN("\x05", "ftBe1", 0       , &TyIs_S, &TyIs_S , SZ1+FTBE);
  ADD_INLINE_FN("\x05", "ftBe2", 0       , &TyIs_S, &TyIs_S , SZ2+FTBE);
  ADD_INLINE_FN("\x05", "ftBe4", 0       , &TyIs_S, &TyIs_S , SZ4+FTBE);
  ADD_INLINE_FN("\x05", "ftBeR", 0       , &TyIs_S, &TyIs_S , SZR+FTBE);

  static TyDict comp; // these are all inside comp
  ADD_ANY_VAR(comp, TyDict, "\x04", "comp", TY_DICT | TY_DICT_MOD, children, /*v=*/0);
  DictStk_add(&k->g.dictStk, &comp);
  ADD_FN("\x06", "single"     , 0   , N_single     , &TyIs_S, TYI_VOID);
  ADD_FN("\x0A", "compileLit" , 0   , N_compileLit , &TyIs_SS, TYI_VOID);
  ADD_FN("\x09", "compileTy"  , 0   , N_compileTy  , &TyIs_UNSET, &TyIs_UNSET);
  ADD_FN("\x06", "findTy"     , 0   , N_findTy     , &TyIs_UNSET, &TyIs_UNSET);
  DictStk_pop(&k->g.dictStk);
  // assert(&comp.v == (S)DictStk_pop(&k->g.dictStk).root);

  TASSERT_EMPTY();
}

// ***********************
// * 8: Execution helpers

void Kern_handleSig(Kern* k, int sig, struct sigcontext* ctx) {
  eprintf("!!! fngi return stack:\n");
  N_dbgRs(k);
  eprintf("!!! Code: token=\"%.*s\" line=%u\n", Dat_fmt(k->g.token), k->g.tokenLine);
  if(sig || k->isTest) {
    Trace_handleSig(sig, ctx);
  }
  exit(sig);
}

void fngiHandleSig(int sig, struct sigcontext ctx) {
  Kern_handleSig(fngiK, sig, &ctx); exit(sig);
}

void fngiErrPrinter() { Kern_handleSig(fngiK, 0, NULL); }

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

void compilePath(Kern* k, CStr* path) {
  FileInfo srcInfo = (FileInfo) {.path = path };
  k->g.srcInfo = &srcInfo;
  Ring_var(_r, 256); UFile f = UFile_new(_r);
  UFile_open(&f, CStr_asSlc(path), File_RDONLY);
  N_assertWsEmpty(k);
  TASSERT_EQ(File_DONE, f.code);
  assert(f.fid);
  k->g.src = (SpReader) {.m = &mSpReader_UFile, .d = &f };
  compileSrc(k);
  DictStk_reset(k);
}

void simpleRepl(Kern* k) {
  REPL_START;  TyDb* db = tyDb(k, false);
  size_t cap;  jmp_buf local_errJmp;  jmp_buf* prev_errJmp = civ.fb->errJmp;
  Ring_var(_r, 256);  BufFile f = BufFile_init(_r, (Buf){0});
  k->g.src = (SpReader) {.m = &mSpReader_BufFile, .d = &f };

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
    eprintf(" :"); TyI_printAll(TyDb_top(db)); NL;
  }
  civ.fb->errJmp = prev_errJmp;
  REPL_END
}
