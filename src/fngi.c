
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

Key SUPER;

TyFn _TyFn_imm;

/*extern*/ Kern* fngiK = NULL;

// ***********************
// * 0: Base data structurescnmb   

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

Slc Slc_frCStrMaybe(CStr* c) { return c ? Slc_frCStr(c) : (Slc){}; }
I4 TyI_cmp(TyI* a, TyI* b) {
  while(true) {
    if(a->meta != b->meta)     return S_cmp(a->meta, b->meta);
    if(a->arrLen != b->arrLen) return S_cmp(a->arrLen, b->arrLen);
    if(a->ty != b->ty)         return S_cmp((S)a->ty, (S)b->ty);
    I4 c = Slc_cmp(Slc_frCStrMaybe(a->name), Slc_frCStrMaybe(b->name));
    if(c) return c;
    if(not a->next) return b->next ? -1 : 0;
    if(not b->next) return 1;
    a = a->next; b = b->next;
  }
}

I4 Ty_cmp(Ty* node, Key* key) {
  I4 c = Slc_cmp(Slc_frCStrMaybe(node->name), key->name);
  if(c)                        return c;
  if(node->tyKey and key->tyI) return TyI_cmp(node->tyKey, key->tyI);
  if(node->tyKey or  key->tyI) return S_cmp((S)node->tyKey, (S)key->tyI);
  else                         return 0;
}

I4 Ty_find(Ty** node, Key* key) {
  return Bst_find((Bst**)node, key, (BstCmp)&Ty_cmp);
}

Ty* Ty_add(Ty** root, Ty* add) {
  Slc name = Slc_frCStrMaybe(add->name);
  Key key = { .name = name, .tyI = add->tyKey };
  return (Ty*) Bst_add((Bst**)root, (Bst*)add, &key, (BstCmp)&Ty_cmp);
}

Slc popSlc(Kern* k) {
  WS_POP2(S dat, S len); return (Slc) { .dat = (U1*)dat, .len = len };
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

TyDict* DictStk_topMaybe(DictStk* stk) {
  if(stk->sp == stk->cap) return NULL;
  return DictStk_top(stk);
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
  .name = (CStr*) ("\x0A" "baseCompFn"),
  .meta = TY_FN | TY_FN_NATIVE,
  .code = (U1*)baseCompFn,
};

void TyDb_init(TyDb* db) {
  db->tyIs = Stk_init(db->tyIsDat, TYDB_DEPTH);
  db->done = Stk_init(db->doneDat, TYDB_DEPTH);
}

void TyDb_setDone(TyDb* db, HowDone done) {
  S* cur = Stk_topRef(&db->done);
  ASSERT(not *cur, "done set multiple times");
  *cur = done;
}

void TyDb_clearDone(TyDb* db) { *Stk_topRef(&db->done) = NOT_DONE; }

void DictStk_reset(Kern* k) {
  k->g.dictStk.sp = k->g.dictStk.cap;
  DictStk_add(&k->g.dictStk, &k->g.rootDict);
}

void Kern_errCleanup(Kern* k) {
  k->g.metaNext = 0; k->g.cstate = 0;
  k->g.fnLocals = 0; k->g.fnState = 0;
  k->g.curTy = NULL;
  k->g.compFn = &TyFn_baseCompFn;
  DictStk_reset(k);  k->g.modStk.sp = k->g.modStk.cap;
  k->g.blk = NULL;
  k->g.code.len = 0;

  k->g.bbaDict = &k->bbaDict;
  // BBA_drop(&k->g.bbaTyImm);
  // TyDb_init(&k->g.tyDbImm);
  if(k->g.tyDb.bba) {
    // BBA_drop(k->g.tyDb.bba);
    TyDb_init(&k->g.tyDb); TyDb_new(&k->g.tyDb);
  }
  k->fb->ep = NULL;
  Stk_clear(&k->fb->ws); Stk_clear(&k->fb->rs);
  Stk_clear(&k->fb->info);
  k->g.code.len = 0;

  // // Drop all but the last
  SllSpArena** root = &k->fb->sllArena;
  while(*root and (*root)->next) {
    SllSpArena* drop = (SllSpArena*)Sll_pop((Sll**)root);
    Sp_Xr0(drop->arena,drop);
  }
  assert(*root);
}

void Kern_init(Kern* k, FnFiber* fb) {
  *k = (Kern) {
    .bbaCode = (BBA) { &civ.ba },
    .bbaDict = (BBA) { &civ.ba },
    .bbaRepl = (BBA) { &civ.ba },
    .bbaSllArena  = (BBA) { &civ.ba },
    .sllArena = (SllSpArena) { .arena = BBA_asSpArena(&k->bbaSllArena) },
    .fb = fb,
    .g = {
      .compFn = &TyFn_baseCompFn,
      .dictStk = (DictStk) { .dat = k->g.dictBuf, .sp = DICT_DEPTH, .cap = DICT_DEPTH },
      .modStk  = (DictStk) { .dat = k->g.modBuf,  .sp = DICT_DEPTH, .cap = DICT_DEPTH },
      .token = (Buf){.dat = k->g.tokenDat, .cap = 64},
      .bbaDict = &k->bbaDict,
      .bbaTyImm = (BBA) { &civ.ba },
    },
  };
  fb->sllArena = &k->sllArena;
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
  CBst* found = CBst_get(k->g.cBst, tokenSlc(k));
  if(found) { tokenDrop(k); return found->key; }
  CStr* key = CStr_new(BBA_asArena(k->g.bbaDict), *Buf_asSlc(&k->g.token));
  CBst* add = BBA_alloc(k->g.bbaDict, sizeof(CBst), RSIZE);
  ASSERT(key and add, "tokenCStr OOM");
  *add = (CBst) { .key = key }; CBst_add(&k->g.cBst, add);
  tokenDrop(k);
  return key;
}

// Allocate, initialize and add to dict a new Ty from the information in Kern.
// This includes the token as the fn name.
//
// It is the caller's job to initialize .v
// It is also the caller's job to handle TyFn and TyDict values.
Ty* Ty_newTyKey(Kern* k, U2 meta, CStr* key, TyI* tyKey) {
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
  ty->name  = key; ty->tyKey = tyKey;
  ty->parent = DictStk_topMaybe(&k->g.modStk);
  ty->meta = meta;
  ty->line = k->g.srcInfo->line;
  ty->file = k->g.srcInfo;
  Kern_addTy(k, ty);
  return ty;
}

Ty* Ty_new(Kern* k, U2 meta, CStr* key) {
  return Ty_newTyKey(k, meta, key, NULL);
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
  .name = (CStr*) ("\x0F" "__catchMarker__"),
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
  eprintf("!!! instr %0.u: %+10.*s: ", k->fb->ep, Instr_fmt(instr)); dbgWs(k); NL;
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
    case OWR : WS_ADD((S) ((OwnedValue*)popLit(k, 4))->ref);  R0
    case LR: WS_ADD(RS_topRef(k) + popLit(k, 2)); R0
    case GR: {
      TyVar* g = tyVar((Ty*)popLit(k, 4));
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
    case XW:  xImpl(k, (Ty*) WS_POP());        R0
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
  if(fn->name) eprintf("!!! executeFn %.*s\n", Ty_fmt(fn));
  else         eprintf("!!! executeFn <unknown>\n");
  if(isFnNative(fn)) {
    return executeNative(k, fn);
  }
  ASSERT(not isFnAbsmeth(fn), "attempted execution of absmeth");
  cfb->ep = fn->code;
  executeLoop(k);
}

TyI* executeFnSynty(Kern* k, TyFn* fn) {
  ASSERT(isFnSynty(fn), "expected synty fn");
  executeFn(k, fn); return (TyI*) WS_POP();
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

bool TyI_unsized(TyI* tyI);
bool TyDict_unsized(TyDict* ty) {
  if(isDictNative(ty)) return false;
  return TyI_unsized(ty->fields); // check last field
}

// Return the KNOWN TyDict size. Callers must also check TyDict_unsized.
S TyDict_size(TyDict* ty) {
  ASSERT(not isDictMod(ty), "attempted size of TY_DICT_MOD");
  if(isDictNative(ty)) return szIToSz((S)ty->children);
  if(isDictRole(ty))   return 2 * RSIZE; // { &Data, &Methods }
  return ty->sz;
}

Ty* Ty_unalias(Ty* ty) {
  while(true) {
    if(not isTyVar(ty))             return ty;
    if(not isVarAlias((TyVar*) ty)) return ty;
    ty = (Ty*) ((TyVar*)ty)->v;
  }
}

TyDict* tyStruct(Ty* ty) {
  TyDict* d = tyDict(ty);
  ASSERT(isDictStruct(d), "require struct");
  return d;
}

Ty* TyDict_find(TyDict* dict, Key* s) {
  while(true) {
    Ty* find = dict->children;
    I4 i = Ty_find(&find, s);
    if(i or not find) {
      // prevent recursion
      if(not s->tyI and Slc_eq(SUPER.name, s->name)) return NULL;
      Ty* super = TyDict_find(dict, &SUPER);
      if(not super) return NULL;
      dict = tyDictB(tyVar(super)->tyI->ty);
    } else  return Ty_unalias((Ty*) find);
  }
}

Ty* TyDict_scanTyKey(Kern* k, TyDict* dict, TyI* tyKey) {
  Key key = (Key) { .name = tokenSlc(k), .tyI = tyKey };
  Ty* ty = TyDict_find(dict, &key);
  if(not ty) return NULL;
  tokenDrop(k); return ty;
}

Ty* TyDict_scanTy(Kern* k, TyDict* dict) {
  return TyDict_scanTyKey(k, dict, NULL);
}

TyVar* TyDict_field(TyDict* d, TyI* field) {
  Key key = (Key) { CStr_asSlc(field->name) };
  TyVar* var = (TyVar*) TyDict_find(d, &key);
  assert(var && isTyVar((Ty*)var) && not isVarGlobal(var));
  return var;
}

// ***********************
//   * 3.XX: TyI

I4 InOut_cmp(InOut* a, InOut* b) {
  I4 c = TyI_cmp(a->inp, b->inp); if(c) return c;
  return TyI_cmp(a->out, b->out);
}

I4 FnSig_find(FnSig** node, InOut* key) {
  return Bst_find((Bst**)node, key, (BstCmp)&InOut_cmp);
}

FnSig* FnSig_add(FnSig** root, FnSig* add) {
  return (FnSig*) Bst_add((Bst**)root, (Bst*)add, &add->io, (BstCmp)&InOut_cmp);
}

FnSig* FnSig_findOrAdd(Kern* k, InOut io) {
  FnSig* root = k->g.fnSigBst;
  I4 cmp = FnSig_find(&root, &io);
  if(root and not cmp) return root;
  FnSig* add = (FnSig*) BBA_alloc(k->g.bbaDict, sizeof(FnSig), RSIZE);
  ASSERT(add, "FnSig OOM");
  *add = (FnSig) { .meta = FN_SIG, .io = io };
  root = k->g.fnSigBst; assert(not FnSig_add(&root, add));
  return add;
}

I4 TyIBst_find(TyIBst** node, TyI* key) {
  return Bst_find((Bst**)node, key, (BstCmp)&TyI_cmp);
}

TyIBst* TyIBst_add(TyIBst** root, TyIBst* add) {
  return (TyIBst*) Bst_add((Bst**)root, (Bst*)add, &add->tyI, (BstCmp)&TyI_cmp);
}

// TyI Binary Search Tree (database) design.
// TyI's are linked-lists. Say you already had a type:
//        B -> C
// Now you want a new type:
//   A -> B -> C
// We can reuse the "B -> C" in the second type.
//
// When we findOrAdd we check for the whole chain (A -> B -> C). If we find it
// we are done.  If we can't find it we create the first node and recursively
// check+create sub-chains
TyI* TyI_findOrAdd(Kern* k, TyI* tyI) {
  TyIBst* root = k->g.tyIBst;
  I4 cmp = TyIBst_find(&root, tyI);
  if(root and not cmp) return &root->tyI;
  TyIBst* add = (TyIBst*) BBA_alloc(k->g.bbaDict, sizeof(TyIBst), RSIZE);
  ASSERT(add, "TyI OOM");
  *add = (TyIBst) { .tyI = *tyI };
  if(tyI->next) add->tyI.next = TyI_findOrAdd(k, tyI->next);
  root = k->g.tyIBst; assert(not TyIBst_add(&root, add));
  return &add->tyI;
}

void TyI_rootAdd(Kern* k, TyI** root, TyI* tyI) {
  // note: root will temporarily point to add, and add will be mutated
  TyI add = *tyI; Sll_add((Sll**)root, TyI_asSll(&add));
  *root = TyI_findOrAdd(k, *root);
}

// Return whether the type is unsized
bool TyI_unsized(TyI* tyI) {
  if(TyI_refs(tyI))             return false;
  if(TY_UNSIZED == tyI->arrLen) return true;
  ASSERT(isTyDictB(tyI->ty), "TyI non-ref must be dict");
  return TyDict_unsized((TyDict*)tyI->ty);
}

S TyI_sz(TyI* tyI) {
  if(TyI_refs(tyI)) return RSIZE;
  ASSERT(isTyDictB(tyI->ty), "TyI must have refs or be a dict.");
  return TyDict_size((TyDict*)tyI->ty);
}

bool TyI_check(TyI* r, TyI* g);
bool _TyI_checkAll(TyI* r, TyI* g) {
  while(true) {
    if(not r)               return not g;
    if(not g)               return false;
    if(not TyI_check(r, g)) return false;
    r = r->next; g = g->next;
    if((not r) and (not g)) return true;
  }
}

bool InOut_check(InOut* r, InOut* g) {
  return (_TyI_checkAll(r->inp, g->inp) &&
          _TyI_checkAll(r->out, g->out));
}

// Return 1 if the types check, else 0.
bool TyI_check(TyI* r, TyI* g) { // r=require g=given
  if(TyI_refs(r) != TyI_refs(g))             return false;
  if(r->ty == g->ty)                         return true;
  if(isFnSig(r->ty) && isFnSig(g->ty)) {
    FnSig* rSig = (FnSig*)r->ty; FnSig* gSig = (FnSig*)g->ty;
    return InOut_check(&rSig->io, &gSig->io);
  }
  if(!isTyDictB(r->ty) || !isTyDictB(g->ty)) return false;
  TyDict* rd = (TyDict*)r->ty; TyDict* gd = (TyDict*)g->ty;
  if((&Ty_S == rd) or (&Ty_U4 == rd)) {
    return (  (&Ty_U1 == gd) or (&Ty_U2 == gd)
           or (&Ty_U4 == gd) or (&Ty_S  == gd) );
  } else if ((&Ty_U2 == rd) and (&Ty_U1 == gd)) {
    return true;
  } else if ((&Ty_I4 == rd) or (&Ty_SI == rd)) {
    return (&Ty_I4 == gd) or (&Ty_SI == gd);
  }
  if(isDictStruct(rd) and isDictStruct(gd)) {
    while(true) {
      Ty* super = TyDict_find(gd, &SUPER);
      if(not super) return false;
      gd = tyStruct((Ty*)tyVar(super)->tyI->ty);
      if(rd == gd) return true;
    }
  }
  return false;
}

void TyI_printAll(TyI* tyI);
void TyI_print(TyI* tyI) {
  if(tyI->name) eprintf("%.*s", Dat_fmt(*tyI->name));
  else          eprintf("_");
  eprintf(":");
  for(U1 refs = TyI_refs(tyI), i = 0; i < refs; i++) eprintf("&");
  if(isFnSig(tyI->ty)) {
    FnSig* sig = (FnSig*)tyI->ty;
    eprintf("fnSig["); TyI_printAll(sig->io.inp);
    eprintf(" ->");    TyI_printAll(sig->io.out); eprintf("]");
  } else {
    CStr* name = ((Ty*)tyI->ty)->name;
    if(not name) eprintf("<nullname>");
    else         eprintf("%.*s", Dat_fmt(*name));
  }
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

void _TyI_cloneNewSelf(BBA* bba, TyI** root, TyI* nodes, TyDict* self) {
  assert(self); if(not nodes) return;
  _TyI_cloneNewSelf(bba, root, nodes->next, self);
  if((TyBase*)&Ty_Self == nodes->ty) {
    assert(TyI_refs(nodes));
    TyI add = *nodes; add.ty = (TyBase*)self;
    TyI_cloneAddNode(bba, root, &add);
  } else TyI_cloneAddNode(bba, root, nodes);
}

TyI*  TyI_cloneNewSelf(BBA* bba, TyI* nodes, TyDict* self) {
  if(not nodes) return NULL;
  TyI* root = NULL; _TyI_cloneNewSelf(bba, &root, nodes, self);
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

// Pop one TyI from the top level.
void TyDb_pop(Kern* k, TyDb* db) {
  Sll** root = TyDb_rootSll(db);
  Slc* err = BBA_free(db->bba, Sll_pop(root), sizeof(TyI), RSIZE);
  if(err) SET_ERR(*err);
}

// Free all nodes in the top level
void TyDb_free(Kern* k, TyDb* db, TyI* stream) {
  while(stream) {
    stream = stream->next;
    TyDb_pop(k, db);
  }
}

// Drop one type level
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
  TyI* require = require_; TyI* given = given_;
  S i = 0;
  while(require) {
    if(not given) {
      if(not ERR_EXPECTED) {
        eprintf("!! Type stack underflow:\n");
        tyNotMatch(require_, given_);
      }
      SET_ERR(errCxt);
    }
    if(not TyI_check(require, given)) {
      if(not ERR_EXPECTED) {
        eprintf("!! Given[index=%u] types don't match Require\n", i);
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

void InOut_replaceSelf(Kern* k, InOut* io, TyDict* self) {
  io->inp = TyI_cloneNewSelf(&k->bbaCode, io->inp, self);
  io->out = TyI_cloneNewSelf(&k->bbaCode, io->out, self);
}

void InOut_free(Kern* k, InOut* io) {
  assert(not Sll_free(TyI_asSll(io->out), sizeof(TyI), BBA_asArena(&k->bbaCode)));
  assert(not Sll_free(TyI_asSll(io->inp), sizeof(TyI), BBA_asArena(&k->bbaCode)));
}

FnSig* FnSig_replaceSelf(Kern* k, FnSig* sig, TyDict* self) {
  InOut io = sig->io;            InOut_replaceSelf(k, &io, self);
  sig = FnSig_findOrAdd(k, io);  InOut_free(k, &io);
  return sig;
}

void tyCallSelf(Kern* k, TyDb* db, TyI* inp, TyI* out, TyDict* self) {
  if(not self) return tyCall(k, db, inp, out);
  InOut io = { .inp = inp, .out = out };
  InOut_replaceSelf(k, &io, self);
  tyCall(k, db, io.inp, io.out);
  InOut_free(k, &io);
}

// Check function return type and possibly mark as done.
void tyRet(Kern* k, TyDb* db, HowDone done) {
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

// ********
//   * 4.b: Token scanner

// Inc head, keeping track of newlines/etc
void SrcRing_incHead(Kern* k, Ring* r, U2 inc) {
  FileInfo* f = k->g.srcInfo;
  Slc s = Ring_1st(r); U2 i; U2 len = S_min(inc, s.len);
  for(i = 0; i < len; i++) {
    eprintf("srcInc: 0x%X '%c'\n", s.dat[i], s.dat[i]);
    if(s.dat[i] == '\n') f->line += 1;
  }
  s = Ring_2nd(r); len = inc - i;
  assert(len <= s.len);
  for(i = 0; i < len; i++) {
    eprintf("srcInc: 0x%X '%c'\n", s.dat[i], s.dat[i]);
    if(s.dat[i] == '\n') f->line += 1;
  }
  Ring_incHead(r, inc);
}

U1 toTokenGroup(U1 c) {
  if(c <= ' ')             return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if(c == '_')             return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '#' || c == '|' || c == '.' || c == ':'  ||
     c == '(' || c == ')' || c == '&' || c == ';') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

void skipWhitespace(Kern* k, SpReader f) {
  Ring* r = &SpReader_asBase(k, f)->ring;
  while(true) {
    U1* c = SpReader_get(k, f, 0);
    if(c == NULL) return;
    if(*c > ' ')  return;
    SrcRing_incHead(k, r, 1);
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
  SrcRing_incHead(k, &SpReader_asBase(k, k->g.src)->ring, b->len);
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
#define REQUIRE(T)  ASSERT(CONSUME(T), "Expected '" T "'")

// ********
//   * 4.c: Number Scanner

U1 cToU1(U1 c) {
  if('0' <= c && c <= '9') return c - '0';
  if('a' <= c && c <= 'f') return 10 + c - 'a';
  if('A' <= c && c <= 'F') return 10 + c - 'A';
  return 0xFF; // invalid
}

// Attempt to parse a number from the token
ParsedNumber parseU4(Kern* k, Slc t) {
  ParsedNumber p = {0};
  U1 base = 10; U2 i = 0;
  if(t.len > 2) {
    Slc s = Slc_slc(&t, 0, 2);
    if(Slc_eq(SLC("0b"), s)) { base = 2;  i = 2; }
    if(Slc_eq(SLC("0x"), s)) { base = 16; i = 2; }
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

void compileLit(Kern* k, S v, bool asImm) {
  TyI* tyI; if (v <= 0xFF)   tyI = &TyIs_U1;
       else if (v <= 0xFFFF) tyI = &TyIs_U2;
       else                  tyI = &TyIs_S;
  tyCall(k, tyDb(k, asImm), NULL, tyI);
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

Ty* Kern_findTy(Kern* k, Key* key) { // You probably want to use scanTy
  Ty* ty = NULL;
  DictStk* dicts = &k->g.dictStk;
  for(U2 i = dicts->sp; i < dicts->cap; i++) {
    ty = dicts->dat[i]->children;
    I4 res = Ty_find(&ty, key);
    if((0 == res) && (ty != NULL)) return Ty_unalias(ty);
  }
  return NULL;
}

// You probably want to use scanTy
Ty* Kern_findToken(Kern* k) {
  Key key = { *Buf_asSlc(&k->g.token) };
  return Kern_findTy(k, &key);
}

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

void Kern_addTy(Kern* k, Ty* ty) {
  ty->l = NULL; ty->r = NULL;
  DictStk* dicts = &k->g.dictStk;
  ASSERT(dicts->sp < dicts->cap, "No dicts");
  Ty** root = &DictStk_top(dicts)->children;
  ty = Ty_add(root, ty);
  if(ty) {
    eprintf("!! Overwritten key: %.*s\n", Dat_fmt(*ty->name));
    SET_ERR(SLC("key was overwritten"));
  }
}

Ty* scanTy(Kern* k) {
  scan(k); Ty* ty = Kern_findToken(k);
  if(not ty) return NULL;
  tokenDrop(k);
  return ty;
}

TyI* _scanTyI(Kern* k) {
  scan(k); TyBase* ty = (TyBase*) Kern_findToken(k);
  ASSERT(ty, "Type not found");
  tokenDrop(k);
  if(isTyFnB(ty)) {
    if(isFnSynty((TyFn*)ty)) return executeFnSynty(k, (TyFn*)ty);
    ty = (TyBase*) FnSig_findOrAdd(k, *TyFn_asInOut((TyFn*)ty));
  }
  return TyI_findOrAdd(k, &(TyI){ .ty = ty });
}

TyI* scanTySpec(Kern *k) {
  TyI s = {0};
  while(true) {
    if(CONSUME("?"))      s.arrLen = TY_UNSIZED;
    else if(CONSUME("&")) s.meta += 1;
    else break;
  }
  ASSERT(s.meta <= TY_REFS, "number of '&' must be <= 3");

  if(CONSUME("Arr")) {
    ASSERT(not s.arrLen, "Use Arr[? Ty] not ?Arr[Ty]");
    ASSERT(not TyI_refs(&s), "cannot be reference to array, just use more &");
    REQUIRE("["); S n = TY_UNSIZED;
    if(CONSUME("?")) {}
    else {
      single(k, /*asImm=*/true); tyCall(k, tyDb(k, true), &TyIs_S, NULL);
      n = WS_POP();
    }
    CONSUME(","); s = *scanTySpec(k); REQUIRE("]");
    ASSERT(not s.arrLen, "nested arrays not allowed");
    s.arrLen = n;
  } else {
    TyI* scanned = _scanTyI(k);
    ASSERT(not (scanned->meta   && s.meta),   "type refs double specified");
    ASSERT(not (scanned->arrLen && s.arrLen), "type array double specified");
    s = (TyI) {
      .ty = scanned->ty,
      .meta   = s.meta   + scanned->meta,
      .arrLen = s.arrLen + scanned->arrLen,
    };
  }
  if(not s.ty) {
    eprintf("!! Type not found: %.*s\n", Dat_fmt(k->g.token));
    SET_ERR(SLC("Type not found"));
  }
  if(isTyDictB(s.ty)) {}
  else if(isFnSig(s.ty)) {
    ASSERT(TyI_refs(&s) > 0, "fn signature must be a reference");
  }
  else {
    eprintf("!!! type spec meta=0x%X\n", s.ty->meta);
    SET_ERR(SLC("type spec must be a Dict or &FnSig"))
  }
  return TyI_findOrAdd(k, &s);
}

TyI* scanTyI(Kern* k, CStr* name) {
  TyI tyI = *scanTySpec(k); tyI.name = name;
  return TyI_findOrAdd(k, &tyI);
}


// ***********************
//   * srOffset
// Handles not only storing offset, but also parsing '{' syntax.

bool opCreatesRef(U1 op) {
  switch(op) {
    case LR: return true;
    case GR: return true;
    default: return false;
  }
}

typedef struct {
  U1 op;
  bool checkTy;
  bool clear;
  bool notParse; // parse=look for '{'
  bool asImm;
  TyVar* global;
} SrOffset;
void srOffsetStruct(Kern* k, TyDict* d, U2 offset, SrOffset* st);
void compImm(Kern* k);

void srOffset(Kern* k, TyI* tyI, U2 offset, SrOffset* st) {
  if(not st->notParse) {
    if(CONSUME("{")) return srOffsetStruct(k, tyDictB(tyI->ty), offset, st);
    Kern_compFn(k);
  }

  Buf* b = st->asImm ? NULL : &k->g.code; TyDb* db = tyDb(k, st->asImm);
  if(st->checkTy)   tyCall(k, db, tyI, NULL);
  if(TyI_refs(tyI)) return opOffset(k, b, st->op, SZR, offset, st->global);
  ASSERT(not isTyFnB(tyI->ty), "invalid fn local"); assert(isTyDictB(tyI->ty));
  TyDict* d = (TyDict*) tyI->ty;
  ASSERT(not isDictMod(d), "cannot store in mod");
  if(isDictNative(d)) return opOffset(k, b, st->op, /*szI*/(S)d->children, offset, st->global);
  SrOffset recSt = *st; // Note: we CANNOT modify 'st', since (unlike ftOffset)
                        // it may be re-used in next field.
  recSt.checkTy = false; recSt.clear = false; recSt.notParse = true;
  if(isDictRole(d)) {
    if(opCreatesRef(st->op)) {
      assert(false);
      // srOffset(k, &TyIs_S, offset, &recSt);
    } else {
      assert(RSIZE == ROLE_METH_OFFSET); // ensure meth is "top" of stack
      assert(not recSt.global); // TODO: This should be fine, just checking
      srOffset(k, &TyIs_S, offset + ROLE_METH_OFFSET, &recSt);
      srOffset(k, &TyIs_S, offset + ROLE_DATA_OFFSET, &recSt);
    }
    return;
  }
  assert(isDictStruct(d));
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
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)) {
      Kern_compFn(k);
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
  } assert(false);
}

void ftOffsetStruct(Kern* k, TyDict* d, TyI* field, U2 offset, FtOffset* st);

bool opIsConcrete(U1 op) {
  switch(op) {
    case FTLL: return true;
    case SRLL: return true;
    case FTGL: return true;
    case SRGL: return true;
    default:   return false;
  }
}

// Handle type operations of refs.
// In both cases (FTO and FTLL) we just put the tyI on the stack
// For FTO, the dot compiler already reduced the number of references.
void _tyFtOffsetRefs(Kern* k, TyDb* db, TyI* tyI, FtOffset* st) {
  if(st->noAddTy) return;
  if(st->op == FTLL || st->op == FTGL) return tyCall(k, db, NULL, tyI);
  assert(st->op == FTO); assert(not tyI->next);
  tyCall(k, db, NULL, tyI);
}

void compileFn(Kern* k, TyFn* fn, TyDict* self, bool asImm) {
  tyCallSelf(k, tyDb(k, asImm), fn->inp, fn->out, self);
  if(asImm) return executeFn(k, fn);
  Buf* b = &k->g.code;
  if(isFnInline(fn)) return Buf_extend(b, (Slc){fn->code, .len=fn->len});
  Buf_add(b, XL); Buf_addBE4(b, (U4)fn);
}

// Just pushes &self onto the stack and calls the method.
void compileMethod(Kern* k, TyI* vTyI, TyDict* d, TyFn* meth, U2 offset, FtOffset* st) {
  Buf* b = st->asImm ? NULL : &k->g.code; TyDb* db = tyDb(k, st->asImm);
  assert(not st->noAddTy); // invariant
  if(isFnMeth(meth)) {
    TyI self = { .ty = (TyBase*)d, .meta = /*refs*/1 };
    tyCall(k, db, TYI_VOID, &self);
    opOffset(k, b, ftOpToRef(st->op), 0, offset, st->global);
  } else if (isFnAbsmeth(meth)) {
    ASSERT(opIsConcrete(st->op), "absmeth not allowed on reference");
    tyCall(k, db, TYI_VOID, &TyIs_rSelf);
    // Get &Data first
    if(TyI_refs(vTyI)) {
           opOffset(k, b, st->op, SZR, offset, st->global); // &Role
           opOffset(k, b,    FTO, SZR, offset + ROLE_DATA_OFFSET, NULL);
    } else opOffset(k, b, st->op, SZR, offset + ROLE_DATA_OFFSET, st->global);
    Kern_compFn(k);
    // Get &Methods and ft the offset, then call it
    if(TyI_refs(vTyI)) {
           opOffset(k, b, st->op,  SZR, offset, st->global); // &Role
           opOffset(k, b,     FTO, SZR, offset + ROLE_METH_OFFSET, NULL);
    } else opOffset(k, b, st->op,  SZR, offset + ROLE_METH_OFFSET, st->global);
    opOffset(k, b, FTO, SZR, /*offset*/(S)meth->code, NULL); // get actual method
    tyCall(k, db, meth->inp, meth->out);
    if(st->asImm) executeFn(k, (TyFn*)WS_POP());
    else          Buf_add(&k->g.code, XW);
    return;
  }
  Kern_compFn(k);
  compileFn(k, meth, d, st->asImm);
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
  ASSERT(not isTyFnB(tyI->ty), "invalid fn local");
  ASSERT(isTyDictB(tyI->ty), "invalid non-dict fetch");
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
  ASSERT(isDictStruct(d) || isDictRole(d), "unknown dict type");
  if(CONSUME(".")) {
    Ty* f = TyDict_scanTy(k, d);
    ASSERT(f, "member not found");
    if(isTyVar(f)) {
      TyVar* field = (TyVar*)f;
      assert(not isVarGlobal(field));
      return ftOffset(k, field->tyI, offset + field->v, st);
    } else return compileMethod(k, tyI, d, tyFn(f), offset, st);
  }
  if(addTy) tyCall(k, db, NULL, tyI);
  st->noAddTy = true;
  if(isDictRole(d)) {
    assert(RSIZE == ROLE_METH_OFFSET); // ensure meth is "top" of stack
    ftOffset(k, &TyIs_S, offset + ROLE_DATA_OFFSET, st);
    ftOffset(k, &TyIs_S, offset + ROLE_METH_OFFSET, st);
  }
  else ftOffsetStruct(k, d, d->fields, offset, st);
}

void ftOffsetStruct(Kern* k, TyDict* d, TyI* field, U2 offset, FtOffset* st) {
  if(not field) return;
  ftOffsetStruct(k, d, field->next, offset, st);
  ftOffset(k, field, offset + TyDict_field(d, field)->v, st);
}


// ***********************
//   * compileTy (Var, Dict, Fn)

void compileVarFt(Kern* k, TyVar* v, bool asImm, bool addTy) {
  TyVar* global = isVarGlobal(v) ? v : NULL;
  FtOffset st = (FtOffset) {
    .op = global ? FTGL : FTLL, .findEqual = true, .asImm = asImm,
    .global = global, .noAddTy = not addTy };
  START_IMM(asImm);
  ftOffset(k, v->tyI, /*offset=*/global ? 0 : v->v, &st);
  END_IMM;
}

void compileVar(Kern* k, TyVar* v, bool asImm) {
  TyVar* global = isVarGlobal(v) ? v : NULL;
  if(asImm) ASSERT(global, "'imm#' used with local variable");
  if(CONSUME("=")) {
    START_IMM(asImm);
    SrOffset st = (SrOffset) {
      .op = global ? SRGL : SRLL, .checkTy = true, .asImm = asImm, .global = global
    };
    srOffset(k, v->tyI, /*offset=*/global ? 0 : v->v, &st);
    END_IMM;
  } else compileVarFt(k, v, asImm, /*addTy*/true);
}

// Return the next member through '.', or NULL if there is no '.'
Ty* nextDot(Kern* k, TyDict* d) {
  if(not CONSUME(".")) return NULL;
  Ty* ty = TyDict_scanTy(k, d); ASSERT(ty, "member not found");
  return ty;
}

void assertRefCast(TyI* top) {
  ASSERT(not TyI_unsized(top), "Cannot convert unsized type to pointer");
  ASSERT(RSIZE == TyI_sz(top), "Cannot convert type to pointer unless sz=RSIZE");
}

void compileDict(Kern* k, TyDict* d, bool asImm) {
  assert(not isDictMod(d));
  Kern_compFn(k); // compile next token
  if(IS_UNTY) return;
  TyDb* db = tyDb(k, asImm);
  TyI tyI = (TyI) {.ty = (TyBase*) d};
  TyI* top = TyDb_top(db);
  if(isDictNative(d)) {
    if(TyI_refs(top)) assertRefCast(top);
    else {
      ASSERT((isTyDictB(top->ty) && isDictNative((TyDict*)top->ty)),
           "Cannot cast non-native as native.");
    }
    TyDb_pop(k, db); tyCall(k, db, NULL, &tyI);
  } else if (isDictRole(d)) {
    TyDb_pop(k, db); // Currently &Self is on the stack
    ASSERT(1 == TyI_refs(top), "role: only single refs");
    ASSERT(isTyDict((Ty*)top->ty), "role: only dicts");
    TyVar* impl = (TyVar*) TyDict_find((TyDict*)top->ty, &(Key){
      .name = Slc_frCStr(d->name), .tyI = &tyI });
    ASSERT(impl, "role not implemented for type");
    assert(isTyVar((Ty*)impl) && isVarGlobal(impl));
    Buf* b = &k->g.code;
    assert(not asImm);
    assert(RSIZE == ROLE_METH_OFFSET); // ensure meth is "top" of stack
    lit(b, impl->v);
    tyCall(k, db, NULL, &tyI);
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
  // eprintf("!!! compileTy: %.*s\n", Ty_fmt(ty));
  checkName(k, ty);
  do {
    if(isTyVar(ty))   return compileVar(k, (TyVar*) ty, asImm);
    if(isTyDict(ty)) {
      TyDict* d = (TyDict*) ty;
      if(isDictMod(d)) {
        ty = nextDot(k, d); ASSERT(ty, "Cannot access mod directly.");
        continue;
      }
      return compileDict(k, (TyDict*) ty, asImm);
    }
  } while(0);
  assert(not isFnSig((TyBase*)ty));
  ASSERT(isTyFn(ty), "Unknown type meta"); TyFn* fn = (TyFn*)ty;
  if(isFnSyn(fn)) { WS_ADD(asImm); return executeFn(k, fn); }
  Kern_compFn(k); // non-syn functions consume next token
  if(isFnImm(fn)) { ASSERT(asImm, "fn must be called with 'imm#'"); }
  compileFn(k, fn, /*self*/NULL, asImm);
}

// ***********************
//   * single: compile a single token + compileSrc
void single(Kern* k, bool asImm) {
  scan(k); Slc t = *Buf_asSlc(&k->g.token);
  // eprintf("!!! single: asImm=%X t=%.*s\n", asImm, Dat_fmt(t));
  if(not t.len) return;
  ParsedNumber n = parseU4(k, t);
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

void _N_ret(Kern* k) { tyRet(k, tyDb(k, false), RET_DONE); Buf_add(&k->g.code, RET); }
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
  TyI tyI = (TyI){.ty = (TyBase*)ty};
  tyCall(k, db, &tyI, ty->fields);
}

// ***********************
//   * Core syn functions

void _N_imm(Kern* k);
TyFn _TyFn_imm = TyFn_native("\x04" "_imm", TY_FN_SYN, (U1*)_N_imm, TYI_VOID, TYI_VOID);
void _N_imm(Kern* k) { if(Kern_eof(k)) return;  single(k, true); }

// Compile a token (and all sub-tokens) with asImm=true
void compImm(Kern* k) {
  START_IMM(true); executeFn(k, &_TyFn_imm); END_IMM;
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

TyDict* _useGet(Kern *k) {
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


void _use(Kern* k, TyDict* d) {
  assert(not isDictNative(d));
  DictStk_add(&k->g.dictStk, d);
  DictStk_add(&k->g.modStk, d);
  Kern_compFn(k);
  DictStk_pop(&k->g.dictStk);
  DictStk_pop(&k->g.modStk);
}

void N_cast(Kern* k) {
  bool asImm = WS_POP(); REQUIRE(":"); TyDb* db = tyDb(k, asImm);
  TyI* s = scanTyI(k, NULL); ASSERT(not s->arrLen, "Array not allowed in cast");
  Kern_compFn(k);
  TyI from = *TyDb_top(db); from.next = NULL;
  TyI to   = *s;            to.next   = NULL;
  ASSERT(TyI_check(&from, &to) or TyI_check(&to, &from),
         "cast of non-compatible types");
  tyCall(k, db, &from, &to);
}

void N_mod(Kern* k) {
  N_notImm(k);
  TyDict* d = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_MOD, NULL);
  _use(k, d);
}

void N_use(Kern* k) {
  N_notImm(k); REQUIRE(":");
  _use(k, _useGet(k));
}

void N_fileloc(Kern* k) { // loc that stays for whole file
  N_notImm(k); REQUIRE(":");
  TyDict* d = _useGet(k);
  DictStk_add(&k->g.dictStk, d);
}

// ***********************
//   * fn and fn signature

// fnMeta:IMM to make next function immediate, etc.
void N_fnMeta(Kern* k)   {
  N_notImm(k); REQUIRE(":");
  compImm(k); tyCall(k, tyDb(k, true), &TyIs_S, NULL);
  S meta = WS_POP();
  ASSERT(not (TY_FN_TY_MASK & k->g.metaNext),
         "fn can only be one main type: imm, syn, synty, inline, comment");
  k->g.metaNext |= (TY_FN_TY_MASK & meta);
}


FnSig* parseFnSig(Kern* k) {
  REQUIRE("["); InOut io = {}; bool isInp = true;
  while(true) {
    if(CONSUME("]"))  break;
    if(CONSUME("->")) isInp = false;
    TyI** root = isInp ? &io.inp : &io.out;
    TyI_rootAdd(k, root, scanTySpec(k));
  }
  return FnSig_findOrAdd(k, io);
}


// synty function
// Example:
//    var f:&fnSig[stk:U2, stk:U4 -> S] = &myFn;
void N_fnSig(Kern* k)   {
  TyI* tyI = TyI_findOrAdd(k, &(TyI){ .ty = (TyBase*)parseFnSig(k) });
  WS_ADD((S)tyI);
}

#define SET_FN_STATE(STATE)  k->g.fnState = bitSet(k->g.fnState, STATE, C_FN_STATE)
#define IS_FN_STATE(STATE)   ((C_FN_STATE & k->g.fnState) == (STATE))

void synFnFound(Kern* k, Ty* ty) {
  ASSERT(isTyFn(ty) and isFnSyn((TyFn*)ty),
         "expect only syn functions in function signature");
  executeFn(k, (TyFn*)ty);
}

// Used for both stk and out
void N_stk(Kern *k) {
  N_notImm(k);
  TyI** root;
  if(     IS_FN_STATE(FN_STATE_STK)) root = &tyFn(k->g.curTy)->inp;
  else if(IS_FN_STATE(FN_STATE_OUT)) root = &tyFn(k->g.curTy)->out;
  else { SET_ERR(SLC("stk used after local variable inputs or in body")); }
  CONSUME(":");
  TyI* tyI = scanTyI(k, NULL); ASSERT(not tyI->arrLen, "Stk input cannot be array");
  TyI_rootAdd(k, root, tyI);
}
TyFn TyFn_stk = TyFn_native("\x03" "stk", TY_FN_SYN, (U1*)N_stk, TYI_VOID, TYI_VOID);

typedef struct { TyVar* var; S sz; S els; } VarPre;
VarPre varPre(Kern* k) {
  CStr* name = tokenCStr(k);
  TyVar* v = (TyVar*) Ty_new(k, TY_VAR, name);
  REQUIRE(":"); TyI* tyI = scanTyI(k, name);
  ASSERT(not TyI_unsized(tyI), "Var must have a known size");
  v->tyI = tyI;
  S sz = TyI_sz(tyI);
  if(tyI->arrLen > 1) sz = align(sz, alignment(sz)); // align sz for arrays
  return (VarPre) { .var = v, .sz = sz, .els = tyI->arrLen ? tyI->arrLen : 1 };
}

void localImpl(Kern* k, VarPre* pre) {
  // calculate var offset and update fnLocals offset.
  pre->var->meta |= TY_VAR_LOCAL;
  pre->var->v = align(k->g.fnLocals, alignment(pre->sz));
  k->g.fnLocals = pre->var->v + (pre->els * pre->sz);
}

static inline bool isSelf(TyI* tyI) { return (TyBase*)&Ty_Self == tyI->ty; }

void checkSelf(TyI* arg, bool expectFirst) {
  ASSERT(arg or not expectFirst, "Self must be the first argument");
  for(; arg; arg=arg->next) {
    if(expectFirst and not arg->next) {
      ASSERT(isSelf(arg) and (1 == TyI_refs(arg)),
         "&Self must be the first argument");
    }
    else if(isSelf(arg)) ASSERT(TyI_refs(arg), "Self must be a reference");
  }
}

TyDict* currentSelf(Kern* k) {
  DictStk* m = &k->g.modStk;
  for(U2 i = m->sp; i < m->cap; i++) {
    if(isTyDict((Ty*)m->dat[i])) return m->dat[i];
  }
  return NULL;
}

TyI* replaceSelf(Kern* k, TyI* tyI, TyDict* self) {
  checkSelf(tyI, false);
  if(isSelf(tyI)) {
    TyI n = *tyI; n.ty = (TyBase*) self;
    return TyI_findOrAdd(k, &n);
  }
  return tyI;
}

void N_inp(Kern* k) {
  N_notImm(k);
  ASSERT(IS_FN_STATE(FN_STATE_STK) or IS_FN_STATE(FN_STATE_INP)
         , "inp used after out");
  VarPre pre = varPre(k);
  SET_FN_STATE(FN_STATE_INP);
  TyI_rootAdd(k, &tyFn(k->g.curTy)->inp, pre.var->tyI);
  pre.var->tyI = replaceSelf(k, pre.var->tyI, currentSelf(k));
  localImpl(k, &pre);
}
TyFn TyFn_inp = TyFn_native("\x03" "inp", TY_FN_SYN, (U1*)N_inp, TYI_VOID, TYI_VOID);

void _globalInit(Kern* k, TyVar* v) {
  memset((void*)v->v, 0, TyI_sz(v->tyI));
  SrOffset st = (SrOffset) {
    .op = SRGL, .checkTy = true, .asImm = true, .global = v,
  };
  START_IMM(true); srOffset(k, v->tyI, /*offset*/ 0, &st); END_IMM;
}

void _varLocal(Kern* k, VarPre* pre) {
  localImpl(k, pre);
  if(CONSUME("=")) {
    SrOffset st = (SrOffset) {.op = SRLL, .checkTy = true };
    srOffset(k, pre->var->tyI, /*offset=*/pre->var->v, &st);
  }
}

void N_var(Kern* k) {
  N_notImm(k); ASSERT(not IS_FN_STATE(FN_STATE_NO), "var must be within a function");
  VarPre pre = varPre(k);
  _varLocal(k, &pre);
}

void N_global(Kern* k) {
  N_notImm(k); VarPre pre = varPre(k);
  TyVar* v = pre.var; v->meta |= TY_VAR_GLOBAL;
  v->v = (S) BBA_alloc(&k->bbaCode, pre.els * pre.sz, /*align*/pre.sz);
  ASSERT(v->v, "Global OOM");
  if(CONSUME("=")) {
    _globalInit(k, v);
  }
}

void N_alias(Kern* k) {
  N_notImm(k);
  CStr* key = tokenCStr(k);
  TyVar* v = (TyVar*) Ty_new(k, TY_VAR | TY_VAR_ALIAS, key);
  REQUIRE(":"); v->v = (S) scanTy(k);
}

void fnSignature(Kern* k, TyFn* fn) {
  SET_FN_STATE(FN_STATE_STK);
  REQUIRE("[");
  while(true) {
    if(CONSUME("]")) break;
    if(CONSUME("->")) SET_FN_STATE(FN_STATE_OUT);
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "expected 'do' but reached EOF");
    WS_ADD(/*asImm=*/ false); // all of these are syn functions
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)
       and not Slc_eq(SLC("&"), CStr_asSlc(ty->name))) {
      tokenDrop(k); executeFn(k, (TyFn*)ty);
    } else if (IS_FN_STATE(FN_STATE_OUT)) N_stk(k);
    else                                  N_inp(k);
  }
}

// Walk input and store locals and push stack types
void fnInputs(Kern* k, TyFn* fn) {
  TyDb* db = tyDb(k, false);
  for(TyI* tyI = fn->inp; tyI; tyI = tyI->next) {
    Key key = { CStr_asSlcMaybe(tyI->name) };
    TyVar* v = (TyVar*)Kern_findTy(k, &key);
    if(v and isTyVar((Ty*)v) and not isVarGlobal(v)) {
      SrOffset st = (SrOffset) { .op = SRLL, .checkTy = false, .notParse = true };
      srOffset(k, tyI, v->v, &st);
    } else if(/*isStk and*/ not IS_UNTY) {
      TyI_cloneAddNode(db->bba, TyDb_root(db), tyI);
    }
  }
}

TyFn* parseFn(Kern* k) {
  U2 meta = k->g.metaNext;
  // Create TyFn based on NAME
  TyFn* fn = (TyFn*) Ty_new(k, TY_FN | meta, NULL);
  Ty* prevTy = k->g.curTy; k->g.curTy = (Ty*) fn;

  DictStk_add(&k->g.dictStk, (TyDict*) fn); // local variables
  fnSignature(k, fn);

  Buf* code = &k->g.code;  Buf prevCode = Kern_reserveCode(k, FN_ALLOC);
  const U2 db_startLen = Stk_len(&k->g.tyDb.done);
  TyDb_new(&k->g.tyDb);
  LOCAL_TYDB_BBA(tyDb); TyDb* db = tyDb(k, false);
  if(CONSUME(";")) {}
  else {
    REQUIRE("do"); fnInputs(k, fn);
    SET_FN_STATE(FN_STATE_BODY); Kern_compFn(k); // compile the fn body
  }
  SET_FN_STATE(FN_STATE_NO);

  // Force a RET at the end, whether UNTY or not.
  if( (not IS_UNTY and (RET_DONE != TyDb_done(db)))
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
  return fn;
}

// fn NAME do (... code ...)
// future:
// fn ... types ... do ( ... code ... )
void N_fn(Kern* k) { N_notImm(k); parseFn(k); }

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
//   aka a branch happend but control continued.
//      TyDb -> stateToUse -> fullIfState -> outer
//
//   hadFull=false: there have been no "full" if/elif/else branches, all had RET
//   or some other escape.
//      TyDb -> stateToUse -> outer
//
// When hadFull, any block is immediately merged with fullIfState since they
// have to match. Otherwise, any block that does not have RET sets hadFull=true.

typedef struct {
  bool hadFull;
  HowDone done;
  HowDone hadElse; /*rename elseDone*/
} IfState;

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
  if(IS_UNTY) return is;
  TyDb* db = tyDb(k, false); HowDone done = TyDb_done(db);
  if(is.hadFull) {
    is.done = is.done ? S_min(is.done, done) : done;
    tyMerge(k, db);    // check stateToUse==fullIfState
    tyClone(k, db, 1); // clone outer state
  } else if (TyDb_done(db)) {
    is.done = is.done ? S_min(is.done, done) : done;
    tyMerge(k, db);    // drop stateToUse
    tyClone(k, db, 0); // clone outer statet
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
    is = _N_if(k, is); // look for next elif or else
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
  TyDb_drop(k, db); // drop "stateToUse"
  if(is.hadFull)       TyDb_nip(k, db); // drop "outer", TyDb -> fullIfState
  else if (is.hadElse and is.done) TyDb_setDone(db, is.done);
}

// imm#if implementation.
//
// We evaluate conditional tokens. If false, we treat the `do` token as a
// comment. Once a conditional has been found, all other tokens are treated as
// comments.
void _ifImm(Kern* k) {
  TyDb* db = tyDb(k, true); bool wasElse = false; bool branched = false;
  while(true) {
    if(branched) {
      if(not wasElse) { _N_fslash(k);   REQUIRE("do"); }
      _N_fslash(k);
    } else if (wasElse) Kern_compFn(k);
    else {
      Kern_compFn(k); REQUIRE("do"); tyCall(k, db, &TyIs_S, NULL);
      if(WS_POP()) { Kern_compFn(k); branched = true; }
      else           _N_fslash(k);
    }
    if(wasElse) break;
    else if(CONSUME("elif")) {}
    else if(CONSUME("else")) wasElse = true;
    else break;
  }
}


void N_if(Kern* k) {
  if(WS_POP()) return _ifImm(k);
  TyDb* db = tyDb(k, false);
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
  TyDb_setDone(db, BLK_DONE);
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
  TyDb_setDone(db, BLK_DONE);
}

void N_break(Kern* k) {
  N_notImm(k); Kern_compFn(k);
  tyBreak(k, tyDb(k, false));
  Buf* b = &k->g.code;

  Buf_add(b, SZ2 | JL); // unconditional jump to end of block
  Sll* br = BBA_alloc(k->g.bbaDict, sizeof(Sll), RSIZE); ASSERT(br, "break OOM");
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
  if(TyDb_done(db) < RET_DONE) TyDb_clearDone(db);
  for(Sll* br = blk->breaks; br; br = br->next) {
    srBE2(b->dat + br->dat, b->len - br->dat);
  }
}

// ***********************
//   * struct
//
// struct Foo [ a: U2; b: &U4 ]


void checkSelfIo(InOut* io, bool expectFirst) {
  checkSelf(io->inp, expectFirst); checkSelf(io->out, false);
}

void _field(Kern* k, TyDict* st, VarPre pre) {
  TyVar* v = pre.var;
  v->v = align(st->sz, alignment(pre.sz));
  TyI_rootAdd(k, &st->fields, v->tyI);
  if(TyI_unsized(pre.var->tyI)) {
    st->sz = v->v + pre.sz;
  } else {
    assert(TY_UNSIZED != pre.els);
    st->sz = v->v + (pre.els * pre.sz);
  }
}

void field(Kern* k, TyDict* st) {
  _field(k, st, varPre(k));
}

void _parseDataTy(Kern* k, TyDict* st) {
  N_notImm(k);
  Ty* prevTy = k->g.curTy;        k->g.curTy = (Ty*) st;
  DictStk_add(&k->g.dictStk, st); DictStk_add(&k->g.modStk, st);
  REQUIRE("["); bool hasUnsized = false;
  while(not CONSUME("]")) {
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "Expected ']', reached EOF");
    ASSERT(not hasUnsized, "Detected field after unsized field.");
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)) {
      tokenDrop(k); WS_ADD(/*asImm*/false); executeFn(k, (TyFn*)ty);
    } else {
      field(k, st);
      hasUnsized |= TyI_unsized(st->fields);
      if(Slc_eq(SUPER.name, Slc_frCStr(st->fields->name))) {
        ASSERT(not st->fields->next, "super must be the first field");
        ASSERT(not TyI_refs(st->fields), "can not be super of reference");
      }
    }
  }
  DictStk_pop(&k->g.dictStk); DictStk_pop(&k->g.modStk);
  if(hasUnsized) st->sz = TY_UNSIZED;
  k->g.curTy = prevTy;
}

void N_struct(Kern* k) {
  TyDict* st = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_STRUCT, NULL);
  _parseDataTy(k, st);
}

void N_meth(Kern* k) {
  N_notImm(k);
  TyDict* mod = DictStk_topMaybe(&k->g.modStk);
  ASSERT(isTyDict((Ty*)mod) && isDictStruct(mod), "'meth' can only be in struct");
  k->g.metaNext |= TY_FN_METH;
  TyFn* meth = parseFn(k);
  checkSelfIo(TyFn_asInOut(meth), false); // FIXME: require self
}

// ***********************
//   * role
// role Resource [ meth drop(&Self) do; ]

void N_absmeth(Kern* k) {
  TyDict* role = DictStk_topMaybe(&k->g.modStk);
  ASSERT(isTyDict((Ty*)role) && isDictRole(role), "'absmeth' can only be in role");
  N_notImm(k);
  TyFn* fn = (TyFn*) Ty_new(k, TY_FN | TY_FN_ABSMETH, NULL);
  fn->code = (U1*)align(role->sz, RSIZE); // offset in Role method struct
  FnSig* sig = parseFnSig(k); checkSelfIo(&sig->io, true);
  fn->inp = sig->io.inp; fn->out = sig->io.out;
  TyVar* v = (TyVar*) Ty_newTyKey(k, TY_VAR | TY_VAR_LOCAL, fn->name, &TyIs_RoleField);
  v->tyI = TyI_findOrAdd(k, &(TyI){ .ty = (TyBase*) sig, .meta = 1 });
  _field(k, role, (VarPre){ .var = v, .sz = RSIZE, .els = 1 });
}

void N_role(Kern* k) {
  TyDict* role = (TyDict*) Ty_new(k, TY_DICT | TY_DICT_ROLE, NULL);
  _parseDataTy(k, role);
  for(TyI* field = role->fields; field; field = field->next) {
    ASSERT(isFnSig(field->ty), "Only absmeth data fields allowed for role.");
  }
}

// impl Struct:Role { add = &Struct.add }
void N_impl(Kern* k) {
  N_notImm(k);
  TyDict* st = (TyDict*) scanTy(k);
  ASSERT(st and isTyDict((Ty*)st), "Can only implement for TyDict");
  ASSERT(isDictStruct(st)        , "Can only implement for struct");
  REQUIRE(":"); TyDict* role = (TyDict*) scanTy(k);
  ASSERT(role and isTyDict((Ty*)role) and isDictRole(role),
         "must be impl Type:Role");

  // Create a variable for storing the methods
  TyI* roleTyI = TyI_findOrAdd(k, &(TyI){ .ty = (TyBase*)role });

  DictStk_add(&k->g.dictStk, st);
  TyVar* v = (TyVar*) Ty_newTyKey(k, TY_VAR | TY_VAR_GLOBAL, role->name, roleTyI);
  DictStk_pop(&k->g.dictStk);
  VarPre pre = { .var = v, .sz = TyI_sz(roleTyI), .els = 1 };
  v->v = (S) BBA_alloc(&k->bbaCode, pre.sz, /*align*/RSIZE);
  ASSERT(v->v, "Role impl OOM");
  v->tyI = roleTyI;
  S* storeLoc = (S*)v->v;

  REQUIRE("{"); while(not CONSUME("}")) {
    TyVar* field = tyVar(TyDict_scanTyKey(k, role, &TyIs_RoleField));
    FnSig* sig = (FnSig*) field->tyI->ty; assert(isFnSig((TyBase*)sig));
    REQUIRE("="); START_IMM(true); Kern_compFn(k); END_IMM;
    sig = FnSig_replaceSelf(k, sig, st);
    tyCall(k, tyDb(k, true), &(TyI){.ty = (TyBase*)sig, .meta = 1}, NULL);
    storeLoc[field->v / RSIZE] = WS_POP();
  }
}


// ***********************
//   * '.', '&', '@'

void N_dot(Kern* k) { // A reference is on the stack, get a (sub)field
  bool asImm = WS_POP(); TyDb* db = tyDb(k, asImm);
  ASSERT(not IS_UNTY, "Cannot use '.' on stack references without type checking");
  TyI* top = TyDb_top(db);
  U2 refs = TyI_refs(top);
  ASSERT(refs, "invalid '.', the value on the stack is not a reference");
  TyDict* d = (TyDict*) top->ty; assert(d);
  ASSERT(isTyDict((Ty*)d), "invalid '.', the value on the stack is not a TyDict");
  ASSERT(not isDictMod(d), "accessing mod through ref");

  Buf* b = &k->g.code;
  // Trim the refs to be only 1
  while(refs > 1) { Buf_add(b, FT | SZR); }
  TyDb_pop(k, db);
  Ty* ty = TyDict_scanTy(k, d); ASSERT(ty, "member not found");
  if(isTyVar(ty)) {
    TyVar* var = (TyVar*) ty;
    // Note: "global" field is not a member.
    ASSERT(not isVarGlobal(var), "invalid '.', accessing global through ref");
    FtOffset st = (FtOffset) {.op = FTO, .findEqual = true, .asImm = asImm};
    ftOffset(k, var->tyI, var->v, &st);
    return;
  }
  // Call the method
  TyI this = (TyI) { .ty = (TyBase*)d, .meta = /*refs*/1 };
  tyCall(k, db, NULL, &this); // put back on ty stack (from TyDb_pop above)
  compileTy(k, ty, asImm);
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
      Key key = (Key) { tokenSlc(k) };
      TyVar* var = tyVar(TyDict_find(tyDictB(var->tyI->ty), &key));
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
  bool asImm = WS_POP(); TyDb* db = tyDb(k, asImm);
  scan(k); Ty* ty = Kern_findToken(k); ASSERT(ty, "not found");
  TyDict* d = NULL;
  if (isTyDict(ty)) {
    d = (TyDict*)ty; tokenDrop(k);
    while(CONSUME(".")) {
      ty = TyDict_scanTy(k, d); ASSERT(ty, "expected struct member");
      if(isTyDict(ty)) d = (TyDict*)ty;
      else             break;
    }
    if(isTyVar(ty)) ASSERT(isVarGlobal((TyVar*)ty), "direct Struct field reference");
    else if(isTyFn(ty)) {}
    else ASSERT(false, "invalid struct sub-type");
  }
  if(isTyVar(ty)) {}
  else if (isTyFn(ty)) {
    FnSig* sig = FnSig_findOrAdd(k, *TyFn_asInOut((TyFn*)ty));
    if(d) {
      InOut io = sig->io;            InOut_replaceSelf(k, &io, d);
      sig = FnSig_findOrAdd(k, io);  InOut_free(k, &io);
    }
    tyCall(k, db, NULL, &(TyI){.ty = (TyBase*)sig, .meta = 1 });
    tokenDrop(k);
    // TODO: not a lit... a global?
    if(asImm) WS_ADD((S)ty);
    else      lit(&k->g.code, (S)ty);
    return;
  } else SET_ERR(SLC("'&' can only get ref of variable or typecast"));
  tokenDrop(k);
  TyVar* var = (TyVar*) ty;
  TyVar* global = NULL; S offset = var->v;
  if(isVarGlobal(var)) {
    global = var; offset = 0;
  }
  else ASSERT(not asImm, "local referenced imm");
  TyI* tyI = var->tyI;
  while(not TyI_refs(tyI) and CONSUME(".")) {
    var = (TyVar*) TyDict_scanTy(k, tyDictB(var->tyI->ty));
    ASSERT(var, "field not found");
    assert(isTyVar((Ty*)var)); // TODO: support function
    offset += var->v; tyI = var->tyI;
  }
  Buf* b = asImm ? NULL : &k->g.code;
  if(PEEK(".")) {
    assert(false); // implemented but never tested
    // assert(TyI_refs(tyI));
    // op2(b, FTLL, SZR, offset);
    // return ampRef(k, tyI);
  }
  ASSERT(TyI_refs(tyI) + 1 <= TY_REFS, "refs too large");
  TyI out = (TyI) { .ty = tyI->ty, .meta = tyI->meta + 1 };
  tyCall(k, db, NULL, &out);
  if(asImm) {
    assert(global); WS_ADD(global->v + offset); // note: reference
  }
  else opCompile(b, global ? GR : LR, /*szI*/0, offset, global);
}

bool handleLocalFn(Kern *k) { // return true if handled
  // Using @localFn requires that we compile the variable
  // AFTER the arguments. N_at would compile it before arguments.
  Key key = (Key) { tokenSlc(k) }; Ty* ty = Kern_findTy(k, &key);
  if(not isTyVar(ty)) { return false; } TyVar* v = (TyVar*)ty;
  if((1 != TyI_refs(v->tyI)) or not isFnSig(v->tyI->ty))
    return false;
  tokenDrop(k); Kern_compFn(k); // compile arguments
  FnSig* fn = (FnSig*) v->tyI->ty;
  tyCall(k, tyDb(k, false), fn->io.inp, fn->io.out);
  compileVarFt(k, v, false, /*addTy*/false);
  Buf_add(&k->g.code, XW);
  return true;
}

void N_at(Kern* k) { // '@', aka dereference
  N_notImm(k); TyDb* db = tyDb(k, false);
  ASSERT(not IS_UNTY, "Cannot use '@' without type checking");
  if(handleLocalFn(k)) return;
  Kern_compFn(k);  U1 instr = FT;
  if(CONSUME("="))    instr = SR;
  TyI* tyI = TyDb_top(db); U2 refs = TyI_refs(tyI);
  if(not refs) {
    eprintf("!!! Stack: "); TyDb_print(k, db); NL;
    SET_ERR(SLC("invalid '@', the value on the stack is not a reference"));
  }
  if(SR == instr) Kern_compFn(k); // compile value to store
  if(refs > 1) Buf_add(&k->g.code, instr | SZR);
  else {
    ASSERT(isTyDictB(tyI->ty), "Cannot direct ft/sr non-dict type");
    TyDict* d = (TyDict*) tyI->ty;
    ASSERT(not isDictMod(d), "Cannot @mod");
    if(isDictNative(d)) Buf_add(&k->g.code, instr | (SZ_MASK & (S)d->children));
    else assert(false); // struct not implemented
  }
  TyI inp = (TyI) { .ty = (TyBase*)tyI->ty, .meta = tyI->meta };
  if(instr == SR) {
    TyI inp2 = *TyDb_top(db); inp2.next = &inp;
    tyCall(k, db, &inp2, NULL);
  } else {
    TyI out = (TyI) { .ty = (TyBase*)tyI->ty, .meta = tyI->meta - 1};
    tyCall(k, db, &inp, &out);
  }
}

void N_ptrAddRaw(Kern* k) { // ptr:S index:S cap:S sz:S
  S sz = WS_POP(); WS_POP3(S ptr, S i, S bound);
  ASSERT(i < bound, "ptrAdd OOB");
  WS_ADD(ptr + (i * sz));
}
TyFn TyFn_ptrAddRaw = {
  .name = (CStr*) ("\x09" "ptrAddRaw"), .meta = TY_FN | TY_FN_NATIVE,
  .code = (U1*)N_ptrAddRaw, .inp = &TyIs_Unsafe, .out = &TyIs_Unsafe
};

void N_ptrAdd(Kern* k) { // ptrAdd(ptr, index, cap)
  bool asImm = WS_POP(); TyDb* db = tyDb(k, asImm);
  ASSERT(not IS_UNTY, "ptrAdd requires type checking");
  Kern_compFn(k);
  TyI* tyI = TyDb_top(db);
  if(not tyI || not tyI->next || not tyI->next->next) {
    // invalid type
    tyCall(k, db, &TyIs_rAnySS, &TyIs_rAny); assert(false);
  }
  TyI* ptrTy = tyI->next->next; U1 refs = TyI_refs(ptrTy);
  ASSERT(refs, "ptrAdd requires the ptr to be a reference");
  ASSERT(refs > 1 || not TyI_unsized(ptrTy), "ptrAdd cannot work on unsized types");
  TyI inp[3]; inp[0] = (TyI) {.ty = ptrTy->ty, .meta = ptrTy->meta };
              inp[1] = (TyI) {.ty = (TyBase*)&Ty_S, .next = &inp[0]};
              inp[2] = (TyI) {.ty = (TyBase*)&Ty_S, .next = &inp[1]};
  tyCall(k, db, &inp[2], &inp[0]);
  TyI derefPtr = {.ty = ptrTy->ty, .meta = ptrTy->meta - 1};
  lit(&k->g.code, TyI_sz(&derefPtr));
  Kern_typed(k, false);
  compileFn(k, &TyFn_ptrAddRaw, NULL, asImm);
  Kern_typed(k, true);
}

typedef struct { U1 c; bool unknownEsc; } CharNextEsc;
CharNextEsc charNextEsc(Kern* k, SpReader f) {
  Ring* r = &SpReader_asBase(k, f)->ring;
  U1* c = SpReader_get(k, f, 0);
  ASSERT(c, "expected character, got EOF");
  SrcRing_incHead(k, r, 1);
  if('\\' != *c) return (CharNextEsc) { .c = *c };

  c = SpReader_get(k, f, 0);
  ASSERT(c, "expected character after '\\', got EOF");
  SrcRing_incHead(k, r, 1);
  if('t' == *c)      return (CharNextEsc) { .c = '\t' };
  if('n' == *c)      return (CharNextEsc) { .c = '\n' };
  if(' ' == *c)      return (CharNextEsc) { .c = ' '  };
  if('x' == *c) {
    c = SpReader_get(k, f, 1); ASSERT(c, "expected two characters after '\\'");
    U1 hex = (Ring_get(r, 0) << 8) | Ring_get(r, 1);
    SrcRing_incHead(k, r, 2);
    return (CharNextEsc) { .c = hex };
  }
  return (CharNextEsc) { .c = *c, .unknownEsc = true };
}

void N_char(Kern* k) {
  bool asImm = WS_POP(); REQUIRE(":");
  CharNextEsc c = charNextEsc(k, k->g.src);
  ASSERT(not c.unknownEsc, "Unknown escape in char");
  compileLit(k, c.c, asImm);
}

Buf SpBuf_new(Kern* k, SpArena a, U2 cap) {
  Sp_Xr2(a,alloc, cap, 1);
  return (Buf) { .dat = (U1*) WS_POP(), .cap = cap, };
}

Slc* SpBuf_freeEnd(Kern* k, Buf* b, SpArena a) {
  U2 sz = b->cap - b->len;  if(not sz) return NULL;
  Sp_Xr3(a,free, (S)b->dat + b->len, sz, 1);
  Slc* out = (Slc*) WS_POP();
  if(not out) b->cap = b->len;
  return out;
}

#define STRING_MAX 512
Slc parseSlcU1(Kern* k) {
  Buf b = SpBuf_new(k, *ARENA_TOP, STRING_MAX);
  bool ignoringWhite = true;
  CharNextEsc c;
  while(true) {
    c = charNextEsc(k, k->g.src);
    if(c.c == '|') {
      if(c.unknownEsc) {}
      else             break; // '|' -> done
    }
    else if(c.c == ' ') {
      if(c.unknownEsc) ignoringWhite = false;
      else if(ignoringWhite) continue; // don't add to buffer
    } else if (c.unknownEsc) {
      SET_ERR(SLC("Unknown escaped character"));
    } else if (c.c == '\n') ignoringWhite = true;
    else                    ignoringWhite = false;
    Buf_add(&b, c.c);
  }
  SpBuf_freeEnd(k, &b, *ARENA_TOP);
  return *Buf_asSlc(&b);
}

TyI TyI_SlcU1;
void N_pipe(Kern* k) { // SlcU1 literal, aka |this is a string|
  bool asImm = WS_POP();
  Slc s = parseSlcU1(k);
  if(asImm) { WS_ADD2((S)s.dat, s.len); }
  else      {
    Sp_Xr2(*ARENA_TOP,alloc, sizeof(OwnedValue), RSIZE);
    OwnedValue* owned = (OwnedValue*) WS_POP();
    Sp_Xr2(*ARENA_TOP,alloc, sizeof(Ownership),  RSIZE);
    Ownership*  ship  = (Ownership*) WS_POP();
    ASSERT(owned, "String OOM"); ASSERT(ship, "String OOM");
    *owned = (OwnedValue) { .ref = s.dat, .ty = &Ty_U1, .ownership = ship };
    *ship = (Ownership) { .offset = 0, .len = s.len };

    Buf* b = &k->g.code; Buf_add(b, OWR); Buf_addBE4(b, (S)owned);
    lit(&k->g.code, s.len);
  }
  tyCall(k, tyDb(k, asImm), NULL, &TyI_SlcU1);
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
  Key key = { popSlc(k) };
  WS_ADD((S)Kern_findTy(k, &key));
}

// ********
//   * Role definitions for spor
// In civc, role methods are a struct of pointers to functions.
// In spor, role methods are a struct of pointers to TyFns.

// The actual method globals are defined in Kern_fns()
/*extern*/ MSpArena  mSpArena_BBA      = {};
/*extern*/ MSpReader mSpReader_UFile   = {};
/*extern*/ MSpReader mSpReader_BufFile = {};

// this:&This -> ()
void N_BBA_drop(Kern* k) { BBA* bba = (BBA*)WS_POP(); BBA_drop(bba); }

// this:&This -> S
void N_BBA_maxAlloc(Kern* k) { BBA* bba = (BBA*)WS_POP(); WS_ADD(BBA_maxAlloc(bba)); }

// this:&This sz:S alignment:U2 -> Ref
void N_BBA_alloc(Kern* k) {
  WS_POP2(S sz, U2 alignment); BBA* bba = (BBA*)WS_POP();
  WS_ADD((S)BBA_alloc(bba, sz, alignment));
}

// this:&This dat:Ref sz:S alignment:U2 -> Ref
void N_BBA_free(Kern* k) {
  WS_POP3(S dat, S sz, U2 alignment); BBA* bba = (BBA*)WS_POP();
  WS_ADD((S)BBA_free(bba, (void*)dat, sz, alignment));
}

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



// ***********************
// * 7: Registering Functions

#define ADD_ANY_VAR(VAR, TY, NAMELEN, NAME, META, VNAME, V) \
  CStr_ntVar(LINED(key), NAMELEN, NAME);\
  VAR = (TY) {          \
    .name = LINED(key),              \
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
  SUPER = (Key) { .name = SLC("super") };

  // Native data types
  ADD_TY_NATIVE(Ty_UNSET, "\x08", "Ty_UNSET",  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_Any,   "\x03", "Any"     ,  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_Unsafe,"\x06", "Unsafe"  ,  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_Self,  "\x04", "Self"    ,  0      , (Ty*)(SZR + 1));
  ADD_TY_NATIVE(Ty_RoleField, "\x09", "RoleField", 0  , (Ty*)(SZR + 1));


  TyIs_UNSET  = (TyI) { .ty = (TyBase*)&Ty_UNSET  };
  TyIs_Unsafe = (TyI) { .ty = (TyBase*)&Ty_Unsafe };
  TyIs_rSelf  = (TyI) { .ty = (TyBase*)&Ty_Self, .meta = 1 };
  TyIs_RoleField = (TyI) { .ty = (TyBase*)&Ty_RoleMeth };
  TyIs_rAny   = (TyI) { .ty = (TyBase*)&Ty_Any, .meta = 1 };
  TyIs_rAnyS  = (TyI) { .ty = (TyBase*)&Ty_S, .next = &TyIs_rAny };
  TyIs_rAnySS = (TyI) { .ty = (TyBase*)&Ty_S, .next = &TyIs_rAnyS };

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
  TyIs_S = (TyI) { .ty = (TyBase*)&Ty_S };
  // Ty: S, s
  TyIs_SS = (TyI) { .next = &TyIs_S, .ty = (TyBase*)&Ty_S };
  // Ty: S, s, S
  TyIs_SSS = (TyI) { .next = &TyIs_SS, .ty = (TyBase*)&Ty_S };

  TyIs_U1 = (TyI) { .ty = (TyBase*)&Ty_U1 };
  TyIs_U2 = (TyI) { .ty = (TyBase*)&Ty_U2 };
  TyIs_U4 = (TyI) { .ty = (TyBase*)&Ty_U4 };
  TyIs_U4x2 = (TyI) { .next = &TyIs_U4, .ty = (TyBase*)&Ty_U4 };

  TyIs_rU1 = (TyI) { .ty = (TyBase*)&Ty_U1, .meta = 1 };
  TyIs_rU2 = (TyI) { .ty = (TyBase*)&Ty_U2, .meta = 1 };
  TyIs_rU4 = (TyI) { .ty = (TyBase*)&Ty_U4, .meta = 1 };

  TyIs_rU1_U4 = (TyI) {.ty = (TyBase*)&Ty_U4, .next = &TyIs_rU4};

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
  ADD_FN("\x04", "cast"         , TY_FN_SYN       , N_cast     , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "mod"          , TY_FN_SYN       , N_mod      , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "use"          , TY_FN_SYN       , N_use      , TYI_VOID, TYI_VOID);
  ADD_FN("\x07", "fileloc"      , TY_FN_SYN       , N_fileloc  , TYI_VOID, TYI_VOID);
  ADD_FN("\x02", "fn"           , TY_FN_SYN       , N_fn       , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "meth"         , TY_FN_SYN       , N_meth     , TYI_VOID, TYI_VOID);
  ADD_FN("\x07", "absmeth"      , TY_FN_SYN       , N_absmeth  , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "fnMeta"       , TY_FN_SYN       , N_fnMeta   , TYI_VOID, TYI_VOID);
  ADD_FN("\x05", "fnSig"        , TY_FN_SYNTY     , N_fnSig    , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "var"          , TY_FN_SYN       , N_var      , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "global"       , TY_FN_SYN       , N_global   , TYI_VOID, TYI_VOID);
  ADD_FN("\x05", "alias"        , TY_FN_SYN       , N_alias    , TYI_VOID, TYI_VOID);
  ADD_FN("\x02", "if"           , TY_FN_SYN       , N_if       , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "cont"         , TY_FN_SYN       , N_cont     , TYI_VOID, TYI_VOID);
  ADD_FN("\x05", "break"        , TY_FN_SYN       , N_break    , TYI_VOID, TYI_VOID);
  ADD_FN("\x03", "blk"          , TY_FN_SYN       , N_blk      , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "struct"       , TY_FN_SYN       , N_struct   , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "role"         , TY_FN_SYN       , N_role     , TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "impl"         , TY_FN_SYN       , N_impl     , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "."            , TY_FN_SYN       , N_dot      , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "&"            , TY_FN_SYN       , N_amp      , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "@"            , TY_FN_SYN       , N_at       , TYI_VOID, TYI_VOID);
  ADD_FN("\x06", "ptrAdd"       , TY_FN_SYN       , N_ptrAdd   , TYI_VOID, TYI_VOID);
  ADD_FN("\x08", "destruct"     , TY_FN_SYN       , N_destruct , TYI_VOID, TYI_VOID);
  ADD_FN("\x05", "dbgRs"        , 0               , N_dbgRs    , TYI_VOID, TYI_VOID);
  ADD_FN("\x09", "tAssertEq"    , 0               , N_tAssertEq, &TyIs_SS, TYI_VOID);
  ADD_FN("\x0D", "assertWsEmpty", 0           , N_assertWsEmpty, TYI_VOID, TYI_VOID);
  ADD_FN("\x04", "char"         , TY_FN_SYN       , N_char     , TYI_VOID, TYI_VOID);
  ADD_FN("\x01", "|"            , TY_FN_SYN       , N_pipe     , TYI_VOID, TYI_VOID);
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

#define FN_OVERRIDE(TY_FN, C_FN) _fnOverride(tyFn(TY_FN), C_FN)
TyFn* _fnOverride(TyFn* fn, void (*cFn)(Kern*)) {
    fn->meta |= TY_FN_NATIVE;
    fn->code = (U1*) cFn;
    return fn;
}

void Dat_mod(Kern* k) {
  REPL_START
  COMPILE_EXEC("struct SlcU1 [ dat:&U1  len:U2 ]");
  TyI_SlcU1 = (TyI) { .ty = (TyBase*) Kern_findTy(k, &KEY("SlcU1")) }; assert(TyI_SlcU1.ty);
  REPL_END

  CStr_ntVar(path, "\x0A", "src/dat.fn"); compilePath(k, path);

  TyDict* tyBBA = tyDict(Kern_findTy(k, &KEY("BBA")));

  mSpArena_BBA = (MSpArena) {
    .drop     = FN_OVERRIDE(TyDict_find(tyBBA, &KEY("drop")),     &N_BBA_drop),
    .alloc    = FN_OVERRIDE(TyDict_find(tyBBA, &KEY("alloc")),    &N_BBA_alloc),
    .free     = FN_OVERRIDE(TyDict_find(tyBBA, &KEY("free")),     &N_BBA_free),
    .maxAlloc = FN_OVERRIDE(TyDict_find(tyBBA, &KEY("maxAlloc")), &N_BBA_maxAlloc),
  };
}


// ***********************
// * 8: Execution helpers

void Kern_handleSig(Kern* k, int sig, struct sigcontext* ctx) {
  eprintf("!!! fngi return stack:\n");
  N_dbgRs(k);
  eprintf("!!! Code: token=\"%.*s\" line=%u\n", Dat_fmt(k->g.token), k->g.srcInfo->line + 1);
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
