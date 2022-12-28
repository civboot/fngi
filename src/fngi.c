
#include "./fngi.h"

#define R0        return 0;
#define Ty_fmt(TY)    CStr_fmt((TY)->bst.key)
#define TyFn_fmt(TY)  CStr_fmt((TY)->bst.key)

void dbgWs(Kern *k) {
  Stk* ws = WS; U2 cap = ws->cap; U2 len = cap - ws->sp;
  eprintf("{");
  for(U2 i = 1; i <= len; i++) {
    U4 v = ws->dat[cap - i];
    if(v <= 0xFFFF)   eprintf(" %+8X", v);
    else              eprintf(" %+4X_%X", v >> 16, 0xFFFF & v);
  }
  eprintf("}\n");
}

// ***********************
// * 1: Initialization

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

CStr* tokenCStr(Kern* k) {
  scan(k);
  CStr* out = CStr_new(BBA_asArena(k->g.bbaDict), *Buf_asSlc(&k->g.token));
  tokenDrop(k);
  return out;
}

// Allocate, initialize and add to dict a new Ty from the information in Kern.
// This includes the token as the fn name.
//
// It is the caller's job to initialize: bst.l, bst.r, ty, v.
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
  eprintf("?? Ty_new: %.*s\n", Dat_fmt(k->g.token));
  *ty = (Ty) {
    .bst = (Bst) { .key = key },
    .meta = meta,
    .line = k->g.tokenLine,
    .file = k->g.srcInfo,
  };
  Kern_addTy(k, ty);
  return ty;
}

// ***********************
// * 2: Executing Instructions
// Fngi's assembly is defined in src/constants.zty. These constants are
// auto-generated into src/gen/constants.h
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
  eprintf("?? xImpl ep=%X fn=%X isNative=%u\n", cfb->ep, fn, isFnNative(fn));
  if(isFnNative(fn)) return executeNative(k, fn);
  ASSERT(RS->sp >= fn->lSlots + 1, "execute: return stack overflow");
  INFO_ADD((S)fn);
  RS_ADD((S)cfb->ep);
  cfb->ep = (U1*)ty->v;
  RS->sp -= fn->lSlots; // grow locals (and possibly defer)
}

TyFn catchTy;
void ret(Kern* k) {
  TyFn* ty = (TyFn*) INFO_POP();
  U1 lSlots = (ty == &catchTy) ? 0 : ty->lSlots;
  RS->sp += lSlots;
  cfb->ep = (U1*)RS_POP();
  eprintf("?? ret: ep=%X lSlots=%u\n", cfb->ep, lSlots);
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
  Slc name = instrName(instr);
  eprintf("??? instr: %+10.*s: ", Dat_fmt(name)); dbgWs(k);
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
    case LR: WS_ADD((S)&RS->dat[RS->sp] + popLit(k, 1)); R0

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

    case SZ1 + FTLL: WS_ADD(*(U1*) ((U1*)RS->dat + popLit(k, 1))); R0
    case SZ2 + FTLL: WS_ADD(*(U2*) ((U1*)RS->dat + popLit(k, 1))); R0
    case SZ4 + FTLL: WS_ADD(*(U4*) ((U1*)RS->dat + popLit(k, 1))); R0

    case SZ1 + SR: WS_POP2(l, r); *(U1*)r = l; R0
    case SZ2 + SR: WS_POP2(l, r); *(U2*)r = l; R0
    case SZ4 + SR: WS_POP2(l, r); *(U4*)r = l; R0

    case SZ1 + SRBE: WS_POP2(l, r); srBE((U1*)r, 1, l); R0
    case SZ2 + SRBE: WS_POP2(l, r); srBE((U1*)r, 2, l); R0
    case SZ4 + SRBE: WS_POP2(l, r); srBE((U1*)r, 4, l); R0

    case SZ1 + SRO: r = WS_POP(); *(U1*)(r + popLit(k, 1)) = WS_POP(); R0
    case SZ2 + SRO: r = WS_POP(); *(U2*)(r + popLit(k, 1)) = WS_POP(); R0
    case SZ4 + SRO: r = WS_POP(); *(U4*)(r + popLit(k, 1)) = WS_POP(); R0

    case SZ1 + SRLL: *(U1*) ((U1*)RS->dat + popLit(k, 1)) = WS_POP(); R0
    case SZ2 + SRLL: *(U2*) ((U1*)RS->dat + popLit(k, 1)) = WS_POP(); R0
    case SZ4 + SRLL: *(U4*) ((U1*)RS->dat + popLit(k, 1)) = WS_POP(); R0

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
  Stk* info = &cfb->info;
  eprintf("?? handler start info=%X\n", info);
  PanicHandler h = {.i = info->sp, .rs = RS->sp};
  while(h.i < info->cap) {
    eprintf("?? handler i=%u\n", h.i);
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
  eprintf("?? catchpanic start\n");
  U2 rsDepth = Stk_len(RS);
  eprintf("?? here\n");
  PanicHandler h = getPanicHandler(k);
  if(not h.found) return false;
  eprintf("?? Catching panic\n");
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
  eprintf("?? start executeLoop\n");
  while(cfb) {
    if(setjmp(local_errJmp)) { // got panic
      eprintf("?? PANIC\n");
      if(!catchPanic(k)) {
        eprintf("?? not catching panic line=%u\n", k->g.srcInfo->line);
        Slc path = CStr_asSlcMaybe(k->g.srcInfo->path);
        eprintf("!! Uncaught panic: %.*s[%u]\n", Dat_fmt(path), k->g.srcInfo->line);
        longjmp(*prev_errJmp, 1);
      }
    }
    U1 res = executeInstr(k, popLit(k, 1));
    if(res) {
      if(YLD == res) yield();
      else /* RET */ {
        eprintf("?? here RSlen=%u\n", Stk_len(RS));
        if(0 == Stk_len(RS)) {  // empty stack, fiber done
          cfb->ep = NULL;
          break;
        } else ret(k);
      }
    }
  }
  eprintf("?? end executeLoop\n");
  civ.fb->errJmp = prev_errJmp;
}

void executeFn(Kern* k, TyFn* fn) {
  if(isFnNative(fn)) return executeNative(k, fn);
  cfb->ep = (U1*)fn->v;
  executeLoop(k);
}


// #################################
// # Ty checker

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

void TyDb_print(Kern* k) { TyI_printAll(TyDb_top(&k->g.tyDb)); }

void TyDb_cloneAddNode(Kern* k, TyI* node) {
  Sll_add((Sll**)TyDb_root(&k->g.tyDb), TyI_asSll(TyI_cloneNode(node, k->g.bbaTy)));
}

// Clone from bottom to top, pushing onto root.
// The result will be in the same order.
// This is done so that freeing is done from top to bottom.
void  TyDb_cloneAdd(Kern* k, TyI* nodes) {
  if(not nodes) return;
  TyDb_cloneAdd(k, nodes->next);
  TyDb_cloneAddNode(k, nodes);
}

void TyDb_free(Kern* k, TyI* stream) {
  TyDb* db = &k->g.tyDb;
  TyI** root = TyDb_root(db);
  while(stream) {
    TyI* next = stream->next; // cache since we may free the node
    BBA_free(k->g.bbaTy, Sll_pop((Sll**)root), sizeof(TyI), RSIZE);
    stream = next;
  }
}

void TyDb_drop(Kern* k) {
  TyDb* db = &k->g.tyDb;
  TyDb_free(k, TyDb_top(db));
  Stk_pop(&db->tyIs);
  Stk_pop(&db->done);
}

#define IS_UNTY  (C_UNTY & k->g.fnState)

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
      eprintf("!! Detected stack underflow, need additional types:\n");
      tyNotMatch(require_, given_);
      SET_ERR(errCxt);
    }
    if(not TyI_eq(require, given)) {
      eprintf("!! Given %u Types from right doesn't match Require\n", i);
      tyNotMatch(require_, given_);
      SET_ERR(errCxt);
    }
    given = given->next; require = require->next; i += 1;
  }
  if(sameLen and given) {
    eprintf("!! Type lengths differ:\n"); tyNotMatch(require_, given_);
    SET_ERR(errCxt);
  }
}

// Call a function or equivalent, checking and popping inp; then pushing the out.
void tyCall(Kern* k, TyI* inp, TyI* out) {
  if(IS_UNTY) return;
  eprintf("?? tyCall done=%u\n", TyDb_done(k));
  ASSERT(not TyDb_done(k), "Code after guaranteed 'ret'");
  eprintf("?? tyCall 1\n");
  TyI** root = TyDb_root(&k->g.tyDb);

  tyCheck(inp, *root, false, SLC("Type error during operation (i.e. function call, struct, etc)"));
  TyDb_free(k, inp);
  TyDb_cloneAdd(k, out);
}

// Check function return type and possibly mark as done.
void tyRet(Kern* k, bool done) {
  if(IS_UNTY) return;
  eprintf("?? tyRet\n");
  TyDb* db = &k->g.tyDb;
  ASSERT(not TyDb_done(k), "Code after guaranteed 'ret'");
  tyCheck(tyFn(k->g.curTy)->out, TyDb_top(db), true, SLC("Type error during 'ret'"));
  TyDb_setDone(db, done);
  eprintf("?? tyRet end\n");
}

// Clone the current snapshot and use clone for continued mutations.
void tyClone(Kern* k) {
  if(IS_UNTY) return;
  TyDb* db = &k->g.tyDb;
  TyI* stream = TyDb_top(db);
  TyDb_new(&k->g.tyDb, NULL);
  TyDb_cloneAdd(k, stream);
}

void tyMerge(Kern* k) {
  if(IS_UNTY) return;
  TyDb* db = &k->g.tyDb;
  ASSERT(Stk_len(&db->tyIs) > 1, "tyMerge with 1 or fewer snapshots");
  TyI* top = TyDb_top(db);
  TyI* scnd = (TyI*) &db->tyIs.dat[db->tyIs.sp + 1];
  tyCheck(scnd, top, true, SLC("Type error during merge (i.e. if/else)"));
  TyDb_drop(k);
}


// ***********************
// * 3: Token scanner

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
void scan(Kern* k);
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

static inline bool tokenEq(Kern* k, Slc s) {
  return Slc_eq(s, *Buf_asSlc(&k->g.token));
}

#define CONSUME(T)  tokenConsume(k, SLC(T))
#define REQUIRE(T)  ASSERT(CONSUME(T), "Expected: '" T "'")

// consume token if it equals slc
bool tokenConsume(Kern* k, Slc s) {
  scan(k); bool out = tokenEq(k, s);
  if(out) tokenDrop(k);
  return out;
}

void tokenDrop(Kern* k) {
  Buf* b = &k->g.token;
  eprintf("?? tokenDrop: %.*s\n", Dat_fmt(*b));
  Ring_incHead(&Xr(k->g.src, asBase)->ring, b->len);
  Buf_clear(b);
}

bool Kern_eof(Kern* k) { return Reader_eof(k->g.src); }

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

// #################################
// # Compiler

void lit(Buf* b, U4 v) {
  if (v <= SLIT_MAX)    { Buf_add(b, SLIT | v); }
  else if (v <= 0xFF)   { Buf_add(b, SZ1 | LIT); Buf_add(b, v); }
  else if (v <= 0xFFFF) { Buf_add(b, SZ2 | LIT); Buf_addBE2(b, v); }
  else                  { Buf_add(b, SZ4 | LIT); Buf_addBE4(b, v); }
}

void compileLit(Kern* k, U4 v, bool asNow) {
  eprintf("?? compilingLit: %X asNow=%b\n", v, asNow);
  if(asNow) return WS_ADD(v);
  tyCall(k, NULL, &TyIs_S);
  lit(&k->g.code, v);
}

void compileFn(Kern* k, TyFn* fn) {
  eprintf("?? compileFn: %.*s\n", Ty_fmt((Ty*)fn));
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

static inline void Kern_compFn(Kern* k) { executeFn(k, k->g.compFn); }

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

void ftLocal(Kern* k, TyI* tyI, U2 offset) {
  tyCall(k, NULL, tyI);
  Buf* b = &k->g.code;
  if(TyI_refs(tyI)) return op1(b, FTLL, SZR, offset);
  ASSERT(not isTyFn(tyI->ty), "invalid fn local");
  TyDict* d = (TyDict*) tyI->ty;
  if(isDictNative(d)) return op1(b, FTLL, d->v, offset);
  assert(false); // struct / etc
}

void srLocal(Kern* k, TyI* tyI, U2 offset, bool checkTy) {
  if(checkTy) tyCall(k, tyI, NULL);
  Buf* b = &k->g.code;
  if(TyI_refs(tyI)) return op1(b, SRLL, SZR, offset);
  ASSERT(not isTyFn(tyI->ty), "invalid fn local");
  TyDict* d = (TyDict*) tyI->ty;
  if(isDictNative(d)) return op1(b, SRLL, d->v, offset);
  assert(false); // struct / etc
}

void compileVar(Kern* k, TyVar* v, bool asNow) {
  if(isVarGlobal(v)) assert(false);
  ASSERT(not asNow, "'$' used with local variable");
  if(CONSUME("=")) srLocal(k, v->tyI, /*offset=*/v->v, true);
  else             ftLocal(k, v->tyI, /*offset=*/v->v);
}

void compileTy(Kern* k, Ty* ty, bool asNow) {
  ASSERT(ty, "name not found");
  eprintf("?? compileTy %.*s\n", Ty_fmt(ty));
  if(not IS_UNTY and Stk_len(&k->g.tyDb.done)) {
    eprintf("?? TyIs:"); TyDb_print(k); eprintf("\n");
  }
  if(isTyConst(ty)) return compileLit(k, ty->v, asNow);
  if(isTyVar(ty))   return compileVar(k, (TyVar*) ty, asNow);
  TyFn* fn = tyFn(ty);
  if(isFnPre(fn))  {
    eprintf("?? is Pre\n");
    Kern_compFn(k);
    if(not IS_UNTY and Stk_len(&k->g.tyDb.done)) {
      eprintf("?? TyIs after pre:"); TyDb_print(k); eprintf("\n");
    }
  }

  if(isFnSyn(fn)) {
    WS_ADD(asNow);
    return executeFn(k, fn);
  }
  if(isFnNow(fn)) { ASSERT(asNow, "function must be called with '$'"); }
  if(asNow) executeFn(k, fn);
  else      compileFn(k, fn);
}

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
    scan(k);
    if     (tokenEq(k, SLC("&"))) s.refs += 1;
    else if(tokenEq(k, SLC("@"))) s.derefs += 1;
    else break;
    tokenDrop(k);
  }
  ASSERT(not s.derefs,           "invalid deref on type spec");
  ASSERT(s.refs <= TY_REFS, "number of '&' must be <= 3");

  s.ty = scanTy(k); ASSERT(s.ty, "Type not found");
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

// Compile a single token.
void single(Kern* k, bool asNow) {
  scan(k); Slc t = *Buf_asSlc(&k->g.token);
  if(not t.len) return;
  ParsedNumber n = parseU4(t);
  eprintf("Compiling: %.*s isNum=%b\n", Dat_fmt(t), n.isNum);
  if(n.isNum) {
    tokenDrop(k);
    return compileLit(k, n.v, asNow);
  }
  Ty* ty = scanTy(k); ASSERT(ty, "name not found");
  compileTy(k, ty, asNow);
}

void compileSrc(Kern* k) {
  while(true) {
    Kern_compFn(k);
    if(Kern_eof(k)) break;
  }
}

// #################################
// # Native Type Streams

TYIS(/*extern*/)

// #################################
// # Native Functions
// These functions are implemented as native C code and can be executed by fngi.

// #################################
//   # Utility

static inline S szIToSz(U1 szI) {
  switch(SZ_MASK & szI) {
    case SZ1: return 1;
    case SZ2: return 2;
    case SZ4: return 4;
  }
  assert(false);
}

S TyDict_size(TyDict* ty) {
  if(isDictNative(ty)) return szIToSz(ty->v);
  else                 return ty->sz;
}

S TyI_size(TyI tyI) {
  if(TyI_refs(&tyI)) return RSIZE;
  ASSERT(isTyDict(tyI.ty), "TyI must have refs or be a dict.");
  return TyDict_size((TyDict*)tyI.ty);
}

// #################################
//   # Misc


void N_notNow(Kern* k) { ASSERT(not WS_POP(), "cannot be executed with '%'"); }
void N_unty(Kern* k) {
  WS_POP(); // ignore asNow
  k->g.fnState |= C_UNTY;
  Kern_compFn(k);
  k->g.fnState &= ~C_UNTY;
}

void N_ret(Kern* k) {
  N_notNow(k);
  tyRet(k, true);
  Buf_add(&k->g.code, RET);
}
void N_dropWs(Kern* k) { Stk_clear(WS); }
void N_tAssertEq(Kern* k) { WS_POP2(U4 l, U4 r); TASSERT_EQ(l, r); }

// #################################
//   # Core syn functions

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

// #################################
//   # fn and fn signature

void N_pre(Kern* k)     {
  N_notNow(k); k->g.metaNext |= TY_FN_PRE; }
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

// TyVar* varImpl(Kern* k, TyI* tyI, CStr* key, S ptr) {
void varImpl(Kern* k, TyVar* var, S ptr) {
  if(ptr) var->v = ptr;
  else { // uses fn locals offset
    S sz = TyI_size(*var->tyI);
    var->v = align(k->g.fnLocals, alignment(sz));
    k->g.fnLocals = var->v + sz;
  }
}

// Used for both stk and out
void N_stk(Kern *k) {
  eprintf("?? N_stk\n");
  N_notNow(k);
  Sll** root;
  if(     IS_FN_STATE(FN_STATE_STK)) root = TyFn_inpRoot(tyFn(k->g.curTy));
  else if(IS_FN_STATE(FN_STATE_OUT)) root = TyFn_outRoot(tyFn(k->g.curTy));
  else {
    printf("?? fnState=%X\n", k->g.fnState);
    SET_ERR(SLC("stk used after local variable inputs or in body"));
  }
  CONSUME(":");
  Sll_add(root, TyI_asSll(scanTyI(k)));
}
TyFn TyFn_stk = TyFn_native("\x03" "stk", TY_FN_SYN, (S)N_stk, TYI_VOID, TYI_VOID);

TyVar* varPre(Kern* k) {
  Ty* found = scanTy(k);
  CStr* key; if(found) key = found->bst.key;
             else      key = tokenCStr(k);
  REQUIRE(":");
  TyVar* var = (TyVar*) Ty_new(k, TY_VAR, key);
  TyI* tyI = scanTyI(k);
  tyI->name = key;
  var->tyI = tyI;
  return var;
}

void N_inp(Kern* k) {
  N_notNow(k);
  TyVar* var = varPre(k);
  ASSERT(IS_FN_STATE(FN_STATE_STK) or IS_FN_STATE(FN_STATE_INP)
         , "inp used after out");
  SET_FN_STATE(FN_STATE_INP);
  TyI* tyI = TyI_cloneNode(var->tyI, k->g.bbaDict);
  Sll_add(TyFn_inpRoot(tyFn(k->g.curTy)), TyI_asSll(tyI));
  varImpl(k, var, /*ptr=*/0);
}
TyFn TyFn_inp = TyFn_native("\x03" "inp", TY_FN_SYN, (S)N_inp, TYI_VOID, TYI_VOID);

void fnSignature(Kern* k, TyFn* fn, Buf* b/*code*/) {
  SET_FN_STATE(FN_STATE_STK);
  while(true) {
    if(CONSUME("do")) break;
    if(CONSUME("->")) SET_FN_STATE(FN_STATE_OUT);
    Ty* ty = Kern_findToken(k);
    ASSERT(not Kern_eof(k), "expected 'do' but reached EOF");
    WS_ADD(/*asNow=*/ false); // all of these are syn functions
    if(ty and isTyFn(ty) and isFnSyn((TyFn*)ty)) {
      tokenDrop(k); executeFn(k, (TyFn*)ty);
    } else if (IS_FN_STATE(FN_STATE_OUT)) N_stk(k);
    else                                  N_inp(k);
  }

  for(TyI* tyI = fn->inp; tyI; tyI = tyI->next) {
    eprintf("?? sig inp: "); TyI_print(tyI); eprintf("\n");
    TyVar* v = (TyVar*)Kern_findTy(k, CStr_asSlcMaybe(tyI->name));
    if(v and isTyVar((Ty*)v) and not isVarGlobal(v)) srLocal(k, tyI, v->v, false);
    else if(/*isStk and*/ not IS_UNTY)               TyDb_cloneAddNode(k, tyI);
  }

  if(fn->inp) fn->meta |= TY_FN_PRE;
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
  eprintf("?? N_fn: %.*s\n", TyFn_fmt(fn));

  Buf* code = &k->g.code;  Buf prevCode = *code;
  *code = Buf_new(BBA_asArena(&k->bbaCode), FN_ALLOC);
  ASSERT(code->dat, "Code OOM");

  const U2 db_startLen = Stk_len(&k->g.tyDb.done);
  TyDb_new(&k->g.tyDb, NULL);
  // Locals
  Stk_add(&k->g.dictStk, 0);
  LOCAL_BBA(bbaDict); LOCAL_BBA(bbaTy);
  // BBA localBBA = (BBA) { &civ.ba }; BBA* prevBBA = k->g.bbaDict; k->g.bbaDict = &localBBA;

  fnSignature(k, fn, code);
  SET_FN_STATE(FN_STATE_BODY); Kern_compFn(k); // compile the fn body
  SET_FN_STATE(FN_STATE_NO);

  // Force a RET at the end, whether UNTY or not.
  if( (not IS_UNTY and not TyDb_done(k))
      or  (IS_UNTY and (RET != code->dat[code->len-1]))) {
    WS_ADD(/*asNow=*/false); N_ret(k); // force RET at end.
  }

  TyDb_drop(k);
  ASSERT(db_startLen == Stk_len(&k->g.tyDb.done),
         "A type operation (i.e. if/while/etc) is incomplete in fn");

  // Free unused area of buffers
  BBA_free(&k->bbaCode, code->dat + code->len, code->cap - code->len, 1);

  fn->v = (S)code->dat; fn->len = code->len;
  fn->lSlots = align(k->g.fnLocals, RSIZE) / RSIZE;
  k->g.fnLocals = 0; k->g.metaNext = 0;
  *code = prevCode;
  END_LOCAL_BBA(bbaDict); END_LOCAL_BBA(bbaTy);
  Stk_pop(&k->g.dictStk);
}


// #################################
// # Registering Functions

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

  TyIs_U1_rU1 = (TyI) {.next = &TyIs_rU1, .ty = &Ty_U1 };
  TyIs_U2_rU2 = (TyI) {.next = &TyIs_rU2, .ty = &Ty_U2 };
  TyIs_U4_rU4 = (TyI) {.next = &TyIs_rU4, .ty = &Ty_U4 };

  Kern_addTy(k, (Ty*) &TyFn_baseCompFn);
  Kern_addTy(k, (Ty*) &TyFn_stk);
  Kern_addTy(k, (Ty*) &TyFn_inp);

  // Pure noop syntax sugar
  STATIC_INLINE(TYI_VOID, TYI_VOID, "\x01", "_"    , 0       , /*no code*/);
  STATIC_INLINE(TYI_VOID, TYI_VOID, "\x01", ";"    , 0       , /*no code*/);
  STATIC_INLINE(TYI_VOID, TYI_VOID, "\x01", ","    , 0       , /*no code*/);
  STATIC_INLINE(TYI_VOID, TYI_VOID, "\x02", "->"   , 0       , /*no code*/);


  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x06", "notNow",  TY_FN_SYN, N_notNow);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x04", "unty",    TY_FN_SYN, N_unty);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "ret",     TY_FN_SYN, N_ret);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "$",       TY_FN_SYN, N_dollar);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "(",       TY_FN_SYN, N_paren);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x01", "\\",      TY_FN_COMMENT, N_fslash);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "pre",     TY_FN_SYN, N_pre);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "now",     TY_FN_SYN, N_now);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x03", "syn",     TY_FN_SYN, N_syn);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x06", "inline",  TY_FN_SYN, N_inline);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x07", "comment", TY_FN_COMMENT, N_comment);
  STATIC_NATIVE(TYI_VOID, TYI_VOID, "\x02", "fn",      TY_FN_SYN, N_fn);


  STATIC_NATIVE(&TyIs_SS, &TyIs_S, "\x09", "tAssertEq",  TY_FN_PRE, N_tAssertEq);

  // Stack operators. These are **not** PRE since they directly modify the stack.
  STATIC_INLINE(&TyIs_SS, &TyIs_SS,  "\x03", "swp"  , 0       , SWP   );
  STATIC_INLINE(&TyIs_S,  TYI_VOID,  "\x03", "drp"  , 0       , DRP   );
  STATIC_INLINE(&TyIs_SS, &TyIs_SSS, "\x03", "ovr"  , 0       , OVR   );
  STATIC_INLINE(&TyIs_S,  &TyIs_SS,  "\x03", "dup"  , 0       , DUP   );
  STATIC_INLINE(&TyIs_S,  &TyIs_SS,  "\x04", "dupn" , 0       , DUPN  );

  // Standard operators that use PRE syntax. Either "a <op> b" or simply "<op> b"
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "nop"  , TY_FN_PRE, NOP  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "inc"  , TY_FN_PRE, INC  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x04", "inc2" , TY_FN_PRE, INC2 );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x04", "inc4" , TY_FN_PRE, INC4 );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "dec"  , TY_FN_PRE, DEC  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "inv"  , TY_FN_PRE, INV  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "neg"  , TY_FN_PRE, NEG  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x03", "not"  , TY_FN_PRE, NOT  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x05", "i1to4", TY_FN_PRE, CI1  );
  STATIC_INLINE(&TyIs_S,  &TyIs_S, "\x05", "i2to4", TY_FN_PRE, CI2  );

  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "+"    , TY_FN_PRE, ADD  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "-"    , TY_FN_PRE, SUB  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "%"    , TY_FN_PRE, MOD  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "shl"  , TY_FN_PRE, SHL  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "shr"  , TY_FN_PRE, SHR  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "msk"  , TY_FN_PRE, MSK  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "jn"   , TY_FN_PRE, JN   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "xor"  , TY_FN_PRE, XOR  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x03", "and"  , TY_FN_PRE, AND  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "or"   , TY_FN_PRE, OR   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "=="   , TY_FN_PRE, EQ   );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", "!="   , TY_FN_PRE, NEQ  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x02", ">="   , TY_FN_PRE, GE_U );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "<"    , TY_FN_PRE, LT_U );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x04", "ge_s" , TY_FN_PRE, GE_S );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x04", "lt_s" , TY_FN_PRE, LT_S );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "*"    , TY_FN_PRE, MUL  );
  STATIC_INLINE(&TyIs_SS, &TyIs_S, "\x01", "/"    , TY_FN_PRE, DIV_U);

  // ftN(addr): fetch a value of sz N from address.
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft1"  , TY_FN_PRE, SZ1+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft2"  , TY_FN_PRE, SZ2+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ft4"  , TY_FN_PRE, SZ4+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x03", "ftR"  , TY_FN_PRE, SZR+FT   );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe1", TY_FN_PRE, SZ1+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe2", TY_FN_PRE, SZ2+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBe4", TY_FN_PRE, SZ4+FTBE );
  STATIC_INLINE(&TyIs_S, &TyIs_S, "\x05", "ftBeR", TY_FN_PRE, SZR+FTBE );

  // srN(value, addr): store a value of sz N to address.
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x03", "sr1"  , TY_FN_PRE, SZ1+SR   );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x03", "sr2"  , TY_FN_PRE, SZ2+SR   );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x03", "sr4"  , TY_FN_PRE, SZ4+SR   );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x03", "srR"  , TY_FN_PRE, SZR+SR   );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x05", "srBe1", TY_FN_PRE, SZ1+SRBE );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x05", "srBe2", TY_FN_PRE, SZ2+SRBE );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x05", "srBe4", TY_FN_PRE, SZ4+SRBE );
  STATIC_INLINE(&TyIs_SS, TYI_VOID, "\x05", "srBeR", TY_FN_PRE, SZR+SRBE );
  TASSERT_EMPTY();
}

// Test helpers

void executeInstrs(Kern* k, U1* instrs) {
  cfb->ep = instrs;
  executeLoop(k);
}


CStr_ntLitUnchecked(replPath, "\x08", "/repl.fn");
FileInfo replInfo = {0};

U1* compileRepl(Kern* k, bool withRet) {
  LOCAL_BBA(bbaTy);
  replInfo.path = replPath;
  k->g.srcInfo = &replInfo;
  U1* body = (U1*) BBA_alloc(&k->bbaRepl, 256, 1);
  ASSERT(body, "compileRepl OOM");
  Buf* code = &k->g.code; *code = (Buf){.dat=body, .cap=256};
  compileSrc(k);
  if(withRet) Buf_add(code, RET);
  if(code->len < code->cap) {
    BBA_free(
      &k->bbaRepl,
      /*dat*/code->dat + code->len,
      /*sz*/code->cap - code->len,
      /*alignment*/1);
  }
  *code = (Buf){0};
  END_LOCAL_BBA(bbaTy);
  return body;
}

void simpleRepl(Kern* k) {
  size_t cap;  jmp_buf local_errJmp;  jmp_buf* prev_errJmp = civ.fb->errJmp;
  Ring_var(_r, 256);  BufFile f = BufFile_init(_r, (Buf){0});
  k->g.src = File_asReader(BufFile_asFile(&f));

  civ.fb->errJmp = &local_errJmp;
  eprintf(  "Simple REPL: type EXIT to exit\n");
  U2 rsSp = RS->sp; U2 infoSp = cfb->info.sp;
  while(true) {
    if(setjmp(local_errJmp)) { // got panic
      eprintf("!! Caught panic, WS: "); dbgWs(k);
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
  }
  civ.fb->errJmp = prev_errJmp;
}
