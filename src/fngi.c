
#include "./fngi.h"

#define R0        return 0;
#define Ty_fmt(TY)    CStr_fmt((TY)->bst.key)
#define TyFn_fmt(TY)  CStr_fmt((TY)->d.bst.key)

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
  .d.bst.key = (CStr*) ("\x0A" "baseCompFn"),
  .d.meta = TY_FN | TY_FN_NATIVE,
  .d.v = (Slot)baseCompFn,
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
    }, 0
  };
  assert(0 == k->g.dictBuf[DICT_DEPTH - 1]);
  Stk_add(&k->g.dictStk, 0);
}

// Allocate and initialize a new Ty from the information in Kern.
// This includes the token as the fn name.
//
// It is the caller's job to initialize: bst.l, bst.r, ty, v.
// It is also the caller's job to handle TyFn and TyDict values.
Ty* Ty_new(Kern* k, U2 meta) {
  U1 sz = sizeof(Ty);
  switch (TY_MASK & meta) {
    case TY_FN:   sz = sizeof(TyFn);   break;
    case TY_DICT: sz = sizeof(TyDict); break;
  }
  Ty* ty = (Ty*) BBA_alloc(&k->bbaDict, sz, 4);
  eprintf("?? Ty_new: %.*s\n", Dat_fmt(k->g.token));
  *ty = (Ty) {
    .bst = (Bst) {
      .key = CStr_new(BBA_asArena(&k->bbaDict), *Buf_asSlc(&k->g.token)),
    },
    .meta = meta,
    .line = k->g.tokenLine,
    .file = k->g.srcInfo,
  };
  ASSERT(ty->bst.key, "Ty_new key OOM");
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
  eprintf("?? native\n");
  ((void(*)(Kern*)) fn->d.v)(k);
}

void xImpl(Kern* k, Ty* ty) {
  TyFn* fn = tyFn(ty);
  if(isFnNative(fn)) return executeNative(k, fn);
  eprintf("?? non-native: %X\n", ty->v);
  ASSERT(RS->sp >= fn->lSlots + 1, "execute: return stack overflow");
  INFO_ADD((Slot)fn);
  RS_ADD((Slot)cfb->ep);
  cfb->ep = (U1*)ty->v;
  RS->sp -= fn->lSlots; // grow locals (and possibly defer)
}

TyFn catchTy;
void ret(Kern* k) {
  TyFn* ty = (TyFn*) INFO_POP();
  U1 lSlots = (ty == &catchTy) ? 0 : ty->lSlots;
  RS->sp += lSlots;
  cfb->ep = (U1*)RS_POP();
  eprintf("ret: ep=%X lSlots=%u\n", cfb->ep, lSlots);
}

void jmpImpl(Kern* k, void* ty) {
  TyFn* fn = tyFn(ty);
  ASSERT(0 == fn->lSlots, "jmp to fn with locals");
  cfb->ep = (U1*)fn->d.v;
}

void slcImpl(Kern* k, U1 sz) {
  Slot len = popLit(k, sz);
  WS_ADD((Slot)cfb->ep); WS_ADD(len); // {dat, len}
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
    case DUPN: r = WS_POP(); WS_ADD(r); WS_ADD(0 == r); R0
    case LR: WS_ADD((Slot)&RS->dat[RS->sp] + popLit(k, 1)); R0

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

    case SZ1 + SLC: slcImpl(k, 1); R0;
    case SZ2 + SLC: slcImpl(k, 2); R0;
    case SZ4 + SLC: slcImpl(k, 4); R0;

    default: if(instr >= SLIT) { WS_ADD(0x3F & instr); R0 }
  }
  SET_ERR(Slc_ntLit("Unknown instr"));
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
  while(cfb) {
    if(setjmp(local_errJmp)) { // got panic
      if(!catchPanic(k)) longjmp(*prev_errJmp, 1);
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
  // eprintf("?? executing fn: %.*s\n", Ty_fmt((Ty*)fn));
  if(isFnNative(fn)) return executeNative(k, fn);
  cfb->ep = (U1*)fn->d.v;
  executeLoop(k);
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

void skipWhitespace(Reader f) {
  Ring* r = &Xr(f, asBase)->ring;
  while(true) {
    U1* c = Reader_get(f, 0);
    if(c == NULL) return;
    if(*c > ' ')  return;
    Ring_incHead(r, 1);
  }
}

// Guarantees a token is in Kern.g.token
void scan(Kern* k);
void scanRaw(Kern* k) {
  Reader f = k->g.src; Buf* b = &k->g.token;
  if(b->len) return; // does nothing if token wasn't cleared.
  skipWhitespace(f);
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

#define REQUIRE(T)  ASSERT(tRequire(k, Slc_ntLit(T)), "Expected: " T)
bool tRequire(Kern* k, Slc s) {
  scan(k); bool out = tokenEq(k, s);
  tokenDrop(k);
  return out;
}

void tokenDrop(Kern* k) {
  Buf* b = &k->g.token;
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
    if(Slc_eq(Slc_ntLit("0b"), s)) { base = 2;  i = 2; }
    if(Slc_eq(Slc_ntLit("0x"), s)) { base = 16; i = 2; }
    if(Slc_eq(Slc_ntLit("0c"), s)) { assert(false); } // character
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
  lit(&k->g.code, v);
}

void compileFn(Kern* k, TyFn* fn) {
  eprintf("?? compileFn: %.*s\n", Ty_fmt((Ty*)fn));
  Buf* b = &k->g.code;
  if(isFnInline(fn)) { // inline: len before function code.
    return Buf_extend(b, (Slc){(U1*)fn->d.v, .len=fn->len});
  }
  Buf_add(b, XL); Buf_addBE4(b, (U4)fn);
}

void single(Kern* k, bool asNow);
void baseCompFn(Kern* k) {
  if(Kern_eof(k)) return;
  single(k, false);
}

static inline void Kern_compFn(Kern* k) { executeFn(k, k->g.compFn); }

void compileTy(Kern* k, Ty* ty, bool asNow) {
  ASSERT(ty, "name not found");
  if(isTyConst(ty)) return compileLit(k, ty->v, asNow);
  if(isTyLocal(ty)) assert(false); // TODO
  TyFn* fn = tyFn(ty);
  if(isFnPre(fn))  {
    eprintf("  ?? is Pre\n");
    Kern_compFn(k);
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
  eprintf("?? addty: %.*s\n", Ty_fmt(ty));
  ty->bst.l = NULL; ty->bst.r = NULL;
  Stk* dicts = &k->g.dictStk;
  ASSERT(dicts->sp < dicts->cap, "No dicts");
  Bst** root = (Bst**) &dicts->dat[dicts->sp];
  ty = (Ty*)Bst_add(root, (Bst*)ty);
  ASSERT(not ty, "key was overwritten");
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
  Ty* ty = Kern_findTy(k, t);       ASSERT(ty, "token not found");
  tokenDrop(k); compileTy(k, ty, asNow);
}

void compileSrc(Kern* k) {
  while(true) {
    Kern_compFn(k);
    if(Kern_eof(k)) break;
  }
}

// #################################
// # Base functions

void N_notNow(Kern* k) { ASSERT(not WS_POP(), "cannot be executed with '%'"); }

void _N_dollar(Kern* k);
TyFn _TyFn_N_dollar = TyFn_native("\x01" "$", 0, (Slot)_N_dollar);
void _N_dollar(Kern* k) {
  if(Kern_eof(k)) return;
  single(k, true);
}

void N_dollar(Kern*k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_N_dollar;
  executeFn(k, &_TyFn_N_dollar);
  k->g.compFn = cfn;
}

void N_paren(Kern* k) {
  WS_POP(); // ignore asNow
  while(true) {
    scan(k); if(tokenEq(k, Slc_ntLit(")"))) break;
    ASSERT(not Kern_eof(k), "expected ')' but reached EOF");
    Kern_compFn(k);
  }
  tokenDrop(k); // drop ')'
}

void _N_fslash(Kern* k);
TyFn _TyFn_N_fslash = TyFn_native("\x01" "$", 0, (Slot)_N_fslash);
void _N_fslash(Kern* k) {
  TyFn* cfn = k->g.compFn; k->g.compFn = &_TyFn_N_fslash;
  scanRaw(k);
  // Execute next token if it is '(', else drop it.
  if(tokenEq(k, Slc_ntLit("("))) single(k, true);
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

void N_pre(Kern* k)     {
  eprintf("?? in N_pre\n");
  N_notNow(k); k->g.metaNext |= TY_FN_PRE; }
void N_now(Kern* k)     { _fnMetaNext(k,   TY_FN_NOW); }
void N_syn(Kern* k)     { _fnMetaNext(k,   TY_FN_SYN); }
void N_inline(Kern* k)  { _fnMetaNext(k,   TY_FN_INLINE); }
void N_comment(Kern* k) { _fnMetaNext(k,   TY_FN_COMMENT); }

// fn NAME do (... code ...)
// future:
// fn ... types ... do ( ... code ... )
void N_fn(Kern* k) {
  N_notNow(k);
  U2 meta = k->g.metaNext;
  // Create TyFn based on NAME
  scan(k); TyFn* fn = (TyFn*) Ty_new(k, TY_FN | meta);
  eprintf("?? N_fn: %.*s\n", TyFn_fmt(fn));
  Buf* code = &k->g.code;  Buf prevCode = *code;
  *code = Buf_new(BBA_asArena(&k->bbaCode), FN_ALLOC);
  ASSERT(code->dat, "Code OOM");
  tokenDrop(k);
  //       do    (... code ... )
  REQUIRE("do"); Kern_compFn(k);

  if(RET != code->dat[code->len-1]) Buf_add(code, RET); // force RET at end.
  // Free unused area of buffer
  BBA_free(&k->bbaCode, code->dat + code->len, code->cap - code->len, 1);
  fn->d.v = (Slot)code->dat; fn->len = code->len;
  Kern_addTy(k, (Ty*)fn);
  k->g.metaNext = 0;  *code = prevCode;
}

// Create a static TyFn and add it to the kern.
// NAMELEN: the fngi name's length; NAME: the fngi name
// META: the meta to use;  NAT: the native value to add
#define STATIC_FNTY(NAMELEN, NAME, META, NAT) \
  CStr_ntVar(LINED(key), NAMELEN, NAME);\
  static TyFn LINED(Ty);                \
  LINED(Ty) = (TyFn) {                  \
    .d = {                              \
      .bst.key = LINED(key),            \
      .meta = TY_FN | (META),           \
      .v = NAT,                         \
    }                                   \
  };                                    \
  Kern_addTy(k, (Ty*)&LINED(Ty));

#define STATIC_NATIVE(NAMELEN, NAME, META, KFN) \
    STATIC_FNTY(NAMELEN, NAME, TY_FN_NATIVE | (META), kFn(KFN))

#define STATIC_INLINE(NAMELEN, NAME, META, ...)               \
  assert(sizeof((U1[]){__VA_ARGS__}) < 0xFF);                 \
  static U1 LINED(code)[] = {__VA_ARGS__ __VA_OPT__(,) RET};  \
  STATIC_FNTY(NAMELEN, NAME, TY_FN_INLINE | (META), (Slot)LINED(code)); \
  LINED(Ty).len = sizeof(LINED(code)) - 1;


void Kern_fns(Kern* k) {
  TASSERT_EMPTY();
  Kern_addTy(k, (Ty*) &TyFn_baseCompFn);

  // Pure noop syntax sugar
  STATIC_INLINE("\x01", "_"    , 0       , /*no code*/);
  STATIC_INLINE("\x01", ";"    , 0       , /*no code*/);
  STATIC_INLINE("\x02", "->"   , 0       , /*no code*/);

  STATIC_NATIVE("\x01", "\\",      TY_FN_COMMENT, N_fslash);

  STATIC_NATIVE("\x01", "$",       TY_FN_SYN, N_dollar);
  STATIC_NATIVE("\x01", "(",       TY_FN_SYN, N_paren);
  STATIC_NATIVE("\x06", "notNow",  TY_FN_SYN, N_notNow);
  STATIC_NATIVE("\x03", "pre",     TY_FN_SYN, N_pre);
  STATIC_NATIVE("\x03", "now",     TY_FN_SYN, N_now);
  STATIC_NATIVE("\x03", "syn",     TY_FN_SYN, N_syn);
  STATIC_NATIVE("\x06", "inline",  TY_FN_SYN, N_inline);
  STATIC_NATIVE("\x07", "comment", TY_FN_COMMENT, N_comment);
  STATIC_NATIVE("\x02", "fn",      TY_FN_SYN, N_fn);

  // Stack operators. These are **not** PRE since they directly modify the stack.
  STATIC_INLINE("\x03", "swp"  , 0       , SWP   );
  STATIC_INLINE("\x03", "drp"  , 0       , DRP   );
  STATIC_INLINE("\x03", "ovr"  , 0       , OVR   );
  STATIC_INLINE("\x03", "dup"  , 0       , DUP   );
  STATIC_INLINE("\x04", "dupn" , 0       , DUPN  );

  // Standard operators that use PRE syntax. Either "a <op> b" or simply "<op> b"
  STATIC_INLINE("\x03", "nop"  , TY_FN_PRE, NOP  );
  STATIC_INLINE("\x03", "ret"  , TY_FN_PRE, RET  );
  STATIC_INLINE("\x03", "inc"  , TY_FN_PRE, INC  );
  STATIC_INLINE("\x04", "inc2" , TY_FN_PRE, INC2 );
  STATIC_INLINE("\x04", "inc4" , TY_FN_PRE, INC4 );
  STATIC_INLINE("\x03", "dec"  , TY_FN_PRE, DEC  );
  STATIC_INLINE("\x03", "inv"  , TY_FN_PRE, INV  );
  STATIC_INLINE("\x03", "neg"  , TY_FN_PRE, NEG  );
  STATIC_INLINE("\x03", "not"  , TY_FN_PRE, NOT  );
  STATIC_INLINE("\x05", "i1to4", TY_FN_PRE, CI1  );
  STATIC_INLINE("\x05", "i2to4", TY_FN_PRE, CI2  );
  STATIC_INLINE("\x01", "+"    , TY_FN_PRE, ADD  );
  STATIC_INLINE("\x01", "-"    , TY_FN_PRE, SUB  );
  STATIC_INLINE("\x01", "%"    , TY_FN_PRE, MOD  );
  STATIC_INLINE("\x03", "shl"  , TY_FN_PRE, SHL  );
  STATIC_INLINE("\x03", "shr"  , TY_FN_PRE, SHR  );
  STATIC_INLINE("\x03", "msk"  , TY_FN_PRE, MSK  );
  STATIC_INLINE("\x02", "jn"   , TY_FN_PRE, JN   );
  STATIC_INLINE("\x03", "xor"  , TY_FN_PRE, XOR  );
  STATIC_INLINE("\x03", "and"  , TY_FN_PRE, AND  );
  STATIC_INLINE("\x02", "or"   , TY_FN_PRE, OR   );
  STATIC_INLINE("\x02", "=="   , TY_FN_PRE, EQ   );
  STATIC_INLINE("\x02", "!="   , TY_FN_PRE, NEQ  );
  STATIC_INLINE("\x02", ">="   , TY_FN_PRE, GE_U );
  STATIC_INLINE("\x01", "<"    , TY_FN_PRE, LT_U );
  STATIC_INLINE("\x04", "ge_s" , TY_FN_PRE, GE_S );
  STATIC_INLINE("\x04", "lt_s" , TY_FN_PRE, LT_S );
  STATIC_INLINE("\x01", "*"    , TY_FN_PRE, MUL  );
  STATIC_INLINE("\x01", "/"    , TY_FN_PRE, DIV_U);
  STATIC_INLINE("\x02", "xw"   , TY_FN_PRE, XW  );

  // ftN(addr): fetch a value of sz N from address.
  // srN(value, addr): store a value of sz N to address.
  STATIC_INLINE("\x03", "ft1"  , TY_FN_PRE, SZ1+FT   );
  STATIC_INLINE("\x03", "ft2"  , TY_FN_PRE, SZ2+FT   );
  STATIC_INLINE("\x03", "ft4"  , TY_FN_PRE, SZ4+FT   );
  STATIC_INLINE("\x03", "ftR"  , TY_FN_PRE, SZR+FT   );
  STATIC_INLINE("\x03", "sr1"  , TY_FN_PRE, SZ1+SR   );
  STATIC_INLINE("\x03", "sr2"  , TY_FN_PRE, SZ2+SR   );
  STATIC_INLINE("\x03", "sr4"  , TY_FN_PRE, SZ4+SR   );
  STATIC_INLINE("\x03", "srR"  , TY_FN_PRE, SZR+SR   );
  STATIC_INLINE("\x05", "ftBe1", TY_FN_PRE, SZ1+FTBE );
  STATIC_INLINE("\x05", "ftBe2", TY_FN_PRE, SZ2+FTBE );
  STATIC_INLINE("\x05", "ftBe4", TY_FN_PRE, SZ4+FTBE );
  STATIC_INLINE("\x05", "ftBeR", TY_FN_PRE, SZR+FTBE );
  STATIC_INLINE("\x05", "srBe1", TY_FN_PRE, SZ1+SRBE );
  STATIC_INLINE("\x05", "srBe2", TY_FN_PRE, SZ2+SRBE );
  STATIC_INLINE("\x05", "srBe4", TY_FN_PRE, SZ4+SRBE );
  STATIC_INLINE("\x05", "srBeR", TY_FN_PRE, SZR+SRBE );
  TASSERT_EMPTY();
}

// Test helpers

void executeInstrs(Kern* k, U1* instrs) { cfb->ep = instrs; executeLoop(k); }

U1* compileRepl(Kern* k, bool withRet) {
  U1* body = (U1*) BBA_alloc(&k->bbaRepl, 256, 1);
  ASSERT(body, "compileRepl OOM");
  Buf* code = &k->g.code; *code = (Buf){.dat=body, .cap=256};
  compileSrc(k);
  if(withRet) Buf_add(code, RET);
  if(code->len < code->cap)
    BBA_free(&k->bbaRepl, /*dat*/code->dat + code->len, /*sz*/code->cap - code->len, 1);
  *code = (Buf){0};
  return body;
}

void simpleRepl(Kern* k) {
  size_t cap;
  Ring_var(r, 256);
  BufFile f = BufFile_init(r, (Buf){0});
  k->g.src = File_asReader(BufFile_asFile(&f));

  eprintf(  "Simple REPL: type EXIT to exit\n");
  while(true) {
    size_t len = getline((char**)&f.b.dat, &cap, stdin);
    ASSERT(len < 0xFFFF, "input too large");
    f.b.plc = 0; f.b.len = len; f.b.cap = len;
    eprintf("##### Input: %s", f.b.dat);
    if(0 == strcmp("EXIT", f.b.dat)) return;
    if(len-1) {
      eprintf("?? Compiling Repl\n");
      U1* code = compileRepl(k, true);
      eprintf("?? Executing Repl\n");
      executeInstrs(k, code);
    }
    dbgWs(k);
  }
}
