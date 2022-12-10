
#include "./fngi.h"

#define R0        return 0;

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

void Kern_init(Kern* k, FnFiber* fb) {
  *k = (Kern) {
    .bbaCode = (BBA) { &civ.ba },
    .bbaDict = (BBA) { &civ.ba },
    .fb = fb,
  };
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

static inline TyFn* tyFn(void* p) {
  ASSERT(isTyFn((Ty*)p), "invalid TyFn");
  return (TyFn*)p;
}

// Make sure to check isFnNative first!
static inline void executeNative(Kern* k, TyFn* fn) {
  ((void(*)(Kern*)) fn->d.v)(k);
}

void xImpl(Kern* k, Ty* ty) {
  TyFn* fn = tyFn(ty);
  eprintf("?? xImpl: %X\n", ty->v);
  if(isFnNative(fn)) return executeNative(k, fn);
  ASSERT(RS->sp >= fn->lSlots + 1, "execute: return stack overflow");
  INFO_ADD((Slot)fn);
  RS_ADD((Slot)cfb->ep); cfb->ep = (U1*)ty->v;
  RS->sp -= fn->lSlots; // grow locals (and possibly defer)
}

TyFn catchTy;
void ret(Kern* k) {
  TyFn* ty = (TyFn*) INFO_POP();
  U1 lSlots = (ty == &catchTy) ? 0 : ty->lSlots;
  RS->sp += lSlots;
  cfb->ep = (U1*)RS_POP();
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
  eprintf("??? instr: %.*s\n", Dat_fmt(name));
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
    eprintf("?? Execute loop\n");
    if(setjmp(local_errJmp)) { // got panic
      if(!catchPanic(k)) longjmp(*prev_errJmp, 1);
    }
    U1 res = executeInstr(k, popLit(k, 1));
    if(res) {
      eprintf("  ??? res: %X\n", res);
      if(YLD == res) yield();
      else /* RET */ {
        if(0 == Stk_len(RS)) {  // empty stack, fiber done
          eprintf("??? killing\n");
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
  cfb->ep = (U1*)fn->d.v;
  executeLoop(k);
}

void executeInstrs(Kern* k, Slc instrs) {
  cfb->ep = instrs.dat;
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
void scan(Reader f, Buf* b) {
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

void scanNext(Reader f, Buf* b) {
  Ring_incHead(&Xr(f, asBase)->ring, b->len);
  Buf_clear(b);
  scan(f, b);
}

// #################################
// # Compiler

void lit(Buf* b, U4 v) {
  if (v <= SLIT_MAX)    { Buf_add(b, SLIT | v); }
  else if (v <= 0xFF)   { Buf_add(b, SZ1 | LIT); Buf_add(b, v); }
  else if (v <= 0xFFFF) { Buf_add(b, SZ2 | LIT); Buf_addBE2(b, v); }
  else                  { Buf_add(b, SZ4 | LIT); Buf_addBE4(b, v); }
}

void compileLit(Kern* k, Ty* ty, bool asNow) {
  if(asNow) return WS_ADD(ty->v);
  lit(&k->g.code, ty->v);
}

void compileFn(Kern* k, TyFn* fn) {
  Buf* b = &k->g.code;
  Buf_add(b, XL);
  Buf_addBE4(b, (U4)&fn);
}

void compileTy(Kern* k, Ty* ty, bool asNow) {
  ASSERT(ty, "name not found");
  if(isTyConst(ty)) return compileLit(k, ty, asNow);
  if(isTyLocal(ty)) assert(false); // TODO
  TyFn* fn = tyFn(ty);
  if(isFnPre(fn))  executeFn(k, fn); // recurse before compiling rest

  if(isFnSyn(fn)) {
    WS_ADD(asNow);
    return executeFn(k, fn);
  }
  if(isFnNow(fn)) { ASSERT(asNow, "function must be called with '$'"); }
  if(asNow) executeFn(k, fn);
  else      compileFn(k, fn);
}
