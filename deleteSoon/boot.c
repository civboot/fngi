// Fngi bootstrap from C.

#include "./boot.h"
#include "../bin/const.h"
#include "../bin/spor.h"

#define cfb             ((FnFiber*)civ.fb)

#define WS              (cfb->ws)
#define CS              (cfb->cs)
#define CSZ             (cfb->csz)
#define CS_SP           (&CS.dat[CS.sp])

#define WS_POP()          Stk_pop(&WS)
#define WS_POP2(A, B)     STK_POP2(&WS, A, B)
#define WS_PUSH(V)        Stk_push(&WS, V)
#define WS_PUSH2(A, B)    STK_PUSH2(&WS, A, B)
#define WS_PUSH3(A, B, C) STK_PUSH3(&WS, A, B, C)
#define CS_PUSH(V)        Stk_push(&CS, V)

#define TASSERT_WS(E)     TASSERT_EQ(E, WS_POP());

Slot line;
Globals rootG;
Kern    rootK;
FnFiber rootFb;

void FnFiber_init(FnFiber* fb, U1* mem) {
  fb->csz = Stk1_init(mem,                              CS_DEPTH);
  fb->ws  =  Stk_init((U4*)(fb->csz.dat + fb->csz.cap), WS_DEPTH);
  // call stack is rest of block
  const Slot cap = (BLOCK_SIZE - (CS_DEPTH + (sizeof(Slot) * WS_DEPTH))) / sizeof(Slot);
  fb->cs  =  Stk_init(fb->ws.dat + fb->ws.cap, cap);
}

Block* initBoot(U1 numBlocks) {
  Block* blocks = malloc(numBlocks << BLOCK_PO2);
  initCivUnix((Fiber*)&rootFb, &((BANode*)blocks)[1], &blocks[1], numBlocks - 1);
  BA_init(&civ.ba);
  U1* block1Remaining = (U1*)blocks + numBlocks;
  FnFiber_init(&rootFb, block1Remaining);

  rootG = (Globals) {
    // TODO: put globals in a place it can grow
    .glen = sizeof(Globals), .gcap = sizeof(Globals),
    .bbaPub = &rootK.bbaPub,
    .bbaPriv = &rootK.bbaPriv,
    .src = (File) {
      .buf = (PlcBuf) { .dat = rootG.buf0, .cap = TOKEN_SIZE },
      .code = File_CLOSED,
    },
    .dictStk = Stk_init((Slot*)rootG.dictBuf, DICT_DEPTH),
  };

  rootK.bbaPub   = (BBA) { .ba = &civ.ba, .rooti=BLOCK_END };
  rootK.bbaPriv  = (BBA) { .ba = &civ.ba, .rooti=BLOCK_END };
  rootG.bbaLocal = (BBA) { .ba = &civ.ba, .rooti=BLOCK_END };

  return blocks;
}

TEST(init)
  Block* blocks = initBoot(3);
  WS_PUSH(4); TASSERT_WS(4);

  free(blocks);
END_TEST


// ***********************
// * 4: Executing Instructions
// Fngi's assembly is defined in boot/constants.sp. These constants are
// auto-generated into constants.h, which are imported here.
//
// The VM executes instruction bytecode in the fngi memory space, utilizing
// the fngi globals like CS and WS.

//   *******
//   * 4.a: Utilities
// There are a few utility functions necessary for executing instructions and
// compiling them in tests.

typedef enum { LOC_LOCAL, LOC_PRIV, LOC_PUB } Loc; // Location of name/storage

Loc nameLoc() { // get the location to store names
  if(C_LOCAL    & cfb->g->cstate) return LOC_LOCAL;
  if(C_PUB_NAME & cfb->g->cstate) return LOC_PUB;  else return LOC_PRIV;
}

// Location to store code
Loc codeLoc() { return (C_PUB & cfb->g->cstate) ? LOC_PUB : LOC_PRIV; }

DNode** dictPubRef() {
  Stk* s = &cfb->g->dictStk;  ASSERT(s->sp < s->cap, "dictPubRef");
  // s->dat points to &&DNode, so a pointer to a place in `dat` is a
  // &&&DNode. We derefernce this once to be &&DNode.
  return *(DNode***)&s->dat[s->sp];
}

DNode* nameDict() {
  switch(nameLoc()) {
    case LOC_LOCAL: return cfb->g->dictLocal;
    case LOC_PUB:   return *dictPubRef();
    case LOC_PRIV:  return cfb->g->dictPriv;
    default: assert(false);
  }
}

BBA* locBBA(Loc l) {
  Ref r; switch(l) {
    case LOC_LOCAL: return &cfb->g->bbaLocal;
    case LOC_PUB:   return cfb->g->bbaPub;
    case LOC_PRIV:  return cfb->g->bbaPriv;
    default: assert(false);
  }
}

BBA* nameBBA() { return locBBA(nameLoc()); }
BBA* codeBBA() { return locBBA(codeLoc()); }

#define heap          _heap(codeBBA(), false)
#define topHeap       _heap(codeBBA(), true)
U1* _heap(BBA* bba, bool top) {
  if(top) return (U1*)&bba->ba->blocks[bba->rooti] + bba->cap;
          return (U1*)&bba->ba->blocks[bba->rooti] + bba->len;
}

// bump memory from the bba
U1* bump(BBA* bba, U1 aligned, U4 size) {
  // FIXME: remove aligned, bump is only for code.
  U1 starti = bba->rooti;
  U1* ref;
  ref = BBA_alloc(bba, size, 1);
  ASSERT(starti == bba->rooti, "bump newBlock");
  ASSERT(ref, "bump OOM");
  return ref;
}

// "Clear" the place buffer by moving existing data after plc to the beginning.
void clearPlcBuf(PlcBuf* p) {
  U1* ref = p->dat; p->len -= p->plc; // the new length
  memmove(ref, ref + p->plc, p->len); p->plc = 0;
}

U4 popLit(U1 size) {
  U4 out = ftBE(cfb->ep, size);
  cfb->ep += size;
  return out;
}

void pushH(U4 value, U1 sz) {
  srBE(bump(codeBBA(), /*aligned=*/ false, sz), sz, value);
}

// Start a new block and return it.
Block* newBlock(BBA* bba) {
  Block* b = BA_alloc(bba->ba, &bba->rooti); // note: rooti is mutated.
  ASSERT(b, "newBlock OOM"); bba->len = 0; bba->cap = BLOCK_SIZE;
  return b;
}

void h1(U1 v) { pushH(v, 1); }
void h2(U2 v) { pushH(v, 2); }
void h4(U4 v) { pushH(v, 4); }

//   *******
//   * 4.c: Giant Switch Statement
//
// Instructions (which are really just a single byte) are executed inside a
// giant switch statement. Most instructions modify the working stack and read
// literal values from the execution pointer. Some can also affect the execution
// pointerand the call stack.

void xImpl(U1* fn) {
  eprintf("??? xImpl: %X\n", fn);
  CS_PUSH((Slot)cfb->ep);
  eprintf("??? ... after CS\n");
  Stk1_push(&CSZ, 0);
  eprintf("??? ... after stk1\n");
  cfb->ep = fn;
}

void xNative(U1* fn) {
  ((void(*)()) fn)();
}

void slcImpl(U1 sz) {
  Slot len = popLit(sz);
  WS_PUSH((Slot)cfb->ep); WS_PUSH(len); // {dat, len}
  cfb->ep += len;
}

inline static U1 executeInstr(U1 instr) {
  eprintf("??? instr: %X\n", instr);
  U4 l, r;
  switch ((U1)instr) {
    // Operation Cases
    case NOP: R0
    case RETZ: if(WS_POP()) { R0 } // intentional fallthrough
    case RET: return RET;
    case YLD: return YLD;
    case SWP: WS_POP2(l, r); WS_PUSH2(r, l); R0
    case DRP : WS_POP(); R0
    case OVR : WS_POP2(l, r); WS_PUSH3(l, r, l);          R0
    case DUP : r = WS_POP(); WS_PUSH2(r, r)               R0
    case DUPN: r = WS_POP(); WS_PUSH(r); WS_PUSH(0 == r); R0
    case LR: WS_PUSH((Slot)&CS.dat[CS.sp] + popLit(1)); R0
    case GR: WS_PUSH((Slot)cfb->g + popLit(2)); R0

    case INC : WS_PUSH(WS_POP() + 1); R0
    case INC2: WS_PUSH(WS_POP() + 2); R0
    case INC4: WS_PUSH(WS_POP() + 4); R0
    case DEC : WS_PUSH(WS_POP() - 1); R0
    case INV : WS_PUSH(~WS_POP()); R0
    case NEG : WS_PUSH(-WS_POP()); R0
    case NOT : WS_PUSH(0 == WS_POP()); R0
    case CI1 : WS_PUSH((I4) ((I1) WS_POP())); R0
    case CI2 : WS_PUSH((I4) ((I2) WS_POP())); R0

    case ADD : r = WS_POP(); WS_PUSH(WS_POP() + r); R0
    case SUB : r = WS_POP(); WS_PUSH(WS_POP() - r); R0
    case MOD : r = WS_POP(); WS_PUSH(WS_POP() % r); R0
    case SHL : r = WS_POP(); WS_PUSH(WS_POP() << r); R0
    case SHR : r = WS_POP(); WS_PUSH(WS_POP() >> r); R0
    case MSK : r = WS_POP(); WS_PUSH(WS_POP() & r); R0
    case JN  : r = WS_POP(); WS_PUSH(WS_POP() | r); R0
    case XOR : r = WS_POP(); WS_PUSH(WS_POP() ^ r); R0
    case AND : r = WS_POP(); WS_PUSH(WS_POP() && r); R0
    case OR  : r = WS_POP(); WS_PUSH(WS_POP() || r); R0
    case EQ  : r = WS_POP(); WS_PUSH(WS_POP() == r); R0
    case NEQ : r = WS_POP(); WS_PUSH(WS_POP() != r); R0
    case GE_U: r = WS_POP(); WS_PUSH(WS_POP() >= r); R0
    case LT_U: r = WS_POP(); WS_PUSH(WS_POP() < r); R0
    case GE_S: r = WS_POP(); WS_PUSH(((I4)WS_POP()) >= ((I4)r)); R0
    case LT_S: r = WS_POP(); WS_PUSH(((I4)WS_POP()) < ((I4)r)); R0
    case MUL  :r = WS_POP(); WS_PUSH(WS_POP() * r); R0
    case DIV_U:r = WS_POP(); WS_PUSH(WS_POP() / r); R0
    case DIV_S: WS_POP2(l, r); ASSERT(r, "Div zero");
                WS_PUSH((I4)l / (I4)r);             R0

    // Mem Cases
    case SZ1 + FT: WS_PUSH(*(U1*)WS_POP()); R0
    case SZ2 + FT: WS_PUSH(*(U2*)WS_POP()); R0
    case SZ4 + FT: WS_PUSH(*(U4*)WS_POP()); R0

    case SZ1 + FTBE: WS_PUSH(ftBE((U1*)WS_POP(), 1)); R0
    case SZ2 + FTBE: WS_PUSH(ftBE((U1*)WS_POP(), 2)); R0
    case SZ4 + FTBE: WS_PUSH(ftBE((U1*)WS_POP(), 4)); R0

    case SZ1 + FTO: WS_PUSH(*(U1*) (WS_POP() + popLit(1))); R0
    case SZ2 + FTO: WS_PUSH(*(U2*) (WS_POP() + popLit(1))); R0
    case SZ4 + FTO: WS_PUSH(*(U4*) (WS_POP() + popLit(1))); R0

    case SZ1 + FTLL: WS_PUSH(*(U1*) (CS_SP + popLit(1))); R0
    case SZ2 + FTLL: WS_PUSH(*(U2*) (CS_SP + popLit(1))); R0
    case SZ4 + FTLL: WS_PUSH(*(U4*) (CS_SP + popLit(1))); R0

    case SZ1 + FTGL: WS_PUSH(*(U1*) ((Slot)cfb->g + popLit(2))); R0
    case SZ2 + FTGL: WS_PUSH(*(U2*) ((Slot)cfb->g + popLit(2))); R0
    case SZ4 + FTGL: WS_PUSH(*(U4*) ((Slot)cfb->g + popLit(2))); R0

    case SZ1 + SR: WS_POP2(l, r); *(U1*)r = l; R0
    case SZ2 + SR: WS_POP2(l, r); *(U2*)r = l; R0
    case SZ4 + SR: WS_POP2(l, r); *(U4*)r = l; R0

    case SZ1 + SRBE: WS_POP2(l, r); srBE((U1*)r, 1, l); R0
    case SZ2 + SRBE: WS_POP2(l, r); srBE((U1*)r, 2, l); R0
    case SZ4 + SRBE: WS_POP2(l, r); srBE((U1*)r, 3, l); R0

    case SZ1 + SRO: r = WS_POP(); *(U1*)(r + popLit(1)) = WS_POP(); R0
    case SZ2 + SRO: r = WS_POP(); *(U2*)(r + popLit(1)) = WS_POP(); R0
    case SZ4 + SRO: r = WS_POP(); *(U4*)(r + popLit(1)) = WS_POP(); R0

    case SZ1 + SRLL: *(U1*) (CS_SP + popLit(1)) = WS_POP(); R0
    case SZ2 + SRLL: *(U2*) (CS_SP + popLit(1)) = WS_POP(); R0
    case SZ4 + SRLL: *(U4*) (CS_SP + popLit(1)) = WS_POP(); R0

    case SZ1 + SRGL: *(U1*) ((Slot)cfb->g + popLit(2)) = WS_POP(); R0
    case SZ2 + SRGL: *(U2*) ((Slot)cfb->g + popLit(2)) = WS_POP(); R0
    case SZ4 + SRGL: *(U4*) ((Slot)cfb->g + popLit(2)) = WS_POP(); R0

    case SZ1 + LIT: WS_PUSH(popLit(1)); R0
    case SZ2 + LIT: WS_PUSH(popLit(2)); R0
    case SZ4 + LIT: WS_PUSH(popLit(4)); R0

    // // Jmp Cases
    case LCL: // grow locals stack
      r = (U1)WS_POP();
      ASSERT(CSZ.sp < CSZ.cap, "CSZ oob");
      l = CSZ.dat[CSZ.sp];
      ASSERT((U4)l + r < 0xFF, "Local SZ oob");
      CSZ.dat[CSZ.sp] = l + r;
      ASSERT((I4)CS.sp - r > 0, "Locals oob");
      CS.sp -= r;
      R0
    case XL:  xImpl((U1*)popLit(4));    R0
    case XNL: xNative((U1*)popLit(4));  R0
    case JW:  cfb->ep = (U1*)WS_POP();  R0;
    case XW:  xImpl((U1*)WS_POP());     R0;

    case SZ1 + JL: r = popLit(1); cfb->ep +=  (I1)r - 1; R0
    case SZ2 + JL: r = popLit(2); cfb->ep +=  (I2)r - 1; R0
    case SZ4 + JL: r = popLit(4); cfb->ep  = (U1*)r    ; R0

    case SZ1 + JLZ: r = popLit(1); if(!WS_POP()) { cfb->ep += (I1)r - 1; } R0
    case SZ2 + JLZ: r = popLit(2); if(!WS_POP()) { cfb->ep += (I2)r - 1; } R0
    case SZ4 + JLZ: r = popLit(4); if(!WS_POP()) { cfb->ep  = (U1*)r;    } R0

    case SZ1 + JTBL: assert(false);
    case SZ2 + JTBL: assert(false);
    case SZ4 + JTBL: assert(false); // TODO: not impl

    case SZ1 + SLC: slcImpl(1); R0;
    case SZ2 + SLC: slcImpl(2); R0;
    case SZ4 + SLC: slcImpl(4); R0;

    default: if(instr >= SLIT) { WS_PUSH(0x3F & instr); R0 }
  }
  SET_ERR("Unknown instr");
}

void ret() {
  U1 sh = Stk1_pop(&CSZ);
  sh = (CSZ_CATCH == sh) ? 0 : sh; // if sh is CSZ_CATCH it is actually a panic handler
  CS.sp += sh;
  cfb->ep = (U1*)Stk_pop(&CS);
}

U2 getPanicHandler() { // find the index of the panic handler
  for(U2 i = CSZ.sp; i < CSZ.cap; i += 1) {
    if(CSZ_CATCH == *(CSZ.dat + i)) return i;
  }
  return 0xFFFF; // not found
}

bool catchPanic() { // handle a panic and return whether it is caught
  U2 csDepth = Stk_len(&CS);
  U2 handler = getPanicHandler();
  if(0xFFFF == handler) return false; // no handler
  Stk_clear(WS); WS_PUSH(csDepth); // set WS to {csDepth}
  CSZ.sp = handler; CS.sp = handler; ret(); // ret from catchable fn
  return true;
}

void executeLoop() { // execute fibers until all fibers are done.
  jmp_buf local_errJmp;
  jmp_buf* prev_errJmp = civ.fb->errJmp; civ.fb->errJmp = &local_errJmp;
  while(cfb) {
    eprintf("??Execute loop\n");
    if(setjmp(local_errJmp)) // got panic
      if(!catchPanic()) longjmp(*prev_errJmp, 1);
    U1 res = executeInstr(popLit(1));
    if(res) {
      eprintf("  ??? res: %X\n", res);
      if(YLD == res) yield();
      else /* RET */ {
        if(0 == Stk_len(&CS)) {  // empty stack, fiber done
          eprintf("??? killing\n");
          cfb->ep = NULL;
          break;
        } else ret();
      }
    }
  }
  civ.fb->errJmp = prev_errJmp;
}


U1* compileInstrs(Slc s) {
  U1* out = heap;
  for(U2 i = 0; i < s.len; i += 1) pushH(s.dat[i], 1);
  return out;
}

U1* executeInstrs(U1* instrs) {
  cfb->ep = instrs;
  executeLoop();
  eprintf("??? done execute\n");
}

TEST(executeLoop)
  Block* blocks = initBoot(3);
  Slc five  = Slc_arrLit(SLIT + 2, SLIT + 3, ADD, RET);
  Slot f = (Slot)five.dat;
  Slc call5 = Slc_arrLit(XL, f >> 24, f >> 16, f >> 8, f, RET);
  executeInstrs(five.dat);  TASSERT_WS(5);
  executeInstrs(call5.dat); TASSERT_WS(5);
  free(blocks);
END_TEST

//   *******
//   * 6.a: Token Scanner

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

// Read file blocking. Any errors result in a panic.
U2 readAtLeast(RFile* rf, U2 atLeast) {
  File* f = rf->d;
  ASSERT(f->buf.cap - f->buf.len >= atLeast, "file oob");
  U2 startLen = f->buf.len;
  while(1) {
    Xr(rf, read);
    if(f->code == File_EOF || f->buf.len - startLen >= atLeast) break;
    ASSERT(f->code < File_ERROR, "Unknown read error");
  }
  return f->buf.len - startLen;
}

void readNewAtLeast(RFile* rf, U2 num) {
  rf->d->buf.plc = 0; rf->d->buf.len = 0; readAtLeast(rf, num);
}

void _scan(RFile *rf) {
  MFile* m = rf->m;
  File* f = rf->d;
  U1 firstTg;
  PlcBuf* p = &f->buf;
  U1* dat = p->dat;
  while(true) { // Skip whitespace
    if(p->plc >= p->len) { readNewAtLeast(rf, 1); } // buffer full of white, get new
    if(p->len == 0) return; // TODO: eof check
    firstTg = toTokenGroup(dat[p->plc]); if(firstTg != T_WHITE) break;
    if(dat[p->plc] == '\n') line += 1;
    p->plc += 1;
  }
  // clearPlcBuf(F_plcBuf(*f));
  // if(!p->len) { readAtLeast(m, f, 1); }
  // assert(p->len); U1 c = dat[p->plc];
  // if(firstTg == T_SINGLE) { p->plc += 1; RV }
  // // Parse token until the group changes.
  // while (true) {
  //   if (p->plc >= p->len) readAtLeast(m, f, 1);
  //   if (p->plc >= p->len) break;
  //   ASSERT(p->plc < p->cap, E_cTLen);
  //   c = dat[p->plc];
  //   U1 tg = toTokenGroup(c);
  //   if (tg == firstTg) {}
  //   else if ((tg <= T_ALPHA) && (firstTg <= T_ALPHA)) {}
  //   else break;
  //   p->plc += 1;
  // }
}

int tests() {
  eprintf("# Testing boot.c\n");
  test_init();
  test_executeLoop();
}

int main() {
  tests();
  return 0;
}
