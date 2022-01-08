
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>

char dbgMode = 0x10;

// ********************************************
// ** Core Types

typedef uint8_t Bool;
typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef uint32_t ASz;
typedef ASz APtr;

typedef U16 CSz;
typedef CSz CPtr;

// 256 64k module blocks
/*get aptr  */ #define MAX_APTR 0xFFFFFF
/*get module*/ #define MOD_HIGH_MASK 0xFF0000

#define APO2  2
#define ASIZE sizeof(ASz)
#define CSIZE sizeof(CSz)
#define OK 0

#ifndef FALSE
#define TRUE 1
#define FALSE 0
#endif

#define SET_ERR(E)  if(TRUE) { assert(E); *env.err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR() assert(!*env.err)
#define ASM_ASSERT(C, E) /* Assert return void */  \
  if(!(C)) SET_ERR(E);

// Error classes
#define E_ok           0 // no error
#define E_general 0xE000 // general errors [E000-E010)
#define E_io      0xE010 // IO error class
#define E_asm     0xE0A0 // assembly error class (cause in asm).
#define E_comp    0xE0C0 // compiler error class (cause in comp).
#define E_test    0xA000 // [AXXX] (assert) test case error.

#define E_intern  0xE001 // internal (undefined) error
#define E_undef   0xE002 // undefined error
#define E_unreach 0xE003 // unreachable code
#define E_todo    0xE004 // executed incomplete (to do) code
#define E_wsEmpty 0xE005 // the WS was expected empty

#define E_null    0xE0A1 // null access
#define E_oob     0xE0A2 // out of bounds access
#define E_stkUnd  0xE0A3 // Stack underflow
#define E_stkOvr  0xE0A4 // Stack overflow
#define E_align2  0xE0A5 // access off 2byte allign
#define E_align4  0xE0A6 // access off 4byte align
#define E_divZero 0xE0A7 // divide by zero

// Compiler (i.e. syntax) errors
#define E_cInstr    0xE0C1 // invalid instr
#define E_cToken    0xE0C2 // token invalid
#define E_cTLen     0xE0C3 // token invalid
#define E_cKey      0xE0C4 // key already exists
#define E_cNoKey    0xE0C5 // key not found
#define E_cHex      0xE0C6 // non-hex number
#define E_cSz       0xE0C7 // invalid Sz selected
#define E_cSzAPtr   0xE0C8 // invalid Sz for aptr
#define E_cRet      0xE0C9 // invalid RET
#define E_cDblSr    0xE0CA // Double store
#define E_cDevOp    0xE0CB // device op not impl
#define E_cDictOvr  0xE0CC // dict overflow
#define E_cXHasL    0xE0CD // xs/jmp to non-small
#define E_cXNoL     0xE0CE // x to non-locals
#define E_cErr      0xE0CF // D_assert err code invalid
#define E_cKeyLen   0xE0D0 // Key len too large
#define E_cReg      0xE0D1 // Register error

#define REF_MASK    0xFFFFFF
#define IS_FN        (0x20 << 24)
#define TY_FN_LOCALS (0x10 << 24)

#define dbg(MSG)  if(TRUE){printf(MSG); dbgEnv();}

typedef enum {
  C_OP   = 0x00,
  C_SLIT = 0x40,
  C_JMP =  0x80,
  C_MEM =  0xC0,
} InstrClass;

typedef enum {
  NOP  = 0x00,
  RETZ = 0x01,
  RET  = 0x02,
  SWP  = 0x03,
  DRP  = 0x04,
  OVR  = 0x05,
  DUP  = 0x06,
  DUPN = 0x07,
  DVFT = 0x08,
  DVSR = 0x09,
  RGFT = 0x0A,
  RGSR = 0x0B,

  INC  = 0x10,
  INC2 = 0x11,
  INC4 = 0x12,
  DEC  = 0x13,
  INV  = 0x14,
  NEG  = 0x15,
  NOT  = 0x16,
  CI1  = 0x17,
  CI2  = 0x18,

  ADD  = 0x20,
  SUB  = 0x21,
  MOD  = 0x22,
  SHL  = 0x23,
  SHR  = 0x24,
  AND  = 0x25,
  OR   = 0x26,
  XOR  = 0x27,
  LAND = 0x28,
  LOR  = 0x29,
  EQ   = 0x2A,
  NEQ  = 0x2B,
  GE_U = 0x2C,
  LT_U = 0x2D,
  GE_S = 0x2E,
  LT_S = 0x2F,

  MUL  = 0x30,
  DIV_U= 0x31,
  DIV_S= 0x32,

  JMPL = 0x80,
  JMPW = 0x81,
  JZL  = 0x82,
  JTBL = 0x83,
  XL   = 0x84,
  XW   = 0x85,
  XSL  = 0x86,
  XSW  = 0x87,

  LIT  = 0xC0,
  FT   = 0xC1,
  FTLL = 0xC2,
  FTML = 0xC3,
  SR   = 0xC4,
  SRLL = 0xC5,
  SRML = 0xC6,
} Instr;

typedef enum {
  R_EP = 0x00,
  R_LP = 0x40,
  R_CP = 0x80,
} Reg;

typedef enum {
  SzI1 = 0x00,
  SzI2 = 0x10,
  SzI4 = 0x20,
} SzI;

#define SZ_MASK        0x30
#define RG_MASK        0xC0
#define INSTR_CLASS(INSTR) (InstrClass)(0xC0 & INSTR)
#define INSTR_NO_SZ(INSTR)  (Instr)(~SZ_MASK & (U8)INSTR)
#define INSTR_SZI(INSTR) ((SzI) (INSTR & SZ_MASK))

#define SzIA SzI4

// Generic stack.
typedef struct { U16 sp; U16 size; U8* mem; } Stk;

// Environment
typedef struct {
  APtr ep;  // execution pointer
  APtr mp;  // Module Pointer
  APtr* heap;
  APtr* topHeap;
  APtr* topMem;
  U32* err;
  U32* state;
  U32* testIdx;
  Stk ls;

  // Separate from mem
  Stk ws;
  Stk cs;
} Env;

#define LS_SP           (env.ls.mem - mem + env.ls.sp)
#define CS_SP           (env.cs.mem - mem + env.cs.sp)
#define ENV_MOD_HIGH()  (env.ep & MOD_HIGH_MASK)

typedef struct {
  APtr buf;  // buffer of dicts
  U16 heap;  // heap offset
  U16 end;   // end offset
  U16 lheap; // local heap
  U16 _align;
} Dict;

typedef struct {
  APtr buf;
  U16 end;
  U16* heap;
} DictRef;

#define DEFAULT_DICT \
  {.buf = dict->buf, .end = dict->end, .heap = &dict->heap}

typedef struct {
  U32 value;
  U8 len;
  U8 s[];
} Key;
#define keySizeWLen(LEN)  (4 + 1 + 1 + (LEN))

// Get key len. The top two bits are used for metadata (i.e. constant)
static inline U8 Key_len(Key* key) {
  return 0x3F & key->len;
}

#define MAX_TOKEN 32
#define TOKEN_BUF 0x7D

typedef struct {
  APtr buf; // buffer.
  U8 len;   // length of token
  U8 size;  // characters buffered
  U8 group;
} TokenState;

typedef enum {
  T_NUM, T_HEX, T_ALPHA, T_SPECIAL, T_SYMBOL, T_WHITE
} TokenGroup;

// Debugging
void dbgEnv();
void dbgInstr(Instr instr, Bool lit);
void dbgWs();
void dbgWsFull();
void printToken();

// ********************************************
// ** Globals

void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz);

Env env;
U8* mem = NULL;
Dict* dict = NULL;
TokenState* tokenState = NULL;
char* compilingName;
FILE* srcFile;
SzI instrSzI = SzI1;
Instr globalInstr = NOP;
ssize_t (*readAppend)() = NULL; // Read bytes incrementing tokenBufSize
U32 line = 1;
jmp_buf* err_jmp;

// ********************************************
// ** Utilities

U8 szIToSz(SzI szI) {
  switch (szI) {
    case SzI1: return 1;
    case SzI2: return 2;
    case SzI4: return 4;
    default: SET_ERR(E_cSz);
  }
}

SzI szToSzI(U8 sz) {
  switch (sz) {
    case 1: return SzI1;
    case 2: return SzI2;
    case 4: return SzI4;
    case 0xA: return SzIA;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ APtr alignAPtr(APtr aPtr, U8 sz) {
  U8 mod = aPtr % sz;
  if(mod == 0) return aPtr;
  return aPtr + (sz - mod);
}

// Return value of ASCII hex char.
/*fn*/ U8 charToHex(U8 c) {
  c = c - '0';
  if(c <= 9) return c;
  c = c - ('A' - '0');
  if(c <= 5) return c + 10;
  c = c - ('a' - 'A') + 10;
  ASM_ASSERT(c < 16, E_cHex);
  return c;
}

/*fn*/ void store(U8* mem, APtr aptr, U32 value, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (szI) {
    case SzI1:
      *(mem+aptr) = (U8)value;
      return;
    case SzI2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      *((U16*) (mem+aptr)) = (U16)value;
      return;
    case SzI4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      *((U32*) (mem+aptr)) = value;
      return;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ U32 fetch(U8* mem, APtr aptr, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (szI) {
    case SzI1:
      return (U32) *((U8*) (mem+aptr));
    case SzI2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      return (U32) *((U16*) (mem+aptr));
    case SzI4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      return (U32) *((U32*) (mem+aptr));
    default: SET_ERR(E_cSz);
  }
}

#define Stk_len(STK)  ((STK.size - STK.sp) >> APO2)
#define WS_LEN        Stk_len(env.ws)
#define X_DEPTH       Stk_len(env.cs)
#define _CHK_GROW(SP, SZ) \
  ASM_ASSERT(SP - SZ >= 0, E_stkOvr)

#define WS_PUSH(VALUE)  Stk_push(&env.ws, VALUE)
/*fn*/ void Stk_push(Stk* stk, U32 value) {
  _CHK_GROW(stk, ASIZE);
  store(stk->mem, stk->sp - ASIZE, value, SzIA);
  stk->sp -= ASIZE;
}

#define WS_POP()  Stk_pop(&env.ws)
/*fn*/ U32 Stk_pop(Stk* stk) {
  ASM_ASSERT(stk->sp + ASIZE <= stk->size, E_stkUnd);
  U32 out = fetch(stk->mem, stk->sp, SzIA);
  stk->sp += ASIZE;
  return out;
}

/*fn*/ void Stk_grow(Stk* stk, U16 sz) {
  _CHK_GROW(stk, sz);
  stk->sp -= sz;
}

/*fn*/ void Stk_shrink(Stk* stk, U16 sz) {
  ASM_ASSERT(stk->sp + sz <= stk->size, E_stkUnd);
  stk->sp += sz;
}

APtr toAptr(U32 v, SzI szI) {
  switch (szI) {
    case SzI1: SET_ERR(E_cSzAPtr);
    case SzI2: return ENV_MOD_HIGH() + v;
    case SzI4: return v;
    default: SET_ERR(E_cSz);
  }
}

U8 mergeInstrSzI(SzI szI, U8 instr) {
  switch (INSTR_CLASS(instr)) {
    case C_OP:
    case C_SLIT: return instr;
    case C_JMP:
    case C_MEM: return szI + instr;
    default: assert(FALSE);
  }
}

// ********************************************
// ** Executing Instructions2

void xImpl(APtr aptr) { // impl for "execute"
  // get amount to grow, must be multipled by APtr size .
  U16 growLs = fetch(mem, aptr, SzI1);
  Stk_grow(&env.ls, growLs << APO2);
  // Callstack has 4 byte value: growLs | mp | cptrHigh | cptrLow
  Stk_push(&env.cs, (growLs << 24) + env.ep);
  env.mp = MOD_HIGH_MASK & aptr;
  env.ep = aptr + 1;
}

void xsImpl(APtr aptr) { // impl for "execute small"
  Stk_push(&env.cs, env.ep);
  env.mp = MOD_HIGH_MASK & aptr;
  env.ep = aptr;
}

U32 popLit(SzI szI) {
  U32 out = fetch(mem, env.ep, szI);
  env.ep += szIToSz(szI);
  return out;
}

#define CASE_4(V) \
    case V: \
    case V+1: \
    case V+2: \
    case V+3:

#define CASE_16(V) \
    CASE_4(V) \
    CASE_4(V+4) \
    CASE_4(V+8) \
    CASE_4(V+12)

#define CASE_32(V) \
    CASE_16(V) \
    CASE_16(V+16) \

#define GOTO_SZ(I, SZ) case SZ + I: szI = SZ; goto I;

/* returns: should escape */
inline static void executeInstr(Instr instr) {
  globalInstr = instr; // for debugging
  if(dbgMode) { printf("  * "); dbgInstr(instr, TRUE);  printf("@%X", env.ep - 1); printf("\n"); }

  U32 l, r;
  U32 szMask = 0xFFFFFFFF; // TODO: remove
  SzI szI = SzI2;
  switch ((U8)instr) {
    // Operation Cases
    case NOP: return;
    case RETZ:
      if(WS_POP()) return;
      // intentional fallthrough
    case RET:
      U32 callMeta = Stk_pop(&env.cs);
      Stk_shrink(&env.ls, (callMeta >> 24) << APO2);
      env.mp = MOD_HIGH_MASK & callMeta;
      env.ep = MAX_APTR & callMeta;
      return;
    case SWP:
      r = WS_POP();
      l = WS_POP();
      WS_PUSH(r);
      WS_PUSH(l);
      return;
    case DRP : WS_POP(); return;
    case OVR : r = WS_POP(); l = WS_POP(); WS_PUSH(l); WS_PUSH(r); WS_PUSH(l); return;
    case DUP : r = WS_POP(); WS_PUSH(r); WS_PUSH(r);      return;
    case DUPN: r = WS_POP(); WS_PUSH(r); WS_PUSH(0 == r); return;
    case DVFT: deviceOp(TRUE, SzI4, szMask, 4); return;
    case DVSR: deviceOp(FALSE, SzI4, szMask, 4); return;
    case RGFT:
      r = popLit(SzI1);
      switch (RG_MASK & r) {
        case R_EP: WS_PUSH(env.ep - 1 + (0x3F & r)); return;
        case R_LP:
          WS_PUSH(LS_SP + (0x3F & r)); return;
        case R_CP: WS_PUSH(CS_SP + (0x3F & r)); return;
        default: SET_ERR(E_cReg);
      }
    case RGSR:
      switch (RG_MASK & r) {
        case R_EP: SET_ERR(E_cReg); // SR to EP not allowed
        case R_LP:
          env.ls.sp = WS_POP() - (env.ls.mem - mem) + (0x3F & r);
          return;
        case R_CP:
          env.cs.sp = WS_POP() - (env.cs.mem - mem) + (0x3F & r);
          return;
        default: SET_ERR(E_cReg);
      }
    case INC : WS_PUSH(WS_POP() + 1); return;
    case INC2: WS_PUSH(WS_POP() + 2); return;
    case INC4: WS_PUSH(WS_POP() + 4); return;
    case DEC : WS_PUSH(WS_POP() - 1); return;
    case INV : WS_PUSH(~WS_POP()); return;
    case NEG : WS_PUSH(-WS_POP()); return;
    case NOT : WS_PUSH(0 == WS_POP()); return;
    case CI1 : WS_PUSH((I32) ((I8) WS_POP())); return;
    case CI2 : WS_PUSH((I32) ((I16) WS_POP())); return;

    case ADD : r = WS_POP(); WS_PUSH(WS_POP() + r); return;
    case SUB : r = WS_POP(); WS_PUSH(WS_POP() - r); return;
    case MOD : r = WS_POP(); WS_PUSH(WS_POP() % r); return;
    case SHL : r = WS_POP(); WS_PUSH(WS_POP() << r); return;
    case SHR : r = WS_POP(); WS_PUSH(WS_POP() >> r); return;
    case AND : r = WS_POP(); WS_PUSH(WS_POP() & r); return;
    case OR  : r = WS_POP(); WS_PUSH(WS_POP() | r); return;
    case XOR : r = WS_POP(); WS_PUSH(WS_POP() ^ r); return;
    case LAND: r = WS_POP(); WS_PUSH(WS_POP() && r); return;
    case LOR : r = WS_POP(); WS_PUSH(WS_POP() || r); return;
    case EQ  : r = WS_POP(); WS_PUSH(WS_POP() == r); return;
    case NEQ : r = WS_POP(); WS_PUSH(WS_POP() != r); return;
    case GE_U: r = WS_POP(); WS_PUSH(WS_POP() >= r); return;
    case LT_U: r = WS_POP(); WS_PUSH(WS_POP() < r); return;
    case GE_S: r = WS_POP(); WS_PUSH((I32)WS_POP() >= (I32) r); return;
    case LT_S: r = WS_POP(); WS_PUSH((I32)WS_POP() < (I32) r); return;
    case MUL  :r = WS_POP(); WS_PUSH(WS_POP() * r); return;
    case DIV_U:r = WS_POP(); WS_PUSH(WS_POP() / r); return;
    case DIV_S:
      r = WS_POP();
      ASM_ASSERT(r, E_divZero);
      WS_PUSH((I32) WS_POP() / (I32) r);
      return;

    // Small literal (64 cases)
    CASE_32(C_SLIT)
    CASE_32(C_SLIT+32) WS_PUSH(0x3F & instr); return;

    // Jmp Cases
    case SzI1 + JMPL: r = env.ep; env.ep = toAptr(r + (I8)popLit(SzI1), SzI4); return;
    case SzI2 + JMPL: env.ep = toAptr(popLit(SzI2), SzI2); return;
    case SzI4 + JMPL: env.ep = toAptr(popLit(SzI4), SzI4); return;
JMPW: case SzI2 + JMPW:
      env.ep = toAptr(WS_POP(), szI);
      return;
    GOTO_SZ(JMPW, SzI1)
    GOTO_SZ(JMPW, SzI4)
    case SzI1 + JZL:
      l = env.ep;
      r = popLit(SzI1);
      if(!WS_POP()) { env.ep = toAptr(l + (I8)r, SzI4); }
      return;
    case SzI2 + JZL:
      r = popLit(SzI2);
      if(!WS_POP()) { env.ep = toAptr(r, SzI2); }
      return;
    case SzI4 + JZL:
      r = popLit(SzI4);
      if(!WS_POP()) { env.ep = toAptr(r, SzI4); }
      return;
JTBL: case SzI2 + JTBL: assert(FALSE); // TODO: not impl
    GOTO_SZ(JTBL, SzI1)
    GOTO_SZ(JTBL, SzI4)
XL: case SzI2 + XL:
      xImpl(toAptr(popLit(szI), szI));
      return;
    GOTO_SZ(XL, SzI1)
    GOTO_SZ(XL, SzI4)
XW:
    case SzI2 + XW:
      xImpl(toAptr(WS_POP(), szI));
      return;
    GOTO_SZ(XW, SzI1)
    GOTO_SZ(XW, SzI4)
XSL: case SzI2 + XSL:
      xsImpl(toAptr(popLit(szI), szI)); return;
      return;
    GOTO_SZ(XSL, SzI1)
    GOTO_SZ(XSL, SzI4)
XSW: case SzI2 + XSW:
      xsImpl(toAptr(WS_POP(), szI));
      return;
    GOTO_SZ(XSW, SzI1)
    GOTO_SZ(XSW, SzI4)

    // Mem Cases
LIT: case SzI2 + LIT:
      WS_PUSH(popLit(szI));
      return;
    GOTO_SZ(LIT, SzI1)
    GOTO_SZ(LIT, SzI4)
FT: case SzI2 + FT:
      WS_PUSH(fetch(mem, WS_POP(), szI));
      return;
    GOTO_SZ(FT, SzI1)
    GOTO_SZ(FT, SzI4)
FTLL: case SzI2 + FTLL:
      WS_PUSH(fetch(mem, LS_SP + popLit(SzI1), szI));
      return;
    GOTO_SZ(FTLL, SzI1)
    GOTO_SZ(FTLL, SzI4)
FTML: case SzI2 + FTML:
      WS_PUSH(fetch(mem, (MOD_HIGH_MASK & env.mp) + popLit(SzI2), szI));
      return;
    GOTO_SZ(FTML, SzI1)
    GOTO_SZ(FTML, SzI4)
SR: case SzI2 + SR:
      r = WS_POP(); // Removing this will cause invalid order. stackoverflow.com/questions/376278
      store(mem, r, WS_POP(), szI);
      return;
    GOTO_SZ(SR, SzI1)
    GOTO_SZ(SR, SzI4)
SRLL: case SzI2 + SRLL:
      store(mem, LS_SP + popLit(SzI1), WS_POP(), szI);
      return;
    GOTO_SZ(SRLL, SzI1)
    GOTO_SZ(SRLL, SzI4)
SRML: case SzI2 + SRML:
      store(mem, (MOD_HIGH_MASK & env.mp) + popLit(SzI2), WS_POP(), szI);
      return;
    GOTO_SZ(SRML, SzI1)
    GOTO_SZ(SRML, SzI4)

    default: SET_ERR(E_cInstr);
  }
}

/*fn*/ void execute(U8 instr) {
  env.ep = 1; // so dbg makes it 0
  while(TRUE) {
    executeInstr(instr);
    if(Stk_len(env.cs) == 0) return;
    instr = popLit(SzI1);
  }
}

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) where key is a cstr and value is U32.

#define Dict_key(D, OFFSET)  ((Key*) (mem + D.buf + (OFFSET)))

/*fn*/ U8 cstrEq(U8 slen0, U8 slen1, char* s0, char* s1) {
  if(slen0 != slen1) return FALSE;
  for(U8 i = 0; i < slen0; i += 1) {
    if(s0[i] != s1[i]) return FALSE;
  }
  return TRUE;
}

// find key offset from dict. Else return dict.heap
/*fn*/ U16 Dict_find(DictRef d, U8 slen, char* s) {
  ASM_ASSERT(slen < 0x40, E_cKeyLen);
  U16 offset = 0;
  assert(*d.heap < d.end);

  while(offset < *d.heap) {
    Key* key = Dict_key(d, offset);
    if(cstrEq(Key_len(key), slen, (char *)key->s, s)) return offset;
    U16 entrySz = alignAPtr(keySizeWLen(Key_len(key)), 4);
    offset += entrySz;
  }

  assert(offset == *d.heap);
  return offset;
}

/*fn*/ U16 Dict_set(DictRef d, U8 slen, char* s, U32 value) {
  // Set a key to a value, returning the offset
  U16 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset == *d.heap, E_cKey)
  Key* key = Dict_key(d, offset);
  U16 addedSize = alignAPtr(keySizeWLen(slen), 4);
  ASM_ASSERT(*d.heap + addedSize <= d.end, E_cDictOvr);
  key->value = value;
  key->len = slen;
  memcpy(key->s, s, slen);   // memcpy(dst, src, sz)
  *d.heap += alignAPtr(keySizeWLen(slen), 4);
  return offset;
}

/*fn*/ U32 Dict_get(DictRef d, U8 slen, char* s) {
  U16 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset != *d.heap, E_cNoKey);
  return Dict_key(d, offset)->value;
}

/*fn*/ void Dict_forget(U8 slen, char* s) {
  DictRef d = {.buf = dict->buf, .end = dict->end, .heap = &dict->heap};
  dict->heap = Dict_find(d, slen, s);
}


// ********************************************
// ** Scanner
#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf ((char*) mem + tokenState->buf)

/*fn*/ TokenGroup toTokenGroup(U8 c) {
  if(c <= ' ') return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '_') return T_ALPHA;
  if(c == '~' || c == '\'' || c == '$' ||
     c == '.' || c ==  '(' || c == ')') {
    return T_SPECIAL;
  }
  return T_SYMBOL;
}

// clear token buf and read bytes
/*fn*/ void readNew() {
  tokenLen = 0;
  tokenBufSize = 0;
  readAppend();
}

/*fn*/ void shiftBuf() {
  // Shift buffer left from end of token
  if(tokenLen == 0) return;
  U8 newStart = tokenLen;
  U8 i = 0;
  while(tokenLen < tokenBufSize) {
    tokenBuf[i] = tokenBuf[tokenLen];
    i += 1;
    tokenLen += 1;
  }
  tokenBufSize = tokenBufSize - newStart;
  tokenLen = 0;
}

// Scan next token;
/*fn*/ void scan() {

  // Skip whitespace
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew();
    if(tokenBufSize == 0) return;
    if (toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) {
      shiftBuf();
      break;
    }
    if(tokenBuf[tokenLen] == '\n') line += 1;
    tokenLen += 1;
  }
  if(tokenBufSize < MAX_TOKEN) { readAppend(); }

  U8 c = tokenBuf[tokenLen];
  tokenState->group = (U8) toTokenGroup(c);
  if(tokenState->group <= T_ALPHA) tokenState->group = T_ALPHA;

  // Parse token until the group changes.
  while(tokenLen < tokenBufSize) {
    ASM_ASSERT(tokenLen < MAX_TOKEN, E_cTLen);
    c = tokenBuf[tokenLen];

    TokenGroup tg = toTokenGroup(c);
    if (tg == tokenState->group) {}
    else if (tokenState->group == T_ALPHA && (tg <= T_ALPHA)) {}
    else break;
    tokenLen += 1;
  }
}

U8 scanInstr() {
  DictRef d = DEFAULT_DICT;
  scan();
  U8 instr = Dict_get(d, tokenLen, tokenBuf);
  return mergeInstrSzI(instrSzI, instr);
}

/*fn*/ void cSz() { // `.`
  if(tokenLen >= tokenBufSize) readAppend();
  instrSzI = szToSzI(charToHex(tokenBuf[tokenLen]));
  tokenLen += 1;
}

/*fn*/ void cComment() {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew();
    if (tokenBufSize == 0) return;
    if (tokenBuf[tokenLen] == '\n') return;
    tokenLen += 1;
  }
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
/*fn*/ void cHex() {
  scan();
  if (dbgMode) { printf("# "); printToken(); printf("\n"); }
  U32 v = 0;
  for(U8 i = 0; i < tokenLen; i += 1) {
    U8 c = tokenBuf[i];
    if (c == '_') continue;
    ASM_ASSERT(toTokenGroup(c) <= T_HEX, E_cHex);
    v = (v << 4) + charToHex(c);
  }
  WS_PUSH(v);
  shiftBuf();
}

/*fn*/ void cDictSet() { // `=`
  scan(); // load name token
  if (dbgMode) { printf("= "); dbgWs(); printToken(); printf("\n"); }
  U32 value = WS_POP();
  DictRef d = DEFAULT_DICT;
  Dict_set(d, tokenLen, tokenBuf, value);
}

/*fn*/ void cDictGet() { // `@`
  scan();
  DictRef d = DEFAULT_DICT;
  U32 value = Dict_get(d, tokenLen, tokenBuf);
  if (dbgMode) { printf("@ "); printToken(); printf("PUSH(0x%X)\n", value); }
  WS_PUSH(value);
}

/*fn*/ void cWriteHeap() { // `,`
  U32 value = WS_POP();
  if (dbgMode) { printf(", #%X sz=%u\n", value, szIToSz(instrSzI)); }
  store(mem, *env.heap, value, instrSzI);
  *env.heap += szIToSz(instrSzI);
}

/*fn*/ void cWriteInstr() { // `%`
  U8 instr = scanInstr();
  store(mem, *env.heap, (U8)instr, SzI1);
  if (dbgMode) { printf("%% "); dbgInstr(instr, FALSE); printf(" @%X\n", *env.heap); }
  *env.heap += 1;
}

/*fn*/ void cExecuteInstr() { // ^
  U8 instr = scanInstr();
  if (dbgMode) { printf("^ "); printf("\n"); }
  env.ep = 1;
  executeInstr(instr);
}

/*fn*/ void cExecute() { // $
  scan();
  if(dbgMode) { printf("$ "); dbgWs(); printToken(); printf("\n"); }
  DictRef d = DEFAULT_DICT;
  U32 value = Dict_get(d, tokenLen, tokenBuf);
  WS_PUSH(REF_MASK & value);
  if(TY_FN_LOCALS & value) execute(SzI4 + XW);
  else                     execute(SzI4 + XSW);
}

/*fn*/ Bool compile() {
  if(tokenLen == 0) return TRUE;
  tokenLen = 1; // allows for multi symbols where valid, i.e. =$, $$
  switch (tokenBuf[0]) {
    case '.': cSz(); break;
    case '/': cComment(); break;
    case '#': cHex(); break;
    case '=': cDictSet(); break;
    case '@': cDictGet(); break;
    case ',': cWriteHeap(); break;
    case '%': cWriteInstr(); break;
    case '^': cExecuteInstr(); break;
    case '$': cExecute(); break;
    default:
      printf("!! invalid token: %c\n", tokenBuf[0]);
      SET_ERR(E_cSz);
  }
  return FALSE;
}

/*fn*/ void compileLoop() {
  jmp_buf* prev_err_jmp = err_jmp;
  jmp_buf local_err_jmp;
  err_jmp = &local_err_jmp;

  if(setjmp(local_err_jmp)) {
    printf("\n!!! ERROR\n!!! Env:");
    dbgEnv();
    printf("!!! code=#%X  test=#%X  line=%u\n", *env.err, *env.testIdx, line);
  } else {
    while(TRUE) {
      scan();
      if(compile()) break;
    }
  }

  err_jmp = prev_err_jmp;
}

DictRef popDictRef() {
  U32 rHeap = WS_POP();
  U32 buf = WS_POP();
  return (DictRef) {
    .heap = (U16*) (mem + rHeap),
    .buf = buf,
    .end = dict->end,
  };
}

void deviceOp_dict(Bool isFetch) {
  DictRef d = popDictRef();
  if(isFetch) {
    WS_PUSH(Dict_get(d, tokenLen, tokenBuf));
  } else {
    Dict_set(d, tokenLen, tokenBuf, WS_POP());
  }
}

void deviceOpRDict(Bool isFetch) {
  DictRef d = popDictRef();
  if(isFetch) {
    U32 offset = Dict_find(d, tokenLen, tokenBuf);
    if(offset == *d.heap) {
      WS_PUSH(0);
      return; // not found
    }
    WS_PUSH(d.buf + offset);
  } else Dict_forget(tokenLen, tokenBuf);
}

void deviceOpCatch() {
  // cache ep, call and local stack.
  U32 ep = env.ep;
  U32 cs_sp = env.cs.sp;
  U32 ls_sp = env.ls.sp;

  jmp_buf* prev_err_jmp = err_jmp;
  jmp_buf local_err_jmp;
  err_jmp = &local_err_jmp;

  if(setjmp(local_err_jmp)) {
    // got error, handled below
  } else {
    execute(SzI4 + XW);
  }

  // ALWAYS Reset ep, call, and local stack and clear WS.
  env.ep = ep;
  env.cs.sp = cs_sp;
  env.ls.sp = ls_sp;
  env.ws.sp = env.ws.size; // clear WS

  // update to old error jmp location
  err_jmp = prev_err_jmp;

  // Push current error to WS and clear it.
  U32 out = *env.err;
  *env.err = 0;
  WS_PUSH(out);
}

void deviceOpCompile() {
  ASM_ASSERT(!compile(), E_cRet);
}

// Device Operations
// Note: this must be declared last since it ties into the compiler infra.
void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz) {
  U32 op = WS_POP();
  U32 tmp;
  switch(op) {
    case 0x0: /*D_read*/ readAppend(); break;
    case 0x1: /*D_scan*/ scan(); break;
    case 0x2: /*D_dict*/ deviceOp_dict(isFetch); break;
    case 0x3: /*D_rdict*/ deviceOpRDict(isFetch); break;
    case 0x4: /*D_sz*/
      if(isFetch) WS_PUSH(szIToSz(instrSzI));
      else instrSzI = szToSzI(WS_POP());
      break;
    case 0x5: /*D_comp*/ deviceOpCompile(); break;
    case 0x6: /*D_assert*/ 
      tmp = WS_POP();
      if(!tmp) SET_ERR(E_cErr);
      if(!WS_POP()) SET_ERR(tmp);
      break;
    case 0x7: /*D_wslen*/ WS_PUSH(WS_LEN); break;
    case 0x8: /*D_cslen*/ WS_PUSH(Stk_len(env.cs)); break;
    case 0x9: /*D_xsCatch*/ deviceOpCatch(); break;
    default: SET_ERR(E_cDevOp);
  }
}

// ********************************************
// ** Initialization
//
ssize_t readSrc(size_t nbyte) {
  ssize_t numRead = fread(
    tokenBuf + tokenState->size,
    1, // size
    TOKEN_BUF - tokenState->size, // count
    srcFile);
  assert(!ferror(srcFile));
  if(numRead < 0) return 0;
  tokenBufSize += numRead;
  return 0;
}

#define NEW_ENV_BARE(MS, WS, RS, LS, DS)  \
  U8 localMem[MS] = {0};                  \
  U8 wsMem[WS];                           \
  U8 callStkMem[RS];                      \
  mem = localMem;                         \
  env = (Env) {                           \
    .heap =    (APtr*) (mem + 0x4),       \
    .topHeap = (APtr*) (mem + 0x8),       \
    .topMem =  (APtr*) (mem + 0xC),       \
    .err =     (U32*) (mem + 0x10),       \
    .state =   (U32*) (mem + 0x14),       \
    .testIdx = (U32*) (mem + 0x18),       \
    .ls = { .size = LS, .sp = LS },       \
    .ws = { .size = WS, .sp = WS, .mem = wsMem },     \
    .cs = \
    { .size = RS, .sp = RS, .mem = callStkMem },     \
  };                                      \
  /* configure heap+topheap */            \
  *env.heap = 0x18 + 4;                   \
  dict = (Dict*) (mem + *env.heap);       \
  dict->heap = 0;                         \
  dict->end = DS;                         \
  *env.heap += sizeof(Dict);              \
  /* Then Token State*/                   \
  tokenState = (TokenState*) (mem + *env.heap); \
  *env.heap += sizeof(TokenState);        \
  /* Configure topHeap */                 \
  *env.topHeap = MS;                      \
  *env.topMem = MS;                       \
  /* Reserve space for local stack*/      \
  *env.topHeap -= LS;                     \
  env.ls.mem = mem + *env.topHeap;        \
  /* Then dictionary */                   \
  *env.topHeap -= DS;                     \
  dict->buf = *env.topHeap;               \
  /* Then Token Buf*/                     \
  *env.topHeap -= TOKEN_BUF;              \
  tokenState->buf = *env.topHeap;

#define SMALL_ENV_BARE \
  /*           MS       WS     RS     LS     DICT */    \
  NEW_ENV_BARE(0x10000, 0x100, 0x100, 0x200, 0x2000)

void compileFile(char* s) {
  compilingName = s;
  printf("# Compiling: %s\n", s);
  line = 1;
  readAppend = &readSrc;
  srcFile = fopen(s, "rb");
  assert(srcFile > 0);
  compileLoop(); ASSERT_NO_ERR();
}

#define NEW_ENV(MS, WS, RS, LS, DS) \
  NEW_ENV_BARE(MS, WS, RS, LS, DS); \
  compileFile("spor/spor.sp");

#define SMALL_ENV \
  /*      MS      WS     RS     LS     DICT */    \
  NEW_ENV(0x8000, 0x100, 0x100, 0x200, 0x2000)


// ********************************************
// ** Main
void tests();

/*fn*/ int main() {
  // printf("compiling spor...:\n");

  tests();
  return OK;
}

// ********************************************
// ** Testing

#include <string.h>

U8 szIToSzSafe(SzI szI) {
  switch (szI) {
    case SzI1: return 1;
    case SzI2: return 2;
    case SzI4: return 4;
    default: return 0;
  }
}

void printCStr(U8 len, char* s) { printf("%.*s", len, s); }
void printToken() {
  printf("\"");
  printCStr(tokenLen, tokenBuf);
  printf("\" line=%u ", line);
}

void dbgEnv() {
  printf("  token[%u, %u]=", tokenLen, tokenBufSize);
  printToken();
  printf("stklen:%u ", WS_LEN);
  printf("tokenGroup=%u  ", tokenState->group);
  printf("instr=#%X ", globalInstr);
  printf("sz=%u\n", szIToSzSafe(instrSzI));
}


#define ENUM_STR(V) case V: return "V"

Key keyDNE = {.len = 3, .s = "???" };

Key* Dict_findFn(U32 value) {
  DictRef d = {.buf = dict->buf, .end = dict->end, .heap = &dict->heap};
  U16 offset = 0;
  Key* key = &keyDNE;

  while(offset < dict->heap) {
    Key* atKey = Dict_key(d, offset);
    if(value == (REF_MASK & atKey->value)) key = atKey;
    U16 entrySz = alignAPtr(keySizeWLen(Key_len(atKey)), 4);
    offset += entrySz;
  }
  assert(offset == *d.heap);
  return key;
}

U32 max(I32 a, I32 b) {
  if (a > b) return a;
  return b;
}

void dbgWs() {
  printf(" {%u... ", max(0, ((I32) WS_LEN) - 2));
  if(WS_LEN > 0) {
    if(WS_LEN == 1) printf("   ----  |");
    else printf(" %+8X|", fetch(env.ws.mem, env.ws.sp + ASIZE, SzIA));
    printf(" %+8X}", fetch(env.ws.mem, env.ws.sp, SzIA));
  } else printf("   ----  |   ----  }");
  printf(" ");
}

void dbgWsFull() {
  printf("  {{ ");
  for(I8 i = WS_LEN - 1; i >= 0; i -= 1) {
    printf("%X, ", fetch(env.ws.mem, env.ws.sp + (i << APO2), SzIA));
  }
  printf("}}\n");
}


char* instrStr(Instr instr) {
  switch (instr) {
    case NOP  :  return "NOP  ";
    case RETZ :  return "RETZ ";
    case RET  :  return "RET  ";
    case SWP  :  return "SWP  ";
    case DRP  :  return "DRP  ";
    case OVR  :  return "OVR  ";
    case DUP  :  return "DUP  ";
    case DUPN :  return "DUPN ";
    case DVFT :  return "DVFT ";
    case DVSR :  return "DVSR ";
    case RGFT :  return "RGFT ";
    case RGSR :  return "RGSR ";

    case INC  :  return "INC  ";
    case INC2 :  return "INC2 ";
    case INC4 :  return "INC4 ";
    case DEC  :  return "DEC  ";
    case INV  :  return "INV  ";
    case NEG  :  return "NEG  ";
    case NOT  :  return "NOT  ";
    case CI1  :  return "CI1  ";
    case CI2  :  return "CI2  ";

    case ADD  :  return "ADD  ";
    case SUB  :  return "SUB  ";
    case MOD  :  return "MOD  ";
    case SHL  :  return "SHL  ";
    case SHR  :  return "SHR  ";
    case AND  :  return "AND  ";
    case OR   :  return "OR   ";
    case XOR  :  return "XOR  ";
    case LAND :  return "LAND ";
    case LOR  :  return "LOR  ";
    case EQ   :  return "EQ   ";
    case NEQ  :  return "NEQ  ";
    case GE_U :  return "GE_U ";
    case LT_U :  return "LT_U ";
    case GE_S :  return "GE_S ";
    case LT_S :  return "LT_S ";
    case MUL  :  return "MUL  ";
    case DIV_U:  return "DIV_U";
    case DIV_S:  return "DIV_S";
    default:
  }

  switch ((~SZ_MASK) & instr) {
    case LIT:   return "LIT  ";
    case FT:    return "FT   ";
    case FTLL:  return "FTLL ";
    case FTML:  return "FTML ";
    case SR:    return "SR   ";
    case SRLL:  return "SRLL ";
    case SRML:  return "SRML ";

    case JMPL:  return "JMPL ";
    case JMPW:  return "JMPW ";
    case JZL:   return "JZL  ";
    case JTBL:  return "JTBL ";
    case XL:    return "XL   ";
    case XW:    return "XW   ";
    case XSL:   return "XSL  ";
    case XSW:   return "XSW  ";
  }

  return "instr?";
}


Bool _dbgMemInvalid(SzI szI, APtr aptr) {
  U8 sz = szIToSz(szI);
  if (aptr != alignAPtr(aptr, sz)) {
    printf(" !!ALIGN!! ");
    return TRUE;
  }
  if (aptr == 0) {
    printf(" !!ep=0!! ");
    return TRUE;
  }
  if (aptr + sz > *env.topHeap) {
    printf(" !!ep>=topHeap!! ");
    return TRUE;
  }
  return FALSE;
}

void dbgJmp(Instr instr) {
  if (INSTR_CLASS(instr) != C_JMP) return;

  U32 jloc = 0;
  SzI szI = INSTR_SZI(instr);

  switch (INSTR_NO_SZ(instr)) {
    case JMPL:
    case JZL:
    case XL:
    case XSL:
      if(_dbgMemInvalid(szI, env.ep)) break;
      jloc = toAptr(fetch(mem, env.ep, szI), SzI2);
      if(!jloc) { printf(" !!jloc=0!! "); }
      break;
    case JMPW:
    case XW:
    case XSW:
      if (!WS_LEN) { printf(" ((!WS EMPTY!)) "); break; }
      jloc = fetch(env.ws.mem, env.ws.sp, SzIA); break;
  }
  if(!jloc) return;
  Key* k = Dict_findFn(jloc);
  printf(" ((");
  if(k == &keyDNE) {
    printf("?? 0x%X", jloc);
  } else {
    printCStr(Key_len(k), k->s);
    printf(" 0x%X", jloc);
  }
  printf(")) ");
}

void dbgMem(Instr instr) {
  if (INSTR_CLASS(instr) != C_MEM) return;

  SzI szI;
  switch (INSTR_NO_SZ(instr)) {
    case LIT: szI = INSTR_SZI(instr); break;
    case FTLL:
    case SRLL: szI = SzI1; break;
    case FTML:
    case SRML: szI = SzI2; break;
    default: return;
  }
  if(_dbgMemInvalid(szI, env.ep)) return;
  printf("{0x%X}", fetch(mem, env.ep, szI));
}

void dbgIndent() {
  for(U16 i = 0; i < X_DEPTH; i += 1) printf(" +");
}

#define dbgInstrFmt(SZ, NAME) printf(".%X %s] ", SZ, NAME);
void dbgInstr(Instr instr, Bool lit) {
  dbgWs();
  dbgIndent();
  printf("["); // printf("0x%-2X ", instr);
  if(INSTR_CLASS(instr) == C_SLIT) {
    printf("SL      ] [0x%X}", 0x3F & instr);
    return;
  }
  printf("%s", instrStr(instr));
  switch (INSTR_CLASS(instr)) {
    case C_JMP:
    case C_MEM: printf(" U%u", szIToSz(INSTR_SZI(instr))); break;
    default: printf("   ");
  }
  printf("] ");
  if (lit) {
    dbgJmp(instr);
    dbgMem(instr);
  }
}


#define TEST_ENV_BARE \
  globalInstr = NOP; \
  SMALL_ENV_BARE; \
  U32 heapStart = *env.heap

#define TEST_ENV \
  SMALL_ENV \
  U32 heapStart = *env.heap


char* testBuf = NULL;
U16 testBufIdx = 0;

/*test*/ ssize_t testingRead() {
  size_t i = 0;
  while (i < (TOKEN_BUF - tokenState->size)) {
    U8 c = testBuf[testBufIdx];
    if(c == 0) return i;
    tokenBuf[tokenBufSize] = c;
    tokenBufSize += 1;
    testBufIdx += 1;
    i += 1;
  }
  return i;
}

void compileStr(char* s) {
  compilingName = s;
  testBuf = s;
  testBufIdx = 0;
  line = 1;
  readAppend = &testingRead;
  compileLoop(); ASSERT_NO_ERR();
}

// ********************************************
// ** Tests

/*test*/ void testHex() {
  printf("## testHex...\n"); TEST_ENV_BARE;

  compileStr(".1 #10");
  assert(WS_POP() == 0x10);

  compileStr("/comment\n.2 #10AF");
  U32 result = WS_POP();
  assert(result == 0x10AF);

  compileStr(".4 #1002_3004");
  result = WS_POP();
  assert(0x10023004 == result);

  // Note: ignores sz
  compileStr(".2 #1002_3004");
  result = WS_POP();
  assert(0x10023004 == result);
}

/*test*/ void testDictDeps() {
  printf("## testDictDeps...\n"); TEST_ENV_BARE;
  DictRef d = DEFAULT_DICT;
  assert(cstrEq(1, 1, "a", "a"));
  assert(!cstrEq(1, 1, "a", "b"));

  assert(cstrEq(2, 2, "z0", "z0"));
  assert(!cstrEq(2, 1, "aa", "a"));
  assert(!cstrEq(2, 2, "aa", "ab"));

  assert(0 == Dict_find(d, 3, "foo"));

  // set
  assert(0 == Dict_set(d, 3, "foo", 0xF00));

  // get
  U32 result = Dict_get(d, 3, "foo");
  assert(result == 0xF00);

  assert(12 == Dict_set(d, 5, "bazaa", 0xBA2AA));
  result = Dict_get(d, 5, "bazaa");
  assert(result == 0xBA2AA);
}

/*test*/ void testDict() {
  printf("## testDict\n"); TEST_ENV_BARE;

  compileStr(".2 #0F00 =foo  .4 #000B_A2AA =bazaa"
      " @bazaa @foo .2 @foo");
  assert(0xF00 == WS_POP());   // 2foo
  assert(0xF00 == WS_POP());   // 4foo
  assert(0xBA2AA == WS_POP()); // 4bazaa
}

/*test*/ void testWriteHeap() { // test , and ;
  printf("## testWriteHeap\n"); TEST_ENV_BARE;
  compileStr(".4 #77770101, .2 #0F00, .1 #0,");
  assert(0x77770101 == fetch(mem, heapStart, SzI4));
  assert(0x0F00 == fetch(mem, heapStart+4, SzI2));
  assert(0 == fetch(mem, heapStart+6, SzI1));
}

// These were useful for initial development
// /*test*/ void testExecuteInstr() {
//   printf("## testExecuteInstr\n"); TEST_ENV;
//   compileStr(".4 @E_general");
//   assert(0xE000 == WS_POP());
// 
//   compileStr(".2");
//   assert(SzI2 == instrSzI);
// 
//   compileStr(".4 #5006 #7008 .2 ^DRP");
//   assert(0x5006 == WS_POP());
// 
//   compileStr(".1 #01 #02  ^ADD");
//   assert(0x03 == WS_POP());
// 
//   compileStr(".4 #8 #5 ^SUB");
//   assert(0x03 == WS_POP());
// 
//   compileStr(".4 #8000 #4 ^SUB");
//   assert(0x7FFC == WS_POP());
// 
//   compileStr(".A @D_sz ^DVFT");
//   assert(0x04 == WS_POP());
// 
//   U32 expectDictHeap = (U8*)dict - mem + 4;
//   compileStr(".4 @c_tokenBuf #FF_FFFF ^AND ^FT"); assert(tokenState->buf == WS_POP());
//   compileStr(".4 @topHeap #FF_FFFF ^AND ^FT");  assert(*env.topHeap == WS_POP());
//   compileStr(".4 @c_dictHeap #FF_FFFF ^AND");   assert(expectDictHeap == WS_POP());
// }

/*test*/ void testSporeSp() {
  printf("## testSporeSp\n"); TEST_ENV;
  if(WS_LEN) { dbgWsFull(); assert(FALSE); }

  // Test h1
  heapStart = *env.heap;
  compileStr(".1 #42 ,  #43 $h1");
  assert(heapStart+2 == *env.heap);
  assert(0x42 == fetch(mem, heapStart, SzI1));
  assert(0x43 == fetch(mem, heapStart + 1, SzI1));

  // Test L0
  heapStart = *env.heap;
  compileStr("#7 $L0");
  assert(heapStart+1 == *env.heap);
  U32 v1 = fetch(mem, heapStart, SzI1);
  assert(C_SLIT | 0x7 == fetch(mem, heapStart, SzI1));

  // Test h2
  *env.heap = alignAPtr(*env.heap, 2);
  heapStart = *env.heap;
  compileStr("#1234 $h2");
  assert(heapStart+2 == *env.heap);
  assert(0x1234 == fetch(mem, heapStart, SzI2));

  // Test h4
  *env.heap = alignAPtr(*env.heap, 4);
  heapStart = *env.heap;
  compileStr("#987654 $h4");
  assert(heapStart+4 == *env.heap);
  assert(0x987654 == fetch(mem, heapStart, SzI4));

  // Test various
  compileStr("$getHeap $getTopHeap");
  assert(*env.topHeap == WS_POP());
  assert(*env.heap == WS_POP());

  compileLoop(); ASSERT_NO_ERR();
  compileFile("spor/testAsm2.sp");
}

// /*test*/ void testBoot() {
//   printf("## testBoot\n"); SMALL_ENV;
//   compileFile("spor/boot.sp");
// 
//   // printf("## testBoot... testBoot.sp\n");
//   // compileFile("spor/testBoot.sp");
// 
//   // printf("## testBoot... boot.fn\n");
//   // compileFile("spor/boot.fn");
// }


/*test*/ void tests() {
  testHex();
  testDictDeps();
  testDict();
  testWriteHeap();
  testSporeSp();
  // testBoot();

  assert(0 == WS_LEN);
}
