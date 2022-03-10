// Spor binary kernel.
// This C file implements spor, see ./spor.md for documentation.
//
// The executable produced is intended to be run by a harness,
// see ./harness.md and ./fngi for details.

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include <errno.h>

// print to stderr
#define eprint(str)   fprintf (stderr, str)
#define eprintf(format, args...)   fprintf (stderr, format, args)


typedef enum {
  LOG_SILENT    = 0x00, LOG_SYS       = 0x40, LOG_USER      = 0x20,

  LOG_TRACE     = 0x1F, LOG_DEBUG     = 0x0F, LOG_INFO      = 0x07,
  LOG_WARN      = 0x03, LOG_CRIT      = 0x01,
} UsrLogLvl;

typedef enum {
  LOG_INSTANT   = 0x20,

  LOG_INSTR     = 0x1F, LOG_EXECUTE   = 0x07, LOG_ASM       = 0x03,
  LOG_COMPILER  = 0x01,
} SysLogLvl;

#define LOG_DICT  0x02
#define LOG_ERR   0x05
#define LOG_FILE  0x06
#define LOG_JMP   0x08
#define LOG_OP    0x09
#define LOG_LIT   0x0A
#define LOG_RET_SUCCESS   0x0B
#define LOG_MEM   0x0C

// ********************************************
// ** Core Types

typedef uint8_t Bool;
typedef uint8_t U1;
typedef uint16_t U2;
typedef uint32_t U4;
typedef int8_t I1;
typedef int16_t I2;
typedef int32_t I4;
typedef uint32_t ASz;
typedef ASz APtr;

typedef U2 CSz;
typedef CSz CPtr;

// 256 64k module blocks
/*get module*/ #define MOD_HIGH_MASK 0xFF0000

#define APO2  2
#define ASIZE sizeof(ASz)
#define CSIZE sizeof(CSz)

#ifndef FALSE
#define TRUE 1
#define FALSE 0
#endif

#define ZOAB_JOIN 0x80
#define ZOAB_ARR  0x40

#define SET_ERR(E)  if(TRUE) { assert(E); *env.err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!*env.err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }

// Error classes
#define E_io      0xE010 // IO error class
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
#define E_cSzPtr    0xE0C8 // invalid Sz for a pointer
#define E_cRet      0xE0C9 // invalid RET
#define E_cDevOp    0xE0CB // device op not impl
#define E_cDictOvr  0xE0CC // dict overflow
#define E_cXHasL    0xE0CD // xs/jmp to non-small
#define E_cXNoL     0xE0CE // x to non-locals
#define E_cErr      0xE0CF // D_assert err code invalid
#define E_cKeyLen   0xE0D0 // Key len too large
#define E_cReg      0xE0D1 // Register error
#define E_cStr      0xE0D2 // Str invalid

#define E_cZoab     0xE0F1 // Zoab invalid

#define REF_MASK    0xFFFFFF
#define TY_FN_LARGE  (0x10 << 0x18)
#define TY_FN_SMART  (0x08 << 0x18)

typedef enum {
  C_OP   = 0x00, C_SLIT = 0x40, C_JMP =  0x80, C_MEM =  0xC0,
} InstrClass;

typedef enum {
  NOP  = 0x00, RETZ = 0x01, RET  = 0x02, SWP  = 0x03,
  DRP  = 0x04, OVR  = 0x05, DUP  = 0x06, DUPN = 0x07,
  DVFT = 0x08, DVSR = 0x09, RGFT = 0x0A, RGSR = 0x0B,

  INC  = 0x10, INC2 = 0x11, INC4 = 0x12, DEC  = 0x13,
  INV  = 0x14, NEG  = 0x15, NOT  = 0x16, CI1  = 0x17,
  CI2  = 0x18,

  ADD  = 0x20, SUB  = 0x21, MOD  = 0x22, SHL  = 0x23,
  SHR  = 0x24, BAND = 0x25, BOR  = 0x26, XOR  = 0x27,
  LAND = 0x28, LOR  = 0x29, EQ   = 0x2A, NEQ  = 0x2B,
  GE_U = 0x2C, LT_U = 0x2D, GE_S = 0x2E, LT_S = 0x2F,

  MUL  = 0x30, DIV_U= 0x31, DIV_S= 0x32,

  // Jmps
  JMPL = 0x80, JMPW = 0x81, JZL  = 0x82, JTBL = 0x83,
  XL   = 0x84, XW   = 0x85, XSL  = 0x86, XSW  = 0x87,

  LIT  = 0xC0, FT   = 0xC1, FTLL = 0xC2, FTGL = 0xC3,
  SR   = 0xC4, SRLL = 0xC5, SRGL = 0xC6,
} Instr;

typedef enum {
  R_MP = 0x0, R_EP = 0x1, R_LP = 0x2, R_CP = 0x3,
  R_GB = 0x4,
} Reg;

typedef enum { SzI1 = 0x00, SzI2 = 0x10, SzI4 = 0x20, } SzI;

typedef enum {
  D_read    = 0x00, D_scan    = 0x01, D_dict    = 0x02, D_rdict   = 0x03,
                    D_comp    = 0x05, D_assert  = 0x06, D_wslen   = 0x07,
  D_cslen   = 0x08, D_xCatch  = 0x09, D_memMove = 0x0A, D_memCmp  = 0x0B,
  D_com     = 0x0C, D_zoa     = 0x0D, D_dictDump= 0x0E, D_comZoab = 0x0F,
  D_comDone = 0x10,
} Device;

#define SZ_MASK        0x30
#define INSTR_CLASS(INSTR) (InstrClass)(0xC0 & INSTR)
#define INSTR_NO_SZ(INSTR)  (Instr)(~SZ_MASK & (U1)INSTR)
#define INSTR_SZI(INSTR) ((SzI) (INSTR & SZ_MASK))

#define SzIA SzI4

// Generic stack.
typedef struct { U2 sp; U2 size; U1* mem; } Stk;

typedef enum {
  ERR_DATA_NONE  = 0x00,
  ERR_DATA_INT1  = 0x01,
  ERR_DATA_DATA1 = 0x02,
  ERR_DATA_INT2  = 0x03,
  ERR_DATA_DATA2 = 0x04,
} ErrValTy;

typedef struct {
  U1 valTy;
  U1 _align;
  U2 _align2;

  U2 valueASz;
  U2 valueBSz;
  U4 valueA;
  U4 valueB;
  APtr msg;
} ErrData;

// Environment
typedef struct {
  APtr ep;  // execution pointer
  APtr mp;  // Module Pointer
  APtr gb;  // Global Base
  APtr* heap;
  APtr* topHeap;
  APtr* topMem;
  U4* err;
  U4* state;
  U4* testIdx;
  U2* sysLogLvl;
  U2* usrLogLvl;
  ErrData* errData;
  U1 szI; // global instr sz
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
  U2 heap;  // heap offset
  U2 end;   // end offset
  U2 lheap; // local heap
  U2 _align;
} Dict;

typedef struct {
  APtr buf;
  U2 end;
  U2* heap;
  U1 isLocal;
} DictRef;

#define DEFAULT_DICT \
  {.buf = dict->buf, .end = dict->end, .heap = &dict->heap, .isLocal=FALSE}

typedef struct {
  U4 value;
  U1 len;
  U1 s[];
} Key;
#define keySizeWLen(LEN)  (4 + 1 + (LEN))
#define Dict_key(D, OFFSET)  ((Key*) (mem + D.buf + (OFFSET)))
#define KEY_HAS_TY 0x40   // if 1, dict entry is a non-constant

// Get key len. The top two bits are used for metadata (i.e. constant)
static inline U1 Key_len(Key* key) {
  return 0x3F & key->len;
}

#define MAX_TOKEN 32
#define TOKEN_BUF 0x7D

typedef enum {
  T_NUM = 0,
  T_HEX = 1,
  T_ALPHA = 2,
  T_SINGLE = 3,
  T_SYMBOL = 4,
  T_WHITE = 5,
} TokenGroup;

typedef struct {
  APtr buf; // buffer.
  U1 len;   // length of token
  U1 size;  // total characters buffered
  U1 group;
  U1 _align;
} TokenState;

// Debugging
static inline void logInstr(Instr instr);
void dbgInstr(Instr instr, Bool lit);

// ********************************************
// ** Globals

// From cmdline
U2 startingSysLogLvl = 0;
U2 startingUsrLogLvl = 0;

void deviceOp(Bool isFetch, SzI szI, U1 sz);

Env env;
U1* mem = NULL;
Dict* dict = NULL;
TokenState* tokenState = NULL;
FILE* srcFile;
U1 (*readAtLeast)(U1 num) = NULL; // Read num bytes into tokenBuf
U4 line = 1;
jmp_buf* err_jmp;
long long unsigned int dbgCount = 0;

// ********************************************
// ** Zoab: See https://github.com/civboot/zoa

U1 outbuf[16];
U1* START_BYTES = "\x80\x03";

// Start a zoab log entry.
void zoab_start() { ASM_ASSERT(fwrite(START_BYTES, 1, 2, stdout), E_io); }

void writeOutbuf(U1 len) {
  U4 out = fwrite(outbuf, 1, len, stdout);
}

// Start an array of length and join bit.
void zoab_arr(U1 len, U1 join) {
  if(join) len = ZOAB_ARR | ZOAB_JOIN | len;
  else len = ZOAB_ARR | len;
  outbuf[0] = len;
  writeOutbuf(1);
}

// Write a string of length and join bit.
void zoab_data(U2 len, U1* str, U1 join) {
  while(TRUE) {
    if(len <= 63) {
      if(join) outbuf[0] = ZOAB_JOIN | len;
      else outbuf[0] = len;
      writeOutbuf(1);
      if(len) assert(fwrite(str, 1, len, stdout));
      return;
    }
    // Write a join byte
    outbuf[0] = ZOAB_JOIN | 63;
    writeOutbuf(1);
    assert(fwrite(str, 1, 63, stdout));
    len -= 63;
    str += 63;
  }
}

// Write an integer (bigendian)
void zoab_int(U4 value) {
  outbuf[8]  = value >> 24;
  outbuf[9]  = value >> 16;
  outbuf[10] = value >> 8;
  outbuf[11] = value;
  if (value <= 0xFF) {
    zoab_data(1, outbuf+11, FALSE);
  } else if (value <= 0xFFFF) {
    zoab_data(2, outbuf+10, FALSE);
  } else {
    zoab_data(4, outbuf+8, FALSE);
  }
}

// Write a null-terminated string.
void zoab_ntStr(U1* str, U1 join) {
  zoab_data(strlen(str), str, join);
}



U1 LOG_DICT_PTR[] = {LOG_SYS | LOG_DICT, 0};
U1 LOG_ERR_PTR[] = {LOG_SYS | LOG_ERR, 0};
U1 LOG_FILE_PTR[] = {LOG_SYS | LOG_FILE, 0};

void zoab_infoStart(U1* str, U2 extraLen) {
  zoab_start(); zoab_arr(2 + extraLen, FALSE);
  zoab_int(LOG_INFO); zoab_ntStr(str, FALSE);
}

void zoab_info(U1* str) {
  if(LOG_INFO & *env.usrLogLvl) {
    zoab_infoStart(str, 0);
    fflush(stdout);
  }
}

void zoab_dict(DictRef d, U4 offset) {
  Key* key = Dict_key(d, offset);

  zoab_start();
  zoab_arr(7, /*join=*/ FALSE);
  zoab_data(2, LOG_DICT_PTR, /*join=*/ FALSE);
  zoab_data(Key_len(key), (char *)key->s, /*join=*/ FALSE); // key
  zoab_int(key->value);
  zoab_int(d.buf);
  zoab_int(offset);
  zoab_int(!(KEY_HAS_TY & key->len));
  zoab_int(d.isLocal);
}


void zoab_file(U2 len, char* file) {
  zoab_start(); zoab_arr(2, FALSE);
  zoab_data(2, LOG_FILE_PTR, FALSE);
  zoab_data(len, file, FALSE);
}

// ********************************************
// ** Utilities

U1 szIToSz(SzI szI) {
  switch (szI) {
    case SzI1: return 1;
    case SzI2: return 2;
    case SzI4: return 4;
    default: SET_ERR(E_cSz);
  }
}

SzI szToSzI(U1 sz) {
  switch (sz) {
    case 1: return SzI1;
    case 2: return SzI2;
    case 4: return SzI4;
    case 0xA: return SzIA;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ APtr alignAPtr(APtr aPtr, U1 sz) {
  U1 mod = aPtr % sz;
  if(mod == 0) return aPtr;
  return aPtr + (sz - mod);
}

// Return value of ASCII hex char.
/*fn*/ U1 charToHex(U1 c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('A' <= c && c <= 'F') return 10 + c - 'A';
  ASM_ASSERT('a' <= c && c <= 'f', E_cHex);
  return 10 + c - 'a';
}

/*fn*/ void store(U1* mem, APtr aptr, U4 value, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (szI) {
    case SzI1:
      *(mem+aptr) = (U1)value;
      return;
    case SzI2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      *((U2*) (mem+aptr)) = (U2)value;
      return;
    case SzI4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      *((U4*) (mem+aptr)) = value;
      return;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ U4 fetch(U1* mem, APtr aptr, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (szI) {
    case SzI1:
      return (U4) *((U1*) (mem+aptr));
    case SzI2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      return (U4) *((U2*) (mem+aptr));
    case SzI4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      return (U4) *((U4*) (mem+aptr));
    default: SET_ERR(E_cSz);
  }
}

#define Stk_len(STK)  ((STK.size - STK.sp) >> APO2)
#define WS_LEN        Stk_len(env.ws)
#define X_DEPTH       Stk_len(env.cs)
#define WS_TOP()      fetch(env.ws.mem, env.ws.sp, SzIA)
#define _CHK_GROW(SP, SZ) \
  ASM_ASSERT(SP - SZ >= 0, E_stkOvr)

#define WS_PUSH(VALUE)  Stk_push(&env.ws, VALUE)
/*fn*/ void Stk_push(Stk* stk, U4 value) {
  _CHK_GROW(stk, ASIZE);
  store(stk->mem, stk->sp - ASIZE, value, SzIA);
  stk->sp -= ASIZE;
}

#define WS_POP()  Stk_pop(&env.ws)
/*fn*/ U4 Stk_pop(Stk* stk) {
  ASM_ASSERT(stk->sp + ASIZE <= stk->size, E_stkUnd);
  U4 out = fetch(stk->mem, stk->sp, SzIA);
  stk->sp += ASIZE;
  return out;
}

/*fn*/ void Stk_grow(Stk* stk, U2 sz) {
  _CHK_GROW(stk, sz);
  stk->sp -= sz;
}

/*fn*/ void Stk_shrink(Stk* stk, U2 sz) {
  ASM_ASSERT(stk->sp + sz <= stk->size, E_stkUnd);
  stk->sp += sz;
}

APtr toAptr(U4 v, SzI szI) {
  switch (szI) {
    case SzI1: SET_ERR(E_cSzPtr);
    case SzI2: return ENV_MOD_HIGH() + v;
    case SzI4: return v;
    default: SET_ERR(E_cSz);
  }
}

U1 mergeInstrSzI(SzI szI, U1 instr) {
  switch (INSTR_CLASS(instr)) {
    case C_OP:
    case C_SLIT: return instr;
    case C_JMP:
    case C_MEM: return szI + instr;
    default: assert(FALSE);
  }
}

U4 min(U4 a, U4 b) { if(a < b) return a; return b; }

// dump the first n elements of the working stack [totalLen, data]
void zoab_ws(U2 n) {
  zoab_arr(2, FALSE);
  zoab_int(WS_LEN);
  zoab_data(min(WS_LEN, n) << APO2, env.ws.mem + env.ws.sp, /*join=*/ FALSE);
}

// Dump the call stack
void zoab_cs() {
  zoab_data(X_DEPTH << APO2, env.cs.mem + env.cs.sp, /*join=*/ FALSE);
}

// Dump the locals stack
void zoab_ls() {
  zoab_data(Stk_len(env.ls) << APO2, env.ls.mem + env.ls.sp, /*join=*/ FALSE);
}

void zoab_err(U4 err, U1 isCaught) {
  zoab_start(); zoab_arr(7, FALSE);
  zoab_data(2, LOG_ERR_PTR, FALSE);
  zoab_int(err);
  zoab_int(isCaught);
  zoab_int(env.ep);
  zoab_int(line);

  switch (env.errData->valTy) {
    case ERR_DATA_NONE:
      zoab_arr(0, FALSE);
      break;
    case ERR_DATA_INT2:
      zoab_arr(3, FALSE);
      zoab_int(env.errData->valTy);
      zoab_int(env.errData->valueA);
      zoab_int(env.errData->valueB);
      break;
    default: assert(FALSE);
  }

  if(isCaught) zoab_int(X_DEPTH); // just update the execution depth
  else {
    zoab_arr(2, FALSE); zoab_cs(); zoab_ls();
  }

  fflush(stdout);
}

// ********************************************
// ** Executing Instructions

void xImpl(APtr aptr) { // impl for "execute"
  // get amount to grow, must be multipled by APtr size .
  U2 growLs = fetch(mem, aptr, SzI1);
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

U4 popLit(SzI szI) {
  U4 out = fetch(mem, env.ep, szI);
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

inline static void executeInstr(Instr instr) {
  U4 l, r;
  logInstr(instr);

  SzI szI = SzI2;
  switch ((U1)instr) {
    // Operation Cases
    case NOP: return;
    case RETZ:
      if(WS_POP()) return;
      // intentional fallthrough
    case RET:
      U4 callMeta = Stk_pop(&env.cs);
      Stk_shrink(&env.ls, (callMeta >> 24) << APO2);
      env.mp = MOD_HIGH_MASK & callMeta;
      env.ep = REF_MASK & callMeta;
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
    case DVFT: deviceOp(TRUE, SzI4, 4); return;
    case DVSR: deviceOp(FALSE, SzI4, 4); return;
    case RGFT:
      r = popLit(SzI1);
      switch (r) {
        case R_MP: WS_PUSH(env.mp); return;
        case R_EP: WS_PUSH(env.ep); return;
        case R_LP: WS_PUSH(LS_SP); return;
        case R_CP: WS_PUSH(CS_SP); return;
        case R_GB: WS_PUSH(env.gb); return;
        default: SET_ERR(E_cReg);
      }
    case RGSR:
      switch (r) {
        case R_MP: env.mp = WS_POP(); return;
        case R_EP: SET_ERR(E_cReg); // SR to EP not allowed
        case R_LP: env.ls.sp = WS_POP() - (env.ls.mem - mem); return;
        case R_CP: env.cs.sp = WS_POP() - (env.cs.mem - mem); return;
        case R_GB: env.gb = WS_POP(); return;
        default: SET_ERR(E_cReg);
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
    case BAND: r = WS_POP(); WS_PUSH(WS_POP() & r); return;
    case BOR : r = WS_POP(); WS_PUSH(WS_POP() | r); return;
    case XOR : r = WS_POP(); WS_PUSH(WS_POP() ^ r); return;
    case LAND: r = WS_POP(); WS_PUSH(WS_POP() && r); return;
    case LOR : r = WS_POP(); WS_PUSH(WS_POP() || r); return;
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

    // Small literal (64 cases)
    CASE_32(C_SLIT)
    CASE_32(C_SLIT+32) return WS_PUSH(0x3F & instr);

    // Jmp Cases
    case SzI1 + JMPL: r = env.ep; env.ep = toAptr(r + (I1)popLit(SzI1), SzI4); return;
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
      if(!WS_POP()) { env.ep = toAptr(l + (I1)r, SzI4); }
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
      return xImpl(toAptr(popLit(szI), szI));
    GOTO_SZ(XL, SzI1)
    GOTO_SZ(XL, SzI4)
XW: case SzI2 + XW:
      return xImpl(toAptr(WS_POP(), szI));
    GOTO_SZ(XW, SzI1)
    GOTO_SZ(XW, SzI4)
XSL: case SzI2 + XSL:
      return xsImpl(toAptr(popLit(szI), szI));
    GOTO_SZ(XSL, SzI1)
    GOTO_SZ(XSL, SzI4)
XSW: case SzI2 + XSW:
      return xsImpl(toAptr(WS_POP(), szI));
    GOTO_SZ(XSW, SzI1)
    GOTO_SZ(XSW, SzI4)

    // Mem Cases
LIT: case SzI2 + LIT:
      return WS_PUSH(popLit(szI));
    GOTO_SZ(LIT, SzI1)
    GOTO_SZ(LIT, SzI4)
FT: case SzI2 + FT:
      return WS_PUSH(fetch(mem, WS_POP(), szI));
    GOTO_SZ(FT, SzI1)
    GOTO_SZ(FT, SzI4)
FTLL: case SzI2 + FTLL:
      return WS_PUSH(fetch(mem, LS_SP + popLit(SzI1), szI));
    GOTO_SZ(FTLL, SzI1)
    GOTO_SZ(FTLL, SzI4)
FTGL: case SzI2 + FTGL:
      return WS_PUSH(fetch(mem, env.gb + popLit(SzI2), szI));
    GOTO_SZ(FTGL, SzI1)
    GOTO_SZ(FTGL, SzI4)
SR: case SzI2 + SR:
      r = WS_POP(); return store(mem, WS_POP(), r, szI);
    GOTO_SZ(SR, SzI1)
    GOTO_SZ(SR, SzI4)
SRLL: case SzI2 + SRLL:
      return store(mem, LS_SP + popLit(SzI1), WS_POP(), szI);
    GOTO_SZ(SRLL, SzI1)
    GOTO_SZ(SRLL, SzI4)
SRGL: case SzI2 + SRGL:
      l = WS_POP();
      r = env.gb + popLit(SzI2);
      return store(mem, r, l, szI);
    GOTO_SZ(SRGL, SzI1)
    GOTO_SZ(SRGL, SzI4)

    default: SET_ERR(E_cInstr);
  }
}

/*fn*/ void execute(U1 instr) {
  U2 startingLen = Stk_len(env.cs);
  while(TRUE) {
    executeInstr(instr);
    if(Stk_len(env.cs) == startingLen) return;
    instr = popLit(SzI1);
  }
}

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) where key is a cstr and value is U4.

/*fn*/ U1 cstrEq(U1 slen0, U1 slen1, char* s0, char* s1) {
  if(slen0 != slen1) return FALSE;
  for(U1 i = 0; i < slen0; i += 1) {
    if(s0[i] != s1[i]) return FALSE;
  }
  return TRUE;
}

// find key offset from dict. Else return dict.heap
/*fn*/ U2 Dict_find(DictRef d, U1 slen, char* s) {
  ASM_ASSERT(slen < 0x40, E_cKeyLen);
  U2 offset = 0;
  assert(*d.heap < d.end);
  while(offset < *d.heap) {
    Key* key = Dict_key(d, offset);
    if(cstrEq(Key_len(key), slen, (char *)key->s, s)) return offset;
    U2 entrySz = alignAPtr(keySizeWLen(Key_len(key)), 4);
    offset += entrySz;
  }
  assert(offset == *d.heap);
  return offset;
}

/*fn*/ U2 Dict_set(DictRef d, U1 slen, char* s, U4 value) {
  // Set a key to a value, returning the offset
  U2 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset == *d.heap, E_cKey)
  Key* key = Dict_key(d, offset);
  U2 addedSize = alignAPtr(keySizeWLen(slen), 4);

  ASM_ASSERT(*d.heap + addedSize <= d.end, E_cDictOvr);
  key->value = value;
  key->len = slen;
  memcpy(key->s, s, slen);   // memcpy(dst, src, sz)
  if((LOG_COMPILER & *env.sysLogLvl)) zoab_dict(d, offset);
  *d.heap += alignAPtr(keySizeWLen(slen), 4);
  return offset;
}

/*fn*/ U4 Dict_get(DictRef d, U1 slen, char* s) {
  U2 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset != *d.heap, E_cNoKey);
  return Dict_key(d, offset)->value;
}

/*fn*/ void Dict_forget(U1 slen, char* s) {
  DictRef d = {.buf = dict->buf, .end = dict->end, .heap = &dict->heap, .isLocal=FALSE};
  dict->heap = Dict_find(d, slen, s);
}


// ********************************************
// ** Scanner
#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf ((char*) mem + tokenState->buf)

/*fn*/ TokenGroup toTokenGroup(U1 c) {
  if(c <= ' ') return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '_') return T_ALPHA;
  if(c == '%' || c == '\'' || c == '$' ||
     c == '.' || c ==  '(' || c == ')') {
    return T_SINGLE;
  }
  return T_SYMBOL;
}

// clear token buf and read bytes
void readNewAtLeast(U1 num) {
  tokenLen = 0;
  tokenBufSize = 0;
  readAtLeast(num);
}

/*fn*/ void shiftBuf() {
  // Shift buffer left from end of token
  if(tokenLen == 0) return;
  U1 newStart = tokenLen;
  U1 i = 0;
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
    if(tokenLen >= tokenBufSize) readNewAtLeast(1);
    if(tokenBufSize == 0) return;
    if(toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) break;
    if(tokenBuf[tokenLen] == '\n') line += 1;
    tokenLen += 1;
  }
  shiftBuf(); // Moves buffer to the left (tokenLen=0)
  if(!tokenBufSize) { readAtLeast(1); }

  U1 c = tokenBuf[tokenLen];
  tokenState->group = toTokenGroup(c);
  if(tokenState->group == T_SINGLE) {
    tokenLen += 1; // SINGLE: always single-char token
    return;
  }

  // Parse token until the group changes.
  while (TRUE) {
    if (tokenLen >= tokenBufSize) readAtLeast(1);
    if (tokenLen >= tokenBufSize) break;

    ASM_ASSERT(tokenLen < MAX_TOKEN, E_cTLen);
    c = tokenBuf[tokenLen];

    TokenGroup tg = toTokenGroup(c);
    if (tg == tokenState->group) {}
    else if ((tokenState->group <= T_ALPHA) && (tg <= T_ALPHA)) {}
    else break;
    tokenLen += 1;
  }
}

U1 scanInstr() {
  DictRef d = DEFAULT_DICT;
  scan();
  U1 instr = Dict_get(d, tokenLen, tokenBuf);
  return mergeInstrSzI(env.szI, instr);
}

/*fn*/ void cSz() { // `.`
  if(tokenLen >= tokenBufSize) readAtLeast(1);
  env.szI = szToSzI(charToHex(tokenBuf[tokenLen]));
  tokenLen += 1;
}

/*fn*/ void cComment() {
  while(TRUE) {
    if (tokenLen >= tokenBufSize) readNewAtLeast(1);
    if (tokenBufSize == 0) return;
    if (tokenBuf[tokenLen] == '\n') return;
    tokenLen += 1;
  }
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
/*fn*/ void cHex() {
  scan();
  U4 v = 0;
  for(U1 i = 0; i < tokenLen; i += 1) {
    U1 c = tokenBuf[i];
    if (c == '_') continue;
    ASM_ASSERT(toTokenGroup(c) <= T_HEX, E_cHex);
    v = (v << 4) + charToHex(c);
  }
  WS_PUSH(v);
  shiftBuf();
}

/*fn*/ void cDictSet() { // `=`
  scan(); // load name token
  U4 value = WS_POP();
  DictRef d = DEFAULT_DICT;
  Dict_set(d, tokenLen, tokenBuf, value);
}

/*fn*/ void cDictGet() { // `@`
  scan();
  DictRef d = DEFAULT_DICT;
  U4 value = Dict_get(d, tokenLen, tokenBuf);
  WS_PUSH(value);
}

/*fn*/ void cWriteHeap() { // `,`
  U4 value = WS_POP();
  store(mem, *env.heap, value, env.szI);
  *env.heap += szIToSz(env.szI);
}

/*fn*/ void cWriteInstr() { // `%`
  U1 instr = scanInstr();
  store(mem, *env.heap, (U1)instr, SzI1);
  *env.heap += 1;
}

/*fn*/ void cExecuteInstr() { // ^
  U1 instr = scanInstr();
  env.ep = 1;
  executeInstr(instr);
}

/*fn*/ void cExecute() { // $
  scan();
  DictRef d = DEFAULT_DICT;
  U4 metaRef = Dict_get(d, tokenLen, tokenBuf);
  if(TY_FN_SMART & metaRef) {
    WS_PUSH(FALSE); // pass asInstant=FALSE
  }
  WS_PUSH(REF_MASK & metaRef);
  if(TY_FN_LARGE & metaRef) execute(SzI4 + XW);
  else                       execute(SzI4 + XSW);
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
      eprint("!! Invalid ASM token: ");
      fwrite(tokenBuf, 1, tokenLen, stderr);
      eprint("\n");
      SET_ERR(E_cToken);
  }
  return FALSE;
}

/*fn*/ void compileLoop() {
  jmp_buf* prev_err_jmp = err_jmp;
  jmp_buf local_err_jmp;
  err_jmp = &local_err_jmp;

  if(setjmp(local_err_jmp)) {
    eprintf("\n\n!!! ERROR (stderr): 0x%X\n\n", *env.err);
    zoab_err(*env.err, FALSE);
  } else {
    while(TRUE) {
      scan();
      if(compile()) break;
    }
  }

  err_jmp = prev_err_jmp;
}

DictRef popDictRef() {
  U1 isLocal = WS_POP();
  U4 rHeap = WS_POP();
  U4 buf = WS_POP();
  return (DictRef) {
    .heap = (U2*) (mem + rHeap),
    .buf = buf,
    .end = dict->end,
    .isLocal = isLocal,
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
    U4 offset = Dict_find(d, tokenLen, tokenBuf);
    if(offset == *d.heap) {
      WS_PUSH(0); // not found
      return;
    }
    WS_PUSH(d.buf + offset);
  } else Dict_forget(tokenLen, tokenBuf);
}

void deviceOpCatch() {
  // cache ep, call and local stack.
  U4 ep = env.ep;
  U4 cs_sp = env.cs.sp;
  U4 ls_sp = env.ls.sp;

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
  U4 out = *env.err;
  *env.err = 0;
  WS_PUSH(out);
}

void deviceOpCompile() {
  ASM_ASSERT(!compile(), E_cRet);
}

void deviceOpMemMove() { // {dst src len} -> {}
  U2 len = WS_POP();
  APtr src = WS_POP();
  APtr dst = WS_POP();
  memmove(mem + dst, mem + src, len);
}

void deviceOpMemCmp() { // {a b len} -> {cmp}
  U2 len = WS_POP();
  APtr b = WS_POP();
  APtr a = WS_POP();
  WS_PUSH((U4)(I4) memcmp(mem+a, mem+b, len));
}

// Useful when printing is not working.
// void dbgRaw(U4 len, U1* data) {
//   if (len > 63) return;
//   for(U2 i = 0; i < len; i++) eprintf("%2X ", data[i]);
//   eprint("\n Str: ");
//   for(U2 i = 0; i < len; i++) eprintf("%2c ", data[i]);
//   eprint("\n");
// }

void deviceOpCom(U1 isFetch) { // {len ref -> ioRes}
  if (isFetch) {
    assert(FALSE); // not impl
  } else {
    U4 ref = WS_POP();
    U4 len = WS_POP();
    if(!len) {
      // WS_PUSH(0);
      return;
    }
    ASM_ASSERT(len <= 0x1000, E_io);
    U4 chk = fwrite(mem + ref, 1, len, stdout);
    ASM_ASSERT(chk == len, E_io);
    // WS_PUSH(0);
  }
}

void deviceOpComDone() { // {len ref -> ioRes}
  fflush(stdout);
}

typedef enum {
  CharNoEscape = 1,
  CharEscapeKnown = 2,
  CharEscapeUnknown = 3,
} CharStatus;

typedef struct { U1 c; CharStatus status; } CharResult;

/*fn*/ CharResult parseChar() {
  CharResult out = {.c = tokenBuf[tokenLen], .status = CharNoEscape};
  tokenLen += 1;
  if (out.c != '\\') {
    return out;
  }
  out.status = CharEscapeKnown;
  if (tokenLen >= tokenBufSize) readNewAtLeast(1);
  ASM_ASSERT(tokenBufSize > 0, E_cStr);
  out.c = tokenBuf[tokenLen];
  tokenLen += 1;
  switch (out.c) {
    case 'n': out.c = '\n'; break;
    case 't': out.c = '\t'; break;
    case 's':
    case ' ': out.c = ' '; break;
    case '\\': out.c = '\\'; break;
    case '|': out.c = '|'; break;
    case '{': out.c = '{'; break;
    case '}': out.c = '}'; break;
    case 'x':
      if (tokenLen >= tokenBufSize) readNewAtLeast(2);
      ASM_ASSERT(tokenBufSize >= 2, E_cStr);
      out.c =
          (charToHex(tokenBuf[tokenLen]) << 4)
          + charToHex(tokenBuf[tokenLen + 1]);
      tokenLen += 2;
    default:
      out.status = CharEscapeUnknown;
  }
  return out;
}

void ignoreWhitespace() {
  while(TRUE) {
    if (tokenLen >= tokenBufSize) readNewAtLeast(1);
    if (tokenBuf[tokenLen] > 0x20) return;
    tokenLen += 1;
  }
}

// Parse the str into buffer, returning the end of the str.
/*fn*/ APtr parseStr(APtr buffer, U4 maxLen) { // |
  U1 len = 0;
  APtr start = buffer; // start of current segment.
  buffer += 1; // reserve space for first "count" byte
  ignoreWhitespace();
  while(TRUE) {
    if (tokenLen >= tokenBufSize) readNewAtLeast(1);
    if (tokenBufSize == 0) {
      store(mem, start, len, SzI1);
      return buffer;
    }
    CharResult r;
    r = parseChar();
    switch (r.status) {
      case CharNoEscape:
        switch (r.c) {
          case '{':
          case '}': tokenLen = 0; // rewind token, intentional fallthrough
          case '|':
            store(mem, start, len, SzI1);
            return buffer;
        }
        break;
      case CharEscapeKnown: break; // known escapes: use as-is
      case CharEscapeUnknown:
        switch (r.c) {
          case '\n': ignoreWhitespace(); continue; // \<newline>
          default: SET_ERR(E_cStr);
        }
        break;
    }
    if (len >= 63) {
      // Overflow of single segment, make join segment and start anew.
      store(mem, start, ZOAB_JOIN + len, SzI1);
      start = buffer;
      buffer += 1;
      len = 0;
    }
    store(mem, buffer, r.c, SzI1);
    len += 1;
    buffer += 1;
  }
}

// {&buf, maxLen} Parse zoa string fragment into a buffer.
// return the new end of the buffer.
void deviceOpZoa() {
  U4 maxLen = WS_POP();
  APtr buffer = WS_POP();
  WS_PUSH(parseStr(buffer, maxLen));
}

void deviceOpDictDump(U1 isFetch) {
  APtr entry = 0;
  if (isFetch) entry = WS_POP();
  DictRef d = popDictRef();
  if (isFetch) return zoab_dict(d, entry - d.buf); // single entry

  U2 offset = 0;
  while(offset < *d.heap) {
    zoab_dict(d, offset);
    Key* key = Dict_key(d, offset);
    offset += alignAPtr(keySizeWLen(Key_len(key)), 4);
  }
}

void deviceOpComZoab(U1 isFetch) {
  if(isFetch) {
    U4 i = WS_POP();
    zoab_int(i);
  }
  else {  // {len aptr join}
    U1 join = WS_POP();
    APtr aptr = WS_POP();
    U2 len = WS_POP();
    zoab_data(len, mem + aptr, join);
  }
}

// Device Operations
// Note: this must be declared last since it ties into the compiler infra.
void deviceOp(Bool isFetch, SzI szI, U1 sz) {
  U4 op = WS_POP();
  U4 tmp;
  switch(op) {
    case D_read: WS_PUSH(readAtLeast(WS_POP())); break;
    case D_scan: scan(); break;
    case D_dict: deviceOp_dict(isFetch); break;
    case D_rdict: deviceOpRDict(isFetch); break;
    case D_comp: deviceOpCompile(); break;
    case D_assert:
      tmp = WS_POP();
      if(!tmp) SET_ERR(E_cErr);
      if(!WS_POP()) SET_ERR(tmp);
      env.errData->valTy = ERR_DATA_NONE;
      break;
    case D_wslen: WS_PUSH(WS_LEN); break;
    case D_cslen: WS_PUSH(Stk_len(env.cs)); break;
    case D_xCatch : deviceOpCatch(); break;
    case D_memMove: deviceOpMemMove(); break;
    case D_memCmp: deviceOpMemCmp(); break;
    case D_com: deviceOpCom(isFetch); break;
    case D_zoa: deviceOpZoa(); break;
    case D_dictDump: deviceOpDictDump(isFetch); break;
    case D_comZoab: deviceOpComZoab(isFetch); break;
    case D_comDone: deviceOpComDone(); break;
    default: SET_ERR(E_cDevOp);
  }
}

// ********************************************
// ** Initialization

U4 max(U4 a, U4 b) { if (a > b) return a; return b; }

// Read at least num bytes into tokenBuf.
// If EOF is reached return the number of bytes read.
// If this would overflow tokenBuf, return the number of bytes actually read.
U1 readSrcAtLeast(U1 num) {
  U1 out = 0;
  num = min(TOKEN_BUF, num);
  assert(num);
  while (TRUE) {
    ssize_t numRead = read(
      fileno(srcFile),                      // filedes
      tokenBuf + tokenBufSize,              // buf
      max(num, TOKEN_BUF - tokenBufSize));  // nbyte
    assert(!errno);
    assert(numRead >= 0);
    tokenBufSize += numRead;
    out += numRead;
    if(!numRead) break; // EOF
    if(TOKEN_BUF - tokenBufSize == 0) break;
    if(numRead >= num) break;
  }
  // eprintf("??? readSrcAtLeast read bytes=%3u: ", out);
  // fwrite(tokenBuf + tokenBufSize - out, 1, out, stderr);
  // eprint("\n");
  return out;
}

#define NEW_ENV_BARE(MS, WS, RS, LS, DS, GS)  \
  dbgCount = 0;                           \
  U1 localMem[MS] = {0};                  \
  U1 wsMem[WS];                           \
  U1 callStkMem[RS];                      \
  memset(&localMem, 0, MS);               \
  memset(&wsMem, 0, MS);               \
  memset(&callStkMem, 0, MS);               \
  mem = localMem;                         \
  env = (Env) {                           \
    .ep = 1,                              \
    .mp = 0,                              \
    .gb = 0,                              \
    .heap =    (APtr*) (mem + 0x4),       \
    .topHeap = (APtr*) (mem + 0x8),       \
    .topMem =  (APtr*) (mem + 0xC),       \
    .err =     (U4*) (mem + 0x10),        \
    .state =   (U4*) (mem + 0x14),        \
    .testIdx = (U4*) (mem + 0x18),        \
    .sysLogLvl   = (U2*) (mem + 0x1C),    \
    .usrLogLvl   = (U2*) (mem + 0x1E),    \
    .ls = { .size = LS, .sp = LS },       \
    .ws = { .size = WS, .sp = WS, .mem = wsMem },  \
    .cs = \
      { .size = RS, .sp = RS, .mem = callStkMem }, \
    .szI = SzI4, \
  };                                      \
  *env.sysLogLvl = startingSysLogLvl;     \
  *env.usrLogLvl = startingUsrLogLvl;     \
  /* configure heap+topheap */            \
  *env.heap = GS; /*bottom is globals*/   \
  APtr glbls = 0x1E + 2;                  \
  dict = (Dict*) (mem + glbls);           \
  dict->heap = 0;                         \
  dict->end = DS;                         \
  glbls += sizeof(Dict);                  \
  assert(0x2C == glbls);                  \
  /* Then Token State*/                   \
  tokenState = (TokenState*) (mem + glbls); \
  glbls += sizeof(TokenState);            \
  assert(0x34 == glbls);                  \
  /* Then errData*/                       \
  env.errData = (ErrData*) (mem + glbls); \
  memset(env.errData, 0, sizeof(ErrData));\
  glbls += sizeof(ErrData);               \
  assert(0x48 == glbls);                  \
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
  /*           MS       WS     RS     LS     DICT    GS*/    \
  NEW_ENV_BARE(0x10000, 0x40, 0x100, 0x200, 0x2000, 0x100)


void setCompileFile(char* s) {
  zoab_file(strlen(s), s);
  line = 1;
  readAtLeast = &readSrcAtLeast;
  srcFile = fopen(s, "rb");
  assert(srcFile > 0);
}

void compileFile(char* s) {
  setCompileFile(s);
  compileLoop(); ASSERT_NO_ERR();
}

#define NEW_ENV(MS, WS, RS, LS, DS, GS) \
  NEW_ENV_BARE(MS, WS, RS, LS, DS, GS); \
  WS_PUSH(*env.heap); \
  compileFile("spor.sp");

#define SMALL_ENV \
  /*      MS      WS     RS     LS     DICT    GS*/    \
  NEW_ENV(0x8000, 0x40, 0x100, 0x200, 0x2000, 0x1000)

// ********************************************
// ** Main
void tests();

U4 strToHex(U1 len, U1* s) {
  U4 out = 0;
  for(U1 i = 0; i < len; i++) {
    out = (out << 4) + charToHex(s[i]);
  }
  return out;
}

/*fn*/ int main(int argc, char** argv) {
  if(argc != 4) return 1;

  U1 runTests        = '0' !=      argv[1][0];
  startingSysLogLvl  = strToHex(2, argv[2]);
  startingUsrLogLvl  = strToHex(2, argv[3]);
  eprintf("sysLog=%X  usrLog=%X\n", startingSysLogLvl, startingUsrLogLvl);

  if(runTests) tests();

  return 0;
}

// ********************************************
// ** Testing

Bool _dbgMemInvalid(SzI szI, APtr aptr) {
  U1 sz = szIToSz(szI);
  if (aptr != alignAPtr(aptr, sz))  return TRUE;
  if (aptr == 0)                    return TRUE;
  if (aptr + sz > *env.topHeap)     return TRUE;
  return FALSE;
}

void dbgJmp(Instr instr) {
  U4 jloc = 0;
  SzI szI = INSTR_SZI(instr);

  switch (INSTR_NO_SZ(instr)) {
    case JMPL:
    case JZL:
    case XL:
    case XSL:
      if(_dbgMemInvalid(szI, env.ep)) break;
      jloc = toAptr(fetch(mem, env.ep, szI), SzI2);
      break;
    case JMPW:
    case XW:
    case XSW:
      if (!WS_LEN) return;
      jloc = fetch(env.ws.mem, env.ws.sp, SzIA); break;
  }
  zoab_start();
  zoab_arr(5, FALSE);
  zoab_int(LOG_SYS | LOG_JMP);  zoab_int(instr);
  zoab_int(jloc); zoab_ws(WS_LEN); zoab_int(X_DEPTH);
}

void dbgMem(Instr instr) {
  SzI szI;
  switch (INSTR_NO_SZ(instr)) {
    case LIT: szI = INSTR_SZI(instr); break;
    case FTLL:
    case SRLL: szI = SzI1; break;
    case FTGL:
    case SRGL: szI = SzI2; break;
    default: return;
  }
  if(_dbgMemInvalid(szI, env.ep)) return;
  zoab_arr(3, FALSE);
  zoab_int(LOG_SYS | LOG_MEM);  zoab_int(instr);
  zoab_int(fetch(mem, env.ep, szI));
  return;
}

void dbgInstr(Instr instr, Bool lit) {
  SzI szI;

  if (instr == RET || (instr == RETZ && WS_LEN && (WS_TOP() == 0))) {
      zoab_start();   zoab_arr(5, FALSE);
      zoab_int(LOG_SYS | LOG_RET_SUCCESS);  zoab_int(instr);
      if(env.cs.sp == env.cs.size) zoab_int(0);
      else zoab_int(fetch(env.cs.mem, env.cs.sp, SzI4));
      zoab_ws(WS_LEN); zoab_int(X_DEPTH);
      return;
  }

  switch (INSTR_CLASS(instr)) {
    case C_OP:
      zoab_start();   zoab_arr(3, FALSE);
      zoab_int(LOG_SYS | LOG_OP); zoab_int(instr); zoab_ws(2);
      return;
    case C_SLIT:
      zoab_start();  zoab_arr(2, FALSE);
      zoab_int(LOG_SYS | LOG_LIT);  zoab_int(0x3F & instr);
      return;
    case C_MEM: return dbgMem(instr);
    case C_JMP: return dbgJmp(instr);
  }
}

static inline Bool isExecute(Instr instr) {
  switch ((~SZ_MASK) & instr) {
    case JMPL: return (SZ_MASK & instr) != SzI1;
    case JMPW:
    case XL:
    case XW:
    case XSL:
    case XSW: return TRUE;
    default: return instr == RET;
  }
}

static inline Bool doLogInstr(Instr instr) {
  if(LOG_INSTR == (LOG_INSTR & *env.sysLogLvl)) return TRUE;
  if((LOG_EXECUTE == (LOG_EXECUTE & *env.sysLogLvl))
     && isExecute(instr)) return TRUE;
  return FALSE;
}

static inline void logInstr(Instr instr) {
  dbgCount += 1;
  if(!doLogInstr(instr)) return;
  dbgInstr(instr, TRUE);
}

#define TEST_ENV_BARE \
  SMALL_ENV_BARE; \
  U4 heapStart = *env.heap

#define TEST_ENV \
  SMALL_ENV \
  U4 heapStart = *env.heap


char* testBuf = NULL;
U2 testBufIdx = 0;

// Note: `n` is completely ignored, just fill the buffer if possible.
/*test*/ U1 testingReadAtLeast(U1 n) {
  U1 numRead = 0;
  while (tokenState->size < TOKEN_BUF) {
    U1 c = testBuf[testBufIdx];
    if(c == 0) return numRead;
    tokenBuf[tokenBufSize] = c;
    tokenBufSize += 1;
    testBufIdx += 1;
    numRead += 1;
  }
  return numRead;
}

/*test*/ ssize_t testingRead() {
  return testingReadAtLeast(1);
}

void compileStr(char* s) {
  zoab_file(7, "RAW_STR");
  testBuf = s;
  testBufIdx = 0;
  line = 1;
  readAtLeast = &testingReadAtLeast;
  compileLoop(); ASSERT_NO_ERR();
}

// ********************************************
// ** Tests

/*test*/ void testHex() {
  TEST_ENV_BARE;
  zoab_info("## testDictDeps");

  compileStr(".1 #10");
  assert(WS_POP() == 0x10);

  compileStr("/comment\n.2 #10AF");
  U4 result = WS_POP();
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
  TEST_ENV_BARE;
  zoab_info("## testDictDeps");
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
  U4 result = Dict_get(d, 3, "foo");
  assert(result == 0xF00);

  // assert(12 == Dict_set(d, 5, "bazaa", 0xBA2AA));
  // result = Dict_get(d, 5, "bazaa");
  // assert(result == 0xBA2AA);
}

/*test*/ void testDict() {
  TEST_ENV_BARE;
  zoab_info("## testDict");

  compileStr(".2 #0F00 =foo  .4 #000B_A2AA =bazaa"
      " @bazaa @foo .2 @foo");
  assert(0xF00 == WS_POP());   // 2foo
  assert(0xF00 == WS_POP());   // 4foo
  assert(0xBA2AA == WS_POP()); // 4bazaa
}

/*test*/ void testWriteHeap() { // test , and ;
  TEST_ENV_BARE;
  zoab_info("## testWriteHeap");
  compileStr(".4 #77770101, .2 #0F00, .1 #0,");
  assert(0x77770101 == fetch(mem, heapStart, SzI4));
  assert(0x0F00 == fetch(mem, heapStart+4, SzI2));
  assert(0 == fetch(mem, heapStart+6, SzI1));
}

void assertNoWs() {
  if(WS_LEN) { zoab_start(); zoab_ws(WS_LEN); assert(FALSE); }
}

/*test*/ void testSpore() {
  eprint("## Test Spore\n");
  TEST_ENV;
  zoab_info("## testSpore");
  // TODO: refactor
  // char* logit = "\nTEST printing works!\n";
  // memcpy(mem + *env.heap, logit, strlen(logit));
  // WS_PUSH(*env.heap);  WS_PUSH(strlen(logit));
  // deviceOpCom();
  // assert(!WS_POP());

  // Test zoa str
  heapStart = *env.heap;
  compileStr("$| \\nzoa!\\n|");
  assert(6 == fetch(mem, heapStart, SzI1));
  assert(0 == memcmp("\nzoa!\n", mem + heapStart + 1, 6));
  assertNoWs();

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
  U4 v1 = fetch(mem, heapStart, SzI1);
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

  compileFile("tests/testSpore.sp");
  compileLoop(); ASSERT_NO_ERR();
}

/*test*/ void testFngi() {
  eprint("## testFngi\n");
  TEST_ENV;
  zoab_info("## testFngi");
  eprint("Compiling fngi.fn\n");
  compileFile("fngi.fn");
  fclose(srcFile);
  assertNoWs();
  eprint("Compiling testFngi.fn\n");
  compileFile("tests/testFngi.fn");
  fclose(srcFile);

  assertNoWs();
}

/*test*/ void tests() {
  assert(20 == sizeof(ErrData));
  if (LOG_INFO & startingUsrLogLvl) zoab_infoStart("++ Starting Tests ++", 0);

  testHex();
  testDictDeps();
  testDict();
  testWriteHeap();
  testSpore();
  testFngi();

  assert(0 == WS_LEN);
  if (LOG_INFO & startingUsrLogLvl) zoab_infoStart("++ Tests Complete ++", 0);
  eprint("++ Tests Complete\n");

  zoab_start(); zoab_arr(2, FALSE);
  zoab_int(LOG_USER); zoab_ntStr("Hello world from spor.c!\n", FALSE);
}
