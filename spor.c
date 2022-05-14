// Spor binary kernel.
// This C file implements spor, see ./spor.md for documentation.
//
// The executable produced is intended to be run by a harness,
// see ./harness.md and ./fngi for details.

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include <errno.h>
#include "linux/constants.h"
#include "linux/kernel.h"

typedef U1 Instr;

// print to stderr
#define eprint(str)   fprintf (stderr, str)
#define eprintf(format, args...)   fprintf (stderr, format, args)

typedef enum {
  Ev_log = 0, Ev_file = 1, Ev_dict = 2, Ev_err = 3, Ev_jmp = 4, Ev_ret = 5,
} EventVar;

// ********************************************
// ** Core Types

// 256 64k module blocks
/*get module*/ #define MOD_HIGH_MASK 0xFF0000

#define APO2  2

#ifndef FALSE
#define TRUE 1
#define FALSE 0
#endif

#define BLOCK_PO2 12   // 4KiB block
#define ZOAB_JOIN 0x80
#define ZOAB_ARR  0x40

#define SET_ERR(E)  if(TRUE) { assert(E); env.g->err = E; longjmp(*err_jmp, 1); }
#define ASSERT_NO_ERR()    assert(!env.g->err)
#define ASM_ASSERT(C, E)   if(!(C)) { SET_ERR(E); }

typedef enum {
  C_OP   = 0x00, C_SLIT = 0x40, C_JMP =  0x80, C_MEM =  0xC0,
} InstrClass;

typedef enum { SzI1 = 0x00, SzI2 = 0x10, SzI4 = 0x20, } SzI;

#define SZ_MASK        0x30
#define INSTR_CLASS(INSTR) (InstrClass)(0xC0 & INSTR)
#define INSTR_NO_SZ(INSTR)  (Instr)(~SZ_MASK & (U1)INSTR)
#define INSTR_SZI(INSTR) ((SzI) (INSTR & SZ_MASK))

#define SzIA SzI4

#define LS_SP           (env.ls.mem - mem + env.ls.sp)
#define CS_SP           (env.cs.mem - mem + env.cs.sp)
#define ENV_MOD_HIGH()  (MOD_HIGH_MASK & env.ep)

#define DEFAULT_DICT (&env.g->dict)
#define isLocalDict(dr) (&env.g->dict == dr)

typedef struct {
  U4 value;
  U1 meta;
  U1 len;
  U1 s[];
} Key;

#define keySizeWLen(LEN)  (4 + 1 + 1 + (LEN))
#define Dict_key(D, OFFSET)  ((Key*) (mem + D->ref + (OFFSET)))

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

// Debugging
static inline void logInstr(Instr instr);
void dbgInstr(Instr instr, Bool lit);
void dbgWs(); void dbgFn();

// ********************************************
// ** Globals

// From cmdline
U2 startingSysLogLvl = 0;
U2 startingUsrLogLvl = 0;

void deviceOp(Bool isFetch, SzI szI, U1 sz);

Env env;
U1* mem = NULL;
FILE* srcFile;
U1 (*readAtLeast)(U1 num) = NULL; // Read num bytes into tokenBuf
U4 line = 1;
jmp_buf* err_jmp;
long long unsigned int dbgCount = 0;

// ********************************************
// ** Block Allocator Glue
void S_BA_update(S_BlockAllocator* sba, BlockAllocator* ba)
  /* do */ { sba->cap = ba->cap; sba->rooti = ba->rooti; }

void S_BBA_update(S_BlockBumpArena* sbba, BlockBumpArena* bba)
  /* do */ { sbba->rooti = bba->rooti; sbba->len = bba->len;
             sbba->cap = bba->cap; }

BBAReturn S_BBA_alloc(S_BlockBumpArena* s_bba, U1 aligned, U2 size) {
  // TODO: check validity of BBA memory regions
  BlockAllocator ba = BA_fromS((S_BlockAllocator*) (mem + s_bba->ba));
  BlockBumpArena bba = BBA_fromS(*s_bba, &ba);
  BBAReturn ret;
  if (aligned)  ret = BBA_alloc(&bba, size);
  else          ret = BBA_allocUnaligned(&bba, size);
  S_BA_update((S_BlockAllocator*) (mem + s_bba->ba), &ba);
  S_BBA_update(s_bba, &bba);
  return ret;
}

// ********************************************
// ** Zoab: See https://github.com/civboot/zoa

U1 outbuf[16];
U1* START_BYTES = "\x80\x03";

// Start a zoab log entry.
void zoab_start() { ASM_ASSERT(fwrite(START_BYTES, 1, 2, stdout), E_io); }

void writeOutbuf(U2 len) {
  U4 out = fwrite(outbuf, 1, len, stdout);
}

// Write data of length and join bit.
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

// Start an array of length and join bit.
void zoab_arr(U1 len, U1 join) {
  if(join) len = ZOAB_ARR | ZOAB_JOIN | len;
  else len = ZOAB_ARR | len;
  outbuf[0] = len;
  writeOutbuf(1);
}
void zoab_int(U4 value) { // Write an integer (bigendian)
  // TODO: why start at 8?
  outbuf[8]  = value >> 24;
  outbuf[9]  = value >> 16;
  outbuf[10] = value >> 8;
  outbuf[11] = value;
  if      (value <= 0xFF)     zoab_data(1, outbuf+11, FALSE);
  else if (value <= 0xFFFF)   zoab_data(2, outbuf+10, FALSE);
  else if (value <= 0xFFFFFF) zoab_data(3, outbuf+9, FALSE);
  else                        zoab_data(4, outbuf+8, FALSE);
}

void zoab_ntStr(U1* str, U1 join) { zoab_data(strlen(str), str, join); }
void zoab_arrStart(U1 len) { zoab_start(); zoab_arr(len, FALSE); }
void zoab_enumStart(U1 var) { zoab_arrStart(2); zoab_int(var); }
void zoab_struct(U1 posArgs) { zoab_arr(posArgs + 1, FALSE); zoab_int(posArgs); }


void zoab_file(U2 len, char* file) {
  zoab_enumStart(Ev_file); zoab_struct(1); zoab_data(len, file, FALSE);
}

void zoab_dict(Dict* d, U4 offset) {
  Key* key = Dict_key(d, offset);
  zoab_enumStart(Ev_dict);
  zoab_struct(6);
  zoab_data(Key_len(key), (char *)key->s, /*join=*/ FALSE); // key
  zoab_int(key->value);
  zoab_int(d->ref);    zoab_int(offset);
  zoab_int(key->meta); zoab_int(isLocalDict(d));
}

void zoab_infoStart(U1* str, U2 extraLen) {
  zoab_enumStart(Ev_log); zoab_struct(2); zoab_int(LOG_INFO); zoab_ntStr(str, FALSE);
}

void zoab_info(U1* str) {
  if(LOG_INFO & env.g->usrLogLvl) {
    zoab_infoStart(str, 0);
    fflush(stdout);
  }
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

/*fn*/ APtr alignAPtr(APtr aPtr, U4 sz) {
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
  ASM_ASSERT(aptr < env.g->topMem, E_oob);
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
  ASM_ASSERT(aptr < env.g->topMem, E_oob);
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

/*fn*/ U4 fetchBE(U1* mem, APtr aptr, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < env.g->topMem, E_oob);
  switch (szI) {
    case SzI1: return *(mem + aptr);
    case SzI2: return
        (*(mem + aptr) << 8)
        + *(mem+aptr + 1);
    case SzI4: return
          (*(mem + aptr) << 24)
        + (*(mem+aptr + 1) << 16)
        + (*(mem+aptr + 2) << 8)
        + (*(mem+aptr + 3) << 0);
    default: SET_ERR(E_cSz);
  }
}

void storeBE(U1* mem, APtr aptr, U4 value, SzI szI) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < env.g->topMem, E_oob);
  switch (szI) {
    case SzI1:
      *(mem+aptr) = (U1)value;
      return;
    case SzI2:
      *(mem+aptr) = (U1)(value >> 8);
      *(mem+aptr+1) = (U1)(value);
      return;
    case SzI4:
      *(mem+aptr) = (U1)(value >> 24);
      *(mem+aptr+1) = (U1)(value >> 16);
      *(mem+aptr+2) = (U1)(value >> 8);
      *(mem+aptr+3) = (U1)(value);
      return;
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

/*fn*/ void Stk_pushU1(Stk* stk, U1 value) {
  _CHK_GROW(stk, 1);
  store(stk->mem, stk->sp - 1, value, SzI1);
  stk->sp -= 1;
}

/*fn*/ U4 Stk_popU1(Stk* stk) {
  ASM_ASSERT(stk->sp + 1 <= stk->size, E_stkUnd);
  U4 out = fetch(stk->mem, stk->sp, SzI1);
  stk->sp += 1;
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

// Dump the call/locals stack
void zoab_cs() { zoab_data(X_DEPTH << APO2, env.cs.mem + env.cs.sp, /*join=*/ FALSE); }
void zoab_lsSz() { zoab_data(X_DEPTH, env.lsSz.mem + env.lsSz.sp, /*join=*/ FALSE); }
void zoab_ls() { zoab_data(Stk_len(env.ls) << APO2, env.ls.mem + env.ls.sp, /*join=*/ FALSE); }

void zoab_err(U4 err, U1 isCaught) {
  zoab_enumStart(Ev_err); zoab_struct(8);
  zoab_int(err);
  zoab_int(isCaught);
  zoab_int(env.ep);
  zoab_int(line);
  switch (env.g->errData.valTy) {
    case ERR_DATA_NONE:
      zoab_arr(0, FALSE);
      break;
    case ERR_DATA_INT2:
      zoab_arr(2, FALSE);
      zoab_int(env.g->errData.valueA);
      zoab_int(env.g->errData.valueB);
      break;
    default: assert(FALSE);
  }
  zoab_cs();
  zoab_lsSz();
  zoab_ls();
  fflush(stdout);
}

// ********************************************
// ** Executing Instructions

void xImpl(APtr aptr) { // impl for "execute"
  // get amount to grow, must be multipled by APtr size .
  U2 growLs = fetch(mem, aptr, SzI1);
  Stk_grow(&env.ls, growLs << APO2);
  Stk_push(&env.cs, env.ep);
  Stk_pushU1(&env.lsSz, (U1) growLs);
  env.ep = aptr + 1;
}

void xsImpl(APtr aptr) { // impl for "execute small"
  Stk_push(&env.cs, env.ep);
  Stk_pushU1(&env.lsSz, (U1) 0);
  env.ep = aptr;
}

U4 popLit(SzI szI) {
  U4 out = fetchBE(mem, env.ep, szI);
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

#define GOTO_SZ(J, I, SZ) case SZ + I: szI = SZ; goto J;

inline static void executeInstr(Instr instr) {
  U4 l, r;
  eprintf("??? Instr: %X\n", instr);
  logInstr(instr);
  SzI szI = SzI2;
  switch ((U1)instr) {
    // Operation Cases
    case NOP: return;
    case RETZ:
      if(WS_POP()) return;
      // intentional fallthrough
    case RET:
      r = Stk_pop(&env.cs);
      Stk_shrink(&env.ls, Stk_popU1(&env.lsSz) << APO2);
      env.ep = r;
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
      if (R_LP & r) {
        return WS_PUSH(LS_SP + (0x7F & r)); // local stack pointer + offset
      } else {
        switch (r) {
          case R_EP: WS_PUSH(env.ep); return;
          case R_GB: WS_PUSH(env.gb); return;
          default: SET_ERR(E_cReg);
        }
      }
    case RGSR:
      r = popLit(SzI1);
      if (R_LP & r) {
        env.ls.sp = WS_POP() + (0x7F & r) - (env.ls.mem - mem);
        return;
      } else {
        switch (0xC0 & r) {
          case R_EP: SET_ERR(E_cReg); // SR to EP not allowed
          case R_GB: env.gb = WS_POP(); return;
          default: SET_ERR(E_cReg);
        }
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
    case MSK : r = WS_POP(); WS_PUSH(WS_POP() & r); return;
    case JN  : r = WS_POP(); WS_PUSH(WS_POP() | r); return;
    case XOR : r = WS_POP(); WS_PUSH(WS_POP() ^ r); return;
    case AND : r = WS_POP(); WS_PUSH(WS_POP() && r); return;
    case OR  : r = WS_POP(); WS_PUSH(WS_POP() || r); return;
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
_JMPW: case SzI2 + JMPW:
      env.ep = toAptr(WS_POP(), szI);
      return;
    GOTO_SZ(_JMPW, JMPW, SzI1)
    GOTO_SZ(_JMPW, JMPW, SzI4)
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
_JTBL: case SzI2 + JTBL: assert(FALSE); // TODO: not impl
    GOTO_SZ(_JTBL, JTBL, SzI1)
    GOTO_SZ(_JTBL, JTBL, SzI4)
_XLL: case SzI2 + XLL:
      return xImpl(toAptr(popLit(szI), szI));
    GOTO_SZ(_XLL, XLL, SzI1)
    GOTO_SZ(_XLL, XLL, SzI4)
_XLW: case SzI2 + XLW:
      return xImpl(toAptr(WS_POP(), szI));
    GOTO_SZ(_XLW, XLW, SzI1)
    GOTO_SZ(_XLW, XLW, SzI4)
_XSL: case SzI2 + XSL:
      return xsImpl(toAptr(popLit(szI), szI));
    GOTO_SZ(_XSL, XSL, SzI1)
    GOTO_SZ(_XSL, XSL, SzI4)
_XSW: case SzI2 + XSW:
      return xsImpl(toAptr(WS_POP(), szI));
    GOTO_SZ(_XSW, XSW, SzI1)
    GOTO_SZ(_XSW, XSW, SzI4)

    // Mem Cases
_FT: case SzI2 + FT:
      return WS_PUSH(fetch(mem, WS_POP(), szI));
    GOTO_SZ(_FT, FT, SzI1)
    GOTO_SZ(_FT, FT, SzI4)
_FTO: case SzI2 + FTO:
      return WS_PUSH(fetch(mem, WS_POP() + popLit(SzI1), szI));
    GOTO_SZ(_FTO, FTO, SzI1)
    GOTO_SZ(_FTO, FTO, SzI4)
_FTLL: case SzI2 + FTLL:
      return WS_PUSH(fetch(mem, LS_SP + popLit(SzI1), szI));
    GOTO_SZ(_FTLL, FTLL, SzI1)
    GOTO_SZ(_FTLL, FTLL, SzI4)
_FTGL: case SzI2 + FTGL:
      return WS_PUSH(fetch(mem, env.gb + popLit(SzI2), szI));
    GOTO_SZ(_FTGL, FTGL, SzI1)
    GOTO_SZ(_FTGL, FTGL, SzI4)
_SR: case SzI2 + SR:
      r = WS_POP(); return store(mem, r, WS_POP(), szI);
    GOTO_SZ(_SR, SR, SzI1)
    GOTO_SZ(_SR, SR, SzI4)
_SRO: case SzI2 + SRO:
      r = WS_POP(); return store(mem, r + popLit(SzI1), WS_POP(), szI);
    GOTO_SZ(_SRO, SRO, SzI1)
    GOTO_SZ(_SRO, SRO, SzI4)
_SRLL: case SzI2 + SRLL:
      return store(mem, LS_SP + popLit(SzI1), WS_POP(), szI);
    GOTO_SZ(_SRLL, SRLL, SzI1)
    GOTO_SZ(_SRLL, SRLL, SzI4)
_SRGL: case SzI2 + SRGL:
      l = WS_POP();
      r = env.gb + popLit(SzI2);
      return store(mem, r, l, szI);
    GOTO_SZ(_SRGL, SRGL, SzI1)
    GOTO_SZ(_SRGL, SRGL, SzI4)
_LIT: case SzI2 + LIT:
      return WS_PUSH(popLit(szI));
    GOTO_SZ(_LIT, LIT, SzI1)
    GOTO_SZ(_LIT, LIT, SzI4)

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

// find key offset from dict. Else return dict.len
/*fn*/ U2 Dict_find(Dict* d, U2 slen, char* s) {
  ASM_ASSERT(slen < 0x40, E_cKeyLen);
  U2 offset = 0;
  assert(d->len < d->cap);
  while(offset < d->len) {
    Key* key = Dict_key(d, offset);
    if(cstrEq(Key_len(key), slen, (char *)key->s, s)) return offset;
    U2 entrySz = alignAPtr(keySizeWLen(Key_len(key)), 4);
    offset += entrySz;
  }
  assert(offset == d->len);
  return offset;
}

/*fn*/ U2 Dict_set(Dict* d, U2 slen, char* s, U4 value, U1 meta) {
  // Set a key to a value, returning the offset
  U2 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset == d->len, E_cKey)
  Key* key = Dict_key(d, offset);
  U2 addedSize = alignAPtr(keySizeWLen(slen), 4);

  ASM_ASSERT(d->len + addedSize <= d->cap, E_DictOvr);
  key->value = value;
  key->meta = meta;
  key->len = slen;
  memcpy(key->s, s, slen);   // memcpy(dst, src, sz)
  if((LOG_COMPILER & env.g->sysLogLvl)) zoab_dict(d, offset);
  d->len += alignAPtr(keySizeWLen(slen), 4);
  return offset;
}

/*fn*/ U4 Dict_get(Dict* d, U2 slen, char* s) {
  U2 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset != d->len, E_cNoKey);
  return Dict_key(d, offset)->value;
}

/*fn*/ void Dict_forget(U2 slen, char* s) {
  Dict* d = DEFAULT_DICT;
  env.g->dict.len = Dict_find(d, slen, s);
}


// ********************************************
// ** Scanner
#define tokenBufSize (env.g->ts.size)
#define tokenLen env.g->ts.len
#define tokenBuf ((char*) mem + env.g->ts.buf)

/*fn*/ TokenGroup toTokenGroup(U1 c) {
  if(c <= ' ') return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '_') return T_ALPHA;
  if(c == '%' || c == '\\' || c == '$' || c == '|' ||
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
  U2 newStart = tokenLen;
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
    if(tokenLen >= tokenBufSize) { readNewAtLeast(1); }
    if(tokenBufSize == 0) return;
    if(toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) break;
    if(tokenBuf[tokenLen] == '\n') line += 1;
    tokenLen += 1;
  }
  shiftBuf(); // Moves buffer to the left (tokenLen=0)
  if(!tokenBufSize) { readAtLeast(1); }
  assert(tokenBufSize);

  U1 c = tokenBuf[tokenLen];
  env.g->ts.group = toTokenGroup(c);
  if(env.g->ts.group == T_SINGLE) {
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
    if (tg == env.g->ts.group) {}
    else if ((env.g->ts.group <= T_ALPHA) && (tg <= T_ALPHA)) {}
    else break;
    tokenLen += 1;
  }
}

U1 scanInstr() {
  Dict* d = DEFAULT_DICT;
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
    if (tokenBufSize == 0) break;
    if (tokenBuf[tokenLen] == '\n') break;
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
  U1 meta = WS_POP();
  U4 value = WS_POP();
  eprintf("??? cDictSet %.*s meta=%X value=%X\n", tokenLen, tokenBuf, meta, value);
  Dict* d = DEFAULT_DICT;
  Dict_set(d, tokenLen, tokenBuf, value, meta);
}

/*fn*/ void cDictGet() { // `@`
  scan();
  Dict* d = DEFAULT_DICT;
  U4 value = Dict_get(d, tokenLen, tokenBuf);
  WS_PUSH(value);
}

void bumpStoreBE(U4 value, SzI szI) {
  BBAReturn ret = S_BBA_alloc(&env.g->bba, /*aligned=*/false, szIToSz(szI));
  ASM_ASSERT(!ret.leftoverSize, E_newBlock);
  ASM_ASSERT(ret.data, E_OOM);
  eprintf("??? bumpStoreBE: allocated for: 0x%X, got mem: 0x%X\n", value, (U1*) ret.data - mem);
  storeBE(mem, (U1*) ret.data - mem, value, szI);
}

/*fn*/ void cWriteHeap() { // `,`
  U4 value = WS_POP();
  bumpStoreBE(value, env.szI);
}

/*fn*/ void cWriteInstr() { // `%`
  U1 instr = scanInstr();
  bumpStoreBE(instr, SzI1);
}

/*fn*/ void cExecuteInstr() { // ^
  U1 instr = scanInstr();
  env.ep = 1;
  executeInstr(instr);
}

void _memmove(APtr dst, APtr src, APtr len) {
  // TODO: do bounds checking
  memmove(mem + dst, mem + src, len);
}

/*fn*/ void cExecute() { // $
  scan();
  Dict* d = DEFAULT_DICT;
  U2 offset = Dict_find(d, tokenLen, tokenBuf);
  ASM_ASSERT(offset != d->len, E_cKey);
  Key* key = Dict_key(d, offset);
  eprintf("??? cExecute: %.*s @%X\n", tokenLen, tokenBuf, key->value);
  if(TY_FN_INLINE == (TY_FN_TY_MASK & key->meta)) {
    U1 len = fetch(mem, key->value, SzI1);
    _memmove(env.g->heap, key->value + 1, len);
    env.g->heap += len;
    return;
  }
  if(TY_FN_SYN == (TY_FN_TY_MASK & key->meta)) WS_PUSH(FALSE); // pass asNow=FALSE
  WS_PUSH(key->value);
  if(TY_FN_LARGE & key->meta) execute(SzI4 + XLW);
  else                        execute(SzI4 + XSW);
}

/*fn*/ Bool compile() {
  if(tokenLen == 0) return TRUE;
  tokenLen = 1; // allows for multi symbols where valid, i.e. =$, $$
  switch (tokenBuf[0]) {
    case '.': cSz(); break;
    case '\\': cComment(); break;
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
    eprintf("\n\n!!! ERROR (stderr): 0x%X\n\n", env.g->err);
    zoab_err(env.g->err, FALSE);
  } else {
    while(TRUE) {
      scan();
      if(compile()) break;
    }
  }

  err_jmp = prev_err_jmp;
}

Dict* popDictRef() { 
  APtr d = WS_POP();
  ASM_ASSERT(d, E_null);
  return (Dict*) (mem + d);
}

void deviceOp_dict(Bool isFetch) {
  Dict* d = popDictRef();
  if(isFetch) {
    WS_PUSH(Dict_get(d, tokenLen, tokenBuf));
  } else {
    U1 meta = WS_POP();
    Dict_set(d, tokenLen, tokenBuf, WS_POP(), meta);
  }
}

void deviceOpRDict(Bool isFetch) {
  Dict* d = popDictRef();
  if(isFetch) {
    U4 offset = Dict_find(d, tokenLen, tokenBuf);
    if(offset == d->len) {
      WS_PUSH(0); // not found
      return;
    }
    WS_PUSH(d->ref + offset);
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
    execute(SzI4 + XLW);
  }

  // ALWAYS Reset ep, call, and local stack and clear WS.
  env.ep = ep;
  env.cs.sp = cs_sp;
  env.lsSz.sp = cs_sp / 4;
  env.ls.sp = ls_sp;
  env.ws.sp = env.ws.size; // clear WS

  // update to old error jmp location
  err_jmp = prev_err_jmp;

  // Push current error to WS and clear it.
  U4 out = env.g->err;
  env.g->err = 0;
  WS_PUSH(out);
}

void deviceOpCompile() {
  ASM_ASSERT(!compile(), E_cRet);
}

// SR: {dst src len}: dst = src (of len)
// FT: {dst value:U1 len}: dst = value (fill len)
void deviceOpMemMove(U1 isFetch) {
  U2 len = WS_POP();
  APtr v = WS_POP();
  APtr dst = WS_POP();
  if(isFetch) memset(mem + dst, v, len);
  else _memmove(dst, v, len);
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
      assert(FALSE);
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
  APtr out = parseStr(buffer, maxLen);
  WS_PUSH(out);
  U1 len = fetch(mem, buffer, SzI1);
  assert(out == buffer + len + 1);
}

void deviceOpDictDump(U1 isFetch) {
  APtr entry = 0;
  if (isFetch) {
    entry = WS_POP();
    ASM_ASSERT(entry, E_null);
  }
  Dict* d = popDictRef();
  if (isFetch) return zoab_dict(d, entry - d->ref); // single entry

  U2 offset = 0;
  while(offset < d->len) {
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

void deviceOpBlock(U1 isFetch) {
  // TODO: check validity of BA memory regions
  BlockAllocator* ba = (BlockAllocator*) (mem + WS_POP());
  U1* clientRooti = mem + WS_POP();
  block_t* b;
  if (isFetch) { // alloc
    b = BA_alloc(ba, clientRooti);
    if(b == NULL) WS_PUSH(0);
    else WS_PUSH((U1*) b - mem);
  } else {
    b = (block_t*) (mem + WS_POP());
    BA_free(ba, clientRooti, b);
  }
}

void deviceOpBump(U1 isFetch) {
  S_BlockBumpArena* s_bba = (S_BlockBumpArena*) (mem + WS_POP());
  BBAReturn ret = S_BBA_alloc(s_bba, isFetch, WS_POP());
  WS_PUSH(ret.leftoverSize);
  if(ret.leftover) WS_PUSH((U1*) ret.leftover - mem); else WS_PUSH(0);
  if(ret.data) WS_PUSH((U1*) ret.data - mem);         else WS_PUSH(0);
}

// Device Operations
// Note: this must be declared last since it ties into the compiler infra.
void deviceOp(Bool isFetch, SzI szI, U1 sz) {
  U4 op = WS_POP();
  U4 tmp;
  switch(op) {
    case D_read:
      WS_PUSH(readAtLeast(WS_POP()));
      break;
    case D_scan:
      if(isFetch) scan();
      else        cComment();
      break;
    case D_dict: deviceOp_dict(isFetch); break;
    case D_dictK: deviceOpRDict(isFetch); break;
    case D_comp: deviceOpCompile(); break;
    case D_assert:
      tmp = WS_POP();
      if(!tmp) SET_ERR(E_cErr);
      if(!WS_POP()) SET_ERR(tmp);
      env.g->errData.valTy = ERR_DATA_NONE;
      break;
    case D_wslen: WS_PUSH(WS_LEN); break;
    case D_cslen: WS_PUSH(Stk_len(env.cs)); break;
    case D_xCatch : deviceOpCatch(); break;
    case D_memSet : deviceOpMemMove(isFetch); break;
    case D_memCmp: deviceOpMemCmp(); break;
    case D_com: deviceOpCom(isFetch); break;
    case D_zoa: deviceOpZoa(); break;
    case D_dictDump: deviceOpDictDump(isFetch); break;
    case D_comZoab: deviceOpComZoab(isFetch); break;
    case D_comDone: deviceOpComDone(); break;
    case D_block: deviceOpBlock(isFetch); break;
    case D_bump: deviceOpBump(isFetch); break;
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
  U4 out = 0;
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
  assert(out <= 0xFF);
  // eprintf("??? readSrcAtLeast numRead=%u: ", out);
  // fwrite(tokenBuf + tokenBufSize - out, 1, out, stderr);
  // eprintf("\n ??? Token len=%u: %.*s", tokenLen, tokenLen, tokenBuf);
  return out;
}
#define ENV_CLEANUP() \
    free(mem); \
    free(env.ws.mem); free(env.cs.mem); \
    free(env.lsSz.mem);

#define NEW_ENV_BARE(MS, WS, RS, LS, DS, GS, BLKS)  \
  assert(MS == alignAPtr(MS, 1 << BLOCK_PO2));      \
  assert(BLKS == alignAPtr(BLKS, 4));               \
  assert(BLKS > 0);                                 \
  assert(                   \
    MS > (                  \
      (BLKS << BLOCK_PO2)   \
      + 0x400               \
      + LS + DS + GS        \
    ));                     \
  U1* wsMem = malloc(WS);      assert(wsMem);       \
  U1* callStkMem = malloc(RS); assert(callStkMem);  \
  U1* lsSzMem = malloc(RS / 4); assert(lsSzMem);    \
  mem = malloc(MS);            assert(mem);         \
  dbgCount = 0;                           \
  memset(mem, 0, MS);                     \
  memset(wsMem, 0, WS);                   \
  memset(callStkMem, 0, RS);              \
  env = (Env) {                           \
    .g = (Globals*) mem,                  \
    .k = (Kern*) (mem + sizeof(Globals)), \
    .ep = 1, .gb = 0,                     \
    .ls = { .size = LS, .sp = LS },       \
    .ws = { .size = WS, .sp = WS, .mem = wsMem },  \
    .cs = \
      { .size = RS, .sp = RS, .mem = callStkMem }, \
    .lsSz = \
      { .size = RS / 4, .sp = RS / 4, .mem = lsSzMem }, \
    .szI = SzI4, \
  };                                      \
  APtr glbls = 0; /* location of globals*/ \
  memset(env.g, glbls, sizeof(Globals));  \
  env.g->gheap = sizeof(Globals);         \
  env.g->usrLogLvl = startingUsrLogLvl;   \
  env.g->sysLogLvl = startingSysLogLvl;   \
  env.g->heap = GS; /*bottom is globals*/ \
  env.g->dict.cap = DS; /*heap=0*/        \
                                          \
  env.g->topMem = MS;                     \
  env.g->topHeap =                        \
    MS                                    \
    - (BLKS << BLOCK_PO2)                 \
    - (BLKS * 2);                         \
  /* Allocators */                        \
  env.g->ba.cap = BLKS;                   \
  env.g->ba.nodes = env.g->topHeap;       \
  env.g->ba.blocks = env.g->ba.nodes + (BLKS * 2); \
  BlockAllocator __ba = BA_fromS(&env.g->ba); \
  BA_init(&__ba);                         \
  S_BA_update(&env.g->ba, &__ba);         \
  env.g->bba.ba = ((U1*) &env.g->ba) - mem; \
  env.g->bba.rooti = BLOCK_END;           \
  /* Reserve space for local stack*/      \
  env.g->topHeap -= LS;                   \
  env.ls.mem = mem + env.g->topHeap;      \
  /* Then dictionary */                   \
  env.g->topHeap -= DS;                   \
  env.g->dict.ref = env.g->topHeap;       \
  /* Then Token Buf*/                     \
  env.g->topHeap -= TOKEN_BUF;            \
  env.g->ts.buf = env.g->topHeap;

#define SMALL_ENV_BARE \
  /*           MS       WS     RS     LS     DICT    GS    BLKS */    \
  NEW_ENV_BARE(0x100000, 0x40, 0x100, 0x200, 0x3000, 0x100, 0x10)

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

#define NEW_ENV(MS, WS, RS, LS, DS, GS, BLKS) \
  NEW_ENV_BARE(MS, WS, RS, LS, DS, GS, BLKS); \
  eprintf("??? heap: %X\n", env.g->heap); \
  WS_PUSH(env.g->heap); \
  WS_PUSH(ASIZE); \
  WS_PUSH(SzIA); \
  compileFile("kernel/kernel_c.sp");
  // compileFile("spor.sp");

#define SMALL_ENV \
  /*      MS      WS     RS     LS     DICT    GS     BLKS*/    \
  NEW_ENV(0x200000, 0x40, 0x100, 0x200, 0x3000, 0x1000, 0x20)

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
  if (aptr + sz > env.g->topHeap)     return TRUE;
  return FALSE;
}

void dbgJmp(Instr instr) {
  U4 jloc = 0;
  SzI szI = INSTR_SZI(instr);

  switch (INSTR_NO_SZ(instr)) {
    case JMPL:
    case JZL:
    case XLL:
    case XSL:
      if(_dbgMemInvalid(szI, env.ep)) break;
      jloc = toAptr(fetchBE(mem, env.ep, szI), SzI2);
      break;
    case JMPW:
    case XLW:
    case XSW:
      if (!WS_LEN) return;
      jloc = fetch(env.ws.mem, env.ws.sp, SzIA); break;
  }
  zoab_arrStart(5); zoab_int(Ev_jmp);
  zoab_int(instr); zoab_int(jloc);
  zoab_ws(WS_LEN); zoab_int(X_DEPTH);
}

void dbgMem(Instr instr) {
  // SzI szI;
  // switch (INSTR_NO_SZ(instr)) {
  //   case LIT: szI = INSTR_SZI(instr); break;
  //   case FTLL:
  //   case SRLL: szI = SzI1; break;
  //   case FTGL:
  //   case SRGL: szI = SzI2; break;
  //   default: return;
  // }
  // if(_dbgMemInvalid(szI, env.ep)) return;
  // zoab_arr(3, FALSE);
  // zoab_int(LOG_SYS | LOG_MEM);  zoab_int(instr);
  // zoab_int(fetchBE(mem, env.ep, szI));
}

void dbgInstr(Instr instr, Bool lit) {
  SzI szI;
  if (instr == RET || (instr == RETZ && WS_LEN && (WS_TOP() == 0))) {
      zoab_arrStart(5); zoab_int(Ev_ret); zoab_int(instr);
      if(env.cs.sp == env.cs.size) zoab_int(0); // jloc=0 if empty
      else zoab_int(fetch(env.cs.mem, env.cs.sp, SzI4)); // jloc
      zoab_ws(WS_LEN); zoab_int(X_DEPTH);
      return;
  }
  assert(FALSE);

  // switch (INSTR_CLASS(instr)) {
  //   case C_OP:
  //     zoab_start();   zoab_arr(3, FALSE);
  //     zoab_int(LOG_SYS | LOG_OP); zoab_int(instr); zoab_ws(2);
  //     return;
  //   case C_SLIT:
  //     zoab_start();  zoab_arr(2, FALSE);
  //     zoab_int(LOG_SYS | LOG_LIT);  zoab_int(0x3F & instr);
  //     return;
  //   case C_MEM: return dbgMem(instr);
  //   case C_JMP: return dbgJmp(instr);
  // }
}

static inline Bool isExecute(Instr instr) {
  switch ((~SZ_MASK) & instr) {
    case JMPL: return (SZ_MASK & instr) != SzI1;
    case JMPW:
    case XLL:
    case XLW:
    case XSL:
    case XSW: return TRUE;
    default: return instr == RET;
  }
}

static inline Bool doLogInstr(Instr instr) {
  if(LOG_INSTR == (LOG_INSTR & env.g->sysLogLvl)) return TRUE;
  if((LOG_EXECUTE == (LOG_EXECUTE & env.g->sysLogLvl))
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
  U4 heapStart = env.g->heap

#define TEST_ENV \
  SMALL_ENV \
  U4 heapStart = env.g->heap


char* testBuf = NULL;
U2 testBufIdx = 0;

// Note: `n` is completely ignored, just fill the buffer if possible.
/*test*/ U1 testingReadAtLeast(U1 n) {
  U1 numRead = 0;
  while (env.g->ts.size < TOKEN_BUF) {
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
//
/*test*/ void testMem() {
  TEST_ENV_BARE;
  zoab_info("## testMem");
  // Test our calculations on memory locations (hard-coded in spor.sp).
  assert(0x20 == (size_t) &env.g->dict    - (size_t) env.g);
  assert(0x28 == (size_t) &env.g->ldict   - (size_t) env.g);
  assert(0x30 == (size_t) &env.g->ts      - (size_t) env.g);
  assert(0x34 == (size_t) &env.g->ts.len  - (size_t) env.g);
  assert(0x36 == (size_t) &env.g->ts.size - (size_t) env.g);
  assert(0x3C == (size_t) &env.g->errData - (size_t) env.g);
  assert(0x40 == (size_t) &env.g->errData.valueASz - (size_t) env.g);
  assert(0x42 == (size_t) &env.g->errData.valueBSz - (size_t) env.g);
  assert(0x44 == (size_t) &env.g->errData.valueA - (size_t) env.g);
  assert(0x48 == (size_t) &env.g->errData.valueB - (size_t) env.g);
  // assert(0x50 == (size_t) &env.g->ba      - (size_t) env.g);
  assert(0x5C == (size_t) &env.g->gkey    - (size_t) env.g);
  assert(0x60 == (size_t) &env.g->lkey    - (size_t) env.g);
  assert(0x64 == (size_t) &env.g->gheap   - (size_t) env.g);
  assert(0x68 == (size_t) &env.g->localOffset - (size_t) env.g);
}

/*test*/ void testHex() {
  TEST_ENV_BARE;
  zoab_info("## testDictDeps");

  compileStr(".1 #10");
  assert(WS_POP() == 0x10);

  compileStr("\\comment\n.2 #10AF");
  U4 result = WS_POP();
  assert(result == 0x10AF);

  compileStr(".4 #1002_3004");
  result = WS_POP();
  assert(0x10023004 == result);

  // Note: ignores sz
  compileStr(".2 #1002_3004");
  result = WS_POP();
  assert(0x10023004 == result);
  ENV_CLEANUP();
}

/*test*/ void testDictDeps() {
  TEST_ENV_BARE;
  zoab_info("## testDictDeps");
  Dict* d = DEFAULT_DICT;
  assert(cstrEq(1, 1, "a", "a"));
  assert(!cstrEq(1, 1, "a", "b"));

  assert(cstrEq(2, 2, "z0", "z0"));
  assert(!cstrEq(2, 1, "aa", "a"));
  assert(!cstrEq(2, 2, "aa", "ab"));

  assert(0 == Dict_find(d, 3, "foo"));

  // set
  assert(0 == Dict_set(d, 3, "foo", 0xF00, 0));

  // get
  U4 result = Dict_get(d, 3, "foo");
  assert(result == 0xF00);
  ENV_CLEANUP();
}

/*test*/ void testDict() {
  TEST_ENV_BARE;
  zoab_info("## testDict");

  compileStr("#0F00 #0=foo  #000B_A2AA #0=bazaa @bazaa @foo");
  assert(0xF00 == WS_POP());   // 2foo
  assert(0xBA2AA == WS_POP()); // bazaa
  ENV_CLEANUP();
}

/*test*/ void testWriteHeap() { // test , and ;
  TEST_ENV_BARE;
  eprint("## testWriteHeap\n");
  zoab_info("## testWriteHeap");
  heapStart = env.g->ba.blocks + (env.g->bba.rooti << BLOCK_PO2) + env.g->bba.len;
  eprintf("??? heapStart=%X\n", heapStart);
  // Note: comma stores as big-endian
  compileStr("#77770101 .4, #0F00 .2, #0 .1,");
  assert(0x01017777 == fetch(mem, heapStart, SzI4));
  assert(0x000F == fetch(mem, heapStart+4, SzI2));
  assert(0 == fetch(mem, heapStart+6, SzI1));
  assert(FALSE);
  ENV_CLEANUP();
}

void assertNoWs() {
  if(WS_LEN) {
    eprint("! WS not empty\n");
    zoab_start(); zoab_ws(WS_LEN); 
    assert(FALSE);
  }
}

/*test*/ void testSpore() {
  eprint("## Test Spore\n");
  TEST_ENV;
  zoab_info("## testSpore");
  // TODO: refactor
  // char* logit = "\nTEST printing works!\n";
  // memcpy(mem + env.g->heap, logit, strlen(logit));
  // WS_PUSH(env.g->heap);  WS_PUSH(strlen(logit));
  // deviceOpCom();
  // assert(!WS_POP());

  // // Test h1
  // heapStart = env.g->heap;
  // compileStr(".1 #42 ,  #43 $h1");
  // assert(heapStart+2 == env.g->heap);
  // assert(0x42 == fetch(mem, heapStart, SzI1));
  // assert(0x43 == fetch(mem, heapStart + 1, SzI1));

  // // Test L0
  // heapStart = env.g->heap;
  // compileStr("#7 $L0");
  // assert(heapStart+1 == env.g->heap);
  // U4 v1 = fetch(mem, heapStart, SzI1);
  // assert(C_SLIT | 0x7 == fetch(mem, heapStart, SzI1));

  // // Test h2
  // env.g->heap = alignAPtr(env.g->heap, 2);
  // heapStart = env.g->heap;
  // compileStr("#1234 $h2");
  // assert(heapStart+2 == env.g->heap);
  // assert(0x3412 == fetch(mem, heapStart, SzI2));

  // // Test h4
  // env.g->heap = alignAPtr(env.g->heap, 4);
  // heapStart = env.g->heap;
  // compileStr("#987654 $h4");
  // assert(heapStart+4 == env.g->heap);
  // assert(0x54769800 == fetch(mem, heapStart, SzI4));

  // // Test various
  // compileStr("$_h");
  // assert(env.g->heap == WS_POP());

  // compileFile("tests/testSpore.sp");
  compileLoop(); ASSERT_NO_ERR();
  ENV_CLEANUP();
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
  ENV_CLEANUP();
}

/*test*/ void tests() {
  assert(20 == sizeof(ErrData));
  if (LOG_INFO & startingUsrLogLvl) zoab_infoStart("++ Starting Tests ++", 0);

  testMem();
  testHex();
  testDictDeps();
  testDict();
  testWriteHeap();
  testSpore();
  // testFngi();

  assert(0 == WS_LEN);
  if (LOG_INFO & startingUsrLogLvl) zoab_infoStart("++ Tests Complete ++", 0);
  eprint("++ Tests Complete\n");
}

void dbgWs() {
  eprintf("??? WS[%u]:", WS_LEN);
  for(int i = 1; i <= WS_LEN; i++) {
    eprintf(" %X", fetch(env.ws.mem, env.ws.sp + ((WS_LEN - i) * 4), SzI4));
  }
  eprint("\n");
}

void dbgFn(char* pre, APtr rFn) {
  Dict* d = DEFAULT_DICT;
  U2 offset = 0;
  while(offset < d->len) {
    Key* key = Dict_key(d, offset);
    if(rFn == key->value) {
      eprintf("## %s %.*s: ", pre, Key_len(key), key->s); dbgWs();
      break;
    }
    offset += alignAPtr(keySizeWLen(Key_len(key)), 4);
  }
}
