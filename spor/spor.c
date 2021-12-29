
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

// Global Constants
#define CMP_EQ  0x8000 // Comparison was equal. Was less if LT this, etc

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

#define dbg(MSG)  if(TRUE){printf(MSG); dbgEnv();}

// Size
typedef enum { SzI1, SzI2, SzI4 } SzI;
#define SzIA SzI4

// Mem
typedef enum {
  LIT , FT  , FTLL, FTML,
  SR  , SRLL, SRML,
} MemI;

// Jmp
typedef enum {
  RET , JMPL, JMPW,
  JZL , JTBL,
  XL  , XW  , XSL , XSW ,
} JmpI;

#define NOP 0

// Operations
#define SLIT_MASK      0x40
#define JMP_MASK       0x80
#define MEM_MASK       0xC0
#define SZ_SHIFT       4
#define SZI4_MASK      (SzI4 << SZ_SHIFT)

#define INSTR_DEFAULT  0x00

#define OPI1_START 0x10
#define OPI2_START 0x20

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
  Stk callStk;
} Env;

#define LS_OFFSET() (env.ls.mem - mem)
#define ENV_MOD_HIGH()  (env.ep & MOD_HIGH_MASK)

typedef struct {
  U8 escape; // True if RET but no more stack.
} ExecuteResult;

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
  U8 meta;
  U8 len;
  U8 s[];
} Key;
#define keySizeWLen(LEN)  (4 + 1 + 1 + (LEN))

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
void dbgIndent();
void dbgEnv();
void dbgInstr(U8 i);
void dbgWs();
void dbgWsFull();
void dbgJmpI(JmpI j);
void printToken();

// ********************************************
// ** Globals

void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz);

Env env;
U8* mem = NULL;
Dict* dict = NULL;
TokenState* tokenState = NULL;
U8* compilingName;
FILE* srcFile;
SzI instrSzI = SzI1;
U8 globalInstr = NOP;
ssize_t (*readAppend)() = NULL; // Read bytes incrementing tokenBufSize
U32 line = 1;
jmp_buf* err_jmp;

// ********************************************
// ** Utilities

U32 szIToMask(SzI szI) {
  switch (szI) {
    case 0: return 0xFF;
    case 1: return 0xFFFF;
    case 2: return 0xFFFFFFFF;
    default: SET_ERR(E_cSz);
  }
}

U8 szIToSz(SzI szI) {
  switch (szI) {
    case 0: return 1;
    case 1: return 2;
    case 2: return 4;
    default: SET_ERR(E_cSz);
  }
}

SzI szToSzI(U8 sz) {
  switch (sz) {
    case 1: return SzI1;
    case 2: return SzI2;
    case 4: return SzI4;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ void* align(void* p, U8 sz) {
  U8 mod = (size_t)p % sz;
  if(mod == 0) return p;
  return p + (sz - mod);
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

/*fn*/ void store(U8* mem, APtr aptr, U32 value, U8 sz) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (sz) {
    case 1: 
      *(mem+aptr) = (U8)value;
      break;
    case 2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      *((U16*) (mem+aptr)) = (U16)value;
      break;
    case 4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      *((U32*) (mem+aptr)) = value;
      break;
    default: SET_ERR(E_cSz);
  }
}

/*fn*/ U32 fetch(U8* mem, APtr aptr, U8 sz) {
  ASM_ASSERT(aptr, E_null);
  ASM_ASSERT(aptr < *env.topMem, E_oob);
  switch (sz) {
    case 1:
      return (U32) *((U8*) (mem+aptr));
    case 2:
      ASM_ASSERT(aptr % 2 == 0, E_align2);
      return (U32) *((U16*) (mem+aptr));
    case 4:
      ASM_ASSERT(aptr % 2 == 0, E_align4);
      return (U32) *((U32*) (mem+aptr));
    default: SET_ERR(E_cSz);
  }
}

#define Stk_len(STK)  ((STK.size - STK.sp) >> APO2)
#define WS_LEN        Stk_len(env.ws)
#define X_DEPTH       Stk_len(env.callStk)
#define _CHK_GROW(SP, SZ) \
  ASM_ASSERT(SP - SZ >= 0, E_stkOvr)

#define WS_PUSH(VALUE)  Stk_push(&env.ws, VALUE)
/*fn*/ void Stk_push(Stk* stk, U32 value) {
  _CHK_GROW(stk, ASIZE);
  store(stk->mem, stk->sp - ASIZE, value, ASIZE);
  stk->sp -= ASIZE;
}

#define WS_POP()  Stk_pop(&env.ws)
/*fn*/ U32 Stk_pop(Stk* stk) {
  ASM_ASSERT(stk->sp + ASIZE <= stk->size, E_stkUnd);
  U32 out = fetch(stk->mem, stk->sp, ASIZE);
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

APtr toAPtr(U32 v, U8 sz) {
  switch (sz) {
    case 1: SET_ERR(E_cSzAPtr);
    case 2: return ENV_MOD_HIGH() + v;
    case 4: ASM_ASSERT(v <= MAX_APTR, E_oob); return v;
    default: SET_ERR(E_cSz);
  }
}

APtr toAPtrSzI(U32 v, SzI szI) {
  switch (szI) {
    case SzI1: SET_ERR(E_cSzAPtr);
    case SzI2: return ENV_MOD_HIGH() + v;
    case SzI4: return v;
    default: SET_ERR(E_cSz);
  }
}

U8 mergeInstrSzI(SzI szI, U8 instr) {
  switch (instr >> 6) {
    case 0:
    case 1: return instr;
    case 2:
    case 3: return (szI << SZ_SHIFT) | instr;
  }
}

// ********************************************
// ** Executing Instructions2

void xImpl(APtr aptr) { // impl for "execute"
  // get amount to grow, must be multipled by APtr size .
  U16 growLs = fetch(mem, aptr, 2);
  Stk_grow(&env.ls, growLs << APO2);
  // Callstack has 4 byte value: growLs | mp | cptrHigh | cptrLow
  Stk_push(&env.callStk, (growLs << 24) + env.ep);
  env.mp = MOD_HIGH_MASK & aptr;
  env.ep = aptr + 2;
}

void xsImpl(APtr aptr) { // impl for "execute small"
  Stk_push(&env.callStk, env.ep);
  env.mp = MOD_HIGH_MASK & aptr;
  env.ep = aptr;
}


#define INSTR_SZI(INSTR) ((INSTR & 0x30) >> 4)

U32 popLit(SzI szI) {
  U32 out;
  switch(szI) {
    case SzI1:
      out = fetch(mem, env.ep, 1);
      env.ep += 1;
      break;
    case SzI2:
      out = fetch(mem, env.ep, 2);
      env.ep += 2;
      break;
    case SzI4:
      out = fetch(mem, env.ep, 4);
      env.ep += 4;
      break;
    default: SET_ERR(E_cSz);
  }
  return out;
}

static inline Bool executeMem(U8 instr) {
  SzI szI = INSTR_SZI(instr);
  U32 l;
  switch(instr & 0x0F) {
    case LIT: WS_PUSH(popLit(szI));  return FALSE;
    case FT: WS_PUSH(fetch(mem, WS_POP(), szIToSz(szI))); return FALSE;
    case FTLL:
      WS_PUSH(fetch(mem, LS_OFFSET() + env.ls.sp  + popLit(szI), szIToSz(szI)));
      return FALSE;
    case FTML:
      WS_PUSH(fetch(mem, (MOD_HIGH_MASK & env.mp) + popLit(szI), szIToSz(szI)));
      return FALSE;
    case SR: l = WS_POP(); store(mem, WS_POP(), l, szIToSz(szI)); return FALSE;
    case SRLL:
      store(mem, LS_OFFSET() + env.ls.sp + popLit(szI), WS_POP(), szIToSz(szI));
      return FALSE;
    case SRML:
      store(mem, (MOD_HIGH_MASK & env.mp) + popLit(szI), WS_POP(), szIToSz(szI));
      return FALSE;
    default: SET_ERR(E_cInstr);
  }
}

static inline Bool executeOp(U8 instr) {
  U32 l; U32 r;

  U32 szMask = 0xFFFFFFFF; // TODO: remove

  switch (instr >> 4) {
    // Special    [0x0 - 0x10)
    case 0:
      switch (instr) {
        case 0x0: /*NOP */ return FALSE;
        case 0x1: /*SWP */
          r = WS_POP();
          l = WS_POP();
          WS_PUSH(r);
          WS_PUSH(l);
          return FALSE;
        case 0x2: /*DRP */ WS_POP(); return FALSE;
        case 0x3: /*DRP2*/ WS_POP(); WS_POP(); return FALSE;
        case 0x4: /*DUP */ l = WS_POP(); WS_PUSH(l); WS_PUSH(l);      return FALSE;
        case 0x5: /*DUPN*/ l = WS_POP(); WS_PUSH(l); WS_PUSH(0 == l); return FALSE;
        case 0x6: /*DVF */ deviceOp(TRUE, SzI4, szMask, 4); return FALSE;
        case 0x7: /*DVS */ deviceOp(FALSE, SzI4, szMask, 4); return FALSE;
        case 0x8: /*RGL */ assert(FALSE); // not impl
        case 0x9: /*RGS */ assert(FALSE); // not impl
        default: SET_ERR(E_cInstr);
      }

    // Single Arg [0x10 - 0x20)
    case 1:
      r = WS_POP();
      switch (instr - OPI1_START) {
        case 0: /*INC */ r = r + 1; break;
        case 1: /*INC2*/ r = r + 2; break;
        case 2: /*INC4*/ r = r + 4; break;
        case 3: /*DEC */ r = r - 1; break;
        case 4: /*INV */ r = ~r; break;
        case 5: /*NEG */ r = -r; break;
        case 6: /*NOT */ r = 0==r; break;
        case 7: /*CI1 */ r = (I32) ((I8) r); break;
        case 8: /*CI2 */ r = (I32) ((I16) r); break;
        default: SET_ERR(E_cInstr);
      }
      WS_PUSH(r);
      return FALSE;

    // Two Arg    [0x20 - 0x3F)
    case 2:
      r = WS_POP();
      l = WS_POP();
      switch (instr - OPI2_START) {
        case 0x0: /*ADD */ r = l + r; break;
        case 0x1: /*SUB */ r = l - r; break;
        case 0x2: /*MOD */ r = l % r; break;
        case 0x3: /*SHL */ r = l << r; break;
        case 0x4: /*SHR */ r = l >> r; break;
        case 0x5: /*AND */ r = l & r; break;
        case 0x6: /*OR */ r = l | r; break;
        case 0x7: /*XOR*/ r = l ^ r; break;
        case 0x8: /*LAND*/ r = l && r; break;
        case 0x9: /*LOR */ r = l || r; break;
        case 0xA: /*EQ  */ r = l == r; break;
        case 0xB: /*NEQ */ r = l != r; break;
        case 0xC: /*GE_U*/ r = l >= r; break;
        case 0xD: /*LT_U*/ r = l < r; break;
        case 0xE: /*GE_S*/ r = (I32) l >= (I32) r; break;
        case 0xF: /*LT_S*/ r = (I32) l < (I32) r; break;
        case 0x10: /*MUL  */ r = l * r; break;
        case 0x11: /*DIV_U*/ r = l / r; break;
        case 0x12: /*DIV_S*/
          ASM_ASSERT(r, E_divZero);
          r = (I32) l / (I32) r;
          break;
        default: SET_ERR(E_cInstr);
      }
      WS_PUSH(r);
  }
  return FALSE;
}

static inline Bool executeJmp(U8 instr) {
  SzI szI;
  U32 l, r;
  switch(instr & 0x0F) {
    case RET:
      if(Stk_len(env.callStk) == 0) {
        return TRUE;
      } else {
        U32 callMeta = Stk_pop(&env.callStk);
        Stk_shrink(&env.ls, (callMeta >> 24) << APO2);
        env.mp = MOD_HIGH_MASK & callMeta;
        env.ep = MAX_APTR & callMeta;
      }
      break;
    case JMPL:
      szI = INSTR_SZI(instr);
      env.ep = toAPtrSzI(popLit(szI), szI); break;
    case JMPW: env.ep = toAPtrSzI(WS_POP(), INSTR_SZI(instr));
    case JZL:
      szI = INSTR_SZI(instr);
      l = popLit(szI);
      r = WS_POP();
      if(!r) { env.ep = toAPtrSzI(l, szI); }
      break;
    case JTBL: assert(FALSE); // TODO: not impl
    case XL:
      szI = INSTR_SZI(instr);
      xImpl(toAPtrSzI(popLit(szI), szI));
      break;
    case XW: xImpl(toAPtrSzI(WS_POP(), INSTR_SZI(instr))); break;
    case XSL:
      szI = INSTR_SZI(instr);
      xsImpl(toAPtrSzI(popLit(szI), szI)); break;
      break;
    case XSW: xsImpl(toAPtrSzI(WS_POP(), INSTR_SZI(instr))); break;
    default: SET_ERR(E_cInstr);
  }
  return FALSE;
}


/* returns: should escape */
inline static Bool executeInstr(U8 instr) {
  globalInstr = instr; // for debugging
  if(dbgMode) { printf("  * "); dbgInstr(instr); printf("\n"); }
  switch (instr >> 6) {
    case 0: return executeOp(instr);
    case 1: // Tiny literal
      WS_PUSH(0x3F & instr);
      return FALSE;
    case 2: return executeJmp(instr);
    case 3: return executeMem(instr);
  }
}


/*fn*/ void execute(U8 instr) {
  env.ep = 0;
  while(TRUE) {
    if(executeInstr(instr)) return;
    instr = popLit(SzI1);
  }
}

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) where key is a cstr and value is U32.

#define Dict_key(D, OFFSET)  ((Key*) (mem + D.buf + (OFFSET)))

/*fn*/ U8 cstrEq(U8 slen0, U8 slen1, U8* s0, U8* s1) {
  if(slen0 != slen1) return FALSE;
  for(U8 i = 0; i < slen0; i += 1) {
    if(s0[i] != s1[i]) return FALSE;
  }
  return TRUE;
}

// find key offset from dict. Else return dict.heap
/*fn*/ U16 Dict_find(DictRef d, U8 slen, U8* s) {
  U16 offset = 0;
  assert(*d.heap < d.end);

  while(offset < *d.heap) {
    Key* key = Dict_key(d, offset);
    if(cstrEq(key->len, slen, key->s, s)) return offset;
    U16 entrySz = alignAPtr(keySizeWLen(key->len), 4);
    offset += entrySz;
  }

  assert(offset == *d.heap);
  return offset;
}

/*fn*/ U16 Dict_set(DictRef d, U8 slen, U8* s, U32 value) {
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

/*fn*/ U32 Dict_get(DictRef d, U8 slen, U8 *s) {
  U16 offset = Dict_find(d, slen, s);
  ASM_ASSERT(offset != *d.heap, E_cNoKey);
  return Dict_key(d, offset)->value;
}

/*fn*/ void Dict_forget(U8 slen, U8* s) {
  DictRef d = {.buf = dict->buf, .end = dict->end, .heap = &dict->heap};
  dict->heap = Dict_find(d, slen, s);
}


// ********************************************
// ** Scanner
#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf (mem + tokenState->buf)

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

/*fn*/ void cSz() { // '.': change sz
  if(tokenLen >= tokenBufSize) readAppend();
  U8 sz = charToHex(tokenBuf[tokenLen]);
  tokenLen += 1;

  SzI szI;
  switch (sz) {
    case 1: instrSzI = SzI1; break;
    case 2: instrSzI = SzI2; break;
    case 4: instrSzI = SzI4; break;
    case 0xA: instrSzI = SzIA; break;
    default: SET_ERR(E_cSz);
  }
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
  if (dbgMode) { printf("= "); printToken(); dbgWs(); printf("\n"); }
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
  U8 sz = szIToSz(instrSzI);
  U32 value = WS_POP();
  if (dbgMode) { printf(", #%X sz=%u\n", value, sz); }
  store(mem, *env.heap, value, sz);
  *env.heap += sz;
}

/*fn*/ void cWriteInstr() { // `%`
  U8 instr = scanInstr();
  store(mem, *env.heap, instr, 1);
  if (dbgMode) { printf("%% "); dbgInstr(instr); printf("\n"); }
  *env.heap += 1;
}

/*fn*/ void cExecuteInstr() { // ^
  U8 instr = scanInstr();
  if (dbgMode) { printf("^ "); dbgInstr(instr); dbgWs(); printf("\n"); }
  env.ep = 0;
  executeInstr(instr);
}

/*fn*/ void cExecute() { // $
  scan();
  if(dbgMode) { printf("$ "); printToken(); dbgWs(); printf("\n"); }
  DictRef d = DEFAULT_DICT;
  U32 value = Dict_get(d, tokenLen, tokenBuf);
  WS_PUSH(value);
  execute(JMP_MASK | SZI4_MASK | JMPW);
}

/*fn*/ ExecuteResult compile() {
  DictRef d = DEFAULT_DICT;
  ExecuteResult result = {};
  if(tokenLen == 0) {
    result.escape = TRUE;
    return result;
  }
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
  return result;
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
      scan(); if(*env.err) break;
      ExecuteResult r = compile(); if(*env.err) break;
      if(r.escape) return;
    }
  }

  err_jmp = prev_err_jmp;
}

void deviceOp_dict(Bool isFetch) {
  U32 rHeap = WS_POP();
  U32 buf = WS_POP();
  DictRef d = {
    .heap = (U16*) (mem + rHeap),
    .buf = buf,
    .end = dict->end,
  };
  if(isFetch) {
    WS_PUSH(Dict_get(d, tokenLen, tokenBuf));
  } else {
    Dict_set(d, tokenLen, tokenBuf, WS_POP());
  }
}

void deviceOpRDict(Bool isFetch) {
  DictRef d = DEFAULT_DICT;
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
  U32 cs_sp = env.callStk.sp;
  U32 ls_sp = env.ls.sp;

  jmp_buf* prev_err_jmp = err_jmp;
  jmp_buf local_err_jmp;
  err_jmp = &local_err_jmp;

  if(setjmp(local_err_jmp)) {
    // got error, handled below
  } else {
    execute(JMP_MASK | SZI4_MASK | JMPW);
  }

  // ALWAYS Reset ep, call, and local stack and clear WS.
  env.ep = ep;
  env.callStk.sp = cs_sp;
  env.ls.sp = ls_sp;
  env.ws.sp = env.ws.size; // clear WS

  // Push current error to WS and clear it.
  U32 out = *env.err;
  *env.err = 0;
  WS_PUSH(out);
}

void deviceOpCompile() {
  ExecuteResult result = compile();
  ASM_ASSERT(!result.escape, E_cRet);
}

// Device Operations
// Note: this must be declared last since it ties into the compiler infra.
void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz) {
  DictRef d = DEFAULT_DICT;
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
    case 0x8: /*D_cslen*/ WS_PUSH(Stk_len(env.callStk)); break;
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
    .callStk = \
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
  /*      MS      WS     RS     LS     DICT */    \
  NEW_ENV_BARE(0x8000, 0x100, 0x100, 0x200, 0x1000)

void compileFile(U8* s) {
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
  compileFile("spor/asm.sp");

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

void printCStr(U8 len, U8* s) { printf("%.*s", len, s); }
void printToken() {
  printf("\"");
  printCStr(tokenLen, tokenBuf);
  printf("\" line=%u ", line);
}

void dbgIndent() {
  for(U16 i = 0; i < X_DEPTH; i += 1) printf("  ");
}

void dbgEnv() {
  printf("  token[%u, %u]=", tokenLen, tokenBufSize);
  printToken();
  printf("stklen:%u ", WS_LEN);
  printf("tokenGroup=%u  ", tokenState->group);
  printf("instr=#%X ", globalInstr);
  printf("sz=%u\n", szIToSz(instrSzI));
}


#define ENUM_STR(V) case V: return "V"

U8* memName(MemI m) {
  switch (m) {
    case LIT:   return "LIT  ";
    case FT:    return "FT   ";
    case FTLL:  return "FTLL ";
    case FTML:  return "FTML ";
    case SR:    return "SR   ";
    case SRLL:  return "SRLL ";
    case SRML:  return "SRML ";
    default:    return "?MEM?";
  }
}

U8* jmpName(JmpI j) {
  switch (j) {
    case RET:   return "RET  ";
    case JMPL:  return "JMPL ";
    case JMPW:  return "JMPW ";
    case JZL:   return "JZL  ";
    case JTBL:  return "JTBL ";
    case XL:    return "XL   ";
    case XW:    return "XW   ";
    case XSL:   return "XSL  ";
    case XSW:   return "XSW  ";
    default:    return "?JMP?";
  }
}

U8* opName(U8 op) {
  switch (op >> 4) {
    // Special    [0x0 - 0x10)
    case 0:
      switch (op) {
        case 0x0:  return "NOP  ";
        case 0x1:  return "SWP  ";
        case 0x2:  return "DRP  ";
        case 0x3:  return "DRP2 ";
        case 0x4:  return "DUP  ";
        case 0x5:  return "DUPN ";
        case 0x6:  return "DVF  ";
        case 0x7:  return "DVS  ";
        case 0x8:  return "RGL  ";
        case 0x9:  return "RGS  ";
        case 0xA:  return "FT   ";
        case 0xB:  return "SR   ";
        case 0xC:  return "LIT4 ";
        case 0xD:  return "ZERO ";
        default:   return "?OP0?";
      }

    // Single Arg [0x10 - 0x20)
    case 1:
      switch (op - OPI1_START) {
        case 0:    return "INC  ";
        case 1:    return "INC2 ";
        case 2:    return "INC4 ";
        case 3:    return "DEC  ";
        case 4:    return "INV  ";
        case 5:    return "NEG  ";
        case 6:    return "NOT  ";
        case 7:    return "CI1  ";
        case 8:    return "CI2  ";
        default:   return "?OP1?";
      }

    // Two Arg    [0x20 - 0x3F)
    case 2:
      switch (op - OPI2_START) {
        case 0x0:  return "ADD  ";
        case 0x1:  return "SUB  ";
        case 0x2:  return "MOD  ";
        case 0x3:  return "SHL  ";
        case 0x4:  return "SHR  ";
        case 0x5:  return "AND  ";
        case 0x6:  return "OR   ";
        case 0x7:  return "XOR  ";
        case 0x8:  return "LAND ";
        case 0x9:  return "LOR  ";
        case 0xA:  return "EQ   ";
        case 0xB:  return "NEQ  ";
        case 0xC:  return "GE_U ";
        case 0xD:  return "LT_U ";
        case 0xE:  return "GE_S ";
        case 0xF:  return "LT_S ";
        case 0x10: return "MUL  ";
        case 0x11: return "DIV_U";
        case 0x12: return "DIV_S";
        default:   return "?OP2?";
      }
  }
  return "?OP??";
}

Key keyDNE = {.len = 3, .s = "???" };

Key* Dict_findFn(U32 value) {
  DictRef d = {.buf = dict->buf, .end = dict->end, .heap = &dict->heap};
  U16 offset = 0;

  while(offset < dict->heap) {
    Key* key = Dict_key(d, offset);
    if(value == (0xFFFFFFFFFFFF & key->value)) return key;
    U16 entrySz = alignAPtr(keySizeWLen(key->len), 4);
    offset += entrySz;
  }

  assert(offset == *d.heap);
  return &keyDNE;
}

void dbgJmpI(JmpI j) {
  U32 jloc = 0;

  switch (j) {
    case JMPL:
    case XL:
    case XSL: jloc = toAPtr(fetch(mem, env.ep, 2), 2); break;
    case JMPW:
    case XW:
    case XSW: jloc = fetch(env.ws.mem, env.ws.sp, ASIZE); break;
  }
  if(!jloc) return;
  Key* k = Dict_findFn(jloc);
  printf(" [[ ");
  printCStr(k->len, k->s);
  printf(" ]] ");
}

U32 max(I32 a, I32 b) {
  if (a > b) return a;
  return b;
}

void dbgWs() {
  printf("WS", WS_LEN);
  printf(" [%u...]", max(0, ((I32) WS_LEN) - 2));
  if(WS_LEN > 0) {
    if(WS_LEN == 1) printf("   ----  |");
    else printf(" %+8X|", fetch(env.ws.mem, env.ws.sp + ASIZE, ASIZE));
    printf(" %+8X|", fetch(env.ws.mem, env.ws.sp, ASIZE));
  } else printf("   ----  |   ----  |");
}

void dbgWsFull() {
  printf("  {{ ");
  for(I8 i = WS_LEN - 1; i >= 0; i -= 1) {
    printf("%X, ", fetch(env.ws.mem, env.ws.sp + (i << APO2), ASIZE));
  }
  printf("}}\n");
}


#define dbgInstrFmt(SZ, NAME) printf(".%X %s] ", SZ, NAME);
void dbgInstr(U8 i) {
  printf("[0x%-2X ", i);
  switch (i >> 6) {
    case 0: dbgInstrFmt(4, opName(i)); break;
    case 1: dbgInstrFmt(4, "T_LIT"); break;
    case 2: dbgInstrFmt(INSTR_SZI(i), jmpName(0xF & i)); break;
    case 3: dbgInstrFmt(INSTR_SZI(i), memName(0xF & i)); break;
  }
}


#define TEST_ENV_BARE \
  globalInstr = INSTR_DEFAULT; \
  SMALL_ENV_BARE; \
  U32 heapStart = *env.heap

#define TEST_ENV \
  SMALL_ENV \
  U32 heapStart = *env.heap


U8* testBuf = NULL;
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

void compileStr(U8* s) {
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
  assert(0x77770101 == fetch(mem, heapStart, 4));
  assert(0x0F00 == fetch(mem, heapStart+4, 2));
  assert(0 == fetch(mem, heapStart+6, 1));
}

/*test*/ void testExecuteInstr() {
  printf("## testExecuteInstr\n"); SMALL_ENV;
  compileStr(".4 @E_general");
  assert(0xE000 == WS_POP());

  compileStr(".2");
  assert(SzI2 == instrSzI);

  compileStr(".4 #5006 #7008 .2 ^DRP");
  assert(0x5006 == WS_POP());

  compileStr(".1 #01 #02  ^ADD");
  assert(0x03 == WS_POP());

  compileStr(".4 #8 #5 ^SUB");
  assert(0x03 == WS_POP());

  compileStr(".4 #8000 #4 ^SUB");
  assert(0x7FFC == WS_POP());

  compileStr(".A @D_sz ^DVF");
  assert(0x04 == WS_POP());

  U32 expectDictHeap = (U8*)dict - mem + 4;
  compileStr(".4 @c_tokenBuf ^FT"); assert(tokenState->buf == WS_POP());
  compileStr(".4 @topHeap ^FT");  assert(*env.topHeap == WS_POP());
  compileStr(".4 @c_dictHeap");   assert(expectDictHeap == WS_POP());
}

/*test*/ void testAsm2() {
  printf("## testAsm2\n"); SMALL_ENV;
  compileFile("spor/asm2.sp");
  assert(!WS_LEN);
  compileLoop(); ASSERT_NO_ERR();
  compileFile("spor/testAsm2.sp");
}

// /*test*/ void testBoot() {
//   printf("## testBoot\n"); SMALL_ENV;
//   compileFile("spor/asm2.sp");
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
  testExecuteInstr();
  testAsm2();
  // testBoot();

  assert(0 == WS_LEN);
}
