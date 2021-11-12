
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>

// Debugging
void dbgEnv();

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

#define dbg(MSG)  printf(MSG); dbgEnv()
#define OP_ASSERT(COND, MSG) \
  if(!(COND)) { printf("!A! "); printf(MSG); dbgEnv(); return 1; }
#define OP_CHECK(COND, MSG) \
  if(COND) { printf("!A! "); printf(MSG); dbgEnv(); return 1; }



typedef U8 ErrCode;

// Size
typedef enum { SzI1, SzI2, SzI4 } SzI;
#define SzIA SzI4

// Mem
typedef enum {
  WS,     IMWS,   FTLI,   FTMI,
  FTOI,   SRLI,   SRMI,   SROI,
} MemI;

// Jmp
typedef enum {
  NOJ,          JZ,           JTBL,         JMP,
  _JR0,         CALL,         CNL,          RET,
} JmpI;

// Operations
#define JMP_SHIFT      13
#define JMP_MASK       0xE000

#define SZI_SHIFT      6
#define SZI_MASK       0x00C0

#define INSTR(SZ, JMP, MEM, OP) \
    (JMP << 13) \
  + (MEM << 6 )         \
  + (SZ  << SZI_SHIFT)  \
  +  OP

#define INSTR_DEFAULT (SzI4 << SZI_SHIFT)
#define INSTR_CNL     INSTR(SzI4, CNL, WS, 0)
#define INSTR_W_SZ(INSTR, SZI)    (((~SZI_MASK) & INSTR) | (SZI  << SZI_SHIFT))
#define INSTR_W_JMP(INSTR, JMPI)  (((~JMP_MASK) & INSTR) | (JMPI << JMP_SHIFT))


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
  Stk ls;

  // Separate from mem
  Stk ws;
  Stk callStk;
} Env;

#define LS_OFFSET() ((size_t)mem - (size_t)env.ls.mem)
#define ENV_MOD_HIGH()  (env.ep & MOD_HIGH_MASK)

typedef struct {
  ErrCode err;
  U8 escape; // True if RET but no more stack.
} ExecuteResult;

typedef struct {
  U16 heap;  // heap offset
  U16 end;   // end offset
} Dict;

typedef struct {
  U8 len;
  U8 s[];
} Key;

#define MAX_TOKEN 32
#define TOKEN_BUF 0x7D

typedef struct {
  U8 group;
  U8 len;   // length of token
  U8 size;  // characters buffered
  U8 buf[]; // size=TOKEN_BUF
} TokenState;

typedef enum {
  T_NUM, T_HEX, T_ALPHA, T_SPECIAL, T_SYMBOL, T_WHITE
} TokenGroup;

// ********************************************
// ** Globals

void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz);

Env env;
U8* mem = NULL;
Dict* dict = NULL;
TokenState* tokenState = NULL;
FILE* srcFile;
U16 instr = INSTR_DEFAULT;
ssize_t (*readAppend)() = NULL; // Read bytes incrementing tokenBufSize

// ********************************************
// ** Utilities

/*fn*/ void fail(U8* cstr) {
  printf("!!FAIL!! ");
  printf(cstr);
  printf("\n");
  dbgEnv();
  exit(1);
}

// Bitmask for SZ in bytes. Note: <<3 is same as times 8
#define CUR_SZI         ((instr & SZI_MASK) >> SZI_SHIFT)
#define CUR_SZ_MASK     szIToMask(CUR_SZI)

U32 szIToMask(SzI szI) {
  switch (szI) {
    case 0: return 0xFF;
    case 1: return 0xFFFF;
    case 2: return 0xFFFFFFFF;
    default: fail("invalid szI");
  }
}

U8 szIToSz(SzI szI) {
  switch (szI) {
    case 0: return 1;
    case 1: return 2;
    case 2: return 4;
    default: fail("invalid szI");
  }
}

SzI szToSzI(U8 sz) {
  switch (sz) {
    case 1: return SzI1;
    case 2: return SzI2;
    case 4: return SzI4;
    default: fail("invalid sz");
  }
}

/*fn*/ void* alignSys(void* p, U8 sz) {
  U8 mod = (size_t)p % sz;
  if(mod == 0) return p;
  return p + (sz - mod);
}

/*fn*/ APtr align(APtr aPtr, U8 sz) {
  U8 mod = aPtr % sz;
  if(mod == 0) return aPtr;
  return aPtr + (sz - mod);
}

// Return value of ASCII hex char.
/*fn*/ ErrCode charToHex(U8 c) {
  c = c - '0';
  if(c <= 9) return c;
  c = c - ('A' - '0');
  if(c <= 5) return c + 10;
  c = c - ('a' - 'A');
  return c + 10;
}

/*fn*/ void store(U8* mem, APtr aptr, U32 value, U8 sz) {
  switch (sz) {
    case 1: 
      *(mem+aptr) = (U8)value;
      break;
    case 2: 
      assert(aptr % 2 == 0);
      *((U16*) (mem+aptr)) = (U16)value;
      break;
    case 4:
      assert(aptr % 4 == 0);
      *((U32*) (mem+aptr)) = value;
      break;
    default: fail("store: invalid Sz");
  }
}

/*fn*/ U32 fetch(U8* mem, APtr aptr, U8 sz) {
  if(aptr == 0) fail("null access");
  switch (sz) {
    case 1:
      return (U32) *((U8*) (mem+aptr));
    case 2:
      assert(aptr % 2 == 0);
      return (U32) *((U16*) (mem+aptr));
    case 4:
      assert(aptr % 4 == 0);
      return (U32) *((U32*) (mem+aptr));
    default: fail("fetch: invalid Sz");
  }
}

#define Stk_len(STK)  (STK.size - STK.sp)
#define WS_LEN        Stk_len(env.ws)
void _chk_grow(Stk* stk, U16 sz) {
  if(stk->sp < sz) { fail("stack overflow"); };
}

void _chk_shrink(Stk* stk, U16 sz) {
  if(stk->sp + sz > stk->size ) { fail("stack underflow"); };
}

#define WS_PUSH(VALUE)  Stk_push(&env.ws, VALUE)
/*fn*/ ErrCode Stk_push(Stk* stk, U32 value) {
  _chk_grow(stk, ASIZE);
  store(stk->mem, stk->sp - ASIZE, value, ASIZE);
  stk->sp -= ASIZE;
  return OK;
}

#define WS_POP()  Stk_pop(&env.ws)
/*fn*/ U32 Stk_pop(Stk* stk) {
  _chk_shrink(stk, ASIZE);
  U32 out = fetch(stk->mem, stk->sp, ASIZE);
  stk->sp += ASIZE;
  return out;
}

/*fn*/ void Stk_grow(Stk* stk, U16 sz) {
  _chk_grow(stk, sz);
  stk->sp -= sz;
}

/*fn*/ void Stk_shrink(Stk* stk, U16 sz) {
  _chk_shrink(stk, sz);
  stk->sp += sz;
}

APtr toAPtr(U32 v, U8 sz) {
  switch (sz) {
    case 1: fail("APtr.sz=U8");
    case 2: return ENV_MOD_HIGH() + v;
    case 4:
      assert(v <= MAX_APTR);
      return v;
    default: assert(FALSE);
  }
}


// ********************************************
// ** Executing Instructions

/*fn*/ U16 popImm() {
    U16 out = fetch(mem, env.ep, 2);
    env.ep += 2;
    return out;
}

/* Takes the 16bit instruction and executes it as follows:
 * - mem: perform load/popImm. set srPtr if it is a SR Mem instruction (not
 *   related to SR op).
 * - operation: perform the operation.
 * - If srPtr is set (from mem), pop a value and store it at srPtr.
 * - jmp: Perform jmp/call instruction. If an imm is required pull it.
 *   - RET has special handling if the callstak is empty where it returns to
 *     native execution handling.
 *
 * A few notes:
 * - It's legal to use two immediates (one for MEM and one for JMP).
 * - It's legal to do multiple memory operations in one instriction, i.e. read
 *   from and address using MEM and then use SR op.
 * - CALL can be done after an operation. Useful for LoaDing an address then
 *   calling it.
 */
/*fn*/ ExecuteResult executeInstr(U16 instr) {
  ExecuteResult res = {};

  U8 opI =    (U8 )  (0x3F & instr);
  SzI szI =   (SzI)  (0x7  & (instr >> (            6)));
  MemI memI = (MemI) (0x7  & (instr >> (        2 + 6)));
  JmpI jmpI = (JmpI) (0x7  & (instr >> (3 + 2 + 2 + 6)));
  U32 szMask = szIToMask(szI);
  U8 sz = szIToSz(szI);

  APtr srPtr = 0;
  U32 l; U32 r;

  switch(memI) {
    case WS: break;
    case IMWS: WS_PUSH(szMask & popImm());    break;
    case SRLI:
      srPtr = env.ls.sp + popImm();
      if(!srPtr) fail("SRLI at NULL");
      break;
    case SRMI:
      srPtr = env.mp + popImm();
      if(!srPtr) fail("SRMI at NULL");
      break;
    case SROI: WS_PUSH(szMask & popImm());    break;
    case FTLI: WS_PUSH(fetch(mem, env.ls.sp + popImm(), sz)); break;
    case FTMI: WS_PUSH(fetch(mem, env.mp    + popImm(), sz)); break;
    case FTOI:
      l = WS_POP();       // Address
      WS_PUSH(popImm());  // Second
      WS_PUSH(fetch(mem, l, sz));
      break;
    default: fail("unknown mem");
  }

  switch (opI >> 4) {
    // Special    [0x0 - 0x10)
    case 0:
      switch (opI) {
        case 0x0: /*NOP */ break;
        case 0x1: /*SWP */
          r = szMask & WS_POP();
          l = szMask & WS_POP();
          WS_PUSH(r);
          WS_PUSH(l);
          break;
        case 0x2: /*DRP */ WS_POP(); break;
        case 0x3: /*DRP2*/ WS_POP(); WS_POP(); break;
        case 0x4: /*DUP */ l = WS_POP(); WS_PUSH(l); WS_PUSH(l);      break;
        case 0x5: /*DUPN*/ l = WS_POP(); WS_PUSH(l); WS_PUSH(0 == l); break;
        case 0x6: /*DVL */ deviceOp(TRUE, szI, szMask, sz); break;
        case 0x7: /*DVS */ deviceOp(FALSE, szI, szMask, sz); break;
        case 0x8: /*RGL */ fail("RGL not impl"); break;
        case 0x9: /*RGS */ fail("RGS not impl"); break;
        case 0xA: /*FT  */ WS_PUSH(fetch(mem, WS_POP(), sz)); break;
        case 0xB: /*SR  */
          if(srPtr) fail("SR double store");
          l = WS_POP(); // value
          srPtr = WS_POP();
          WS_PUSH(l);
          break;
      }
      break;

    // Single Arg [0x10 - 0x20)
    case 1:
      r = WS_POP();
      switch (opI - OPI1_START) {
        case 0: /*INC */ r = r + 1; break;
        case 1: /*INC2*/ r = r + 2; break;
        case 2: /*INC4*/ r = r + 4; break;
        case 3: /*INV */ r = ~r; break;
        case 4: /*NEG */ r = -r; break;
        case 5: /*NOT */ r = 0==r; break;
        case 6: /*CI1 */ r = (I32) ((I8) r); break;
        case 7: /*CI2 */ r = (I32) ((I16) r); break;
      }
      WS_PUSH(szMask & r);
      break;

    // Two Arg    [0x20 - 0x3F)
    case 2:
      r = WS_POP();
      l = WS_POP();
      switch (opI - OPI2_START) {
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
        case 0xE: /*GE_S*/
          switch (szI) {
            case SzI1: r = (I8)  l >= (I8)  r; break;
            case SzI2: r = (I16) l >= (I16) r; break;
            case SzI4: r = (I32) l >= (I32) r; break;
          }
          break;
        case 0xF: /*LT_S*/
          switch (szI) {
            case SzI1: r = (I8)  l < (I8)  r; break;
            case SzI2: r = (I16) l < (I16) r; break;
            case SzI4: r = (I32) l < (I32) r; break;
          }
          break;
        case 0x10: /*MUL  */ r = l * r; break;
        case 0x11: /*DIV_U*/ r = l / r; break;
        case 0x12: /*DIV_S*/
          switch (szI) {
            case SzI1: r = (I8)  l / (I8)  r; break;
            case SzI2: r = (I16) l / (I16) r; break;
            case SzI4: r = (I32) l / (I32) r; break;
          }
          break;
      }
      WS_PUSH(szMask & r);
  }

  // *************
  // * Store Result (if selected in Mem)
  if (srPtr) store(mem, srPtr, WS_POP(), sz);

  APtr aptr;
  U16 growLs;

  // *************
  // * Jmp: perform the jump
  switch(jmpI) {
    case NOJ: break;
    case JZ: env.ep = toAPtr(popImm(), 2); break;
    case JTBL: fail("not implemented"); break;
    case JMP: env.ep = WS_POP(); break;
    case _JR0: fail("JR0");
    case CALL:
      aptr = toAPtr(WS_POP(), sz);
      growLs = fetch(mem, aptr, 2); // amount to grow, must be multipled by APtr size.
      Stk_grow(&env.ls, growLs << APO2);
      // Callstack has 4 byte value: growLs | mp | cptrHigh | cptrLow
      Stk_push(&env.callStk, (growLs << 24) + env.ep);
      env.mp = MOD_HIGH_MASK & aptr;
      env.ep = aptr + 2;
      break;
    case CNL: // call no locals
      aptr = toAPtr(WS_POP(), sz);
      Stk_push(&env.callStk, env.ep);
      env.mp = MOD_HIGH_MASK & aptr;
      env.ep = aptr;
      break;
    case RET:
      if(Stk_len(env.callStk) == 0) {
        res.escape = TRUE;
      } else {
        U32 callMeta = Stk_pop(&env.callStk);
        Stk_shrink(&env.ls, (callMeta >> 24) << APO2);
        env.mp = MOD_HIGH_MASK & callMeta;
        env.ep = MAX_APTR & callMeta;
      }
      break;
  }

  return res;
}

/*fn*/ ErrCode execute(U16 instr) {
  env.ep = 0;
  while(TRUE) {
    ExecuteResult res = executeInstr(instr);
    if(res.err) return res.err;
    if(res.escape) return OK;
    U16 instr = popImm();
  }
}

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) wherey is a cstr and value is U32.

#define Dict_key(OFFSET)  ((Key*) (((U8*)dict) + sizeof(Dict) + OFFSET))
// Given ptr to key, get pointer to the value.
#define Key_vptr(KEY) ((U32*) alignSys(((U8*)KEY) + KEY->len + 1, 4))

/*fn*/ U8 cstrEq(U8 slen0, U8 slen1, U8* s0, U8* s1) {
  if(slen0 != slen1) return FALSE;
  for(U8 i = 0; i < slen0; i += 1) {
    if(s0[i] != s1[i]) return FALSE;
  }
  return TRUE;
}

// find key offset from dict. Else return dict.heap
/*fn*/ U16 Dict_find(U8 slen, U8* s) {
  Key* key = Dict_key(0);
  U16 offset = 0;

  while(offset < dict->heap) {
    if(cstrEq(key->len, slen, key->s, s)) {
      return offset;
    }
    U16 entrySz = align(key->len + 1 + 4, 4);
    key += entrySz;
    offset += entrySz;
  }

  assert(offset == dict->heap);
  return offset;
}

/*fn*/ U16 Dict_set(U8 slen, U8* s, U32 value) {
  // Set a key to a value, returning the offset
  U16 offset = Dict_find(slen, s);
  Key* key = Dict_key(offset);
  if(offset == dict->heap) {
    // new key
    U16 addedSize = align(1 + slen + 4, 4);
    assert(dict->heap + addedSize <= dict->end);
    key->len = slen;
    memcpy(key->s, s, slen);   // memcpy(dst, src, sz)
    dict->heap += align(1 + slen + 4, 4);
  }
  U32* v = Key_vptr(key);
  *v = value;
  return offset;
}

/*fn*/ ErrCode Dict_get(U32* out, U8 slen, U8 *s) {
  U16 offset = Dict_find(slen, s);
  OP_ASSERT(offset != dict->heap, "key not found");
  Key* key = Dict_key(offset);
  *out = *Key_vptr(key);
  return OK;
}

/*fn*/ void Dict_forget(U8 slen, U8* s) {
  dict->heap = Dict_find(slen, s);
}


// ********************************************
// ** Scanner
#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf tokenState->buf

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

/*fn*/ ErrCode shiftBuf() {
  // Shift buffer left from end of token
  if(tokenLen == 0) return OK;
  U8 newStart = tokenLen;
  U8 i = 0;
  while(tokenLen < tokenBufSize) {
    tokenBuf[i] = tokenBuf[tokenLen];
    i += 1;
    tokenLen += 1;
  }
  tokenBufSize = tokenBufSize - newStart;
  tokenLen = 0;
  return OK;
}

// Scan next token;
/*fn*/ ErrCode scan() {

  // Skip whitespace
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew();
    if(tokenBufSize == 0) return OK;
    if (toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) {
      shiftBuf();
      break;
    }
    tokenLen += 1;
  }
  if(tokenBufSize < MAX_TOKEN) readAppend();

  U8 c = tokenBuf[tokenLen];
  tokenState->group = (U8) toTokenGroup(c);
  if(tokenState->group <= T_ALPHA) tokenState->group = T_ALPHA;

  // Parse token until the group changes.
  while(tokenLen < tokenBufSize) {
    OP_ASSERT(tokenLen < MAX_TOKEN, "token too large");
    c = tokenBuf[tokenLen];
    TokenGroup tg = toTokenGroup(c);
    if (tg == tokenState->group) {}
    else if (tokenState->group == T_ALPHA && (tg <= T_ALPHA)) {}
    else break;
    tokenLen += 1;
  }

  return OK;
}

/*fn*/ ErrCode cSz() { // '.': change sz
  if(tokenLen >= tokenBufSize) readAppend();
  U8 sz = charToHex(tokenBuf[tokenLen]);
  tokenLen += 1;

  SzI szI;
  switch (sz) {
    case 1: szI = SzI1; break;
    case 2: szI = SzI2; break;
    case 4: szI = SzI4; break;
    case 0xA: szI = SzIA; break;
    default: fail("cSz invalid");
  }
  instr = INSTR_W_SZ(instr, szI);
  return OK;
}

/*fn*/ ErrCode cComment() {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew();
    if (tokenBufSize == 0) return OK;
    if (tokenBuf[tokenLen] == '\n') return OK;
    tokenLen += 1;
  }
  return OK;
}

/*fn*/ ErrCode linestr() {
  while(TRUE) {
    if (tokenLen >= tokenBufSize) readNew();
    if (tokenBufSize == 0) return OK;
    char c = tokenBuf[tokenLen];

    if (c == '\n') {
      tokenLen += 1;
      return shiftBuf();
    }

    if(c == '\\') {
      tokenLen += 1;
      OP_ASSERT(tokenLen < tokenBufSize, "Hanging \\");
      c = tokenBuf[tokenLen];
      if(c == '\\') {}
      else if(c == 'n') c = '\n';
      else if(c == 't') c = '\t';
      else if(c == '0') c = '\0';
      else OP_ASSERT(FALSE, "invalid escaped char");
    }
    store(mem, *env.heap, c, 1);
    *env.heap += 1;
    tokenLen += 1;
  }
  return OK;
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
/*fn*/ ErrCode cHex() {
  U32 v = 0;
  for(U8 i = 0; i < tokenLen; i += 1) {
    U8 c = tokenBuf[i];
    if (c == '_') continue;
    OP_ASSERT(toTokenGroup(c) <= T_HEX, "non-hex number");
    v = (v << 4) + charToHex(c);
  }
  WS_PUSH(CUR_SZ_MASK & v);
  shiftBuf();
  return OK;
}

/*fn*/ ErrCode cPutLoc() { // `&`
  OP_CHECK(WS_PUSH(CUR_SZ_MASK & *env.heap), "readSzPush.push");
  return OK;
}

/*fn*/ ErrCode cNameSet() { // `=`
  U32 value = WS_POP();

  OP_CHECK(scan(), "cNameSet.scan"); // load name token
  Dict_set(tokenLen, tokenBuf, CUR_SZ_MASK & value);
  return OK;
}

/*fn*/ ErrCode cNameGet() { // `@`
  OP_CHECK(scan(), "@ scan"); // load name token
  U32 value; OP_CHECK(Dict_get(&value, tokenLen, tokenBuf), "@ no name");
  OP_CHECK(WS_PUSH(CUR_SZ_MASK & value), "& push");
  return OK;
}

/*fn*/ ErrCode cNameForget() { // `~`
  OP_CHECK(scan(), "~ scan");
  Dict_forget(tokenLen, tokenBuf);
  return OK;
}

/*fn*/ ErrCode cWriteHeap() { // `,`
  U8 sz = szIToSz(CUR_SZI);
  U32 value = WS_POP();
  store(mem, *env.heap, value, sz);
  *env.heap += sz;
  return OK;
}

/*fn*/ ErrCode cWriteInstr() { // `;`
  store(mem, *env.heap, instr, 2);
  instr = INSTR_DEFAULT;
  *env.heap += 2;
  return OK;
}

/*fn*/ ErrCode updateInstr() { // any alphanumeric
  U32 value; OP_CHECK(Dict_get(&value, tokenLen, tokenBuf), "updateInstr: no name");
  U16 mask = ~(value >> 16);
  U16 setInstr = value & 0xFFFF;

  instr = instr & mask;
  instr = instr | setInstr;
  return OK;
}

/*fn*/ ErrCode cExecuteInstr() { // ^
  U16 i = INSTR_W_JMP(instr, RET);
  instr = INSTR_DEFAULT;
  return execute(i);
}

/*fn*/ ErrCode cExecute() { // $
  instr = INSTR_DEFAULT;
  OP_CHECK(scan(), "$ scan");
  U32 value; OP_CHECK(Dict_get(&value, tokenLen, tokenBuf), "$: no name");
  WS_PUSH(value);
  return execute(INSTR_CNL);
}

/*fn*/ ExecuteResult compileOne() {
  ExecuteResult result = {};
  if(tokenLen == 0) {
    result.escape = TRUE;
    return result;
  }
  if(tokenState->group <= T_ALPHA) {
    result.err = updateInstr();
    return result;
  }
  tokenLen = 1; // allows for multi symbols where valid, i.e. =$, $$
  switch (tokenBuf[0]) {
    case '.': result.err = cSz(); break;
    case '/': result.err = cComment(); break;
    case '#': scan(); result.err = cHex(); break;
    case '&': result.err = cPutLoc(); break;
    case '=': result.err = cNameSet(); break;
    case '@': result.err = cNameGet(); break;
    case '~': result.err = cNameForget(); break;
    case ',': result.err = cWriteHeap(); break;
    case ';': result.err = cWriteInstr(); break;
    case '^': result.err = cExecuteInstr(); break;
    case '$': result.err = cExecute(); break;
    // remove?
    case '"': result.err = linestr(); break;
    default:
      printf("invalid token: %c", tokenBuf[0]);
      result.err = 1;
  }
  return result;
}

/*fn*/ ErrCode compile() {
  while(TRUE) {
    scan();
    ExecuteResult r = compileOne();
    OP_CHECK(r.err, "compile");
    if(r.escape) break;
  }
  return OK;
}

// Device Operations
// Note: this must be declared last since it ties into the compiler infra.
void deviceOp(Bool isFetch, SzI szI, U32 szMask, U8 sz) {
  U32 op = WS_POP();
  U32 tmp;
  ExecuteResult result;
  switch(op) {
    case 0: /*D_read*/ readAppend(); break;
    case 1: /*D_scan*/ assert(!scan()); break;
    case 2: /*D_dict*/
      if(isFetch) {
        assert(Dict_get(&tmp, tokenLen, tokenBuf));
        WS_PUSH(szMask & tmp);
      } else {
        Dict_set(tokenLen, tokenBuf, szMask & WS_POP());
      }
      break;
      // FT=get SR=set dict key=tokenBuf, value of sz
    case 3: /*D_instr*/
      if(isFetch) WS_PUSH(instr);
      else instr = WS_POP();
      break;
    case 4: /*D_sz*/
      if(isFetch) WS_PUSH(sz);
      else instr = INSTR_W_SZ(instr, szToSzI(WS_POP()));
      break;
    case 5: /*D_comp*/
      result = compileOne();
      assert(!result.escape);
      assert(!result.err);
      break;
    case 6: assert(/*DV assert*/ WS_POP()); break
    default: fail("device op not impl");
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
  mem = localMem; \
  env = (Env) {                           \
    .heap = (APtr*) (mem + 4),            \
    .topHeap = (APtr*) (mem + 8),         \
    .topMem = (APtr*) (mem + 12),         \
    .ls = { .size = LS, .sp = LS },       \
    .ws = { .size = WS, .sp = WS, .mem = wsMem },     \
    .callStk = \
    { .size = RS, .sp = RS, .mem = callStkMem },     \
  };                                      \
  /* configure heap+topheap */            \
  *env.heap = 16;                         \
  *env.topHeap = MS;                      \
  *env.topMem = MS;                       \
  /* put Local Stack, Dict on topheap */  \
  *env.topHeap -= LS;                     \
  env.ls.mem = mem + *env.topHeap;        \
  *env.topHeap -= sizeof(TokenState) + TOKEN_BUF; \
  tokenState = (TokenState*) (mem + *env.topHeap); \
  *env.topHeap -= DS;                      \
  dict = (Dict*) (mem + *env.topHeap);    \
  dict->heap = 0;                         \
  dict->end = DS;

#define SMALL_ENV_BARE \
  /*      MS      WS     RS     LS     DICT */    \
  NEW_ENV_BARE(0x8000, 0x100, 0x100, 0x200, 0x1000)


#define NEW_ENV(MS, WS, RS, LS, DS) \
  NEW_ENV_BARE(MS, WS, RS, LS, DS); \
  srcFile = fopen("spore/asm.sa", "rb"); \
  readAppend = &readSrc; \
  assert(!compile());

#define SMALL_ENV \
  /*      MS      WS     RS     LS     DICT */    \
  NEW_ENV(0x8000, 0x100, 0x100, 0x200, 0x1000)


// ********************************************
// ** Main
void tests();

/*fn*/ int main() {
  printf("compiling spore...:\n");

  tests();
  return OK;
}

// ********************************************
// ** Tests
#include <string.h>

void dbgEnv() {
  printf("~~~ token[%u, %u]=%.*s  ", tokenLen, tokenBufSize, tokenLen, tokenBuf);
  printf("stklen:%u ", WS_LEN);
  printf("tokenGroup=%u  ", tokenState->group);
  printf("instr=0x%x\n", instr, instr);
}

#define TEST_ENV_BARE \
  instr = INSTR_DEFAULT; \
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

#define COMPILE(S) \
  testBuf = S; \
  testBufIdx = 0; \
  readAppend = &testingRead; \
  assert(!compile());


/*test*/ void testHex() {
  printf("## testHex #01...\n"); TEST_ENV_BARE;

  COMPILE(".1 #10");
  assert(WS_POP() == 0x10);

  printf("## testHex #10AF...\n");
  COMPILE("/comment\n.2 #10AF");
  U32 result = WS_POP();
  assert(result == 0x10AF);

  COMPILE(".4 #1002_3004");
  result = WS_POP();
  assert(0x10023004 == result);

  COMPILE(".2 #1002_3004");
  result = WS_POP();
  assert(0x3004 == result);
}

/*test*/ void testLoc() {
  printf("## testLoc...\n"); TEST_ENV_BARE;
  COMPILE(".4& .2&");
  U32 result1 = WS_POP(); U32 result2 = WS_POP();
  assert(result1 == (U16)heapStart);
  assert(result2 == heapStart);
}

/*test*/ void testQuotes() {
  printf("## testQuotes...\n"); TEST_ENV_BARE;
  COMPILE("\"foo bar\" baz\\0\n");

  assert(0 == strcmp(mem + heapStart, "foo bar\" baz"));
}

/*test*/ void testDictDeps() {
  printf("## testDictDeps... cstr\n"); TEST_ENV_BARE;
  assert(cstrEq(1, 1, "a", "a"));
  assert(!cstrEq(1, 1, "a", "b"));

  assert(cstrEq(2, 2, "z0", "z0"));
  assert(!cstrEq(2, 1, "aa", "a"));
  assert(!cstrEq(2, 2, "aa", "ab"));

  printf("## testDictDeps... dict\n");
  assert(0 == Dict_find(3, "foo"));

  // set
  assert(0 == Dict_set(3, "foo", 0xF00));

  // get
  U32 result;
  assert(!Dict_get(&result, 3, "foo"));
  assert(result == 0xF00);

  assert(8 == Dict_set(5, "bazaa", 0xBA2AA));
  assert(!Dict_get(&result, 5, "bazaa"));
  assert(result == 0xBA2AA);

  // reset
  assert(0 == Dict_set(3, "foo", 0xF00F));
  assert(!Dict_get(&result, 3, "foo"));
  assert(result == 0xF00F);
}

/*test*/ void testDict() {
  printf("## testDict\n"); TEST_ENV_BARE;

  COMPILE(".2 #0F00 =foo  .4 #000B_A2AA =bazaa"
      " @bazaa @foo .2 @foo");
  assert(0xF00 == WS_POP());   // 2foo
  assert(0xF00 == WS_POP());   // 4foo
  assert(0xBA2AA == WS_POP()); // 4bazaa
}

/*test*/ void testWriteHeap() { // test , and ;
  printf("## testWriteHeap\n"); TEST_ENV_BARE;
  COMPILE(".4 #77770101, .2 #0F00, ;");
  assert(0x77770101 == fetch(mem, heapStart, 4));
  assert(0x0F00 == fetch(mem, heapStart+4, 2));
  assert(INSTR_W_SZ(0, SzI2) == fetch(mem, heapStart+6, 2));
}

/*test*/ void testExecuteInstr() { // test ^
  printf("## testExecuteInstr\n"); SMALL_ENV;
  COMPILE(".4 @Sz2");
  assert(0x00C00040 == WS_POP());

  instr = ~0x1800; // instr with unused=0 else=1
  COMPILE(".4 NOJ WS NOP");
  assert(INSTR_DEFAULT == instr);

  COMPILE(".4 #5006 #7008 .2 DRP^");
  assert(0x5006 == WS_POP());

  COMPILE(".1 #01 #02  ADD^");
  assert(0x03 == WS_POP());

  COMPILE(".A @D_sz DVL^");
  assert(0x04 == WS_POP());
}


/*test*/ void tests() {
  testHex();
  testLoc();
  testQuotes();
  testDictDeps();
  testDict();
  testWriteHeap();
  testExecuteInstr();
}

