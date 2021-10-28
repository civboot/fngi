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
#define FALSE 0
#define TRUE 1
#endif

#ifndef elif
#define elif else if
#endif


typedef U8 ErrCode;

// Size
typedef enum {
  Sz1,            Sz2,
  _Sz_r,          Sz4,
} SzI;
#define SzA Sz4

// Mem
typedef enum {
  SRLI,   SRMI,   SROI,   FTLI,
  FTCI,   FTOI,   IMWS,   WS,
} MemI;

// Jmp
typedef enum {
  NOJ,          JZ,           JTBL,         JST,
  _JR0,         CALL,         CNL,          RET,
} JmpI;

// Operation
typedef enum {
  FT,           SR,           DVF,          DVS,
  NOP,          DRP,          INV,          NEG,
  EQZ,          EQZ_NC,       DRP2,         OVR,
  ADD,          SUB,          MOD,          MUL,
} OpI;

// Generic stack.
typedef struct {
  U16 sp;
  U16 size;
  U8* mem;
} Stk;

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
  U32 v[3]; // value "stack". 0=top, 1=scnd, 2=extra
  U8 sz;
  U8 len;
  Bool usesImm;
} OpData;

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

typedef ssize_t (*read_t)();

// ********************************************
// ** Globals
Env env;
U8* mem = NULL;
Dict* dict = NULL;
TokenState* tokenState = NULL;
FILE* srcFile;

#define INSTR(SZ, JMP, MEM, OP) \
    (SZ  << 14)  \
  + (JMP << 11) \
  + (MEM << 6 ) \
  +  OP

#define INSTR_DEFAULT INSTR(Sz4, NOJ, WS, NOP)
#define INSTR_CNL     INSTR(Sz4, CNL, WS, NOP)

U16 instr = INSTR_DEFAULT;

// ********************************************
// ** Helpers

/*fn*/ void fail(U8* cstr) {
  printf("!!FAIL!! ");
  printf(cstr);
  printf("\n");
  exit(1);
}

/*fn*/ U8 szToBytes(SzI sz) {
  return (U8)sz + 1;
}

/*fn*/ void* alignSys(void* p, U8 szBytes) {
  U8 mod = (size_t)p % szBytes;
  if(mod == 0) return p;
  return p + (szBytes - mod);
}

/*fn*/ APtr align(APtr aPtr, U8 szBytes) {
  U8 mod = aPtr % szBytes;
  if(mod == 0) return aPtr;
  return aPtr + (szBytes - mod);
}

/*fn*/ void store(U8* mem, APtr aptr, U32 value, U8 sz) {
  switch (sz) {
    case 1: 
      *(mem+aptr) = (U8)value;
      break;
    case 2: 
      assert(aptr % 2 == 0);
      *(((U16*) mem)+aptr) = (U16)value;
      break;
    case 4:
      assert(aptr % 4 == 0);
      *(((U32*) mem)+aptr) = value;
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
      return *(((U16*)mem)+aptr);
    case 4:
      assert(aptr % 4 == 0);
      return *(((U32*)mem)+aptr);
    default: fail("fetch: invalid Sz");
  }
}

void _chk_grow(Stk* stk, U16 sz) {
  if(stk->sp < sz) { fail("stack overflow"); };
}

void _chk_shrink(Stk* stk, U16 sz) {
  if(stk->sp + sz > stk->size ) { fail("stack underflow"); };
}

#define WS_PUSH(VALUE, SZ)  Stk_push(&env.ws, VALUE, SZ)
/*fn*/ ErrCode Stk_push(Stk* stk, U32 value, U8 sz) {
  _chk_grow(stk, sz);
  store(stk->mem, stk->sp - sz, value, sz);
  stk->sp -= sz;
  return OK;
}

#define WS_POP(SZ)  Stk_pop(&env.ws, SZ)
/*fn*/ U32 Stk_pop(Stk* stk, U8 sz) {
  _chk_shrink(stk, sz);
  U32 out = fetch(stk->mem, stk->sp, sz);
  stk->sp += sz;
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

#define Stk_len(STK) (STK.size - STK.sp)

// Shift opdata to the right.
/*fn*/ void shift_op(OpData* out) {
  out->v[2] = out->v[1];
  out->v[1] = out->v[0];
  out->len += 1;
}

// ********************************************
// ** Operations

#define OP_ARGS OpData *out
#define OP_ASSERT(COND, MSG) \
  if(!(COND)) { printf("!A! "); printf(MSG); dbgEnv(); return 1; }

#define OP_CHECK(COND, MSG) \
  if(COND) { printf("!A! "); printf(MSG); dbgEnv(); return 1; }

typedef ErrCode (*op_t)(OP_ARGS);

ErrCode op_notimpl(OP_ARGS) {
  fail("op not implemented");
}

ErrCode op_fetch(OP_ARGS) { out->v[0] = fetch(mem, out->v[0], out->sz); }
ErrCode op_store(OP_ARGS) { store(mem, out->v[1], out->v[0], out->sz); out->len = 0; }
// DVF
// DVS
ErrCode op_nop(OP_ARGS) {};
ErrCode op_drp(OP_ARGS) { out->v[0] = out->v[1]; out->len -= 1; };
ErrCode op_inv(OP_ARGS) { out->v[0] = ~out->v[0]; };
ErrCode op_neg(OP_ARGS) {
  switch (out->sz) {
    case 1: out->v[0] = (U32) (-(I8)out->v[0]); break;
    case 2: out->v[0] = (U32) (-(I16)out->v[0]); break;
    case 4: out->v[0] = (U32) (-(I32)out->v[0]); break;
  }
}
ErrCode op_eqz(OP_ARGS) { out->v[0] = out->v[0] == 0; }
ErrCode op_eqz_nc(OP_ARGS) { shift_op(out); out->v[0] = out->v[1] == 0; }
ErrCode op_drop2(OP_ARGS) { out->len = 0; }
ErrCode op_ovr(OP_ARGS) { shift_op(out); out->v[0] = out->v[2]; }
ErrCode op_add(OP_ARGS) { out->v[0] = out->v[1] + out->v[0]; out->len = 1; }
ErrCode op_sub(OP_ARGS) { out->v[0] = out->v[1] - out->v[0]; out->len = 1; }
ErrCode op_mod(OP_ARGS) { out->v[0] = out->v[1] % out->v[0]; out->len = 1; }
ErrCode op_mul(OP_ARGS) { out->v[0] = out->v[1] * out->v[0]; out->len = 1; }

op_t ops[] = {
  // FT,          SR,           DVF,          DVS,
  op_fetch,       op_store,     op_notimpl,   op_notimpl,

  // NOP,         DRP,          INV,          NEG,
  op_nop,         op_drp,       op_inv,       op_neg,

  // EQZ,        EQZ_NC,        DRP2,         OVR,
  op_eqz,        op_eqz_nc,     op_drop2,     op_ovr,

  // ADD,        SUB,           MOD,          MUL,
  op_add,        op_notimpl,    op_notimpl,   op_notimpl,
};

/*fn*/ U16 popImm() {
    U16 out = fetch(mem, env.ep, 2);
    env.ep += 2;
    return out;
}

#define dbgExecute() printf("sz=%x top=%x snd=%x len=%u srPtr=%x usesImm=%u\n", \
    sz, top, snd, len, srPtr, usesImm)

U8* _jz_jtbl_err = "JZ/JTBL require IMM for offset/table";
U8* _jmp_call_err = "call requies mem access";
U8* _jmp_mem_err = "jumps require Mem.Store = WS";

/*fn*/ ExecuteResult executeInstr(U16 instr) {
  ExecuteResult res = {};

  OpI opI =   (OpI)    (0x3F & instr);
  MemI i_mem = (MemI)   (0x7  & (instr >>              6));
  JmpI jmp =   (JmpI)   (0x7  & (instr >>     (2 + 3 + 6)));
  SzI szI =     (SzI)   (0x7  & (instr >> (3 + 2 + 3 + 6)));
  U8 sz = szToBytes(szI);

  if(opI == FT && !(i_mem == WS && szI == SzA)) {
      fail("FT must use WS and size=ptr");
  }

  U32 top = 0;
  U32 snd = 0;
  U8 len = 1;
  APtr srPtr = 0;
  U8 usesImm = FALSE;

  // *****************
  // * Mem: get the appropriate values

  // Get Top
  switch(i_mem) {
    case SRLI:
      usesImm = TRUE;
      srPtr = LS_OFFSET() + env.ls.sp + popImm();
      top = WS_POP(sz);
      break;
    case SRMI:
      usesImm = TRUE;
      srPtr = env.mp + popImm();
      top = WS_POP(sz);
      break;
    case SROI:
    case FTLI:
    case FTCI:
    case FTOI: fail("unknown mem");
    case IMWS:
      usesImm = TRUE;
      top = popImm();
      break;
    case WS:
      if(Stk_len(&env.ws) == 0) len = 0;
      else top = WS_POP(sz);
      break;
    default: fail("unknown mem");
  }

  // Get Second
  if(opI == SR) {
    assert(i_mem==WS || i_mem==IMWS);
    snd = WS_POP(ASIZE);
    len += 1;
  } else {
    if(i_mem == FTCI) {
      snd = popImm();
      len += 1;
    } elif(Stk_len(&env.ws) >= sz) {
      snd = WS_POP(sz);
      len += 1;
    }
  }

  // *************
  // * Op: perform the operation
  OpData data = {.v = {top, snd, 0}, .sz = sz, .len = len, .usesImm = usesImm };
  ops[(U8) opI] (&data); // call op from array

  APtr aptr;
  U16 growLs = 0;
  // *************
  // * Jmp: perform the jump
  switch(jmp) {
    case NOJ: break;
    case JZ: 
      if (i_mem != WS) fail(_jz_jtbl_err);
      env.ep = toAPtr(popImm(), 2);
      break;
    case JTBL:
      if (i_mem != WS) fail(_jz_jtbl_err);
      fail("not implemented");
    case JST:
      if(i_mem <= SROI) fail(_jmp_mem_err);
      assert(data.len > 0);
      env.ep = data.v[0];
      data.v[0] = data.v[1];
      data.v[1] = data.v[2];
      data.len -= 1;
      break;
    case _JR0: fail("JR0");
    case CALL:
      if(i_mem <= FTOI) fail(_jmp_call_err);
      aptr = toAPtr(WS_POP(sz), sz);
      growLs = fetch(mem, aptr, 2); // amount to grow, must be multipled by APtr size.
      Stk_grow(&env.ls, growLs << APO2);
      // Callstack has 4 byte value: growLs | module | 2-byte-cptr
      Stk_push(&env.callStk, (growLs << 24) + env.ep, 4);
      env.ep = aptr + 2;
      env.mp = aptr >> 8;
      break;
    case CNL: // call no locals
      if(i_mem <= SROI) fail(_jmp_mem_err);
      aptr = toAPtr(WS_POP(sz), sz);
      Stk_push(&env.callStk, env.ep, 4);
      env.ep = aptr;
      break;
    case RET:
      if(Stk_len(env.callStk) == 0) res.escape = TRUE;
      else {
        U32 callMeta = Stk_pop(&env.callStk, ASIZE);
        env.ep = MOD_HIGH_MASK & callMeta;
        Stk_shrink(&env.ls, (callMeta >> 24) << APO2);
      }
      break;
  }

  // *************
  // * Store Result
  if (srPtr && data.len > 0) store(mem, srPtr, data.v[0], sz);
  elif (data.len) Stk_push(&env.ws, data.v[0], sz);

  if (data.len > 1) Stk_push(&env.ws, data.v[1], sz);
  if (data.len > 2) Stk_push(&env.ws, data.v[2], sz);
  return res;
}

/*fn*/ ErrCode execute(U16 instr) {
  while(TRUE) {
    ExecuteResult res = executeInstr(instr);
    if(res.err) { return 1; }
    if(res.escape) {
      break;
    }
    U16 instr = fetch(mem, env.ep, 2);
    env.ep += 2;
  }
  env.ep = 0;
  return OK;
}

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) wherey is a cstr and value is U32.

#define Dict_key(OFFSET)  ((Key*) (((U8*)dict) + sizeof(Dict) + OFFSET))
// Given ptr to key, get pointer to the value.
#define Key_vptr(KEY) ((U32*) alignSys(((U8*)KEY) + KEY->len + 1, 4));

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

void dbgToken() {
  printf("token: size=%u, len=%u\n", tokenBufSize, tokenLen);
}

#define IS_WHITESPC(C) (C<=' ')

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

// Read bytes incrementing tokenBufSize
/*fn*/ void readAppend(read_t r) {
  r();
}

// clear token buf and read bytes
/*fn*/ void readNew(read_t r) {
  tokenLen = 0;
  tokenBufSize = 0;
  readAppend(r);
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
/*fn*/ ErrCode scan(read_t r) {

  // Skip whitespace
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew(r);
    if(tokenBufSize == 0) return OK;
    if (toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) {
      shiftBuf();
      break;
    }
    tokenLen += 1;
  }
  if(tokenBufSize < MAX_TOKEN) readAppend(r);

  U8 c = tokenBuf[tokenLen];
  tokenState->group = (U8) toTokenGroup(c);
  if(tokenState->group <= T_ALPHA) tokenState->group = T_ALPHA;

  // Parse token until the group changes.
  while(tokenLen < tokenBufSize) {
    OP_ASSERT(tokenLen < MAX_TOKEN, "token too large");
    c = tokenBuf[tokenLen];
    TokenGroup tg = toTokenGroup(c);
    if (tg == tokenState->group) {}
    elif (tokenState->group == T_ALPHA && (tg <= T_ALPHA)) {}
    else break;
    tokenLen += 1;
  }

  return OK;
}

/*fn*/ ErrCode tilNewline(read_t r) {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew(r);
    if (tokenBufSize == 0) return OK;
    if (tokenBuf[tokenLen] == '\n') return OK;
    tokenLen += 1;
  }
  return OK;
}

/*fn*/ ErrCode linestr(read_t r) {
  while(TRUE) {
    if (tokenLen >= tokenBufSize) readNew(r);
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
      elif(c == 'n') c = '\n';
      elif(c == 't') c = '\t';
      elif(c == '0') c = '\0';
      else OP_ASSERT(FALSE, "invalid escaped char");
    }
    store(mem, *env.heap, c, 1);
    *env.heap += 1;
    tokenLen += 1;
  }
  return OK;
}

// Taking a char that is known to be hex, return the hex value.
/*fn*/ ErrCode charToHex(U8 c) {
  c = c - '0';
  if(c <= 9) return c;
  c = c - ('A' - '0');
  if(c <= 5) return c + 10;
  c = c - ('a' - 'A');
  return c + 10;
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
/*fn*/ ErrCode tokenHex() {
  OP_ASSERT(tokenLen > 0, "hanging #");
  U32 v = 0;
  U8 i = 0;
  U8 tkLen = 0;
  while(i < tokenLen) {
    U8 c = tokenBuf[i];

    if (c == '_') { i+= 1; continue; }
    OP_ASSERT(toTokenGroup(c) <= T_HEX, "non-hex number");
    v = (v << 4) + charToHex(c);

    tkLen += 1;
    i += 1;
  }
  // convert tkLen to sz bytes.
  if(tkLen <= 2) tkLen = 1;
  elif(tkLen <= 4) tkLen = 2;
  else tkLen = 4;
  Stk_push(&env.ws, v, tkLen);
  shiftBuf();
  return OK;
}

/*fn*/ ErrCode readSz(read_t r, U8* sz) {
  if(tokenLen >= tokenBufSize) readAppend(r);
  *sz = charToHex(tokenBuf[tokenLen]);
  tokenLen += 1;
  return OK;
}

/*fn*/ ErrCode readSzPush(read_t r, U32 value) {
  // read the next symbol to get sz and push value.
  U8 sz; OP_CHECK(readSz(r, &sz), "readSzPush");
  OP_ASSERT(sz == 2 || sz == 4, "size invalid");
  OP_CHECK(Stk_push(&env.ws, value, sz), "readSzPush.push");
  return OK;
}

/*fn*/ ErrCode readSzPop(read_t r, U32* out) {
  U8 sz; OP_CHECK(readSz(r, &sz), "readSzPop");
  OP_ASSERT(sz == 2 || sz == 4, "size invalid");
  *out = WS_POP(sz);
  return OK;
}

/*fn*/ ErrCode cPutLoc(read_t r) { // `&`
  OP_ASSERT(tokenLen == 1, "only one & allowed");
  readSzPush(r, *env.heap);
  return OK;
}

/*fn*/ ErrCode cNameSet(read_t r) { // `=`
  U32 value; OP_CHECK(readSzPop(r, &value), "cNameSet.read");

  OP_CHECK(scan(r), "cNameSet.scan"); // load name token
  Dict_set(tokenLen, tokenBuf, value);
  return OK;
}

/*fn*/ ErrCode cNameGet(read_t r) { // `@`
  U8 sz; OP_CHECK(readSz(r, &sz), "cNameGet");
  OP_CHECK(scan(r), "@ scan"); // load name token
  U32 value; OP_CHECK(Dict_get(&value, tokenLen, tokenBuf), "@ no name");
  OP_CHECK(Stk_push(&env.ws, value, sz), "& push");
  return OK;
}

/*fn*/ ErrCode cNameForget(read_t r) { // `~`
  OP_CHECK(scan(r), "~ scan");
  Dict_forget(tokenLen, tokenBuf);
  return OK;
}

/*fn*/ ErrCode cWriteHeap(read_t r) { // `,`
  U8 sz; OP_CHECK(readSz(r, &sz), ",.sz");
  U32 value = WS_POP(sz);
  store(mem, *env.heap, value, sz);
  *env.heap += sz;
  return OK;
}

/*fn*/ ErrCode cWriteInstr(read_t r) { // `;`
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
  U16 i = instr;
  instr = INSTR_DEFAULT;
  return execute(i);
}

/*fn*/ ErrCode cExecute(read_t r) { // $
  instr = INSTR_DEFAULT;
  OP_CHECK(scan(r), "$ scan");
  U32 value; OP_CHECK(Dict_get(&value, tokenLen, tokenBuf), "$: no name");
  WS_PUSH(value, 4);
  return execute(INSTR_CNL);
}

/*fn*/ ErrCode compile(read_t r) {
  while(TRUE) {
    scan(r);
    if(tokenLen == 0) return OK;
    if(tokenState->group <= T_ALPHA) {
      OP_CHECK(updateInstr(), "compile.instr");
      continue;
    }
    tokenLen = 1; // allows for multi symbols where valid, i.e. =$, $$
    switch (tokenBuf[0]) {
      case '/': OP_CHECK(tilNewline(r), "compile /"); continue;
      case '"': OP_CHECK(linestr(r), "compile \""); continue;
      case '#': scan(r); OP_CHECK(tokenHex(), "compile #"); continue;
      case '&': OP_CHECK(cPutLoc(r), "compile &"); continue;
      case '=': OP_CHECK(cNameSet(r), "compile ="); continue;
      case '@': OP_CHECK(cNameGet(r), "compile @"); continue;
      case '~': OP_CHECK(cNameForget(r), "compile ~"); continue;
      case ',': OP_CHECK(cWriteHeap(r), "compile ,"); continue;
      case ';': OP_CHECK(cWriteInstr(r), "compile ;"); continue;
      case '^': OP_CHECK(cExecuteInstr(), "compile ^"); continue;
      case '$': OP_CHECK(cExecute(r), "compile $"); continue;
    }
    printf("invalid token: %c", tokenBuf[0]);
    return 1;
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
  assert(!compile(*readSrc));

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
  printf("tokenGroup=%u  ", tokenState->group);
  printf("instr=0x%x\n", instr, instr);
}

#define TEST_ENV_BARE \
  SMALL_ENV_BARE; \
  U32 heapStart = *env.heap

#define TEST_ENV \
  SMALL_ENV \
  U32 heapStart = *env.heap


U8* testBuf = NULL;
U16 testBufIdx = 0;

// read_t used for testing.
/*test*/ ssize_t testing_read() {
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
  assert(!compile(*testing_read));


/*test*/ void testHex() {
  printf("## testHex #01...\n"); TEST_ENV_BARE;

  COMPILE("#10");
  assert(WS_POP(1) == 0x10);

  printf("## testHex #10AF...\n");
  COMPILE("/comment\n#10AF");
  U32 result = WS_POP(2);
  assert(result == 0x10AF);
}

/*test*/ void testLoc() {
  printf("## testLoc...\n"); TEST_ENV_BARE;
  COMPILE("&4 &2");
  U16 result1 = WS_POP(2); U32 result2 = WS_POP(4);
  assert(result1 == (U16)heapStart);
  assert(result2 == heapStart);
}

/*test*/ void testQuotes() {
  printf("## testQuotes...\n"); TEST_ENV_BARE;
  COMPILE("\"foo bar\" baz\\0\n");

  assert(0 == strcmp(mem + heapStart, "foo bar\" baz"));
}

/*test*/ void testDictDeps() {
  printf("## testDict... cstr\n"); TEST_ENV_BARE;
  assert(cstrEq(1, 1, "a", "a"));
  assert(!cstrEq(1, 1, "a", "b"));

  assert(cstrEq(2, 2, "z0", "z0"));
  assert(!cstrEq(2, 1, "aa", "a"));
  assert(!cstrEq(2, 2, "aa", "ab"));

  printf("## testDict... dict\n");
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

  COMPILE("#0F00 =2foo  #000B_A2AA =4bazaa @4bazaa @4foo @2foo ");
  assert(0xF00 == WS_POP(2));   // 2foo
  assert(0xF00 == WS_POP(4));   // 4foo
  assert(0xBA2AA == WS_POP(4)); // 4bazaa
}

/*test*/ void testWriteHeap() { // test , and ;
  printf("## testWriteHeap\n"); TEST_ENV_BARE;
  COMPILE("#77770101,4 #0F00,2 ;");
  assert(0x77770101 == fetch(mem, heapStart, 4));
  assert(0x0F00 == fetch(mem, heapStart+4, 2));
  assert(INSTR_DEFAULT == fetch(mem, heapStart+6, 2));
}

/*test*/ void testExecuteInstr() { // test ^
  printf("## testExecuteInstr\n"); SMALL_ENV;
  COMPILE("@4 Sz2");
  assert(0xC0004000 == WS_POP(4));

  instr = 0xF9FF; // instr with unused=0
  COMPILE("Sz4 NOJ WS NOP");
  assert(INSTR_DEFAULT == instr);

  COMPILE("#01 #02   Sz1 ADD RET^");
  assert(0x03 == WS_POP(1));
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

