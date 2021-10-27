#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>



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


#define ASIZE sizeof(ASz)
#define CSIZE sizeof(CSz)
#define FALSE 0
#define TRUE 1
#define elif else if
#define NULL16 0xFFFF
#define OK 0

typedef U8 ErrCode;

// Size
typedef enum {
  S_U8,   S_U16,
  S_U32,  S_UNDEF
} SzBits;

// Mem
typedef enum {
  SRLP,   SRCP,   SROI,   FTLP,
  FTCI,   FTOI,   IMWS,   WS,
} Mem;

// Jmp
typedef enum {
  JZ,     CALL,   JST,    CNW,
  JTBL,   _JR0,   RET,    NOJ,
} Jmp;

// Operation
typedef enum {
  FT,           SR,           DVF,          DVS,
  NOP,          DRP,          INV,          NEG,
  EQZ,          EQZ_NC,       DRP2,         OVR,
  ADD,          SUB,          MOD,          MUL,
} Op;

// 1MiB
#define MEM_SIZE 0x100000
#define WS_SIZE  0x100
#define RS_SIZE  0x100
#define LS_SIZE  0x8000
#define DICT_SIZE 0x1000

// Generic stack.
typedef struct {
  U16 sp;
  U16 size;
  U8* mem;
} Stk;

// Environment
typedef struct {
  APtr cp;            // seCtion Pointer
  APtr lp;            // local stack pointer
  APtr* heap;
  APtr* topHeap;
  APtr* topMem;
  Stk ls;

  // Separate from mem
  Stk ws;
  Stk rs;
} Env;

Env env;
U8* mem;

typedef struct {
  U32 v[3]; // value "stack". 0=top, 1=scnd, 2=extra
  SzBits sz;
  U8 len;
  Bool usesImm;
} OpData;

// ********************************************
// ** Helpers

void fail(U8* cstr) {
  printf(cstr);
  printf("\n");
  exit(1);
}

ErrCode szBits_toBytes(SzBits sz) {
  switch (sz) {
    case S_U8: return 1;
    case S_U16: return 2;
    case S_U32: return 4;
    default: fail("invalid Sz");
  }
}

SzBits bytesToSz(U8 bytes) {
  if(bytes <= 1) return S_U8;
  if(bytes <= 2) return S_U16;
  if(bytes <= 4) return S_U32;
  fail("invalid bytes");
}

void* alignSys(void* p, U8 szBytes) {
  U8 mod = (size_t)p % szBytes;
  if(mod == 0) return p;
  return p + (szBytes - mod);
}

APtr align(APtr aPtr, U8 szBytes) {
  U8 mod = aPtr % szBytes;
  if(mod == 0) return aPtr;
  return aPtr + (szBytes - mod);
}

void store(U8* mem, APtr aptr, U32 value, SzBits sz) {
  switch (sz) {
    case S_U8: 
      *(mem+aptr) = (U8)value;
      break;
    case S_U16: 
      assert(aptr % 2 == 0);
      *(((U16*) mem)+aptr) = (U16)value;
      break;
    case S_U32:
      assert(aptr % 4 == 0);
      *(((U32*) mem)+aptr) = value;
      break;
    default: fail("store: invalid Sz");
  }
}

U32 fetch(U8* mem, APtr aptr, SzBits sz) {
  U8 value;
  switch (sz) {
    case S_U8: 
      return (U32) *((U8*) (mem+aptr));
    case S_U16: 
      assert(aptr % 2 == 0);
      return *(((U16*)mem)+aptr);
    case S_U32:
      assert(aptr % 4 == 0);
      return *(((U32*)mem)+aptr);
    default: fail("fetch: invalid Sz");
  }
}

ErrCode Stk_push(Stk* stk, U32 value, SzBits sz) {
  U8 szBytes = szBits_toBytes(sz);
  if(stk->sp < szBytes) { fail("stack overflow"); }
  store(stk->mem, stk->sp - szBytes, value, sz);
  stk->sp -= szBytes;
  return OK;
}

U32 Stk_pop(Stk* stk, SzBits sz) {
  U8 szBytes = szBits_toBytes(sz);

  if(stk->sp + szBytes > stk->size) { fail ("stack underflow"); }
  U32 out = fetch(stk->mem, stk->sp, sz);
  stk->sp += szBytes;
  return out;
}

// Shift opdata to the right.
void shift_op(OpData* out) {
  out->v[2] = out->v[1];
  out->v[1] = out->v[0];
  out->len += 1;
}

// ********************************************
// ** Operations

#define OP_ARGS OpData *out
#define OP_ASSERT(COND, MSG) \
  if(!(COND)) { printf(MSG); printf("\n"); return 1; }

#define OP_CHECK(COND, MSG) \
  if(COND) { printf(MSG); printf("\n"); return 1; }

typedef U8 (*op_t)(OP_ARGS);

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
    case S_U8: out->v[0] = (U32) (-(I8)out->v[0]); break;
    case S_U16: out->v[0] = (U32) (-(I16)out->v[0]); break;
    case S_U32: out->v[0] = (U32) (-(I32)out->v[0]); break;
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

// ********************************************
// ** Spore Dict
// key/value map (not hashmap) wherey is a cstr and value is U32.
typedef struct {
  U16 heap;  // heap offset
  U16 end;   // end offset
} Dict;
Dict* dict = NULL;

typedef struct {
  U8 len;
  U8 s[];
} Key;

#define Dict_key(OFFSET)  ((Key*) (dict + sizeof(Dict) + OFFSET))
// Given ptr to key, get pointer to the value.
#define Key_vptr(KEY) ((U32*) alignSys(KEY + KEY->len + 1, 4));

U8 cstrEq(U8 slen0, U8 slen1, U8* s0, U8* s1) {
  if(slen0 != slen1) return FALSE;
  for(U8 i = 0; i < slen0; i += 1) {
    if(s0[i] != s1[i]) return FALSE;
  }
  return TRUE;
}

// find key offset from dict. Else return dict.heap
U16 Dict_find(U8 slen, U8* s) {
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

U16 Dict_set(U8 slen, U8* s, U32 value) {
  // Set a key to a value, returning the offset
  U16 offset = Dict_find(slen, s);
  Key* key = Dict_key(offset);
  if(offset == dict->heap) {
    // new key
    key->len = slen;
    memcpy(key->s, s, slen);   // memcpy(dst, src, sz)
    dict->heap += align(1 + slen + 4, 4);
  }
  U32* v = Key_vptr(key);
  *v = value;
  return offset;
}

ErrCode Dict_get(U32* out, U8 slen, U8 *s) {
  U16 offset = Dict_find(slen, s);
  OP_ASSERT(offset != dict->heap, "key not found");
  *out = *Key_vptr(Dict_key(offset));
  return OK;
}

void Dict_forget(U8 slen, U8* s) {
  dict->heap = Dict_find(slen, s);
}


// ********************************************
// ** Scanner

typedef enum {
  T_NUM, T_HEX, T_ALPHA, T_SPECIAL, T_SYMBOL, T_WHITE
} TokenGroup;

#define MAX_TOKEN 32
#define TOKEN_BUF 0x7D

TokenGroup tokenGroup;

typedef struct {
  U8 group;
  U8 len;   // length of token
  U8 size;  // characters buffered
  U8 buf[]; // size=TOKEN_BUF
} TokenState;

TokenState* tokenState = NULL;

#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf tokenState->buf
#define tokenGroup ((TokenGroup) tokenState->group)

void dbgToken() {
  printf("token: size=%u, len=%u\n", tokenBufSize, tokenLen);
}

#define IS_WHITESPC(C) (C<=' ')

TokenGroup toTokenGroup(U8 c) {
  if(c <= ' ') return T_WHITE;
  if('0' <= c && c <= '9') return T_NUM;
  if('a' <= c && c <= 'f') return T_HEX;
  if('A' <= c && c <= 'F') return T_HEX;
  if('g' <= c && c <= 'z') return T_ALPHA;
  if('G' <= c && c <= 'Z') return T_ALPHA;
  if(c == '~' || c == '\'' || c == '$' ||
     c == '.' || c ==  '(' || c == ')') {
    return T_SPECIAL;
  }
  return T_SYMBOL;
}

typedef ssize_t (*read_t)(size_t nbyte);

// Read bytes incrementing tokenBufSize
void readAppend(read_t r) {
  r(TOKEN_BUF - tokenBufSize);
}

// clear token buf and read bytes
void readNew(read_t r) {
  tokenLen = 0;
  tokenBufSize = 0;
  readAppend(r);
}

ErrCode shiftBuf() {
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

// Scans next token.
ErrCode scan(read_t r) {

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

  tokenLen = 0;
  U8 c = tokenBuf[tokenLen];
  tokenState->group = (U8) toTokenGroup(c);
  if(tokenGroup <= T_ALPHA) tokenState->group = T_ALPHA;

  // Parse token until the group changes.
  while(tokenLen < tokenBufSize) {
    c = tokenBuf[tokenLen];
    TokenGroup tg = toTokenGroup(c);
    if (tg == tokenGroup) {}
    elif (tokenGroup == T_ALPHA && tg <= T_ALPHA) {}
    else break;
    OP_ASSERT(tokenLen < MAX_TOKEN, "token too large");
    tokenLen += 1;
  }

  return OK;
}

ErrCode tilNewline(read_t r) {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew(r);
    if (tokenBufSize == 0) return OK;
    if (tokenBuf[tokenLen] == '\n') return OK;
    tokenLen += 1;
  }
  return OK;
}

ErrCode linestr(read_t r) {
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
    mem[*env.heap] = c;
    *env.heap += 1;
    tokenLen += 1;
  }
  return OK;
}

// Taking a char that is known to be hex, return the hex value.
ErrCode charToHex(U8 c) {
  c = c - '0';
  if(c <= 9) return c;
  c = c - ('A' - '0');
  if(c <= 5) return c + 10;
  c = c - ('a' - 'A');
  return c + 10;
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
ErrCode tokenHex() {
  OP_ASSERT(tokenLen > 0, "hanging #");
  U32 v = 0;
  U8 i = 0;
  U8 tokenSize = 0;
  while(i < tokenLen) {
    U8 c = tokenBuf[i];

    if (c == '_') { i+= 1; continue; }
    OP_ASSERT(toTokenGroup(c) <= T_HEX, "non-hex number");
    v = (v << 4) + charToHex(c);

    tokenSize += 1;
    i += 1;
  }
  Stk_push(&env.ws, v, bytesToSz((tokenSize>>1) + tokenSize % 2));
  shiftBuf();
  return OK;
}

ErrCode putLoc(read_t r) { // `&`
  OP_ASSERT(tokenLen == 1, "only one & allowed");
  if(tokenLen >= tokenBufSize) readAppend(r);
  U8 szBytes = charToHex(tokenBuf[tokenLen]);
  SzBits sz = bytesToSz(szBytes);
  tokenLen += 1;
  OP_ASSERT(sz == S_U16 || sz == S_U32, "& size invalid");
  OP_CHECK(Stk_push(&env.ws, *env.heap, sz), "putLoc.push");
  return OK;
}

ErrCode compile(read_t r) {
  while(TRUE) {
    scan(r);
    if(tokenLen == 0) return OK;
    char c = tokenBuf[0];
    if(c == '/') { OP_CHECK(tilNewline(r), "compile.tilNewline"); }
    elif (c == '"') { OP_CHECK(linestr(r), "compile.linestr"); }
    elif (c == '#') { scan(r); OP_CHECK(tokenHex(), "compile.tokenHex"); }
    elif (c == '&') OP_CHECK(putLoc(r), "compile.putLoc");
  }
  return OK;
}


// ********************************************
// ** Initialization
#define NEW_ENV(MS, WS, RS, LS, DS)       \
  U8 localMem[MS] = {0};                       \
  U8 wsMem[WS];                           \
  U8 rsMem[RS];                           \
  mem = localMem; \
  env = (Env) {                           \
    .cp = 0,                              \
    .heap = (APtr*) (mem + 4),              \
    .topHeap = (APtr*) (mem + 8),           \
    .topMem = (APtr*) (mem + 12),           \
    .ls = { .size = LS, .sp = LS },       \
    .ws = { .size = WS, .sp = WS, .mem = wsMem },     \
    .rs = { .size = RS, .sp = RS, .mem = rsMem },     \
  };                                      \
  /* configure heap+topheap */            \
  *env.heap = 16;                         \
  *env.topHeap = MS;                      \
  *env.topMem = MS;                       \
  /* put Local Stack, Dict on topheap */  \
  *env.topHeap -= LS;                     \
  env.ws.mem = mem + *env.topHeap;        \
  *env.topHeap -= sizeof(TokenState) + TOKEN_BUF; \
  tokenState = (TokenState*) (mem + *env.topHeap); \
  *env.topHeap -= DS;                      \
  dict = (Dict*) (mem + *env.topHeap);

#define SMALL_ENV \
  /*      MS      WS     RS     LS     DICT */    \
  NEW_ENV(0x4000, 0x100, 0x100, 0x200, 0x200)


// ********************************************
// ** Main
void tests();

int main() {
  printf("compiling spore...:\n");

  tests();
  return OK;
}

// ********************************************
// ** Tests
#include <string.h>

#define TEST_ENV \
  SMALL_ENV; \
  U32 heapStart = *env.heap

U8* testBuf = NULL;
U16 testBufIdx = 0;

// read_t used for testing.
ssize_t testing_read(size_t nbyte) {
  size_t i = 0;
  while (i < nbyte) {
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

#define POP(S)  Stk_pop(&env.ws, S)


ErrCode testHex() {
  TEST_ENV;
  printf("... mem=%x heap=%x topHeap=%x ts=%x\n", mem, env.heap, env.topHeap, tokenState);

  printf("## testHex #01...\n");
  COMPILE("#10\0");
  assert(POP(S_U8) == 0x10);

  printf("## testHex #10AF...\n");
  COMPILE("/comment\n#10AF\0");
  U32 result = POP(S_U16);
  assert(result == 0x10AF);

  return OK;
}

ErrCode testLoc() {
  printf("## testLoc...\n");
  TEST_ENV;
  COMPILE("&4 &2");
  U16 result1 = POP(S_U16); U32 result2 = POP(S_U32);
  assert(result1 == (U16)heapStart);
  assert(result2 == heapStart);
  return OK;
}

ErrCode testQuotes() {
  printf("## testQuotes...\n");
  TEST_ENV;
  COMPILE("\"foo bar\" baz\\0\n\0");

  assert(0 == strcmp(mem + heapStart, "foo bar\" baz\0"));
  return OK;
}

ErrCode testDict() {
  printf("## testDict... cstr\n");
  assert(cstrEq(1, 1, "a", "a"));
  assert(!cstrEq(1, 1, "a", "b"));

  assert(cstrEq(2, 2, "z0", "z0"));
  assert(!cstrEq(2, 1, "aa", "a"));
  assert(!cstrEq(2, 2, "aa", "ab"));

  printf("## testDict... dict\n");
  TEST_ENV;
  return OK;
}

void tests() {
  assert(!testHex());
  assert(!testLoc());
  assert(!testQuotes());
  assert(!testDict());
}

