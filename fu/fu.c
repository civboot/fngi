#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>


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
  U8* mem;
  APtr cp;            // seCtion Pointer
  APtr lp;            // local stack pointer
  APtr heap;

  Stk ws;
  Stk rs;
  Stk ls;
  Stk dict;
} Env;

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

U8 szBits_toBytes(SzBits sz) {
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

void store(U8* mem, APtr aptr, U32 value, SzBits sz) {
  switch (sz) {
    case S_U8: 
      *(mem+aptr) = (U8)value;
      break;
    case S_U16: 
      assert(aptr % 2 == 0);
      *(mem+aptr) = (U16)value;
      break;
    case S_U32:
      assert(aptr % 4 == 0);
      *(mem+aptr) = (U32)value;
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
      return (U32) *((U16*) (mem+aptr));
    case S_U32:
      assert(aptr % 4 == 0);
      return (U32) *((U32*) (mem+aptr));
    default: fail("fetch: invalid Sz");
  }
}

U8 Stk_push(Stk* stk, U32 value, SzBits sz) {
  U8 szBytes = szBits_toBytes(sz);
  if(stk->sp < szBytes) { fail("stack overflow"); }
  store(stk->mem, stk->sp - szBytes, value, sz);
  stk->sp -= szBytes;
}

U32 Stk_pop(Stk* stk, SzBits sz) {
  U8 szBytes = szBits_toBytes(sz);
  if(stk->sp + szBytes >= stk->size) { fail ("stack underflow"); }
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

#define OP_ARGS Env* env, OpData *out
#define OP_ASSERT(COND, MSG) \
  if(!(COND)) { printf(MSG); printf("\n"); return 1; }

#define OP_CHECK(COND) \
  if(COND) { return 1; }

typedef U8 (*op_t)(OP_ARGS);

U8 op_notimpl(OP_ARGS) {
  fail("op not implemented");
}

U8 op_fetch(OP_ARGS) { out->v[0] = fetch(env->mem, out->v[0], out->sz); }
U8 op_store(OP_ARGS) { store(env->mem, out->v[1], out->v[0], out->sz); out->len = 0; }
// DVF
// DVS
U8 op_nop(OP_ARGS) {};
U8 op_drp(OP_ARGS) { out->v[0] = out->v[1]; out->len -= 1; };
U8 op_inv(OP_ARGS) { out->v[0] = ~out->v[0]; };
U8 op_neg(OP_ARGS) {
  switch (out->sz) {
    case S_U8: out->v[0] = (U32) (-(I8)out->v[0]); break;
    case S_U16: out->v[0] = (U32) (-(I16)out->v[0]); break;
    case S_U32: out->v[0] = (U32) (-(I32)out->v[0]); break;
  }
}
U8 op_eqz(OP_ARGS) { out->v[0] = out->v[0] == 0; }
U8 op_eqz_nc(OP_ARGS) { shift_op(out); out->v[0] = out->v[1] == 0; }
U8 op_drop2(OP_ARGS) { out->len = 0; }
U8 op_ovr(OP_ARGS) { shift_op(out); out->v[0] = out->v[2]; }
U8 op_add(OP_ARGS) { out->v[0] = out->v[1] + out->v[0]; out->len = 1; }
U8 op_sub(OP_ARGS) { out->v[0] = out->v[1] - out->v[0]; out->len = 1; }
U8 op_mod(OP_ARGS) { out->v[0] = out->v[1] % out->v[0]; out->len = 1; }
U8 op_mul(OP_ARGS) { out->v[0] = out->v[1] * out->v[0]; out->len = 1; }

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

TokenState* tokenState; // memory inside env.mem for access by program.

#define tokenBufSize (tokenState->size)
#define tokenLen tokenState->len
#define tokenBuf tokenState->buf
#define tokenGroup ((TokenGroup) tokenState->group)

void dbgToken() {
  printf("token: size=%u, len=%u\n", tokenBufSize, tokenLen);
}

#define IS_WHITESPC(C) (C<=' ')

TokenGroup toTokenGroup(U8 c) {
  printf("toTokenGroup: %u %c\n", c, c);
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
  printf("readNew...");
    tokenLen = 0;
    tokenBufSize = 0;
    readAppend(r);
}




U8 shiftBuf() {
  // Shift buffer left from end of token
  if(tokenLen == 0) return 0;
  U8 newStart = tokenLen;
  U8 i = 0;
  while(tokenLen < tokenBufSize) {
    tokenBuf[i] = tokenBuf[tokenLen];
    i += 1;
    tokenLen += 1;
  }
  tokenBufSize = tokenBufSize - tokenLen;
  tokenLen = 0;
}

// Scanns next token for use by either fu or fni
U8 scan(read_t r) {
  while(TRUE) {
    printf("scanning..."); dbgToken();
    if(tokenLen >= tokenBufSize) {
      tokenLen = 0;
      readNew(r);
    }
    if(tokenBufSize == 0) return 0;

    if (toTokenGroup(tokenBuf[tokenLen]) != T_WHITE) {
      shiftBuf();
      break;
    }
    tokenLen += 1;
  }
  if(tokenBufSize < MAX_TOKEN) readAppend(r);
  printf("scanning non-ws...\n");
  dbgToken();

  tokenLen = 0;
  U8 c = tokenBuf[tokenLen];
  tokenState->group = (U8) toTokenGroup(c);
  if(tokenGroup <= T_ALPHA) tokenState->group = T_ALPHA;

  while(tokenLen < tokenBufSize) {
    TokenGroup tg = toTokenGroup(c);
    printf("tg=%u", tg);
    if (tg == tokenGroup) {}
    elif (tokenGroup == T_ALPHA && tg <= T_ALPHA) {}
    else break;
    OP_ASSERT(tokenLen < MAX_TOKEN, "token too large");
    tokenLen += 1;
    c = tokenBuf[tokenLen];
  }
  printf("... scan done."); dbgToken();
}

U8 tilNewline(read_t r) {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) {
      readNew(r);
    }
    if (tokenBufSize == 0) return 0;
    if (tokenBuf[tokenLen] == '\n') return 0;
  }
}

U8 linestr(read_t r, Env* env) {
  while(TRUE) {
    if(tokenLen >= tokenBufSize) readNew(r);
    if (tokenBufSize == 0) return 0;
    if (tokenBuf[tokenLen] == '\n') {
      tokenLen += 1;
      return shiftBuf();
    }

    char c = tokenBuf[tokenLen];
    if(c == '\\') {
      tokenLen += 1;
      OP_ASSERT(tokenLen < tokenBufSize, "Hanging \\");
      c = tokenBuf[tokenLen];
      if(c == '\\') {} // leave as-is
      elif(c == 'n') c = '\n';
      elif(c == 't') c = '\t';
      elif(c == '0') c = '\0';
      else OP_ASSERT(FALSE, "invalid escaped char");
    }
    env->mem[env->heap] = c;
    env->heap += 1;
  }
}

// Taking a char that is known to be hex, return the hex value.
U8 charToHex(U8 c) {
  c = c - '0';
  if(c <= 9) return c;
  c = c - ('A' - '0');
  if(c <= 5) return c + 10;
  c = c - ('a' - 'A');
  return c + 10;
}

// Parse a hex token from the tokenLen and shift it out.
// The value is pushed to the ws.
U8 tokenHex(Env* env) {
  printf("tokenHex...\n");
  OP_ASSERT(tokenLen > 0, "hanging #");
  U32 v = 0;
  U8 i;
  U8 tokenSize = 0;
  while(i < tokenLen) {
    U8 c = tokenBuf[i];
    if (c == '_') { i+= 1; continue; }
    OP_ASSERT(toTokenGroup(c) <= T_HEX, "non-hex number");
    v = (v << 4) + charToHex(c);
    tokenSize += 1;
    i += 1;
  }

  Stk_push(&env->ws, v, bytesToSz((tokenSize>>1) + tokenSize % 2));
  shiftBuf();
}

U8 compile(read_t r, Env* env) {
  while(TRUE) {
    printf("compile...\n");
    scan(r);
    if(tokenLen == 0) return 0;
    char c = tokenBuf[0];
    printf("char: %u\n", c);
    if(c == '/') { OP_CHECK(tilNewline(r)); }
    elif (c == '"') { OP_CHECK(linestr(r, env)); }
    elif (c == '#') { shiftBuf(); scan(r); OP_CHECK(tokenHex(env)); }
  }
}


// ********************************************
// ** Initialization
#define TOKEN_LOC(mem) (mem + 4)

#define HEAP_LOC  \
  ( \
    sizeof(TokenState) + TOKEN_BUF  \
  )

#define NEW_ENV(MS, WS, RS, LS, DS)       \
  U8 mem[MS];                             \
  tokenState = (TokenState*) TOKEN_LOC(mem); \
  U8 wsMem[WS];                           \
  U8 rsMem[RS];                           \
  Env env = {                             \
    .mem = mem,                           \
    .cp = 0,                              \
    .heap = HEAP_LOC,                     \
    .ws = { .sp = WS, .mem = wsMem },     \
    .rs = { .sp = RS, .mem = rsMem },     \
    .ls = {                               \
      .sp = LS_SIZE,                      \
        .mem = mem+MS-LS                  \
    },                                    \
    .dict = {                             \
      .sp = DS,                           \
      .mem = mem+MS-LS-DS,                \
    },                                    \
  }


#define SMALL_ENV \
  /*      MS      WS     RS     LS     DS */  \
  NEW_ENV(0x4000, 0x100, 0x100, 0x200, 0x200)

// ********************************************
// ** Tests
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

U8 testHex() {
  printf("## testHex...\n");
  SMALL_ENV;
  testBuf = "#01";
  OP_CHECK(compile(*testing_read, &env));
  assert(Stk_pop(&env.ws, S_U8) == 0x01);

  return 0;
}

U8 tests() {
  OP_CHECK(testHex());
}

int main() {
  printf("compiling fu...:\n");


  tests();
  return 0;
}
