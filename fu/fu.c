#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

// ********************************************
// ** Core Types

typedef uint8_t Bool;
typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint32_t ASz;
typedef ASz APtr;

typedef U16 CSz;
typedef CSz CPtr;

#define ASIZE sizeof(ASz)
#define CSIZE sizeof(CSz)

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

  Stk ws;
  Stk rs;
} Env;

typedef struct {
  U32 v[3];
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
    case S_U8: 1; break;
    case S_U16: 2; break;
    case S_U32: 4; break;
    default: fail("invalid Sz");
  }
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

// ********************************************
// ** Operations

#define OP_ARGS Env* env, OpData *out
typedef void (*op_t)(OP_ARGS);

void op_notimpl(OP_ARGS) {
  fail("op not implemented");
}

void op_fetch(OP_ARGS) {
  out->v[0] = fetch(env->mem, out->v[0], out->sz);
}

void op_store(OP_ARGS) {
  store(env->mem, out->v[1], out->v[0], out->sz);
  out->len = 0;
}

void op_nop(OP_ARGS) {};


op_t ops[] = {
  // FT,          SR,           DVF,          DVS,
  op_fetch,       op_store,     op_notimpl,   op_notimpl,

  // NOP,         DRP,          INV,          NEG,
  op_nop,         op_notimpl,   op_notimpl,   op_notimpl,

  // EQZ,        EQZ_NC,       DRP2,         OVR,
  op_notimpl,    op_notimpl,   op_notimpl,   op_notimpl,

  // ADD,          SUB,          MOD,
  op_notimpl,    op_notimpl,   op_notimpl,   op_notimpl,
};

// ********************************************
// ** Initialization and Main
#define NEW_ENV(WSM, RSM, MEM)            \
  {                                       \
    .mem = MEM,                           \
    .cp = 0,                              \
    .lp = MEM_SIZE,                       \
    .ws = { .sp = WS_SIZE, .mem = WSM },  \
    .rs = { .sp = RS_SIZE, .mem = RSM },  \
  }


int main() {
  U8 mem[MEM_SIZE];
  U8 wsMem[WS_SIZE];
  U8 rsMem[RS_SIZE];

  printf("Fu\n");
  printf("%i", SRCP);

  Env env = NEW_ENV(mem, wsMem, rsMem);

  fail("bad");
  return 0;
}
