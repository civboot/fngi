#ifndef __FNGI_TY_H
#define __FNGI_TY_H
#include <stdint.h>
#include "spor_alloc.h"
#include "constants.h"

#define ASIZE   4
#define SZA     SZ4

typedef uint8_t              Bool;
typedef uint8_t              U1;
typedef uint16_t             U2;
typedef uint32_t             U4;
typedef uint32_t             UA;
typedef int8_t               I1;
typedef int16_t              I2;
typedef int32_t              I4;
typedef uint32_t             APtr;
typedef uint32_t             Ref;

typedef struct { U2 sp; U2 size; U1* mem; } Stk;

typedef struct { APtr ref; U2 len; U2 cap; } Dict;

typedef struct {
  APtr buf; // buffer.
  U2 len;   // length of token
  U2 size;  // total characters buffered
  U1 group;     U1 _align; U2 _align2;
} TokenState;

typedef struct {
  U1 valTy;     U1 _align;  U2 _align2;
  U2 valueASz;
  U2 valueBSz;
  U4 valueA;
  U4 valueB;
  APtr msg;
} ErrData;

// Glue for these types in spor-space
typedef struct { APtr nodes; APtr blocks; U1 cap; U1 rooti; } S_BlockAllocator;
typedef struct { APtr ba; U1 rooti; U2 len; U2 cap; } S_BlockBumpArena;
#define BA_fromS(S) { .rooti=(S)->rooti, \
  .nodes=(BlockNode*) (mem+(S)->nodes), .blocks=(block_t*) (mem + (S)->blocks)}
#define BBA_fromS(S, BA) {.ba=(BA), .len=(S).len, .cap=(S).cap}

typedef struct {
  APtr _null;
  APtr heap;
  APtr topHeap;
  APtr topMem;
  U2 err;
  U2 state;
  U4 _unimpl1;
  U4 _unimpl2;
  U2 sysLogLvl;
  U2 usrLogLvl;
  Dict dict;
  Dict ldict;
  TokenState ts;
  ErrData errData;
  S_BlockBumpArena bba;
  APtr gkey;
  APtr lkey;
  APtr gheap;
  U2 localOffset;
  APtr compFn;
  S_BlockAllocator ba;
} Globals;

typedef struct {
} Kern;

// Environment
typedef struct {
  Globals* g;
  Kern* k;
  APtr ep;  // execution pointer
  APtr gb;  // Global Base
  Stk ls;

  // Working and Call Stack. Note: separate from mem
  Stk ws;
  Stk cs;
  Stk lsSz;

  U1 szI; // global instr sz
} Env;


#endif // __FNGI_TY_H
