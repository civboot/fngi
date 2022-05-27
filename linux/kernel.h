#ifndef __FNGI_TY_H
#define __FNGI_TY_H
#include <stdint.h>
#include "constants.h"

#define RSIZE       4
#define SZR         SZ4
#define WS_DEPTH    16
#define CS_DEPTH    128
#define TOKEN_SIZE  128

#define BLOCK_END  0xFF
#define BLOCK_PO2  12
#define BLOCK_SIZE (1<<BLOCK_PO2)

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

typedef U1 Instr;

typedef struct { Ref ref; U2 len; }                         Slc;
typedef struct { Ref ref; U2 len; U2 cap; }                 Buf;
typedef struct { Ref ref; U2 len; U2 size; U1 group; }      TokenState;
typedef struct { Ref ref; U2 sp; U2 cap; }                  Stk;
typedef struct { U1 previ; U1 nexti; }                      BANode;
typedef struct { Ref nodes; Ref blocks; U1 rooti; U1 cap; } BA;
typedef struct { Ref ba; U1 rooti; U2 len; U2 cap; }        BBA;
typedef struct { Ref root; Ref free; }                      Dict;
typedef struct { Ref l; Ref r; Ref ckey; U1 m0; U1 m1; U4 v; } DNode;

typedef struct {
  U1 valTy;     U1 _align;  U2 _align2;
  U2 valueASz;
  U2 valueBSz;
  U4 valueA;
  U4 valueB;
  Ref msg;
} ErrData;

typedef struct {
  Ref memTop;
  BA ba;
  BBA bba;
  BBA bbaTmp;
} Kern;

typedef struct {
  U2 _null;
  U2 err;
  Stk ws;   // working stack
  Stk ls;   // locals stack
  Stk cs;   // call stack
  Stk csSz; // call stack size bytes
  Buf gbuf;   // global data buffer (for tracking growing globals)
  Ref modBBA; // current module's bba
  Ref tmpBBA; // current temporary bba
  TokenState t;
  U1 buf0[TOKEN_SIZE];
} Globals;

typedef struct { Ref ep; } VM;

#endif // __FNGI_TY_H
