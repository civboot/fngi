#ifndef __FNGI_H
#define __FNGI_H

#include "civ/civ_unix.h"
#include "./gen/const.h"
#include "./gen/spor.h"

#define WS_DEPTH    16
#define RS_DEPTH    128
#define TOKEN_SIZE  128
#define DICT_DEPTH  10

// To use: function must accept a "Kernel* k" pointer
#define cfb             (k->fb)

#define WS              (&cfb->ws)
#define RS              (&cfb->rs)

#define WS_POP()          Stk_pop(WS)
#define WS_POP2(A, B)     STK_POP2(WS, A, B)
#define WS_ADD(V)         Stk_add(WS, V)
#define WS_ADD2(A, B)     STK_PUSH2(WS, A, B)
#define WS_ADD3(A, B, C)  STK_PUSH3(WS, A, B, C)
#define RS_PUSH(V)        Stk_add(RS, V)

#define TASSERT_WS(E)     TASSERT_STK(E, WS)

#define FNGI_TEST(NAME, numBlocks)            \
  TEST_UNIX(NAME, numBlocks)                  \
    Globals g = {0};                          \
    FnFiber fnFb = {0};                       \
    Fiber_init((Fiber*)&fnFb, &localErrJmp);  \
    assert(FnFiber_init(&fnFb, &g));          \
    Civ_init((Fiber*)&fnFb);                  \
    Kern _k = {0}; Kern* k = &_k;             \
    Kern_init(k, &fnFb);

// ################################
// # Types

// Note: these are not cleanly laid out in memory.
typedef struct { U1 meta; CStr name; Slot node; } TyI;
typedef struct { U1 len; TyI dat[]; } TyDat;

typedef struct { CStr path; } FileInfo;

// A Ty can be a const, function, variable or dict. See META_TY_MASK in
// const.zty.
typedef struct _Ty {
  Bst          bst;  // symbol name and dict search.
  struct _Ty*  parent;
  U2           meta; // specifies node type
  U2           line; // src code line of definition.
  FileInfo*    info; // source file
  TyDat*       ty;   // type information (fields, etc)
  Slot         v;    // either a value or pointer (depends on node type/etc)
} Ty;

// Dict type nodes have an extra pointer which points to the TyDict that has
// them as a 'v' (dict parent). This is only used when finding the full name
// of a node for debugging.
typedef struct { Ty d; Ty* tyParent; } TyDict;

typedef struct {
  U2 glen; U2 gcap; // global data used and cap
  U2 metaNext; // meta of next fn
  U2 cstate;
  U1 fnState;
  U1 localOffset;
  U1 logLvlSys;
  U1 logLvlUsr;
  Ty* curTy;     // current type (fn, struct) being compiled
  void* compFn;  // current function that does compilation
  BBA* bbaCode; // where code is stored
  BBA* bbaDict; // where dictionary/etc is stored
  Ty* dictBuf[DICT_DEPTH];
  Stk dictStk;
  File src; FileInfo* srcInfo; U2 line;
  Buf tokenBuf;
} Globals;

typedef struct {
  Fiber fb;
  U1* ep;             // execution pointer
  Stk ws; Stk rs;     // working and return stack
  Stk info;           // info stack
  Globals* g;         // globals base pointer
} FnFiber;

typedef struct {
  U4 _null;
  BBA bbaCode;
  BBA bbaDict;
  Globals g;     // kernel globals
  Ty*   dict;    // kernel dictionary (root)
  FnFiber* fb;   // current fiber.
} Kern;

// ################################
// # Init
void Kern_init(Kern* k, FnFiber* fb);

// Initialze FnFiber (beyond Fiber init).
bool FnFiber_init(FnFiber* fb, Globals* g);

#endif // __FNGI_H
