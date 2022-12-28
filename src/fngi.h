#ifndef __FNGI_H
#define __FNGI_H

#include "civ/civ_unix.h"
#include "./gen/const.h"
#include "./gen/spor.h"

#define SZR         SZ4

#define WS_DEPTH    16
#define RS_DEPTH    128
#define TOKEN_SIZE  128
#define DICT_DEPTH  10
#define FN_ALLOC    256

#define SLIT_MAX    0x2F

// To use: function must accept a "Kernel* k" pointer
#define cfb             (k->fb)

#define WS              (&cfb->ws)
#define RS              (&cfb->rs)

#define WS_POP()          Stk_pop(WS)
#define WS_POP2(A, B)     Stk_pop2(WS, A, B)
#define WS_ADD(V)         Stk_add(WS, V)
#define WS_ADD2(A, B)     Stk_add2(WS, A, B)
#define WS_ADD3(A, B, C)  Stk_add3(WS, A, B, C)
#define RS_ADD(V)         Stk_add(RS, V)
#define RS_POP()          Stk_pop(RS)
#define INFO_ADD(V)       Stk_add(&cfb->info, V)
#define INFO_POP(V)       Stk_pop(&cfb->info)

#define TASSERT_WS(E)     TASSERT_STK(E, WS)
#define TASSERT_EMPTY()   TASSERT_EQ(0, Stk_len(WS))

#define TEST_FNGI(NAME, numBlocks)            \
  TEST_UNIX(NAME, numBlocks)                  \
    FnFiber fnFb = {0};                       \
    Fiber_init((Fiber*)&fnFb, &localErrJmp);  \
    assert(FnFiber_init(&fnFb));              \
    civ.fb = (Fiber*)&fnFb;                   \
    Kern _k = {0}; Kern* k = &_k;             \
    Kern_init(k, &fnFb);

#define END_TEST_FNGI   END_TEST_UNIX

// ################################
// # From gen/name.c
extern U1* unknownInstr;
Slc instrName(U1 instr);
U1* szName(U1 szI);

// ################################
// # Types

typedef struct { CStr* path; U2 line; } FileInfo;
typedef struct { U1 inpLen; U1 outLen; U1 _packedTyI[]; } TyDat;

// A Ty can be a const, function, variable or dict depending on meta. See
// TY_MASK in const.zty.
#define TY_BODY \
  Bst          bst;  /* symbol name and dict search. */ \
  struct _Ty*  parent;  \
  U2           meta; /* specifies node type */ \
  U2           line; /* src code line of definition. */ \
  FileInfo*    file; /* source file */ \
  S            v;    /* either a value or pointer (depends on node type/etc) */

typedef struct _Ty { TY_BODY } Ty;

typedef struct _TyI {
  struct _TyI*   next;
  U1    meta;
  CStr* name;
  Ty*   ty;
} TyI;

typedef struct { TY_BODY; TyI* tyI; } TyVar;

typedef struct {
  TY_BODY
  TyI* inp;
  TyI* out;
  U2 len; // size of spor binary
  U1 lSlots;
} TyFn;

#define TyFn_native(CNAME, META, NFN, INP, OUT) {       \
  .bst.key = (CStr*) ( CNAME ),             \
  .meta = TY_FN | TY_FN_NATIVE | (META),    \
  .v = NFN, \
  .inp = INP, .out = OUT \
}


#define TyFn_static(NAME, META, LSLOTS, DAT) \
  static TyFn NAME;               \
  NAME = (TyFn) {                 \
    .meta = TY_FN | META,       \
    .v = DAT,                   \
    .lSlots = LSLOTS,             \
  };

static inline Sll** TyFn_inpRoot(TyFn* this) { return (Sll**)&this->inp; }
static inline Sll** TyFn_outRoot(TyFn* this) { return (Sll**)&this->out; }
static inline Sll*  TyI_asSll(TyI* this)     { return (Sll*)this; }

static inline TyFn litFn(U1* dat, U2 meta, U2 lSlots) {
  return (TyFn) {
    .meta = TY_FN | meta,
    .v = (S)dat,
    .lSlots = lSlots,
  };
}

typedef struct {
  TY_BODY;
  TyI* tyI; // fields, variants, etc
  U2 sz;
  Ty* tyParent;
} TyDict;

#define TYDB_DEPTH 16
typedef struct {
  Stk tyIs;
  Stk done;
  S   tyIsDat[TYDB_DEPTH];
  S   doneDat[TYDB_DEPTH];
} TyDb;

typedef struct {
  U2 glen; U2 gcap; // global data used and cap
  U2 metaNext; // meta of next fn
  U2 cstate;
  U2 fnLocals; // locals size
  U1 fnState;
  U1 logLvlSys;  U1 logLvlUsr;
  Ty* curTy;    // current type (fn, struct) being compiled
  TyFn* compFn; // current function that does compilation
  S dictBuf[DICT_DEPTH];
  Stk dictStk;
  Reader src; FileInfo* srcInfo;
  Buf token; U1 tokenDat[64]; U2 tokenLine;
  Buf code;
  TyDb tyDb;
  BBA* bbaDict;
  BBA* bbaTy;
} Globals;

typedef struct {
  Fiber fb;
  U1* ep;             // execution pointer
  Stk ws; Stk rs;     // working and return stack
  Stk info;           // info stack
} FnFiber;

typedef struct {
  U4 _null;
  BBA bbaCode;
  BBA bbaDict;
  BBA bbaRepl;
  Globals g;     // kernel globals
  Ty*   dict;    // kernel dictionary (root)
  FnFiber* fb;   // current fiber.
} Kern;

// ################################
// # Init
void Kern_init(Kern* k, FnFiber* fb);

// Initialze FnFiber (beyond Fiber init).
bool FnFiber_init(FnFiber* fb);

static inline S kFn(void(*native)(Kern*)) { return (S) native; }

#define LOCAL_BBA(NAME) \
  BBA  NAME       = (BBA) { &civ.ba }; \
  BBA* prev##NAME = k->g.NAME;   \
  k->g.NAME       = &NAME;

#define END_LOCAL_BBA(NAME) \
  BBA_drop(&NAME); \
  k->g.NAME = prev##NAME;

#define REPL_START \
  TyDb_new(&k->g.tyDb, NULL); LOCAL_BBA(bbaTy);

#define REPL_END \
  TyDb_drop(k); END_LOCAL_BBA(bbaTy);



// ################################
// # Execute

void executeFn(Kern* k, TyFn* fn);

// #################################
// # Scan
// scan fills g.token with a token. If one already exists it is a noop.
// scan does NOT affect g.src's ring buffer, except to increment tail with
// characters.
// When the token is used, tokenDrop should be called. This will update
// src's ring buffer as well so the next token can be scanned.
void scan(Kern* k);
void tokenDrop(Kern* k); // clear token and update src.ring
U1 cToU1(U1 c); // return 0-15 or 0xFF if not a hex integer.
typedef struct { bool isNum; U4 v; } ParsedNumber;
ParsedNumber parseU4(Slc t);

// #################################
// # Compiler
#define IS_TY(M)   { return (M) == (TY_MASK & ty->meta); }
static inline bool isTyConst(Ty* ty)  IS_TY(TY_CONST)
static inline bool isTyFn(Ty* ty)     IS_TY(TY_FN)
static inline bool isTyVar(Ty* ty)    IS_TY(TY_VAR)
static inline bool isTyDict(Ty* ty)   IS_TY(TY_DICT)
#undef IS_TY
#define IS_FN(M)   { return (M) & fn->meta; }
static inline bool isFnPre(TyFn* fn)       IS_FN(TY_FN_PRE)
static inline bool isFnNative(TyFn* fn)    IS_FN(TY_FN_NATIVE)
#undef IS_FN
#define IS_FN(M)   { return (TY_FN_TY_MASK & fn->meta) == (M); }
static inline bool isFnNormal(TyFn* fn)    IS_FN(TY_FN_NORMAL)
static inline bool isFnNow(TyFn* fn)       IS_FN(TY_FN_NOW)
static inline bool isFnSyn(TyFn* fn)       IS_FN(TY_FN_SYN)
static inline bool isFnInline(TyFn* fn)    IS_FN(TY_FN_INLINE)
static inline bool isFnComment(TyFn* fn)   IS_FN(TY_FN_COMMENT)
#undef IS_FN
#define IS_DICT(M)   { return (M) == (TY_DICT_MSK & ty->meta); }
static inline bool isDictNative(TyDict* ty)    IS_DICT(TY_DICT_NATIVE)
static inline bool isDictStruct(TyDict* ty)    IS_DICT(TY_DICT_STRUCT)
#undef IS_DICT
static inline bool isVarGlobal(TyVar* v) { return TY_VAR_GLOBAL & v->meta; }
static inline U1   TyI_refs(TyI* tyI) { return TY_REFS & tyI->meta; }

static inline TyFn* tyFn(void* p) {
  ASSERT(isTyFn((Ty*)p), "invalid TyFn");
  return (TyFn*)p;
}

static inline TyDict* tyDict(Ty* ty) {
  ASSERT(isTyDict(ty), "invalid TyDict");
  ASSERT(not isDictNative((TyDict*)ty), "native dict");
  return (TyDict*) ty;
}

Ty* Kern_findTy(Kern* k, Slc t);
void Kern_addTy(Kern* k, Ty* ty);

void Kern_fns(Kern* k);
void single(Kern* k, bool asNow);
void compileSrc(Kern* k);

// Compile a stream to bbaRepl, returning the start.
//
// Any functions/etc will still be compiled to the normal locations.
U1*  compileRepl(Kern* k, bool withRet);

// #################################
// # Misc

#define WS_SLC()          Slc_fromWs(k)
static inline Slc Slc_fromWs(Kern* k) {
  U1* dat = (U1*)WS_POP();
  return (Slc){dat, .len=WS_POP()};
}


// #################################
// # Test Helpers

#define _COMPILE_VARS(CODE) \
  BufFile_var(LINED(bf), 64, CODE); \
  Reader LINED(f) = File_asReader(BufFile_asFile(&LINED(bf))); \
  k->g.src = LINED(f);

#define COMPILE_NAMED(NAME, CODE, withRet) \
  _COMPILE_VARS(CODE); U1* NAME = compileRepl(k, withRet)
#define COMPILE(CODE, withRet) \
  _COMPILE_VARS(CODE); compileRepl(k, withRet)
#define COMPILE_EXEC(CODE)    \
  COMPILE_NAMED(LINED(code), CODE, true); executeInstrs(k, LINED(code));

void executeInstrs(Kern* k, U1* instrs);

void simpleRepl(Kern* k);


// Get the current snapshot
static inline TyI* TyDb_index(TyDb* db, U2 i) {
  ASSERT(i < Stk_len(&db->tyIs), "TyDb OOB index");
  return (TyI*) db->tyIs.dat[db->tyIs.sp + i];
}

static inline TyI* TyDb_top(TyDb* db) { return (TyI*) Stk_top(&db->tyIs); }

// Get a reference to the current snapshot
static inline TyI** TyDb_root(TyDb* db) { return (TyI**) Stk_topRef(&db->tyIs); }

// Get/set whether current snapshot is done (guaranteed ret)
static inline bool TyDb_done(Kern* k) { return Stk_top(&k->g.tyDb.done); }
static inline void TyDb_setDone(TyDb* db, bool done) { *Stk_topRef(&db->done) = done; }

// Free from the current snapshot.
//
// "stream" can be either TyDb_top (freeing the entire snapshot), or a
// separate type stream which indicates the length of items to drop.
void TyDb_free(Kern* k, TyI* stream);

// Drop the current snapshot
void TyDb_drop(Kern* k);

// Create a new snapshot
static inline void TyDb_new(TyDb* db, TyI* tyI) {
  Stk_add(&db->tyIs, (S)tyI);
  Stk_add(&db->done, false);
}

void tyCheck(TyI* require, TyI* given, bool sameLen, Slc errCxt);
void tyCall(Kern* k, TyI* inp, TyI* out);
void tyRet(Kern* k, bool done);
void tySplit(Kern* k);
void tyMerge(Kern* k);
void TyI_printAll(TyI* tyI);


#define TYI_VOID  NULL
#define TYIS(PRE) \
  PRE Ty  Ty_U1;     \
  PRE Ty  Ty_U2;     \
  PRE Ty  Ty_U4;     \
  PRE Ty  Ty_S;      \
  PRE Ty  Ty_I1;     \
  PRE Ty  Ty_I2;     \
  PRE Ty  Ty_I4;     \
  PRE Ty  Ty_SI;     \
  PRE TyI TyIs_S;      /* S          */ \
  PRE TyI TyIs_SS;     /* S, S       */ \
  PRE TyI TyIs_SSS;    /* S, S, S    */ \
  PRE TyI TyIs_U1;     /* U1         */ \
  PRE TyI TyIs_U2;     /* U2         */ \
  PRE TyI TyIs_U4;     /* U4         */ \
  PRE TyI TyIs_U4x2;   /* U4 U4      */ \
  PRE TyI TyIs_rU1;    /* &U1        */ \
  PRE TyI TyIs_rU2;    /* &U2        */ \
  PRE TyI TyIs_rU4;    /* &U4        */ \
  PRE TyI TyIs_U1_rU1; /* &U1, U1    */ \
  PRE TyI TyIs_U2_rU2; /* &U2, U2    */ \
  PRE TyI TyIs_U4_rU4; /* &U4, U4    */

TYIS(extern)

#endif // __FNGI_H
