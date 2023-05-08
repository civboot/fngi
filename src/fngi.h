#ifndef __FNGI_H
#define __FNGI_H

#include "civ_unix.h"
#include "const.h" // from gen/
#include "spor.h"  // from gen/

#define FNGI_VERSION "0.1.0"

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
#define WS_POP3(A, B, C)  Stk_pop3(WS, A, B, C)
#define WS_ADD(V)         Stk_add(WS, V)
#define WS_ADD2(A, B)     Stk_add2(WS, A, B)
#define WS_ADD3(A, B, C)  Stk_add3(WS, A, B, C)
#define RS_ADD(V)         Stk_add(RS, V)
#define RS_POP()          Stk_pop(RS)
#define INFO_ADD(V)       Stk_add(&cfb->info, V)
#define INFO_POP(V)       Stk_pop(&cfb->info)

#define TASSERT_WS(E)     TASSERT_STK(E, WS)
#define TASSERT_EMPTY()   TASSERT_EQ(0, Stk_len(WS))

#define Ty_fmt(TY)    CStr_fmt((TY)->name)

#define TEST_FNGI(NAME, numBlocks)            \
  TEST_UNIX(NAME, numBlocks)                  \
    civ.errPrinter = &fngiErrPrinter;         \
    FnFiber fnFb = {0};                       \
    Fiber_init((Fiber*)&fnFb, &localErrJmp);  \
    assert(FnFiber_init(&fnFb));              \
    civ.fb = (Fiber*)&fnFb;                   \
    Kern _k = {0}; Kern* k = &_k; fngiK = k;  \
    Kern_init(k, &fnFb); _k.isTest = true;

#define END_TEST_FNGI   END_TEST_UNIX

// ################################
// # From gen/name.c
extern U1* unknownInstr;
Slc instrName(U1 instr);
U1* szName(U1 szI);

// ################################
// # Fngi Roles
struct _Kern;
struct _TyFn;

// Sp_XrN: Execute Spor Role with N arguments
#define Sp_Xr0(ROLE, METHOD) \
  do { WS_ADD((S) (ROLE).d);                      executeFn(k, (ROLE).m->METHOD); } \
  while (0)

#define Sp_Xr1(ROLE, METHOD, A0) \
  do { WS_ADD((S) (ROLE).d); WS_ADD(A0);          executeFn(k, (ROLE).m->METHOD); } \
  while (0)

#define Sp_Xr2(ROLE, METHOD, A0, A1) \
  do { WS_ADD((S) (ROLE).d); WS_ADD2(A0, A1);     executeFn(k, (ROLE).m->METHOD); } \
  while (0)

#define Sp_Xr3(ROLE, METHOD, A0, A1, A2) \
  do { WS_ADD((S) (ROLE).d); WS_ADD3(A0, A1, A2); executeFn(k, (ROLE).m->METHOD); } \
  while (0)

// Spore version of Arena
typedef struct {
  struct _TyFn*  drop;     // this:&This -> ()
  struct _TyFn*  alloc;    // this:&This sz:S alignment:U2 -> Ref
  struct _TyFn*  free;     // this:&This dat:Ref sz:S alignment:U2 -> ()
  struct _TyFn*  maxAlloc; // this:&This -> S
} MSpArena;
typedef struct { MSpArena* m; void* d; } SpArena;

extern MSpArena mSpArena_BBA;

static inline SpArena BBA_asSpArena(BBA* bba) {
  return (SpArena) { .m = &mSpArena_BBA, .d = bba };
}

// Spore version of Reader
typedef struct {
  struct _TyFn*  read;   // this:&This -> ()
  struct _TyFn*  asBase; // this:&This -> &BaseFile
} MSpReader;
typedef struct { MSpReader* m; void* d; } SpReader;

extern MSpReader mSpReader_UFile;
extern MSpReader mSpReader_BufFile;
BaseFile* SpReader_asBase(struct _Kern* k, SpReader r);
U1*       SpReader_get(struct _Kern* k, SpReader r, U2 i);

// ################################
// # Types

typedef struct { CStr* path; U2 line; } FileInfo;
typedef struct { U1 inpLen; U1 outLen; U1 _packedTyI[]; } TyDat;

// A Ty can be a const, function, variable or dict depending on meta. See
// TY_MASK in const.zty.
struct _TyI;
#define TY_BASE \
  void* l; void* r;       \
  U2           meta; /* specifies node type */
typedef struct { TY_BASE } TyBase;

#define TY_BODY \
  TY_BASE \
  U2           line; /* src code line of definition. */ \
  CStr* name; struct _TyI* tyKey; \
  struct _Ty*  parent;  \
  FileInfo*    file; /* source file */
typedef struct _Ty { TY_BODY; } Ty;

typedef struct _TyI {
  struct _TyI*   next;
  U2 meta;  U2 arrLen;
  CStr*          name;
  TyBase*        ty;
} TyI;

typedef struct _TyIBst {
   struct _TyIBst* l; struct _TyIBst* r;
   TyI tyI;
} TyIBst;

typedef struct { Slc name; TyI* tyI; } Key;
#define KEY(NAME)  (Key){ .name = SLC(NAME) }

typedef struct { TY_BODY; S v; TyI* tyI; } TyVar;

typedef struct { TyI* inp; TyI* out;   } InOut;
typedef struct { TY_BASE; InOut io;    } FnSig;

typedef struct _TyFn {
  TY_BODY
  Ty* locals;
  U1* code;
  TyI* inp;
  TyI* out;
  U2 len; // size of spor binary
  U1 lSlots;
} TyFn;

static inline InOut* TyFn_asInOut(TyFn* fn) { return (InOut*) &fn->inp; }

#define TyFn_native(CNAME, META, NFN, INP, OUT) {       \
  .name = (CStr*) ( CNAME ),                \
  .meta = TY_FN | TY_FN_NATIVE | (META),    \
  .code = NFN, \
  .inp = INP, .out = OUT \
}

#define TyFn_static(NAME, META, LSLOTS, DAT) \
  static TyFn NAME;               \
  NAME = (TyFn) {                 \
    .meta = TY_FN | META,       \
    .code = DAT,                   \
    .lSlots = LSLOTS,             \
  };

static inline Sll*  TyI_asSll(TyI* this)     { return (Sll*)this; }

static inline TyFn litFn(U1* code, U2 meta, U2 lSlots) {
  return (TyFn) {
    .meta = TY_FN | meta,
    .code = code,
    .lSlots = lSlots,
  };
}

typedef struct {
  TY_BODY;
  Ty* children;
  TyI* fields;
  U2 sz;
} TyDict;

static inline Sll** TyDict_fieldsRoot(TyDict* ty) { return (Sll**) &ty->fields; }

typedef struct _Ownership {
  struct _Ownership* next;
  U2 meta; U2 offset;
  S len;
} Ownership;
typedef struct { void* ref; TyDict* ty; Ownership* ownership; } OwnedValue;

#define TYDB_DEPTH 16
typedef struct {
  BBA* bba;
  Stk tyIs; // stack of TyI, aka blocks
  Stk done; // indication of whether block is ret/cont/break/etc
  S   tyIsDat[TYDB_DEPTH];
  S   doneDat[TYDB_DEPTH];
} TyDb;

// Flow Block (loop/while/etc)
typedef struct _Blk {
  struct _Blk* next;
  S start;
  Sll* breaks;  // store breaks to update at end
  TyI* startTyI; // The type at start of block
  TyI* endTyI;   // The type at end of block (including break)
} Blk;
static inline Sll*  Blk_asSll(Blk* this)     { return (Sll*)this; }

typedef struct { TyDict** dat;   U2 sp;   U2 cap;           } DictStk;

typedef struct _SllSpArena
{ struct _SllSpArena* next;  SpArena arena; } SllSpArena;

static inline Sll* SllSpArena_asSll(SllSpArena* this) { return (Sll*)this; }

typedef struct {
  U2 glen; U2 gcap; // global data used and cap
  U2 metaNext; // meta of next fn
  U2 cstate;
  U2 fnLocals; // locals size
  U1 fnState;
  U1 logLvlSys;  U1 logLvlUsr;
  TyDict* curMod; // current parent module (mod, struct, etc)
  Ty* curTy;      // current type (fn, struct) being compiled
  TyFn* compFn;   // current function that does compilation
  TyDict rootDict;
  TyDict* dictBuf[DICT_DEPTH];
  DictStk dictStk;    // Type is: &&Ty (double ref to dictionary)
  SpReader src;
  // Reader src;
  FileInfo* srcInfo;
  Buf token; U1 tokenDat[64]; U2 tokenLine;
  Buf code;
  CBst* cBst; TyIBst* tyIBst; FnSig* fnSigBst;
  TyDb tyDb; TyDb tyDbImm; BBA bbaTyImm;
  BBA* bbaDict;
  Blk* blk;
} Globals;

typedef struct {
  Fiber fb;
  SllSpArena* sllArena;
  U1* ep;             // execution pointer
  Stk ws; Stk rs;     // working and return stack
  Stk info;           // info stack
} FnFiber;

typedef struct _Kern {
  U4 _null;
  bool isTest;
  BBA bbaCode;
  BBA bbaDict;
  BBA bbaRepl;
  BBA bbaSllArena;
  SllSpArena sllArena;
  Globals g;     // kernel globals
  FnFiber* fb;   // current fiber.
} Kern;

extern Kern* fngiK;


// ################################
// # Kernel
void dbgWs(Kern *k);
static inline S RS_topRef(Kern* k) { Stk* rs = RS; return (S)&rs->dat[rs->sp]; }
void dbgRs(Kern* k);
void Kern_handleSig(Kern* k, int sig, struct sigcontext* ctx);
void fngiErrPrinter();
void fngiHandleSig(int sig, struct sigcontext ctx);

void DictStk_reset(Kern* k);
void Kern_init(Kern* k, FnFiber* fb);

// Initialze FnFiber (beyond Fiber init).
bool FnFiber_init(FnFiber* fb);

static inline U1* kFn(void(*native)(Kern*)) { return (U1*) native; }

#define ARENA_TOP (&k->fb->sllArena->arena)

#define LOCAL_TYDB_BBA(NAME) \
  BBA  bba_##NAME       = (BBA) { &civ.ba }; \
  BBA* prevBba_##NAME   = k->g.NAME.bba;   \
  k->g.NAME.bba     = &bba_##NAME;

#define END_LOCAL_TYDB_BBA(NAME) \
  BBA_drop(&bba_##NAME); \
  k->g.NAME.bba     = prevBba_##NAME;

#define REPL_START \
  TyDb_new(&k->g.tyDb); LOCAL_TYDB_BBA(tyDb);

#define REPL_END \
  TyDb_drop(k, &k->g.tyDb); END_LOCAL_TYDB_BBA(tyDb); \
  DictStk_reset(k);



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
ParsedNumber parseU4(Kern* k, Slc t);

// #################################
// # Compiler
#define IS_TY(M)   return ty && ((M) == (TY_MASK & ty->meta))
static inline bool isTyVar(Ty* ty)         { IS_TY(TY_VAR); }
static inline bool isTyDict(Ty* ty)        { IS_TY(TY_DICT); }
static inline bool isTyDictB(TyBase* ty)   { IS_TY(TY_DICT); }
static inline bool isTyFn(Ty* ty) {
  if((TY_FN || TY_FN_SIG) == ty->meta) return false; else IS_TY(TY_FN); }
static inline bool isTyFnB(TyBase* ty) { return isTyFn((Ty*)ty); }
#undef IS_TY
#define IS_FN(M)   { return (M) & fn->meta; }
static inline bool isFnNative(TyFn* fn)    IS_FN(TY_FN_NATIVE)
#undef IS_FN
#define IS_FN(M)   { return (TY_FN_TY_MASK & fn->meta) == (M); }
static inline bool isFnNormal(TyFn* fn)    IS_FN(TY_FN_NORMAL)
static inline bool isFnImm(TyFn* fn)       IS_FN(TY_FN_IMM)
static inline bool isFnSyn(TyFn* fn)       IS_FN(TY_FN_SYN)
static inline bool isFnSynty(TyFn* fn)     IS_FN(TY_FN_SYNTY)
static inline bool isFnInline(TyFn* fn)    IS_FN(TY_FN_INLINE)
static inline bool isFnComment(TyFn* fn)   IS_FN(TY_FN_COMMENT)
static inline bool isFnMethod(TyFn* fn)    IS_FN(TY_FN_METH)
static inline bool isFnAbsmeth(TyFn* fn)   IS_FN(TY_FN_ABSMETH)
#undef IS_FN
#define FN_SIG (TY_FN || TY_FN_SIG)
static inline bool isFnSig(TyBase* ty) { return FN_SIG == ty->meta; }
#define IS_DICT(M)   { return (M) == (TY_DICT_MSK & ty->meta); }
static inline bool isDictNative(TyDict* ty)    IS_DICT(TY_DICT_NATIVE)
static inline bool isDictMod(TyDict* ty)       IS_DICT(TY_DICT_MOD)
static inline bool isDictStruct(TyDict* ty)    IS_DICT(TY_DICT_STRUCT)
static inline bool isDictRole(TyDict* ty)      IS_DICT(TY_DICT_ROLE)
#undef IS_DICT
static inline bool isVarAlias(TyVar* v)  { return TY_VAR_ALIAS  & v->meta; }
static inline bool isVarGlobal(TyVar* v) { return TY_VAR_GLOBAL & v->meta; }
static inline U1   TyI_refs(TyI* tyI)    { return TY_REFS & tyI->meta; }

static inline TyFn* tyFn(void* p) {
  ASSERT(isTyFn((Ty*)p), "invalid TyFn");
  return (TyFn*)p;
}

static inline TyDict* tyDict(Ty* ty) {
  ASSERT(isTyDict(ty), "invalid TyDict");
  ASSERT(not isDictNative((TyDict*)ty), "native dict");
  return (TyDict*) ty;
}

static inline TyDict* tyDictB(TyBase* ty) { return tyDict((Ty*)ty); }

static inline TyVar* tyVar(Ty* ty) {
  ASSERT(isTyVar(ty), "expected TyVar");
  return (TyVar*) ty;
}

Ty* Kern_findTy(Kern* k, Key* key);
void Kern_addTy(Kern* k, Ty* ty);

void Kern_fns(Kern* k);
void Dat_mod(Kern* k);
void single(Kern* k, bool asImm);
void compileSrc(Kern* k);

// Compile a stream to bbaRepl, returning the start.
//
// Any functions/etc will still be compiled to the normal locations.
U1*  compileRepl(Kern* k, bool withRet);
void compilePath(Kern* k, CStr* path);

// #################################
// # Misc

#define WS_SLC()          Slc_fromWs(k)
static inline Slc Slc_fromWs(Kern* k) {
  U1* dat = (U1*)WS_POP();
  return (Slc){dat, .len=WS_POP()};
}

static inline S ftSzI(U1* addr, U1 szI) {
  S out;
  switch(szI) {
    case SZ1: out = *(U1*)addr; break;
    case SZ2: out = *(U2*)addr; break;
    case SZ4: out = *(U4*)addr; break;
    default: assert(false);
  }
  eprintf("??? ftSzI from %X: %X\n", addr, out);
  return out;
}

static inline void srSzI(U1* addr, U1 szI, S v) {
  eprintf("??? srSzI addr=%X szI=%X v=%X\n", addr, szI, v);
  switch(szI) {
    case SZ1: *(U1*)addr = v; return;
    case SZ2: *(U2*)addr = v; return;
    case SZ4: *(U4*)addr = v; return;
  }
  assert(false);
}

// #################################
// # Test Helpers

#define _COMPILE_VARS(CODE) \
  BufFile_var(LINED(bf), 64, CODE); \
  k->g.src = (SpReader) {.m = &mSpReader_BufFile, .d = &LINED(bf) };

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

// Get a reference to the current snapshot
static inline Sll** TyDb_rootSll(TyDb* db) { return (Sll**) Stk_topRef(&db->tyIs); }

// Get/set whether current snapshot is done (guaranteed ret)
static inline bool TyDb_done(TyDb* db) { return Stk_top(&db->done); }
static inline void TyDb_setDone(TyDb* db, bool done) { *Stk_topRef(&db->done) = done; }

// Free from the current snapshot.
//
// "stream" can be either TyDb_top (freeing the entire snapshot), or a
// separate type stream which indicates the length of items to drop.
void TyDb_free(Kern* k, TyDb* db, TyI* stream);

// Drop the current snapshot
void TyDb_drop(Kern* k, TyDb* db);

// Create a new snapshot
static inline void TyDb_new(TyDb* db) {
  Stk_add(&db->tyIs, 0);
  Stk_add(&db->done, false);
}

static inline TyDb* tyDb(Kern* k, bool asImm) { return asImm ? &k->g.tyDbImm : &k->g.tyDb; }
void tyCheck(TyI* require, TyI* given, bool sameLen, Slc errCxt);
void tyCall(Kern* k, TyDb* db, TyI* inp, TyI* out);
void tyRet(Kern* k, TyDb* db, bool done);
void tySplit(Kern* k);
void tyMerge(Kern* k, TyDb* db);
void TyI_printAll(TyI* tyI);
Ty* TyDict_find(TyDict* dict, Key* s);


#define TYI_VOID  NULL

#define TYIS(PRE) \
  PRE TyDict  Ty_UNSET;    \
  PRE TyDict  Ty_Any;      \
  PRE TyDict  Ty_Unsafe;   \
  PRE TyI TyIs_UNSET;  \
  PRE TyI TyIs_Unsafe; \
  PRE TyI TyIs_rAny;   \
  PRE TyI TyIs_rAnyS;  \
  PRE TyI TyIs_rAnySS; \
  PRE TyDict  Ty_U1;     \
  PRE TyDict  Ty_U2;     \
  PRE TyDict  Ty_U4;     \
  PRE TyDict  Ty_S;      \
  PRE TyDict  Ty_I1;     \
  PRE TyDict  Ty_I2;     \
  PRE TyDict  Ty_I4;     \
  PRE TyDict  Ty_SI;     \
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
  PRE TyI TyIs_rU1_U4; /* &U1, U4    */ \

TYIS(extern)

void N_assertWsEmpty(Kern* k);

#endif // __FNGI_H
