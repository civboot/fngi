#ifndef __FNGI_H
#define __FNGI_H

#include "civ_unix.h"
#include "comp.h"  // from gen/
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
#define TASSERT_EMPTY()   do { \
  if(Stk_len(WS)) { \
    eprintf("!!! Not empty: "); dbgWs(k); eprintf("\n"); \
    TASSERT_EQ(0, Stk_len(WS)) \
  } } while(0)



#define FNGI_EXPECT_ERR(CODE, MSG)             \
  civ.fb->state |= Fiber_EXPECT_ERR;      \
  HANDLE_ERR(                             \
    { CODE;                               \
      eprintf("!!! expected error never happend\n"); assert(false); } \
    , { handleExpectedErr(SLC(MSG)); Kern_errCleanup(k); })

#define Ty_fmt(TY)    CStr_fmt((TY)->name)
#define Instr_fmt(I)  Dat_fmt(instrName(I))

#define TEST_FNGI(NAME, numBlocks)            \
  TEST_UNIX(NAME, numBlocks)                  \
    civ.errPrinter = &fngiErrPrinter;         \
    FnFiber fnFb = {0};                       \
    Fiber_init((Fiber*)&fnFb, &localErrJmp);  \
    assert(FnFiber_init(&fnFb));              \
    civ.fb = (Fiber*)&fnFb;                   \
    Kern _k = {0}; Kern* k = &_k; fngiK = k;  \
    Kern_init(k, &fnFb); _k.isTest = true;

#define END_TEST_FNGI   TASSERT_EMPTY(); END_TEST_UNIX

// ################################
// # From gen/name.c
extern U1* unknownInstr;
Slc instrName(U1 instr);
U1* szName(U1 szI);

// ################################
// # Fngi Roles
struct _Kern;
// struct _TyFn;

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

extern MSpArena mSpArena_BBA;

static inline SpArena BBA_asSpArena(BBA* bba) {
  return (SpArena) { .m = &mSpArena_BBA, .d = bba };
}

// Spore version of Reader
extern MSpReader mSpReader_UFile;
extern MSpReader mSpReader_BufFile;
BaseFile* SpReader_asBase(struct _Kern* k, SpReader r);
U1*       SpReader_get(struct _Kern* k, SpReader r, U2 i);

// ################################
// # Types


// A Ty can be a const, function, variable or dict depending on meta. See
// TY_MASK in const.zty.

#define KEY(NAME)  (Key){ .name = SLC(NAME) }
#define TY_KEY(TY) (Key){ .name = Slc_frCStr((TY)->name), .tyI = (TY)->tyKey }

extern TyFn _TyFn_imm;
#define START_IMM(AS_IMM) \
    TyFn* cfn = k->g.c.compFn; if(AS_IMM) k->g.c.compFn = &_TyFn_imm

#define END_IMM   k->g.c.compFn = cfn

static inline InOut* TyFn_asInOut(TyFn* fn) { return (InOut*) &fn->inp; }

#define TyFn_native(CNAME, META, NFN, INP, OUT) {       \
  .name = (CStr*) ( CNAME ),                \
  .meta = TY_FN | TY_FN_NATIVE | (META),    \
  .code = NFN, \
  .inp = INP, .out = OUT \
}

#define TyFn_static(NAME, META, LSLOTS, DAT) \
  CStr_ntVar(LINED(name), "\x08", "<static>"); \
  static TyFn NAME;               \
  NAME = (TyFn) {                 \
    .name = LINED(name),        \
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

static inline Sll** TyDict_fieldsRoot(TyDict* ty) { return (Sll**) &ty->fields; }

typedef struct _Ownership {
  struct _Ownership* next;
  U2 meta; U2 offset;
  S len;
} Ownership;
typedef struct { void* ref; TyDict* ty; Ownership* ownership; } OwnedValue;

typedef enum { NOT_DONE, BLK_DONE, RET_DONE } HowDone;

#define TYDB_DEPTH 16
typedef struct { S tyIsDat[TYDB_DEPTH]; S doneDat[TYDB_DEPTH]; } TyDbDat;

// Flow Block (loop/while/etc)
static inline Sll*  Blk_asSll(Blk* this)     { return (Sll*)this; }

typedef struct _SllSpArena
{ struct _SllSpArena* next;  SpArena arena; } SllSpArena;

static inline Sll* SllSpArena_asSll(SllSpArena* this) { return (Sll*)this; }

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
  BBA bbaCode; BBA bbaDict;
  BBA bbaRepl; BBA bbaTmp;
  BBA bbaSllArena;
  U1 tokenDat[64];
  TyDict* dictBuf[DICT_DEPTH]; TyDict* implBuf[DICT_DEPTH];
  TyDbDat tyDbDat; TyDbDat tyDbImmDat;
  SllSpArena sllArena;
  Globals g;     // kernel globals
  FnFiber* fb;   // current fiber.
} Kern;

extern Kern* fngiK;
void Kern_errCleanup(Kern*);


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
// scan fills g.c.token with a token. If one already exists it is a noop.
// scan does NOT affect g.c.src's ring buffer, except to increment tail with
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
static inline bool isFnMeth(TyFn* fn)      IS_FN(TY_FN_METH)
static inline bool isFnAbsmeth(TyFn* fn)   IS_FN(TY_FN_ABSMETH)
#undef IS_FN
#define FN_SIG (TY_FN | TY_FN_SIG)
static inline bool isFnSig(TyBase* ty) { return FN_SIG == ty->meta; }
#define IS_DICT(M)   { return (M) == (TY_DICT_MSK & ty->meta); }
static inline bool isDictNative(TyDict* ty)    IS_DICT(TY_DICT_NATIVE)
static inline bool isDictMod(TyDict* ty)       IS_DICT(TY_DICT_MOD)
static inline bool isDictStruct(TyDict* ty)    IS_DICT(TY_DICT_STRUCT)
static inline bool isDictRole(TyDict* ty)      IS_DICT(TY_DICT_ROLE)
#undef IS_DICT
#define IS_VAR(M)    { return (M) == (TY_VAR_MSK & v->meta); }
static inline bool isVarAlias(TyVar* v)  IS_VAR(TY_VAR_ALIAS)
static inline bool isVarLocal(TyVar* v)  IS_VAR(TY_VAR_LOCAL)
static inline bool isVarGlobal(TyVar* v) IS_VAR(TY_VAR_GLOBAL)
static inline bool isVarConst(TyVar* v)  IS_VAR(TY_VAR_CONST)
static inline U1   TyI_refs(TyI* tyI)    { return TY_REFS & tyI->meta; }
#undef IS_VAR

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
void Core_mod(Kern* k);
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
  return out;
}

static inline void srSzI(U1* addr, U1 szI, S v) {
  switch(szI) {
    case SZ1: *(U1*)addr = v; return;
    case SZ2: *(U2*)addr = v; return;
    case SZ4: *(U4*)addr = v; return;
  }
  assert(false);
}

// #################################
// # Test Helpers

#define SET_SRC(CODE) \
  BufFile_var(LINED(bf), 64, CODE); \
  k->g.c.src = (SpReader) {.m = &mSpReader_BufFile, .d = &LINED(bf) };

#define COMPILE_NAMED(NAME, CODE, withRet) \
  SET_SRC(CODE); U1* NAME = compileRepl(k, withRet)
#define COMPILE(CODE, withRet) \
  SET_SRC(CODE); compileRepl(k, withRet)
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
static inline HowDone TyDb_done(TyDb* db) { return Stk_top(&db->done); }
void TyDb_setDone(TyDb* db, HowDone done);

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
void tyRet(Kern* k, TyDb* db, HowDone done);
void tySplit(Kern* k);
void tyMerge(Kern* k, TyDb* db);
void TyI_printAll(TyI* tyI);
Ty* TyDict_find(TyDict* dict, Key* s);
S TyDict_sz(TyDict* ty);


#define TYI_VOID  NULL

#define TYIS(PRE) \
  PRE TyDict  Ty_UNSET;    \
  PRE TyDict  Ty_Any;      \
  PRE TyDict  Ty_Unsafe;   \
  PRE TyDict  Ty_Self;     \
  PRE TyDict  Ty_RoleMeth; \
  PRE TyDict  Ty_RoleField;\
  PRE TyI TyIs_UNSET;  \
  PRE TyI TyIs_Unsafe; \
  PRE TyI TyIs_rSelf; \
  PRE TyI TyIs_RoleField;\
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
  PRE TyI TyIs_SI;     /* SI         */ \
  PRE TyI TyIs_rU1;    /* &U1        */ \
  PRE TyI TyIs_rU2;    /* &U2        */ \
  PRE TyI TyIs_rU4;    /* &U4        */ \
  PRE TyI TyIs_rU1_U4; /* &U1, U4    */ \

TYIS(extern)

void N_assertWsEmpty(Kern* k);

#endif // __FNGI_H
