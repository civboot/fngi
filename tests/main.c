
#include <assert.h>

#include "fngi.h"

void disassemble(void* fn, U2 len) {
  U1* c = fn; eprintf("## Disassemble %p [len=%u]\n", c, len);
  for(U2 i = 0; i < len; i++) {
    U1 instr = c[i]; Slc name = instrName(instr);
    eprintf("%.2X [%.*s]\n", instr, Dat_fmt(name));
  }
}
#define DISASSEMBLE(FN) \
  TyFn* LINED(fn) = tyFn(Kern_findTy(k, &KEY(FN))); \
  eprintf("DISASSEMBLED %.*s [len=%u]:\n", Ty_fmt(LINED(fn)), LINED(fn)->len); \
  disassemble(LINED(fn)->code, LINED(fn)->len);

TEST(basic)
  TASSERT_EQ(7,    cToU1('7'));
  TASSERT_EQ(0xB,  cToU1('b'));
  TASSERT_EQ(0xB,  cToU1('B'));
  TASSERT_EQ(0xFF, cToU1('-'));
END_TEST

TEST_FNGI(init, 10)
  TASSERT_EQ(WS_DEPTH, WS->cap);
  TASSERT_EQ(WS->sp,   WS->cap);
  WS_ADD(4); TASSERT_WS(4);
END_TEST_FNGI

void N_add42(Kern* k) {
  WS_ADD(WS_POP() + 42);
}

// Helpers to execute functions
static inline void _xFn(Kern* k, TyFn fn) { executeFn(k, &fn); }

// XFN(dat, meta, lSlots)
#define XFN(...)  _xFn(k, litFn(__VA_ARGS__))

TEST_FNGI(call, 1)
  // Running some code

  TyFn_static(fiveFn, 0, 0, ((U1[]){SLIT + 2, SLIT + 3, ADD, RET}) );
  executeFn(k, &fiveFn);       TASSERT_WS(5);

  // Calling a function
  Buf_var(call5, 16);
  Buf_add(&call5, XL); Buf_addBE4(&call5, (S)&fiveFn); Buf_add(&call5, RET);
  XFN(call5.dat, 0, 0);       TASSERT_WS(5);

  // Executing a native
  TyFn_static(add42, TY_FN_NATIVE, 0, kFn(N_add42));
  Buf_var(callAdd42, 16);
  Buf_add(&callAdd42, XL); Buf_addBE4(&callAdd42, (S)&add42); Buf_add(&callAdd42, RET);
  WS_ADD(3); XFN(callAdd42.dat, 0, 0);  TASSERT_WS(45);
  TASSERT_EMPTY();
END_TEST_FNGI

#define TASSERT_TOKEN(T) \
  tokenDrop(k); scan(k); \
  TASSERT_SLC_EQ(T, *Buf_asSlc(&k->g.c.token));

TEST_FNGI(scan, 1)
  U1 dat[32]; k->g.c.token = (Buf){.dat = dat, .cap = 32};
  BufFile_var(bf, 8, "  this-has(*) eight tokens");
  k->g.c.src = (SpReader) {.m = &mSpReader_BufFile, .d = &bf };

  TASSERT_TOKEN("this");  TASSERT_TOKEN("-"); TASSERT_TOKEN("has");
  TASSERT_TOKEN("(");     TASSERT_TOKEN("*"); TASSERT_TOKEN(")");
  TASSERT_TOKEN("eight"); TASSERT_TOKEN("tokens");
END_TEST_FNGI

Slc testCxt = SLC("test");
#define TY_CHECK(REQ, GIV, SAMELEN) tyCheck(REQ, GIV, SAMELEN, testCxt)

TEST_FNGI(compile0, 4)
  k->g.c.fnState |= C_UNTY;
  Kern_fns(k);
  TASSERT_EMPTY();

  // Very explicit memory layout
  BufFile_var(bf, 8, "42 + 7 ret;");
  k->g.c.src = (SpReader) {.m = &mSpReader_BufFile, .d = &bf };
  U1 codeDat[256]; k->g.c.code = (Buf){.dat=codeDat, .cap=256};
  compileSrc(k);
  XFN(codeDat, 0, 0);
  TASSERT_WS(49);
  TASSERT_EMPTY();

  COMPILE_EXEC("inc 4");       TASSERT_WS(5);
  COMPILE_EXEC("inc2 4");      TASSERT_WS(6);
  COMPILE_EXEC("dec  4");      TASSERT_WS(3);
  COMPILE_EXEC("1 + 2");       TASSERT_WS(3);
  COMPILE_EXEC("1 shl 4");     TASSERT_WS(1 << 4);
  TASSERT_EMPTY();
  COMPILE_EXEC("7 - (3 + 2)"); TASSERT_WS(2);
  TASSERT_EMPTY();

  COMPILE("fn answer[ ] do 0x42", false);
  TyFn* answer = tyFn(Kern_findTy(k, &KEY("answer")));
  executeFn(k, answer);    TASSERT_WS(0x42);
  COMPILE_EXEC("answer;");  TASSERT_WS(0x42);
  TASSERT_EMPTY();

  COMPILE_EXEC("0x44");     TASSERT_WS(0x44);
  COMPILE_EXEC("char:d");   TASSERT_WS('d');
  COMPILE_EXEC("char:\\n"); TASSERT_WS('\n');
  COMPILE_EXEC("char:\\t"); TASSERT_WS('\t');
  COMPILE_EXEC("char: ");   TASSERT_WS(' ');
  COMPILE_EXEC("char:\\ "); TASSERT_WS(' ');
END_TEST_FNGI

TEST_FNGI(inlineFns, 4)
  k->g.c.fnState |= C_UNTY;
  Kern_fns(k);
  COMPILE_EXEC("swp(1, 4)");    TASSERT_WS(1); TASSERT_WS(4);

  TASSERT_EMPTY();
END_TEST_FNGI

TEST_FNGI(compile1, 10)
  Kern_fns(k);
  k->g.c.fnState |= C_UNTY;

  COMPILE_EXEC("fn maths[ ] do (_ + 7 + (3 * 5))  maths(4)"); TASSERT_WS(26);
  COMPILE_EXEC("1 \\comment \\(3 + 4) + 7"); TASSERT_WS(8)
  COMPILE_EXEC("fn maths2[stk:U2 -> U2] do (_ + 10)  maths2(6)"); TASSERT_WS(16);
  TyFn* maths2 = tyFn(Kern_findTy(k, &KEY("maths2")));
  TASSERT_EQ(&Ty_U2, (TyDict*)maths2->inp->ty);  TASSERT_EQ(NULL , maths2->inp->name);
  TASSERT_EQ(&Ty_U2, (TyDict*)maths2->out->ty);  TASSERT_EQ(NULL , maths2->out->name);
  TASSERT_EQ(NULL, (TyDict*)maths2->inp->next); TASSERT_EQ(NULL, maths2->out->next);

  COMPILE_EXEC("fn maths3[x:U2 -> U2] do (x + 4) maths3(3)");
  TyFn* maths3 = tyFn(Kern_findTy(k, &KEY("maths3")));
  TASSERT_EQ(&Ty_U2, (TyDict*)maths3->inp->ty);
  TASSERT_SLC_EQ("x" , CStr_asSlc(maths3->inp->name));
  TASSERT_EQ(&Ty_U2, (TyDict*)maths3->out->ty);  TASSERT_EQ(NULL , maths3->out->name);
  TASSERT_EQ(NULL, maths3->inp->next); TASSERT_EQ(NULL, maths3->out->next);
  TASSERT_EQ(1, maths3->lSlots);
  TASSERT_WS(7);

  COMPILE_EXEC("fn pop2[a:U1 b:U2 c:S -> \\a:S] do (a); pop2(0x1234 2 3)");
  TASSERT_WS(0x34);
  TyFn* pop2 = tyFn(Kern_findTy(k, &KEY("pop2")));
  TASSERT_SLC_EQ("c" , CStr_asSlc(pop2->inp->name));
  TASSERT_SLC_EQ("b" , CStr_asSlc(pop2->inp->next->name));
  TASSERT_SLC_EQ("a" , CStr_asSlc(pop2->inp->next->next->name));
  TASSERT_EQ(&Ty_S, (TyDict*)pop2->inp->ty);
  TASSERT_EQ(2, pop2->lSlots);

  COMPILE_EXEC("tAssertEq(42, 42)");
  COMPILE_EXEC("tAssertEq(0x10 + 3, 0x13)");
  COMPILE_EXEC("tAssertEq(1 shl 4, 0x10)");
  COMPILE_EXEC("tAssertEq(0x10 shr 4, 1)");
END_TEST_FNGI

TEST_FNGI(comment, 10)
  Kern_fns(k);
  COMPILE_EXEC("\\1 \\(foo bar) \\(3 4 5)\n\\ 1 2 3 this is a line\n\\3");
  TASSERT_EQ(0, Stk_len(WS));
END_TEST_FNGI

TEST_FNGI(tyDb, 4)
  Kern_fns(k);
  LOCAL_TYDB_BBA(tyDb);

  TY_CHECK(&TyIs_S, &TyIs_S,  false);
  TY_CHECK(&TyIs_S, &TyIs_S,  true);
  TY_CHECK(&TyIs_S, &TyIs_SS, false);

  EXPECT_ERR(TY_CHECK(&TyIs_S, &TyIs_SS, true), "test");
  EXPECT_ERR(TY_CHECK(&TyIs_U4x2, &TyIs_rU1_U4, true), "test");

  TyDb* db = &k->g.tyDb;

  TyDb_new(db);

  // Call a function which returns [S], the type gets put on
  tyCall(k, db, NULL, &TyIs_S); TY_CHECK(&TyIs_S, TyDb_top(db), true);

  // Now call a function which consumes [S], the type stack empties
  tyCall(k, db, &TyIs_S, NULL); TY_CHECK(NULL, TyDb_top(db), true);

  // ret[done] causes errors on future operations
  DictStk_add(&k->g.implStk, Kern_findTy(k, &KEY("+")));
  tyCall(k, db, NULL, &TyIs_S);
  tyRet(k, db, RET_DONE);  TASSERT_EQ(RET_DONE, TyDb_done(db));
  FNGI_EXPECT_ERR(tyCall(k, db, &TyIs_S, NULL), "Code after guaranteed 'ret'");
  END_LOCAL_TYDB_BBA(tyDb);
END_TEST_FNGI

TEST_FNGI(compileTy, 6)
  Kern_fns(k);
  REPL_START
  COMPILE_EXEC("fn pop2  [  a:U1 b:U2 c:S -> \\a:U1] do (a)");
  COMPILE_EXEC("fn pop2_ [stk:U1 b:U2 c:S -> \\a:U1]  do (_)");
  COMPILE_EXEC("fn add   [stk:S  b:S      -> \\sum:S] do (_ + b)");
  COMPILE_EXEC("add(42, 7)"); TASSERT_WS(49);

  COMPILE_EXEC("fn addUs[stk:U1 b:U2 -> \\a:S ] do (_ + b)");
  COMPILE_EXEC("addUs(cast:U1(0x12) cast:U2(0x34))"); TASSERT_WS(0x46);
  REPL_END
END_TEST_FNGI

#define TY_LEN  Sll_len((Sll*)TyDb_top(&k->g.tyDb))
TEST_FNGI(compileIf, 10)
  Kern_fns(k);
  REPL_START

  k->g.c.fnState |= C_UNTY;

  COMPILE_EXEC("if(1) do ;");                              TASSERT_EQ(0, Stk_len(WS));
  COMPILE_EXEC("if(1) do 0x42 else 0x33");                 TASSERT_WS(0x42);
  COMPILE_EXEC("if(0) do 0x42 else 0x33");                 TASSERT_WS(0x33);
  COMPILE_EXEC("if(1) do 0x42 elif(1) do 0x11 else 0x33"); TASSERT_WS(0x42);
  COMPILE_EXEC("if(0) do 0x42 elif(1) do 0x11 else 0x33"); TASSERT_WS(0x11);
  COMPILE_EXEC("if(0) do 0x42 elif(0) do 0x11 else 0x33"); TASSERT_WS(0x33);
  TASSERT_EQ(0, Stk_len(WS));
  TASSERT_EQ(0, TY_LEN);

  k->g.c.fnState = (~C_UNTY) & k->g.c.fnState;
  TASSERT_EQ(0, TY_LEN); TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC("if(1) do ;");
  TASSERT_EQ(0, TY_LEN); TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC("if(1) do 0x42 else 0x33"); TASSERT_EQ(1, TY_LEN);
  COMPILE_EXEC("tAssertEq 0x42");          TASSERT_EQ(0, TY_LEN);

  COMPILE_EXEC("fn ifRet[stk:S -> S] do ( if(_) do (0x22 ret;) else 0x33 )")
  COMPILE_EXEC("tAssertEq(0x22, ifRet(1))");
  COMPILE_EXEC("tAssertEq(0x33, ifRet(0))");

  COMPILE_EXEC("fn ifElifRet[x:S -> S] do ("
               "  if(  x == 0x42) do (x ret;)"
               "  elif(x == 0x11) do (0 ret;)"
               "  else 0x33"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifRet(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifRet(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifRet(7))");

  COMPILE_EXEC("fn ifElifRetMid[x:S -> S] do ("
               "  if  (x == 0x42) do (0x42)"
               "  elif(x == 0x11) do (0 ret;)"
               "  else 0x33"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifRetMid(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifRetMid(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifRetMid(7))");

  COMPILE_EXEC("fn ifElifStk[stk:S -> S] do ("
               "  if  (dup; == 0x42) do (_)"
               "  elif(dup; == 0x11) do (drp; 0 ret;)"
               "  else (drp; 0x33)"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifStk(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifStk(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifStk(7))");

  #define IF_ALL_RET \
    "if(0) do (0 ret;) elif(0) do (0 ret;) else (1 ret;)"

  eprintf("??? FAILURE\n");
  COMPILE_EXEC("fn one[ -> S] do (" IF_ALL_RET ")");
  COMPILE_EXEC("tAssertEq(1,    one;)");
  COMPILE_EXEC("fn ifRet1[ -> S] do ( if(1) do ret 2; ret 4; )");
  COMPILE_EXEC("tAssertEq(2, ifRet1,)");

  TASSERT_EQ(1, Stk_len(&k->g.tyDb.tyIs));
  SET_SRC("fn bad[ -> S] do (" IF_ALL_RET "4 )");
  FNGI_EXPECT_ERR(compileRepl(k, true), "Code after guaranteed 'ret'");
  TASSERT_EQ(1, Stk_len(&k->g.tyDb.tyIs));

  REPL_END
END_TEST_FNGI

TEST_FNGI(compileBlk, 10)
  Kern_fns(k);
  REPL_START
  COMPILE_EXEC("S(0) blk( drp; )"); TASSERT_EMPTY();

  SET_SRC("S(0) blk( drp; cont; )");
  FNGI_EXPECT_ERR(compileRepl(k, true), "cont not identical");

  COMPILE_EXEC(
    "fn loop[ -> S] do (\n"
    "  S 0 -> blk(\n"
    "    if(dup, >= 5) do (drp; break 0x15)\n"
    "    inc; cont;\n"
    "  )\n"
    ") loop()"); TASSERT_WS(0x15);
  REPL_END
  TASSERT_EQ(0, Stk_len(&k->g.tyDb.tyIs));
END_TEST_FNGI

TEST_FNGI(compileVar, 10)
  Kern_fns(k); REPL_START;
  COMPILE_EXEC(
      "fn useVar[stk:S -> S] do (\n"
      "  var a: S = (_ + 7);  ret inc(a);\n"
      ") useVar(0x42)"); TASSERT_WS(0x4A);

  TASSERT_EMPTY();

  COMPILE_EXEC("fn idenRef[a:&S -> &S] do ( a )\n");
  COMPILE_EXEC("fn ftRef  [a:&S -> S ] do ( @a )\n");
  TyFn* ftRef = (TyFn*) Kern_findTy(k, &KEY("ftRef"));
  TASSERT_EQ(1, ftRef->inp->meta);
  TASSERT_EQ(0, ftRef->out->meta);

  COMPILE_EXEC("fn useRef[a:S -> S ] do ( ftRef(&a) )\n");
  COMPILE_EXEC("useRef(0x29)");
  COMPILE_EXEC("tAssertEq(0x29)");
  COMPILE_EXEC("fn getRefs[a:S -> &S &S] do ( var b:U4; &a, &b )\n");
  COMPILE_EXEC("getRefs(2);");
  WS_POP2(S a, S b);
  U4 localBot = RS_topRef(k) - 12; // 12 == size(ret addr) + size(A) + size(B)
  TASSERT_EQ(localBot    , a);
  TASSERT_EQ(localBot + 4, b);
  REPL_END
END_TEST_FNGI

TEST_FNGI(compileStruct, 10)
  Kern_fns(k); REPL_START;

  COMPILE_EXEC(
      "fn chTy[stk:S -> U2] do ( ret U2; )"
      "chTy(0x42)"); TASSERT_WS(0x42);

  COMPILE_EXEC("struct Foo [ a: U2; b: U1; c: U4 ]");
  TyDict* Foo = (TyDict*) Kern_findTy(k, &KEY("Foo"));
  assert(Foo);
  TASSERT_EQ(TY_DICT | TY_DICT_STRUCT, Foo->meta);
  TASSERT_EQ(8, Foo->sz);
  assert(TyDict_find(Foo, &KEY("a")));

  COMPILE_EXEC("struct A [ a: S ]");
  COMPILE_EXEC("struct B [ a: A; b: S ]");
  COMPILE_EXEC(
    "fn simpleCreate[a:S -> B] do ("
    "  B(A(a), 0x42)"
    "); simpleCreate(0x33)"); TASSERT_WS(0x42); TASSERT_WS(0x33);

  COMPILE_EXEC("destruct(simpleCreate(0x35)) tAssertEq(0x42)")
  TASSERT_WS(0x35);
  TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC(
    "fn useField[a:A -> S] do ( a.a + 7 )"
    "  tAssertEq(0x18, useField(A 0x11))");

  COMPILE_EXEC("fn getStruct[b:B -> B] do ( b ); getStruct(B(A 0x4321, 0x321))");
  TASSERT_WS(0x321); TASSERT_WS(0x4321);
  REPL_END
END_TEST_FNGI

TEST_FNGI(fnSig, 10)
  Kern_fns(k); REPL_START;

  COMPILE_EXEC(
    "fn add1[a:S -> S] do (\n"
    "  a + 1\n"
    "); tAssertEq(add1(4), 5)");

   COMPILE_EXEC(
     "fn useFn [ f:&fnSig[S->S] -> S] do (\n"
     "  @f(8)\n"
     ")");
   COMPILE_EXEC("tAssertEq(9, useFn(&add1))");

  REPL_END
END_TEST_FNGI

TEST_FNGI(alias, 10)
  Kern_fns(k); REPL_START;

  COMPILE_EXEC("struct A [ a: S ]  alias Aa:A\n"
    "fn useAlias[a:Aa -> A] do (\n"
    "  Aa(a.a)\n"
    "); useAlias(A(0x30))\n"); TASSERT_WS(0x30);

  REPL_END
END_TEST_FNGI


TEST_FNGI(structBrackets, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ a1: S, a2: S ]");
  COMPILE_EXEC("struct B [ b1: A, b2: S ]");
  COMPILE_EXEC("fn buildA[ -> A] do ( var a: A = { a1 = 0x33  a2 = 0x22 } a )")
  COMPILE_EXEC("buildA()"); TASSERT_WS(0x22); TASSERT_WS(0x33);
  COMPILE_EXEC("fn buildB[ -> B] do ( var b: B = {\n"
               "  b1 = { a1 = 0x35  a2 = 0x25 }\n"
               "  b2 = 0x11\n"
               "} b )")
  COMPILE_EXEC("buildB()"); TASSERT_WS(0x11); TASSERT_WS(0x25); TASSERT_WS(0x35);
  COMPILE_EXEC("fn buildBwA[ -> B] do ( var b: B = {\n"
               "  b1 = buildA()\n"
               "  b2 = 0x11\n"
               "} b )")
  COMPILE_EXEC("buildBwA()"); TASSERT_WS(0x11); TASSERT_WS(0x22); TASSERT_WS(0x33);


  REPL_END
END_TEST_FNGI

TEST_FNGI(global, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("global myA:S = 32");
  TyVar* a = tyVar(Kern_findTy(k, &KEY("myA")));
  TASSERT_EQ(0, Stk_len(WS));
  TASSERT_EQ(32, ftSzI((U1*)a->v, SZR));
  COMPILE_EXEC("imm#tAssertEq(32, myA)");
  COMPILE_EXEC("tAssertEq(32, myA)");

  COMPILE_EXEC("struct Foo [ a: S; b: S; c: S]");
  COMPILE_EXEC("global foo:Foo = Foo(7, 3, 12)");
  TASSERT_EQ(0, Stk_len(WS));
  TyVar* foo = tyVar(Kern_findTy(k, &KEY("foo")));
  TASSERT_EQ(7, ftSzI((U1*)foo->v, SZR));

  COMPILE_EXEC("tAssertEq(7, foo.a)   tAssertEq(3, foo.b)");
  COMPILE_EXEC("@ &foo.a"); TASSERT_WS(7);
  COMPILE_EXEC("imm#tAssertEq(7, foo.a)");
  COMPILE_EXEC("foo.a = 0x444 tAssertEq(0x444, foo.a)");
  COMPILE_EXEC("imm#( tAssertEq(0x444, foo.a); tAssertEq(3, foo.b) )");
  COMPILE_EXEC("assertWsEmpty;");

  COMPILE_EXEC("const c1:S = 12");
  COMPILE_EXEC("tAssertEq(12, c1)");

  COMPILE_EXEC("const cFoo:Foo = foo;");
  COMPILE_EXEC("tAssertEq(0x444, cFoo.a); tAssertEq(3, foo.b)");
  REPL_END
END_TEST_FNGI

TEST_FNGI(mod, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("mod foo ( fn one[ -> S] do 1 )");
  COMPILE_EXEC("imm#tAssertEq(1, foo.one())");
  COMPILE_EXEC("use:foo ( imm#tAssertEq(1, one()) )");
  REPL_END
END_TEST_FNGI

TEST_FNGI(structDeep, 12)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ a: S ]");
  COMPILE_EXEC("struct B [ a: A; b: S; rA: &A ]");
  COMPILE_EXEC("struct C [a: &A, b: &B]")
  COMPILE_EXEC("fn cGetA[c:&C -> S] do ( c.b.a.a );");
  COMPILE_EXEC("fn useC[ -> S S] do (\n"
               "  var a: A = A 1\n"
               "  var b: B = B(A 2, 3, &a)\n"
               "  var c: C = C(&a, &b)\n"
               "  tAssertEq(b.rA.a, 1)\n"
               "  tAssertEq(@(&b.rA.a), 1)\n"
               "  c.b.a.a, cGetA(&c)\n"
               ")");
  COMPILE_EXEC("useC;");
    TASSERT_WS(2); TASSERT_WS(2);

  COMPILE_EXEC("fn assign[ -> S] do (\n"
               "  var a: A;\n"
               "  a.a = 0x42\n"
               "  a.a\n"
               ")");
  COMPILE_EXEC("assign()"); TASSERT_WS(0x42);

  COMPILE_EXEC("fn assignRef[a:&A] do (a.a = 0x44)")
  COMPILE_EXEC("fn useAssignRef[ -> S] do (var a:A; assignRef(&a); a.a)");
  COMPILE_EXEC("useAssignRef()"); TASSERT_WS(0x44);
  REPL_END
END_TEST_FNGI

TEST_FNGI(structSuper, 12)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ a: S ]");
  COMPILE_EXEC("struct B [ parent: A; b: S ]");

  COMPILE_EXEC("fn useB[b:B -> S S] do (\n"
               "  b.b, b.a\n"
               ")");
  COMPILE_EXEC("useB(B(A(0x11), 0x12))"); TASSERT_WS(0x11); TASSERT_WS(0x12);

  COMPILE_EXEC("fn thing[ ] do ( var a:A = { a = 0x15 } ) ");
  COMPILE_EXEC("fn newB[ -> B] do ( var b:B = { a = 0x15  b = 0x16 } b)");
  COMPILE_EXEC("useB(newB;)");            TASSERT_WS(0x15); TASSERT_WS(0x16);

  COMPILE_EXEC("struct _Slc [ dat:&U1  len:U2 ]");
  COMPILE_EXEC("struct _Buf [ parent:_Slc  cap:U2 ]");
  TyDict* s = tyDict(Kern_findTy(k, &KEY("_Slc")));
  TyDict* b = tyDict(Kern_findTy(k, &KEY("_Buf")));
  TASSERT_EQ(6, s->sz); TASSERT_EQ(8, b->sz);

  // Prove the type-checker allows either a Slc or Buf pointer.
  COMPILE_EXEC("fn ignoreSlc[s:&_Slc] do ()");
  COMPILE_EXEC("fn ignoreBuf[s:&_Buf] do ()");
  COMPILE_EXEC(
      "fn passStuff[ ] do (\n"
      "  var s:_Slc;  var b:_Buf;\n"
      "  ignoreSlc(&s)\n"  // note: ignoreBuf(&s) causes a compiler error
      "  ignoreBuf(cast:&_Buf(&s))\n" // cast allows it (not really valid)
      "  ignoreSlc(&b)  ignoreBuf(&b)\n"
    ")");
  REPL_END
END_TEST_FNGI

TEST_FNGI(method, 20)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ v:S; meth aDo [self: &Self, x: S -> S] do ( self.v + x ) ]")
  COMPILE_EXEC("fn callADo[x:S a:A -> S] do ( a.aDo(x) )");
  COMPILE_EXEC("tAssertEq(8, callADo(3, A 5)) assertWsEmpty;");

  COMPILE_EXEC("impl A( fn nonMeth[x:S -> S] do ( 7 + x ) )")
  COMPILE_EXEC("fn callNonMeth[x:S a:A -> S] do ( a.nonMeth(x) )");
  COMPILE_EXEC("tAssertEq(10, callNonMeth(3, A 1)) assertWsEmpty;");

  COMPILE_EXEC("global a:A = A 5");
  COMPILE_EXEC("tAssertEq(5, a.v)");
  COMPILE_EXEC("tAssertEq(13, a.aDo(8))");
  COMPILE_EXEC("tAssertEq(14, a.nonMeth(7))");

  COMPILE_EXEC("imm#tAssertEq(13, a.aDo(8))");
  COMPILE_EXEC("imm#tAssertEq(14, a.nonMeth(7))");

  COMPILE_EXEC("impl A( meth aDoSelf [self: &Self, x: S -> S] do ( self.v + x ) )")
  COMPILE_EXEC("imm#tAssertEq(13, a.aDoSelf(8))");

  TyDict* A = tyDict(Kern_findTy(k, &KEY("A")));
  TyFn* aDo = tyFn(TyDict_find(A, &KEY("aDo")));
  COMPILE_EXEC("&A.aDo"); TASSERT_WS((S)aDo);
  REPL_END
END_TEST_FNGI

TEST_FNGI(ptr, 20) // tests necessary for libraries
  Kern_fns(k); REPL_START
  // Get the value at a pointer
  COMPILE_EXEC("fn ftPtr[a:S -> S] do ( @(&a) ) ftPtr(0x42)");
  TASSERT_WS(0x42);

  // Set the value at a pointer, then get it
  COMPILE_EXEC("fn srPtr[a:S -> S] do ( \\set @(&a) = (a + 3); a ) srPtr(3)");
  TASSERT_WS(6);

  // Just get pointer and an offset of 1
  COMPILE_EXEC("fn getPtrs[x:S -> &S, &S] do (&x, ptrAdd(&x, 1, 10)) getPtrs(5)")
  WS_POP2(S x, S xPlus1);
  TASSERT_EQ(x + sizeof(S), xPlus1);

  // Get a value at the pointer index 1
  COMPILE_EXEC("fn ftPtr1[a:S b:S -> S] do ( @ptrAdd(&a, 1, 2) ) ftPtr1(0xBAD, 0x733) ");
  TASSERT_WS(0x733);
  REPL_END
END_TEST_FNGI

TEST_FNGI(arr, 20) // tests necessary for libraries
  Kern_fns(k);
  REPL_START
  COMPILE_EXEC(
    "fn simpleArr[x:S -> S] do (\n"
    "  var a: Arr [3 S]\n"
    "  var b: S = 0x42\n"
    "  a = 3; @ptrAdd(&a, 1, 3) = 4; @ptrAdd(&a, 2, 3) = 5;\n"
    "  tAssertEq(0x42, b)\n"
    "  @ptrAdd(&a, 3, 4) = 9; \\ note: setting b\n"
    "  tAssertEq(9, b)\n"
    "  @ptrAdd(&a, x, 3)\n"
    ")");
  COMPILE_EXEC("simpleArr 0"); TASSERT_WS(3);
  COMPILE_EXEC("simpleArr 1"); TASSERT_WS(4);
  COMPILE_EXEC("simpleArr 2"); TASSERT_WS(5);

  COMPILE_EXEC("fn unknown[x:?&S] do ( )")
  TyFn* unknown = tyFn(Kern_findTy(k, &KEY("unknown")));
  assert(TY_UNSIZED == unknown->inp->arrLen);

  COMPILE_EXEC("struct UnsizedThing [ len:U2  dat:Arr[ ? U1] ]");
  REPL_END
END_TEST_FNGI

TEST_FNGI(role, 20)
  Kern_fns(k);
  REPL_START

  CStr_ntVar(aaa, "\x03", "aaa");
  TyI fakeKey = { .name = aaa, .ty = NULL };
  TyDict tyKeyDict = {
    .name = aaa, .tyKey = &fakeKey,
    .meta = TY_DICT | TY_DICT_STRUCT,
  };
  Kern_addTy(k, (Ty*)&tyKeyDict);
  Key key = { .name = SLC("aaa"), .tyI = &fakeKey };
  TyDict* found = (TyDict*)Kern_findTy(k, &key);
  TASSERT_EQ(&tyKeyDict, found);

  COMPILE_EXEC(
    "role Adder [\n"
    "  absmeth add [ &Self \\(b)S -> S ]\n"
    "]");
  TyDict* Adder = tyDict(Kern_findTy(k, &KEY("Adder")));
  TyFn* add = tyFn(TyDict_find(Adder, &KEY("add")));
  TASSERT_EQ(NULL, add->inp->name);
  assert(add->inp->ty == (TyBase*) &Ty_S);
  TyVar* addV = tyVar(TyDict_find(Adder, &(Key){
    .name = SLC("add"), .tyI = &TyIs_RoleField }));
  assert(isFnSig(addV->tyI->ty));

  COMPILE_EXEC(
    // "unty fn add [ self:&Self, b:S -> S ] do;\n"
    "struct A [\n"
    "  a: S\n"
    "  meth add [ self:&Self, b:S -> S ] do (\n"
    "    self.a + b\n"
    "  )\n"
    "]");
  TyDict* A = tyDict(Kern_findTy(k, &KEY("A")));
  TyFn* A_add = tyFn(TyDict_find(A, &KEY("add")));

  COMPILE_EXEC("global myA: A = A(5)\n tAssertEq(8, myA.add(3))\n")
  COMPILE_EXEC("impl A:Adder { add = &A.add }");

  // Test that it's actually implemented
  TyVar* A_Adder = tyVar(TyDict_find(A, &(Key) {
    .name = SLC("Adder"), .tyI = &(TyI) { .ty = (TyBase*)Adder, } }));
  assert(A_Adder); TASSERT_EQ((S)A_add, *(S*)A_Adder->v);

  COMPILE_EXEC(
    "fn useAdder[a: Adder -> S] do (\n"
     "  a.add(5)\n"
    ")");

  COMPILE_EXEC(
    "fn getAdder[a: A -> Adder] do (\n"
    "  Adder(&a)"
    ")");
  COMPILE_EXEC("getAdder(A 5)"); TASSERT_WS(A_Adder->v); WS_POP();

  // Check that
  COMPILE_EXEC("fn getGotAdder[a: Adder -> Adder] do (a)");
  COMPILE_EXEC("getGotAdder(getAdder(A 5))"); TASSERT_WS(A_Adder->v); WS_POP();

  COMPILE_EXEC(
    "fn createUseAdder[a: A -> S] do (\n"
    "  useAdder(Adder(&a))"
    ")");
  COMPILE_EXEC("createUseAdder(A 3)"); TASSERT_WS(8);

  // TODO
  COMPILE_EXEC(
    "fn useAdderRef[aR:&Adder -> S] do (\n"
    "  var a:Adder = @aR\n"
    "  a.add(5)\n"
    ")");
  COMPILE_EXEC(
    "global a:A = A 5\n"
    "global adder:Adder = Adder &a\n"
  );
  COMPILE_EXEC("useAdderRef(&adder)"); TASSERT_WS(10);

  REPL_END
END_TEST_FNGI

TEST_FNGI(misc, 20)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("imm#tAssertEq(5, 1 + 4)");
  COMPILE_EXEC("fn useLit[a: S -> S] do ( a + lit#(4 + 12) )");
  COMPILE_EXEC("tAssertEq(0x15, useLit(5))");
  REPL_END
END_TEST_FNGI

TEST_FNGI(file_basic, 20)
  Kern_fns(k);
  N_assertWsEmpty(k);
  CStr_ntVar(path, "\x0E", "tests/basic.fn");
  compilePath(k, path);
END_TEST_FNGI

#define TYDICT_SZ(MOD, NAME) \
   TyDict_sz(tyDict(TyDict_find(MOD, &KEY(NAME))))

TEST_FNGI(core, 20)
  Kern_fns(k); Core_mod(k);
  REPL_START
  COMPILE_EXEC(
    "fn useSlc[ -> U1] do (\n"
    "  var dat:Arr[12 U1]\n"
    "  dat                  = char:h \n"
    "  @ptrAdd(&dat, 1, 12) = char:i \n"
    "  @ptrAdd(&dat, 2, 12) = char:p \n"
    "  var s: Slc = Slc(&dat, U2(12))\n"
    "  tAssertEq(char:h, s.@0)\n"
    "  tAssertEq(char:i, s.@1)\n"
    "  s.@2\n"
    ")");
  COMPILE_EXEC("useSlc;"); TASSERT_WS('p');

  COMPILE_EXEC(
    "fn getSlc[ -> Slc] do (\n"
    "  |hello \\| slc|\n"
    ")");
  COMPILE_EXEC("getSlc;"); WS_POP2(S dat, U2 len);
  Slc result = (Slc) {(U1*)dat, len};
  TASSERT_SLC_EQ("hello | slc", result);

  TyDict* comp = tyDict(Kern_findTy(k, &KEY("comp")));
  TASSERT_EQ(sizeof(TyBase) - 2, TYDICT_SZ(comp, "TyBase"));
  TASSERT_EQ(sizeof(Ty), TYDICT_SZ(comp, "Ty"));
  TASSERT_EQ(sizeof(TyDict) - 2, TYDICT_SZ(comp, "TyDict"));
  TASSERT_EQ(sizeof(Globals), TYDICT_SZ(comp, "Globals"));

  REPL_END
END_TEST_FNGI

#define USE_ARENA_FNGI \
    "  var a: &Any; a = arena.alloc(10, 4); tAssert(S a)\n" \
    "  var b: &Any; b = arena.alloc(12, 4); tAssert(S b)\n" \
    "  tAssert(not S arena.free(b, 12, 4))\n" \
    "  tAssert(not S arena.free(a, 10, 4))\n" \
    "  (a, b)\n"

TEST_FNGI(bba, 20)
  Kern_fns(k); Core_mod(k);
  REPL_START

  COMPILE_EXEC("fn useBBA[ arena:&BBA -> &Any &Any] do (\n" USE_ARENA_FNGI ")");
  eprintf("??? bbaDict=%p\n", k->g.bbaDict);
  COMPILE_EXEC("comp.g.bbaDict"); TASSERT_WS((S)k->g.bbaDict);
  COMPILE_EXEC("useBBA(comp.g.bbaDict)");
  void* a = BBA_alloc(k->g.bbaDict,    10, 4);
  void* b = BBA_alloc(k->g.bbaDict,    12, 4);
  assert(not BBA_free(k->g.bbaDict, b, 12, 4));
  assert(not BBA_free(k->g.bbaDict, a, 10, 4));
  TASSERT_WS((S)b); TASSERT_WS((S)a);

  Arena arena = BBA_asArena(k->g.bbaDict);

  COMPILE_EXEC("fn useArena[ arena:Arena -> &Any &Any] do (\n" USE_ARENA_FNGI ")");
  COMPILE_EXEC("useArena(Arena comp.g.bbaDict)");
  a = Xr(arena,alloc,    10, 4);
  b = Xr(arena,alloc,    12, 4);
  assert(not Xr(arena,free, b, 12, 4));
  assert(not Xr(arena,free, a, 10, 4));
  TASSERT_WS((S)b); TASSERT_WS((S)a);
  REPL_END
END_TEST_FNGI

TEST_FNGI(repl, 20)
  Kern_fns(k);
  simpleRepl(k);
END_TEST_FNGI

int main(int argc, char* argv[]) {
  ARGV = argv;
  SETUP_SIG((void *)fngiHandleSig);

  char* arg; bool repl = false;
  for(int i = 1; i < argc; arg = argv[i++]) {
    if(strcmp("--repl", arg)) repl = true;
    else {
      eprintf("Unrecognized argument: %s\n", arg);
    }
  }

  eprintf("# Running tests\n");
  test_basic();
  test_init();
  test_call();
  test_scan();
  test_compile0();
  test_compile1();
  test_inlineFns();
  test_comment();
  test_tyDb();
  test_compileTy();
  test_compileIf();
  test_compileBlk();
  test_compileVar();
  test_compileStruct();
  test_fnSig();
  test_alias();
  test_structBrackets();
  test_global();
  test_mod();
  test_structDeep();
  test_structSuper();
  test_method();
  test_ptr();
  test_arr();
  test_role();
  test_misc();
  test_file_basic();
  test_core();
  test_bba();
  eprintf("# Tests complete\n");

  if(repl) test_repl();

  return 0;
}
