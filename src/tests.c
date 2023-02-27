
#include "./fngi.h"

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

  TyFn_static(fiveFn, 0, 0, (S)((U1[]){SLIT + 2, SLIT + 3, ADD, RET}) );
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
  TASSERT_SLC_EQ(T, *Buf_asSlc(&k->g.token));

TEST_FNGI(scan, 1)
  U1 dat[32]; k->g.token = (Buf){.dat = dat, .cap = 32};
  BufFile_var(bf, 8, "  this-has(*) eight tokens");
  k->g.src = (SpReader) {.m = &mSpReader_BufFile, .d = &bf };

  TASSERT_TOKEN("this");  TASSERT_TOKEN("-"); TASSERT_TOKEN("has");
  TASSERT_TOKEN("(");     TASSERT_TOKEN("*"); TASSERT_TOKEN(")");
  TASSERT_TOKEN("eight"); TASSERT_TOKEN("tokens");
END_TEST_FNGI

Slc testCxt = SLC("test");
#define TY_CHECK(REQ, GIV, SAMELEN) tyCheck(REQ, GIV, SAMELEN, testCxt)

TEST_FNGI(compile0, 4)
  k->g.fnState |= C_UNTY;
  Kern_fns(k);
  TASSERT_EMPTY();

  // Very explicit memory layout
  BufFile_var(bf, 8, "42 + 7 ret;");
  k->g.src = (SpReader) {.m = &mSpReader_BufFile, .d = &bf };
  U1 codeDat[256]; k->g.code = (Buf){.dat=codeDat, .cap=256};
  compileSrc(k); XFN(codeDat, 0, 0); TASSERT_WS(49);
  TASSERT_EMPTY();

  COMPILE_EXEC("inc 4");       TASSERT_WS(5);
  COMPILE_EXEC("inc2 4");      TASSERT_WS(6);
  COMPILE_EXEC("dec  4");      TASSERT_WS(3);
  COMPILE_EXEC("1 + 2");       TASSERT_WS(3);
  COMPILE_EXEC("1 shl 4");     TASSERT_WS(1 << 4);
  TASSERT_EMPTY();
  COMPILE_EXEC("7 - (3 + 2)"); TASSERT_WS(2);
  TASSERT_EMPTY();

  COMPILE("fn answer do 0x42", false);
  TyFn* answer = tyFn(Kern_findTy(k, SLC("answer")));
  executeFn(k, answer);    TASSERT_WS(0x42);
  COMPILE_EXEC("answer;");  TASSERT_WS(0x42);
  TASSERT_EMPTY();
END_TEST_FNGI

TEST_FNGI(inlineFns, 4)
  k->g.fnState |= C_UNTY;
  Kern_fns(k);
  COMPILE_EXEC("swp(1, 4)");    TASSERT_WS(1); TASSERT_WS(4);

  TASSERT_EMPTY();
END_TEST_FNGI

TEST_FNGI(compile1, 10)
  Kern_fns(k);
  k->g.fnState |= C_UNTY;

  COMPILE_EXEC("fn maths do (_ + 7 + (3 * 5))  maths(4)"); TASSERT_WS(26);
  COMPILE_EXEC("1 \\comment \\(3 + 4) + 7"); TASSERT_WS(8)
  COMPILE_EXEC("fn maths2 stk:U2 -> U2 do (_ + 10)  maths2(6)"); TASSERT_WS(16);
  TyFn* maths2 = tyFn(Kern_findTy(k, SLC("maths2")));
  TASSERT_EQ(&Ty_U2, maths2->inp->ty);  TASSERT_EQ(NULL , maths2->inp->name);
  TASSERT_EQ(&Ty_U2, maths2->out->ty);  TASSERT_EQ(NULL , maths2->out->name);
  TASSERT_EQ(NULL, maths2->inp->next); TASSERT_EQ(NULL, maths2->out->next);

  COMPILE_EXEC("fn maths3 x:U2 -> U2 do (x + 4) maths3(3)");
  TyFn* maths3 = tyFn(Kern_findTy(k, SLC("maths3")));
  TASSERT_EQ(&Ty_U2, maths3->inp->ty);
  TASSERT_SLC_EQ("x" , CStr_asSlc(maths3->inp->name));
  TASSERT_EQ(&Ty_U2, maths3->out->ty);  TASSERT_EQ(NULL , maths3->out->name);
  TASSERT_EQ(NULL, maths3->inp->next); TASSERT_EQ(NULL, maths3->out->next);
  TASSERT_EQ(1, maths3->lSlots);
  TASSERT_WS(7);

  COMPILE_EXEC("fn pop2 a:U1 b:U2 c:S -> \\a:S do (a); pop2(0x1234 2 3)");
  TASSERT_WS(0x34);
  TyFn* pop2 = tyFn(Kern_findTy(k, SLC("pop2")));
  TASSERT_SLC_EQ("c" , CStr_asSlc(pop2->inp->name));
  TASSERT_SLC_EQ("b" , CStr_asSlc(pop2->inp->next->name));
  TASSERT_SLC_EQ("a" , CStr_asSlc(pop2->inp->next->next->name));
  TASSERT_EQ(&Ty_S, pop2->inp->ty);
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

  EXPECT_ERR(TY_CHECK(&TyIs_S, &TyIs_SS, true));
  EXPECT_ERR(TY_CHECK(&TyIs_U4x2, &TyIs_rU1_U4, true));

  TyDb* db = &k->g.tyDb;

  TyDb_new(db);

  // Call a function which returns [S], the type gets put on
  tyCall(k, db, NULL, &TyIs_S); TY_CHECK(&TyIs_S, TyDb_top(db), true);

  // Now call a function which consumes [S], the type stack empties
  tyCall(k, db, &TyIs_S, NULL); TY_CHECK(NULL, TyDb_top(db), true);

  // ret[done] causes errors on future operations
  k->g.curTy = Kern_findTy(k, SLC("+"));
  tyCall(k, db, NULL, &TyIs_S);
  tyRet(k, db, true);  TASSERT_EQ(true, TyDb_done(db));
  EXPECT_ERR(tyCall(k, db, &TyIs_S, NULL));
  END_LOCAL_TYDB_BBA(tyDb);
END_TEST_FNGI

TEST_FNGI(compileTy, 6)
  Kern_fns(k);
  REPL_START
  COMPILE_EXEC("fn pop2    a:U1 b:U2 c:S -> \\a:U1  do (a)");
  COMPILE_EXEC("fn pop2_ stk:U1 b:U2 c:S -> \\a:U1  do (_)");
  COMPILE_EXEC("fn add   stk:S  b:S      -> \\sum:S do (_ + b)");
  COMPILE_EXEC("add(42, 7)"); TASSERT_WS(49);
  REPL_END
END_TEST_FNGI

#define TY_LEN  Sll_len((Sll*)TyDb_top(&k->g.tyDb))
TEST_FNGI(compileIf, 10)
  Kern_fns(k);
  REPL_START
  k->g.fnState |= C_UNTY;

  COMPILE_EXEC("if(1) do ;");                              TASSERT_EQ(0, Stk_len(WS));
  COMPILE_EXEC("if(1) do 0x42 else 0x33");                 TASSERT_WS(0x42);
  COMPILE_EXEC("if(0) do 0x42 else 0x33");                 TASSERT_WS(0x33);
  COMPILE_EXEC("if(1) do 0x42 elif(1) do 0x11 else 0x33"); TASSERT_WS(0x42);
  COMPILE_EXEC("if(0) do 0x42 elif(1) do 0x11 else 0x33"); TASSERT_WS(0x11);
  COMPILE_EXEC("if(0) do 0x42 elif(0) do 0x11 else 0x33"); TASSERT_WS(0x33);
  TASSERT_EQ(0, Stk_len(WS));
  TASSERT_EQ(0, TY_LEN);

  k->g.fnState = (~C_UNTY) & k->g.fnState;
  TASSERT_EQ(0, TY_LEN); TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC("if(1) do ;");
  TASSERT_EQ(0, TY_LEN); TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC("if(1) do 0x42 else 0x33"); TASSERT_EQ(1, TY_LEN);
  COMPILE_EXEC("tAssertEq 0x42");          TASSERT_EQ(0, TY_LEN);

  COMPILE_EXEC("fn ifRet stk:S -> S do ( if(_) do (0x22 ret;) else 0x33 )")
  COMPILE_EXEC("tAssertEq(0x22, ifRet(1))");
  COMPILE_EXEC("tAssertEq(0x33, ifRet(0))");

  COMPILE_EXEC("fn ifElifRet x:S -> S do ("
               "  if(  x == 0x42) do (x ret;)"
               "  elif(x == 0x11) do (0 ret;)"
               "  else 0x33"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifRet(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifRet(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifRet(7))");

  COMPILE_EXEC("fn ifElifRetMid x:S -> S do ("
               "  if  (x == 0x42) do (0x42)"
               "  elif(x == 0x11) do (0 ret;)"
               "  else 0x33"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifRetMid(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifRetMid(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifRetMid(7))");

  COMPILE_EXEC("fn ifElifStk stk:S -> S do ("
               "  if  (dup; == 0x42) do (_)"
               "  elif(dup; == 0x11) do (drp; 0 ret;)"
               "  else (drp; 0x33)"
               ")")
  COMPILE_EXEC("tAssertEq(0x42, ifElifStk(0x42))");
  COMPILE_EXEC("tAssertEq(0,    ifElifStk(0x11))");
  COMPILE_EXEC("tAssertEq(0x33, ifElifStk(7))");


  #define IF_ALL_RET \
    "if(0) do (0 ret;) elif(0) do (0 ret;) else (1 ret;)"

  COMPILE_EXEC("fn one -> S do (" IF_ALL_RET ")");
  COMPILE_EXEC("tAssertEq(1,    one;)");
  COMPILE_EXEC("fn ifRet1 -> S do ( if(1) do ret 2; ret 4; )");
  COMPILE_EXEC("tAssertEq(2, ifRet1,)");

  // putting anything after fails, since the if/elif/else block all RET
  // this does panic, but there is some kind of memory error in dropping.
  // EXPECT_ERR(COMPILE_EXEC("fn bad -> S do (" IF_ALL_RET "4 )"));
  // TyDb_drop(k); // panic means cleanup wasn't handled

  REPL_END
  TASSERT_EQ(0, Stk_len(&k->g.tyDb.tyIs));
END_TEST_FNGI

TEST_FNGI(compileBlk, 10)
  Kern_fns(k);
  REPL_START
  TASSERT_EQ(0, k->g.fnState & C_UNTY);

  COMPILE_EXEC(
      "0 blk(\n"
      "  if(dup, >= 5) do (drp; brk 0x15)\n"
      "  inc; cont;\n"
      ")"); TASSERT_WS(0x15);

  REPL_END
  TASSERT_EQ(0, Stk_len(&k->g.tyDb.tyIs));
END_TEST_FNGI

TEST_FNGI(compileVar, 10)
  Kern_fns(k); REPL_START;
  COMPILE_EXEC(
      "fn useVar stk:S -> S do (\n"
      "  var a: S = (_ + 7);  ret inc(a);\n"
      ") useVar(0x42)"); TASSERT_WS(0x4A);

  COMPILE_EXEC("fn idenRef a:&S -> &S do ( a )\n");
  COMPILE_EXEC("fn ftRef   a:&S -> S  do ( @a )\n");
  TyFn* ftRef = (TyFn*) Kern_findTy(k, SLC("ftRef"));
  TASSERT_EQ(1, ftRef->inp->meta);
  TASSERT_EQ(0, ftRef->out->meta);

  COMPILE_EXEC("fn useRef a:S -> S  do ( ftRef(&a) )\n");
  COMPILE_EXEC("useRef(0x29) tAssertEq(0x29)")
  COMPILE_EXEC("fn getRefs a:S -> &S &S do ( var b:U4; &a, &b )\n");
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
      "fn chTy stk:S -> U2 do ( ret U2; )"
      "chTy(0x42)"); TASSERT_WS(0x42);

  COMPILE_EXEC("struct Foo [ a: U2; b: U1; c: U4 ]");
  TyDict* Foo = (TyDict*) Kern_findTy(k, SLC("Foo"));
  assert(Foo);
  TASSERT_EQ(TY_DICT | TY_DICT_STRUCT, Foo->meta);
  TASSERT_EQ(8, Foo->sz);
  assert(TyDict_find(Foo, SLC("a")));

  COMPILE_EXEC("struct A [ a: S ]");
  COMPILE_EXEC("struct B [ a: A; b: S ]");
  COMPILE_EXEC(
    "fn simpleCreate a:S -> B do ("
    "  B(A(a), 0x42)"
    "); simpleCreate(0x33)"); TASSERT_WS(0x42); TASSERT_WS(0x33);

  COMPILE_EXEC("destruct(simpleCreate(0x35)) tAssertEq(0x42)")
  TASSERT_WS(0x35);
  TASSERT_EQ(0, Stk_len(WS));

  COMPILE_EXEC(
    "fn useField a:A -> S do ( a.a + 7 )"
    "  tAssertEq(0x18, useField(A 0x11))");

  COMPILE_EXEC("fn getStruct b:B -> B do ( b ); getStruct(B(A 0x4321, 0x321))");
  TASSERT_WS(0x321); TASSERT_WS(0x4321);
  REPL_END
END_TEST_FNGI

TEST_FNGI(structBrackets, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ a1: S, a2: S ]");
  COMPILE_EXEC("struct B [ b1: A, b2: S ]");
  COMPILE_EXEC("fn buildA -> A do ( var a: A = { a1 = 0x33  a2 = 0x22 } a )")
  COMPILE_EXEC("buildA()"); TASSERT_WS(0x22); TASSERT_WS(0x33);
  COMPILE_EXEC("fn buildB -> B do ( var b: B = {\n"
               "  b1 = { a1 = 0x35  a2 = 0x25 }\n"
               "  b2 = 0x11\n"
               "} b )")
  COMPILE_EXEC("buildB()"); TASSERT_WS(0x11); TASSERT_WS(0x25); TASSERT_WS(0x35);
  COMPILE_EXEC("fn buildBwA -> B do ( var b: B = {\n"
               "  b1 = buildA()\n"
               "  b2 = 0x11\n"
               "} b )")
  COMPILE_EXEC("buildBwA()"); TASSERT_WS(0x11); TASSERT_WS(0x22); TASSERT_WS(0x33);


  REPL_END
END_TEST_FNGI

TEST_FNGI(global, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("var a:S = 32");
  TyVar* a = tyVar(Kern_findTy(k, SLC("a")));
  TASSERT_EQ(0, Stk_len(WS));
  TASSERT_EQ(32, ftSzI((U1*)a->v, SZR));
  COMPILE_EXEC("imm#tAssertEq(32, a)");
  COMPILE_EXEC("tAssertEq(32, a)");

  COMPILE_EXEC("struct Foo [ a: S; b: S; c: S]");
  COMPILE_EXEC("var foo:Foo = Foo(7, 3, 12)");
  TASSERT_EQ(0, Stk_len(WS));
  TyVar* foo = tyVar(Kern_findTy(k, SLC("foo")));
  TASSERT_EQ(7, ftSzI((U1*)foo->v, SZR));

  COMPILE_EXEC("tAssertEq(7, foo.a)   tAssertEq(3, foo.b)");
  COMPILE_EXEC("imm#tAssertEq(7, foo.a)");
  COMPILE_EXEC("foo.a = 0x444 tAssertEq(0x444, foo.a)");
  COMPILE_EXEC("imm#( tAssertEq(0x444, foo.a); tAssertEq(3, foo.b) )");
  COMPILE_EXEC("assertWsEmpty;");
  REPL_END
END_TEST_FNGI

TEST_FNGI(mod, 10)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("mod foo ( fn one -> S do 1 )");
  COMPILE_EXEC("imm#tAssertEq(1, foo.one())");
  COMPILE_EXEC("loc:foo ( imm#tAssertEq(1, one()) )");
  REPL_END
END_TEST_FNGI

TEST_FNGI(structDeep, 12)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ a: S ]");
  COMPILE_EXEC("struct B [ a: A; b: S ]");
  COMPILE_EXEC("struct C [a: &A, b: &B]")
  COMPILE_EXEC("fn cGetA c:&C -> S do ( c.b.a.a );");
  COMPILE_EXEC("fn useC -> S S do (\n"
               "  var a: A = A 1\n"
               "  var b: B = B(A 2, 3)\n"
               "  var c: C = C(&a, &b)\n"
               "  c.b.a.a, cGetA(&c)\n"
               ")");
  COMPILE_EXEC("useC;");
    TASSERT_WS(2); TASSERT_WS(2);

  COMPILE_EXEC("fn assign -> S do (\n"
               "  var a: A;\n"
               "  a.a = 0x42\n"
               "  a.a\n"
               ")");
  COMPILE_EXEC("assign()"); TASSERT_WS(0x42);

  COMPILE_EXEC("fn assignRef a:&A do (a.a = 0x44)")
  COMPILE_EXEC("fn useAssignRef -> S do (var a:A; assignRef(&a); a.a)");
  COMPILE_EXEC("useAssignRef()"); TASSERT_WS(0x44);
  REPL_END
END_TEST_FNGI

TEST_FNGI(method, 20)
  Kern_fns(k); REPL_START
  COMPILE_EXEC("struct A [ v:S; meth aDo self: &A, x: S -> S do ( self.v + x ) ]")
  COMPILE_EXEC("fn callADo x:S a:A -> S do ( a.aDo(x) )");
  COMPILE_EXEC("tAssertEq(8, callADo(3, A 5)) assertWsEmpty;");

  COMPILE_EXEC("loc:A( fn nonMeth x:S -> S do ( 7 + x ) )")
  COMPILE_EXEC("fn callNonMeth x:S a:A -> S do ( a.nonMeth(x) )");
  COMPILE_EXEC("tAssertEq(10, callNonMeth(3, A 1)) assertWsEmpty;");

  COMPILE_EXEC("var a:A = A 5");
  COMPILE_EXEC("tAssertEq(5, a.v)");
  COMPILE_EXEC("tAssertEq(13, a.aDo(8))");
  COMPILE_EXEC("tAssertEq(14, a.nonMeth(7))");

  COMPILE_EXEC("imm#tAssertEq(13, a.aDo(8))");
  COMPILE_EXEC("imm#tAssertEq(14, a.nonMeth(7))");
  REPL_END
END_TEST_FNGI

TEST_FNGI(file_basic, 20)
  Kern_fns(k);
  N_assertWsEmpty(k);
  CStr_ntVar(path, "\x0E", "tests/basic.fn");
  compilePath(k, path);
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
  test_structBrackets();
  test_global();
  test_mod();
  test_structDeep();
  test_method();
  test_file_basic();
  eprintf("# Tests complete\n");

  if(repl) test_repl();

  return 0;
}
