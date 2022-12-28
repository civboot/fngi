
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
  k->g.src = File_asReader(BufFile_asFile(&bf));

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
  Reader f = File_asReader(BufFile_asFile(&bf));
  k->g.src = f;
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
  COMPILE_EXEC("answer");  TASSERT_WS(0x42);
  TASSERT_EMPTY();
END_TEST_FNGI

TEST_FNGI(compile1, 5)
  Kern_fns(k);
  k->g.fnState |= C_UNTY;
  Ty* Ty_U2 = Kern_findTy(k, SLC("U2"));
  Ty* Ty_S  = Kern_findTy(k, SLC("S"));

  COMPILE_EXEC("pre fn maths do (_ + 7 + (3 * 5))  maths(4)"); TASSERT_WS(26);
  COMPILE_EXEC("1 \\comment \\(3 + 4) + 7"); TASSERT_WS(8)
  COMPILE_EXEC("fn maths2 stk:U2 -> U2 do (_ + 10)  maths2(6)"); TASSERT_WS(16);
  TyFn* maths2 = tyFn(Kern_findTy(k, SLC("maths2")));
  TASSERT_EQ(Ty_U2, maths2->inp->ty);  TASSERT_EQ(NULL , maths2->inp->name);
  TASSERT_EQ(Ty_U2, maths2->out->ty);  TASSERT_EQ(NULL , maths2->out->name);
  TASSERT_EQ(NULL, maths2->inp->next); TASSERT_EQ(NULL, maths2->out->next);

  COMPILE_EXEC("fn maths3 x:U2 -> U2 do (x + 4) maths3(3)"); TASSERT_WS(7);
  TyFn* maths3 = tyFn(Kern_findTy(k, SLC("maths3")));
  TASSERT_EQ(Ty_U2, maths3->inp->ty);
  TASSERT_SLC_EQ("x" , CStr_asSlc(maths3->inp->name));
  TASSERT_EQ(Ty_U2, maths3->out->ty);  TASSERT_EQ(NULL , maths3->out->name);
  TASSERT_EQ(NULL, maths3->inp->next); TASSERT_EQ(NULL, maths3->out->next);
  TASSERT_EQ(1, maths3->lSlots);

  COMPILE_EXEC("fn pop2 a:U1 b:U2 c:S -> \\a:S do (a); pop2(0x1234 2 3)");
  TASSERT_WS(0x34);
  TyFn* pop2 = tyFn(Kern_findTy(k, SLC("pop2")));
  TASSERT_SLC_EQ("c" , CStr_asSlc(pop2->inp->name));
  TASSERT_SLC_EQ("b" , CStr_asSlc(pop2->inp->next->name));
  TASSERT_SLC_EQ("a" , CStr_asSlc(pop2->inp->next->next->name));
  TASSERT_EQ(Ty_S, pop2->inp->ty);
  TASSERT_EQ(2, pop2->lSlots);

  COMPILE_EXEC("tAssertEq(42, 42)");
  COMPILE_EXEC("tAssertEq(0x10 + 3, 0x13)");
  COMPILE_EXEC("tAssertEq(1 shl 4, 0x10)");
  COMPILE_EXEC("tAssertEq(0x10 shr 4, 1)");
END_TEST_FNGI

TEST_FNGI(tyDb, 4)
  Kern_fns(k);

  TyI_printAll(&TyIs_S);      eprintf("\n");
  TyI_printAll(&TyIs_U4_rU4); eprintf("\n");
  TY_CHECK(&TyIs_S, &TyIs_S,  false);
  TY_CHECK(&TyIs_S, &TyIs_S,  true);
  TY_CHECK(&TyIs_S, &TyIs_SS, false);

  EXPECT_ERR(TY_CHECK(&TyIs_S, &TyIs_SS, true));
  EXPECT_ERR(TY_CHECK(&TyIs_U4x2, &TyIs_U4_rU4, true));

  TyDb* db = &k->g.tyDb;

  TyDb_new(db, NULL);

  // Call a function which returns [S], the type gets put on
  tyCall(k, NULL, &TyIs_S); TY_CHECK(&TyIs_S, TyDb_top(db), true);

  // Now call a function which consumes [S], the type stack empties
  tyCall(k, &TyIs_S, NULL); TY_CHECK(NULL, TyDb_top(db), true);

  // ret[done] causes errors on future operations
  k->g.curTy = Kern_findTy(k, SLC("+"));
  tyCall(k, NULL, &TyIs_S);
  tyRet(k, true);  TASSERT_EQ(true, TyDb_done(k));
  EXPECT_ERR(tyCall(k, &TyIs_S, NULL));

END_TEST_FNGI

TEST_FNGI(compileTy, 6)
  Kern_fns(k);
  eprintf("?? compiling typed pop2\n");
  COMPILE_EXEC("fn pop2    a:U1 b:U2 c:S -> \\a:U1 do (a)");
  COMPILE_EXEC("fn pop2_ stk:U1 b:U2 c:S -> \\a:U1 do (_)");
  // Not yet working because locals is wrong.
  // Locals tyI needs to be a clone of the one used in the fn signature,
  // otherwise the next-chain polutes it.
  // COMPILE_EXEC("fn add   stk:S b:S       -> \\sum:S do (_ + b)");

END_TEST_FNGI

TEST_FNGI(repl, 20)
  Kern_fns(k);
  simpleRepl(k);
END_TEST_FNGI

int main(int argc, char* argv[]) {
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
  test_tyDb();
  test_compileTy();
  eprintf("# Tests complete\n");

  if(repl) test_repl();

  return 0;
}
