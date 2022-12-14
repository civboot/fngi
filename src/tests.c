
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

  TyFn_static(fiveFn, 0, 0, (Slot)((U1[]){SLIT + 2, SLIT + 3, ADD, RET}) );
  executeFn(k, &fiveFn);       TASSERT_WS(5);

  // Calling a function
  Buf_var(call5, 16);
  Buf_add(&call5, XL); Buf_addBE4(&call5, (Slot)&fiveFn); Buf_add(&call5, RET);
  XFN(call5.dat, 0, 0);       TASSERT_WS(5);

  // Executing a native
  TyFn_static(add42, TY_FN_NATIVE, 0, kFn(N_add42));
  Buf_var(callAdd42, 16);
  Buf_add(&callAdd42, XL); Buf_addBE4(&callAdd42, (Slot)&add42); Buf_add(&callAdd42, RET);
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


TEST_FNGI(compile0, 4)
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
  TyFn* answer = tyFn(Kern_findTy(k, Slc_ntLit("answer")));
  executeFn(k, answer);    TASSERT_WS(0x42);
  COMPILE_EXEC("answer");  TASSERT_WS(0x42);
  TASSERT_EMPTY();
END_TEST_FNGI

TEST_FNGI(compile1, 4)
  Kern_fns(k);
  COMPILE_EXEC("pre fn maths do (_ + 7 + (3 * 5))  maths(4)"); TASSERT_WS(26);
  COMPILE_EXEC("1 \\comment \\(3 + 4) + 7"); TASSERT_WS(8)
END_TEST_FNGI

TEST_FNGI(repl, 20)
  Kern_fns(k);
  simpleRepl(k);
END_TEST_FNGI

int main() {
  eprintf("# Running tests\n");
  test_basic();
  test_init();
  test_call();
  test_scan();
  test_compile0();
  test_compile1();
  eprintf("# Tests complete\n");

  // test_repl();

  return 0;
}
