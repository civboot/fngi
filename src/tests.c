
#include "./fngi.h"

FNGI_TEST(init, 10)
  TASSERT_EQ(WS_DEPTH, WS->cap);
  TASSERT_EQ(WS->sp,   WS->cap);

  WS_ADD(4); TASSERT_WS(4);
END_TEST

void N_add42(Kern* k) {
  eprintf("?? Native: Adding 42\n");
  WS_ADD(WS_POP() + 42);
}

// Helpers to execute functions
static inline void _xFn(Kern* k, TyFn fn) { executeFn(k, &fn); }
#define XFN(...)  _xFn(k, litFn(__VA_ARGS__))

FNGI_TEST(call, 1)
  // Running some code
  TyFn fiveFn = litFn((U1[]){SLIT + 2, SLIT + 3, ADD, RET}, 0, 0);
  executeFn(k, &fiveFn);       TASSERT_WS(5);

  // Calling a function
  Buf_var(call5, 16);
  Buf_add(&call5, XL); Buf_addBE4(&call5, (Slot)&fiveFn); Buf_add(&call5, RET);
  XFN(call5.dat, 0, 0);       TASSERT_WS(5);

  // Executing a native
  TyFn add42 = litFn((U1*)N_add42, TY_FN_NATIVE, 0);
  Buf_var(callAdd42, 16);
  Buf_add(&callAdd42, XL); Buf_addBE4(&callAdd42, (Slot)&add42); Buf_add(&callAdd42, RET);
  WS_ADD(3); XFN(callAdd42.dat, 0, 0);  TASSERT_WS(45);
END_TEST

#define TASSERT_TOKEN(T) \
  scanNext(f, b);        \
  TASSERT_SLC_EQ(T, *Buf_asSlc(b));

FNGI_TEST(scan, 1)
  Buf_var(_b, 32); Buf* b = &_b;
  BufFile_var(bf, 8, "  this-has(*) eight tokens");
  Reader f = File_asReader(BufFile_asFile(&bf));

  TASSERT_TOKEN("this");  TASSERT_TOKEN("-"); TASSERT_TOKEN("has");
  TASSERT_TOKEN("(");     TASSERT_TOKEN("*"); TASSERT_TOKEN(")");
  TASSERT_TOKEN("eight"); TASSERT_TOKEN("tokens");
END_TEST

int main() {
  Slc s = Slc_ntLit("World");
  eprintf("Hello %.*s!\n", Dat_fmt(s));

  test_init();
  test_call();
  test_scan();
  return 0;
}
