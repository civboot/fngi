
#include "./fngi.h"

FNGI_TEST(init, 10)
  TASSERT_EQ(WS_DEPTH, WS->cap);
  TASSERT_EQ(WS->sp,   WS->cap);

  WS_ADD(4); TASSERT_WS(4);
END_TEST

FNGI_TEST(call, 1)
  Slc five  = Slc_lit(SLIT + 2, SLIT + 3, ADD, RET);

  Buf_var(call5, 16);
  Buf_add(&call5, XL); Buf_addBE4(&call5, (Slot)five.dat);

  executeInstrs(k, five);  TASSERT_WS(5);
  // executeInstrs(call5.dat); TASSERT_WS(5);
END_TEST

int main() {
  Slc s = Slc_ntLit("World");
  eprintf("Hello %.*s!\n", Dat_fmt(s));

  test_init();
  test_call();
  return 0;
}
