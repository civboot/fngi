
#include "./fngi.h"

FNGI_TEST(init, 10)
  TASSERT_EQ(WS_DEPTH, WS->cap);
  TASSERT_EQ(WS->sp,   WS->cap);

  WS_ADD(4); TASSERT_WS(4);
END_TEST

int main() {
  Slc s = Slc_ntLit("World");
  eprintf("Hello %.*s!\n", Dat_fmt(s));

  test_init();
  return 0;
}
