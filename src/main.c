
#include "./fngi.h"

int main() {
  Slc s = Slc_ntLit("World");
  eprintf("Hello %.*s!\n", Dat_fmt(s));
  return 0;
}
