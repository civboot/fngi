#include "civ/civ.h"
#include <execinfo.h>

#define BT_SIZE   128
void printTrace() {
  void*    array[BT_SIZE];
  int      size    = backtrace(array, BT_SIZE);
  char**   strings =  backtrace_symbols(array, size);
  assert(strings); // No debug symbols found
  for(int i = 0; i < size; i++) printf("%s\n", strings[i]);
  free(strings);
}

U1* szName(U1 szI) {
  switch(szI) {
    case SZ1: return "SZ1";
    case SZ2: return "SZ2";
    case SZ4: return "SZ4";
  }
  return NULL;
}
