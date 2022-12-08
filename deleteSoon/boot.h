#ifndef __BOOT_H
#define __BOOT_H

#include "../civ/civ_unix.h"

typedef struct {
  U2 glen; U2 gcap; // global data used and cap
  U2 metaNext; // meta of next fn
  U2 cstate;
  U1 fnState;
  U1 localOffset;
  U1 logLvlSys;
  U1 logLvlUsr;
  DNode* curNode; // current node (fn, struct) being compiled
  void* compFn;   // current function that does compilation
  BBA  bbaLocal;  DNode* dictLocal;
  BBA* bbaPub;
  BBA* bbaPriv;   DNode* dictPriv;
  Stk dictStk;
  File src; RFile srcR;
  U1 buf0[TOKEN_SIZE];
  DNode* dictBuf[DICT_DEPTH];
} Globals;

typedef struct {
  Fiber fb;
  U1* ep;             // execution pointer
  Stk ws; Stk ls;     // working and local stacks
  Stk cs; Stk1 csz;   // call and call size stacks
  Globals* g;         // globals base pointer
} FnFiber;

#endif // __BOOT_H
