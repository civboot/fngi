#ifndef __KERNEL_H
#define __KERNEL_H
#include <stdint.h>
#include "constants.h"
#include "../linux/types.h" // TODO: support more than just linux

typedef U2 FErr;

// If F_INDEX is set, fid is a file descriptor.
// Else it is a "mock" BufPlc
#define F_INDEX    (1 << (RSIZE * 4 - 1))
#define F_FD(F)    ((~F_INDEX) & (F).fid)
const U2 F_seeking  = 0x00;
const U2 F_reading  = 0x01;
const U2 F_writing  = 0x02;
const U2 F_stopping = 0x03;

const U2 F_done     = 0xD0;
const U2 F_stopped  = 0xD1;
const U2 F_eof      = 0xD2;

const U2 F_error    = 0xE0;
const U2 F_Eperm    = 0xE1;
const U2 F_Eio      = 0xE2;

#define F_plcBuf(F)  ((PlcBuf*) &(F).b)
typedef struct {
  U4 pos;   // current position in file. If seek: desired position.
  Ref fid;  // file id or reference
  Buf b;    // buffer for reading or writing data
  U2 plc;   // place, makes buf a PlcBuf. write: write pos.
  U2 code;  // status or error (F_*)
} File;

typedef void (*fileMethod)(File* f);

typedef struct {
  Ref open;    // open the file
  Ref close;   // immediately close the file
  Ref stop;    // stop current operation
  Ref seek;    // seek to pos
  Ref clear;   // clear all data after pos
  Ref read;    // read data from pos
  Ref insert;  // insert data at pos
} FileMethods;

typedef struct { FileMethods* m; File* f; } FileRole;

#define BLOCK_END  0xFF
#define BLOCK_PO2  12
#define BLOCK_SIZE (1<<BLOCK_PO2)
#define CSZ_CATCH 0xFF

typedef U1 Instr;

typedef struct { Ref ref; U2 sp; U2 cap; }                  Stk;
typedef struct { U1 previ; U1 nexti; }                      BANode;
typedef struct { Ref nodes; Ref blocks; U1 rooti; U1 cap; } BA;
typedef struct { Ref ba; U1 rooti; U2 len; U2 cap; }        BBA;
typedef struct { Ref root; Ref free; }                      Dict;
typedef struct { Ref l; Ref r; Ref ckey; U1 m0; U1 m1; U4 v; } DNode;

#define Stk_init(CAP, REF) (Stk) {.ref = REF, .sp = CAP, .cap = CAP}

typedef struct {
  U1 valTy;     U1 _align;  U2 _align2;
  U2 valueASz;
  U2 valueBSz;
  U4 valueA;
  U4 valueB;
  Ref msg;
} ErrData;

typedef struct {
  U4 _null;  Ref memTop;
  BA ba;
  BBA bbaPub;    BBA bbaPriv;
  Ref dict;
} Kern;

typedef struct {
  Ref next; Ref prev; // LL pointers to other fibers.
  Ref ep;           // execution pointer
  Stk ws; Stk ls;   // working and local stacks
  Stk cs; Stk csz;  // call and call size stacks
  Ref gb;           // globals base pointer
  U2 err;
} Fiber;

typedef struct {
  U2 glen; U2 gcap; // global data used and cap
  Ref fb; // current fiber
  U2 cstate;
  U1 logLvlSys;
  U1 logLvlUsr;
  Ref bbaPub;   Ref bbaPriv;
  Ref dictPub;  Ref dictPriv;
  Ref srcM;
  File src;
  U1 buf0[TOKEN_SIZE];

  int syserr;
} Globals;

#endif // __KERNEL_H
