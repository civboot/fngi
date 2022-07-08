#include <stddef.h>
#include <stdio.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include "../kernel/kernel.h"

#define sizeofField(TYPE, FIELD)   sizeof(((TYPE*)0)->FIELD)

U1* header = (
"\\ @file kernel/offsets.sp\n"
"\\ DO NOT EDIT MANUALLY! THIS FILE WAS GENERATED BY: etc/make.py\n"
"\\\n"
"\\ @brief Defines global variable offsets.\n"
);

int writeto(U1* path) {
  int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 00600); assert(fd); return fd;
}

#define WRITE_FIELD(META, TYPE, PRE, FIELD, COMMENT) \
    assert(dprintf(fd,                         \
        "#%.2X   #%X=%-20s  \\ %s\n",          \
        offsetof(TYPE, FIELD), META,           \
                PRE #FIELD, COMMENT))

#define WRITE_INDEX(TYPE, PRE, FIELD, COMMENT) \
    assert(dprintf(fd,                         \
        "#%.2X   #0=%-20s  \\ %s\n",           \
        offsetof(TYPE, FIELD) / RSIZE,         \
                PRE #FIELD, COMMENT))

void main() {
  int fd = writeto("kernel/offsets.sp");
  assert(dprintf(fd, header) > 0);

  assert(dprintf(fd, "\n\\ struct Buf { ... }\n") > 0);
  WRITE_FIELD(0, Buf, "Buf_", dat, "Ref: data");
  WRITE_FIELD(0, Buf, "Buf_", len, "U2: length of buffer");
  WRITE_FIELD(0, Buf, "Buf_", cap, "U2: cap of buffer");

  assert(dprintf(fd, "\n\\ struct Kernel { ... }\n") > 0);
  WRITE_FIELD(0, Kern, "K_", memTop, "Ref: highest address in memory");
  WRITE_FIELD(0, Kern, "K_", ba, "BA struct: kernel BA");
  WRITE_FIELD(0, Kern, "K_", bbaPub, "BBA struct: kernel BBA");
  WRITE_FIELD(0, Kern, "K_", bbaPriv, "BBA struct: private BBA");
  WRITE_FIELD(0, Kern, "K_", dict, "&Dict: kernel dictionary");

  assert(dprintf(fd, "\n\\ struct Globals { ... }\n") > 0);
  WRITE_FIELD(TY_VAR | SZ2, Globals, "G_", glen, "U2");
  WRITE_FIELD(TY_VAR | SZ2, Globals, "G_", gcap, "U2");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", fb, "&Fiber: current fiber");
  WRITE_FIELD(TY_VAR | SZ2, Globals, "G_", cstate, "U2: compiler state");
  WRITE_FIELD(TY_VAR | SZ1, Globals, "G_", logLvlSys, "U1");
  WRITE_FIELD(TY_VAR | SZ1, Globals, "G_", logLvlUsr, "U1");
  WRITE_FIELD(TY_VAR | SZ2, Globals, "G_", metaNext, "U2: next function's meta");
  WRITE_FIELD(TY_VAR | SZ1, Globals, "G_", localOffset, "U1: current offset of locals");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", compFn, "Ref: function used for compiling");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", bbaLocal, "BBA: local BBA");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", dictLocal, "&DNode: local dict");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", bbaPub, "&BBA: current public bba");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", bbaPriv, "&BBA: current private bba");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", srcM, "&FileMethods: src file methods");
  WRITE_FIELD(TY_VAR | SZR, Globals, "G_", src, "&File: src File");

  assert(dprintf(fd, "\n\\ struct Fiber { ... }\n") > 0);
  WRITE_FIELD(0, Fiber, "Fb_", ws, "Stk struct: working stack");
  WRITE_FIELD(0, Fiber, "Fb_", gb, "&Globals: globals base pointer");
  WRITE_FIELD(0, Fiber, "Fb_", err, "U2: panic error");

  assert(dprintf(fd, "\n\\ struct File { ... }\n") > 0);
  WRITE_FIELD(0, File, "Fs_", buf, "Buf: buffer");
  WRITE_FIELD(0, File, "Fs_", plc, "U2: plc in buffer");
  WRITE_FIELD(0, File, "Fs_", code, "U2: file code");

  assert(dprintf(fd, "\n\\ BA { Ref nodes; Ref blocks; U1 rooti; U1 cap; }\n"));
  WRITE_FIELD(0, BA, "BA_", nodes, "&Node: start of nodes len cap*2");
  WRITE_FIELD(0, BA, "BA_", blocks, "&Block: start of 4k blocks len cap");
  WRITE_FIELD(0, BA, "BA_", rooti, "U1: root index");
  WRITE_FIELD(0, BA, "BA_", cap, "U1: number of nodes and blocks");

  assert(dprintf(fd, "\n\\ BBA { Ref ba; U1 rooti; U2 len; U2 cap; }\n"));
  WRITE_FIELD(0, BBA, "BBA_", ba, "&BA");
  WRITE_FIELD(0, BBA, "BBA_", rooti, "U1: owned block root index");
  WRITE_FIELD(0, BBA, "BBA_", len, "U2: unsigned heap");
  WRITE_FIELD(0, BBA, "BBA_", cap, "U2: signed topheap");
  WRITE_INDEX(BBAMethods, "BBAm_", bump, "method index");
  WRITE_INDEX(BBAMethods, "BBAm_", newBlock, "method index");
  WRITE_INDEX(BBAMethods, "BBAm_", drop, "method index");

  assert(dprintf(fd, "\n\\ struct DNode { ... }\n") > 0);
// typedef struct { Ref l; Ref r; Ref ckey; U2 m; U4 v; } DNode;
  WRITE_FIELD(0, DNode, "DN_", l, "Ref: left");
  WRITE_FIELD(0, DNode, "DN_", r, "Ref: right");
  WRITE_FIELD(0, DNode, "DN_", ckey, "Ref: counted data key");
  WRITE_FIELD(0, DNode, "DN_", m, "U2: meta");
  WRITE_FIELD(0, DNode, "DN_", v, "Ref: value, which may be a constant");

  assert(dprintf(fd, "\n") > 0); close(fd);
}
