#ifndef __CIV_UNIX_H
#define __CIV_UNIX_H

#include <fcntl.h>  // creat, open
#include "./civ.h"

#define File_FD(F)      ((~File_INDEX) & (F).fid)

#define TEST_UNIX(NAME, numBlocks)  TEST(NAME)  \
    jmp_buf localErrJmp, expectErrJmp; \
    Fiber __fb; \
    Block* __blocks = malloc(numBlocks << BLOCK_PO2); \
    initCivUnix(&__fb, &((BANode*)__blocks)[1], &__blocks[1], numBlocks - 1); \
    civ.fb->errJmp = &localErrJmp;     \
    BA_init(&civ.ba); \
    if(setjmp(localErrJmp)) { civ.civErrPrinter(); exit(1); }


#define END_TEST_UNIX \
    free(__blocks); END_TEST

void initCivUnix(Fiber* fb, BANode* nodes, Block* blocks, U1 numBlocks);

#define File_RDWR      O_RDWR
#define File_RDONLY    O_RDONLY
#define File_WRONLY    O_WRONLY
#define File_TRUNC     O_TRUNC

File File_malloc(U4 bufCap);
void File_readAll(File* f);

int File_handleErr(File* f, int res);
bool File_drop(File* f);
void File_open(File* f, Slc s, Slot options);
void File_close(File* f);
void File_read(File* f);
RFile File_asRFile(File* d);

#endif // __CIV_UNIX_H
