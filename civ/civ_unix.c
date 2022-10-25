#include <unistd.h> // read, write, lseek

#include "./civ_unix.h"

#define File_FD(F)      ((~File_INDEX) & (F).fid)

// #################################
// # Core Types and common methods

void civErrPrinter() { eprintf("!! Error: %.*s\n", civ.fb->err.len, civ.fb->err.dat); }

bool CStr_varAssert(U4 line, U1* STR, U1* LEN) {
  if(1 != strlen(LEN)) {
    eprintf("ERROR CStr_var [line=%u]: LEN must be single byte (line=%u)");
    return false;
  }
  if(LEN[0] != strlen(STR)) {
    eprintf("ERROR CStr_var [line=%u]: Use LEN = \"\\x%.2X\"\n", line, strlen(STR));
    return false;
  }
  return true;
}


void initCivUnix(Fiber* fb, BANode* nodes, Block* blocks, U1 numBlocks) {
  civ.ba = (BA) {
    .nodes = nodes, .blocks = blocks,
    .rooti  = BLOCK_END, .cap = numBlocks,
  };
  civ.civErrPrinter = civErrPrinter;
  civ.fb = fb;
  fb->prev = fb; fb->next = fb;
  fb->err.len = 0;
}

// #################################
// # File
File File_malloc(U4 bufCap) {
  return (File) {
    .buf = (PlcBuf) { .dat = malloc(bufCap), .cap = bufCap },
    .code = File_CLOSED,
  };
}

int File_handleErr(File* f, int res) {
  if(errno == EWOULDBLOCK) { errno = 0; return res; }
  if(res < 0) { f->code = File_EIO; }
  return res;
}

void File_open(File* f, Slc path, Slot options) {
  assert(f->code == File_CLOSED);
  assert(path.len < 255);
  uint8_t pathname[256];
  memcpy(pathname, path.dat, path.len);
  pathname[path.len] = 0;
  int fd = File_handleErr(f, open(pathname, O_NONBLOCK, O_RDWR));
  if(fd < 0) return;
  f->pos = 0; f->fid = File_INDEX | fd;
  f->buf.len = 0; f->buf.plc = 0; f->code = File_DONE;
}

void File_close(File* f) {
  assert(f->code >= File_DONE);
  if(close(File_FD(*f))) f->code = File_ERROR;
  else                   f->code = File_CLOSED;
}

bool File_drop(File* f) {
  if(f->code != File_CLOSED) File_close(f);
  if(f->code != File_CLOSED) return false;
  Xr(civ.fb->arena, free, f->buf.dat, f->buf.cap, sizeof(Slot));
  return true;
}

void File_stop(File* f) { }
void File_seek(File* f, ISlot offset, U1 whence) {
  assert(f->code == File_READING || f->code >= File_DONE);
  File_handleErr(f, lseek(File_FD(*f), offset, whence));
}

void File_read(File* f) {
  assert(f->code == File_READING || f->code >= File_DONE);
  int len;
  if(!(File_INDEX & f->fid)) { // mocked file.
    PlcBuf* p = (PlcBuf*) f->fid;
    len = U4_min(p->len - p->plc, f->buf.cap - f->buf.len);
    memmove(f->buf.dat, p->dat + p->plc, len); p->plc += len;
  } else {
    f->code = File_READING;
    len = read(File_FD(*f), f->buf.dat + f->buf.len, f->buf.cap - f->buf.len);
    len = File_handleErr(f, len);  assert(len >= 0);
  }
  f->buf.len += len; f->pos += len;
  if(f->buf.len == f->buf.cap) f->code = File_DONE;
  else if (0 == len)           f->code = File_EOF;
}

void File_write(File* f) {
  assert(false);
}

// Read until the buffer is full or EOF.
// If the file-code is a DONE code then also clear the buf.len
void File_readAll(File* f) {
  if(f->code >= File_DONE) f->buf.len = 0;
  do { File_read(f); } while (f->code < File_DONE);
}

MFile mFile = (MFile) {
  .drop = &File_drop,
  .open  = &File_open,
  .close = &File_close,
  .stop =  &File_stop,
  .seek =  &File_seek,
  .read =  &File_read,
  .write = &File_write,
};

RFile File_asRFile(File* d) { return (RFile) { .m = &mFile, .d = d }; }
