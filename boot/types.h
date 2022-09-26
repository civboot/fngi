#ifndef __TYPES_H
#define __TYPES_H

#define RSIZE       4
#define SZR         SZ4
#define REF_MAX     UINT32_MAX
#define WS_DEPTH    16
#define CS_DEPTH    128
#define TOKEN_SIZE  128

typedef uint8_t              U1;
typedef uint16_t             U2;
typedef uint32_t             U4;
typedef uint32_t             UA;
typedef int8_t               I1;
typedef int16_t              I2;
typedef int32_t              I4;
typedef uint32_t             APtr;
typedef uint32_t             Ref;
typedef Ref                  Slot;

typedef struct { Ref dat; U2 len; }                         Slc;
typedef struct { Ref dat; U2 len; U2 cap; }                 Buf;
typedef struct { Ref dat; U2 len; U2 cap; U2 plc; }         PlcBuf;

#endif // __TYPES_H
