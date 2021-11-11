#include <stdio.h>
#include <stdint.h>

typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;


U8 one = 1;
U32 negOne = 0xFF;

int main() {
  U32 a = (U32) negOne;
  I32 b = (I32) negOne;
  I32 c = (I32) ((I8) negOne);
  U32 d = (U32) ((U32) negOne);
  U32 e = (U8) -((I8) one);
  // a: 255  b:255  c:-1
  printf("a: %i  b:%i  c:%i  d:%x  e:%x\n", a, b, c, d, e);
}
