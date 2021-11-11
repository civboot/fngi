#include <stdio.h>
#include <stdint.h>

typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;

U8 negOne = 0xFF;

int main() {
  U32 a = (U32) negOne;
  I32 b = (I32) negOne;
  I32 c = (I32) ((I8) negOne);
  // a: 255  b:255  c:-1
  printf("a: %i  b:%i  c:%i\n", a, b, c);
}
