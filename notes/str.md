# How to format strings

A StrBuf is really just a subtype of Buf[U1] -- that's easy enough.

C++'s method of string formatting is probably best here:

```
StrBuf s = StrBuf.withCap(allocTop(), 128);
s << |Hello | << name << | your age is | << age;
```

What this _really_ gets down to is not strings, but methods.

## Methods

Methods are a sub-type of `TY_FN`
