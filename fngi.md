# Fngi details

Fngi is bootstrapped from [spor](./spor.md) and can use spore assembly inline.

For both spore and fngi there are three token groups. 
- special single characters: `~ ' $ . ( )`
- alphanumeric characters (case sensitive): `0-9 a-b A-Z`
- other symbols
- whitespace will separate any tokens.

Examples:
- `**abc` is two tokens, `**` and `abc`
- `$$abc` is three tokens`$`, `$` and `abc`. This is because `$` is a "special
  single character" and can only have a token length of 1.

> Spore's use of token groups is probably the primary philosophical divergence
> from FORTH.

Like FORTH, spore allows you to define functions that are either compiled or
run instantly (affecting compilation, often called "macros" in other
languages). Also like FORTH, spore's functions operate by push/poping from a
working stack, while function calls use a call stack to track address changes.

Spore diverges from FORTH in the following ways:
- Does not follow FORTH naming standards in any way. Typically closer to
  C-naming conventions.
- Although it is stack-based, all functions (except those with no stack inputs)
  cause the next token to be compiled before them. This means that syntax is
  typically written in C-like polish notation instead of FORTH's reverse-polish
  notation.
- Far more inclusive support of locals, with a separate locals stack and
  execution mechanisms which automatically allocate space for locals when
  executed and deallocate when returned.

Example spore function to calculate fibronacci recursively. Also included
is the equivalent C code.

```
// Fngi Syntax
fn fib [n:U4] -> [U4] do (
  if(.n <= #1) do ret #0
  ret fib(.n - #1) + fib(.n - #1)
)

// C Syntax
#include <stdint.h>
uint32_t fib(uint32_t n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}
```

For those familiar with FORTH the above will seem like syntax that _must_
require a far-too complicated parser. The truth is the opposite: the parser for
the above is basically no more complicated than FORTH's. Let's explain a few
details on how the parser/compiler stays extremely minimal:

- `fn` is an INSTANT function. It compiles the next token using the
  "fnInputOutput" compiler (which is just a function), then expects `do` for
  syntactic surgar, then compiles the next token (note: just _1 token_, not some
  kind of special-purpose syntax) as the function body. This is where `()` come
  in.
- `(..)` is NOT an expression. `(` is simply an instant function that compiles
  until it encounters the token `)`. In this case, what is between them is the
  function body.
- `if` is an instant function that compiles the next token to determine the if
  clause, then it expects `do` for sugar, then the next token to determines the
  body. It inserts appropraite `JZL` (jump zero literal) instructions around
  these. It can also handle `elif` and `else` tokens (not described here).
- `.` is an instant function that does "variable" compilation. It is used for
  fetching and storing variables of all types (globals, inputs or locals).
- `#` interprets the next token as a number, hex by default. `#m42` would be
  deci**m**al, `#b0011` would be **b**inary. `#x42` can be used to more
  explicitly specify hex.
