# Fngi details

Fngi is a general purpose concrete syntax[1] language with similar constraints
to C. Unlike C it has a rich macro system allowing for rich syntax extensions.
It targets a bare-bones virtual machine called [spor](./spor.md).

[1] a "concrete syntax" is (tounge-in-cheek) the opposite of an "abstract
syntax". It is defined as a syntax where the compiler only reads a token stream
and never creates an abstract syntax tree of any kind.

## Fngi basics:

"Single tokens": these tokens are never grouped together

* `( ... )`: groups an execution.
* `   %   `: compile next token using spore asm. Can be `%( ... )`
* `   $   `: run next token instantly.           Can be `$( ... )`
* `  'c'  `: push literal character value on stack
* `   .   `: enter name compiler (.foo = myFn(3))

Otherwise a token is a group of:
* alphanumeric characters (including `_`), i.e. `foo_bar123`
* symbol characters not in the above "single" tokens, i.e. `+-*&`

Functions can be defined for any token that is not purely numeric
* Numerics: `0x123 (hex)  12345 (decimal)  0b110101 (binary)`
* Valid function names:  `+++  0b12 (not binary)  123xx (not decimal)`

Fngi is a stack based language. Most stack based languages (i.e. FORTH) use
postfix notation, also called reverse polish notation. However, most "modern"
(and more readable) languages use prefix notation, also called standard polish
notation. Fngi uses mostly prefix notation. Here are two examples to add 1 and
2:

- prefix: `add(1, 2)`
- postfix: `1 2 add`

In fngi, functions that accept arguments are almost always labeled as PRE
meaning they use prefix notation (the exception is purely stack-altering
functions like dup, ovr, drp and a few compiler-only functions). Functions
that don't accept arguments use postfix notation (they don't need `()`).

However, unlike C or most languages that have an abstract syntax tree,
fngi goes about this in a different way. PRE functions work by compiling
only the next token, then compling themselves.

This means: `myFn 1`

Gets compiled as: `#1$L0 $xsl myFn`

> Note: this is spor assembly. #1 is the hex number, `$L0` compiles it as a "0
> byte" literal (fits in the instr) and  xsl means "execute small". See
> [spor.md](./spor.md) for more details.

So how do we pass more than one argument? It's easy, we use parenthesis!
`(` will continue to compile tokens until it hits `)`

So we can do: `myFn2(1 2 + 3)  // in C: myFn(1, 2 + 3)`

Fngi has one more element used for documentation (and a few syntax-fns, which
we'll get to). It defines `,` and `;` as "instantly do nothing".

This allows you to write: `myFn(1, 2 + 3)  // , to visually separate args`

The comma doesn't actually do anything, it's more like a comment to help
people be able to read your code. Feel free to omit it.

There's one more "instant do nothing" function, `_`. By convention this means
"use the stack".

So you might write: `myFn(_, 2 + 3)  // first arg is from stack`

Again the `_` does absolutely nothing except communicate to the programmer than
an argument is from the stack.

Below is an example fngi function to calculate fibronacci recursively. Also
included is the equivalent C code.

```
fn fib [n:U4] -> U4 do (
  if(.n <= 1) do ret 0;
  ret fib(.n - 1) + fib(.n - 1);
)
```

Let's explain a few details on how the parser/compiler stays extremely minimal:

- `fn` is an SMART function, meaning the compiler executes it instead of
  compiling it to the heap. It compiles the next token/s using the
  `c_fnInputOutput` function which is called a syntax-fn since it reads tokens
  and constructs a small abstract syntax tree. The `fn` syntax-fn expects the
  syntax to be structured as follows: 
    `fn [fn-name] [inputs] -> [outputs] do [code]`
- `(..)` is NOT an expression. `(` is simply an instant function that compiles
  until it encounters the token `)`. In this case, what is between them is the
  function body.
- `if` is another smart syntax-fn which expects syntax of the form
  `if [check] do [code]`. It can also peek, if the next token is `elif` or
  `eldo` then it can look like:
  `if [check] do [code] elif [check] do [code] ... eldo [code]`
- `.` is another syntax-fn that does "variable" compilation. It is used for
  fetching and storing variables of all types (globals, inputs or locals),
  as well as references, module lookups, etc.
- If a token can be interpreted as a number, then it is a number. Otherwise it's
  treated as a function (which might be SMART/INSTANT).

# Similarities to FORTH
Like FORTH, fngi allows you to define functions that are either compiled or
run instantly (affecting compilation, often called "macros" in other
languages). Also like FORTH, fngi's functions operate by push/poping from a
working stack and uses a call stack to track calls/returns.  In addition, both
FORTH and fngi have very low requirements for implementation.  Fngi is
implemented in about 1000 lines of spor assembly, and spor is implemented in
about 1000 lines of C. Like FORTH, fngi/spor do not come with memory management
or any other bells or whistles. They practically bootstraps themselves from
nothing.

Spore diverges from FORTH in the following ways:
- Does not follow FORTH naming standards in any way. Typically closer to
  C naming.
- Although it is stack-based, all functions (except those with no stack inputs
  or pure stack manipulation like dup/drp/ovr) cause the next token to be
  compiled before them. This means that syntax is typically written in C like
  standard-polish notation instead of FORTH's (hard to read) reverse-polish
  notation.
- Far more inclusive support of locals. Has a separate locals stack and
  execution mechanisms which automatically allocate space for locals when
  executed and deallocate when returned.
- Has less dependence on the working stack (data stack in FORTH).

```
// Fngi Syntax
fn fib [n:U4] -> U4 do (
  if(.n <= 1) do ret 0;
  ret fib(.n - 1) + fib(.n - 1);
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
the above is basically no more complicated than FORTH's.
