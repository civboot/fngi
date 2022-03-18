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
* `   %   `: compile next token/s using spore asm. Can be `%( ... )`
* `   $   `: run next token/s instantly.           Can be `$( ... )`
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
notation. Fngi uses mostly standard notation. Here are two examples to add 1 and
2, both are valid fngi:

- prefix: `add(1, 2)`
- postfix: `1 2 add;`

In fngi, functions that accept arguments are almost always labeled as PRE
meaning they use prefix notation (the exception is purely stack-altering
functions: swp, drp, ovr, dup, dupn). Functions that don't accept arguments use
postfix notation (they don't need `()` or `;`). However, by convention they
are followed with `;`

Fngi has a few tokens defined for documentation/syntactic surgar:
* `,` does nothing and is intended as syntactic surgar (documentation) to
  separate arguments in functions.
* `;` does nothing and is intended to be consumed when no arguments are being
  passed to a PRE function, i.e. `ret;`
* `_` does nothing and is used to be consumed (and document) when a value is
  from the stack, i.e. `add(_, 2)`
* `\` is a swiss-army knife comment, described below. It can also be consumed
  and document values from the stack, i.e. `add(\a, 2)` or `\a + \b`

Unlike C or most languages that have an abstract syntax tree, fngi goes about
parsing in a different way. PRE functions work by compiling only the next token,
and then compling themselves.

This means: `myFn 1`

Gets compiled as: `#1$L0 $xsl myFn`

> Note: this is spor assembly. #1 is hex 0x1, `$L0` compiles it as a "0 byte"
> literal (fits in the instr) and  xsl means "execute small". See
> [spor.md](./spor.md) for more details.

So how do we pass more than one argument? It's easy, we use parenthesis!
`(` will continue to compile tokens until it hits `)`

So we can do: `myFn2(1, 2 + 3)  // in C: myFn(1, 2 + 3)`

### Comments
The ultimate "do nothing" token is the swiss-army knife comment: `\`

Comments have three forms:
* `\token` comments out a single token.
* `\ line comment` is a line comment
* `\(block comment)` is a block comment

So you might write one of the below:
* `myFn(_, 2 + 2)  \ "a" from the stack`
* `myFn(\a, 2 + 3)`
* `myFn(\(&a), 2 + 3)`

## Example

Below is an example fngi function to calculate fibronacci recursively. Also
included is the equivalent C code.

```
fn fib [n:U4] -> U4 do (
  if(.n <= 1) do ret 0;
  ret(fib(.n - 1) + fib(.n - 1));
)
```

Let's explain a few details on how the parser/compiler stays extremely minimal:

- `fn` is an SMART function, meaning the compiler executes it instead of
  compiling it to the heap.  The `fn` "syntax-fn" expects the syntax to be
  structured as follows: `fn [fn-name] [inputs] -> [outputs] do [code]`
- `(..)` is NOT an expression. `(` is simply an instant function that compiles
  until it encounters the token `)`. In this case, what is between them is the
  function body.
- `if` is another smart "syntax-fn" which expects syntax of the form
  `if [check] do [code]`. It can also peek at whether the next token is `elif`
  or `eldo` then it can look like: `if [check] do [code] elif [check] do [code]
  ... eldo [code]`
- `.` is another syntax-fn that does "variable" compilation. It is used for
  fetching and storing variables of all types (globals, inputs or locals),
  as well as references, module lookups, etc.
- If a token can be interpreted as a number, then it is a number. Otherwise it's
  treated as a constant or function (which might be SMART/INSTANT).

## Fngi "macros": SMART and INSTANT
Fngi allows any function to be run "instantly" at compile time. In spor this is
done using `$token` and it is the same in fngi.

In fngi, `$token(1, 2)` will run not only `token` instantly, but also it's args
(assuming `token` is PRE). 

> The mechanism by which this accomplished involves very little code and is a
> bit clever, see the definitions in `./spor.sp` if you are curious.

Some functions are defined as INSTANT, which means they will panic if executed
without `$` (or in a `$(...)` block).

Other functions are defined as SMART, and these are the true workhorses of fngi.
A SMART function is (1) always run instantly (2) it is passed a boolean
(`asInstant`) which indicates whether the user wanted it to be run instantly
(i.e. whether the user used `$`).

Some smart functions call `assertNoInstant(_)` right away, meaning they never
want to be called with `$`. These typically implement some syntax such as `fn`,
`if`, etc.

Other smart functions have different behavior whether called instantly or not.
For example `$(1 + 3)` will put `4` on the stack at compile time. However,
`1 + 3` will compile the literals and `%ADD` instruction into the function.

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
\ Fngi Syntax
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
