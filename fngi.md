# Fngi details

Fngi is a general purpose concrete syntax[1] language with similar constraints
to C. Unlike C it has a rich macro system allowing for arbitrary syntax
extensions. It targets a bare-bones virtual machine called [spor](./spor.md),
which (eventually) can be compiled to native code.

[1] a "concrete syntax" is (tounge-in-cheek) the opposite of an "abstract
syntax". It is defined as a syntax where the compiler only reads a token stream
and never creates an abstract syntax tree of any kind.

## Fngi basics

Fngi is a stack based language. Most stack based languages (i.e. FORTH) use
postfix notation, also called reverse polish notation. However, most "modern"
(and more readable) languages use prefix notation, also called standard polish
notation. Fngi uses mostly standard notation. Here are two examples to add 1 and
2, both are valid fngi:

- prefix: `add(1, 2)`
- postfix: `1 2 add;`

In fngi, functions that accept arguments are labeled as PRE, meaning they use
prefix notation (the exception is purely stack-altering functions: swp, drp,
ovr, dup, dupn). Functions that don't accept arguments use postfix notation
(they don't need `()` or `;`). However, by convention they are followed with
`;`

Fngi has a few tokens defined for documentation/syntactic surgar:
- `,` does nothing and is intended as syntactic surgar (documentation) to
  separate arguments in functions.
- `;` does nothing and is intended to be consumed when no arguments are being
  passed to a PRE function, i.e. `ret;`
- `\` is a swiss-army knife comment, described below. It is NOT consumed by
  a pre function.

Unlike C (or most languages) that have an abstract syntax tree, fngi goes about
parsing in a radically simpler manner. Conceptually, functions (that take an input)
take in only a _single token_ of input. So you can do `inc 1`. However, some tokens
execute themselves to compile _more_ tokens, so you can also do `add(1, 2)`

This allows fngi's compiler to not keep track of an abstract syntax tree; compiling
 tokens "instantly" as they appear in the stream. This is critical for fngi to
 be able to self-bootstrap on very minimal targets like microcontrollers, but
it also makes reasoning about fngi syntax very easy (in some ways).

For example: `myFn 1`

Gets compiled as (spor assembly, see spor.md):

```spor
#1$L     \ compile literal 0x1
$xx:myFn \ compile a call to myFn
```

So how do we pass more than one argument? It's easy, we use parenthesis!
`(` will continue to compile tokens until it hits `)`

So we can do: `myFn2(1, 2 + 3)`

Which is compiled as

```spor
#1$L      \ literal 0x1
#2$L      \ literal 0x2
#3$L      \ literal 0x3. Note + was PRE so this is before %ADD
%ADD      \ addition {2 3 -> 5}
$xx:myFn2 \ call to myFn2, which will have a stack of {1 5}
```

### Comments

The ultimate "do nothing" token is the swiss-army knife comment: `\`

Comments have three forms:

- `\token` comments out a single token.
- `\ line comment` is a line comment
- `\(block comment)` is a block comment

So you might write one of the below:

- `myFn(_, 2 + 2)  \ "a" from the stack`
- `myFn(\a, 2 + 3)`
- `myFn(\(&a), 2 + 3)`

## Example

Below is an example fngi function to calculate fibronacci recursively.

```fngi
fn fib inp(n:U4) -> out(_:U4) do (
  if(.n <= 1) do ret 1;
  ret(fib(dec .n) + fib(.n - 2));
)
```

Let's explain a few details on how the parser/compiler stays extremely minimal:

- `fn` is an SYN function, meaning the compiler executes it instead of
  compiling it to the heap. It is a "syntax function" that executes `syn`
  functions like `inp` and `out` until it reaches a `do` token.
- `(..)` is NOT an expression. `(` is simply an syn function that compiles
  until it encounters the token `)`. In this case, what is between them is the
  function body.
- `if` is another syn "syntax function" which expects syntax of the form
  `if [check] do [code]`. It can also peek at whether the next token is `elif`
  or `else` then it can look like: `if [check] do [code] elif [check] do [code]
  ... else [code]`
- `.` is another "syntax function" that does "variable" compilation. It is used
  for fetching and storing variables of all types (globals, inputs or locals),
  as well as references, module lookups, etc.
- If a token can be interpreted as a number, then it is a number. Otherwise it's
  treated as a constant or function (which might be a syn).

## Fngi syntax

Fngi has the following rules for token groups.

"Single tokens": these tokens are never grouped together

- `( ... )`: groups an execution.
- `$`: run next token/s NOW (compile time). Can be `$( ... )`
- `.`: enter name compiler `.foo = myFn(3)`

Otherwise a token is a group of:

- alphanumeric characters (including `_`), i.e. `foo_bar123`
- symbol characters not in the above "single" tokens, i.e. `+-*&` is a single
  token

Functions can be defined for any token that is not purely numeric

- Numerics: `0x123 (hex)  12_345 (decimal)  0b11_0101 (binary)  0cA 0c\n
  (ascii character)`
- Valid function names:  `+++  0b12 (not binary)  123xx (not decimal)`

## Fngi "macros": SYN and NOW

Fngi allows any function to be run "NOW" at compile time. In spor this is
done using `$token` and it is very similar in fngi.

In fngi, `$token(1, 2)` will run not only `token` NOW, but also it's args
(assuming `token` is PRE).

> The mechanism by which this is accomplished involves very little code and is a
> bit clever, see the definitions in `./spor.sp` if you are curious.

Some functions are defined as NOW, which means they will panic if executed
without `$` (or in a `$(...)` block).

Other functions are defined as SYN, and these are the true workhorses of fngi
syntax.  A SYN function is (1) always run NOW and (2) is passed a boolean
(`asNow`) which indicates whether the user wanted it to be run NOW
(i.e. whether the user used `$`).

Some syn functions call `notNow;` right away, meaning they never want to be
called with `$`. These typically implement some syntax such as `fn`, `if`, etc.

Other syn functions have different behavior whether called with `$` or not.
For example `$(1 + 3)` will put `4` on the stack at compile time. However,
`(1 + 3)` will compile both the literals and the `%ADD` instruction into the
function.

> Note: If you want to compile the operation at compile time, use `$L(1 + 3)`

## Similarities to FORTH

Like FORTH, fngi allows you to define functions that are either compiled or
run NOW (affecting compilation, often called "macros" in other languages). Also
like FORTH, fngi's functions operate by push/poping from a working stack and
useing a call stack to track calls/returns. In addition, both FORTH and fngi
have very low implementation requirements. Fngi is implemented in about 1000
lines of spor assembly, and spor is implemented in about 2000 lines of C. Like
FORTH, fngi/spor do not come with hardware memory management requirements or
any other bells or whistles. They practically bootstraps themselves from
nothing.

Fngi diverges from FORTH in the following ways:

- Does not follow FORTH naming standards in any way. Typically closer to C
  naming.
- Although it is stack-based, all functions (except those with no stack inputs
  or pure stack manipulation like dup/drp/ovr) cause the next token to be
  compiled before them. This means that syntax is typically written in C like
  standard-polish notation instead of FORTH's (hard to read) reverse-polish
  notation.
- Far more inclusive support of locals. Has a separate locals stack and
  execution mechanisms which automatically allocate space for locals when
  executed and deallocate when returned, making this both performant and
  minimal.
- Has less dependence on the working stack (called the data stack in FORTH).
  No support for complex stack manipulation like `ROLL, TUCK, ?DUP`, double
  stack manipulation like `2DROP, 2SWAP`  or return stack manipulation like
  `R>`

```c
// C Syntax
uint32_t fib(uint32_t n) {
    if (n <= 1) return 1;
    return fib(n-1) + fib(n-2);
}
```

```forth
( FORTH Syntax)
:FIB ( n -> fibN)
  DUP 1 <= IF DROP 1 EXIT THEN
  DUP -1 FIB        ( n fib(n-1) }
  SWP 2 - FIB + ;
```

```fngi
\ Fngi Syntax
fn fib inp(n:U4) -> out(_:U4_) do (
  if(.n <= 1) do ret 1;
  ret(fib(dec .n) + fib(.n - 2));
)
```

For those familiar with FORTH the above will seem like syntax that _must_
require a far-too complicated parser. The truth is the opposite: the parser for
the above is basically no more complicated than FORTH's.

There are definitely a few areas of complexity in fngi that are not in forth
though:

- The virtual machine model definitely adds a bit of complexity, and the range
  of operations supported by the virtual machine (locals, global offset,
  security, etc) adds some complexity. However, it (in theory) reduces the
  binary size and allows easier portability. Really one just needs to port spor
  and fngi comes along for the ride!
- Support of the locals stack requres an extra stack, but also reduces the
  needed size of the working (data) stack.
- Support of a range of dictionary types, like constant, syn, pre, etc has a
  little more complexity. However, FORTH already had to handle NOW functions, so
  the complexity is largely in the forth compiler too.  Add to this that fngi
  provides basic type checking from the beginning (you can't execute a constant
  for instance) makes things well worth it.
