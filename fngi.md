# Fngi details

Fngi is a general purpose strongly-typed concrete syntax[1] language with
similar constraints to C. Unlike C it has a rich macro system allowing for
arbitrary syntax extensions. It targets a bare-bones virtual machine called
[spor](./spor.md), which will eventually be compileable to native code.

[1] a "concrete syntax" is (tounge-in-cheek) the opposite of an "abstract
syntax". It is defined as a syntax where the compiler only reads a token stream
and never creates an abstract syntax tree of any kind.

## Fngi basics

Fngi is a stack based language. This means arguments for a function can be
gotten either from the current stack or an argument passed to them.  Here are
two examples to add 1 and 2, both are valid fngi:

- prefix: `add(1, 2)`
- postfix: `1 2 add;`

Fngi syntax is token based -- no need to create anything complicated if
compiling a single token is all you need for structuring code. The important
thing is that a "single token" can be a syn function, which gets executed
immediately and creates its own syntax!

In fngi, all function calls (except syn functions) compile the next token first.
This means that `1 + 4` put's `1` on the stack, then puts `4` on the stack then
adds them (calls `+`). One of the most important syn functions is `(`. The only
thing it does is compile tokens until it hits `)`.

Tricks like this simplify the entire language dramatically. The way that `fn`
(how you define a function) is defined is (1) parse inputs/outputs by compiling using
`inp` or `stk` depending on whether you've encountered `->` and then (2)
_compile a single token_, which is your function body.

> Fngi has a few tokens defined for documentation/syntactic surgar:
> - `,` does nothing and is intended as syntactic surgar (documentation) to
>   separate arguments in functions.
> - `;` does nothing and is intended to be consumed when no arguments are being
>   passed to a PRE function, i.e. `ret;`
> - `\` is a swiss-army knife comment, described below. It is NOT consumed by
>   a pre function.

For example: `myFn 1`

Gets compiled as:

* push literal `1` onto working stack
* call `myFn`

So how do we pass more than one argument? It's easy, we use parenthesis!
`(` will continue to compile tokens until it hits `)`

So we can do: `myFn2(1, 2 + 3)`

Which is compiled as:

* push literal `1` onto working stack
* push literal `2` onto working stack
* push literal `3` onto working stack
* call `+` (add). This consumes two values from WS and pushes one.
* call `myFn`

### Comments

The ultimate "do nothing" token is the swiss-army knife comment: `\`

Comments have three forms:

- `\token` comments out a single token.
- `\(block comment)` is a block comment
- `\ line comment` is a line comment

So you might write one of the below:

- `myFn(_, 2 + 2)  \ first argument from the stack`
- `myFn(\a, 2 + 3)`
- `myFn(\(&a), 2 + 4)`

The fngi comment is something that would be fantastic for other languages to
replicate. Being able to sprinkle little `\token` comments to declare meaning
improves readability tremendously.

## Example

Below is an example fngi function to calculate fibronacci recursively.

```fngi
fn fib n:U4 -> U4 do (
  if(n <= 1) do ( ret 1 )
  fib(dec n) + fib(n - 2)
)
```

Let's explain a few details on how the parser/compiler stays extremely minimal:

- `fn` is an SYN function, meaning the compiler executes it instead of
  compiling it to the heap. It handles the input syntax until `do` then compiles
  a single token for it's body.
- `(..)` is NOT an expression. `(` is simply an syn function that compiles
  until it encounters the token `)`. In this case, what is between them is the
  function body.
- `if` is another syn "syntax function" which expects syntax of the form
  `if [check] do [code]`. It can also peek at whether the next token is `elif`
  or `else` then it can look like: 
  `if [check] do [code]  elif [check] do [code]  ...  else [code]`
- local variables are handled by the token compiler. They can be fetched or if
  the next token is `=` they will be set.
- If a token can be interpreted as a number, then it is a number. Otherwise it's
  treated as a constant, variable or function (which might be a syn function).

## Fngi syntax

Fngi has the following rules for token groups.

"Single tokens": these tokens are never grouped together

- `( ... )`: groups an execution.
- `$`: run next token/s NOW (compile time). Can be `$( ... )`
- `.`: used for name paths, i.e. `my.name.chain`.

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

In fngi, `$token(1, 2)` will run not only `token` NOW, but also it's args and
anything inside of it's args. This is because `$` set's itself as the "compile
fn" for a single token. `$`'s compile function just passes `asNow=true` for
every token that is compiled.

Some functions are defined as NOW, which means they will panic if executed
without `$` (or in a `$(...)` block).

Other functions are defined as SYN, and these are the true workhorses of fngi
syntax.  A SYN function is (1) always run NOW and (2) get's passed whether it
was called with asNow.

All fngi syntax besides literals, variable handling, and function calling is
implemented with syn functions.

> Note: If you want to compile literal computed at compile-time use `$L(1 + 3)`

## Structs
Structs are very similar to C with the addition of stack operations.

```
\ Define a struct
struct A [
  a1: U4
]

struct B [
  b1: A
  b2: U4
]

\ Take as input and construct
fn createB a:A -> B do (
  // construct variable
  var b: B = {
    // a = a;          // this is valid
    b1 = { a1 = a.a1 } // or construct manually
    b2 = 32
  }
  b.field1 = (b.field1 + 7) // assign fields
  ret b // put on stack
)

\ Define methods
define B {
  meth sum(this: &B) -> U4 do ( this.b1.a1 + this.b2 )
}

\ Use methods
var b  : B  = { b1 = { a1 = 42 }, b2 = 33 }
var sum: U4 = ( b.sum() + b.sum() )
```

## Similarities to FORTH

Like FORTH, fngi allows you to define functions that are either compiled or
run NOW (affecting compilation, often called "macros" in other languages). Also
like FORTH, fngi's functions operate by push/poping from a working stack and
using a call stack to track calls/returns.

Both FORTH and fngi have very low implementation requirements. Fngi is
implemented in about 2-3k lines of C, which is within the same order of
magnitude of most FORTH implementations. Like FORTH, fngi does not come with
hardware memory management requirements or any other bells or whistles. They
practically bootstraps themselves from nothing.

Fngi diverges from FORTH in the following ways:

- Does not follow FORTH naming scheme in any way. Typically closer to C
  naming.
- Although it is stack-based, all functions (except syn functions) cause the
  next token to be compiled before them. This means that syntax is typically
  written in C like standard-polish notation instead of FORTH's (hard
  to read) reverse-polish notation.
- Fngi has far more support for local variables.
- Has less dependence on the working stack (called the data stack in FORTH).
  No support for complex stack manipulation like `ROLL, TUCK, ?DUP`, double
  stack manipulation like `2DROP, 2SWAP`  or return stack manipulation like
  `R>`

## C, FORTH and fngi Comparison

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
fn fib n:U4 -> U4 do (
  if(n <= 1) do ( ret 1 )
  fib(dec n) + fib(n - 2)
)
```

For those familiar with FORTH the above will seem like syntax that _must_
require a far-too complicated parser. The truth is the opposite: the parser for
the above is basically no more complicated than FORTH's.

There are definitely a few areas of complexity in fngi that are not in forth
though:

* The virtual machine model definitely adds a bit of complexity, and the range
  of operations supported by the virtual machine (locals, security, etc) adds
  some complexity.
* Support of a range of dictionary types, like constant, var, dict, etc adds at
  most a byte (of metadata) plus a pointer for each function.
* Type support adds some data overhead, but not really very much.

