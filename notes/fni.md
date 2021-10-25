# fni: a fu language

`fni` is a forth-like language that builds on fu directly and is intended to
"morph" into fngi.

**Right now this is just some rough notes**

## Syntax

fni's syntax is split into three groups:
- special single characters: `~ ' $ . ( )`
- alphanumeric characters (case sensitive): `0-9 a-b A-Z`
- other symbols
- whitespace will separate any tokens.

A token is characters from one group next to eachother, except "special
characters" where each character is always a single token. For instance `aa` is
one token but `a a` is two.  Similarily `$a` is two tokens since they are in
separate groups. `($a)` is four tokens since `($)` are all special
single-character tokens.

That's the entire syntax, there is nothing but tokens, which are looked up as
fns (functions). The word used for "token" is "fname" (function name). When a
fname is encountered by the compiler it is looked up in the dictionary and then
may one of three types:
- `FN`: normal fn (function). The function is compiled into the current word.
- `IM` (immediate): the function is run immediately (think: a macro).
- `PRE` (prefix): the fname is looked up but not compiled. The next word is
  compiled. When that word is completed the function is compiled.


## Introduction to Syntax

The first group is made up of special characters.

- `~`: this is the "char" command. It compiles the next character as it's ASCII
  byte value. For instance ~A is 65 since that is it's ASCII binary value.
  Also supports simple escapes like `~\n` (newline).  Note: `~ ` is a space.
- `'`: toggles the PRE status of the next word. For IMM words causes it's
  execution to be delayed.
- `$`: makes a non-IMM fn be IMM, or run at compile time. For a PRE fn it will
  run the next fn at compile-time as well. For an already IM fn it will run it
  with a flag set letting it know that `$` was used, which may alter the
  behavior. For example, `$if` will do a compile-time if block (discarding the
  compiled fn/group if false) instead of compiling an `if` statement into the
  function.
- `.` is used for module lookups. It uses the typestack to find the next word
  in it's namespace. More details will be given later.

`(` and `)` are the real treat of fu vs a more traditional forth. All `(` does
is compile and execute words until it encounters `)`.  The purpose of this primarily
has to do with the PRE fns or fns prefixed with `'`.

Let's look at some examples. Like forth, the following pushes `1 2` on the stack and
then calls `add` (which may add them)

```
1 2 add
```

By convention, all symbol functions (i.e. `+`, `-`, `*`, etc) are PRE, so you could
also write this as:

```
1 + 2
```

Let's walk through the above as the compiler sees each token:
- `1`: compiles the literal value `1`
- `+`: `+` is PRE, so looks up the tyIdx of `+` but will only compile it after
  compiling the next word.
- `2`: compiles the literal value `2`
- Now that the "next word" has been compiled, `+` is compiled.

You might represent what the compiler outputs as the following:

```
1 2 '+
```

> Note: `'` switched the normally PRE `+` to be compiled as a normal FN.

Now comes the fun stuff, `()`. Let's look at another example:

```
1 + (2 * 3)
```

- `1`: compiles the literal
- `+`: is PRE, compile next item first
- `(`: start compiling until `)` is encountered. Note that this will _further delay the `+` prefix_)
- `2`: compile the literal
- `*`: is PRE (multiply), compiling next item
- `3` compile the literal
- the "next item" of `*` is compiled, so compile `*`
- `)` end of opening paren
- the "next item" of `+` is compiled, so compile `+`

The result will be:

```
1 2 3 '* '+
```

The above is how you would have to normally write the commands in FORTH or
similar reverse-polish notation languages. In the opinion of the author of fni,
using parens is much easier and adds almost no complexity to the parser or
compiler. In other words readability is massively increased with almost no
increase in complexity.

Using `'` and `(` you can also make more traditional function call. If you have
a fn named `point` which takes `x` and `y` you might call it with:

```
'point(3 (7 * 8))
```

This is more readable than equivalent FORTH
```
3 7 8 * point
```

## A few minor points:

**one token lookahead** the compiler supports one token lookahead.  Any IM fn
can see what the next word is as well as it's tyIdx through `nextFname` and
`nextTyIdx`.

**comments are c-like**. `//` is a line comment, `/* this is a block comment*/`
Spaces are recommended after comments since these are not in the special character
group (so `//+` will get parsed as a token, not a comment).

## Control Flow

The `()` syntax gives us another advantage over FORTH in that we can use it
for our control flow words:

```
if (x < 3) ( /* do something */ )
elif (x > 5) ( /* do something */ )
else ( /* do else */ )
```

> Note: if/elif/else are not built into the compiler but are rather IM fns.
> By convention, most fns should not be IM even if that is their primary use.
> Instead the author should use `$` to make them IM when that is desired, which
> helps improve the readability of the code. However, words like `if` are
> "super immediate" in that they control compilation and do something different
> if `$` is used.

How does this happen? The `if/elif/else` statements are only slightly more complex
than forth's. It's a little too complex to describe the operation here, but basically
the `if` fn is immediate and compiles two tokens and inserts unfilled jmps with the
address pushed onto the compile-WS to fill later. It then uses lookahead to
handle the elif/else cases correctly.

While statements can be done similarily. The point is that because the
control-flow words know that they only need to compile two tokens, they can
handle the structuring correctly. In FORTH you have to use `BEGIN <check> WHILE
<while block> REPEAT` which is quite a lot of words without helpful sigils to
know the scopes.

```
while(x 2 foo) ( /* do something */ )
```

> Note: in simple cases you can choose not to use `'foo(x 2)` to avoid too many
> parens. It's up to the author's taste.

In addition, if you have a function/variable that can be used as the while clause,
you can avoid parenthesis.

```
while x ( /* do something */ )
```

## Defining a function and local variables
Like forth, fni has two stacks. Unlike forth fni's working stack (WS) is
extremely limited (does not support addressing, only supports two-arg
operations) whereas the return stack is highly accessible and used in a c-like
fashion to store local variables by offset.

The language evolves itself, but early on the only function definition is nativefn
which does not support locals:

```
nativefn myNativeFn (
  // function definition
)
```

Later in the self-bootstrap, a more featureful fn with locals is implemented in fni
and available.

```
localsfn myLocalsFn (
  // type definition: ( inp1 inp2)
  inpNoTy 2 # index 0, size=2
  inpNoTy 2 # index 1, size=2
  varNoTy 4 # index 2, size=4
  varNoTy 4 # index 3, size=4
  // function definition
)
```

inputs and (local) vars must be defined at the top of the function and will
be stored in the return stack.

`inp1` and `inp2` are size 2. `localsfn` will
store the indexes and offsets and then compile them in reverse order (to
visually match the type signature) as `setlocal` operations.

## Type stack
TODO: explain `fn` and the type stack (advanced).

## Unstructured Notes
**Ignore these, these are my unstructured notes**

Let's say locals offsets are: x:0 y:4 z:8
```
                //    x y  values  store_y  store_x
=x 10           // => 0    10               $SRLP_WS
=x =y (10 20)   // => 0 4  10 20   $SRLP_WS $SRLP_WS
```

1 x STORE
