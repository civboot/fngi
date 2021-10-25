# fun: a fu language

Fun is a forth-like language that builds on fu directly.

**Right now this is just some rough notes**

## Syntax

Fu's syntax is split into three groups:
- special characters: `~ ' . ( )`
- alphanumeric characters (case sensitive): `0-9 a-b A-Z`
- other symbols

A token is characters from one group next to eachother. Whitespace always
separates tokens. For instance `aa` is one token but `a a` is two.
Similarily `$a` is two tokens since they are in separate groups. `($a)` is four
tokens.

That's the entire syntax, there is nothing but tokens, which are looked up as
fns (functions). The word used for "token" is "fname" (function name). When a
fname is encountered by the compiler it is looked up in the dictionary and then
may one of three types:
- `FN`: normal fn (function). The function is compiled into the current word.
- `IMM` (immediate): the function is run immediately (think: a macro).
- `PRE` (prefix): the fname is looked up but not compiled. The next word is
  compiled. When that word is completed the function is compiled.


## Special character group:

The first group is made up of special characters.

- `~`: this is the "char" command. It compiles the next character as it's ASCII
  (or UTF-8 if supported) code integer. For instance ~A is 65 since that is
  it's ASCII binary value.  Also supports simple escapes like `~\n` (newline).
  Note: `~ ` is a space.
- `'`: toggles whether the next word is a PRE word.
- `.` is used for module lookups. It uses the typestack to find the next word
  in it's namespace. More details will be given later.

`(` and `)` are the real treat of fu vs a more traditional forth. All `(` does
is compile and execute words until it encounters `)`.  The purpose of this primarily
has to do with the PRE words, which there are many and you can use `'` to make any
word PRE.

Let's look at some examples. Like forth, the following pushes `1 2` on the stack and
then calls `add` (which might add them)

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

Note that `'` switched the normally PRE `+` to be normal.

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

The above is how you would have to normally write the above using
reverse-polish notation. In the opinion of the author of fun, using parens is
much easier and adds almost no complexity to the parser or compiler.

Using `'` and `(` you can also make more traditional function call. If you have
a function point which takes `x, y` you might create it using:

```
'point(3 (7 * 8))
```

This sure beats how it would be done in reverse-polish:
```
3 7 8 '* point
```

## Unstructured Notes




Let's say locals offsets are: x:0 y:4 z:8
```
                //    x y  values  store_y  store_x
=x 10           // => 0    10               $SRLP_WS
=x =y (10 20)   // => 0 4  10 20   $SRLP_WS $SRLP_WS
```

1 x STORE
