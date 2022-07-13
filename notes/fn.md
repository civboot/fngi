# Creating fn and types

First of all, types will go on the end of \ dictionaries based on a `C_TYPED`
bit. Types will be stored in unaligned \ memmory next to pub/priv code.

Now why did I need `SN_find` again lol... oh ya, because I
NEED TO BUILD THE TYPE LOOKUP DICTIONARIES
... and the whole type system as well...
... no. This isn't that hard. It isn't as hard as what I wrote below.

I need to implement JUST "builtin" types, and have a function to handle them
and convert them to declVar. _Eventually_ that function will point to a better
implementation, but for now it will just build the locals/etc and provide
clean syntax.

That's really _all that's needed_. Also, I don't have to implement this as
`ufn`, this can be `fn` and call the relevant type registration stuff (if
they are non-null).

Stuff I do need:
- stk: SYN function that checks it's part of a function types and calls ref to
  adds input types.
- inp: SYN function that checks it's part of a function types and calls ref to
  add input types and compile to local variables.
- out: SYN function that checks it's part of a function types and calls ref to
  add output types.
- var: SYN function which can be used either on function input or body.
  Adds space to locals size but doesn't automatically do anything. Can be
  extended to use the dot compiler to auto-assign (i.e. `var x = 7;`)
- Initial PRIV functions to handle the above for only hand-rolled builtin
  types.

## Type representation
Types will be a bytestream (called a typestream) of unaligned big-endian values.

The first byte of a type is a meta byte: `TTT- ----`
- `TTT` is the same as the low-byte in DNode meta. Specifies CONST, FN, VAR, DICT
  which breaks down into GLOBAL, LOCAL, STRUCT, ENUM, MOD, SUBMOD
  - Some of the above are never used in the type stack (GLOBAL and LOCAL must
    be always 0, VAR shouldn't exist (use CONST), SUB/MOD can never exist)
- If the type is STRUCT/ENUM, the next byte specifies the number of fields/variants
- If the type is FN, the next two bytes specify the number of inputs + outputs
- If the type is CONST, only one type item follows.

Each type item starts with a byte: `R--- ----`

- If R=0 this is a builtin type, the meta is: `0EEE -DDD`
  - `E` is an enum of which one: `Any, Slot, U1, I1, U2, I2, U4, I4`
  - `D` contains the reference depth, meaning the physical size (in the struct,
    local, etc) is a Ref but the data (final FTO size) is the one selected by
    `E`.
  - `Any` types can never be materialized: you can only have a reference to them,
    you can never perform an operation (i.e. FT, ADD, etc) on them. They enable
    generic references (i.e. List[U4], BstU4[MyStruct])
- If R=1 this is a reference type, the meta is: `1--- --SS` 
  - It is followed by a big-endian pointer of size `SS` which is an `&DNode`

The type stack will unpack these into the following structs:

```
struct TyItem {
  ty: Ref  \ reference to data. Is null for builtin types.
  meta: U1 \ TYI meta
}

struct TyRepr {
  first:  Slc[TyItem];
  second: Slc[TyItem];
  meta: \ TYR meta
}
```



## Stuff I don't need (but may use a bit of)

Example syntax:

```
ufn myFn stk(a:U1 b:U2) inp c:U2; var i:U2; inp d:Ref -> out(x:Ref y:Ref)
  var i:U2 = 7;
  ... code
  var j:U2 = 10;
  ... code
)
```

I have now created `if` syntax (sans type checking). I now wish to create ufn syntax.
For a _complete_ implementation, I need:
- Define how types are represented and stored (probably BST using them as a key?)
- Read types and sizes, for input compiling
- Role method: push types
- Role method: register current types (get hard pointer)

Add later:

This is... a lot. In particular, there is a chicken-and-egg problem with the
definition of types and structs. Some of this can be avoided by substituting roles and
doing the types later, but I don't like that.

Instead, let's do a shortcut! The stk, inp, etc fns WILL use a role method... except for
a select number of built-in types. For these, it will handle them directly. This solves
another chicken-and-egg problem, which is that we need a way to represent these types
in structs anyway.

