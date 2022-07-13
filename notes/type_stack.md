# Type Stack

Fngi will bootstrap itself into types. Types are _entirely optional_ for fngi,
meaning you can have a version of the language which has no type checking and
you can define un-typechecked functions even when the language does have type
checking. However, types are typically _extremely useful_ and therefore
recommended for most cases.

Fngi has a compile-time "ty stack" (type stack), which is just an array of
pointers who's length is the system stack size (typically 12 - 32). There are
two recognized types in fngi:

- **concrete** type: native (i.e. U4), Struct, Enum, and Role instance.
- **executable** type: the type _itself_ is a role instance of ExecTy. The
  type checker calls it's methods depending on context.

## Quick Example

A quick example (using only native concrete types), say you had the following
definitions:

```fngi
fn add3[\a:U4, b:U4 -> out:U4] do (
  \a + 3 + .b
)
```

A few important points:

- `\a` in the type name causes it to _remain on the stack_.
- `b` is a local variable, it gets pushed to the locals stack.
- In the code block, `\a` before the `+` is purely for documentation, it has no
  effect on the ty stack.

Type stack changes:

- At the function start, the ty stack contains `[U4]`, which is `\a`
- `+` is a `PRE` function, so encountering it the syntax does not (yet) change
  the ty stack. _It is only when a function is executed that the ty stack is
  changed_.
- The compiler knows the type of literal `3` and pushes `U4` on the ty stack.
  The ty stack is now `[U4 U4]`
- `+` is now evaluated. It's type signature is `[U4, U4 -> U4]`. It's inputs are
  popped and checked (which they pass) then the result is pushed on the type
  stack. It is now `[U4]`
- The next `+` is again a `PRE`, so doesn't (yet) affect the ty stack.
- `.b` is a known type (from the fn signature). The dot compiler pushes `U4`
  onto the ty stack, so it is again `[U4 U4]`
- The `+` is evaluated and checked. The ty stack is `[U4]`
- The function ends, and the remaining ty stack is compared to the function
  signature. It checks out!

## If Statements

Conditionals look like below. Note a few things:

- Who knows for sure why this code was written, don't focus on that.
- `U4` is documented as the return value (inferred).
- Note that every conditional leaves a U4 on the stack _or_ it returns from the
  function (and it's return value matches the function)

```fngi
fn doSomething[x: U4, y:U4 -> a:U4, b:Bool] do (
  if (.x) do (.y + 1)  \ [ -> U4]
  elif (.x - 1) do (.y + 2)
  else ( ret (.y+3, FALSE); )
  ret (_, TRUE);
)
```

How does the type stack do this?

- Before the `if` statement executes anything (including it's conditional check)
  it clones the type stack. Call this `ifBefore`
- After it's conditional check, it makes sure that the type stack is identical
  (the conditional check cannot change the type stack).
- After it's code block, it clones the type stack again. Call this `ifAfter`
  - Exception: if the code block ends in a `ret` of any kind then type checking
    of the function is enforced, but type checking of the if statement is not.
    Therefore anytime a `ret` is compiled it updates a global variable for
    scope-checkers like this.
- elif's conditional check starts with `ifBefore` and is checked to make sure it
  doesn't change.
- elif's code block starts with `ifBefore` and checked to make sure it's
  identical to `ifAfter`.
- the `else` clause ends in ret. It starts with `ifBefore`, but per the
  exception above it is only checked for the function type.
- The if block then "returns" it's type stack by setting the main type stack to
  `ifAfter`.

> Note: panics also avoid a type check obviously.

## While Loop / Switch

All `loop` statements must have the same type at their beginning as their end.
All of their `break` statements must have the same type as well. However, values
can be returned from the `break` statements, the logic is similar to `if`.

Switch statements are the same.

## Native Types

Native types are stored in the native dictionary. This contains only enough
information to write untyped fngi code, it does _not_ support type checking
beyond the most primitive aspects (i.e. de-reference count, calling an large
function like a small, etc).

There are 5 `TY_*` bitmaps stored in the dictionary meta. They are:

- `TY_CONST`: a UA constant.
- `TY_FN`: a function `NORMAL|NOW|SYN|SYN_I`, `POST|PRE`
  - If defined on a Struct/Enum/Role the fn is considered a method. When type
    checked, the dot compiler will auto-insert the instance reference if it is the
    last input type (top of stack).
- `TY_MODULE`: a module with a pointer to it's sub-dictionary, with ty bits for
  whether it is a Module, Struct, Enum, Role, or RoleImpl (implemented role).
- `TY_GLOBAL`: a global variable offset, sz, and reference count
- `TY_VAR`: (TODO: rename `TY_LOCAL -> TY_VAR`)
  - if defined in the locals dict this is a local variable offset, sz, reference
    count and input bit.
  - if defined in a MODULE=struct this is a struct field with an offset, sz and
    reference count.
  - if defined in a MODULE=enum this is an enum field with the variant number,
    sz and reference count (of the embedded type).

## Concrete Types

Concrete types are stored in a BTree with [key=&dictKey value=type]. This allows
you to look up a type if you know the dictionary pointer.

- Fns store SLLs of their input and output types. Locals are not stored.
- The base Struct/Enum/Role Ty just stores its size, and it's field members store a
  pointer to their concrete types.
- RoleImpl's are just a Role struct constant which has the function methods
  filled in.

## Executable Types

> Cancel all of this: "executable types" are just SYN. The SYN function can
> check if it is being called during a type block and act accordingly.

Executable types do not have a dictionary entry. Instead, they are a role
instance of the following form:

```fngi
role ExecTy [
  register: &fn [e:#ExecTy -> &CompTy];
]
```

`register` will "register" a new instance. It will do this by compiling tokens
to determine what the sub-types are. By convention this would look like below.
`List` in this example is an ExecTy, and it will compile `[U4]` to determine
what it's supposed to be "generic" over.

```fngi
fn foo [a:List[U4]] do ( ... )
```

Some important points:

- `register` is supposed to create a globally-unique instance (i.e. a struct) of
  the types it consumes, so that if it is called again with the same types it
  will return the same pointer. This allows you to compare the types!
- ExecTy are kind of like `NOW` or `SYN` functions, except for the type
  stack. Instead of being "compiled" they are "executed."
- ExecTy's are very powerful. They can consume any syntax, they can store
  whatever context they want, etc.
- However, ExecTy's cannot cause the function to _emit different code for
  different types_. fngi still has a "concrete syntax" and functions are
  compiled in a streaming fashion.
- Because of the above constaint, ExecTys are really only a way to have
  _generic references_. They are only actually useful for implementing
  containers (i.e. BTree, List, etc) or similar use-cases where you want to put
  a generic type in and then get it out (and you want the type checker to
  guarantee the types).
- However, theoretically an ExecTy could probably be paired in some way with a
  Role to allow _methods_ be tied to the data, allowing for something closer to
  generics. I haven't seriously considered this, but I think it's largely not
  recommended (although there may be certain use cases).
- If code generation is what you want/need I'd recommend a macro for
  accomplishing it: not using ExecTy's.

