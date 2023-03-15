## Type casting in fngi

Fngi aims to "get out of your way" when writing types and functions.  One of the
primary ways it does this with minimal complexity is allowing simple type casting
when the types are bit-compatible.

If two structures are field-identical (in types, not necessarily in names) then
they can be explicitly cast to eachother. For instance, `Foo [ a: U4, b: S]` and
`Bar [x: U4, y: S]` can be cast into eachother.

If a pointer type is required who's first fields are identical to a given type,
then the pointers can be cast. For instance a reference to `B [ a: U4, b: U4]` can
be cast into a reference to `A [ a: U4 ]`.

This is allowed because the required type `A` has the same
fields at it's start as `B`. Therefore a pointer to `B` can be auto-cast into an `A`
pointer since they are bit-for-compatible.

You could think of `B` as a "subclass" of `A` and you would be partially
correct, since methods defined for `A` can be run using `B`. However, fngi
doesn't keep track of concepts like "subclasses".

### How does the user do this?
This is only done when the `given` TyI has the `auto` bit set on it's first
type. This bit is set by the `auto` syn function, like so:

```
fn foo a:&A do ( ... )

\ in some function
var b:B = ( ... )
foo(auto &b)
```

The above compiles. If `auto` was removed you would get a type error.

### More details
auto-type casting applies not just to structs, but also to function pointers and
Roles. For Role casts, the _names_ of the methods must match in addition to their
type signatures. This means that a `File` Role, which starts with the `drop` and
`close` methods, can be cast into a `Resource` Role which only has those
methods.
