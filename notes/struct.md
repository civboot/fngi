# Design of struct data types

STRUCT's are defined with the following syntax:

```fngi
STRUCT 0x40 Name [ // 64=size of TY_DICT dictionary for this struct.
  field1: U4
  field2: U4
  field3: OtherStruct
]
```

- Alignment is C-like (bit packed while respecting system alignment).
- `Name` appears as a dictionary entry of type `TY_DICT`. This means
  that it is actually a pointer to another dictionary.
- field1-3 are keys in this other dictionary. They are `TY_FIELD`
  and have a pointer to their type and offset, among other things.

Before `TY_DICT` can be implemented I must implement both block and buddy-arena
alloctors.

In the long-term (with the dot compiler) structs can be created using something
like the following:

```fngi
.foo = {
  field1 = 4;
  field2 = 5;
  field3 = {.a = 7};
}
```

To actually use struct's and their fields in the short-term, it is necessary to
implement `VAR`, an untyped pre-cursor to the `.` compiler. This automatically
handles recursive lookups. It does NOT handle assigning structs to a struct
constructor, type checking, or many other things.

```fngi
// foo (concrete local struct)
VAR foo.field;
VAR foo.field = 7;
VAR &foo.field;

// Or rFoo (reference struct)
VAR rFoo.field;
VAR rFoo.field = 7;
VAR &rFoo.field;
```
