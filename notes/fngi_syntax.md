
Function syntax
```
fn foo [?a:U4 ?b:U2 c:u4] -> [U4] do (
  // ...
)
```

- `?` declares an input that is _not stored_ as a local. Instead, it remains the
  stack. The name is used only for documentation (and for the type checker).
  These variables _must_ be the left-most variables.
- The other inputs (without `?`) are popped in reverse-order from the stack at
  the beginning of the function and stored as locals.
- `-> [...]` is optional and specifies the return type.
- `do` is syntactic surgar to make function definitions (and if/while/etc) more
  readable, especially in cases where `()` are not used.
