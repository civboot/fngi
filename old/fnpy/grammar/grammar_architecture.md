Because fngi requires functions to be defined before they can be used,
it has to do some pretty special things to be able to define its own
grammar.

Some of the grammar functions can be defined based only on what
came before them such as number, iden, etc.

However anywhere a "name" is there can also be a macro, meaning the definition
for name has to include the definition for macro.  But the definition of macro
includes the definition of expression, which includes... everything.

How do we resolve this? Is fngi the language doomed?

Not quite. Expression will be defined thusly

```
let expressionOptions: Sll[Parser] = (
  let ll: Sll[Parser] = Sll[Parser] cArena;
  ll.push(number);
  ll.push(iden);
  ll
);

let expression: fn [] -> Bool (
  let head:  = expressionOptions;
  while true then (
    let parser: Parser = head.get[];
    if parser[] then (
      return true;
    ) else (
      head = head.next$[];
    )
  )
  return false;
)
```

