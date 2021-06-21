fngi supports passing arguments to/from the stack. In fact, in the stage0
compiler it is the ONLY way to pass values between functions.

A function foo that accepts two U32's `a` and `b` and outputs a single `Bool`
might look like this

```
fn foo{[U32] a, [U32] b} -> [Bool] (
  if equalU32(0, a) then
    false
  else
    equalU32(a, b)
)
```

We represent stacks the same way the stack is represented in memory. Stacks
grow down, therefore the left-most value is at the "top" of the stack (it will
be popped first).

`{<first_stack_item>, <second_stack_item>, ...}`

So one way to call foo with `a=1` and `b=2` would be:
```
push! 2; // b
push! 1; // a
foo();
```

Another way would be:
```
foo{1, 2};
```

Finally a third would be:
```
push2!{1, 2};
foo();
```

Notice that the second and third look like a reverse of the first. This is
because the compiler will automatically handle reversing the inputs for you.
In fact, if all of the computations inside the stack expression are calls to
const functions (i.e. they do not affect global state) then the compiler does
this with zero overhead (it actually reverses the AST nodes).

> If the expressions are not constant, then reversing the nodes could cause
> unexpected behavior. Therefore instead of reversing the nodes it computes
> them in the order written, then inserts a `reverse2!()` call before calling
> the function.
