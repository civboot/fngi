fngi supports passing arguments to/from the stack. In fact, in the stage0
compiler it is the ONLY way to pass values between functions.

A function foo that accepts two U32's `a` and `b` and outputs a single `Bool`
might look like this

```
fn foo: stk[U32; U32] -> stk[Bool] do (
  if a == 0 do false
  else a == b
)
```

We represent stacks the same way the stack is represented in memory. Stacks
grow down, therefore the left-most value is at the "top" of the stack (it will
be popped first).

`{<top_stack_item>, <second_stack_item>, ...}`

So one way to call foo with `a=1` and `b=2` would be:
```
push! 2; // b
push! 1; // a
foo$();
```

Another way would be:
```
foo$ stk!{1, 2};
```

Notice that the second is the reverse of the first. This is because the
compiler will automatically handle reversing the expression inputs for you.
This only works if they are consts -- if they are not, there is a compiler error
and you MUST use the first method.
