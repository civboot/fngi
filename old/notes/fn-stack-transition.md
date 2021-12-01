I need to decide how the compiler is going to handle both kinds of functions
(stk and non-stk).

The first hard rule is that a function inp/ret type needs to be either one of
stk or other ty. I don't think I can support inp/ret values which are BOTH
types (i.e. `stk[U32] Point` -> stk[U32, I32] Foo`)

Do I want to allow input `stk` types and output other tys? I think so, but
need to explore the space a bit more...

The primary problem is the _transition_ between stk and non-stk data types.
For instance, here is how a "normal" function call might go:

```
x: U32 = 0;
y: U32 = 0;
x := foo$ {x; y};
```

It is pretty clear here: a copy of x and y are passed in as `inp` and the
reference to x is passed in as `ret`.

However, what if `foo` is type `stk[U32; U32] -> stk[U32]`? That is the core
problem I'm trying to solve here.

# Solution
I think I'm going with the 2nd attempt: assignment can pull from either a non-stk
function or a stk function. Passing variables INTO a stk function can be done
using `pushXXX` or one of the first defined macros, `stk!`.

## 1st Attempt
I think having the compiler figure out the gynmasitcs necessary for this kind
of thing to "just work" is not correct. I think that a macro, or something like
a macro, is necessary.

Let's consider a few options. First, the simplest:

```
pushU32$ y;
pushU32$ x;
foo$()
x := popU32$();
```

Our one-liner has now become 4 lines, very regrettable. Not only that, but we
must keep track of pushing/popping in the correct order, which can be somewhat
painful (but is likely unavoidable). Let's try with a macro that can accomplish
the above in one line.

```
stkCall! x := foo${x; y};
```

The above macro would build up the syntax something like this:
- Look at the syntax and recognize that it is a call/assign style (it could
  handle other styles probably, like no asignment).
- Investigate the type of the return value and use the correct `popTY`.
- Simply call the function with no argument before the return value is popped.
- Check that one of the following are true. If not, it can't do the next step.
  - there is only one input
  - OR all the inputs are constants or calls to const functions
- Reverse the order of the input expressions and insert them as `pushU32$` statements.
- This could also be one of the first call types to support multiple assigment,
  i.e. `{x; z} := fncall$()` since it would be pretty trivial to impelment.

Okay, that's actually making a lot of sense. We can easily implement this macro
in fngi as one of the first things we do.

I think the above can handle all the edge cases:
- If the function inp/ret are both stk do above
- If the function inp is stk but ret is non-stk simply make the call/assignment normal.
- If the function inp is non-stk but ret is non-stk then do the normal call but
  deconstruct the assignment.
- If there is no assignment simply leave it out.


Made up FAQ:
- How does this solve the problem with resolving operators like `+`?
  - in stage0, all operators (besides `$`) must be purely stack-based.
    For `$` to be used with `+`/etc operators it must have a stk ret with one
    value. In later versions of fngi this will probably be lifted and the compiler
    can handle the funny buisness of figuring out how to pass values around.
- Can you nest `stkCall!` to use inside an expression?
  - sure, but the resulting expression must return a single value stk just like
    above.

## 2nd Attempt

On the other hand, how difficult is all of this really? Let's look at an
alternative approach.

```
stk{x} := foo$ stk{x; y};
```

Here we can see that there is de-structuring happening but it is all language
supported. This would again only support non-mutating expressions, but that is
okay.

However, I don't think there is much value in this over the macro approach and
will bloat the core language. Let's take another step back. What if we simply
allow `:=` to pull from the stack if the right side returns stack values? That
would seem to be pretty clean. How about:

```
x := foo$ push!{x; y};
```

Push can be implemented in fngi, and pulling from the stack is easy enough to
implement. The language can later implement de-structuring for pulling multiple
values from the stack.

# Problem of temporaries
Temporaries are memory locations that the compiler inserts into
multi-expression statements. For instance (if there were no datastack) `z := (x
+ 1) * (y + 3)` requires the result of "x + 1" to be stored, the result of "y +
  3" to be stored, then them both multiplied together.

In pure-datastack languages this isn't a problem -- the datastack acts as
a kind of implicit temporary store. Similarily for fngi, we don't have to worry
about temporaries for pure-datastack operations.

I think for stage0 the solution is quite clear: no temporaries. All operators
besides call are already stack based, and we can just enforce that all
expressions in any operator must emit stack items. This is as easy as wrapping
them in `pushU32`/etc if they don't already emit the value.
