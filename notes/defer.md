# Defer

fngi will support defer. It will be similar to golang with a few caveats:

- There is no "defer stack". The first defer simply puts the beginning of the
  block on the call stack. Following defers mutate this value (and the end of
  their blocks is a hard-jump to the previous defer block).
- Because of the hard-jumping nature, you cannot (should not?) use defers
  inside of if-statements. This is especially true of the first defer.
  For continuing defers, if it is skipped it won't run... unless there is a
  defer _after_ it (since all the defer blocks are hard-wired together).  This
  makes it extremely touchy when code changes.
  - Skipping the first defer will cause later defers to mutate the existing
    callstack: _very_ bad.
  - However, if there was only one defer or you could guarantee ALL defers
    were skipped if it was skipped then it could be skipped... at your own
    risk!
- Defers do nothing special when a panic happens. In other words, panics
  ignore defer just like they ignore the call stack.
- The defered token has no special handling of arguments before the deferred
  code executes... why does golang even do that???

When the `defer` is called it compiles a `DST` (defer start) instr and empty byte. This:

- pushes ep+2 onto the call stack
- jmps forward by the byte amount.
- The empty byte should be updated with the block size by an END or equivalent.

It then compiles the next token (which can be `( ... )`) and updates the jmp
byte.

When the next `defer` is called it compiles a `DCON` (defer continue) instr and
empty byte. This:

- **mutates** the call stack (doesn't push) to be ep+2
- jmps foward by the byte amount (which again is determined by next token).

> We could also use 6 bytes to read+update registers and do a JMPL.
> However, we have a lot of room on the jmp command. I think it makes the most
> sense to put it there, since defers will be very frequently used.

When the function rets, it actually returns into the last encountered defer
block. This will hard-jump to previous defer blocks until the first one, which
will ret like a "normal" function.

## Use and Examples

Conceptually, defer statements work by putting a defer block of code on a stack.
The deferred blocks are then executed in FILO (first in last out) order.

```fngi
fn foo[] do (
  defer println(|printed last|);
  .v = .Foo.new() defer (
    println(|freeing|); .v.free() );
  print(|printed first: |); println(v.value);
  defer println(|printed before free|);
  println(|printed second|);
  ret;
)

\ Outputs:
\ printed first: 42
\ printed second
\ printed before free
\ freeing
\ printed last
```

If a `ret` was run before a defer, then those deferred items won't be run.

Defers are mostly used for:

- freeing memory before function return.
- closing open resources.
- performing logic that must be placed at the end.

## Type Stack

Defers work with the type-stack system. The rules are:

- Defer blocks are assumed to have nothing on the stack when they start and
  should "ret" nothing.
- Otherwise, rets are checked as they normally are.

If all returns are rets the right types, and all defers are returning nothing,
then the function will return the values it says it will.

> Possible feature: defer blocks that return something could "pop" that type off
> the _bottom_ of the return type stack. Thus you could use defers for returning
> values. Would probably want to specify this with, i.e. `defer [U4] ( ... )`
