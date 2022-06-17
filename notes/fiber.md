# Fibers

In designing fibers to use `async.h`, then main challenge is what to do for `D_catch`,
which needs to be able to catch a panic. But then I realized: `D_catch` only
actually needs the current stack depth of the catching function to be stored
somewhere, which can be stored in a bitmap. The sp is already stored in the callstack.
Simple and effective.

I had _thought_ for the kernel to handle role methods that it needed to grow
it's own stack, but that was incorrect. Role methods can be a normal `xlImpl` call,
which only affects the fngi state.

So, before we can design fibers, we must define the behavior of our panic
handling. On returning from a `D_catch` nothing special will happen. In
particular the following will NOT happen:

- Not clear the err state. It is the caller's job to check the err state and
  act appropriately.
- Not change the WS state, whether there was a panic or not. It is the caller's
  job to act appropriately.

The _only_ thing `D_catch` will do is set a `catch` bit in csz when calling the
function.

When an error is caught (through longjmp) the following will happen:
- Find the largest catch bit less than the current CS depth.
- Clear the WS and push the previous CS depth (depth at the error).
  - Note: clearing the WS is essential to ensure there is enough for the CS depth and
    for standard error handling operations which may require pushing values. On some
    architectures, the WS may also be copied for inspection, or logged.
- Set the CS and CSZ depths to the catch depth

It does NOT do the following, which must be done by the caller:
- Change the locals stack depth. Most callers will want to set this back to what
  it was when `D_catch` was called.  This can be done by either traversing the
  call stack or by caching the initial value in a global variable / compiled
  location.
  - **Note:** you CANNOT cache it in a local variable, since the locals stack
    will be in the wrong location in an error condition!
  - If callers really wanted fast and re-entrant panic handlers, you could create a
    global stack which pushed and popped the local stack locations.
  - There will be a single global value reserved for this purpose, used soley
    for assertPanic. Users can also use it in their own applications if they
    know it will be used only once (the maximum number of panic handlers in
    most applications).

## Async Design

With the above design, the maximum entire stack depth of fngi's VM is _very_
small, probably on the order of 16 calls and maybe a few KiB of memory. Also,
most of the "state" for an async.h style design is already contained in the
Fiber class. This includes the execution pointer, ws, call stack, etc.
Actually, there possibly isn't any reason to use async.h at all.

However, the existing design of executeLoop doesn't work, as it has no support
for yield calls. The new design should:

- Simply start executing from ep. Not take in any arguments.
- executeInstr will _return_ a value to indicate one of:
  - `EXECUTE_NORMAL`: executeLoop will continue executing instructions.
  - `EXECUTE_RET`: executeLoop will handle a return.
    - If there is no CS depth, it will remove the fiber from the LL and return.
      The fiber will not be run again (unless re-added).
    - Otherwise, it will appropriately update the execution pointer/etc.
  - `EXECUTE_YLD`: will cause executeLoop to simply return immediately. Another
    fiber will be run.

