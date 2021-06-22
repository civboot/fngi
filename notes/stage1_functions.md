
Fngi has two ways to do input/output when calling a function:
- passing values on the env.dataStack
- passing values within the function's "local variable" space on the env.returnStack

Here is how a function is called (in both assembly and emulated here):
- When a function returns a structure, it will actually take a pointer to the
  return struct in it's input and then mutate the caller's local variable.
  Therefore, space for the function's (non-datastack) return values are
  already allocated in the caller's locals (by the function that had called
  it).
- The caller decrements the return stack (the stack grows down) by the total
  size of:
  - a pointer to the return struct (if it returns non-datastack values)
  - (non-datastack) inputs
  - function locals
  - finally, the address to continue executing (ep) when the function returns
    which will be at the "top" of the stack (lowest in memory)

What does all of this look like? Let's look at the memory layout of a function
after it has been called TODO add source

          104 ...             # ^^^ the callee's stack
          100 &retValue       # pointer to the return struct
          96  arg0: u32       # first argument
          92  arg1: u32       # second argument
          88  local0: u32     # local variable
    sp -> 84  returnAddr      # calee's continued execution pointer

Now, let's look at the psuedo-assembly for calling such a function:

          sub 16, sp      # allocate function's rstack (stack grows down)
          call myFunction # call the function, pushing next address on rstack
          add 16, sp      # drop function's rstack (stack shrinks up)

All of this is to say that our definition for "return" is simply to
pop the last value into the ep. The compiler will insert the other
operations around the return call.
