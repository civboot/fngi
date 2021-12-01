The execution model of fngi is webassembly. All AST nodes compile down to wasm
and const/macro ones are "folded" by executing them. This has several advantages:

- Can compile AST nodes directly to wasm bytecode.
- Direct use of wasm standard and conventions to help reduce the design space
  and aid in conceptual execution.
- The function-local stack allows for calls like
  `foo$native{popU32$(),popI16$()}`. If arguments were passed on the stack,
  `popI16$()` would need to be run before `popU32$()` since the left-most item
  is at the "top" of the stack. Simply reading the AST and reversing it is not
  an option either: that would affect the order in which values are popped
  _off_ the stack (or other state if using a different function). By passing
  values in via function locals, the interpreter can simply popU32$() first,
  inserting into index0, then popI16$() and insert into index1.

There are 2 stacks the interpreter must keep track of:

- The wasm native data stack used for passing data between native instructions.
- The locals stack which holds:
  - non-wasm local variables/fn arguments
  - wasm local variables
    - In interpreted mode, this must be an `arr[_ U16]` lookup table into the actual
      offsets. This is because wasm requires monotomoically incrementing local ids,
      but the local variables can be of size 4 or 8 bytes (i.e. u32 vs u64).
      When compiled to native (non-wasm) bytecode, the lookup table can be
      discarded since the offsets can be computed instead.


When executing a funcidx, the interpreter must:
- bump the locals stack for:
  - the non-wasm locals size (aligned)
  - the wasm locals (including paramters) size (automatically aligned)
  - the wasm locals index lookup (aligned)
- pop values from the stack and insert into the wasm paramaters (i.e. locals start)
- call the function.
- when the function returns it must unwind the stack appropriately.

