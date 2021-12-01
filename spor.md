# Spor

Spor is a stack-based assembler which borrows design princples from FORTH. It
is not designed for the following principles:
- Self-bootstrapping in as little native code as possible, while still meeting
  the other goals.
- Support the self-bootstrapping of fngi with shared utilities like a scanner,
  key/U32 dictionary, lean bytecode target, memory model, error (panic)
  handling, native device interface and a simple security model.
- Readability is _not_ a design goal. It is an assembly language, and thus lacks
  many modern convieniences. None-the-less because it is self-bootstrapping,
  and therefore able to create it's own macros and syntax, it can achieve higher
  readability as it bootstraps.

Spor has a 16bit bytecode with a stack-based interpreter. It assembler starts as
extremely primitive but bootstraps itself into a more "full featured"
macro-heavy "language".

A single 16bit spor instruction speficies a size (1, 2 or 4 bytes) and can
perform multiple orthoginal operations:
- mem: immediate load (of 16bit value), memory fetch, memory store, etc
- operation: logical operation (add, subtract, eq, etc)
- jump: jmp, jump table, call or return

The assembly language is stack-based. It also has a builtin dictionary of 32bit
values and a register to store the currently built instruction. The syntax looks
like this:

```
$loc add1
  .2 LIT ADD RET; #1,

$loc add2
  .2 XSL; @add1,
  .2 XSL; @add1,
  RET;
```

The assembler supports the following native tokens.

Pushing and setting stack values:
- `/` starts a line comment.
- `.N` sets the size to N bytes, i.e. `.4` set's the global instruction size to
  4 bytes.
- `#NN` pushes a 4 byte unsigned hex number to the stack. Ex: `#1_2345` pushes 0x12345.
- `=<token>` set's the dictionary entry for `<token>` to the value on the stack.
  i.e. `#42 =foo` would set `foo` to 0x42.
- `@<token>` get's the dictionary entry for <token>`. I.e. `@foo` would put 0x42
  on the stack (assuming it had been set like above).
- `,` pops a value from stack and writes it to heap. The size is controlled by `.`

Compiling an executing instructions:
- `<non-native token>` any non-native token is interpreted as a dictionary
  lookup to mask and sets the global instruction register. Asm instructions are
  defined with a 16bit mask in the high 2bytes and 16bit instruction in the
  low 2bytes, i.e. `#003F_0020 =ADD`. Using just a plain `ADD` will mask and set
  the current instruction.
- `;` compile the current instruction register to heap and clear it (but don't
  clear size bits).
- `^` run the current instruction register then clear it. This ignores JMP. This
  is useful for doing small bits of algebra and stack manipulation in the
  assembly. Does not clear size bits.
- `$<token>` gets `<token>` from dictionary and immediately begins executing it.
  This is an assembly "macro", which must have been previously compiled to the
  heap at the location in asm.

From the above we can now break down this code:
```
$loc add1
  .2 LIT ADD RET; #1,
```

- `$loc add1` is a macro defined in `asm2.sa`. It simply sets dictionary entry
  "add1" to the current heap location.
- `.2 LIT ADD RET` is the components of our instruction. `.2` is the size in
  bytes, `LIT` is the memory (literal), `ADD` is the operation and `RET` causes
  a return. They update the global instruction.
- `;` compiles the global instruction (writes to heap) and then clears it (but
  does not change sz).
- `#1,` pushes the hex value `1` to the stack and `,` compiles it to the heap.
  It's the immediate value referenced by the `LIT` in the previous instruction.

For `add2` `XSL` means "eXecute Small Literal". It looks up the address to
execute as a literal. "Small" means the function has no locals stack. (read more
about that in the docs for XL vs XSL in `spor/asm.sa`).

