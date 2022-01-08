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

Spor has an 8bit bytecode with a stack-based interpreter. It assembler starts as
extremely primitive but bootstraps itself into a more "full featured"
macro-heavy "language".

The assembly language is stack-based. It also has a builtin dictionary of 32bit
values and a register to store the current instr size (1, 2 or 4 bytes).

The syntax looks like

```
// (sfn means "small function" aka no locals)
$c_sfn add1   // [U4] -> [U4]: add one
  #1$L0       // compile literal of 0x1
  %ADD %RET   // add and return

$c_sfn add2 // [U4] -> [U4]: add two
  $xsl  add1 // xsl means "execute small"
  $jmpl add1 // jmpl does a jump, add1 does the return
```

The assembler syntax is obsenely simple and fngi builds on top of it. It
supports the following tokens.

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

Compiling and executing instructions:
- `%` compile the next token's dictionary value as an instruction. Does not
  clear sz bits.
- `^` run the next token's dictionary value as an instruction. This is useful
  for doing small bits of algebra and stack manipulation in the assembly. Does
  not clear size bits.
- `$<token>` gets `<token>` from dictionary and immediately executes it.
  This is an assembly "macro", which must have been previously compiled to the
  heap at the location in asm.

From the above we can now break down this code:
```
$c_sfn add1
  #1$L0
  %ADD %RET
```

- `$c_sfn add1` is a macro defined in `spor/spor.sp`. It simply sets dictionary
  entry "add1" to the current heap location and sets a few global values.
- #1$L0 `#1` puts the value 0x1 on the stack immediately and `$L0` consumes it
  and compiles it as a "small literal" (uses only one byte)
- `%ADD  %RET` compiles the `ADD` and then the `RET` instructions. they are defined in `asm.sp`

For `add2` `xsl` means "eXecute Small Literal". It looks up the address to
execute as a literal. "Small" means the function has no locals stack.

Read the full documentation in [spor/spor.sp](./spor/spor.sp). There is vim
syntax highlighting in `etc/spor.vim`

