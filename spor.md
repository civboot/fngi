# Spor

> **Note**: spor requires `spor.c` (or something like it) and then bootstraps
> most of it's "useful" syntax in [`spor.sp`](./spor.sp)

Spor is a stack-based assembler which borrows design princples from FORTH. It
is designed for the following principles:

- Self-bootstrapping in as little native code as possible, while still meeting
  the other goals.
- Support the self-bootstrapping of fngi with shared utilities like a scanner,
  key/U32 dictionary, lean bytecode target, memory model, error (panic)
  handling, native device interface and a simple security model.
- Readability is _not_ a major design goal. It is an assembly language, and thus
  lacks many modern convieniences. None-the-less because it is
  self-bootstrapping, and therefore able to create it's own macros and syntax,
  it can achieve higher readability as it bootstraps.

Spor has an 8bit bytecode with a stack-based interpreter. The assembler starts as
extremely primitive but bootstraps itself into a more "full featured"
macro-heavy "language".

The assembly language is stack-based. It also has a builtin dictionary of 4 byte
values and a register to store the current instr "sz bits" of `SZ1`, `SZ2` or
`SZ4`.

The syntax looks like

```spor
\ (sfn means "small function" aka no locals)
$FN add1  \ {U4 -> U4}: add one
  #1$L        \ compile literal of 0x1
  %ADD %RET   \ add and return

$SFN add2 \ [U4] -> [U4]: add two
  $xx:add1   \ xx performs an "execute small or large"
  $jmp:add1 \ jmp does a jump, add1 does the return
```

The assembler syntax is obsenely simple and fngi builds on top of it. It
supports the following tokens.

Pushing and setting stack values:

- `\` starts a line comment which ends at the newline.
- `.N` sets the sz bits (size) to N bytes, i.e. `.2` set's the global
  instruction size to 2 bytes (`SZ2`).
- `#NNNN_NNNN` pushes an (up to) 4 byte unsigned hex number to the stack. Ex:
  `#1_2345` pushes 0x12345.
- `=<token>` set's the dictionary (value, meta) entry for `<token>` to the value
  on the stack.  i.e. `#42 #0 =foo` would set `foo`'s value to 0x42 and meta to
  0 (aka a constant).
- `@<token>` get's the dictionary value for `<token>`. I.e. `@foo` would put 0x42
  on the stack (assuming it had been set like above).
- `,` pops a value from stack and writes it to heap and increments heap. The
  size is controlled by `.` (note: this is rarely used, pretty much just for
  bootstrapping h1, h2, etc).
- `:` defines a function. This is necessary because the name is stored in the
  same memory space as the function code.

Compiling and executing instructions:

- `%` compile the next token's dictionary value as an instruction. Does not
  clear sz bits (set by `.`).
- `^` run the next token's dictionary value as an instruction. This is useful
  for doing small bits of algebra and stack manipulation in the assembly. Does
  not clear size bits.
- `$<token>` gets `<token>` from dictionary and immediately executes it.
  This is an assembly "macro", which must have been previously compiled to the
  heap at the location in asm.

From the above we can now break down this code:

```spor
$FN add1
  #1$L
  %ADD %RET
```

- `$FN add1` is a macro defined in `spor/spor.sp`. It simply sets dictionary
  entry "add1" to the current heap location and sets a few global values.
- #1$L `#1` puts the value 0x1 on the stack immediately and `$L` consumes it
  and compiles it as a "literal"
- `%ADD  %RET` compiles the `ADD` and then the `RET` instructions. they are
  defined in `boot/constants.sp`

Read the full documentation in [spor.sp](./spor.sp). There is vim syntax
highlighting in `etc/spor.vim`
