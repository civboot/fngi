# Spore


## Spore Assembly
Spore has a 16bit bytecode and a very primitive assembler, from which it
bootstraps itself into a more "full featured" stack based language.

A single 16bit spore instruction speficies a size to use of 1, 2 or 4 bytes and
can perform multiple orthoginal operations:
- immediate load (of 16bit value), memory fetch, memory store, etc
- logical operation (add, subtract, eq, etc)
- jump, jump table, call or return

The assembly language is stack-based. It also has a builtin dictionary of 32bit
values and a register to store the currently built instruction. The syntax looks
like this:

```
$loc add1
  .2 IMWS ADD RET; #1,

$loc add2
  .2 IMWS CALL; @add1,
  .2 IMWS CALL; @add1,
  RET;
```

Let's break that down a little. Spore (the assembly and the lanugage) supports
three token groups:
- special single characters: `~ ' $ . ( )`
- alphanumeric characters (case sensitive): `0-9 a-b A-Z`
- other symbols
- whitespace will separate any tokens.

So `**abc` will be two tokens, `**` and `abc`. But special characters are always
a single character tokens so `$$abc` is three tokens `$`, `$` and `abc`.

The assembler supports the following native tokens.

Pushing and setting stack values:
- `/` starts a line comment.
- `.N` sets the size to N bytes, i.e. `.4` set's the instruction size to 4
  bytes.
- `#NN` pushes a hex number to the stack. The size is controlld by `.`. For
  instance `.2 #12345` would be the hex number `0x2345` (2 bytes/16bit value).
- `=<token>` set's the dictionary entry for `<token>` to the value on the stack.
  i.e. `#42 =foo` would set foo to 0x42.
- `@<token>` get's the dictionary entry for <token>`. I.e. `@foo` would put 0x42
  on the stack (with above set).
- `~<token>` forget all items in dictionary until and including `<token>`.
- `,` pops a value of size controlled by `.` and writes it to the heap.

Compiling an executing instructions:
- `<non-native token>` any non-native token is interpreted as a dictionary
  lookup to mask and set the global instruction register. Asm instructions
  are defined with a 16bit mask in the high two bytes and 16bit instruction
  in the low 16bits, i.e. `#003F_0020 =ADD`. Using just a plain `ADD` will
  mask and set the instruction.
- `;` compile the current instruction register to heap and clear it (but don't
  clear size bits).
- `^` run the current instruction register. This ignores JMP. This is useful for
  doing small bits of algebra and stack manipulation in the assembly.
- `$<token>` get `<token>` from dictionary and immediately begin executing it.
  This is the assembly's "macro" language.

From the above we can now break down this code:
```
$loc add1
  .2 IMWS ADD RET; #1,
```

- `$loc add1` is a macro defined in `asm2.sa` which defines a location.
  It is basically shorthand for `.4
