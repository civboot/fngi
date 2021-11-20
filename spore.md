# Spore

Spore is a stack-based language similar to FORTH but with some tricks to
make it more readable. It is bootstrapped from a [Spore Assembler](#spore-assembly)
and can use spore assembly (from now on called just "asm") inline.

For both spore and asm there are three token groups. This is the major
divergence from FORTH:
- special single characters: `~ ' $ . ( )`
- alphanumeric characters (case sensitive): `0-9 a-b A-Z`
- other symbols
- whitespace will separate any tokens.

Examples:
- `**abc` is two tokens, `**` and `abc`
- `$$abc` is three tokens`$`, `$` and `abc`. This is because `$` is a "special
  single character" and can only have a token length of 1.

Like FORTH, spore allows you to define functions that are either compiled or
run immediately and affect compilation (called macros in other languages). Also
like FORTH, spore's functions operate by push/poping from a working stack, while
function calls use a call stack to track address changes.

Spore diverges from FORTH in the following ways:
- Does not follow FORTH naming standards in any way. Typically closer to
  C-naming conventions.
- Addition of PRE(fix) words allowing for writing code in polish notation
  instead of forth's reverse polish notation. Typically symbol functions like
  `+` are PRE whereas alphanumeric functions are not, but can be made PRE using
  `'`, i.e. `'foo #42` to call foo with 0x42 on the stack.
- Far more inclusive support of locals, with a separate locals stack and calling
  mechanisms which automatically allocate space for locals when called and
  deallocate when returned (supported by sponge bytecode with `CALL` and `CNL`).

Example spore function to calculate fibronacci recursively. Also included
is the equivalent C code.

```
// Spore Syntax
fn fib [inp n:U4  ret U4] (
  if(n <= 1) 'ret 0
  'ret 'fib(n - 1) + 'fib(n -1)
)

// C Syntax
int fib(int n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}
```

For those familiar with FORTH the above will seem like excessive syntax that
requires a far-too complicated parser. The truth is the opposite: the parser
for the above is no more complicated than FORTH's. Let's explain a few details
on how the parser/compiler stays extremely minimal:

- `fn` is an IMM function. It compiles the next token using a "variable
  compiler", then compiles the next token as the function body. This is where
  `()` come in.
- `(..)` is not an expression. `(` is simply an immediate function that compiles
  until it encounters the token `)`. This is used in several places.
- `if` compiles the next token to determine the if clause, then the next token
  to determine the body. It inserts appropraite `JZ` instructions around these.
  It can also handle `elif` and `else` tokens (not described here).
- `'` forces `fib` to be a PRE function, so the next token gets compiled
  before it does.
- `-` is already PRE (like most symbols are) so it compiles the next token (`1`)
  before it get's written, making it `n 1 SUB`.


## Spore Assembly
> Note: improved macros are currently being defined in `spore/asm2.sa`. Stay
> tuned.

Spore has a 16bit bytecode and a very primitive assembler, from which it
bootstraps itself into a more "full featured" stack based language.

A single 16bit spore instruction speficies a size (1, 2 or 4 bytes) and can
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
about that in the docs for XL vs XSL in `spore/asm.sa`).

