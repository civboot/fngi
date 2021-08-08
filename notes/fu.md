# Fu virtual machine

After realizing that wasm doesn't suit the project's boostrapping goals, I'm
trying to branch out. I considered uxn as the target, but there are several issues
with that:

1. I don't think locking myself to 64KiB at this stage is recommended.
2. uxn was designed for writing forth-like machines. Among other things, It has
   no way to load values from a "locals" stack in even a reasonably
   performant/consice way, and 3/4 of it's opcode space is used for keep/return
   operations that I have almost no use for.

However, uxn taught me several things:
1. Having devices built directly into the virtual machine is awesome. I'm totally
   going to implement it for mine.
2. Having your own custom virtual machine is awesome.

So, the fu virtual machine and bytecode is now born. Fortunately I've actually
thought a lot about this from my experience thinking about the J1.

## Registers
The mem operation (see Binary layout) controls memory access. fu has several
registers:

- `WS`: 1KiB (2^10) bytes working stack for non-immediate operations. Values are
  popped from the stack for operations (if W is specified in Mem) and pushed to
  the stack as the result of operations.
- `RS` 256 (2^8) bytes return stack for call/return with a variable bit-length
  array for tracking sector (we'll get to this) as well as parallel 256 bytes
  for stack-size tracking.
- `SE`: sector register of variable bit-length denoting the memory sector of a
  16 bit half-pointer.
- `SP` addr-bit register is used for local values with the targeting language.
  It automatically decrements on CALL and increments on RET (covered later)
- `EP`: execution pointer

With that in mind the basic rule is this: a single operation can theoretically
access any number of the registers or immediate values, but it can only ever
access memory once. This allows the fu virtual machine to be implemented
in highly performant hardware at some point in the future. **The Mem bits are
what controls what memory is accessed.**

## Binary layout
A fu instruction is 32 bits. The address space and operations are all a maximum
of 32 bits (with room to allow 64 bits in the future).

- I: 16 bit immediate value
- M: 5 bit memory access mode
- J: 3 bit jump mode
- S: 2 bit size mode
- O: remaining 8 bits for the operation

```
                   Jump     Size
  Immediate        |   Mem  |  Op
  IIIIIIIIIIIIIIII JJJ MMMM SS ...
```

### Immediate
The immediate value is a 16 bit value which is part of the instruction. It can
be interpreted either signed or unsigned depending on the operation.

### Size
The size affects the type of opcode performed. It does not affect how memory is
accessed.

- 0: 8bit
- 1: 16bit
- 3: 32bit
- 4: reserved for 64bit

### Mem


Definitions:
- F(SE+IM) means "fetch SE+IM" aka "fetch sector + immediate" aka use the value
  at the immediate address in the specified sector. S(...) means "store" at
  that address.
- Top means the top value used in the operation
- Second means the second value used in the operation (may not be used).
- WS0 means top of working stack, WS1 means second value.
- Storing to the WS means pushing values to the top of the WS.

```
Top       Second  Store     Example Forth       Description
-----------------------------------------------------------------------
WS0       WS1     WS        0x1 0x2 ADD ( 0x3)  Add two values on WS
IM        WS0     WS        (IM=1) ADD          increment top of WS

F(SP+IM)  WS0     WS        (IM=4) ADD          add with @local offset=4
WS0       WS1     S(SP+IM)  (IM=4) ADD          store sum @local offset=4

F(SE+IM)  WS0     WS        ADD                 add to value at half-ptr
F(WS0)    WS1     WS        ADD                 get value at top and add
WS0       WS1     S(SE+IM)  ADD                 add and store at half-ptr
IMM       WS1     S(WS0)    ADD                 add Second to IMM, store @WS0

IMM       ???     ???       Register operation
```

The last "Register operations" performs some operation on a register with the
following syntax. The register id is gotten from the lowest 8 bits. The
operation depends on the highest bit:

- high-bit of IMM == 0: Top=REG, Second=WS0, Store=WS
- high-bit of IMM == 1: Top=WS0, Second=WS1, Store=REG

### Jump

The following Jump modes are possible:

- x00: nojmp: do not perform jump
- x02: ret: return
- x03: jmpoff: consume store, jump to offset
- x03: ifjmpimm: consume store, jump to offset=IM if bool(store)
- x04: call: consume store, call address

Some constraints:
- nojmp has no constraints
- ret cannot be used with a Store `@S(SP+...)`
- jmpoff, ifjmpimm and call cannot be used with a Store at _any_ memory address
  (the "Store" must be consumed)
- ifjmpimm cannot be used with any operations that use IM.


"consume store" means that the result of mem is passed into Jump.  If Jump=0
then the "default" action is taken (i.e. values pushed onto the stack, etc).

If jump=2

If Jump=2 then the default action is taken _but_ the EP, SP and RS are all
updated for the return. 

- (Address, StackSize) popped from RS
- EP set to Address
- SP shrunk by StackSize

Note: it is invalid to have Jump != 0 and Store `@S(SP+...))`. It is invalid to
have Jump=1 or 2 and store at any address.


### Operations
> (~30 so far)

Unary: only top is used or consumed for these.
- `IDN`: identity, simply store Top. This can be a NOOP if Top=WS
- `DRP`: drop Top by consuming it but not storing it
- `INV`: inverse bitwise
- `NEG`: two's compliment
- `EQZ`: 1 if TOP=0, else 0
- `EQZ_NC`: 1 if TOP=0, else 0. Do not consume Top

Binary operations are in syntax {r; l} (r i.e. right is at top of stack). This
is so that when fngi-like languages compile them `a+b` gets compiled as
`push$a; push$b; add[T]$()`

- `DRP2`: drop 2. Consume both top and second and store neither
- `OVR:` do not consume top or second, store second

- `EQU: l == b`
- `NEQ: l != b`
- `GT_U: l > b`
- `GT_S: l > b` interpreted as signed
- `GE_U: l >= b`
- `GE_S: l >= b` interpreted as signed
- `LE_U: l <= b`
- `LE_S: l <= b`
- `LE_U: l <= b`
- `LE_S: l <= b`

- `ADD` `l+r`
- `SUB` `l-r`
- `MOD` `l%r`
- `MUL` `l*r`
- `DIV_U` `l/r`
- `DIV_S` `l/r`
- `OR` `l bitor r`
- `XOR` `l bitxor r`

Shift operations have syntas `{shiftLen: U8; value}`
- `SHL` `shiftLen << value`
- `SHR` `shiftLen >> value`

Device operations have syntax `{deviceAndPort: U16; value}`
- `DEI` `{deviceAndPort}` DeviceIn, get the value at the device port.
- `DEO` `{deviceAndPort, value}`, send the value to the device port.

Note: Not supported are `swap`, `rotX`, ... since they require pushing more
than 1 value to the stack.

