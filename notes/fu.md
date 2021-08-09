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

## Stacks
The following are considered "register stacks", meaning they can be updated
simulationiously with memory read/writes or even with eachother.

The rules are:
- a single instruction can theoretically access any number of the registers,
  register stacks or an immediate value.
- a single instruction it can only ever access memory once
- a single instruction can only push up to one value on a specific stack, or
  remove up to two.

This allows the fu virtual machine to be implemented in highly performant
hardware at some point in the future.

Register stacks include:
- `WS`: 1KiB (2^10) bytes "Working Stack" for non-immediate operations. Values
  are popped from the stack for operations (if W is specified in Mem) and
  pushed to the stack as the result of operations.
- `RS` 256 (2^8) bytes "Return Stack" for call/return with a variable
  bit-length array for tracking sector (we'll get to this) as well as parallel
  256 bytes for stack-size tracking.

## Registers
The mem operation (see Binary layout) controls memory access. fu has very few
registers:

- `SE`: sector register of variable bit-length denoting the memory sector of a
  16 bit half-pointer.
- `LP` locals pointer, addr-bit register is used for local values with the
  targeting language. It automatically decrements on CALL and increments on
  RET (covered later)
- `EP`: execution pointer


## Device Operations
Fu steals the DEI and DEO operations from the uxn project. Devices allow the
implementer to create standard hardware interfaces that are extremely simple to
implement and interract with from the bytecode. Devices work similar to Unix
Sockets, you can Input/Ouput (DEI/DEO) with them in units of U8, U16 or
U32.

A device and port (dport) is specified with a 16 bit value. 12 bits selects the
device with the remaining 8 bits selecting the port.

## Memory Operations
Even 32bit fu supports 16 bit memory access and function calls through the use
of the sector register (SE), which gets stored on the return stack (RS)
alongside the addresses to return to. The sector gets updated anytime there is
a jump or call into a 32 bit address, or a RET. All functions within a sector
can address memory and other local functions using 16 bit address space.

When compiling for a 32bit fu machine it is important to separate the functions
in this way... or never use 16bit addressing (a significant loss of
performance).


## Fu32 Binary layout
A fu32 instruction is 32 bits. The address space and operations are all a
maximum of 32 bits. Note also that there is a fu8 instruction set which follows
many of the same conventions and can be peep-hole optimized into fu32
instructions.

A single fu32 instruction can specify up to **three different operations that
(might) all happen within a single clock cycle** on a suitably built machine.
The approach is inspired by the [J1
microprocessor](https://excamera.com/files/j1.pdf) with the desire to make a
more general-purpose CPU. For demonstration, `#4200 U32 RET SRSE ADD` will:
- Add two unsigned 32 bit (U32) values on the stack
- Store their value at the 16bit hex address 0x4200
- Return, which not only continues executing from the previous function but
  also updates the working stack pointer and the sector pointer.

fu32's byte layout is as follows:

- I: 16 bit immediate value
- S: 2 bit size mode
- J: 3 bit jump mode
- M: 4 bit memory access mode
- X: 1 bit unused
- O: 6 bit operation

```
                      Jump
  Immediate      Size |   Mem    Op
  IIIIIIIIIIIIIIII SS JJJ MMMM X OOOOOO
```

### Immediate
The immediate value is a 16 bit value which is part of the instruction. It can
be interpreted either signed or unsigned depending on the operation.

Large immediate values can be loaded in a single instruction using the
appropriate mem mode.

### Size
The size affects the type of opcode performed. It does not affect how memory is
accessed.

- 0: 8bit
- 1: 16bit
- 3: 32bit
- 4: undefined

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
Name    Top       Second  Store     Description
--------------------------------------------------------
WS      WS0       WS1     WS        Working Stack
IMWS    IM        WS0     WS        IMmediate Working Stack

FTLP    FT(SP+IM) WS0     WS        FeTch Locals Pointer
SRLP    WS0       WS1     ST(SP+IM) StoRe Locals Pointer

FTSE    FT(SE+IM) WS0     WS        FeTch from SEctor offset
SRSE    WS0       WS1     ST(SE+IM) StoRe to SEctor offset
SROI    IMM       WS1     ST(WS0)   StoRe Operate Immediate
LDOI    FT(WS0)   IMM     WS        LoaD Operate Immediate
```

### Jump

A Jump involves:
- Seting EP to the address
  - If the address is 16bit then EP = SE+address.
  - If the address is 32bit then update SE to the new sector.
- Execution continues at new EP

The following Jump modes are possible:

- x00: NOJ: No Jump, do not perform jump (but do the rest of the operation)
- x01: JST: Jump to STore. Consumes store.
- x02: JIB: Jump to Immediate if Bool(store)
- x03: CALL: Call an address
  - push EP onto RS, including current SE.
  - fetch 16bit value at store, grow WS by that amount and push onto parallel RS
  - jump to store+2 (skipping WS size)
- x04: RET: return
  - pop address and WS growth from RS.
  - shrink WS by grown amount
  - jump to address
- x05: CNW: Call an address without a working stack update. Does not require
  memory read.

(future extensions):
- x06: JWB: Jump to WS if Bool(store)

Some constraints:
- NOJ has no constraints.
- All others besides RET must have mem.Store=WS
- CALL cannot be used with any fetch or store mem/operations, as it requires
  a fetch. Note that CNW would not have this constraint.
- JIB cannot be used with any `mem` instructions that use IM, as it is needed
  for the jump.
- (maybe) RET and CALL cannot be used with REG=RS operations.

### Operations
> (~31 so far)

Operations modify what will be Stored. If Store is WS the result will
be pushed to the stack, if the store is `S(SP+IM)` then the result will
instead go to the immediate offset of the stack pointer. Jmp operations
may also consume what is Store -- it doesn't matter to the op.

Unary: only top is used or consumed for these.
- `IDN`: identity, simply store Top. This can be a NOOP if Top=WS
- `DRP`: drop Top by consuming it but not storing it
- `INV`: inverse bitwise
- `NEG`: two's compliment
- `EQZ`: 1 if TOP=0, else 0
- `EQZ_NC`: 1 if TOP=0, else 0. Do not consume Top.

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
- `LE_S: l <= b` interpreted as signed
- `LE_U: l <= b`
- `LE_S: l <= b` interpreted as signed

- `ADD` `l+r`
- `SUB` `l-r`
- `MOD` `l%r`
- `MUL` `l*r`
- `DIV_U` `l/r`
- `DIV_S` `l/r` interpreted as signed
- `OR` `l bitor r`
- `XOR` `l bitxor r`
- `SHL` `shiftLen << value`
- `SHR` `shiftLen >> value`

Store operations ignore `size` to pull the addr as a `Ptr` size.
- `LD`: `{addr: Ptr} -> value`
- `SR` `{addr: Ptr; value}`

Device operations ignore size for the dport.
- `DEI` `{dport: U16}` DeviceIn, get the value at the device port.
- `DEO` `{dport: U16; value}` send the value to the device port.

> Note: Not supported are `swap`, `rotX`, ... since they require pushing more
> than 1 value to the stack.


## Fu8
8bit fu has the same basic layout as 32bit, except the first 2/3 bits select
what _part_ of the 32bit operation is being executed. The purpose of fu8 is:
- To allow running on 8/16 bit embedded systems (supporting only that subset of
  `size`)
- To allow compilers to emit fu8 and then use peephole optimization to trivially
  convert it to fu32. Peephole optimization is when the optimizer runs over bytecode
  and combines/alters operations to be more efficient but equivalent operations.

```
11   | 11   | 2size | jump
11   | size | mem
size | operation
```

2size is from the stack and is one of:
- 0: 16 bit jmp value
- 1: 32 bit jmp value



### Mem
Below is the fu32 description and what it instead does in fu8.

Note that IMM, instead of being inside the instruction, denotes data following
the current instruction. The number of bytes is determined by the size.


- WS: is a NOOP in fu8
- IMWS: push IMM onto WS
- FTLP: Fetch value at `SP+IM` onto WS
- SRLP: Store WS0 at `SP+IM`
- FTSE: Fetch value at `SE+IM` onto WS
- SROI: causes trap in fu8
- LDOI: causes trap in fu8

### Jump

- nojmp: is a noop in fu8


### Fu8 Alignment Requirements
Note: there is no alignment requirements on Imediate values following IMM
instructions.

The header of a binary fu8 file contains the big/little endianness.  If the
current endianness is non-compliant it can easily be changed by scanning the
file and updating the values, then updating the header. Otherwise, code should
be compiled in such a way so that endianness doesn't mater. This means that
data load/store of native data should not depend on byte order.

For working stack operations the implementation ensures that byte order doesn't
matter. Emulators can accomplish this by growing the stack down for little
endian and growing it up for big endian. This means that the following is
always accurate no matter the system.

```
( 1234 + 5678 )
IMWS 12 IMWS 34 IMWS 5678 ADD16
```


