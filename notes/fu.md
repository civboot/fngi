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

- `CP`: SeCtor Ptr register of variable bit-length denoting the memory sector of a
  16 bit half-pointer.
- `LP` locals pointer, addr-bit register is used for local values with the
  targeting language. It automatically decrements on CALL and increments on
  RET (covered later)
- `EP`: execution pointer

## Types
U8, U16 and U32 are used to specify the bit-width regardless of whether
they are signed or unsigned (the operation determines signedness).

CPtr means a "half pointer", aka a 16bit pointer. APtr is an "absolute
pointer" aka a 32bit pointer. Typically only immediate (compile-time) pointers
are SPtr's, so Ptr also means APtr.

On 16bit systems, an SPtr and APtr must have the same size of 16bits and the
sector is always "0".

## Device Operations
Fu steals the DEI and DEO (renamed DVF/DVS for DeVice Fetch/Store) operations
from the uxn project. Devices allow the implementer to create standard hardware
interfaces that are extremely simple to implement and interract with from the
bytecode. Devices work similar to Unix Sockets, you can Input/Ouput (DVF/DVS)
with them in units of U8, U16 or U32.

A device and port (DvPort) is specified with a U16. If the high bit is not 1
(register mode described below) then the lowest 8 bits are the devices port
and the upper 11 bits are the device to select. DVF and DVS are used to
read/write values of the specified size to the device, which are implemented
by either the runtime or registered by the software.

> TODO: specify standard device ports.

**Register Mode**

If the high bit is 1 then register mode is activated.  The remaining 15 bits of
`DvPort` will be split into a 4 bit register selector and an 11 bit signed
offset. DVF will read the value of the reg+Offset, DVS will write
`value+offset` to the register.

```
  Reg  SignedOffset
1 RRRR III IIII IIII
```

The registers are:
- 0-9: general purpose value registers. They must hold USize values, and can be
  used for implementation specific uses (like holding temporary values or
  implementing extra stacks).
- A 1010 reserved
- B 1011 reserved
- C 1100 EP: the execution pointer register. Writing to this traps.
- D 1101 AP: the alocator pointer register. This should point to the "global"
  allocator currently being used. The API for such an allocator will be
  implementation specific.
- E 1110 CP: the CPctor ptr.
- F 1111 LP: the Locals ptr register.

> Note that implementors don't have to actually store these in hardware
> registers: they can just as easily go in non-accessible memory.

By pulling the offset from the immediate, it is possible to get a SP+offset in
a single fu16 instruction. This dramatically reduces the execution time
of calling functions, where you frequently want to pass pointers to the local
stack.

## Memory Operations
fu16 supports 16 bit memory access and function calls through the use
of the sector register (CP), which gets stored on the return stack (RS)
alongside the addresses to return to. The sector gets updated anytime there is
a jump or call into a 32 bit address, or a RET. All functions within a sector
can address memory and other local functions using 16 bit address space.

When compiling for a 32bit fu machine it is important to separate the functions
in this way... or never use 16bit addressing (a significant loss of
performance).


## fu16 Binary layout
A fu16 instruction is 16 bits, with a possible 16bit immediate value following
it. The address space and operations are all a maximum of 32 bits. Note also
that there is a fu8 instruction set which follows many of the same conventions
and can be peep-hole optimized into fu16 instructions (described later).

A single fu16 instruction can specify up to **three different operations that
(might) all happen within a single clock cycle** on a suitably built machine.
The approach is inspired by the [J1
microprocessor](https://excamera.com/files/j1.pdf) with the desire to make a
more general-purpose CPU. For demonstration, `00 RET SRCP ADD 4200` will:
- Add two unsigned 32 bit (00) values on the stack
- Store their value at the 16bit address 0x4200 offset by the CP (sector ptr).
- Return, which not only continues executing from the previous function but
  also updates the working stack pointer and the sector pointer.

fu16's byte layout is as follows:

```
 |-- 10b Mode ----|- 6b Op -|
      Jump Unused
  Size   | |  Mem   Operation
    SS JJJ XX MMM   OO OOOO
```

### Immediate
The immediate value is a 16 bit value which must follow the instruction if
the instruction requires an IM. It can be interpreted either signed or unsigned
depending on the operation.

Only 16bit immediates are supported. 32bit constants can be pushed to the stack
by LoaDing them from sector memory or pushing two 16bit constants. The former
is typically more compact, while the later can be faster on memory-bottlenecked
systems. Sector reads are typically preferred.

### Size
The size affects the type of opcode performed. For instance ADD8 will add two
8bit numbers, ADD16 will add two 16bit numbers. Certain components of
operations, like addresses, are unnaffected by size.

- 0: 32bit
- 1: 16bit
- 3: 8bit
- 4: undefined

### Mem
Definitions:
- FT(CP+IM) means "fetch CP+IM" aka "fetch sector + immediate" aka use the
  value at the immediate address in the specified sector. ST(...) means "store"
  at that address.
- Top means the top value used in the operation.
- Second means the second value used in the operation (may not be used).
- WS0 means top of working stack, WS1 means second value of working stack.
- Storing to the WS means pushing values to the top of the WS. Some operations
  may have also consumed 1 or more values from the WS.

```
x bin Name    Top       Second  Store     Description
--------------------------------------------------------------
0 000 SRLP    WS0       WS1     ST(SP+IM) StoRe Locals Pointer
1 001 SRCP    WS0       WS1     ST(CP+IM) StoRe to seCtor Ptr offset
2 010 SROI    IM        WS1     ST(WS0)   StoRe Operate Immediate
3 011 FTLP    FT(SP+IM) WS0     WS        FeTch Locals Pointer
4 100 FTCP    FT(CP+IM) WS0     WS        FeTch from seCtor Ptr offset
5 101 FTOI    FT(WS0)   IM      WS        FeTch Operate Immediate
6 110 IMWS    IM        WS0     WS        IMmediate Working Stack
7 111 WS      WS0       WS1     WS        Working Stack
```

### Jump

A Jump involves:
- Seting EP to the address
  - If the address is 16bit then EP = CP+address
  - If the address is 32bit then update CP to the new sector
- Execution continues at new EP

For all jumps, a jump of size=U8 will trap, U16 is interpreted as an offset
from the sector, U32 is an absolute jump.

The following Jump modes are possible:
- 0 000 JIB: Jump to Immediate if Bool(store)
- 1 001 CALL: Call an address
  - push EP onto RS, including current CP.
  - fetch 16bit value at store, grow WS by that amount and push onto parallel RS
  - jump to store+2 (skipping WS size)
- 2 010 JST: Jump to STore. Consumes store.
- 3 011 CNW: Call an address without a working stack update. Does not require
    memory read.
- 4 100 reserved
- 5 101 reserved
- 6 110 RET: return
  - pop address and WS growth from RS.
  - shrink WS by grown amount
  - jump to address
- 7 110 NOJ: No Jump, do not perform jump (but do the rest of the operation)

Some constraints on jump modes:
- NOJ and RET have no constraints.
- All others must have mem.Store=WS so they can consume it.
  - TODO: It's possible they could consume Second in this case, more thought
    must be aplied.
- JIB can ONLY be used with mem=WS as it requires the IMM for it's
  offset (all mem operations besides WS use an IMM)
- CALL can ONLY be used with mem={WS, IMWS}. It also can't be used with
  operations FT or SR as it must perform a fetch (only one memory operation
  allowed).

### Operations: 6 bits

*Special Operations*:

Operations modify what will be Stored. If Store is WS the result will
be pushed to the stack, if the store is `S(SP+IM)` then the result will
instead go to the immediate offset of the stack pointer.

Store operations ignore `size` to pull the addr as a `Ptr` size. They
can not be used with CALL, as that requires a memory operation.
- `00 000000 FT {addr: Ptr} -> value` load. 
  - Can only be used with mem=WS
- `01 000001 SR {value; addr: Ptr}` store. Note that the address is Second,
  allowing for storing an IMM value with IMWS.
  - Can only be used with mem of {WS, IMWS}

Device operations ignore size for the DvPort, which is always U16 (See
**Device Operations** for more details and clarifications).
- `02 000010 DVF` `{dvPort: U16}` DeviceIn, get the value at the dvPort
- `03 000011 DVS` `{dvPort: U16; value}` send the value to the dvPort

Unary: only top is used or consumed for these.
- `04 000100 IDN`: identity, simply store Top.
- `DRP`: drop Top by consuming it but not storing it
- `INV`: inverse bitwise
- `NEG`: two's compliment
- `EQZ`: 1 if TOP=0, else 0
- `EQZ_NC`: 1 if TOP=0, else 0. Do not consume Top.

Binary operations are in syntax {r; l} (r i.e. right is at top of stack). This
is so that when fngi-like languages compile them `a+b` gets compiled as
`push$a; push$b; add[T]$()`:
- `DRP2` drop 2. Consume both top and second and store neither
- `OVR` do not consume top or second, store second
- `ADD` `l+r`
- `SUB` `l-r`
- `MOD` `l%r`
- `MUL` `l*r`
- `DIV_U` `l/r`
- `DIV_S` `l/r` interpreted as signed
- `OR` `l bitor r`
- `XOR` `l bitxor r`

Shift operations are type `{shiftLen; value}`, allowing the shift length to be
an immediate.
- `SHL` `shiftLen << value`
- `SHR` `shiftLen >> value`

- `EQU: l == b`
- `NEQ: l != b`
- `GE_U: l >= b`
- `GE_S: l >= b` interpreted as signed
- `LT_U: l < b`
- `LT_S: l < b` interpreted as signed

It is possible these comparison operations won't be implemented, as they are
not strictly necessary. The same logic can be achieved in the same cycles using
the above by simply reversing argument order when necessary.
- `GT_U: l > b`
- `GT_S: l > b` interpreted as signed
- `LE_U: l <= b`
- `LE_S: l <= b` interpreted as signed


## Fu8
fu8 (8bit instruction) has the same basic layout as fu16, except the first 2/3
bits select what _part_ of the 16bit instruction is being executed. The purpose
of fu8 is:
- To enable more easily being emulated on 16 bit embedded systems.
- Easier to implement a virutal machine for.
- To allow compilers to emit fu8 and then use peephole optimization to trivially
  convert it to fu16. Peephole optimization is one of the simplest optimizers
  to write. It involves runing over bytecode and combining/altering operations
  to be more efficient but equivalent operations.


Byte layout:
```
11   | 11   | 1size | jump
11   | size | X | mem
size | operation
```

1size is a 1 bit size value and is one of:
- 0: 32 bit
- 1: 16 bit

Like fu16, all immediates must be 16bit EXCEPT mem.IMWS with size=8, which can be
an 8 bit immediate. 32bit immediates are still not allowed, as they would
diverge from fu16 and make peephole optimization impossible.

Fu8 is intended to be the "cross platform" bytecode for fu. It should be able
to run anywhere with minimal modifications. This is not true for fu16, which
depends on endianness (see Alignment section for details).

### Mem
Below is the fu16 description and what it instead does in fu8.

- SRLP: Store WS0 at `SP+IM`
- SRCP: Store WS0 at `CP+IM`
- SROI: causes trap in fu8
- FTLP: Fetch value at `SP+IM` onto WS
- FTCP: Fetch value at `CP+IM` onto WS
- FTOI: causes trap in fu8
- IMWS: push IMM onto WS
- WS: causes NOOP in fu8

### Jump
Unlike fu16, there are no restrictions on jump instructions in fu8

- NOJ: is a noop in fu8
- JST: jumps to WS
- JIB: jumps to IMM if Bool(WS)
- CALL: calls WS, updating LP appropriately
- RET: returns
- CNW: calls WS

### Operations
fu8 operations are identical to fu16 ones, except they are always
pulled from and stored to the WS.

### Fu8 Alignment Requirements
There is no alignment requirements on Imediate values following IMM
instructions. For constants (on memory load), there is alignment requirements,
but all values of the same type must be packed together in their respective
sectors. The header of a binary fu8 file contains the big/little endianness setting
for the binary.  It also contains offsets for all of the global values in each
sector, which must be broken into data type sections. It looks like this (all
numbers are hex, `//` means a comment).

```
// Constant Header Section
0000 // sector number
0100 // offset for beginning of 32bit values
0200 // offset for end of 32bit values, beginning of 16bit values
0332 // end of 16bit values
0001 // next sector number
// ...
```

If the current endianness is non-compliant it can easily be changed by scanning
the file and updating the values, then updating the header. Otherwise, code
should be compiled in such a way so that endianness doesn't mater. This means
that load/store of native data should not depend on byte order.

For working stack operations the implementation ensures that byte order doesn't
matter. Emulators can accomplish this (while preserving native load/stores of
the emulated stack) by growing the stack down for little endian and growing it
up for big endian. This means that the following is always accurate no matter
the system.

```
( Result: 12 since 34 is dropped )
IMWS 1234
DRP8 // Drops 34

( Result: 1234 + 5678 = 6912 )
IMWS8 12
IMWS8 34
IMWS16 5678 ADD16
```
