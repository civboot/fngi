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

Fu has two goals, in order:
- Develop a virtual machine that can run on either 16 or 32 bit systems
  performantly. This is called fu16.
- (once done) be able to backfill that into a binary format (fu8) that can be
  shipped and recompiled easily (like wasm but designed to run directly without
  a large compiler).

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
more general-purpose CPU. For demonstration, `1,SRCP,ADD,RET #4200` will:
- Add two unsigned 32 bit (4 byte) values on the stack
- Store their value at the 16bit address 0x4200 offset by the CP (sector ptr).
- Return, which not only continues executing from the previous function but
  also updates the working stack pointer and the sector pointer.

fu16's bit layout is as follows:

```
 |-- 10b Mode ----|- 6b Op -|
      Jump Unused
  Size   | |  Mem   Operation
    SS JJJ XX MMM   OO OOOO
```

The assebmly syntax is similar to the mental model of the order things execute
in:

```
<size in bytes>,<mem>,<op>,<jmp>
```

Each has defaults:
- size in bytes: 0, aka APtr size
- jmp: NOJ
- mem: WS
- op: NOP

Other assembly syntax:
- `/ line comment`
- `#NN` inserts a 8bit unsigned hex number, i.e. `#1F`
- `#NNNN` inserts a 16bit unsigned hex number, i.e. `#001F`
- `#NNNN_NNNN` inserts a 32bit unsigned hex number, i.e. `#001F_4200`
- `=<name>` store's name's current location. Can override old values.
- `@N<name>` inserts name's location (n-byte value).
- `!N<name>` sets an n-byte value at name's location equal to the current
  location. Used for lookahead jumps and defining functions late.
- `"<ascii>` creates an ascii string until newline, there are no escapes.
- `%<hex>` creates a binary string until newline using two digit hex values,
  whitespace is ignored. i.e. `00 12 F3`
- `>` ptr-align the current location.
- `]` or an EOF character ends the assembly parsing. Used when embedding fu
  assembly in other languages but still using the native assembler.

Example to add a byte to 42 and return
```
1,IMWS,ADD,RET   #0042
```

Example forward jump if zero.
```
// if top=0 will jump to where ifZero is set.
,,,JZ =ifZero #0000
// code if not zero
!2ifZero
```

### Immediate
The immediate value is a 16 bit value which must follow the instruction if
the instruction requires an IM. It can be interpreted either signed or unsigned
depending on the operation.

Only 16bit immediates are supported. 32bit constants can be pushed to the stack
by LoaDing them from sector memory or pushing two 16bit constants and using
math to join them. The former is typically more compact, while the later can be
faster on due to cache coherency.

### Size
The size affects the type of opcode performed. For instance ADD8 will add two
8bit numbers, ADD16 will add two 16bit numbers. Certain components of
operations, like addresses and device operations, are unnaffected by size.

- 0: 8bit
- 1: 16bit
- 2: 32bit
- 4: undefined

### Mem

All memory operations are in the table below. Keep reading for
further information.

```
  bin Name    Top       Second  Store     Description
--------------------------------------------------------------
0 000 SRLP    WS0       WS1     ST(LP+IM) StoRe LocalsPtr
1 001 SRCP    WS0       WS1     ST(CP+IM) StoRe to seCtorPtr offset
2 010 SROI    IM        WS1     ST(WS0)   StoRe Operate Immediate
3 011 FTLP    FT(LP+IM) WS0     WS        FeTch from LocalsPtr offset
4 100 FTCI    FT(CP+IM) WS0     WS        FeTch from seCtorPtr offset
5 101 FTOI    FT(WS0)   IM      WS        FeTch Operate Immediate
6 110 IMWS    IM        WS0     WS        IMmediate Working Stack
7 111 WS      WS0       WS1     WS        Working Stack
```

The Mem bits define how memory is used for the operation. There are 8
possibilities, describing them in reverse order (simplest to most complex):
- WS: WS means "Working Stack". All values are gotten from the WS and values
  are pushed to the working stack.
- IMWS: IM means "IMmediate". IMWS uses the 16bit immedaite value (after the
  current operation) as the "top" value and the working stack as the "second".
  It stores it's value on the working stack.
- FTOI: means "FeTch Operate Immediate". It fetches the value at the address
  on the working stack and operates on it using the immediate value.
- FTCI: means "FeTch seCtor pointer Immediate". It fetches from immediate
  with an offset of the sector pointer. It stores on the working stack.
- FTLP: means "FeTch LocalsPtr". It uses the immediate as the offset to
  fetch from the function locals. It stores on the working stack.
- SROI: means "StoRe Operate Immediate". It uses the immediate value for
  the operation and uses WS1 as the "second". It stores the value
  at the address stored in WS0.
- SRCP: means "StoRe seCtorPtr". It uses the WS for the operation but then
  stores at the immediate value offset by the sector pointer.
- SRLP: means "StoRe LocalsPtr". It uses the WS for the operation but
  stores at the immediate value offset by the function locals.

Cheatsheat for above table:
- FT(CP+IM) means "fetch CP+IM" aka "fetch sector + immediate" aka use the
  value at the immediate address in the specified sector. ST(...) means "store"
  at that address.
- Top means the top value used in the operation.
- Second means the second value used in the operation (may not be used).
- WS0 means top of working stack, WS1 means second value of working stack.
- Storing to the WS means pushing values to the top of the WS. Some operations
  may have also consumed 1 or more values from the WS.


### Jmp

A Jmp (Jump) involves:
- Seting EP to the address
  - If the address is 16bit then EP = CP+address
  - If the address is 32bit then update CP to the new sector
- Execution continues at new EP

For all jumps, a jump of size=U8 will trap, U16 is interpreted as an offset
from the sector, U32 is an absolute jump.

The following Jump modes are possible:
- 0 000 JZ: Jump to Immediate if store=zero
- 1 001 CALL: Call an address
  - pop ptr off of store, convert to APtr using CP if necessary.
  - fetch 16bit growWs value at ptr.
  - grow WS by growWs.
  - push `EP+INSTR_WIDTH` onto RS, including current CP and amount WS grew.
  - jump to ptr+2 (skipping WS size)
- 2 010 JST: Jump to STore. Consumes store.
- 3 011 CNW: Call an address without a working stack update. Does not require
    memory read.
- 4 100 JTBL: consume StoRe and jump to the jump table which uses the IMM for
  it's size.
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
- JZ and JTBL can ONLY be used with mem=WS as they require the IMM for their
  offset/table-meta (all mem operations besides WS use an IMM)
- CALL can ONLY be used with mem={WS, IMWS}. It also can't be used with
  operations FT or SR as it must perform a fetch (only one memory operation
  allowed).

#### `JTBL` Jump Table Description

The jump table must be layed out as follows:

- Instr containing `JTBL`
- `IM` 16bit value containing the size of the jump table.
- array of 16 bit values containing the sectorPtr to jump to.
  - index0 must contain the location that is jumped to if the store value is 1.
    A value of 0 will begin execution immediately after the jmp table and the
    final index is the "else/default" branch.


### Operations: 6 bits

Operations modify what will be Stored. If Store is WS the result will
be pushed to the stack, if the store is `S(LP+IM)` then the result will
instead go to the immediate offset of the stack pointer.

**Special Operations**:

Special store operations ignore `size` or require size to be a specific value.

Load/Store can not be used with CALL, as that requires a memory operation. They
also will always pull a ptr for their ptr argument.
- `01 FT {addr: Ptr} -> value` load.
  - Can only be used with mem of {WS} and size=ptrSize
- `02 SR {addr: Ptr, value}` store. Note that the address is Second,
  allowing for storing an IMM value with IMWS.
  - Can only be used with mem of {WS; IMWS}

Device operations can only work with size=U16. They will update the stack
differently per operation, ignoring the size bits. See **Device Operations**
for more details and clarifications.
- `03 DVF` `{dvPort: U16}` DeviceIn, get the value at the dvPort
- `04 DVS` `{...; dvPort: U16}` send the value to the dvPort.

**Non-special operations**: these use size normally.

Unary: only top is used or consumed for these.
- `04 000100 NOP`: no operation/identity. Simply store Top
- `DRP`: drop Top by consuming it but not storing it
- `INV`: inverse bitwise
- `NEG`: two's compliment
- `EQZ`: 1 if TOP=0, else 0
- `EQZ_NC`: 1 if TOP=0, else 0. Do not consume Top.

Binary operations are in syntax {l; r} (r i.e. right is at top of stack). This
is so that when fngi-like languages compile them `a+b` gets compiled as
`push$a; push$b; add[T]$()`:
- `DRP2` drop 2. Consume both top and second and store neither.
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

## Device Operations
Fu steals the DEI and DEO (renamed DVF/DVS for DeVice Fetch/Store) operations
from the [uxn](https://wiki.xxiivv.com/site/uxn.html) project. Devices allow
the implementer to create standard hardware interfaces that are extremely
simple to implement and interract with from the bytecode. Devices work similar
to Unix Sockets, you can Input/Ouput (DVF/DVS) with them in units of U8, U16 or
U32.

A device and port (DvPort) is specified with a U16. If the high bit is not 1
(register mode described below) then the lowest 8 bits are the devices port
and the upper 11 bits are the device to select. DVF and DVS are used to
read/write values of the specified size to the device, which are implemented
by either the runtime or registered by the software.

Note that device operations are more like a function call than an instruction.
They may take more than one clock cycle and may consume or return multiple
stack values. They also completely ignore the `size` bits in the instruction.
They are a convienient way for the implementation system to define important
interfaces between fu and itself, allowing for important functions to be
implemented in the implementation language (which may be assembly) and later be
overriden by a fu implementation.

In general, writing an APtr to port 0xFF of a Device will register it. The
written APtr must point to a function that accepts the `DPort` and performs
the necessary operations. However, note that many devices will be implemented
(and therefore registered) by the implementation language of fu. Reading port
0xFF returns an APtr to the fu-implemented Device or 0 if it is not registered,
or 1 if the system implements.

"Required" devices and ports include:
- `0x000 stdin`
- `0x001 stdout`
- `0x002 stderr` may be the same as stdout on some systems.
- `0x003-00F` reserved
- `0x010 coresystem`
  - 00 errno
    - fetch: get errno
    - store: set errno
  - 01 clock operations:
    - `fetch [] -> U32`: get microseconds clock
    - `fetch U32 -> []`: store: enter sleep mode until clock is >=
      microseconds.
  - 02-ED reserved
  - EE exit
  - EF-FF reserved

- `0x011 heap manager`: the heap memory manager.
  - Fetch port 0 `[] -> APtr`: returns the heap current heap ptr.
  - Store port 1 `U32 -> APtr`: grows heap after aligning it. Returns the
    aligned heap ptr.
  - Fetch port 2 `[] -> APtr`: returns the current minimum of the memory.
  - Fetch port 3 `[] -> APtr`: returns the current maximum of the memory.
- `0x012 block memory manager`: the block memory manager. This may
  be defined by the system or can also be registered. The block manager
  must be able to allocate and free 4KiB blocks of data.
  - Store port 0 (APtr): set the current address of the block manager.
  - Fetch port 1 (APtr): get the current address of the block manager.

- `0x013` a semi-efficient arena allocator of between `ASz*2` (min) and 4KiB (1 block) memory.
  - 00: get/set primary arena.
    - `load [] -> APtr` returns the current primary arena.
    - `store [arena: APtr] -> []` sets the current primary arena.
  - 01: alloc/dealoc from primary arena.
    - `load [po2: U8] -> APtr` allocates a value of po2 size from the primary arena.
    - `store [ptr: APtr; po2: U8]` deallocates the ptr from the primary arena.
  - 02: alloc new arena / drop arena.
    - `load [] -> APtr` allocates a new arena from the ROOT arena.
    - `store [arena: APtr] -> []` drops an entire arena.
  - 03: alloc/dealoc from specified arena.
    - `load [arena: APtr; po2: U8] -> APtr` allocates a value of po2 size from the specified arena.
    - `store [arena: APtr; ptr: APtr; po2: U8]` deallocates the ptr from the specified arena.
  - 04 `load [arena: APtr] -> &Stats`: get pointer to live arena stats such as
    memory used/etc (to be defined).

- `0x14`: native fu parser. This allows a high level language to emit fu
  without implementing it's own assembler.
  - 00: parse from the dvPort. Will parse and emit fua until encountering `]`.
  - 01: get/set the dvPort to parse from.
  - 02: get/set the current fu code pointer.
  - 03: get/set the current fu dictionary pointer.

- `0x015-100`: reserved

The following may not be supported on some/most systems:
- `0x101 syscall1` a 1-arg linux syscall
- `0x102 syscall2` a 2-arg linux syscall
- `0x103 syscall3` a 3-arg linux syscall
- `0x104 syscall4` a 4-arg linux syscall
- `0x105 syscall5` a 5-arg linux syscall
- `0x106-08F` reserved
- `0x190-1FF iodevices` open/operate on file io devices, i.e.
  files/sockets/etc. May not be supported.

- `0x200-3FF`: for arbitrary peripherals, especially for operating systems and
  micro-controllers. Typically this is divided up by:
  - 0x200-20F: SPI devices
  - 0x210-21F: I2C devices
  - 0x220-22F: UART devices
  - 0x230-23F: Ethernet
  - ...
  - 0x300 keyboard; 0x301 mouse; 0x302 text display; ...
  - ... fill out other standards.
- `0xF00-FFF`: implementation specific


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
- B 1011 RS: return stack operations, fetch=pop store=push
- C 1100 EP: the execution pointer register. Writing to it will trap.
- D 1101 AP: the alocator pointer register. This should point to the "global"
  allocator currently being used. The API for such an allocator will be
  implementation specific.
- E 1110 CP: the seCtorPtr.
- F 1111 LP: the LocalsPtr register. Writing to will trap.

> Note that implementors don't have to actually store these in hardware
> registers: they can just as easily go in non-accessible memory.

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
11   | 0 | 2b size | 3b jump
11   | 1 | 2b size | 3b mem
size | 6b operation
```

Like fu16, all immediates must be 16bit EXCEPT mem.IMWS with size=8, which can be
an 8 bit immediate. 32bit immediates are still not allowed, as they would
diverge from fu16 and make peephole optimization impossible.

Fu8 is intended to be the "cross platform" bytecode for fu. It should be able
to run anywhere with minimal modifications. This is not true for fu16, which
depends on endianness (see Alignment section for details).

### Mem
Below is the fu16 description and what it instead does in fu8.

- SRLP: Store WS0 at `LP+IM`
- SRCP: Store WS0 at `CP+IM`
- SROI: causes trap in fu8
- FTLP: Fetch value at `LP+IM` onto WS
- FTCI: Fetch value at `CP+IM` onto WS
- FTOI: causes trap in fu8
- IMWS: push IMM onto WS
- WS: causes NOOP in fu8

### Jump
Unlike fu16, there are no restrictions on jump instructions in fu8

- JZ: jumps to IMM if store=zero
- CALL: calls WS, updating LP appropriately
- JST: jumps to WS
- CNW: calls WS without update to ws growth.
- RET: return
- NOJ: is a noop in fu8

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

## Some thoughts on future direction of fu (notes)

- I'm missing indexed globals, functions and jumpBlocks. They are pretty much
  required for code generation and combining

- Fu8 is the "library" format. It is intended to be completely cross-platform
  and aid in gluing multiple libraries together and even enable system linking
  and machine asm generation.

- I had thought you could easily convert the indexed operations into a more
  "native" one, but you really can't... At least not with blobs having
  pointers... Unless blobs have types? They totally can, but this is
  non-trivial.
  - If its going to be converted into native code, you're going to have to be
    able to compile blobs that have references to other blobs... Or not allow
    references inside of const blobs...

- For the non-IMWS and nonLocal immeditates, the immediate can encode it's
  index type in the high bits as import/module global/fn/jumpBlock. Leaves 13
  bits for 8192 indexes. Then the operation ALWAYS gets converted to an APtr or
  the specified ty by the machine.

- 32 bit pointers can also refer to indexes. The 2 high bits being non-zero
  specifies that the other high 12 bits are a moduleIdx, and the low 16 are a
  typed index. Leaves 1GiB for other memory.
  - For 16 bit systems there is only 1 Idx module space.

- The binary has tables of tys, fns, globals, imports+blobs.
  - Fns are a cstr name, followed by a ty (inp/out) followed by an sized U16
    array of offsets for jumps to use, followed by the grow size, followed by
    the code. The compiler slurps this up, keeps track of fn location and
    offset array pointers for when local jumps happen.
  - Globals are ordered by type with the locations of transitions specified.
    They are accessed by indexs.
  - Blobs are part of the globals index space. At their start is a sized array
    of offsets for them by index. The whole thing gets copied directly into
    memory.

