# Spore virtual machine

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

So, the spore virtual machine and bytecode is now born. Fortunately I've actually
thought a lot about this from my experience thinking about the J1.

Spore has two goals, in order:
- Develop a virtual machine that can run on either 16 or 32 bit systems
  performantly. This is called spore16.
- (once done) be able to backfill that into a binary format (spore8) that can be
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

This allows the spore virtual machine to be implemented in highly performant
hardware at some point in the sporeture.

Register stacks include:
- `WS`: 1KiB (2^10) bytes "Working Stack" for non-immediate operations. Values
  are popped from the stack for operations (if W is specified in Mem) and
  pushed to the stack as the result of operations.
- `RS` 256 (2^8) bytes "Return Stack" for call/return with a variable
  bit-length array for tracking module (we'll get to this) as well as parallel
  256 bytes for stack-size tracking.

## Registers
The mem operation (see Binary layout) controls memory access. spore has very few
registers:

- `MP`: Module Ptr register of variable bit-length denoting the module block of
  a 16 bit half-pointer.
- `LP` locals pointer, addr-bit register is used for local values with the
  targeting language. It automatically decrements on CALL and increments on
  RET (covered later)
- `EP`: execution pointer

## Types
U8, U16 and U32 are used to specify the bit-width regardless of whether
they are signed or unsigned (the operation determines signedness).

MPtr means a "half pointer", aka a 16bit pointer. APtr is an "absolute
pointer" aka a 32bit pointer. Typically only immediate (compile-time) pointers
are MPtr's, so Ptr also means APtr.

On 16bit systems, an MPtr and APtr must have the same size of 16bits and the
module block is always "0".

## Memory Operations
spore16 supports 16 bit memory access and function calls through the use
of the module register (MP), which gets stored on the return stack (RS)
alongside the addresses to return to. The module gets updated anytime there is
a jump or call into a 32 bit address, or a RET. All functions within a module
can address memory and other local functions using 16 bit address space.

When compiling for a 32bit spore machine it is important to separate the functions
in this way... or never use 16bit addressing (a significant loss of
performance).


## spore16 Binary layout
A spore16 instruction is 16 bits, with a possible 16bit immediate value following
it. The address space and operations are all a maximum of 32 bits. Note also
that there is a spore8 instruction set which follows many of the same conventions
and can be peep-hole optimized into spore16 instructions (described later).

A single spore16 instruction can specify up to **three different operations that
(might) all happen within a single clock cycle** on a suitably built machine.
The approach is inspired by the [J1
microprocessor](https://excamera.com/files/j1.pdf) with the desire to make a
more general-purpose CPU. For demonstration, `1,SRMI,ADD,RET #4200` will:
- Add two unsigned 32 bit (4 byte) values on the stack
- Store their value at the 16bit address 0x4200 offset by the MP (module ptr).
- Return, which not only continues executing from the previous function but
  also updates the working stack pointer and the module pointer.

spore16's bit layout is as follows:

```
    
   Jmp    Mem   Sz Operation
   JJJ XX MMM   SS OO OOOO
   |high byte|  |low byte |
```

The assebmly syntax can put any command in any order, then uses `;` to compile them:


Each has defaults:
- size in bytes: 0, aka APtr size
- jmp: NOJ
- mem: WS
- op: NOP

Assembly syntax:
- `/ line comment`
- `#NN / #NNNN / #NNNN_NNNN` pushes a 8/16/32 unsigned hex number, i.e. `#1F
  /``#001F`/ `#001F4200`
- `&N` push heap value onto the stack. N=2 for MPtr, N=4 for APtr.
- `=N<name>` pop a value of N bytes from the stack and store at name. If name
  doesn't exist, add to dict.
- `@N<name>` get value from name of size N bytes.
- `~<name>` Forget all items in the dictionary until and including `<name>`
- `,N` pop N bytes from the stack and write to and update heap (compile them).

Compiling and executing asm. Note that there is a global 16bit value that gets
updated by the below:
- `;` compile the current 16bit instr to heap and clear the instr (sets to default
  values).
- `^` run the current instr and clear it.
- `$<name>` begin executing at location stored in name.
- alpha-numeric i.e. "foo": get the U32 value from the dict. Treat the first 16
  bits as a mask and second 16bits as a value to update the current asm
  instruction.

Example to add a byte to 42 and return
```
S1 IMWS ADD RET; #0042 ,2
```

Example forward jump if zero.
```
// if top=0 will jump to where "if" label is set.
JZ;       // jump-if-zero instruction
&2        // push current location (2 bytes)
=2 if     // store in if
#0000 ,2  // push 0x0000 on stack and compile it (initial jump location)

// ... code if not zero

@2if      // get 2byte value at if
~if       // forget label (for saving space in dict).
&2        // get 2byte current location
!22       // at address which was stored in if, store current location.
```

That looks like a lot, let's reduce it to what it would normally look like:
```
JZ;  &2 =2if  #0000,2

// ... code if not zero ...

// end-ifnz
@2if ~if &2 !22
```

### Immediate
The immediate value is a 16 bit value which must follow the instruction if
the instruction requires an IM. It can be interpreted either signed or unsigned
depending on the operation.

Only 16bit immediates are supported. 32bit constants can be pushed to the stack
by LoaDing them from module memory or pushing two 16bit constants and using
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

0 000 WS      WS0       WS1     WS        Working Stack
1 001 IMWS    IM        WS0     WS        IMmediate Working Stack
2 010 SRLI    WS0       WS1     ST(LP+IM) StoRe LocalsPtr offset
3 011 SRMI    WS0       WS1     ST(MP+IM) StoRe to ModulePtr offset
4 100 SROI    IM        WS1     ST(WS0)   StoRe Operate Immediate
5 101 FTLI    FT(LP+IM) WS0     WS        FeTch from LocalsPtr offset
6 110 FTMI    FT(MP+IM) WS0     WS        FeTch from ModulePtr offset
7 111 FTOI    FT(WS0)   IM      WS        FeTch Operate Immediate
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
- FTMI: means "FeTch Module pointer Immediate". It fetches from immediate
  with an offset of the module pointer. It stores on the working stack.
- FTLI: means "FeTch LocalsPtr". It uses the immediate as the offset to
  fetch from the function locals. It stores on the working stack.
- SROI: means "StoRe Operate Immediate". It uses the immediate value for
  the operation and uses WS1 as the "second". It stores the value
  at the address stored in WS0.
- SRMI: means "StoRe ModulePtr". It uses the WS for the operation but then
  stores at the immediate value offset by the module pointer.
- SRLI: means "StoRe LocalsPtr". It uses the WS for the operation but
  stores at the immediate value offset by the function locals.

Cheatsheat for above table:
- FT(MP+IM) means "fetch MP+IM" aka "fetch module + immediate" aka use the
  value at the immediate address in the specified module. ST(...) means "store"
  at that address.
- Top means the top value used in the operation.
- Second means the second value used in the operation (may not be used).
- WS0 means top of working stack, WS1 means second value of working stack.
- Storing to the WS means pushing values to the top of the WS. Some operations
  may have also consumed 1 or more values from the WS.


### Jmp

A Jmp (Jump) involves:
- Seting EP to the address
  - If the address is 16bit then EP = MP+address
  - If the address is 32bit then update MP to the new module
- Execution continues at new EP

For all jumps, a jump of size=U8 will trap, U16 is interpreted as an offset
from the module, U32 is an absolute jump.

Calls work a little different in spore than in many languages:
- You can call using CALL or CNL. CALL expects the value at the pointer to be
  a growLs value which defines how much the localstack needs to be grown.
  This value is stored on the callStack so that RET will automatically update
  the locals stack when returning.
- Calls can be either 16bit or 32bit. 16 bit will be within the current "module"
  (the upper 16bits of the execution pointer stay the same). Note that the
  current limit for the number of modules is 256 so they can fit inside a
  single byte (and the other byte can be used for the growLs value on the
  callStack).
- The localsStack can only grow by up to 256 * APtrSize so it can fit in a
  single byte;

The following Jump modes are possible:
- 0 NOJ: No Jump, do not perform jump (but do the rest of the operation)
- 1 JZ: Jump to Immediate if store=zero
- 2 JTBL: consume StoRe and jump to the jump table which uses the IMM for
  it's size.
- 3 JST: Jump to STore. Consumes store.
- 4 reserved
- 5 CALL: Call an address
  - pop ptr off of store, convert to APtr using MP if necessary.
  - fetch growLs value at ptr. growLs is from 0-256 and needs to be multipled
    by ASIZE to get the amount the localstack should grow.
  - grow LS by growLs.
  - push the growLs byte, current module byte and current ep 16 bit value (4
    bytes total) onto the CallStack. These are typically packed as a 32 bit
    value.
  - jump to ptr+2 (skipping WS size)
- 6 CNL: Call an address without a locals stack update. Does not require
    a memory read. Still pushes relevant values onto call stack so that RET
    works identically.
- 7 RET: return
  - pop return module,address and growWs from CallStack
  - shrink local stack by grown amount
  - jump to return module+mptr.

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
- array of 16 bit values containing the modulePtr to jump to.
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
- `FT {addr: Ptr} -> value` fetch.
  - Can only be used with mem of {WS} and size=ptrSize
- `SR {addr: Ptr, value}` store. Note that the address is Second,
  allowing for storing an IMM value with IMWS.
  - Can only be used with mem of {WS; IMWS}

Device operations can only work with size=U16. They will update the stack
differently per operation, ignoring the size bits. See **Device Operations**
for more details and clarifications.
- `DVF` `{dvPort: U16}` DeviceIn, get the value at the dvPort
- `DVS` `{...; dvPort: U16}` send the value to the dvPort.

**Non-special operations**: these use size normally.

Unary: only top is used or consumed for these.
- `NOP`: no operation/identity. Simply store Top
- `DRP`: drop Top by consuming it but not storing it
- `INV`: inverse bitwise
- `NEG`: two's compliment
- `EQZ`: 1 if TOP=0, else 0
- `EQZ_NC`: 1 if TOP=0, else 0. Do not consume Top.

Binary operations are in syntax {l; r} (r i.e. right is at top of stack). This
is so that when fngi-like languages compile them `a+b` gets compiled as
`push$a; push$b; add[T]$()`:
- `DRP2` drop 2. Consume both top and second and store neither.
- `SWAP` swap two values.
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
Device operations allow for communication between the asm bytecode and the
"backend", aka the language implementing the bytecode. This allows spore
assembly to be self-hosting, since not only can instrunctions and fns be
executed from asm (using `$`) but the compiler infrasture itself such as
`read`, `scan`, dict get/set, etc. This allows for the asm to be self
hosting.

When device operation command is called it always pulls {dvId:1 port:1} from
the stack. These values do not have alignment requirements. In addition,
if IMWS is used, then the dvId is the high order byte and the port is the low
order byte.

In the future, the assembly will be able to register/override dvIds by simply
executing a DVSR to port 0xFF of the dvId with a 32bit APtr to the code to
execute. The signature of a device must be:
```
{port:1 meta:1}
```

The meta byte has the following bit structure. X is reserved for future use.
```
     direction 0=LOAD 1=STORE
     | sz in bytes
XXXX DSSS
```

The device may use the working stack and other environment registers directly.
It may use the sz given to it for popping it's arguments or it may ignore them.
Whatever it does, it should document it well.


## Register Operations


## Spore8
spore8 (8bit instruction) has the same basic layout as spore16, except the first 2/3
bits select what _part_ of the 16bit instruction is being executed. The purpose
of spore8 is:
- To enable more easily being emulated on 16 bit embedded systems.
- Easier to implement a virutal machine for.
- To allow compilers to emit spore8 and then use peephole optimization to trivially
  convert it to spore16. Peephole optimization is one of the simplest optimizers
  to write. It involves runing over bytecode and combining/altering operations
  to be more efficient but equivalent operations.


Byte layout:
```
11   | 0 | 2b size | 3b jump
11   | 1 | 2b size | 3b mem
size | 6b operation
```

Like spore16, all immediates must be 16bit EXCEPT mem.IMWS with size=8, which can be
an 8 bit immediate. 32bit immediates are still not allowed, as they would
diverge from spore16 and make peephole optimization impossible.

Spore8 is intended to be the "cross platform" bytecode for spore. It should be able
to run anywhere with minimal modifications. This is not true for spore16, which
depends on endianness (see Alignment section for details).

### Mem
Below is the spore16 description and what it instead does in spore8.

- SRLI: Store WS0 at `LP+IM`
- SRMI: Store WS0 at `MP+IM`
- SROI: causes trap in spore8
- FTLI: Fetch value at `LP+IM` onto WS
- FTMI: Fetch value at `MP+IM` onto WS
- FTOI: causes trap in spore8
- IMWS: push IMM onto WS
- WS: causes NOOP in spore8

### Jump
Unlike spore16, there are no restrictions on jump instructions in spore8

- JZ: jumps to IMM if store=zero
- CALL: calls WS, updating LP appropriately
- JST: jumps to WS
- CNL: calls WS without update to local stack growth.
- RET: return
- NOJ: is a noop in spore8

### Operations
spore8 operations are identical to spore16 ones, except they are always
pulled from and stored to the WS.

### Spore8 Alignment Requirements
There is no alignment requirements on Imediate values following IMM
instructions. For constants (on memory load), there is alignment requirements,
but all values of the same type must be packed together in their respective
module blocks. The header of a binary spore8 file contains the big/little
endianness setting for the binary.  It also contains offsets for all of the
global values in each module, which must be broken into data type sections. It
looks like this (all numbers are hex, `//` means a comment).

```
// Constant Header Section
0000 // module number
0100 // offset for beginning of 32bit values
0200 // offset for end of 32bit values, beginning of 16bit values
0332 // end of 16bit values
0001 // next module number
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

## Some thoughts on future direction of spore (notes)

- I'm missing indexed globals, functions and jumpBlocks. They are pretty much
  required for code generation and combining

- Spore8 is the "library" format. It is intended to be completely cross-platform
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

