
Spore steals the DEI and DEO (renamed DVF/DVS for DeVice Fetch/Store) operations
from the [uxn](https://wiki.xxiivv.com/site/uxn.html) project. Devices allow
the implementer to create standard hardware interfaces that are extremely
simple to implement and interract with from the bytecode. A rough analogy for
Device Operations is a simpler unix syscall.

A device and port (DvPort) is specified with a U16. If the high bit is not 1
(register mode described below) then the lowest 8 bits are the devices port
and the upper 11 bits are the device to select. DVF and DVS are used to
read/write values of the specified size to the device, which are implemented
by either the runtime or registered by the software.

Note that device operations are more like a function call than an instruction.
They may take more than one clock cycle and may consume or return multiple
stack values. They also completely ignore the `size` bits in the instruction.
They are a convienient way for the implementation system to define important
interfaces between spore and itself, allowing for important functions to be
implemented in the implementation language (which may be assembly) and later be
overriden by a spore implementation.

In general, writing an APtr to port 0xFF of a Device will register it. The
written APtr must point to a function that accepts the `DPort` and performs
the necessary operations. However, note that many devices will be implemented
(and therefore registered) by the implementation language of spore. Reading port
0xFF returns an APtr to the spore-implemented Device or 0 if it is not registered,
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

- `0x14`: native spore parser. This allows a high level language to emit spore
  without implementing it's own assembler.
  - 00: parse from the dvPort. Will parse and emit sporea until encountering `]`.
  - 01: get/set the dvPort to parse from.
  - 02: get/set the current spore code pointer.
  - 03: get/set the current spore dictionary pointer.

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
- E 1110 MP: the ModulePtr.
- F 1111 LI: the LocalsPtr register. Writing to will trap.

> Note that implementors don't have to actually store these in hardware
> registers: they can just as easily go in non-accessible memory.
