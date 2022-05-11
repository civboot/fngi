# Civboot OS

The purpose of fngi (and spore) is to bootstrap into a Civboot OS. It also wants
to be a general-purpose programming language outside of Civboot, but that is
it's primary purpose.

One of the things I've realized is that a virtual machine bytecode permits a lot
of experimentation with how this might be implemented. In addition, if Civboot
software remains lean and small, then there we can affort using a virtual
environment for most code execution. This means that not only can the spore
bytecode interpreter be the basis for executing code: it can be the _entire_
kernel as well. This was always in the back of my mind as it is analagous to
CollapseOS's FORTH operating system, but I hadn't fully fleshed out the ideas
behind a permissions model/etc. Interestingly, almost everything here makes it
a better (more secure) general purpose language as well.

The first point is that there needs to be some concept of the following:

- ring: there is only ring0 and ring-other. ring0 can access and write to any
  memory and do anything. ring-other is a number between 1-127 which can only
  write to memory within it's ring.
- proc: a process with a certain ring and permissions. It gets a "big block"
  of 16k for it's code to be compiled in, it's own locals-stack, working stack
  and call stack.
- permissions: a U32 containing permissions bits regarding what the proc can do.

The basic architecture:

- The interpreter keeps a bytearray where each byte represents a 4k block in
  system memory. This is called the memring. Each byte is a ring number where
  the highest bit represents whether the 4k block is globally readable.
- The interpreter knows the currently executing proc. Whenever a memory or
  device operation is performed, the permissions bits and memring is checked
  before they are performed.

In the final Civboot, the above (or some version that is better which I haven't
thought of yet) will be powered by the hardware. The advantages to the above
are:

- Low runtime cost w/out hardware, zero runtime cost with hardware. Register
  memory or a separate small (10-12 bit) bus could be used for the ring array
  to enable zero-cost checking of memory access. Possibly the memory itself
  could store the ring number for each block and the current ring be updated
  by the kernel.
- Almost zero complexity. I believe the above is aproximately as simple as you
  could design a memory and device protection system. The allowed rings could
  be grown to 255 by using a bitarray for the global-read bit, and could be
  grown to more by using 16bits for the ring number. However, I don't believe
  Civboot will _ever_ require more than 255 process _groups_ (note _not_
  processes).

## Proc Model

The core data type throughout the OS is the 4k block and the Arena allocator.
Processes communicate by passing bvalues and arenas between eachother,
where bvalue is the primary "root" value being communicated. Both can be null.
These are set as global variables and can be changed when
cont(rc) or exit(rc) is called.

The standard bvalue has two types: bdata which is essentially a len of bytes
and blist which is a len of bvalues (pointers). All pointers should be to
memory within the corresponding arena. Process are not _required_ to use
bvalue for their root data type, but it is strongly recommended for most
cases. This diverges from linux where the standard datatype is an unstructured
stream of bytes. CivbootOS simplifies the UNIX design by allowing communication
of entire blocks and arenas, reducing the glue that commonly exists between
processes serializing and deserializing simple data.

## Device Registration

Not all device operations need to be implemented by the native backend. Most
will be "registered" and the native backend will simply execute them. Before
executing them it will:

- Cache the current ring and permissions bits.
- Set the ring=0 and permissions=full
- Basically do `D_xsCatch` on the registered function.
- Before returning it will re-set the ring and permissions. If
  there was a panic it will be re-thrown.

Therefore all device operations run as ring0 and can only be registered by a
ring0 process. They can be thought of as kernel-level "drivers." It is their
job to do appropriate permissions checking.

DeviceId=0 is reserved for compiler-internal operations and will be protected
by a permissions bit. DeviceId=1 will be used for "os" operations and uses
various permissions bits. The operations include:

- log for writing bvalue logs. Caller must drop bvalue.
- env for fetching environment values.
- block for de/allocing from the block allocator
- arenaNew for creating and dropping "root registered" arenas. These have their
  own blocks and can be passed between processes.
- proc for processes
  - LD creates a new process. Must pass in the process owned arena and
  config (both optional).
  - SR continues a process using ioArena and ioValue.
- fs for communication with the file system "directory" database. LD reads
  fs node, SR updates them.
- disk for communication with a "disk access" device. Requires a fs node to
  specify which one.

> At the device level, eveything is NOT a file. There is very distincly a
> "disk", "udp", etc. At the programmer level there will still be URIs to
> access things ergonomically.

## Permissions Bits

TODO: need to define.
