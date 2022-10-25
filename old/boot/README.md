
There are many files in here, but almost all of them are very simple.

The spor/fngi bootstrapper compiles these in the following order:
- [constants.sp](./constants.sp): layout and documentation for integer constants
- [errors.sp](./errors.sp): integer constant error codes, used for assertions.
- [boot.sp](./boot.sp): the entrypoint for bootstrapping. This file is written
  in spor and defines (among many other things) `$fngi`, allowing for fngi
  syntax.
- [boot.fn](./boot.fn): fngi code to self-bootstrap fngi. Things start to get
  interesting here.


Files you probably don't care about:
- [types.ty](./types.ty): Used for python harness. Will hopefully be used in the
  future for fngi structs.
- [constants.h](./constants.h) this is directly generated from the spor constant
  files.
- [offsets.sp](./offsets.sp): This file is auto-generated from the C (or in the
  future fngi) struct types. It is compiled after `constants.sp`.

