# Fngi: the language that grows on the silicone

> **NOTICE:** in development. Spor is somewhat usable (albeit without even file
> handling support), with a line-of-sight for boostrapping into fngi.

If we want to build an entirely understandable tech stack (as part of
civboot.org or bootstrappable.org) we need a language which can be
implemented easily, has excellent expressiveness and readability, can
modify itself, and has a simple but effective security model built in. The
language has to be able to unroll from simple beginnings (implemented in a hex
editor or similar) to a full-featured language that we can use to build a
Civboot -- and have fun in the process!

That language is Fngi, a language inspired primarily by FORTH and C. It is
self-bootstrapped at runtime from Spor: an assembly bytecode, interpreter and
syntax. Spor itself is self-bootstrapped from an extremely lean native
implementation, which is currently ~1-2 thousand lines of C but is intended to
be writeable in a hex editor (when targeting a simple processor).

Fngi itself is intended to bootstrap a full-featured OS in very minimal amounts
of code that is runnable on microcontrollers with 64k of memory or less and zero
hardware support for memory maangement, thread handling, etc.

Read more:
* [spor.md](./spor.md)
* [fngi.md](./fngi.md)
* [harness and zoab](./harness.md)
* [Unstructured Notes](./notes/)

> Fngi and civboot should be considred an [Obsolete
> Technology](http://xkcd.com/1891)

# Goals
fngi's goal is to evolve into nothing less than the primary programming language
and operating system for [Civboot](http://civboot.org). There are many steps
along the way, some of them complete.

- [X] create an assembly. Done, see [spor.c](./spor.c)
- [X] bootstrap the assembly into fngi. Mostly done, see [spor.sp](./spor.sp)
- [ ] boostrap fngi into a _more_ full-featured language. This will be in
  [fngi.fn](./fngi.fn). This is mostly adding core operators (`+`, `-`, `*`,
  etc), along with several other expected pieces.
- [ ] Write initial way to define namespaces, structs and enums.
- [ ] Write block and arena-buddy allocators, as well as standardized ways to
    use them (i.e. an arena stack).
- [ ] Write way to keep track of types on a "type stack", which the vairious
      sub-compilers will use (i.e. `fn`, `if`, etc).
- [ ] Write `fn`, `if`, `while`, etc to replace the asembly defined versions
  and aquire type safety.
- [ ] Write zoab in fngi (see [zoa][zoa] project).
- [ ] Write zoat in fngi (see [zoa][zoa] project).
- [ ] Write `struct`, allowing for zoa serializable types from the beginning.
- [ ] Implement zoac (see [zoa][zoa] project).
- [ ] Implement core OS functions to move and modify files.
- [ ] Implement zosh (see [zoa][zoa] project).
- [ ] write a text editor


[zoa]: http://github.com/vitiral/zoa

# Helping / Hacking

The current command I use to run fngi and all tests is below. Eventually
I'll incorporate all this into the harness itself:

```
gcc *.c && sleep 0.3 && \
  ./a.out | PYTHONPATH="$PWD/zoa:$PYTHONPATH" \
  python3 harness.py
```

## Contributing

When opening a PR to submit code to this repository you must include the
following disclaimer in your first commit message:

```
I <author> assent to license this and all future contributions to this project
under the dual licenses of the UNLICENSE or MIT license listed in the
`UNLICENSE` and `README.md` files of this repository.
```

# LICENSING

This work is part of the Civboot project and therefore primarily exists for
educational purposes. Attribution to the authors and project is appreciated but
not necessary.

Therefore this body of work is licensed using the [UNLICENSE](./UNLICENSE),
unless otherwise specified at the beginning of the source file.

If for any reason the UNLICENSE is not valid in your jurisdiction or project,
this work can be singly or dual licensed at your discression with the MIT
license below.

```
Copyright 2021 Garrett Berg

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
