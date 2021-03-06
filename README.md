# fngi: a readable language that grows from the silicone

> **UPDATE:** the kernel rewrite is complete, fngi works again! I'm now
> implementing `fn` syntax (without type checking) in `kernel/fngi.fn`. See
> `notes/fn.md` for more details.

If we want to build an entirely understandable tech stack (as part of
[civboot.org](http://civboot.org) or [bootstrappable.org](http://bootstrappable.org))
we need a language which can be implemented easily, has excellent expressiveness
and readability, can modify itself, and has a simple but effective security
model built in. The language has to be able to unroll from simple beginnings
(implemented in a hex editor or similar) to a full-featured language that we can
use to build a Civboot -- and have fun in the process!

That language is fngi, a language inspired primarily by FORTH and C. It is
self-bootstrapped at runtime from spor: an assembly bytecode, interpreter and
syntax. Spor itself is self-bootstrapped from an extremely lean native
implementation, which is ~1-2 thousand lines of C.

Fngi itself has the following space targets:

- >=32KiB microcontroller/processor: can bootstrap a text-based OS providing
  human-editable source and assembly that can be recompiled on the
  microcontroller (aka self-bootstrapping). The processor does _not_ need to
  have memory-mapping, thread management, etc -- it can be a very bare-bones
  device.
- >=4KiB microcontroller: can run pre-compiled spor assembly. Does not need to
  be self-bootstrapping.

Read more:

- [spor.md](./spor.md)
- [fngi.md](./fngi.md)
- [harness and zoab](./harness.md)
- [Unstructured Notes](./notes/)

> Fngi and civboot should be considred an
> [Obsolete Technology](http://xkcd.com/1891)

## Motivation
Fngi is a "macro first" language. Why? So that it can evolve to be whatever is
needed of it.  To understand this better, go back to early programming
languages: unix had sh and C.

Sh is barely a lanugage. It has only a single type, the string.
It has an extremely obtuse syntax who's sole method of operation is to
dynamically execute scripts and modify strings. In truth, the shell "syntax"
depends on a host of of other complex syntaxes: awk, grep, regexes, individual
command lines, formats of system files (`/proc/`), etc.

By contrast, C has a rigid syntax, a complex macro system, a complex build and
linking system and and no dynamic features. You could never use C as a
scripting language, since it has no way to execute a function you just defined.

What if there could be only one _base_ language, and you could use it as a seed
for more application-specific instances? It would have to be dynamic, but also
compile to a VM bytecode which can be executed immediately, but can also be
built into a native executable. If we are following in the footsteps of sh, it
should also be extremely small and simple in it's beginninings.

The first thing to note about fngi is that it has a **concrete syntax**. This
means there is no (inherent) abstract syntax tree -- every token is simply
compiled or executed immediately. In fngi, any function can declare itself as
`syn` (syntactical) and take control of its own destiny, reading tokens and
doing with them whatever it wishes. With great power comes great
responsibility. This leads to a radical "macro first" language, where language
features can be added by modules and imported as-needed.

## Goals

fngi's goal is to evolve into nothing less than the primary programming language
and operating system for [Civboot](http://civboot.org). There are many steps
along the way, some of them complete.

- [X] create an assembly. Done, see [spor.c](./spor.c)
- [X] bootstrap the assembly into fngi. Mostly done, see [spor.sp](./spor.sp)
- [x] Write zoab in fngi (see [zoa][zoa] project).
- [X] boostrap fngi into a _more_ full-featured language. This will be in
  [fngi.fn](./fngi.fn). This is mostly adding core operators (`+`, `-`, `*`,
  etc), along with several other expected pieces.
- [X] rewrite the kernel to allow for modules/etc
  - [X] Create a 4k block allocator and arena-buddy-allocator.
- [X] Write an initial "untyped" version of `fn` and `if`
  - [ ] Implement forward declaration of functions
- [ ] Write initial way to define namespaces, structs and enums. (`STRUCT`,
      ENUM`, etc)
- [ ] Write dot compiler (variable modifier).
- [ ] Create an ultra-basic text-editor.
- [ ] Create an ultra-basic CLI built on the text-editor.
- [ ] Implement a tyStk (type checking)
- [ ] Write `fn`, `if`, `while` using tyStk to and aquire type safety.
- [ ] Implement core OS functions to move and modify files and implement a
      standardized way to communicate user-input and display info with the
      harness.
- [ ] Write zoat in fngi (see [zoa][zoa] project).
- [ ] Implement zoac (see [zoa][zoa] project).
- [ ] Implement zosh (see [zoa][zoa] project).
- [ ] Write a glorious text editor, zat

[zoa]: http://github.com/vitiral/zoa

## Helping / Hacking

The current command I use to run/test is:

```
$ python3 etc/make.py --build --test
```

There is not yet a way to compile a file outside of `kernel/fngi.fn`,
and there won't be until modules are implemented. See `linux/kernel.c` for
implementation details.

## Contributing

When opening a PR to submit code to this repository you must include the
following disclaimer in your first commit message:

```text
I <author> assent to license this and all future contributions to this project
under the dual licenses of the UNLICENSE or MIT license listed in the
`UNLICENSE` and `README.md` files of this repository.
```

## LICENSING

This work is part of the Civboot project and therefore primarily exists for
educational purposes. Attribution to the authors and project is appreciated but
not necessary.

Therefore this body of work is licensed using the [UNLICENSE](./UNLICENSE),
unless otherwise specified at the beginning of the source file.

If for any reason the UNLICENSE is not valid in your jurisdiction or project,
this work can be singly or dual licensed at your discression with the MIT
license below.

```text
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
