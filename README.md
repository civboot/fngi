# fngi: a readable language that grows from the silicon

> **WARNING:** this language is in alpha stage. Things are expected to be
> broken.
>
> However, the language is in a usable, if extremely unstable state. Please see
> the issue tracker for how to help, or ask on [discord][discord] (click
> [here][discord invite] to get an invite).

If we want to build an entirely understandable tech stack (as part of
[civboot.org](http://civboot.org) or [bootstrappable.org](http://bootstrappable.org))
we need a language which can be implemented easily, has excellent expressiveness
and readability, can modify itself, and has a simple but effective security
model built in. The language has to be able to unroll from simple beginnings
(implemented in a hex editor or similar) to a full-featured language that we can
use to build a Civboot -- and have fun in the process!

That language is fngi, a language inspired primarily by FORTH and C. It is
implemented in a miniscule amount of C (fewer than 2-3k lines) and contains all
the bells and whistles you might want from C, as well as an absurdly powerful
macro system that can mutate it's own syntax at runtime -- literally it can
built itself into a new programming language while it runs.

Fngi itself has the following space targets:

- >=128KiB microcontroller/processor: can bootstrap a text-based OS providing
  human-editable source and assembly that can be recompiled on the
  microcontroller (aka self-bootstrapping). The processor does _not_ need to
  have memory-mapping, thread management, etc -- it can be a very bare-bones
  device.
- >=4KiB microcontroller: can run pre-compiled spor assembly. Will not be
  self-bootstrapping.

Read more:

- [fngi.md](./fngi.md)
- [harness and zoab](./harness.md)
- [Unstructured Notes](./notes/)

> Fngi and civboot should be considred an
> [Obsolete Technology](http://xkcd.com/1891)

## Building
You need the 32 bit toolchain to build fngi (to use `-m32` flag in gcc).
On ubuntu this is installed with `sudo apt-get install gcc-multilib`

You then need the source code for the below repos in a single directory.
use either the ssh or http forms below
```
cd ~/my_personal_project_directory/
git clone git@github.com:civboot/fngi.git  # https://github.com/civboot/fngi.git
git clone git@github.com:civboot/civc.git  # https://github.com/civboot/civc.git
git clone git@github.com:civboot/cxt.git   # https://github.com/civboot/cxt.git
git clone git@github.com:civboot/zoa.git   # https://github.com/civboot/zoa.git

cd fngi
make test
```

## REPL
You can open a very basic REPL with:

```
make test ARGS=--repl
```

Example repl sesion:
```
\ Note: this is a line comment
\ put 0xF1 and 2 on the stack (stack is in hex)
\ After the colon is the types
0xF1 2
> {       F1        2} : S S

\ add the top of the stack to 9
+ 9
> {       F1        B} : S S

\ drop the working stack
drp drp
> {} :

\ define a function that multiples input by 2
\ TODO: input types
pre fn mulTwo stk:S -> S do ( * 2 )
mulTwo(4)
> {        8} : S
```

## Motivation
Fngi is a "macro first" language. Why? So that it can evolve to be whatever is
needed of it.  To understand this better, go back to early programming
languages: unix had sh and C.

Unix sh is barely a language. It has only a single type, the string.  It has an
extremely obtuse syntax who's sole method of operation is to dynamically execute
scripts and modify strings. In truth, the shell "syntax" depends on a host of of
other complex syntaxes: awk, grep, regexes, individual command lines, formats of
system files (`/proc/`), etc.

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

- [X] Make a complete fngi language, then throw it away. See
  [round_two](./notes/round_two.md)
- [x] Create [civc] library to build on top of, based on lessons learned
- [ ] Create fngi
  - [X] spor VM for executing compiled instructions
  - [X] TyDb for setting up the type checker
  - [X] fn for writing type-checked functions
  - [X] if/elif/else for flow control
  - [x] blk to create loop/while/etc.
  - [x] var, struct
  - [x] compilePath to compile source
  - [ ] global
  - [ ] `mod` to set the current dictionary path (module support).
- [ ] Create standard library
  - [ ] Allocator options and allocator stack
  - [ ] String formatting
  - [ ] String printing
  - [ ] Regex
  - [ ] File reading and modification
  * [ ] zoa integration
- [ ] Create an ultra-basic text-editor (zat)
- [ ] Extend zat to create an ultra-basic CLI
- [ ] Implement a tyStk (type checking) and hook into `fn`, `if`, etc
* [ ] Implement a better shell (shrm)
- [ ] Design and build a cross-compiler to compile spor -> IR -> native assembly
- [ ] Create an embedded operating system (RISC-V, ARM, etc)
- [ ] Create custom (stack-based) hardware and port the operating system

See [civboot.org](http://civboot.org) for future goals.

[zoa]: http://github.com/civboot/zoa
[civc]: http://github.com/civboot/civc
[discord]: https://discord.com/channels/1083089060765118464/1083089061553639477
[discord invite]: https://discord.com/invite/2DYwsbJ84H

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
