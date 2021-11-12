# Fngi: the language that grows on the silicone

> **NOTICE:** in development and currently unusable

If we want to build an entirely understandable tech stack (as part of
civboot.org or bootstrappable.org) we need a language which can be
implemented easily, has excellent expressiveness and readability, and can
modify itself one expression at a time. The language has to be able to
unroll from simple beginnings (implemented in a hex editor or Forth) to be
full-featured that we can build a Civboot, with all the software complexity
that entails, on top of it.

That language is Fngi, a language inspired primarily by FORTH, C and Rust. It
is designed to be able to be bootstrapped from Forth ([Why
Forth?](notes/why_forth.md)) with relatively minimal code and the intent of
being fully self-hosting on memory constrained systems with less than 2MiB of
total memory (and hopefully less, the real target is more like 32KiB).

Fngi targets a somewhat familiar C-like syntax, except it values consistency
and understandability above all-else. This means that parens `()` are always an
expression, `{}` is always an initialized concrete data type and `[]` is
always a type declaration or type name. Fngi also allows expressions to more
easily compose than in most languages: many constructs such as blocks and
if-statements are expressions that can return a value. See
[example.fn](notes/example.fn) for example syntax.

Fngi is a strongly typed language that can be run dynamically at compile time.
Along with the powerful macro system, large parts of the fngi language are
written in fngi without the need for a binary bootstraping executable. TODO:
add document with more info.

Fngi targets a C-like runtime without a system allocator. It has C-like
structs, Rust-like traits/interfaces and Rust-like enums. It has C-like memory
management with a few different standards for its API (no global allocator
except heap grow/shrink, arena/GC/application-specific allocators encouraged).
It is designed to seamlessly compile directly into wasm or to a physical or
virtual machine.

# Spore
Fngi is implemented in spore, a virtual machine bytecode, assembly and
stack-based language. Spore starts as a very primitive assembler and 16bit
bytecode. The bytecode is sophisticated enough to implement a general-purpose
language in (fngi) but simple enough to implement in a few thousand lines
of C or assembly.



# Hacking

When opening a PR to submit code to this repository you must assent to submit
your code under the license listed in this project. You must put this assent
in your commit message. This is only required for your first commit:

```
I <author> assent to license this contribution under the dual licenses of
the UNLICENSE or MIT license listed in the `UNLICENSE` and `README.md` files of
this repository.
```

To setup the python environment do the below. 

> Note: `requirements.txt` dependencies are only used for testing purposes.

```
python3 -m venv venv/
source venv/bin/activate
pip install -r requirements.txt
# Run tests
pytest -vvv fnpy/* tests/*
```

# LICENSING

The body of work in this directory are licensed using the
[UNLICENSE](./UNLICENSE), unless otherwise specified at the beginning of the
source file (only for files in `testing/`).

If for any reason the UNLICENSE is not valid in your jurisdiction or project,
this work can be dual licensed with the MIT license below at your discression.

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
