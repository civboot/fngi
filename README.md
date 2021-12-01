# Fngi: the language that grows on the silicone

> **NOTICE:** in development and currently unusable

If we want to build an entirely understandable tech stack (as part of
civboot.org or bootstrappable.org) we need a language which can be
implemented easily, has excellent expressiveness and readability, can
modify itself one expression at a time, and has a simple security model built in
that at least prevents us from shooting _ourselves_ in the foot. The language
has to be able to unroll from simple beginnings (implemented in a hex editor or
similar) to a full-featured language that we can build a Civboot, with all the
software complexity that entails, on top of it.

That language is Fngi, a language inspired primarily by FORTH and C. It is
self-bootstrapped at runtime from Spor: an assembly bytecode, interpreter and
syntax. Spor itself is self-bootstrapped from an extremely lean native
implementation (~1000-2000 lines of C).

Read more about Spor at [spor.md](./spor.md).

Read more about Fngi at [fngi.md](./fngi.md)


# Hacking

When opening a PR to submit code to this repository you must assent to submit
your code under the license listed in this project. You must put this assent
in your commit message. This is only required for your first commit:

```
I <author> assent to license this contribution under the dual licenses of
the UNLICENSE or MIT license listed in the `UNLICENSE` and `README.md` files of
this repository.
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
