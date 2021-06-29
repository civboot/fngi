# Fngi: the language that grows on the silicone

Fngi is an experimental (**NOTICE: in development and currently unusable**)
language inspired primarily by FORTH, C and Rust. It is designed to be
able to be bootstrapped from Forth ([Why Forth?](notes/why_forth.md)) with
relatively minimal code and the intent of being fully self-hosting on memory
constrained systems with less than 2MiB of total memory (and hopefully less).

Fngi's purpose is largely educational and as a foundational language of the
civboot.org project. It also aims to also provide an alternate starting point
for bootstrappeable.org.

Fngi targets familiar C-like syntax, except it values consistency and
understandability above all-else. This means that parens `()` are always an
expression, `{}` is always an initialized concrete data type and `[]` are
always a type declaration or type name. See [example.fn](notes/example.fn) for
example syntax.

Fngi is a strongly typed language that can be run dynamically at compile time.
Along with the powerful macro system, large parts of the fngi language are
written in fngi without the need for a binary bootstraping executeable. TODO:
add document with more info.

Fngi targets a C-like runtime without a system allocator. It has C-like
structs, Rust-like traits/interfaces and Rust-like enums. It has C-like memory
management with a few different standards for it's API (no global allocator
except heap grow/shrink, arena/GC/application-specific allocators encouraged).
It is designed to seamlessly compile directly into wasm or to a physical or
virtual machine.

Fngi has two parallel implementations, python and Forth. Along with allowing
faster prototyping in python, it allows for a robust [Test
Strategy](notes/test_strategy.md) to exist with minimal effort.

