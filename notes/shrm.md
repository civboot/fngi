# shrm: the fngi shell language

> **WARNING:** shrm does not yet exist. This document simply lays out what
> _will_ exist.

shrm is a library of [fngi] which implements a re-imagined shell.

Forget about sh/bash/zsh/python. What features **must** a good shell language
have:

* Interactive: you must be able to run commands dynamically and work with the
  output directly.
* Dynamic: types and functions must be changeable, data must be mutable, memory
  management must be automatic. You must be able to define functions and then
  change their definition.
* Composable: it should be easy to break appart data and put it back together.
  It should be easy to transform data from one function/script" into the
  arguments for another.
* Multi-process: it must be possible to spin up multiple processes and
  work with the data (at least pipe it)
* Error handling: it must be possible to cleanly handle any errors. Resources
  must be automatically closed.
* Writeable and Readable: the base (non-extended) syntax must be extremely easy
  to write while still being fairly readable. Writeability is prioritize due
  to how it is driven interactively and often thrown away.

But what is the ultimate feature that a shell is missing:

* Text-editor functionality to run a "block" of code (code with no newlines),
  query/view the output+logs, then make changes to the block and run again.
* Stream handling: should easily handle streams of data. Open
  files/scripts/tables and work with the structured data output by filtering,
  joining, aggregating, etc.

But now we are almost talking about a data query language in something like a
Jupiter notebook. Indeed, I want shrm to be directly useable as a data query
language for Civboot databases. Therefore I want it to follow as many principles
of the [D database language] as possible for a conrete syntax language.

[D database language]: https://www.dcs.warwick.ac.uk/~hugh/TTM/DTATRM.pdf

## How fngi will grow into shrm

fngi is a compiled, typesafe, manually memory managed language. How can it
possibly evolve into shrm from a simple library import? First of all: fngi's
syntax is completely controlled by `syn` functions. Syntax can be implemented by
simply calling such a function, i.e. `$shrm`.

Even with this though, how do we handle the differences in types?

* fngi public types are exported with a module/library. Converting to/from
  dynamic, memory managed types should be fairly trivial... if well architected
  for that purpose.
* fngi makes use of an "arena stack". A dynamic language can simply push a
  new arena (allocator), call a fngi function, copy out the returned data and
  then pop & drop the arena from the stack. Types that are not just plain-old-data
  (POD) will not be allowed or will require special handling; i.e. must be a
  dynamic type returned by fngi using a separate (non-stack) passed-in arena.

shrm values will have pointers to their allocator and type embedded with them.
They are pushed onto a global LL for garbage collection. The GC knows how to
walk the type tree and visit each shrm value, so no refcounts/etc need to be
held.

shrm is _not_ a high performance language. For that, use fngi. However, besides
the stop-the-world GC, most libraries will be implemented in fngi, while scripts
will often (but not always!) be implemented in shrm. Keep in mind that shrm is
just a library of fngi, so fngi functions and modules can easily be implemented
in-line when needed.

[fngi]: http://github.com/civboot/fngi

## How to make a database language
I believe a database language can be represented as streams of data processing.
The streams and processing can be represented as a series of transforms
represented in Role (interface) types.

What roles would be necessary?

> Note: More thought is necessary on how shrm will handle these dynamic types
> role types.  It's very possible that is one purpose of shrm -- to enable
> dynamic types for purposes like this!

```
enum Agg {
  any,
  count,
  countDistinct,
  sum,
  min,
  max,
  ...
}

\ The role that all database types must implement.
\ Note: some methods can be NULL if not supported.
role DataType {
  \ The full key for this data type
  fn tuple(self) -> List[DataType];

  \ Convert to/from bytes
  fn serialize(self) -> bytes;
  fn deserialize(b: bytes) -> Self;

  \ Compare two of the same type
  fn compare(other: Self, self) -> ISlot;

  \ Match against a user-defined string (i.e. regex)
  fn match(other: str, self) -> bool;

  \ Aggregate two of the same type, returning the new type.
  fn aggregate(agg: Agg, other: Self, self) -> Self;
}

\ The role that all data sources (i.e. tables, functions)
\ must implement.
role DataSource[D] {
  \ Walk the data source using a key of an array of DataTypes.
  \
  \ If the key matches the primary key of the source, this should return a
  \ stream with ordered=true, enabling a merge join.
  fn walk(key: List[D], self) -> Stream[D];

  \ Join two data sources with given constraints to create a new data source.
  fn join(
    key: List[DataType],
    filter: List[Filter[D]],
    aggregation: List[Aggregation[D]],
    other: Self,
    self) -> Self;
}
```


