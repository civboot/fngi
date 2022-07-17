# Dot compiler

The dot compiler is probably the most syntax-heavy component of fngi.
The initial dot compiler supports all of the below syntaxes:

```fngi
\   .var            \ variable fetch
\   .var = <token>  \ variable store
\   .&var           \ variable reference (also function)
\   .@var           \ variable dereference
\   .@var = <token> \ variable dereference store
```

In order to accomplish this (and cleanup the path for 16bit) several changes
need(ed) to be made:

- [done] Reference depth had to be added to both local and globals. We only support
  depth <= 3, so this easily fits in the current 8bit meta.
- [done] Switch to &metaRef instead of metaRef for everything. This also clears the way
  for moving the meta byte to be separate from the value (not a good idea for
  16bit systems, probably not a good idea regardless).
  - first use either meta or &meta in C.
  - then populate both for all types.
  - then remove &metaRef (converting to just &ref)
- [done] Declaration for locals/globals needs to support reference depth
