https://github.com/civboot/fngi/issues/9

Here are a few small changes/tweaks that are on my mind. These are necessary for the language to feel much more "complete" and actually usable.

Basically this will support
* struct methods
* reading/writing files
* debugging: walking stack from REPL and viewing locals
* modules and generic types
* adding methods to structs
* text-based macros

Test:
* [x] Get panics working. Have function to auto cleanup state
* [x] Create syn function (and C function) which expects panic message and cleans up. Use it liberally
* [x] imm syn function and imm behavior is generally poorly tested
* [x] do serious testing of Role and &Role

Document
* [x] write starter document

Cleanup
* [x] ~Get rid of Globals struct~ move globals and all types to comp and use
  code generation.

Features:
* [x] civc
  * ~Reader and Writer should be different types -- no file type!~ nah, not worth it.
  * [x] Add Str type (really just a Slc[U1]), conventionally for readable text.
  * [x] rename Bst -> CBst (CStr key)
  * [X] BStk: moved to https://github.com/civboot/civc/issues/1, not going to implement now
  * ~Arr: similar to a Buf, but the data is inline. `struct Arr { U2 len; U2 cap; U1 dat[]; }`~ nah, maybe later
* [x] use 2 byte literal for all jmps (if/blk/etc). It's easy to later add an if1 or blk1 for optimization.
* [x] Add `XL` which executes a local offset and `XLR` which executes a local role method offset (two literals!). These are how fngi handles function pointers. Possibly remove XW as there is literally no way to make it type-safe since it would need to be the LAST value on the stack.
  * You could do syntax like `(1, 2, 3) -> xw myFunPtr` I suppose... might not be the worst...
* [x] Add TyDbNow, track and check types at compile time. It is impossible to turn off the type checker at compile time.
* [X] g.dictStk needs to be double pointers, else things might change behind values
* [X] Don't throw away locals metadata. Simplifies generics, is good for native compilation and practically necessary for debugging.
  * Store it on the TyFn (new field)
* [x] Support Role directly in fngi
  * need to create Sp_UFile and Sp_BufFile implementations that are callable from fngi/spor
  * add functions to call the native version if it is available
* [x] rename now -> imm (immediate)
* [x] add TyMod (TY_DICT_MOD) type
   * [ X] add `using` syntax for definition inside sub-dictionary
* [X] add globals support
* [x] add necessary constants, functions and roles to fngi namespaces
  * [x] module comp: token (compile single token), call (call function), lit (compile literal)
* [x] Struct methods
* [X] aliases, struct inheritance 
* [x] Str syntax
  * [x] `|this is a string, it ends here|`
  * ~'this-is-a-token-string this_is_not` token strings end with whitespace or any of: `,()`~ not necessary in base compiler.
* [x] Roles and role syntax
* [x] Minor
  * [x] &global nees to be implemented. Add tests in multiple features (struct, role, etc)
  * [x] imm#if is not implemented. Do that.
  * [x] rename `with` -> `use`
  * [x] implement lit (compile literal immediate)
  * [x] Implement const
  * [x] disallow overriding methods. It's very dangerous, since binary
        compatibility means that functions will call the "parent" class's method
        even if a child overrode it. There's no such problem for "associated
        functions" since you need to use the full type for that (it can't be
        accessed on the method) so there's less confusion.
  * [x] Lots of TODOs where I'm using literal instead of global reference
* [ ] Native implemented roles
  * [x] Arena (BAA)
  * [ ] File (UFile)
  * [ ] Reader
* [x] ~Logger #28~ this won't make it into 0.2.0, but there will be a stdout writer I can use.
* [ ] Str libraries: concat, search, format number, etc.
  * [ ] the fundamental data type for searching: SllSlc
  * [ ] the fundamental data type for mutating: DllBuf (PieceChain with Buffers)
* [ ] Debug support
  * [ ] Error handling - starts new fiber and inspect broken one
  * [ ] inspection helpers: print inspected locals, etc
  * [ ] TokenLoc: store locations of tokens in a file for later lookup / debugging (dbgRs)


