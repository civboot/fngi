# Round Two

My first implementation of fngi (round 1) was a success! I managed to create an
assembly language (spor) which bootstrapped into a syntactically readable
language (fngi). If you want to see that version, check out commit 402df12.

I learned a lot from it, but I've come to realize the approach is misguided. I
don't want to recreate [bootstrappable.org] -- I should make a _simple
language_ that is _inspectible_.

My new language will stick to many of the design principles, but will be
different in the following ways:

1. It will have tight C integration. This primarily means that it can call C
   functions "natively" and there will no longer be a DV opcode.
2. It will depend on a few small libraries which follow the fngi design
   philosophy and can be independently tested. These are [zoa][zoa] and
   [civc][civc]. This will let me abstract the "simple but complete bedrock"
   from the interesting task of writing a simple compiler.
3. It will be inspecible from the start. This means:
  * All function calls will be "double pointers" where the first pointer is to
    the dictionary node containing all the metadata. This will be tracked in an
    "info stack" which means that the callstack can easily be printed.
  * I will prioritize the REPL, with a simple line-oriented one from the
    beginning. When there is an error you will be able to run fngi code in-line
    to inspect the function and walk the stack. This will be extendable from
    fngi code.
4. The spor AST will contain hints of line number and block ending. This is
   mostly for the debugger and future IR parser to be able to injest, but it
   will also be useful for...
5. ... the typechecker (and structs) will be available from the start. I've
   decided that while it would be really cool to implement the core language in
   the non-type checked language, it doesn't really make sense. I'd rather spend
   a thousand lines of C code (really, I think it will be that small) and have
   an ergonomic language from the start than muscle my way to creating a type
   checker in a non-typed language.

[zoa]: http://github.com/civboot/zoa
[civc]: http://github.com/civboot/civc
[bootstrappable.org]: http://bootstrappable.org
