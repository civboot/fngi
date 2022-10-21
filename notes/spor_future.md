# Is Spor Syntax necessary?

> **Note:** this will eliminate the spor language (i.e. `#2$L0 %ADD`), not the
> bytecode. The bytecode is still extremely valuable for dynamic execution,
> safety and optimizations.

One of the primary benefits of FORTH is it's ability to transition to/from
native execution. You can write code natively (i.e. assembly) and execute it
from within FORTH.

Why don't I have this? I think it is primarily because I developed spor and DV
instructions. This may have been a mistake.

I could add an `XN` function. It would execute a native function pointer of type
`void (*)()`. This would still interact just fine with my error handling, and
everything else I've implemented.

This is so stupid-simple it's almost embarassing. I got very distracted by DV.
This would permit me several very important things:

 1. I could develop C libraries for components of fngi. I was going to do this
    with `civ_h` anyway, but this makes an even stronger case for it.
 2. Native execution, as stated above.
 3. Eliminate a rather anoying syntax. I don't like spor that much and it's not
    fun to write.
 4. The development of fngi from C would be more streamlined. Since it would be
    more common to call fngi functions from C, I would create a better framework
    for doing so.

The other thing is that I think I was too obsessed with a "single file" library.
I'm now starting to embrace a few other healthy approaches to programming:

 1. Create sub-languages (i.e. zty) which can be parsed by dynamic compilers like
    fngi and python, and generate code for static compilers like C.
 2. Initially I wanted to bootstrap from x86 assembly. That died (I used C), so
    then I wanted to bootstrap as minimally as possible. That died (I added a
    block allocator, arenas, BST, etc), but I kept spor around. I don't think
    any of this was truly helpful. I've bloated my DV's to the point of
    absurdity and created way too much mis-direction to be good for me.

## Some Awesome Sauce
With the use of zty I could have so much generated code that is later consumed
by the fngi zty parser directly.

 1. Auto-generate the fngi functions (in C) for the natives, i.e. `+ - shl`.
    There is no reason for this to be (human written) boilerplate!
 2. Generate `_fromStack` and `_toStk` methods for structs for easy interaction.
    This has the added benefit that everything can be serialized for logging to
    python!

## Embedding with C
Perhaps the coolest feature is that this would enable instant C embedding. C
could register a function to be callable from fngi by just wrapping it in a
`void (*)()` function and adding it to the fngi dictionary directly.

