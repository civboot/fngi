# Harness and Zoab

> Fngi/spor uses a "harness" for passing arguments and interfacing with the
> operating system. The harness is located in [./fngi](./fngi), which is an
> executable python script.

Fngi doesn't interface directly with the linux environment in the typical
fashion. The primary differences are:

  1. It uses zoab for it's configuration and logging, receiving/sending
     structured data.
  2. The structured data it sends is structured in a specific way, allowing for
     the harness to implement core debugging and developer ergonomics tooling.

Why is it implemented this way? There are several reasons:

  1. [Zoab][zoa] is unbelievably simple and efficient to both implement and use.
     It has almost no cost over logging simple strings.
  2. Logging is a core part of the developer code -> test cycle. This is
     _especially_ true on small or embedded systems. One of the first things a
     developer does when targetting an embeded system, after getting blinky
     light working, is to get serial IO of some kind.

So logging will exist anyway, why not just use text logs?

There are gobs of useful information to be had when an error occurs which help a
developer triage the issue. Automating the finding of this information (with
only a little bit of context) is next to trivial when using a full-featured
langauge to code the automation. On the flip side, finding this information
within fngi/spor, which are ultra-simple kernels that unroll into more
full-fetured languages, would bloat them unnecessarily.

For example: trying to format and log the entire stack trace (with function
names, local variable breakdown, etc) would add an absurd amount of complexity
(and memory requirements!) for fngi/spor's core implementation. Not only that,
this complexity would have to be built in very early to have nearly any benefit.
Conversly, simply logging the raw bytes of the stack is trivial. It is also
almost trivial for the harness to take note of the memory addresses of functions
(as they are registered) and local variable offsets and names. Inspecting a
dumped stack is then a fairly simple excersize in software development when
using a full-featured language with memory managed HashMaps.

When developing fngi while using a fngi environment, the "more stable" existing
environment can act as the harness and deal with the debug information, while
the more dynamic system under development can ignore that complexity. You could
even use a separate machine+display to handle the debug output, allowing
minimalism of resource usage. The same is therefore allowed when developing fngi
on a modern OS.

[zoa]: http://github.com/vitiral/zoa
