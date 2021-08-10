I need to take a bit of a break from implementing this. Here are some notes to
self.

1. I'm implementing fu/. I just finished implementing fu/machine.py. See
   notes/fu.md for what I'm even thinking here.
2. I need a way to actually write instructions to a bytearray so that I can
   execute them.
3. Then test a few basic operations using machine.runFu8
4. All of the fu8 "modes" are implemented, but I havne't implemented almost any
   operations. Use the "Binary" stuff from the wasm commits.
5. Write some tests and a test framework. Use the wast tests to get some
   inspiration, maybe try to port a few.
