
The use of python as the initial implementation language is about much more
than just having a fully-featured and prototypeable language. Python is
an _excellent_ language for developing tests in. By choosing python we
are able to:

1. Use the excellent `ctypes` library to get perfect C structures with
   minimal programming.
1. Write our initial implementation without too much worry over
   memory/types/etc.
1. Write an extensive test suite of all our prototypes
1. Write an _interface into our forth implementation_ to call forth-implemented
   fngi functions and inspect their results (on the datastack)
1. Write a class which uses the above interface to drive various
   forth-implemented interfaces such as the hash map implementation, memory
   managers, etc.
1. Putting 2 and 2 together: when running a test built for python we can
   simultaniously run the test for forth. If there is a failure in forth we can
   then inspect both memory regions and compare them. Is the forth data stack
   different from python's? In what way?  Is the forth return stack different?
   In what way?

