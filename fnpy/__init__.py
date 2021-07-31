# Python implementation of fngi language.
#
# This implementation simultaniously defines a "bytecode spec" for the stage0
# compiler runtime. This will allow a future implementation (aka in Forth)
# to have a reference for testing and inspecting the runtime, including
# variables, function defintions, etc.
#
# This requires careful and extensive use of python's `ctypes` module,
# which provides c-like (and therefore fngi-like) datatypes for us to use.

# (c-style) types and how to allocate them.
from . import env

# The parser
from . import parser

