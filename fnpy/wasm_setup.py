# Imported by wasm.py
from collections import OrderedDict
from dataclasses import dataclass
import copy
import ctypes

# This is the only way to get the base class for ctypes (_CData) which is
# technically private.
DataTy = ctypes.c_uint8.__bases__[0].__bases__[0]

class Ref(DataTy):
    _fields_ = []
    def __init__(self, ty: DataTy): self.ty = ty

WASM_INSTS = {}

@dataclass
class WasmInst:
    """A wasm instruction."""
    code: int # wasm bytecode
    inputs: List[DataTy]
    outputs: List[DataTy]
    call: Callable[["Env"], None]
    payload: None

@dataclass
class WasmInstPayload:
    """A wasm instruction with a compile-time payload, i.e. a constant"""
    inst: WasmInst
    payload: List[DataTy]


def nativeFn(code, inputs: List[DataTy], outputs: List[DataTy]):
    """Takes some types and a python defined function and converts to an
    instantiated NativeFn.
    """

    # Stack values must be pushed in reverse order (right to left)
    outputsPushTys = list(reversed(outputs))

    def wrapper(pyDef):
        nonlocal name

        def callDef(env):
            nonlocal name # for availability when debugging
            # pop stack items from left to right
            args = [env.dataStack.pop(ty).value for ty in inputs]
            # call the function with them in that order
            outStack = pyDef(env, *args)
            outStack = outStack if outStack else []
            # reverse the output stack because that is the correct order for
            # pushing to the stack
            outStack.reverse()
            assert len(outStack) == len(outputs)
            # push outputs to the data stack
            for out, ty in zip(outStack, outputsPushTys):
                env.dataStack.push(ty(out))

        global NATIVE_FUNCS
        NATIVE_FUNCS[funcidx] = nativeFnInstance
        return nativeFnInstance
    return wrapper


@nativeFn('quit', [], [])
def quit(env):
    """Stop running the interpreter."""
    env.running = False

@nativeFn('i32.const', [I32], [I32])
def _i32_const(env): pass

@nativeFn('i64.const', [I64], [I64])
def _i64_const(env): pass

@nativeFn('call', [], [])
def _i64_const(env, funcidx):
    pass # executed by machine

@nativeFn('call_indirect', [], [])
def _i64_const(env, funcidx):
    pass # executed by machine
