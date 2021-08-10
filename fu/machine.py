from .imports import *
from .env import FuEnv, createEnv
from .stack import Ty
from .instr import Instr, MemM, JumpM, Op
from .actions import INSTR_ACTIONS

def sizeBitsToTy(bits: int):
    """
    0: 32bit    1: 16bit
    3: 8bit     4: undefined
    """
    if bits == 0: return U32
    elif bits == 1: return U16
    elif bits == 3: return U8
    else: raise Trap(f"size {bits}")


def getImm(env, instr, ep, ty=U16):
    """Get the immediate associated with an ep."""
    imm = env.mem.fetch(APtr(ep), ty)
    return ep + ctypes.sizeof(ty), imm

def unpackFu8(env: EnvFu, instr: int):
    ep = env.ep
    imm = None

    # 11   | X | 2b size | 3b X
    if (instr & 0xC0) == 0xC0: # get ty for jmp and mem
        ty = sizeBitsToTy((instr & 0x18) >> 3)

    # 11   | 0 | 2b size | 3b jump
    if (instr & 0xE0) == 0xC0:
        instr = JumpM(instr & 0x07)
        if instr is JumpM.JIB:
            ep, imm = getImm(env, instr, ep)

    # 11   | 1 | 2b size | 3b mem
    elif (instr & 0xE0) == 0xE0:
        instr = MemM(instr & 0x07)
        if instr is MemM.IMWS:
            ep, imm = getImm(env, instr, ep, ty=ty)
        elif instr is MemM.WS:
            pass
        else:
            ep, imm = getImm(env, instr, ep)

    # size | 6b operation
    else:
        ty = sizeBitsToTy((instr & 0xC) >> 6)
        instr = Op(instr & 0x3F)

    return ep, instr, ty, imm


def runFu8(env: FuEnv):

    while True:
        instrEp = env.ep
        instr = env.mem.fetchv(APtr(instrEp), U8)
        env.setEp(ep + INSTR_WIDTH)
        newEp, instr, ty, imm = unpackFu8(env, instr)
        if ep != newEp: env.setEp(newEp)

        INSTR_ACTIONS[instr](env, instr, ty, imm)
