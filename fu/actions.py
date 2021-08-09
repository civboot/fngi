from .imports import *
from .env import FuEnv, createEnv
from .stack import Ty
from .instr import Instr, MemM, JumpM, Op

def _notImplemented(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive):
    raise NotImplementedError(f'{instr}')

def _noop(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive): pass

def _trap(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive):
    raise Trap(f"{instr} ty={ty} imm={imm}")

# SRLP: Store WS0 at `LP+IM`
def _SRLP(env, instr, ty, imm):
    value = env.ws.pop(ty)
    env.mem.store(env.lp + imm.value, value)

# SRCP: Store WS0 at `CP+IM`
def _SRCP(env, instr, ty, imm):
    value = env.ws.pop(ty)
    env.mem.store(env.cp + imm.value, value)

# FTLP: Fetch value at `LP+IM` onto WS
def _FTLP(env, instr, ty, imm):
    value = env.mem.load(env.lp + imm.value, ty)
    env.ws.push(value)

# FTCP: Fetch value at `CP+IM` onto WS
def _FTCP(env, instr, ty, imm):
    value = env.mem.load(env.cp + imm.value, ty)
    env.ws.push(value)

# IMWS: push IMM onto WS
def _IMWS(env, instr, ty, imm):
    sign = -1 if imm.value < 0 else 1
    value = sign * ty(abs(imm.value))
    env.ws.push(value)

INSTR_ACTIONS = {
    # Mem
    MemM.SRLP: _SRLP,
    MemM.SRCP: _SRCP,
    MemM.SROI: _trap,
    MemM.FTLP: _FTLP,
    MemM.FTCP: _FTCP,
    MemM.FTOI: _trap,
    MemM.IMWS: _IMWS,
    MemM.WS  : _noop,
}

def testActions():
    env = createEnv()
