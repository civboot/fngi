from .imports import *
from .env import FuEnv, createEnv
from .stack import Ty
from .instr import Instr, MemM, JumpM, Op

def _notImplemented(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive):
    raise NotImplementedError(f'{instr}')

def _noop(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive): pass

def _trap(env: FuEnv, instr: Instr, ty: Ty, imm: Primitive):
    raise Trap(f"{instr} ty={ty} imm={hex(imm.value)}")

##########################
# Mem Modes

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

##########################
# Jump Modes

# JIB: jumps to IMM if Bool(WS)
def _JIB(env, instr, ty, imm):
    if env.ds.popv(ty): env.ep = env.cp + imm.value

# CALL: Call an address
# - pop ptr off of store, conert to APtr using CP if necessary.
# - fetch 16bit growWs value at ptr.
# - grow WS by growWs.
# - push EP+INSTR_WIDTH onto RS, including current CP and amount WS grew.
# - jump to ptr+2 (skipping WS size)
def _CALL(env, instr, ty, imm):
    aPtr = env.aPtr(env.ws.pop(ty))

    wsGrow = env.mem.fetchv(env.aPtr(aPtr), U16)
    env.ws.grow(wsGrow)
    env.pushr(APtr(aPtr.value+INSTR_WIDTH), wsGrow)
    # note: skipping the U16 we read for stack growth.
    env.setEp(aPtr.value + ctypes.sizeof(U16))

# JST: jumps to WS
def _JST(env, instr, ty, imm):
    aPtr = env.aPtr(env.ws.pop(ty))
    env.setEp(aPtr)

# CNW: calls WS without update to ws growth.
def _CNW(env, instr, ty, imm):
    aPtr = env.aPtr(env.ws.pop(ty))
    env.pushr(aPtr, wsGrow=0)
    env.setEp(aPtr)

# RET: return
# - pop address and WS growth from RS.
# - shrink WS by grown amount
# - jump to address
def _RET(env, instr, ty, imm):
    aPtr, wsGrow = env.popr()
    env.ws.shrink(wsGrow)
    env.setEp(aPtr)

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

    # Jump
    JumpM.JIB: _JIB,
    JumpM.CALL: _CALL,
    JumpM.JST : _JST,
    JumpM.CNW : _CNW,
    JumpM.RET : _RET,
    JumpM.NOJ: _noop,
}

def testActions():
    env = createEnv()
