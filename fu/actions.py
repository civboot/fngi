from .imports import *
from .env import FuEnv, createEnv
from .stack import Ty, StkUnderflowError
from .instr import Instr, MemM, JumpM, Op

class ExitFuError(RuntimeError): pass

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

# JZ: jumps to IMM if Bool(WS)
def _JZ(env, instr, ty, imm):
    if env.ds.popv(ty): env.ep = env.cp + imm.value

# CALL: Call an address
# - pop ptr off of store, conert to APtr using CP if necessary.
# - fetch 16bit growWs value at ptr.
# - grow WS by growWs.
# - push EP onto RS, including current CP and amount WS grew.
# - jump to ptr+2 (skipping WS size)
def _CALL(env, instr, ty, imm):
    ep = env.ep

    aPtr = env.aPtr(env.ws.pop(ty))
    wsGrow = env.mem.fetchv(env.aPtr(aPtr), U16)
    env.ws.grow(wsGrow)
    env.pushr(APtr(ep), wsGrow)
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
    try:
        aPtr, wsGrow = env.popr()
    except StkUnderflowError:
        env.setEp(None)
        raise ExitFuError()

    env.ws.shrink(wsGrow)
    env.setEp(aPtr)


##########################
# Operations

def _ADD(env, instr, ty, imm):
    right = env.ws.popv(ty)
    left = env.ws.popv(ty)
    env.ws.push(ty(left + right))


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
    JumpM.JZ: _JZ,
    JumpM.CALL: _CALL,
    JumpM.JST : _JST,
    JumpM.CNW : _CNW,
    JumpM.RET : _RET,
    JumpM.NOJ: _noop,

    # Operations
    Op.FT: _notImplemented,
    Op.SR: _notImplemented,
    Op.DVF: _notImplemented,
    Op.DVS: _notImplemented,
    Op.IDN: _notImplemented,

    Op.DRP: _notImplemented,
    Op.INV: _notImplemented,
    Op.NEG: _notImplemented,
    Op.EQZ: _notImplemented,
    Op.EQZ_NC: _notImplemented,

    Op.DRP2: _notImplemented,
    Op.OVR: _notImplemented,
    Op.ADD: _ADD,
    Op.SUB: _notImplemented,
    Op.MOD: _notImplemented,
    Op.MUL: _notImplemented,
    Op.DIV_U: _notImplemented,
    Op.DIV_S: _notImplemented,
    Op.OR: _notImplemented,
    Op.XOR: _notImplemented,
    Op.SHL: _notImplemented,
    Op.SHR: _notImplemented,
    Op.EQU: _notImplemented,
    Op.NEQ: _notImplemented,
    Op.GE_U: _notImplemented,
    Op.GE_S: _notImplemented,
    Op.LT_U: _notImplemented,
    Op.LT_S: _notImplemented,
}

def testActions():
    env = createEnv()
