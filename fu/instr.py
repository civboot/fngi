from .imports import *
from .env import FuEnv
from .stack import Ty

class Instr(BetterEnum): pass

class MemM(Instr):
    SRLP = 0x0
    SRCP = 0x1
    SROI = 0x2
    FTLP = 0x3
    FTCP = 0x4
    FTOI = 0x5
    IMWS = 0x6
    WS   = 0x7

class JumpMd(Instr):
    JIB       = 0x0
    CALL      = 0x1
    JST       = 0x2
    CNW       = 0x3
    reserved0 = 0x4
    reserved1 = 0x5
    RET       = 0x6
    NOJ       = 0x7

class Op(Instr):
    FT  = 0x00
    SR  = 0x01
    DVF = 0x02
    DVS = 0x03
    IDN = 0x04

    DRP = enumVal()
    INV = enumVal()
    NEG = enumVal()
    EQZ = enumVal()
    EQZ_NC = enumVal()

    DRP2 = enumVal()
    OVR = enumVal()
    ADD = enumVal()
    SUB = enumVal()
    MOD = enumVal()
    MUL = enumVal()
    DIV_U = enumVal()
    DIV_S = enumVal()
    OR = enumVal()
    XOR = enumVal()
    SHL = enumVal()
    SHR = enumVal()
    EQU = enumVal()
    NEQ = enumVal()
    GE_U = enumVal()
    GE_S = enumVal()
    LT_U = enumVal()
    LT_S = enumVal()


def testInstrAPI():
    assert 0x0 == MemM.SRLP.value
    assert "SRLP" == MemM.SRLP.name

    assert 0x00 == Op.FT.value
    assert 0x04 == Op.IDN.value
    assert 0x05 == Op.DRP.value

    assert Op.DRP == Op(0x05)
    assert Op.DRP == Op.fromStr("DRP")

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
