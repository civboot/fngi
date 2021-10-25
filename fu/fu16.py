from .instr import OpM
from .instr import MemM
from .instr import instrSizeTy
from .instr import JmpM
from .instr.MemM import *

INSTR_WIDTH = 2

def instrOp(instr: int):
    return 0x3F & instr

def instrMem(instr: int):
    return 0x7 & (instr >> 6)

def instrJmp(instr: int):
    return 0x7 & (instr >> (2 + 3 + 6))

def instrSize(instr: int):
    return 0x7 & (instr >> (3 + 2 + 3 + 6))

def doMem(env: FuEnv, mem: int, sizeTy):

    return top, scnd, srPtr, usesImm

def _notImplemented(op, *_args):
    raise NotImplementedError(f"op: {op}")

opsMap = {
    OpM.DRP: lambda op, sizeTy, top, scnd: [scnd],
    OpM.DRP: _notImplemented,
    OpM.INV: _notImplemented,
    OpM.NEG: _notImplemented,
    OpM.EQZ: lambda op, sizeTy, top, scnd: [top == 0, scnd],
    OpM.EQZ_NC: [top == 0, top, scnd],

    OpM.DRP2: lambda op, sizeTy, top, scnd: [],
    OpM.OVR: lambda op, sizeTy, top, scnd: [scnd, top, scnd],
    OpM.ADD: lambda op, sizeTy, top, scnd: [scnd + top],
    OpM.SUB: lambda op, sizeTy, top, scnd: [scnd - top],

    # TODO...
}

def runFu16Instr(env: FuEnv, instr: int):
    op = OpM(               0x3F & instr)
    mem = MemM(             0x7  & (instr >>              6))
    jmp = JmpM(             0x7  & (instr >>     (2 + 3 + 6)))
    sizeTy = instrSizeTy(   0x7  & (instr >> (3 + 2 + 3 + 6)))
    if op == Op.FT and not (mem  == WS and sizeTy == env.ptrTy):
        raise TypeError("FT must use WS and size=ptr")
    if op == Op.SR and mem not in (WS, IMWS):
        raise TypeError("SR can only be used with WS,IMWS")
    if jmp not in (JmpM.NOJ, JmpM.RET):
        if jmp in (JmpM.JIB, JmpM.JTBL) and mem != WS:
            raise TypeError(
                "JIB/JTBL require IMM for offset/table meta so mem cannot use it.")
        elif mem.value <= SROI.value:
            raise TypeError("jumps require Mem.Store = WS")

    ####################
    # Mem: get the values to use for the op.
    top = 0
    srPtr = 0 # 0=WS, else it is a pointer.
    usesImm = False

    if mem.value <= FTLP.value:
        usesImm = True
        if mem.value <= SRCP.value:
            if mem == SRLP: # SRLP
                srPtr = env.lp + env.popImm()
                top = env.ws.popv(sizeTy)
            else: # SRCP
                srPtr = env.cp + env.popImm()
                top = env.ws.popv(sizeTy)
        else:
            if mem == SROI: # SROI
                srPtr = env.ws.popv(env.ptrTy)
                top = env.popImm()
            else: # FTLP
                top = env.fetchLp(env.popImm())

    else: # >= FTCI
        if mem.value <= FTCI.value:
            if mem == FTCI: # FTCI
                usesImm = True
                top = env.fetchCp(env.popImm())
            else: # FTOI
                top = env.mem.fetchv(env.ws.popv(env.ptrTy), sizeTy)
        else:
            if mem == IMWS: # IMWS
                usesImm = True
                top = env.popImm()
            else: # WS
                top = env.ws.popv(sizeTy)

    # Get second value
    if op == Op.SR:
        if mem != (WS, IMWS): raise TypeError("SR can only be used with WS,IMWS")
        snd = env.ws.popv(env.ptrTy)
    else:
        if mem == FTCI: snd = env.popImm()
        else: snd = env.ws.popv(sizeTy)

    ####################
    # Op: perform the operation
    stk = opsMap[Op(op)](op, sizeTy, top, scnd)
    assert len(stk) <= 3

    ####################
    # Jump: perform the jump
    jmpLoc = 0

    if jmp == JmpM.NOJ:
        pass
    elif jmp == JmpM.JIB:
        jmpLoc = env.popImm()
    elif jmp == JmpM.CALL:
        raise NotImplementedError("CALL")
    elif jmp == JmpM.JST:
        jmpLoc, stk = stk[0], stk[1:]
    elif jmp == JmpM.CNW:
        raise NotImplementedError("CNW")
    elif jmp == JmpM.JTBL:
        raise NotImplementedError("JTBL")
    elif jmp == JmpM.RET:
        raise NotImplementedError("RET")

    ####################
    # Store result
    if srPtr: env.mem.store(srPtr, stk[0])
    elif stk: env.ws.push(stk[0])

    if len(stk) > 1: env.ws.push(stk[1])
    if len(stk) > 2: env.ws.push(stk[2])


def runFu16(env: FuEnv):
    while True:
        instrEp = env.ep
        instr = env.mem.fetchv(APtr(instrEp), U16)
        env.setEp(ep + INSTR_WIDTH)
        runFu16Instr(env, instr)
