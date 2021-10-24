from .instr.MemM import *
from .instr import instrSizeTy

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
    top = 0
    srPtr = 0 # 0=WS, else it is a pointer.

    if mem <= FTLP.value:
        if mem <= SRCP.value:
            if mem == SRLP.value: # SRLP
                srPtr = env.lp + env.popImm()
                top = env.ws.popv(sizeTy)
            else: # SRCP
                srPtr = env.cp + env.popImm()
                top = env.ws.popv(sizeTy)
        else:
            if mem == SROI.value: # SROI
                srPtr = env.ws.popv(env.ptrTy)
                top = env.popImm()
            else: # FTLP
                top = env.fetchLp(env.popImm())

    else: # >= FTCI
        if mem <= FTCI.value:
            if mem == FTCI.value: # FTCI
                top = env.fetchCp(env.popImm())
            else: # FTOI
                top = env.mem.fetchv(env.ws.popv(env.ptrTy), sizeTy)
        else:
            if mem == IMWS.value: # IMWS
                top = env.popImm()
            else: # WS
                top = env.ws.popv(sizeTy)

    # Get second value
    if mem == FTCI.value: snd = env.popImm()
    else: snd = env.ws.popv(sizeTy)


def runFu16Instr(env: FuEnv, instr: int):
    sizeTy = instrSizeTy(instrSize(instr))
    mem = instrMem(instr)
    jmp = instrJmp(instr)
    op = instrOp(instr)


def runFu16(env: FuEnv):
    while True:
        instrEp = env.ep
        instr = env.mem.fetchv(APtr(instrEp), U16)
        env.setEp(ep + INSTR_WIDTH)
        runFu16Instr(env, instr)
