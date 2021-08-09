from .imports import *


class MemM(BetterEnum):
    SRLP  = 0x0
    SRCP  = 0x1
    SROI  = 0x2
    FTLP  = 0x3
    FTCP  = 0x4
    FTOI  = 0x5
    IMWS  = 0x6
    WS    = 0x7

class JumpMd(BetterEnum):
    JIB = 0x0
    CALL = 0x1
    JST = 0x2
    CNW = 0x3
    reserved0 = 0x4
    reserved1 = 0x5
    RET = 0x6
    NOJ = 0x7

class Op(BetterEnum):
    FT = 0x0
    SR = 0x1
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
