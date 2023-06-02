/* Custom generated by etc/gen.py */

#include "civ.h"
#include "spor.h"

/*extern*/ U1* unknownInstr = "UNKNOWN";

Slc instrName(U1 instr) {
  switch(instr) {
    case NOP             : return Slc_ntLit("NOP");
    case RETZ            : return Slc_ntLit("RETZ");
    case RET             : return Slc_ntLit("RET");
    case YLD             : return Slc_ntLit("YLD");
    case SWP             : return Slc_ntLit("SWP");
    case DRP             : return Slc_ntLit("DRP");
    case OVR             : return Slc_ntLit("OVR");
    case DUP             : return Slc_ntLit("DUP");
    case DUPN            : return Slc_ntLit("DUPN");
    case DV              : return Slc_ntLit("DV");
    case RG              : return Slc_ntLit("RG");
    case OWR             : return Slc_ntLit("OWR");
    case LR              : return Slc_ntLit("LR");
    case GR              : return Slc_ntLit("GR");
    case XR              : return Slc_ntLit("XR");
    case INC             : return Slc_ntLit("INC");
    case INC2            : return Slc_ntLit("INC2");
    case INC4            : return Slc_ntLit("INC4");
    case DEC             : return Slc_ntLit("DEC");
    case INV             : return Slc_ntLit("INV");
    case NEG             : return Slc_ntLit("NEG");
    case NOT             : return Slc_ntLit("NOT");
    case CI1             : return Slc_ntLit("CI1");
    case CI2             : return Slc_ntLit("CI2");
    case ADD             : return Slc_ntLit("ADD");
    case SUB             : return Slc_ntLit("SUB");
    case MOD             : return Slc_ntLit("MOD");
    case SHL             : return Slc_ntLit("SHL");
    case SHR             : return Slc_ntLit("SHR");
    case MSK             : return Slc_ntLit("MSK");
    case JN              : return Slc_ntLit("JN");
    case XOR             : return Slc_ntLit("XOR");
    case AND             : return Slc_ntLit("AND");
    case OR              : return Slc_ntLit("OR");
    case EQ              : return Slc_ntLit("EQ");
    case NEQ             : return Slc_ntLit("NEQ");
    case GE_U            : return Slc_ntLit("GE_U");
    case LT_U            : return Slc_ntLit("LT_U");
    case GE_S            : return Slc_ntLit("GE_S");
    case LT_S            : return Slc_ntLit("LT_S");
    case MUL             : return Slc_ntLit("MUL");
    case DIV_U           : return Slc_ntLit("DIV_U");
    case DIV_S           : return Slc_ntLit("DIV_S");
    case FT + SZ1        : return Slc_ntLit("FT1");
    case FT + SZ2        : return Slc_ntLit("FT2");
    case FT + SZ4        : return Slc_ntLit("FT4");
    case FTBE + SZ1      : return Slc_ntLit("FTBE1");
    case FTBE + SZ2      : return Slc_ntLit("FTBE2");
    case FTBE + SZ4      : return Slc_ntLit("FTBE4");
    case FTO + SZ1       : return Slc_ntLit("FTO1");
    case FTO + SZ2       : return Slc_ntLit("FTO2");
    case FTO + SZ4       : return Slc_ntLit("FTO4");
    case FTLL + SZ1      : return Slc_ntLit("FTLL1");
    case FTLL + SZ2      : return Slc_ntLit("FTLL2");
    case FTLL + SZ4      : return Slc_ntLit("FTLL4");
    case FTGL + SZ1      : return Slc_ntLit("FTGL1");
    case FTGL + SZ2      : return Slc_ntLit("FTGL2");
    case FTGL + SZ4      : return Slc_ntLit("FTGL4");
    case SR + SZ1        : return Slc_ntLit("SR1");
    case SR + SZ2        : return Slc_ntLit("SR2");
    case SR + SZ4        : return Slc_ntLit("SR4");
    case SRBE + SZ1      : return Slc_ntLit("SRBE1");
    case SRBE + SZ2      : return Slc_ntLit("SRBE2");
    case SRBE + SZ4      : return Slc_ntLit("SRBE4");
    case SRO + SZ1       : return Slc_ntLit("SRO1");
    case SRO + SZ2       : return Slc_ntLit("SRO2");
    case SRO + SZ4       : return Slc_ntLit("SRO4");
    case SRGL + SZ1      : return Slc_ntLit("SRGL1");
    case SRGL + SZ2      : return Slc_ntLit("SRGL2");
    case SRGL + SZ4      : return Slc_ntLit("SRGL4");
    case SRLL + SZ1      : return Slc_ntLit("SRLL1");
    case SRLL + SZ2      : return Slc_ntLit("SRLL2");
    case SRLL + SZ4      : return Slc_ntLit("SRLL4");
    case LIT + SZ1       : return Slc_ntLit("LIT1");
    case LIT + SZ2       : return Slc_ntLit("LIT2");
    case LIT + SZ4       : return Slc_ntLit("LIT4");
    case LCL             : return Slc_ntLit("LCL");
    case XL              : return Slc_ntLit("XL");
    case JL + SZ1        : return Slc_ntLit("JL1");
    case JL + SZ2        : return Slc_ntLit("JL2");
    case JL + SZ4        : return Slc_ntLit("JL4");
    case JLZ + SZ1       : return Slc_ntLit("JLZ1");
    case JLZ + SZ2       : return Slc_ntLit("JLZ2");
    case JLZ + SZ4       : return Slc_ntLit("JLZ4");
    case JTBL + SZ1      : return Slc_ntLit("JTBL1");
    case JTBL + SZ2      : return Slc_ntLit("JTBL2");
    case JTBL + SZ4      : return Slc_ntLit("JTBL4");
    case SLIC + SZ1      : return Slc_ntLit("SLIC1");
    case SLIC + SZ2      : return Slc_ntLit("SLIC2");
    case SLIC + SZ4      : return Slc_ntLit("SLIC4");
    case XW              : return Slc_ntLit("XW");
    case XLL             : return Slc_ntLit("XLL");
    case XRL             : return Slc_ntLit("XRL");
    case SLIT + 0x0      : return Slc_ntLit("{0x00}");
    case SLIT + 0x1      : return Slc_ntLit("{0x01}");
    case SLIT + 0x2      : return Slc_ntLit("{0x02}");
    case SLIT + 0x3      : return Slc_ntLit("{0x03}");
    case SLIT + 0x4      : return Slc_ntLit("{0x04}");
    case SLIT + 0x5      : return Slc_ntLit("{0x05}");
    case SLIT + 0x6      : return Slc_ntLit("{0x06}");
    case SLIT + 0x7      : return Slc_ntLit("{0x07}");
    case SLIT + 0x8      : return Slc_ntLit("{0x08}");
    case SLIT + 0x9      : return Slc_ntLit("{0x09}");
    case SLIT + 0xA      : return Slc_ntLit("{0x0A}");
    case SLIT + 0xB      : return Slc_ntLit("{0x0B}");
    case SLIT + 0xC      : return Slc_ntLit("{0x0C}");
    case SLIT + 0xD      : return Slc_ntLit("{0x0D}");
    case SLIT + 0xE      : return Slc_ntLit("{0x0E}");
    case SLIT + 0xF      : return Slc_ntLit("{0x0F}");
    case SLIT + 0x10     : return Slc_ntLit("{0x10}");
    case SLIT + 0x11     : return Slc_ntLit("{0x11}");
    case SLIT + 0x12     : return Slc_ntLit("{0x12}");
    case SLIT + 0x13     : return Slc_ntLit("{0x13}");
    case SLIT + 0x14     : return Slc_ntLit("{0x14}");
    case SLIT + 0x15     : return Slc_ntLit("{0x15}");
    case SLIT + 0x16     : return Slc_ntLit("{0x16}");
    case SLIT + 0x17     : return Slc_ntLit("{0x17}");
    case SLIT + 0x18     : return Slc_ntLit("{0x18}");
    case SLIT + 0x19     : return Slc_ntLit("{0x19}");
    case SLIT + 0x1A     : return Slc_ntLit("{0x1A}");
    case SLIT + 0x1B     : return Slc_ntLit("{0x1B}");
    case SLIT + 0x1C     : return Slc_ntLit("{0x1C}");
    case SLIT + 0x1D     : return Slc_ntLit("{0x1D}");
    case SLIT + 0x1E     : return Slc_ntLit("{0x1E}");
    case SLIT + 0x1F     : return Slc_ntLit("{0x1F}");
    case SLIT + 0x20     : return Slc_ntLit("{0x20}");
    case SLIT + 0x21     : return Slc_ntLit("{0x21}");
    case SLIT + 0x22     : return Slc_ntLit("{0x22}");
    case SLIT + 0x23     : return Slc_ntLit("{0x23}");
    case SLIT + 0x24     : return Slc_ntLit("{0x24}");
    case SLIT + 0x25     : return Slc_ntLit("{0x25}");
    case SLIT + 0x26     : return Slc_ntLit("{0x26}");
    case SLIT + 0x27     : return Slc_ntLit("{0x27}");
    case SLIT + 0x28     : return Slc_ntLit("{0x28}");
    case SLIT + 0x29     : return Slc_ntLit("{0x29}");
    case SLIT + 0x2A     : return Slc_ntLit("{0x2A}");
    case SLIT + 0x2B     : return Slc_ntLit("{0x2B}");
    case SLIT + 0x2C     : return Slc_ntLit("{0x2C}");
    case SLIT + 0x2D     : return Slc_ntLit("{0x2D}");
    case SLIT + 0x2E     : return Slc_ntLit("{0x2E}");
    case SLIT + 0x2F     : return Slc_ntLit("{0x2F}");
  }
  return (Slc) {.dat = unknownInstr, .len = 7};
}
