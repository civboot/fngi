/** @file kernel/constants.h
 * DO NOT EDIT MANUALLY! THIS FILE WAS GENERATED BY etc/make.py
 *
 * @brief Contains common types in fngi
 */
#ifndef __KERNEL_CONSTANTS_H
#define __KERNEL_CONSTANTS_H


#define I_MEM                0x40
#define I_JMP                0x80
#define NOP                  0x0
#define RETZ                 0x1
#define RET                  0x2
#define SWP                  0x3
#define DRP                  0x4
#define OVR                  0x5
#define DUP                  0x6
#define DUPN                 0x7
#define DV                   0x8
#define RG                   0x9
#define INC                  0x10
#define INC2                 0x11
#define INC4                 0x12
#define DEC                  0x13
#define INV                  0x14
#define NEG                  0x15
#define NOT                  0x16
#define CI1                  0x17
#define CI2                  0x18
#define ADD                  0x20
#define SUB                  0x21
#define MOD                  0x22
#define SHL                  0x23
#define SHR                  0x24
#define MSK                  0x25
#define JN                   0x26
#define XOR                  0x27
#define AND                  0x28
#define OR                   0x29
#define EQ                   0x2A
#define NEQ                  0x2B
#define GE_U                 0x2C
#define LT_U                 0x2D
#define GE_S                 0x2E
#define LT_S                 0x2F
#define MUL                  0x30
#define DIV_U                0x31
#define DIV_S                0x32
#define SZ1                  0x0
#define SZ2                  0x10
#define SZ4                  0x20
#define FT                   0x40
#define FTO                  0x41
#define FTLL                 0x42
#define FTGL                 0x43
#define SR                   0x44
#define SRO                  0x45
#define SRLL                 0x46
#define SRGL                 0x47
#define LIT                  0x48
#define JMPL                 0x80
#define JMPW                 0x81
#define JZL                  0x82
#define JTBL                 0x83
#define XLL                  0x84
#define XLW                  0x85
#define XSL                  0x86
#define XSW                  0x87
#define SLIT                 0xC0
#define D_assert             0x1
#define D_catch              0x2
#define D_memset             0x3
#define D_memcmp             0x4
#define D_memmove            0x5
#define D_bump               0x6
#define D_log                0x7
#define D_file               0x8
#define D_scan               0x9
#define D_dictFind           0xD
#define R_LP                 0x80
#define R_EP                 0x0
#define R_GB                 0x1
#define T_NUM                0x0
#define T_HEX                0x1
#define T_ALPHA              0x2
#define T_SINGLE             0x3
#define T_SYMBOL             0x4
#define T_WHITE              0x5
#define SZ_MASK              0x30
#define META_TY_MASK         0xC0
#define TY_CONST             0x0
#define TY_FN                0x40
#define TY_VAR               0x80
#define TY_DICT              0xC0
#define REF_MASK             0xFFFFFF
#define MOD_MASK             0xFF0000
#define TY_FN_PRE            0x20
#define TY_FN_LARGE          0x10
#define TY_FN_TY_MASK        0xC
#define TY_FN_NORMAL         0x0
#define TY_FN_NOW            0x4
#define TY_FN_SYN            0x8
#define TY_FN_INLINE         0xC
#define TY_VAR_INPUT         0x1
#define TY_VAR_REF           0xC
#define ZOAB_TY              0xC0
#define ZOAB_JOIN            0x80
#define ZOAB_ARR             0x40
#define ZOAB_PTR             0xC0
#define LOG_SILENT           0x0
#define LOG_USER             0x10
#define LOG_TRACE            0x1F
#define LOG_DEBUG            0x17
#define LOG_INFO             0x13
#define LOG_WARN             0x11
#define LOG_CRIT             0x10
#define LOG_INSTR            0x27
#define LOG_EXECUTE          0x23
#define LOG_ASM              0x21
#define LOG_COMPILER         0x20
#define E_ok                 0x0
#define E_general            0xE000
#define E_io                 0xE010
#define E_asm                0xE0A0
#define E_comp               0xE0C0
#define E_test               0xA000
#define E_intern             0xE001
#define E_undef              0xE002
#define E_unreach            0xE003
#define E_todo               0xE004
#define E_wsEmpty            0xE005
#define E_unimpl             0xE006
#define E_dv                 0xE007
#define E_null               0xE0A1
#define E_oob                0xE0A2
#define E_stkUnd             0xE0A3
#define E_stkOvr             0xE0A4
#define E_align2             0xE0A5
#define E_align4             0xE0A6
#define E_divZero            0xE0A7
#define E_oom                0xE0A8
#define E_cInstr             0xE0C1
#define E_cToken             0xE0C2
#define E_cTLen              0xE0C3
#define E_cKey               0xE0C4
#define E_cNoKey             0xE0C5
#define E_cHex               0xE0C6
#define E_sz                 0xE0C7
#define E_cSzPtr             0xE0C8
#define E_cRet               0xE0C9
#define E_cDblSr             0xE0CA
#define E_cDevOp             0xE0CB
#define E_DictOvr            0xE0CC
#define E_cXHasL             0xE0CD
#define E_cXNoL              0xE0CE
#define E_cErr               0xE0CF
#define E_cKeyLen            0xE0D0
#define E_cReg               0xE0D1
#define E_cStr               0xE0D2
#define E_cNotGlobal         0xE0E0
#define E_cIsX               0xE0E1
#define E_cIsXS              0xE0E2
#define E_cJmpL1             0xE0E3
#define E_cNotFn             0xE0E4
#define E_cNotFnLarge        0xE0E5
#define E_cMod               0xE0E6
#define E_cLSz               0xE0E7
#define E_cNotType           0xE0E9
#define E_cNotLocal          0xE0EA
#define E_cNotVar            0xE0EB
#define E_cNotFnOrConst      0xE0EC
#define E_eof                0xE0ED
#define E_cUnclosed          0xE0EE
#define E_cReqNow            0xE0EF
#define E_cNoNow             0xE0EF
#define E_cUnknownEsc        0xE0F0
#define E_cZoab              0xE0F1
#define E_cNeedToken         0xE0F2
#define E_cNeedNumber        0xE0F3
#define E_cBadRefs           0xE0F4
#define E_cRefEq             0xE0F5
#define E_cInlineLarge       0xE0F6
#define E_cColon             0xE0F7
#define E_cFnSyn             0xE0F8
#define E_newBlock           0xE0F9
#define E_iBlock             0xE0B0
#define E_ptrBlk             0xE0B1
#define E_aaPo2              0xE0B2
#define ERR_DATA_NONE        0x0
#define ERR_DATA_INT1        0x1
#define ERR_DATA_DATA1       0x2
#define ERR_DATA_INT2        0x3
#define ERR_DATA_DATA2       0x4

#endif // __KERNEL_CONSTANTS_H
