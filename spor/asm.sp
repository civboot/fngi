// Spore assembly constants.
//
// Bit layout of an instruction (X=unused)
//    Jmp    Mem   Sz Operation
//    JJJ XX MMM   SS OO OOOO
//   | high byte |  low byte |

// Instr Bit Layout (S=sz bit)
// 00XX XXXX: operation
// 01XX XXXX: small literal
// 10SS XXXX: jmp
// 11SS _XXX: mem


.4
// **********
// * Instructions

// Jmps:
// - Jumps can be to either a literal (L) or to an item on the working stack (W).
// - X means "execute" and is a call.
// - XS is an "execute small" and means that the function has no local stack.
// - XL/XW will execute a function that has a local stack. The size of the
//   local stack is stored in the first 16bits at the function's address, which
//   are loaded by the execute instr and stored in the highest byte in the
//   callstack (which RET uses to shrink the local stack on return).

// # Operations: Special
#00 =NOP   // { -> }     no operation
#01 =RETZ  // Return if zero
#02 =RET   // Return
#03 =SWP   // {l r -> r l} swap
#04 =DRP   // {l -> }    drop
#05 =DRP2  // {l r -> }  drop 2
#06 =DUP   // {l -> l l} duplicate
#07 =DUPN  // {l -> l l==0} DUP then NOT
#08 =DVFT  // Device Operation Load
#09 =DVSR  // Device Operation Store
#0A =RGL   // Register Load
#0B =RGS   // Register Store

// # Operations: One Inp {l} -> One Out
#10 =INC   // {l+1}  increment 1
#11 =INC2  // {l+2}  increment 2
#12 =INC4  // {l+4}  increment 4
#13 =DEC   // {l-4}  decrement 1
#14 =INV   // {~l}   Bitwise Inversion
#15 =NEG   // {-l}   Negate (2's compliment)
#16 =NOT   // {l==0} Logical NOT
#17 =CI1   // {ISz}  Convert I1 to ISz
#18 =CI2   // {ISz}  Convert I2 to ISz
// future: leading 0's, trailing 0's, count of 1's
// Some single-arg extension commands might be:
// (7) floating point abs, negative, ceil, floor, trunc, nearest, and sqrt
// (1) i -> f conversion
// (1) f -> i conversion

// # Operations: Two Inp {l r} -> One Out
#20 =ADD   // {l +  r } add
#21 =SUB   // {l -  r } subtract
#22 =MOD   // {l %  r } integer modulo (remainder)
#23 =SHL   // {l << r } bit shift left
#24 =SHR   // {l >> r } bit shift right
#25 =AND   // {l &  r } bitwise and
#26 =OR    // {l |  r } bitwise or
#27 =XOR   // {l ^  r } bitwise xor
#28 =LAND  // {l && r } logical and
#29 =LOR   // {l || r } logical or
#2A =EQ    // {l == r } equal
#2B =NEQ   // {l != r } not equal
#2C =GE_U  // {l >= r } unsigned greater than or equal
#2D =LT_U  // {l <  r } unsigned less than
#2E =GE_S  // {l >= r } signed greater than or equal
#2F =LT_S  // {l <  r } signed less than

#30 =MUL   // {l *  r } multiplication
#31 =DIV_U // {l / r  } unsigned division
#32 =DIV_S // {l / r  } signed division
// Double-arg extension commands might be:
// floating point: add,sub,mul,div,ge,lt

// # Small Literal [0x40 - 0x80)
#40 =SLIT

// # Sizes
#00 =SZ1
#10 =SZ2
#20 =SZ4
#20 =SZA

// # Jmp      Description
#80 =JMPL  // Jmp to Literal
#81 =JMPW  // Jmp to WS
#82 =JZL   // Jmp to Literal if store==0
#83 =JTBL  // Jump to Table index using size=Literal
#84 =XL    // Execute Literal (mPtr)
#85 =XW    // Execute WS (aPtr)
#86 =XSL   // Execute Small Literal (no LS update)
#87 =XSW   // Execute Small WS (no LS update)

// # Mem      Store    Description
#C0 =LIT   // LIT         Literal
#C1 =FT    // FT(WS)      FeTch WS
#C2 =FTLL  // FT(LP+LIT)  FeTch LocalsPtr offset
#C3 =FTML  // FT(MP+LIT)  FeTch ModulePtr offset
#C4 =SR    // SR(WS)      StoRe WS
#C5 =SRLL  // SR(LP+LIT)  StoRe LocalsPtr offset
#C6 =SRML  // SR(MP+LIT)  StoRe ModulePtr offset

// **********
// * Device Operations
#00 =D_read   // read from src, filling up tokenBuf
#01 =D_scan   // scan next word into tokenBuf[0:tokenLen]
#02 =D_dict   // FT=get SR=set dict key=tokenBuf
#03 =D_rdict  // FT=get reference to val  SR=forget including key
#04 =D_sz     // get/set current sz in bytes
#05 =D_comp   // compile (assemble) the token in tokenBuf
#06 =D_assert // error if != 0
#07 =D_wslen  // working stk len
#08 =D_cslen  // call stk len
// {-> err} D_xsCatch executes small function from WS but catches a panic.
// Note: caches and restores ep, call stack and local stack state and clears
// working stack (besides the returned err).
#09 =D_xsCatch

// **********
// * Memory Locations
#0000_0000 =null
#0000_0004 =heap
#0000_0008 =topHeap
#0000_000C =topMem
#0000_0010 =err
#0000_0014 =c_state
#0000_0018 =testIdx

// Dictionary Struct
#0000_001C =c_dictBuf
#0000_0020 =c_dictHeap
#0000_0022 =c_dictEnd
#0000_0024 =c_dictLHeap

// TokenBuf Struct
#0000_0028 =c_tokenBuf   // TokenBuf struct
#0000_002C =c_tokenLen

// **********
// * Global Compiler Variables
@heap .4^FT =c_rKey        #0 .4, // [U4] rKey, ref to current dict key.
@heap .4^FT =c_localOffset #0 .2, // [U2] Local Offset (for local var setup)

// **********
// * Global Constants
.4
#8000 =cmpEq  // Comparison was equal. Was less if LT this, vice-versa.
#0001_0000 =cAllowPanicMask

// 1-byte masks for fnMeta (upper byte of dict value)
#01 =IS_LARGE_FN // 1=has local stack (XL/XW instead of XSL/XSW)
#02 =IS_INSTANT // 1=is instant
#04 =IS_SUPER // 1=super always instant fn ($ may alter behavior)
#08 =IS_PRE // 1=is prefix

// masks for varMeta (same position as fnMeta)
#C0 =VAR_SZ_MASK // sz instr to use for var.

// The following are byte masks for meta (byte following dict value)
#07 =META_TY_MASK // # Lower three bits determine type
#00 =IS_CONST // constant value, should be compiled as a literal
#01 =IS_FN    // function, can be called and has an fnMeta
#02 =IS_LOCAL   // local variable, has varMeta. Accessed with FTLL/SRLL
#03 =IS_GLOBAL  // global variable, has varMeta. Accessed with FTML/SRML
#FF_FFFF =REF_MASK

// IS_FN meta
#08 =FN_LOCALS // fn has locals

// Error Classes
// [E000 - E100): built-in errors.
//  E100: device-specific hardware errors
// [E200-E800): reserved
//  E800+: application errors
//  AXXX_EXXX: test case assertion error.

#0     =E_ok      // no error
#E000  =E_general // general errors [E000-E010)
#E010  =E_io      // IO error class
#E0A0  =E_asm     // assembly error class (cause in asm).
#E0C0  =E_comp    // compiler error class (cause in comp).
#A000  =E_test    // [AXXX] (assert) test case error.

#E001  =E_intern  // internal (undefined) error
#E002  =E_undef   // undefined error
#E003  =E_unreach // unreachable code
#E004  =E_todo    // executed incomplete (to do) code
#E005  =E_wsEmpty // the WS was expected empty

#E0A1  =E_null    // null access
#E0A2  =E_oob     // out of bounds access
#E0A3  =E_stkUnd  // Stack underflow
#E0A4  =E_stkOvr  // Stack overflow
#E0A5  =E_align2  // access off 2byte allign
#E0A6  =E_align4  // access off 4byte align
#E0A7  =E_divZero // divide by zero

#E0C1  =E_cInstr  // invalid instr
#E0C2  =E_cToken  // token invalid
#E0C3  =E_cTLen   // token invalid
#E0C4  =E_cKey    // key already exists
#E0C5  =E_cNoKey  // dict key not found
#E0C6  =E_cHex    // non-hex number
#E0C7  =E_cSz     // invalid Sz selected
#E0C8  =E_cSzAPtr // invalid Sz for aptr
#E0C9  =E_cRet    // invalid RET
#E0CA  =E_cDblSr  // Double store
#E0CB  =E_cDevOp  // device op not impl
#E0CC  =E_DictOvr // dict overflow
#E0CD  =E_cXHasL  // small-execute to fn w/locals
#E0CE  =E_cXNoL   // large-execute to fn wo/locals
#E0CF  =E_cErr    // D_assert err code invalid

#E0D0  =E_cNotGlobal // using a non-global as global
#E0D1  =E_cIsX       // using an XS for an X
#E0D1  =E_cIsXS      // using an X for an XS
#E0D2  =E_cJmpL1     // JMP1 over too much space
#E0D2  =E_cNotFn
#E0D2  =E_cMod       // different modules
#E0D2  =E_cLSz       // literal sz

