// This file bootstraps spor from the native (i.e. C) implementation into
// a more full-featured language with helpful macros.
//
// # Table of Contents
// Search for these headings to find information
//
// 1. Instructions: contains definition of spore assembly instructions and
//    documentation.
//    a. Operations: Special
//    b. Operations: One Inp -> One Out
//    c. Operations: Two Inp -> One Out
//    d. Small Literal
//    e. Sizes
//    f. Jmp
//    g. Mem
// 2. Registers and Device Operations
// 3. Memory Locations, Globals and Constants
// 4. Errors

// **********
// * 1. Instructions
// Spor uses 8 bit instructions with the following bit layout:
//
// Note: (S=sz bit)
// 00XX XXXX: operation
// 01XX XXXX: small literal
// 10SS XXXX: jmp
// 11SS _XXX: mem

// # Operations: Special
#00 =NOP   // { -> }     no operation
#01 =RETZ  // Return if zero
#02 =RET   // Return
#03 =SWP   // {l r -> r l} swap
#04 =DRP   // {l -> }    drop
#05 =DRP2  // {l r -> }  drop 2
#06 =DUP   // {l -> l l} duplicate
#07 =DUPN  // {l -> l l==0} DUP then NOT
#08 =DVFT  // Device Operation Fetch
#09 =DVSR  // Device Operation Store
#0A =RGFT  // Register Fetch
#0B =RGSR  // Register Store

// # Operations: One Inp -> One Out
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

// # Operations: Two Inp -> One Out
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

// # Jmp
//
// Jumps can be to either a literal (L) or to an item on the working stack (W).
// - X means "execute" and is a call.
// - XS is an "execute small" and means that the function has no local stack.
// - XL/XW will execute a function that has a local stack. The size of the
//   local stack is stored in the first 16bits at the function's address, which
//   are loaded by the execute instr and stored in the highest byte in the
//   callstack (which RET uses to shrink the local stack on return).

//   Jmp      Description
#80 =JMPL  // Jmp to Literal
#81 =JMPW  // Jmp to WS
#82 =JZL   // Jmp to Literal if store==0
#83 =JTBL  // Jump to Table index using size=Literal
#84 =XL    // Execute Literal (mPtr)
#85 =XW    // Execute WS (aPtr)
#86 =XSL   // Execute Small Literal (no LS update)
#87 =XSW   // Execute Small WS (no LS update)

// JZL and JMPL for SZ=1
// For SZ=1 they jump to the 1 byte signed offset from the location
// of the LITERAL (not the location of the operation).

// # Mem      Store    Description
#C0 =LIT   // LIT         Literal
#C1 =FT    // FT(WS)      FeTch WS
#C2 =FTLL  // FT(LP+LIT)  FeTch LocalsPtr offset
#C3 =FTML  // FT(MP+LIT)  FeTch ModulePtr offset
#C4 =SR    // SR(WS)      StoRe WS
#C5 =SRLL  // SR(LP+LIT)  StoRe LocalsPtr offset
#C6 =SRML  // SR(MP+LIT)  StoRe ModulePtr offset

// Common instr+szs
@SZ1 @LIT  ^OR  =LIT1
@SZ2 @LIT  ^OR  =LIT2
@SZ4 @LIT  ^OR  =LIT4

@SZ1 @JZL  ^OR  =JZL1

@SZ2 @XSL  ^OR  =XSL2
@SZ2 @XL   ^OR  =XL2
@SZ2 @JMPL ^OR  =JMPL2

// **********
// * 2. Registers and Device Operations
//
// Registers and device operations can be accessed through RGXX and DVXX
// operations. RG operations include a 1 byte literal. The 1 byte literal has
// the following byte format:
// RROO OOOO: R=register O=offset
//
// FT will return the register value + offset
// SR will store the value + offset in the register

#00 =R_EP // execution pointer, SR will panic
#40 =R_LP // local stack pointer
#80 =R_CP // call stack pointer

// Device operations with DVFT and DVSR
#00 =D_read   // read from src, filling up tokenBuf
#01 =D_scan   // scan next word into tokenBuf[0:tokenLen]
#02 =D_dict   // [&buf &heap] FT=get SR=set dict key=tokenBuf
#03 =D_rdict  // [&buf &heap] FT=get reference to val  SR=forget including key
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
// * 3. Memory Locations, Globals and Constants
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

// Global Compiler Variables
@heap .4^FT =c_rKey        #0 .4, // [U4] rKey, ref to current dict key.
@heap .4^FT =c_rLKey       #0 .4, // [U4] rLKey, ref to current L dict key.
@heap .4^FT =c_localOffset #0 .2, // [U2] Local Offset (for local var setup)

// Constants
#8000 =cmpEq  // Comparison was equal. Was less if LT this, vice-versa.
#0001_0000 =cAllowPanicMask

// 1-byte masks for fnMeta (upper byte of dict value)
#01 =IS_LARGE_FN // 1=has local stack (XL/XW instead of XSL/XSW)
#02 =IS_INSTANT // 1=is instant
#04 =IS_SUPER // 1=super always instant fn ($ may alter behavior)
#08 =IS_PRE // 1=is prefix

// masks for varMeta (same position as fnMeta)
#C0 =VAR_SZ_MASK // sz instr to use for var.

// Meta types
#40 =KEY_HAS_TY // dict entry is a non-constant
#E0 =META_TY_MASK // # Upper three bits determine type
#20 =TY_FN    // function, can be called and has an fnMeta
#40 =TY_LOCAL   // local variable, has varMeta. Accessed with FTLL/SRLL
#60 =TY_GLOBAL  // global variable, has varMeta. Accessed with FTML/SRML
#FF_FFFF =REF_MASK
#FF_0000 =MOD_MASK

// FN meta bits [001L XXXX] L=locals
#10 =TY_FN_LOCALS

// **********
// * 4. Errors
//
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
#E0D0  =E_cKeyLen // Key len too large
#E0D1  =E_cReg    // Register error

#E0E0  =E_cNotGlobal // using a non-global as global
#E0E1  =E_cIsX       // using an XS for an X
#E0E2  =E_cIsXS      // using an X for an XS
#E0E3  =E_cJmpL1     // JMP1 over too much space
#E0E4  =E_cNotFn
#E0E5  =E_cMod       // different modules
#E0E6  =E_cLSz       // literal sz
#E0E7  =E_cNotType
#E0E9  =E_cNotLocal

// **********
// * Core Utility Macros
// These macros must be defined in pure ASM. They build on eachother
// to make the syntax much more readable.
//
// h1 [U1] -> []                : push 1 byte to heap
// h2 [U2] -> []                : push 2 byte to heap
// h4 [U4] -> []                : push 4 byte to heap
// L0 [U1] -> []                : compile a small literal [#0 - #3F]
// $dictSet <key> [U4] -> []    : set a dictionary key to value
// $dictGet <key> [] -> [U4]    : get dictionary key's value
// $loc <token> [] -> []        : set current heap location to <token>
// hpad [U4] -> []              : pad heap with NOPs
// hal2 [] -> []                : align heap for 2 byte literal
// hal4 [] -> []                : align heap for 4 byte literal
// ha2 [] -> []                 : align heap to 2 bytes
// ha4 [] -> []                 : align heap to 4 bytes
//
// Assertions: these panic with the supplied errCode if cond is not met.
// assert [cond errCode]
// assertNot [cond errCode]
//
// Test Assertions: these panic with E_test if the cond is not met.
// tAssert, tAssertNot, tAssertEq, tAssertNe
// Spore assembly constants.

@heap .4^FT =h1  // h1: {val:1} push 1bytes from stack to heap
  .1@heap @SLIT ^OR,  .4%FT // fetch heap {val, heap}
  .1%SR                        // store 1 byte value at heap
  // fetch heap and INC
                      .1@heap @SLIT ^OR,
  .4%FT               .4%INC // {heap+1}
  .1@heap @SLIT ^OR,  .4%SR  // heap=heap+1
  %RET // (unaligned)

@heap .4^FT =L0   // L0: compile a small literal (unchecked)
          .1%LIT
  #3F,      %AND        // truncated to bottom 6 bits
  .1%LIT    @SLIT,
  %OR       .2%JMPL @h1, // made into SLIT instr and stored. (aligned)

%NOP // (unaligned)
@heap .4^FT =h2  // h2: {val:2} push 2bytes from stack to heap
            @heap$L0
  .4%FT       .2%SR          // store 2 byte value at heap
  @heap$L0    .4%FT          // {heap}
  .4%INC2     %SRML .2@heap, // heap=heap+2
  %RET // (unaligned)

@heap .4^FT =h4  // h4: {val:4} push 4bytes from stack to heap
            @heap$L0
  %FT       .4%SR           // store 4 byte value at heap
  @heap$L0    %FT           // {heap}
  .4%INC4     %SRML .2@heap, // heap=heap+4
  %RET // (unaligned)

@heap .4^FT =_dict // setup for dict.
  // Scan next token
            @D_scan$L0
  %DVFT
  // put {dict.buf &dict.heap} on stack
            .4%FTML @c_dictBuf $h2
  %NOP      .2%LIT @c_dictHeap $h2
  %RET // (unaligned)

@heap .4^FT =dictSet // dictSet: Set "standard" dictionary to next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVSR // device dict SR
  %RET // (unaligned)

@heap .4^FT =dictGet // dictGet: Get the value of the next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVFT
  %RET // (unaligned)

@heap .4^FT =dictGetR // dictGetR: Get the &metaRef of the next token.
            .2%XSL @_dict $h2
  @D_rdict$L0   %DVFT
  %RET // (unaligned)

@heap .4^FT =loc // $loc <name>: define a location
            @heap$L0
  %FT       .2%XSL @dictSet $h2
  %RET // (unaligned)

$loc getHeap     %FTML @heap $h2      %RET // (unaligned)
$loc setHeap     %SRML @heap $h2      %RET // (unaligned)
$loc getTopHeap  %FTML @topHeap $h2   %RET // (unaligned)
$loc setTopHeap  %SRML @topHeap $h2   %RET // (unaligned)

%NOP // (aligned)
$loc hpad // {pad} write pad bytes to heap.
  // WHILE(pad) we write NOP to heap
  @heap ^FT // c-stk{loopStart}
    .4%DUP        .2%JZL // if(pad == 0) breakTo
      .4@heap ^FT ^SWP #0 $h2 // c-stk{breakTo loopStart}
    @NOP$L0       .2%XSL @h1 $h2 // write a noop
    .4%DEC        .2%JMPL    $h2 // DEC and jmp to loopStart
  .4@heap ^FT ^SWP .2^SR // update breakTo spot
  %DRP           %RET // (aligned)

$loc _hal // {align} heap align (for) literal
  .4%DUP        .2%XSL @getHeap $h2 // {align align heap}
  .4%SWP        .4%MOD // {align heap%align}
  // pad = (align-1) - heap%align
  .4%SWP          %DEC // {heap%align align-1}
  .4%SWP          %SUB
  .4%NOP        .2%JMPL @hpad $h2

// halN: heap align (for) N-byte literal.
$loc hal2   #2$L0          .2%JMPL @_hal $h2 // (aligned)
$loc hal4   #4$L0          .2%JMPL @_hal $h2 // (aligned)


// Assert checks a condition or panics with an error
// ex: <some check> @E_myError assert
$hal2 $loc assertNot // {failIfTrue errCode}
                  %SWP
  %NOT            %SWP // fallthrough (aligned)
$loc assert    // {failIfFalse errCode}
  @D_assert$L0     %DVFT
  %RET // (unaligned)

$hal2 $loc tAssert
        .2%LIT @E_test $h2
  $hal2 %JMPL @assert $h2

$loc tAssertNot     .4%NOT $hal2 .2%JMPL @tAssert,
$loc tAssertEq      .4%EQ  $hal2 .2%JMPL @tAssert,
$loc tAssertNe      .4%NEQ $hal2 .2%JMPL @tAssert,

$hal2 $loc _ha // {align} heap align (with NOPs)
                .2%XSL @getHeap $h2 // {align heap}
  .4%SWP        .4%MOD // {heap%align}
  $hal2 .2%JMPL @hpad $h2

// haN: {align} heap align N byte.
#2 $_ha
$loc ha2   #2$L0   .2%JMPL @_ha $h2
$loc ha4   #4$L0   .2%JMPL @_ha $h2

// **********
// * Jmp and Literal Macros
// These are used for compiling 2 byte jmps and 1-4 byte literals. As you can
// imagine, these are essential operations!
//
// $xsl <token>                 : compile a execute to small function
// $jmpl <token>                : compile a jump to small function
// L1 / L2 / L4  [U] -> []      : compile a 1 / 2 / 4 byte literal.

$hal4 $loc toRef // {metaRef} -> {ref}
  .4%LIT @REF_MASK $h4 %AND %RET

$hal2 $loc _j2 // {metaRef instr} compile jmpInstr to 2 byte metaRef
  $hal2 .2%XSL @hal2 $h2    // enforce proper alignment
  $hal2 .2%XSL @h1 $h2      // compile instr {metaRef}
  $hal2 .2%XSL @toRef $h2   // {ref}
  $hal2 .2%JMPL @h2 $h2     // compile addr

$hal2 $loc _xsl // $_xsl <token> : compile unchecked xsl
  $hal2 .2%XSL @dictGet $h2 // {metaRef}
  .1%LIT @XSL2 $h1  // push .2%XSL instr
  $hal2 .2%JMPL @_j2 $h2

$hal2 $loc _jmpl // $_jmpl <token>: compile unchecked jmpl
  $_xsl dictGet             // {metaRef}
  .1%LIT @JMPL2 $h1 // push .2%JMPL instr
  $hal2 .2%JMPL @_j2 $h2


$hal2 $loc L1 // {U1} compile 1 byte literal
  .1%LIT @LIT1 $h1 // push .1%LIT instr
  $_xsl h1 // compile it
  $_jmpl h1

$hal2 $loc c1 // @INSTR $c1: compile "compile INSTR"
  $_xsl L1             // compile the INSTR literal itself
  // compile xsl to h1
  $hal2 .2%LIT @h1 $h2
  $hal2 .2%LIT @XSL2 $h2
  $_jmpl _j2

$hal2 $loc L2 // {U1} compile 2 byte literal
  $_xsl hal2 // enforce proper alignment
  @LIT2 $c1  // compile .2%LIT instr
  $_jmpl h2  // compile the 2 byte literal

$hal2 $loc L4 // {U1} compile 4 byte literal
  $_xsl hal4 // enforce proper alignment
  @LIT4 $c1  // compile .4%LIT
  $_jmpl h4  // compile the 4 byte literal

$loc toMeta // INLINE {metaRef} -> {meta}
  @SLIT #18 ^OR $c1  @SHR $c1  %RET

$loc keyHasTy  // {&metaRef} dict value is a constant
  %INC4 .1%FT @KEY_HAS_TY$L1 %AND %RET

// These take {metaRef} and tell information about it

$loc isFn         $toMeta  @META_TY_MASK$L1 %AND  @TY_FN$L1      %EQ %RET
$loc fnHasLocals  $toMeta  @TY_FN_LOCALS$L1 %AND %RET

$loc assertHasTy // [&metaRef]
  $_xsl keyHasTy @E_cNotType $L2 $_jmpl assert
$loc assertFn   $_xsl isFn  @E_cNotFn $L2  $_jmpl assert // [metaRef] -> []

$loc assertXs // [metaRef]
  %DUP $_xsl assertFn
  $_xsl fnHasLocals  @E_cIsX $L2  $_jmpl assertNot

$loc assertX // {metaRef}
  %DUP $_xsl assertFn
  $_xsl fnHasLocals  @E_cIsX $L2  $_jmpl assert

$hal4 $loc toMod @MOD_MASK $L4 %AND %RET // {ref} -> {mod}
$loc isSameMod // {metaRef metaRef} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET

$hal2 $loc curMod   .2%FTML @c_rKey  $h2 .4%FT  $_jmpl toMod // [] -> [mod]
$hal2 $loc isCurMod $_xsl toMod  $_xsl curMod %EQ %RET        // [ref] -> [isCurMod]

$hal2 $loc assertCurMod  $_xsl isCurMod  @E_cMod$L2  $_jmpl assert

$loc _jSetup // [&metaRef] -> [metaRef]: checked jmp setup
  %DUP $_xsl assertHasTy
  .4%FT // {metaRef}
  %DUP $_xsl assertXs
  %DUP $_jmpl assertCurMod

$loc xsl // $xsl <token> : compile .2%xsl
  $_xsl dictGetR $_xsl _jSetup // {metaRef}
  .1%LIT @XSL2 $h1  // get .2%XSL instr
  $_jmpl _j2

$loc xl // $xl <token> : compile .2%xl
  $_xsl dictGet // {metaRef}
  %DUP $_xsl assertX
  %DUP $_xsl assertCurMod
  @XL2$L1  $_jmpl _j2

$loc jmpl // $jmpl <token> : compile jmpl2
  $_xsl dictGetR $_xsl _jSetup // {metaRef}
  @JMPL2$L1  $_jmpl _j2


$hal2 $loc c_updateRKey // [] -> [&metaRef] update and return current key
        .4%FTML @c_dictBuf $h2  // dict.buf
  $hal2 .2%FTML @c_dictHeap $h2 // dict.heap
  .4%ADD // {&newKey}
  %DUP $hal2 .4%SRML @c_rKey $h2 // rKey=newKey
  %RET // return &metaRef (newKey)

$loc metaSet // {U4 mask:U1} -> U4 : apply meta mask to U4 value
  #18 $L0  %SHL  // make mask be upper byte
  %OR %RET

$loc c_keySetTyped // {&metaRef} -> []
  %INC4 %DUP // {&len &len}
  .1%FT @KEY_HAS_TY$L1 %OR // {&len tyKeyLen}
  %SWP .1%SR %RET            // update tyKeyLen

// Note: this uses locals
$loc c_keySetTy // {mask:U1 &metaRef} mask current key's meta to be a Ty
  #1 $h1 // 1 local [&metaRef]
  %DUP .4%SRLL #0$h1 // cache &metaRef {mask &metaRef}
  %DUP $_xsl c_keySetTyped // make key "typed" {mask &metaRef}
  .4%FT // fetch key {mask metaRef}
  %SWP $_xsl metaSet // apply mask {metaRef}
  .4%FTLL#0$h1  .4%SR // update key
  %RET

$hal2 $loc _declFn // [meta]
  @TY_FN$L1 %OR // {meta}
  $_xsl c_updateRKey // {meta &metaRef}
  $_xsl loc
  $hal2 .2%XL @c_keySetTy $h2
  $ha2
  // clear locals by setting localDict.heap=dict.heap (start of localDict.buf)
  #0$L0        .2%SRML @c_localOffset $h2  // zero localDict.offset
  #0$L0        .4%SRML @c_dictLHeap $h2    // zero localDict.heap
  %RET

$ha2 $loc SFN  // $SFN <token>: define location of small function
  #0$L0         $_jmpl _declFn

$hal2 $SFN FN  // $FN <token>: define location of function with locals
  @TY_FN_LOCALS$L1 $_jmpl _declFn

// Backfill the fn meta
$SFN c_makeTy // {meta mask} make an existing symbol a type.
  $_xsl dictGetR   // {meta &metaRef}
  $hal2 .2%XL @c_keySetTy $h2
  %RET

$SFN c_makeFn // {meta} <token>: set meta for token to be a small function.
  @TY_FN$L1 %OR  $_jmpl c_makeTy

#0 $c_makeFn SFN
#0 $c_makeFn xsl
#0 $c_makeFn jmpl
#0 $c_makeFn L0
#0 $c_makeFn L1
#0 $c_makeFn L2
#0 $c_makeFn L4

#0 $c_makeFn loc
#0 $c_makeFn h1
#0 $c_makeFn h2
#0 $c_makeFn h4
#0 $c_makeFn dictSet
#0 $c_makeFn dictGet
#0 $c_makeFn dictGetR
#0 $c_makeFn getHeap
#0 $c_makeFn setHeap
#0 $c_makeFn hpad
#0 $c_makeFn hal2
#0 $c_makeFn hal4
#0 $c_makeFn ha2
#0 $c_makeFn ha4
#0 $c_makeFn metaSet
#0 $c_makeFn toRef
#0 $c_makeFn toMod
#0 $c_makeFn toMeta
#0 $c_makeFn curMod
#0 $c_makeFn keyHasTy
#0 $c_makeFn isFn
#0 $c_makeFn isSameMod
#0 $c_makeFn isCurMod
#0 $c_makeFn fnHasLocals
#0 $c_makeFn c_updateRKey
#0 $c_makeFn c_keySetTyped
@TY_FN_LOCALS $c_makeFn c_keySetTy

#0 $c_makeFn assert
#0 $c_makeFn assertNot
#0 $c_makeFn tAssert
#0 $c_makeFn tAssertNot
#0 $c_makeFn tAssertEq
#0 $c_makeFn tAssertNe
#0 $c_makeFn assertFn
#0 $c_makeFn assertXs
#0 $c_makeFn assertCurMod
#0 $c_makeFn assertHasTy

$SFN getSz           @D_sz$L0          %DVFT %RET
$SFN setSz           @D_sz$L0          %DVSR %RET
$SFN getWsLen        @D_wslen$L0       %DVFT %RET
$SFN c_xsCatch       @D_xsCatch$L0     %DVFT %RET
$SFN c_scan          @D_scan$L0        %DVFT %RET
$SFN panic   #0 $L0 %SWP  $jmpl assert // {errCode}: panic with errCode
$SFN assertWsEmpty   $xsl getWsLen  @E_wsEmpty $L2  $jmpl assertNot
$assertWsEmpty


// **********
// * ASM Flow Control
// - `$IF ... $END` for if blocks
// - `$LOOP ... $BREAK0 ... $AGAIN $END` for infiinte loops with breaks.
//
// All flow control pushes the current heap on the WS, then END/AGAIN correctly
// stores/jmps the heap where they are called from.

$SFN IF // {} -> {&jmpTo} : start an if block
  @JZL1 $L1  $xsl h1 // compile .1%JZL instr
  $xsl getHeap // {&jmpTo} push &jmpTo location to stack
  #0$L0               $xsl h1 // compile 0 (jump pad)
  %RET

$SFN assertLt128  #10 $L0 %LT_U   @E_cJmpL1 $L2   $jmpl assert

$SFN END // {&jmpTo} -> {} : end of IF or BREAK0
  %DUP          // {&jmpTo &jmpTo}
  $xsl getHeap  // {&jmpTo &jmpTo heap}
  %SWP %SUB     // {&jmpTo (heap-&jmpTo)}
  %DUP $xsl assertLt128
  %SWP .1%SR %RET // store at location after start (1 byte literal)

// $LOOP ... $BREAK0 ... $AGAIN $END
$SFN LOOP   $jmpl getHeap   // push location for AGAIN
$SFN BREAK0 $xsl IF  %SWP %RET // {&loopTo} -> {&breakTo &loopTo}
$SFN AGAIN // {&loopTo} -> {} : run loop again
  @JMPL $c1  // compile jmp
  $xsl getHeap  // {&loopTo heap}
  %SWP %SUB     // {heap-&loopTo}
  %DUP $xsl assertLt128
  %NEG          // make negative for backwards jmp
  $jmpl h1      // compile as jmp offset

// **********
// * Defining and Using global variables

$SFN isGlobal     $toMeta  @META_TY_MASK$L1 %AND  @TY_GLOBAL$L1  %EQ %RET
$SFN assertGlobal $xsl isGlobal  @E_cNotGlobal$L2 $jmpl assert

$SFN assertSzI // {szI}
  %DUP #CF$L1 %AND @E_cSz$L2 $xsl assertNot // non-sz bits empty
  #6$L0 %SHR #3$L1 %LT_U @E_cSz$L2 $jmpl assert // sz bits < 3

$SFN szToSzI // [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L0 %EQ $IF  %DRP @SZ1 $L1 %RET  $END
  %DUP #2$L0 %EQ $IF  %DRP @SZ2 $L1 %RET  $END
       #4$L0 %EQ $IF  %DRP @SZ4 $L1 %RET  $END
  @E_cSz$L2 $xsl panic


$SFN szILowerToSzI // convert a szI in lower bits to a proper szI
  #3$L0 %AND #4$L0 %SHL %RET


$SFN szIToSz // {szI} -> {sz}
  %DUP @SZ1$L1 %EQ $IF  %DRP #1$L0 %RET  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP #2$L0 %RET  $END
       @SZ4$L1 %EQ $IF       #4$L0 %RET  $END
  @E_cSz$L2 $xsl panic

$SFN haN  // {szI} align heap
  $xsl szIToSz $_jmpl _ha
$SFN halN // {szI} align heap for literal to szI
  $xsl szIToSz $_jmpl _hal

$SFN hN // {value szI} write a value of szI to heap
  %DUP @SZ1$L1 %EQ $IF  %DRP $jmpl h1  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP $jmpl h2  $END
  %DUP @SZ4$L1 %EQ $IF  %DRP $jmpl h4  $END
  @E_cSz$L2 $xsl panic

$SFN joinSzTyMask #4$L0 %SHR %OR %RET // {tyMask szI} -> {tyMask}

$FN c_global // <value> <szI> $c_global <token>: define a global variable of sz
  #1 $h1 // 1 local [szI:U1]
  %DUP $xsl assertSzI // {value szI}
  %DUP $xsl haN // align heap {value szI}
  .1%SRLL#0$h1 // cache sz {value}
  $xsl c_updateRKey // {value &metaRef}
  $xsl loc
  @TY_GLOBAL$L1  .1%FTLL#0$h1   $xsl joinSzTyMask // {value &metaRef mask}
  %SWP $xl c_keySetTy // update key ty
  .1%FTLL#0$h1  $jmpl hN // write value to heap

$SFN _gSetup // {&metaRef} -> {metaRef} : checked global setup
  %DUP $_xsl assertHasTy
  .4%FT %DUP $_jmpl assertGlobal

$SFN gRef // gRef [] -> [&global] : get global reference
  $_xsl dictGetR  $xsl _gSetup // {metaRef}
  $xsl toRef // {&global}
  $jmpl L4

// {metaRO szLit instr} compile a literal memory instr.
//   szLit the size of the literal to compile for the instr.
//   metaRO: either a reference or an offset with a conforming meta attached to
//     it (meta at upper byte, szI in lowest 2 bits).
$FN _memLitImpl
  #1 $h1 // 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 // store instr {metaR0 szLit}
  %DUP $xsl halN // align for literal {metaR0 szLit}
  .1%SRLL #0$h1 // store szLit {metaR0}
  %DUP $toMeta $xsl szILowerToSzI // {metaRO szI}
  .1%FTLL #1$h1 %OR $xsl h1 // compile (szI | instr) {metaRO}
  .1%FTLL #0$h1  $jmpl hN // compile literal of proper instrSz

$SFN gGet // gGet [] -> [global] : get global value
  $_xsl dictGetR  $xsl _gSetup // {metaRef}
  %DUP $xsl assertCurMod
  @SZ2$L1  @FTML$L1  $xl _memLitImpl %RET

$SFN gSet // gSet [value] -> [] : set global value
  $_xsl dictGetR  $xsl _gSetup // {metaRef}
  %DUP $xsl assertCurMod
  @SZ2$L1  @SRML$L1  $xl _memLitImpl %RET

$SFN c_makeGlobal // {szI} <token>: set meta for token to be a global.
  #4$L0 %SHR  @TY_GLOBAL$L1  %OR  $_jmpl c_makeTy

@SZA $c_makeGlobal heap
@SZA $c_makeGlobal topHeap
@SZA $c_makeGlobal topMem
@SZ4 $c_makeGlobal err
@SZ4 $c_makeGlobal c_state
@SZ4 $c_makeGlobal testIdx
@SZA $c_makeGlobal c_dictBuf
@SZ2 $c_makeGlobal c_dictHeap
@SZ2 $c_makeGlobal c_dictEnd
@SZ2 $c_makeGlobal c_dictLHeap
@SZA $c_makeGlobal c_tokenBuf
@SZ1 $c_makeGlobal c_tokenLen

@SZ4 $c_makeGlobal c_rKey
@SZ4 $c_makeGlobal c_rLKey
@SZ2 $c_makeGlobal c_localOffset

// **********
// * Local Variables
// Defining and using local variables.
//
// #2 $c_local myLocal : declares a local variable of sz=2
// $c_localEnd // Reserves correct amount of space in fn. Required for all fns with locals.
// $lSet myLocal
// $lGet myLocal

$SFN isLocal     $toMeta  @META_TY_MASK$L1 %AND  @TY_LOCAL$L1  %EQ %RET
$SFN assertLocal $xsl isLocal  @E_cNotLocal$L2 $jmpl assert

$FN min // [a b] -> [min]
  #1 $h1 // one local, b
  .4%SRLL #0 $h1
  %DUP // {a a}
  .4%FTLL #0 $h1 // {a a b}
  %GE_U %RETZ               // if(!(a >= b)) ret a
  %DRP .4%FTLL #0 $h1 %RET  // ret b

$SFN _ldict
  $xsl c_scan
  $gGet c_dictBuf   $gGet c_dictHeap  %ADD // ldict.buf
  $gRef c_dictLHeap                        // &ldict.heap
  %RET

$SFN ldictGet   $xsl _ldict @D_dict$L0  %DVFT %RET
$SFN ldictSet   $xsl _ldict @D_dict$L0  %DVSR %RET
$SFN ldictGetR  $xsl _ldict @D_rdict$L0 %DVFT %RET

$SFN c_updateRLKey // [] -> [&metaRef] update and return current local key
  $gGet c_dictBuf   $gGet c_dictHeap  %ADD // ldict.buf
  $gGet c_dictLHeap                        // ldict.heap
  %ADD // {&newLkey}
  %DUP $gSet c_rLKey  %RET


$FN align // {aptr sz}: return the aptr aligned properly with szI
  #1 $h1 // locals [sz:U1]
  .1%SRLL#0$h1 // cache sz
  %DUP // {aptr aptr}
  .1%FTLL#0$h1 %MOD // {aptr aptr%sz}
  %DUP $IF
    .1%FTLL#0$h1 %SWP %SUB // sz - aptr%sz
    %ADD %RET // aptr + (sz - aptr%sz)
  $END
  %DRP %RET

$SFN alignSzI $xsl szIToSz  $xl align  %RET

// <szI> $c_local myLocal: declare a local variable of sz
// This stores the offset and sz for lRef, lGet and lSet to use.
$FN c_local
  #1 $h1 // locals [szI:U1]
  %DUP  $xsl assertSzI
  %DUP  .1%SRLL#0$h1 // cache szI
  @TY_LOCAL$L1  %SWP  $xsl joinSzTyMask  // {mask}
  $gGet c_localOffset // {mask loff}
  .1%FTLL#0$h1  $xsl alignSzI // align local offset {mask loff}
  // c_localOffset = offset + sz
  %DUP  .1%FTLL#0$h1 $xsl szIToSz %ADD $gSet c_localOffset
  $xsl c_updateRLKey // {mask loff &metaRef}
  %SWP $xsl ldictSet  // set to localOffset {mask &metaRef}
  $xl c_keySetTy %RET

$SFN c_localEnd // end local declarations and write the number of slots needed.
  $gGet c_localOffset #4$L0 $xl align
  #2$L0 %SHR $jmpl h1

$SFN _lSetup // {&metaO} -> {metaO} : checked local setup
  %DUP $_xsl assertHasTy
  .4%FT %DUP $_jmpl assertLocal // returns metaOffset

$SFN lRef // lRef [] -> [&local] : get local reference
  $_xsl ldictGetR  $xsl _lSetup // {metaLocalOffset}
  $xsl toRef // {localOffset}
  %RGFT @R_LP$h1  %ADD %RET

$SFN lGet // lGet [] -> [local] : get local value
  $_xsl ldictGetR  $xsl _lSetup // {metaO}
  @SZ1$L1  @FTLL$L1  $xl _memLitImpl %RET

$SFN lSet // lSet [value] -> [] : set local value
  $_xsl ldictGetR  $xsl _lSetup // {metaO}
  @SZ1$L1  @SRLL$L1  $xl _memLitImpl %RET
