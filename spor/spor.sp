// This file bootstraps spor from the native (i.e. C) implementation into
// a more full-featured language with helpful macros.
//
// Note: this file requires the compiler to pass the current heap value on the
// stack so that it can define it's first function (getHeap), which it then
// uses to define the other functions.
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
#05 =OVR   // {l r -> }  drop 2
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
#C0 =LIT   // {} -> {literal}     Literal (U1, U2 or U4)
#C1 =FT    // {addr} -> {value}   FeTch value from addr
#C2 =FTLL  // {} -> {local}       FeTch from LP + U2 literal offset
#C3 =FTGL  // {} -> {global}      FeTch from GB + U2 literal offset
#C4 =SR    // {addr value} -> {}  Store value at addr
#C5 =SRLL  // {value} -> {}       StoRe value at LP + U2 literal offset
#C6 =SRGL  // {value} -> {}       StoRe value at GB + U2 literal offset

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

#00 =R_MP // global base pointer
#01 =R_EP // execution pointer, SR will panic
#02 =R_LP // local stack pointer
#03 =R_CP // call stack pointer
#04 =R_GB // global base pointer

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
#0A =D_memMove // {dst src len} "dst = src [len]" move bytes safely.
#0B =D_memCmp  // {&a &b len} -> I32: <0 if a<b; >0 if a>b; 0 if a==b

// **********
// * 3. Memory Locations, Globals and Constants
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
#0000_0030 =c_rKey         // [U4] rKey, ref to current dict key.
#0000_0034 =c_rLKey        // [U4] rLKey, ref to current L dict key.
#0000_0030 =c_compFn       // [U4] compiler function (single token)
#0000_0038 =c_localOffset  // [U2] Local Offset (for local var setup)

// Constants
#0 =NULL
#0 =FALSE
#1 =TRUE
#0001_0000 =cAllowPanicMask

// Meta types
#40 =KEY_HAS_TY // dict entry is a non-constant
#E0 =META_TY_MASK // # Upper three bits determine type
#20 =TY_FN    // function, can be called and has an fnMeta
#40 =TY_LOCAL   // local variable, has varMeta. Accessed with FTLL/SRLL
#60 =TY_GLOBAL  // global variable, has varMeta. Accessed with FTGL/SRGL
#FF_FFFF =REF_MASK
#FF_0000 =MOD_MASK

// FN meta bits [TTTL TT-P] L=locals T=fnTy P=pre
#0C =TY_FN_TY_MASK // 0b1100
#01 =TY_FN_PRE     // always run immediately and passed compile state.
#00 =TY_FN_NORMAL  // normally compiled and run at runtime
#04 =TY_FN_INSTANT // normally run immediately (compile time)
#08 =TY_FN_SMART   // always run immediately and passed compile state.
#10 =TY_FN_LOCALS  // has locals (called with XL or XW)

// Local meta bits [TTTI --SS] I=input S=szI
#10 =TY_LOCAL_INPUT

// Compiler State
#8000 =C_COMPILE     // default compile (not execute) tokens.
#4000 =C_INSTANT     // "$" sets this. Toggles INSTANT for next.


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
#E0C8  =E_cSzPtr  // invalid Sz for aptr
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
#E0E5  =E_cNotFnLocals
#E0E6  =E_cMod       // different modules
#E0E7  =E_cLSz       // literal sz
#E0E9  =E_cNotType
#E0EA  =E_cNotLocal
#E0EB  =E_cNotVar
#E0EC  =E_cNotFnOrConst

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

%NOP // (unaligned) Note: heap is still on the stack!
^INC =getHeap
                .4%FTGL @heap.2,
    %RET // (unaligned)

$getHeap =h1  // h1: {val:1} push 1bytes from stack to heap
                      .4%FTGL @heap.2, // fetch heap {val, heap}
  %SWP                .1%SR            // store 1 byte value at heap
  %NOP                .4%FTGL @heap.2, // fetch heap {val, heap}
  %INC                .4%SRGL @heap.2, // heap=heap+1
  %RET // (unaligned)

$getHeap =L0   // L0: compile a small literal (unchecked)
          .1%LIT
  #3F,      %AND        // truncated to bottom 6 bits
  .1%LIT    @SLIT,
  %OR       .2%JMPL .2@h1, // made into SLIT instr and stored. (aligned)

%NOP // (unaligned)
$getHeap =h2  // h2: {val:2} push 2bytes from stack to heap
              .4%FTGL @heap.2, // fetch heap {val, heap}
  %SWP        .2%SR            // store 2 byte value at heap
  %NOP        .4%FTGL @heap.2, // {heap}
  .4%INC2     %SRGL   @heap.2,   // heap=heap+2
  %RET // (unaligned)

$getHeap =h4  // h4: {val:4} push 4bytes from stack to heap
            .4%FTGL @heap.2, // fetch heap {val, heap}
  %SWP      .4%SR           // store 4 byte value at heap
  %NOP      .4%FTGL @heap.2, // {heap}
  %INC4     .4%SRGL @heap.2, // heap=heap+4
  %RET // (unaligned)

$getHeap =dictArgs // args for dict.
  // put {dict.buf &dict.heap} on stack
            .4%FTGL @c_dictBuf $h2
  %NOP      .2%LIT @c_dictHeap $h2
  %RET // (unaligned)

$getHeap =_dict // setup for dict.
  // Scan next token
            @D_scan$L0
  %DVFT     .2%JMPL @dictArgs$h2 // (aligned)

%NOP // (unaligned)
$getHeap =dictSet // dictSet: Set "standard" dictionary to next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVSR // device dict SR
  %RET // (unaligned)

$getHeap =dictGet // dictGet: Get the value of the next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVFT
  %RET // (unaligned)

$getHeap =dictGetR // dictGetR: Get the &metaRef of the next token.
            .2%XSL @_dict $h2
  @D_rdict$L0   %DVFT
  %RET // (unaligned)

$getHeap =loc // $loc <name>: define a location
            @heap$L0
  %FT       .2%XSL @dictSet $h2
  %RET // (unaligned)

$loc setHeap     %SRGL @heap $h2      %RET // (unaligned)
$loc getTopHeap  %FTGL @topHeap $h2   %RET // (unaligned)
$loc setTopHeap  %SRGL @topHeap $h2   %RET // (unaligned)

%NOP // (aligned)
$loc hpad // {pad} write pad bytes to heap.
  // WHILE(pad) we write NOP to heap
  @heap ^FT // c-stk{loopStart}
    .4%DUP        .2%JZL // if(pad == 0) breakTo
      .4@heap ^FT ^SWP #0 $h2 // c-stk{breakTo loopStart}
    @NOP$L0       .2%XSL @h1 $h2 // write a noop
    .4%DEC        .2%JMPL    $h2 // DEC and jmp to loopStart
  .4@heap ^FT .2^SR // update breakTo spot
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

$loc toMeta // INSTANT {metaRef} -> {meta}
  @SLIT #18 ^OR $c1  @SHR $c1  %RET

$loc isTyped  // {&metaRef} dict value is a constant
  %INC4 .1%FT @KEY_HAS_TY$L1 %AND %RET

// These take {metaRef} and tell information about it

$loc isTyFn      $toMeta  @META_TY_MASK$L1 %AND  @TY_FN$L1  %EQ %RET
$loc isFnLocals  $toMeta  @TY_FN_LOCALS$L1 %AND %RET

$loc assertTyped // [&metaRef]
  $_xsl isTyped @E_cNotType $L2 $_jmpl assert
$loc assertFn   $_xsl isTyFn  @E_cNotFn $L2  $_jmpl assert // [metaRef] -> []

$loc assertFnSmall // [metaRef]
  %DUP $_xsl assertFn
  $_xsl isFnLocals  @E_cIsX $L2  $_jmpl assertNot

$loc assertFnLocals // [metaRef]
  %DUP $_xsl assertFn
  $_xsl isFnLocals  @E_cIsX $L2  $_jmpl assert

$hal4 $loc toMod @MOD_MASK $L4 %AND %RET // {ref} -> {mod}
$loc isSameMod // {metaRef metaRef} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET

$hal2 $loc curMod   .2%FTGL @c_rKey$h2 .4%FT  $_jmpl toMod // [] -> [mod]
$hal2 $loc isCurMod $_xsl toMod  $_xsl curMod %EQ %RET        // [ref] -> [isCurMod]

$hal2 $loc assertCurMod  $_xsl isCurMod  @E_cMod$L2  $_jmpl assert

$loc _jSetup // [&metaRef] -> [metaRef]: checked jmp setup
  %DUP $_xsl assertTyped
  .4%FT // {metaRef}
  %DUP $_xsl assertFnSmall
  %DUP $_jmpl assertCurMod

$loc xsl // $xsl <token> : compile .2%xsl
  $_xsl dictGetR $_xsl _jSetup // {metaRef}
  .1%LIT @XSL2 $h1  // get .2%XSL instr
  $_jmpl _j2

$loc xl // $xl <token> : compile .2%xl
  $_xsl dictGet // {metaRef}
  %DUP $_xsl assertFnLocals
  %DUP $_xsl assertCurMod
  @XL2$L1  $_jmpl _j2

$loc jmpl // $jmpl <token> : compile jmpl2
  $_xsl dictGetR $_xsl _jSetup // {metaRef}
  @JMPL2$L1  $_jmpl _j2


$hal2 $loc c_updateRKey // [] -> [&metaRef] update and return current key
        .4%FTGL @c_dictBuf $h2  // dict.buf
  $hal2 .2%FTGL @c_dictHeap $h2 // dict.heap
  .4%ADD // {&newKey}
  %DUP $hal2 .4%SRGL @c_rKey $h2 // rKey=newKey
  %RET // return &metaRef (newKey)

$loc metaSet // {metaRef meta:U1} -> U4 : apply meta to metaRef
  #18 $L0  %SHL  // make meta be upper byte
  %OR %RET

$loc rMetaSet // {&metaRef meta:U1} -> U4 : apply meta to &metaRef
  %OVR .4%FT %SWP // {&metaRef metaRef meta}
  $_xsl metaSet   // {&metaRef newMetaRef}
  .4%SR %RET

$loc c_keySetTyped // {&metaRef} -> []
  %INC4 %DUP // {&len &len}
  .1%FT @KEY_HAS_TY$L1 %OR // {&len tyKeyLen}
  .1%SR %RET            // update tyKeyLen

$loc c_keySetTy // {meta:U1 &metaRef} -> {} meta current key's meta to be a Ty
  %DUP $_xsl c_keySetTyped // make key "typed" {meta &metaRef}
  %SWP $_xsl rMetaSet
  %RET

$hal2 $loc _declFn // [meta]
  @TY_FN$L1 %OR // {meta}
  $_xsl c_updateRKey // {meta &metaRef}
  $_xsl loc
  $_xsl c_keySetTy
  $ha2
  // clear locals by setting localDict.heap=dict.heap (start of localDict.buf)
  #0$L0        .2%SRGL @c_localOffset $h2  // zero localDict.offset
  #0$L0        .4%SRGL @c_dictLHeap $h2    // zero localDict.heap
  %RET

$ha2 $loc SFN  // $SFN <token>: define location of small function
  #0$L0         $_jmpl _declFn

$hal2 $loc FN  // $FN <token>: define location of function with locals
  @TY_FN_LOCALS$L1 $_jmpl _declFn

$hal2
$loc INSTANT // {} modify current function to be instant
  .4%FTGL @c_rKey$h2  @TY_FN_INSTANT$L1   $_jmpl rMetaSet

// Backfill the fn meta
$SFN c_makeTy // {meta} make an existing symbol a type.
  $_xsl dictGetR   // {meta &metaRef}
  $_xsl c_keySetTy
  %RET

$SFN c_makeFn // {meta} <token>: set meta for token to be a small function.
  @TY_FN$L1 %OR  $_jmpl c_makeTy

$hal2 $SFN PRE .4%FTGL @c_rKey$h2  @TY_FN_PRE$L1   $_jmpl rMetaSet

@TY_FN_INSTANT        $c_makeFn SFN
@TY_FN_INSTANT        $c_makeFn FN
@TY_FN_INSTANT        $c_makeFn INSTANT
#0                    $c_makeFn xsl
#0                    $c_makeFn jmpl
@TY_FN_PRE            $c_makeFn L0
@TY_FN_PRE            $c_makeFn L1
@TY_FN_PRE            $c_makeFn L2
@TY_FN_PRE            $c_makeFn L4

#0                    $c_makeFn loc
@TY_FN_PRE            $c_makeFn h1
@TY_FN_PRE            $c_makeFn h2
@TY_FN_PRE            $c_makeFn h4
@TY_FN_PRE @TY_FN_INSTANT ^OR $c_makeFn c1
@TY_FN_PRE            $c_makeFn dictSet
#0                    $c_makeFn dictGet
#0                    $c_makeFn dictGetR
#0                    $c_makeFn dictArgs
#0                    $c_makeFn getHeap
@TY_FN_PRE            $c_makeFn setHeap
@TY_FN_PRE            $c_makeFn hpad
#0                    $c_makeFn hal2
#0                    $c_makeFn hal4
#0                    $c_makeFn ha2
#0                    $c_makeFn ha4
@TY_FN_PRE            $c_makeFn metaSet
@TY_FN_PRE            $c_makeFn toRef
@TY_FN_PRE            $c_makeFn toMod
@TY_FN_INSTANT @TY_FN_PRE ^OR $c_makeFn toMeta
#0                    $c_makeFn curMod
@TY_FN_PRE            $c_makeFn isTyped
@TY_FN_PRE            $c_makeFn isTyFn
@TY_FN_PRE            $c_makeFn isSameMod
@TY_FN_PRE            $c_makeFn isCurMod
@TY_FN_PRE            $c_makeFn isFnLocals
#0                    $c_makeFn c_updateRKey
@TY_FN_PRE            $c_makeFn c_keySetTyped
@TY_FN_PRE            $c_makeFn c_keySetTy

@TY_FN_PRE            $c_makeFn assert
@TY_FN_PRE            $c_makeFn assertNot
@TY_FN_PRE            $c_makeFn tAssert
@TY_FN_PRE            $c_makeFn tAssertNot
@TY_FN_PRE            $c_makeFn tAssertEq
@TY_FN_PRE            $c_makeFn tAssertNe
@TY_FN_PRE            $c_makeFn assertFn
@TY_FN_PRE            $c_makeFn assertFnSmall
@TY_FN_PRE            $c_makeFn assertFnLocals
@TY_FN_PRE            $c_makeFn assertCurMod
@TY_FN_PRE            $c_makeFn assertTyped

$SFN isFnPre     $PRE $toMeta  @TY_FN_PRE$L1 %AND %RET
$SFN isFnNormal  $PRE $toMeta  @TY_FN_TY_MASK$L1 %AND  @TY_FN_NORMAL$L1  %EQ %RET
$SFN isFnInstant $PRE $toMeta  @TY_FN_TY_MASK$L1 %AND  @TY_FN_INSTANT$L1  %EQ %RET
$SFN isFnSmart   $PRE $toMeta  @TY_FN_TY_MASK$L1 %AND  @TY_FN_SMART$L1  %EQ %RET
$SFN isTyLocal   $PRE $toMeta  @META_TY_MASK$L1 %AND  @TY_LOCAL$L1  %EQ %RET
$SFN isLocalInput $PRE $toMeta  @TY_LOCAL_INPUT$L1 %AND %RET
$SFN isTyGlobal  $PRE $toMeta  @META_TY_MASK$L1 %AND  @TY_GLOBAL$L1  %EQ %RET
$SFN assertTyLocal $PRE $xsl isTyLocal  @E_cNotLocal$L2 $jmpl assert
$SFN assertTyGlobal $PRE $xsl isTyGlobal  @E_cNotGlobal$L2 $jmpl assert

$SFN getSz           @D_sz$L0          %DVFT %RET
$SFN setSz     $PRE  @D_sz$L0          %DVSR %RET
$SFN getWsLen        @D_wslen$L0       %DVFT %RET
$SFN c_xsCatch $PRE  @D_xsCatch$L0     %DVFT %RET
$SFN c_scan          @D_scan$L0        %DVFT %RET
$SFN panic   $PRE #0 $L0 %SWP  $jmpl assert // {errCode}: panic with errCode
$SFN unreach @E_unreach$L2 $jmpl panic // {}: assert unreachable code
$SFN assertWsEmpty   $xsl getWsLen  @E_wsEmpty $L2  $jmpl assertNot
$assertWsEmpty

$SFN ldictBuf // {} -> {ldict.buf:APtr}
  $hal2 .4%FTGL @c_dictBuf$h2
  $hal2 .2%FTGL @c_dictHeap$h2
  %ADD %RET

$SFN ldictArgs // {} -> dictArgs
  $xsl ldictBuf
  @c_dictLHeap$L2  // &ldict.heap
  %RET
$SFN ldictHeap $xsl ldictArgs .2%FT %ADD %RET // {} -> ldictHeap

$SFN _ldict $xsl c_scan $jmpl ldictArgs
$SFN ldictGet   $xsl _ldict @D_dict$L0  %DVFT %RET
$SFN ldictSet   $PRE $xsl _ldict @D_dict$L0  %DVSR %RET
$SFN ldictGetR  $xsl _ldict @D_rdict$L0 %DVFT %RET
$SFN reteq $PRE $INSTANT @NEQ $c1 @RETZ $c1 %RET


// **********
// * ASM Flow Control
// - `$IF ... $END` for if blocks
// - `$LOOP ... $BREAK0 ... $AGAIN $END` for infiinte loops with breaks.
//
// All flow control pushes the current heap on the WS, then END/AGAIN correctly
// stores/jmps the heap where they are called from.

$SFN IF  $PRE $INSTANT // {} -> {&jmpTo} : start an if block
  @JZL1 $c1 // compile .1%JZL instr
  $xsl getHeap // {&jmpTo} push &jmpTo location to stack
  #0$L0  $xsl h1 // compile 0 (jump pad)
  %RET

$SFN assertLt128  $PRE #80 $L1 %LT_U   @E_cJmpL1 $L2   $jmpl assert

$SFN END  $INSTANT // {&jmpTo} -> {} : end of IF or BREAK0
  %DUP          // {&jmpTo &jmpTo}
  $xsl getHeap  // {&jmpTo &jmpTo heap}
  %SWP %SUB     // {&jmpTo (heap-&jmpTo)}
  %DUP $xsl assertLt128
  .1%SR %RET // store at location after start (1 byte literal)

$SFN ELSE $INSTANT // {&ifNotJmpTo} -> {&elseBlockJmpTo}
  @JMPL $c1         // (end IF) compile unconditional jmp to end of ELSE
  $xsl getHeap %SWP // {&elseBlockJmpTo &ifNotJmpTo}
  #0$L0 $xsl h1     // compile jmp lit for &elseBlockJmpTo
  $jmpl END         // end of IF block (beginning of ELSE)

// $LOOP ... $BREAK0 ... $AGAIN $END
$SFN LOOP   $INSTANT $xsl getHeap  $jmpl ldictSet
$SFN BREAK0 $PRE $INSTANT $xsl IF $jmpl ldictSet
$SFN BREAK_EQ $PRE $INSTANT @NEQ $c1  $jmpl BREAK0 // break if equal
$SFN AGAIN $INSTANT
  @JMPL $c1  // compile jmp
  $xsl getHeap  // {heap}
  $xsl ldictGet // {heap &loopTo}
  %SUB     // {heap-&loopTo}
  %DUP $xsl assertLt128
  %NEG          // make negative for backwards jmp
  $jmpl h1      // compile as jmp offset

$SFN END_BREAK $xsl ldictGet $jmpl END

$SFN END_N $PRE $INSTANT // {...(N &jmpTo) numJmpTo}
  $LOOP l0 %DUP %RETZ
    %SWP $xsl END
    %DEC // dec numJmpTo
  $AGAIN l0

// * Some utilities for globals + locals

$SFN assertSzI $PRE // {szI}
  %DUP #CF$L1 %AND @E_cSz$L2 $xsl assertNot // non-sz bits empty
  #6$L0 %SHR #3$L1 %LT_U @E_cSz$L2 $jmpl assert // sz bits < 3

$SFN szToSzI $PRE // [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L0 %EQ $IF  %DRP @SZ1 $L1 %RET  $END
  %DUP #2$L0 %EQ $IF  %DRP @SZ2 $L1 %RET  $END
       #4$L0 %EQ $IF  %DRP @SZ4 $L1 %RET  $END
  @E_cSz$L2 $xsl panic

$SFN szILowerToSzI $PRE // convert a szI in lower bits to a proper szI
  #3$L0 %AND #4$L0 %SHL %RET

$SFN _metaRefSzI $PRE $toMeta $jmpl szILowerToSzI // {metaRef} -> {szI}

$SFN szIToSz $PRE // {szI} -> {sz}
  %DUP @SZ1$L1 %EQ $IF  %DRP #1$L0 %RET  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP #2$L0 %RET  $END
       @SZ4$L1 %EQ $IF       #4$L0 %RET  $END
  @E_cSz$L2 $xsl panic

$SFN haN  $PRE // {szI} align heap
  $xsl szIToSz $_jmpl _ha
$SFN halN $PRE // {szI} align heap for literal to szI
  $xsl szIToSz $_jmpl _hal

$SFN hN $PRE // {value szI} write a value of szI to heap
  %DUP @SZ1$L1 %EQ $IF  %DRP $jmpl h1  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP $jmpl h2  $END
  %DUP @SZ4$L1 %EQ $IF  %DRP $jmpl h4  $END
  @E_cSz$L2 $xsl panic

$SFN joinSzTyMeta $PRE #4$L0 %SHR %OR %RET // {tyMask szI} -> {tyMask}

$SFN _lSetup $PRE // {&metaO} -> {metaO} : checked local setup
  %DUP $_xsl assertTyped
  .4%FT %DUP $_jmpl assertTyLocal // returns metaOffset

$SFN _gSetup $PRE // {&metaRef} -> {metaRef} : checked global setup
  %DUP $_xsl assertTyped
  .4%FT %DUP $_jmpl assertTyGlobal

// {metaRO szInstr szLit instr} compile a literal memory instr.
//   szLit the size of the literal to compile for the instr.
//   metaRO: either a reference or an offset with a conforming meta attached to
//     it (meta at upper byte, szI in lowest 2 bits).
$FN _instrLitImpl $PRE
  #1 $h1 // 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 // store instr          {metaR0 szInstr szLit}
  %DUP $xsl halN // align for literal   {metaR0 szInstr szLit}
  .1%SRLL #0$h1 // store szLit          {metaR0 szInstr}
  .1%FTLL #1$h1 %OR $xsl h1 // compile (szInstr | instr) {metaRO}
  .1%FTLL #0$h1  $jmpl hN // compile literal of proper instrSz

$FN _getSetImpl $PRE // {localInstrSz localInstr globalInstrSz globalInstr}
  #1 $h1 // locals (see below)
  .1%SRLL#3$h1   .1%SRLL#2$h1 // 2=globalInstrSz 3=globalInstr
  .1%SRLL#1$h1   .1%SRLL#0$h1 // 0=localInstrSz 1=localInstr
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _lSetup %DUP $xsl _metaRefSzI // {metaRef szInstr}
    .1%FTLL#0$h1 .1%FTLL#1$h1  $xl _instrLitImpl %RET
  $END %DRP
  $xsl dictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _gSetup %DUP $xsl _metaRefSzI // {metaRef szInstr}
    .1%FTLL#2$h1 .1%FTLL#3$h1  $xl _instrLitImpl %RET
  $END @E_cNotType$L2 $xsl panic

// (create _xxxImpl for fngi to use)
$SFN _getImpl
  @SZ1$L1  @FTLL$L1  // local sz + instr
  @SZ2$L1  @FTGL$L1  // global sz + instr
  $xl _getSetImpl %RET

$SFN _setImpl
  @SZ1$L1  @SRLL$L1  // local sz + instr
  @SZ2$L1  @SRGL$L1  // global sz + instr
  $xl _getSetImpl %RET

$SFN _refImpl
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _lSetup // {metaOffset}
    $xsl toRef  %RGFT @R_LP$h1  %ADD // add to local stk ptr
    $jmpl L1
  $END %DRP
  $xsl dictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _gSetup  $xsl toRef $jmpl L4 // write literal directly TODO: use c_lit
  $END @E_cNotType$L2 $xsl panic

$SFN REF  $INSTANT $xsl c_scan $jmpl _refImpl
$SFN GET  $INSTANT $xsl c_scan $jmpl _getImpl
$SFN SET  $INSTANT $xsl c_scan $jmpl _setImpl

$FN GLOBAL  $INSTANT // <value> <szI> $GLOBAL <token>: define a global variable of sz
  #1 $h1 // 1 local [szI:U1]
  %DUP $xsl assertSzI // {value szI}
  %DUP $xsl haN // align heap {value szI}
  .1%SRLL#0$h1 // cache sz {value}
  $xsl c_updateRKey // {value &metaRef}
  $xsl loc
  @TY_GLOBAL$L1  .1%FTLL#0$h1   $xsl joinSzTyMeta // {value &metaRef meta}
  %SWP $xsl c_keySetTy // update key ty
  .1%FTLL#0$h1  $jmpl hN // write value to heap

$SFN c_makeGlobal $PRE // {szI} <token>: set meta for token to be a global.
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
$SFN c_updateRLKey // [] -> [&metaRef] update and return current local key
  $GET c_dictBuf   $GET c_dictHeap  %ADD // ldict.buf
  $GET c_dictLHeap                        // ldict.heap
  %ADD // {&newLkey}
  %DUP $SET c_rLKey  %RET

$FN align $PRE // {aptr sz}: return the aptr aligned properly with szI
  #1 $h1 // locals [sz:U1]
  .1%SRLL#0$h1 // cache sz
  %DUP // {aptr aptr}
  .1%FTLL#0$h1 %MOD // {aptr aptr%sz}
  %DUP $IF
    .1%FTLL#0$h1 %SWP %SUB // sz - aptr%sz
    %ADD %RET // aptr + (sz - aptr%sz)
  $END
  %DRP %RET

$SFN align4 $PRE #4$L0 $xl align %RET
$SFN alignSzI $PRE $xsl szIToSz  $xl align  %RET

// implement LOCAL or INPUT. Mostly just updating ldict key and globals.
$ha2 $FN _localImpl $PRE // {szI:U1 meta:U1}
  #1 $h1 // 0=szI:U1  1=meta:U1

  // assert current function is valid
  .4%FTGL @c_rKey$h2 %DUP $xsl assertTyped .4%FT // {szI meta fnMetaRef}
    $xsl assertFnLocals // {szI meta}

  .1%SRLL#1$h1 // 1=meta
  %DUP  $xsl assertSzI // {szI}
  %DUP  .1%SRLL#0$h1 // {szI} cache szI
  @TY_LOCAL$L1  .1%FTLL#1$h1 %OR // update full meta {szI meta}
  %SWP  $xsl joinSzTyMeta  // {meta}
  $GET c_localOffset // {meta loff}
  .1%FTLL#0$h1  $xsl alignSzI // align local offset {meta loff}
  // c_localOffset = offset + sz
  %DUP  .1%FTLL#0$h1 $xsl szIToSz %ADD  $SET c_localOffset
  $xsl c_updateRLKey // {meta loff &metaRef}
  %SWP $xsl ldictSet  // set to localOffset {meta &metaRef}
  $xsl c_keySetTy %RET

// #<szI> $LOCAL myLocal: declare a local variable of sz
// This stores the offset and sz for lRef, lGet and lSet to use.
$SFN LOCAL $INSTANT  #0             $L1 $xl _localImpl %RET
$SFN INPUT $INSTANT  @TY_LOCAL_INPUT$L1 $xl _localImpl %RET

$SFN Dict_keyLen   $PRE %INC4 .1%FT  #3F$L1 %AND %RET             // {&key} -> {len:U1}
$SFN Dict_keySz    $PRE $xsl Dict_keyLen #5$L0 %ADD $jmpl align4  // {&key} -> {sz:U1}
$SFN Dict_nextKey  $PRE %DUP $xsl Dict_keySz %ADD %RET                 // {&key} -> {&key}

// {&key} -> {} recursive function to compile INPUTs
// Inputs are "compiled" (i.e. a SRLL is compiled) in reverse order.
// This maps them well to the conventional stack nomenclature.
$FN _compileInputs $PRE
  #1$h1 // locals 0=&key:APtr
  %DUP  .4%SRLL#0$h1 // {&key}
  $xsl ldictHeap $reteq // return if key=ldictHeap
  .4%FTLL#0$h1  $xsl Dict_nextKey  $xl _compileInputs // get the next key and recurse {}
  .4%FTLL#0$h1  %DUP $xsl isTyped %SWP .4%FT // {hasTy metaRef}
  %DUP $xsl isLocalInput %SWP // {hasTy isLocal metaRef}
  $xsl isLocalInput %LAND %LAND %RETZ // {}
  .4%FTLL#0$h1  .4%FT  %DUP $xsl _metaRefSzI // {metaRef szInstr}
  @SZ1$L1 @SRLL$L1 $xl _instrLitImpl
  %RET

// - Updates the number of slots for the FN
// - compiles SRLL for each INPUT in reverse order.
$SFN END_LOCALS
  $GET c_localOffset #4$L0 $xl align
  #2$L0 %SHR $xsl h1 // update number of slots
  $xsl ldictBuf $xl _compileInputs %RET

// **********
// * Fngi Compile Loop

$FN c_parseNumber // {} -> {value isDecimal}
  @SZ4 $LOCAL value
  @SZ1 $LOCAL i
  @SZ1 $LOCAL c
  @SZ1 $LOCAL base
  $END_LOCALS
  #A$L0 $SET base
  #0$L0 $SET value
  #0$L0 $SET i

  // Get correct base
  $GET c_tokenBuf .1%FT #30$L0 %EQ $IF // if c0 == '0' {}
    $GET c_tokenBuf %INC .1%FT %DUP // {c1 c1}
    #62$L1 %EQ  $IF // if .tokenBuf@1=='b' {c}
      #2$L0  $SET base  #2$L0 $SET i
    $END
    #78$L1 %EQ  $IF // if .tokenBuf@1=='x' {}
      #10$L0 $SET base  #2$L0 $SET i
    $END
  $END

  $LOOP l0
    $GET i  $GET c_tokenLen $BREAK_EQ b0
    $GET c_tokenBuf $GET i %ADD .1%FT // {c}
    %DUP $SET c
    // Check that c is between '0' and '9'
    %DUP #30$L0 %LT_U %SWP // {c<'0' c}
    #3A$L0 %GE_U %LOR $IF   #0$L0 @FALSE$L0 %RET   $END
    $GET base  $GET value %MUL // {base * value}
    $GET c  #30$L0 %SUB %ADD $SET value // value = value*10 + (c - '0')
    $GET i %INC $SET i // i += 1
  $AGAIN l0  $END_BREAK b0
  $GET i  @E_cToken$L2 $xsl assert // assert there was a token
  $GET value @TRUE$L0 %RET

$assertWsEmpty

$SFN c_stateCompile $GET c_state @C_COMPILE$L2 %AND %RET // {} -> state
$SFN c_stateInstant $GET c_state @C_INSTANT$L2 %AND %RET // {} -> state

$SFN c_lit $PRE // {asInstant value:U4} : compile proper sized literal
  %SWP %NOT %RETZ // if instant, leave on stack
  %DUP #40$L1 %LT_U        $IF  $jmpl L0  $END
  %DUP #FF$L1 %INC %LT_U   $IF  $jmpl L1  $END
  %DUP #FFFF$L2 %INC %LT_U $IF  $jmpl L2  $END
                                $jmpl L4

$SFN xSzI $PRE // {metaRef} -> {szI}: return the size requirement of the X instr
   $xsl isCurMod $IF  @SZ2$L1 %RET  $END  @SZ4$L1 %RET

$SFN c_fn $PRE // {&metaRef}: compile a function
  %DUP $xsl assertTyped
  .4%FT %DUP $xsl isTyFn @E_cNotFn$L2 $xsl assert // {metaRef}
  %DUP $xsl xSzI %DUP // {metaRef instrSzI szLit} (lit and instr are same size)
  %OVR $xsl isFnLocals $IF @XL$L1 $ELSE @XSL$L1 $END // {metaRef instrSzI szLit instr}
  $xl _instrLitImpl %RET

$SFN execute // {metaRef} -> {...}: execute a function
  %DUP $xsl toRef %SWP // {ref metaRef}
  $xsl isFnLocals  $IF %XW %RET $END
  %JMPW

$SFN _compConstant // {asInstant} -> {asInstant metaRefFn[nullable]}
  $xl c_parseNumber $IF  $xsl c_lit #0$L0 %RET  $END %DRP

  // Handle local dictionary. Only constants allowed here.
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    %DUP $xsl isTyped  @E_cNotFnOrConst$L2 $xsl assertNot
    .4%FT $xsl c_lit  #0$L0 %RET
  $END %DRP

  $xsl dictArgs  @D_rdict$L0 %DVFT // {asInstant &metaRef}

  // Constant
  %DUP $xsl isTyped %NOT $IF  .4%FT $xsl c_lit #0$L0 %RET  $END

  // Must be a function
  .4%FT %DUP $xsl isTyFn @E_cNotFn$L2 $xsl assert // {asInstant metaRef}
  %RET

// {asInstant metaRefFn} -> {metaRefFn} check fn type and update asInstant
$FN _compFnAsInstant $PRE
  %DUP $xsl isFnNormal $IF
    %SWP $IF @TY_FN_INSTANT$L1 $ELSE #0$L0 $END // {metaRefFn newFnTy}
    $jmpl metaSet
  $END %SWP %DRP %RET // not normal = leave alone

$FN fngiSingle // {asInstant} -> {}
  @SZA $LOCAL metaRef $END_LOCALS
  $xsl c_scan

  // Handle constants
  $xsl _compConstant // {asInstant metaRefFn[nullable]}
  %DUP %NOT $IF  %DRP %DRP %RET  $END // {metaRefFn}
  $xl _compFnAsInstant // {metaRefFn} update instant type depending
  // if pre, recursively call fngiSingle (compile next token first)
  %DUP $xsl isFnPre $IF
    $SET metaRef  $xl fngiSingle  $GET metaRef
  $END
  %DUP $xsl isFnNormal
  $IF    $jmpl c_fn
  $ELSE  $jmpl execute  $END


// The core fngi compiler loop
$dictGetR c_compFn  @fngiSingle .4^SR // initialize c_compFn
@SZ4 $c_makeGlobal c_compFn

$FN fngi
  $LOOP l0  @FALSE$L0  $GET c_compFn %XSW  $AGAIN l0

$SFN c_number  $xsl c_scan $xl c_parseNumber %RET
