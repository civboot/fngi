\  |00322998|L0027|@158D {0...      72C4|        1} [JMPL  U2] +++ "a" ((_setImpl   0x1527)) 

\ This file bootstraps spor from the native (i.e. C) implementation into
\ a more full-featured language with helpful macros.
\
\ Note: this file requires the compiler to pass the current heap value on the
\ stack so that it can define it's first function (getHeap), which it then
\ uses to define the other functions.
\
\ # Table of Contents
\ Search for these headings to find information
\
\ [1] Instructions: contains definition of spore assembly instructions and
\    documentation.
\    [1.a] Operations: Special
\    [1.b] Operations: One Inp -> One Out
\    [1.c] Operations: Two Inp -> One Out
\    [1.d] Small Literal [0x40 - 0x80)
\    [1.e] Sizes: SZ1, SZ2, SZ4
\    [1.f] Jmp: jumping, execution, tables
\    [1.g] Mem: fetch, store, locals, globals
\ [2] Registers and Device Operations (RGFT, RGSR, DVFT, DVSR)
\ [3] Constants
\    [3.a] Dict Ty Bits
\    [3.b] Zoab
\    [3.c] Log Levels
\    [3.d] Errors
\ [4] Globals
\ [5] Bootstrap Macros: necessary functionality for the rest of the language.
\ [6] Core functions and macros: bread and butter of spor assembly
\ [7] ASM Flow Control (IF, LOOP, etc)
\ [8] Scanning and Alignment Utilities: reading, peeking and szI alignment
\ [9] globals and locals
\ [10] Zoa strings and logging zoab
\ [11] Fngi compile loop

\ **********
\ * [1] Instructions
\ Spor uses 8 bit instructions with the following bit layout:
\
\ Note: (S=sz bit)
\ 00XX XXXX: operation
\ 01XX XXXX: small literal
\ 10SS XXXX: jmp
\ 11SS _XXX: mem

\ # [1.a] Operations: Special
#00 =NOP   \ { -> }     no operation
#01 =RETZ  \ Return if zero
#02 =RET   \ Return
#03 =SWP   \ {l r -> r l} swap
#04 =DRP   \ {l -> }    drop
#05 =OVR   \ {l r -> }  drop 2
#06 =DUP   \ {l -> l l} duplicate
#07 =DUPN  \ {l -> l l==0} DUP then NOT
#08 =DVFT  \ Device Operation Fetch
#09 =DVSR  \ Device Operation Store
#0A =RGFT  \ {-> v}  Register Fetch
#0B =RGSR  \ {v}     Register Store

\ # [1.b] Operations: One Inp -> One Out
#10 =INC   \ {l+1}  increment 1
#11 =INC2  \ {l+2}  increment 2
#12 =INC4  \ {l+4}  increment 4
#13 =DEC   \ {l-4}  decrement 1
#14 =INV   \ {~l}   Bitwise Inversion
#15 =NEG   \ {-l}   Negate (2's compliment)
#16 =NOT   \ {l==0} Logical NOT
#17 =CI1   \ {ISz}  Convert I1 to ISz
#18 =CI2   \ {ISz}  Convert I2 to ISz
\ future: leading 0's, trailing 0's, count of 1's
\ Some single-arg extension commands might be:
\ (7) floating point abs, negative, ceil, floor, trunc, nearest, and sqrt
\ (1) i -> f conversion
\ (1) f -> i conversion

\ # [1.c] Operations: Two Inp -> One Out
#20 =ADD   \ {l +  r } add
#21 =SUB   \ {l -  r } subtract
#22 =MOD   \ {l %  r } integer modulo (remainder)
#23 =SHL   \ {l << r } bit shift left
#24 =SHR   \ {l >> r } bit shift right
#25 =BAND  \ {l &  r } bitwise and
#26 =BOR   \ {l |  r } bitwise or
#27 =XOR   \ {l ^  r } bitwise xor
#28 =LAND  \ {l && r } logical and
#29 =LOR   \ {l || r } logical or
#2A =EQ    \ {l == r } equal
#2B =NEQ   \ {l != r } not equal
#2C =GE_U  \ {l >= r } unsigned greater than or equal
#2D =LT_U  \ {l <  r } unsigned less than
#2E =GE_S  \ {l >= r } signed greater than or equal
#2F =LT_S  \ {l <  r } signed less than

#30 =MUL   \ {l *  r } multiplication
#31 =DIV_U \ {l / r  } unsigned division
#32 =DIV_S \ {l / r  } signed division
\ Double-arg extension commands might be:
\ floating point: add,sub,mul,div,ge,lt

\ # [1.d] Small Literal [0x40 - 0x80)
#40 =SLIT

\ # [1.e] Sizes
#00 =SZ1
#10 =SZ2
#20 =SZ4
#20 =SZA

\ # [1.f] Jmp
\
\ Jumps can be to either a literal (L) or to an item on the working stack (W).
\ - X means "execute" and is a call.
\ - XS is an "execute small" and means that the function has no local stack.
\ - XL/XW will execute a function that has a local stack. The size of the
\   local stack is stored in the first 16bits at the function's address, which
\   are loaded by the execute instr and stored in the highest byte in the
\   callstack (which RET uses to shrink the local stack on return).

\   Jmp      Description
#80 =JMPL  \ Jmp to Literal
#81 =JMPW  \ Jmp to WS
#82 =JZL   \ Jmp to Literal if store==0
#83 =JTBL  \ Jump to Table index using size=Literal
#84 =XL    \ Execute Literal (mPtr)
#85 =XW    \ Execute WS (aPtr)
#86 =XSL   \ Execute Small Literal (no LS update)
#87 =XSW   \ Execute Small WS (no LS update)

\ JZL and JMPL for SZ=1
\ For SZ=1 they jump to the 1 byte signed offset from the location
\ of the LITERAL (not the location of the operation).

\ # [1.g] Mem|Store              |Description
#C0 =LIT    \ {} -> {literal}    |Literal (U1, U2 or U4)
#C1 =FT     \ {addr} -> {value}  |FeTch value from addr
#C2 =FTLL   \ {} -> {local}      |FeTch from LP + U1 literal offset
#C3 =FTGL   \ {} -> {global}     |FeTch from GB + U2 literal offset
#C4 =SR     \ {value addr} -> {} |Store value at addr
#C5 =SRLL   \ {value} -> {}      |StoRe value at LP + U1 literal offset
#C6 =SRGL   \ {value} -> {}      |StoRe value at GB + U2 literal offset
#C7 =FTLO   \ {} -> {@local}     |fetch offset local (deref'd)
#C8 =FTGO   \ {} -> {@global}    |fetch offset global (deref'd)
#C9 =SRLO   \ {value} -> {}      |store offset local (deref'd)
#CA =SRGO   \ {value} -> {}      |store offset global (deref'd)

\ Common instr+szs
@SZ2 @XSL  ^BOR  =XSL2
@SZ2 @JMPL ^BOR  =JMPL2

\ **********
\ * [2] Registers and Device Operations
\
\ Registers and device operations can be accessed through RGXX and DVXX
\ operations. RG operations include a 1 byte literal. The 1 byte literal has
\ the following byte format:
\ 1OOO OOOO: (R_LP) local stack pointer with 7bit offset (O)
\ 0XXX XXXR: register. X is currently undefined.
\
\ FT will return the register value + offset
\ SR will store the value + offset in the register

#80 =R_LP \ local stack pointer
#00 =R_EP \ execution pointer, SR will panic
#01 =R_GB \ global base pointer

\ Device operations with DVFT and DVSR
#00 =D_read   \ read from src, filling up tokenBuf
#01 =D_scan   \ FT: scan next word (tokenBuf)  SR: line comment
#02 =D_dict   \ [&buf &heap isLocal] FT=get SR=set dict key=tokenBuf
#03 =D_rdict  \ [&buf &heap] FT=get reference to val  SR=forget including key
#05 =D_comp   \ compile (assemble) the token in tokenBuf
#06 =D_assert \ error if != 0
#07 =D_wslen  \ get working stack length (in slots)
#08 =D_cslen  \ get call stack lengh (in slots)
\ {-> err} D_xCatch executes large function from WS but catches a panic.
\ The errCode is returned (or 0 if no error).
\ Note: caches and restores ep, call stack and local stack state and clears
\ working stack (besides the returned err).
#09 =D_xCatch
#0A =D_memSet  \ {dst v len} "dst = v [len]". FT: memset, SR: memmove
#0B =D_memCmp  \ {&a &b len} -> I32: <0 if a<b; >0 if a>b; 0 if a==b
#0C =D_com     \ {&msg len} -> {ioResult}: write to debug stream
#0D =D_zoa     \ {} -> {} parse zoa to heap
#0E =D_dictDump\ {<dict-args> [&entry]} dump a dictionary FT=entry SR=full
#0F =D_comZoab \ FT{U4}  SR{len &data join}
#10 =D_comDone \ signifies end of one com. On linux this is a flush.

\ **********
\ * [3] Constants
#0 =FALSE
#1 =TRUE

\ * [3.a] Dict Ty Bits
\ Meta types
#40 =KEY_HAS_TY \ if 1, dict entry is a non-constant
#E0 =META_TY_MASK \ upper three bits determine type
#20 =TY_FN    \ function, can be called and has an fnMeta
#40 =TY_LOCAL   \ local variable, has varMeta. Accessed with FTLL/SRLL
#60 =TY_GLOBAL  \ global variable, has varMeta. Accessed with FTGL/SRGL
#80 =TY_DICT    \ a "dictionary" type. Points to a new dictionary which has
                \ it's own type.
#FF_FFFF =REF_MASK
#FF_0000 =MOD_MASK

\ FN meta bits [TTTL TTSP] L=locals T=fnTy S=syntax P=pre
#0C =TY_FN_TY_MASK \ 0b1100
#01 =TY_FN_PRE     \ Compile next token first. Creates pre-order arguments.
#00 =TY_FN_NORMAL  \ Normally compiled, can use $ to make instant
#04 =TY_FN_INSTANT \ Required to be run as instant (must use $)
#08 =TY_FN_SMART   \ Always run immediately, compiler will pass asInstant
#0C =TY_FN_SMART_I \ A smart function was called as asInstant (runtime only)

#10 =TY_FN_LARGE  \ has locals (called with XL or XW)

\ Local meta bits  [TTTI RRSS] I=input R=ref S=szI
\ Global meta bits [TTT- RRSS]         R=ref S=szI
#10 =TY_LOCAL_INPUT
#0C =TY_VAR_REF
#03 =TY_VAR_SZ

\ * [3.b] Zoab
#C0 =ZOAB_TY   \ bitmask: all types
#80 =ZOAB_JOIN \ bitmask: join type
#40 =ZOAB_ARR  \ bitmask: arr type
#C0 =ZOAB_PTR  \ equality: next 4 bytes are a pointer.

\ * [3.c] Log Levels
#00 =LOG_SILENT

\ Log Levels
#40 =LOG_SYS
#20 =LOG_USER
#1F =LOG_TRACE
#0F =LOG_DEBUG
#07 =LOG_INFO
#03 =LOG_WARN
#01 =LOG_CRIT

\ Language Level (builtin) Logs
#1F =LOG_INSTR
#07 =LOG_EXECUTE
#03 =LOG_ASM
#01 =LOG_COMPILER

\ * [3.d] Errors
\ [E000 - E100): built-in errors.
\  E100: device-specific hardware errors
\ [E200-E800): reserved
\  E800+: application errors
\  AXXX_EXXX: test case assertion error.

#0     =E_ok      \ no error
#E000  =E_general \ general errors [E000-E010)
#E010  =E_io      \ IO error class
#E0A0  =E_asm     \ assembly error class (cause in asm).
#E0C0  =E_comp    \ compiler error class (cause in comp).
#A000  =E_test    \ [AXXX] (assert) test case error.

#E001  =E_intern  \ internal (undefined) error
#E002  =E_undef   \ undefined error
#E003  =E_unreach \ unreachable code
#E004  =E_todo    \ executed incomplete (to do) code
#E005  =E_wsEmpty \ the WS was expected empty
#E006  =E_unimpl  \ unimplemented error

#E0A1  =E_null    \ null access
#E0A2  =E_oob     \ out of bounds access
#E0A3  =E_stkUnd  \ Stack underflow
#E0A4  =E_stkOvr  \ Stack overflow
#E0A5  =E_align2  \ access off 2byte allign
#E0A6  =E_align4  \ access off 4byte align
#E0A7  =E_divZero \ divide by zero

#E0C1  =E_cInstr  \ invalid instr
#E0C2  =E_cToken  \ token invalid
#E0C3  =E_cTLen   \ token invalid
#E0C4  =E_cKey    \ key already exists
#E0C5  =E_cNoKey  \ dict key not found
#E0C6  =E_cHex    \ non-hex number
#E0C7  =E_cSz     \ invalid Sz selected
#E0C8  =E_cSzPtr  \ invalid Sz for aptr
#E0C9  =E_cRet    \ invalid RET
#E0CA  =E_cDblSr  \ Double store
#E0CB  =E_cDevOp  \ device op not impl
#E0CC  =E_DictOvr \ dict overflow
#E0CD  =E_cXHasL  \ small-execute to fn w/locals
#E0CE  =E_cXNoL   \ large-execute to fn wo/locals
#E0CF  =E_cErr    \ D_assert err code invalid
#E0D0  =E_cKeyLen \ Key len too large
#E0D1  =E_cReg    \ Register error
#E0D2  =E_cStr    \ Str invalid

#E0E0  =E_cNotGlobal \ using a non-global as global
#E0E1  =E_cIsX       \ using an XS for an X
#E0E2  =E_cIsXS      \ using an X for an XS
#E0E3  =E_cJmpL1     \ JMP1 over too much space
#E0E4  =E_cNotFn
#E0E5  =E_cNotFnLarge
#E0E6  =E_cMod       \ different modules
#E0E7  =E_cLSz       \ literal sz
#E0E9  =E_cNotType
#E0EA  =E_cNotLocal
#E0EB  =E_cNotVar
#E0EC  =E_cNotFnOrConst
#E0ED  =E_eof
#E0EE  =E_cUnclosed   \ unclosed paren/brace/etc
#E0EF  =E_cReqInstant \ fn is INSTANT but no '$' used
#E0EF  =E_cCompOnly   \ fn is SMART and requires no $ used.
#E0F0  =E_cUnknownEsc \ unknown character escape
#E0F1  =E_cZoab       \ Zoab invalid
#E0F2  =E_cNeedToken  \ token not present
#E0F2  =E_cNeedNumber \ number not present
#E0F3  =E_cBadRefs    \ too many de/refs
#E0F3  =E_cRefEq      \ .&var = not allowed.

#E0B0  =E_iBlock      \ invalid block index
#E0B1  =E_ptrBlk      \ invalid block ptr
#E0B2  =E_aaPo2       \ invalid po2

\ **********
\ * [4] Globals
#0000_0004 =heap
#0000_0008 =topHeap
#0000_000C =topMem
#0000_0010 =err
#0000_0014 =c_state
#0000_0018 =testIdx
#0000_001C =sysLogLvl
#0000_001E =usrLogLvl

\ Dictionary Struct
#0000_0020 =c_dictBuf   \ U4
#0000_0024 =c_dictHeap  \ U2
#0000_0026 =c_dictEnd   \ U2
#0000_0028 =c_dictLHeap \ U2 + U2(align)

\ TokenBuf Struct
#0000_002C =c_tokenBuf   \ [APtr] TokenBuf struct
#0000_0030 =c_tokenLen   \ [U1] length of token
#0000_0031 =c_tokenSize  \ [U1] characters buffered
#0000_0032 =c_tokenGroup \ [U1] token group

\ Global Error Variables
#0000_0034 =c_errValTy     \ [U1]
#0000_0038 =c_dataASz      \ [U2]
#0000_003A =c_dataBSz      \ [U2]
#0000_003C =c_errVal1      \ [U4]
#0000_0040 =c_errVal2      \ [U4]
#0000_0044 =c_msg          \ [APtr]

\ Block allocator (12 bytes, see fngi.fn)
#0000_0048 =BA_kernel

\ Global Compiler Variables
#0000_0054 =c_rKey         \ [U4] &key of current dict key
#0000_0058 =c_rLKey        \ [U4] &key of current ldict key.
#0000_005C =c_gheap        \ [U4] global heap
#0000_0060 =c_localOffset  \ [U2] Local Offset (for local var setup)

#00  =ERR_DATA_NONE
#01  =ERR_DATA_INT1
#02  =ERR_DATA_DATA1
#03  =ERR_DATA_INT2
#04  =ERR_DATA_DATA2

#62 @c_gheap .4^SR  \ initial value of global heap

\ **********
\ * [5] Bootstrap Macros
\ These macros must be defined in pure ASM. They build on eachother
\ to make the syntax much more readable.
\
\ fn h1 [U1] -> []                : push 1 byte to heap
\ fn h2 [U2] -> []                : push 2 byte to heap
\ fn h4 [U4] -> []                : push 4 byte to heap
\ fn L0 [U1] -> []                : compile a small literal [#0 - #3F]
\ fn $dictSet <key> [U4] -> []    : set a dictionary key to value
\ fn $dictGet <key>  [] -> [U4]   : get dictionary key's value
\ fn $dictGetK <key> [] -> [APtr] : get the key's &key
\ fn $loc <token> [] -> []        : set token to the current heap location
\ fn hpad [U4] -> []              : pad heap with NOPs
\ fn hal2 [] -> []                : align heap for 2 byte literal
\ fn hal4 [] -> []                : align heap for 4 byte literal
\ fn ha2 [] -> []                 : align heap to 2 bytes
\ fn ha4 [] -> []                 : align heap to 4 bytes
\
\ Assertions: these panic with the supplied errCode if cond is not met.
\
\   assert [cond errCode]
\   assertNot [cond errCode]
\
\ Test Assertions: these panic with E_test if the cond is not met.
\ tAssert, tAssertNot, tAssertEq
\ Spore assembly constants.

%NOP \ (unaligned) Note: below is using value pushed by compiler.
^INC =getHeap
                .4%FTGL @heap.2,
    %RET \ (unaligned)

$getHeap =h1  \ h1: {val:1} push 1bytes from stack to heap
                      .4%FTGL @heap.2, \ fetch heap {val, heap}
  %NOP                .1%SR            \ store 1 byte value at heap
  %NOP                .4%FTGL @heap.2, \ fetch heap {val, heap}
  %INC                .4%SRGL @heap.2, \ heap=heap+1
  %RET \ (unaligned)

$getHeap =L0   \ L0: compile a small literal (unchecked)
          .1%LIT
  #3F,      %BAND       \ truncated to bottom 6 bits
  .1%LIT    @SLIT,
  %BOR       .2%JMPL .2@h1, \ made into SLIT instr and stored. (aligned)

%NOP \ (unaligned)
$getHeap =h2  \ h2: {val:2} push 2bytes from stack to heap
              .4%FTGL @heap.2, \ fetch heap {val, heap}
  %NOP        .2%SR            \ store 2 byte value at heap
  %NOP        .4%FTGL @heap.2, \ {heap}
  .4%INC2     %SRGL   @heap.2,   \ heap=heap+2
  %RET \ (unaligned)

$getHeap =h4  \ h4: {val:4} push 4bytes from stack to heap
            .4%FTGL @heap.2, \ fetch heap {val, heap}
  %NOP      .4%SR           \ store 4 byte value at heap
  %NOP      .4%FTGL @heap.2, \ {heap}
  %INC4     .4%SRGL @heap.2, \ heap=heap+4
  %RET \ (unaligned)

$getHeap =dictArgs \ args for dict.
  \ put {dict.buf &dict.heap isLocal=FALSE} on stack
            .4%FTGL @c_dictBuf $h2
  %NOP      .2%LIT @c_dictHeap $h2
  #0$L0     %RET \ isLocal=FALSE
  %NOP \ (unaligned)

$getHeap =_dict \ setup for dict.
  \ Scan next token
            @D_scan$L0
  %DVFT     .2%JMPL @dictArgs$h2 \ (aligned)

%NOP \ (unaligned)
$getHeap =dictSet \ dictSet: Set "standard" dictionary to next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVSR \ device dict SR
  %RET \ (unaligned)

$getHeap =dictGet \ dictGet: Get the value of the next token.
            .2%XSL @_dict $h2
  @D_dict$L0   %DVFT
  %RET \ (unaligned)

$getHeap =dictGetK \ dictGetK: Get the &key of the next token.
            .2%XSL @_dict $h2
  @D_rdict$L0   %DVFT
  %RET \ (unaligned)

$getHeap =loc \ $loc <name>: define a location
            .4%FTGL @heap$h2
  %NOP      .2%XSL @dictSet$h2
  \ Clear ldict (locals dict)
  #0$L0        .2%SRGL @c_localOffset $h2  \ zero localDict.offset
  #0$L0        .4%SRGL @c_dictLHeap $h2    \ zero localDict.heap
  %RET \ (unaligned)

$loc setHeap     %SRGL @heap $h2      %RET \ (unaligned)
$loc getTopHeap  %FTGL @topHeap $h2   %RET \ (unaligned)
$loc setTopHeap  %SRGL @topHeap $h2   %RET \ (unaligned)

%NOP \ (aligned)
$loc hpad \ {pad} write pad bytes to heap.
  \ WHILE(pad) we write NOP to heap
  @heap ^FT \ c-stk{loopStart}
    .4%DUP        .2%JZL \ if(pad == 0) breakTo
      .4@heap ^FT ^SWP #0 $h2 \ c-stk{breakTo loopStart}
    @NOP$L0       .2%XSL @h1 $h2 \ write a noop
    .4%DEC        .2%JMPL    $h2 \ DEC and jmp to loopStart
  @heap .4^FT ^SWP .2^SR \ update breakTo spot
  %DRP           %RET \ (aligned)

$loc _hal \ {align} heap align (for) literal
  .4%DUP        .2%XSL @getHeap $h2 \ {align align heap}
  .4%SWP          %MOD \ {align heap%align}
  \ pad = (align-1) - heap%align
  .4%SWP          %DEC \ {heap%align align-1}
  .4%SWP          %SUB
  .4%NOP        .2%JMPL @hpad $h2

\ halN: heap align (for) N-byte literal.
$loc hal2   #2$L0          .2%JMPL @_hal $h2 \ (aligned)
$loc hal4   #4$L0          .2%JMPL @_hal $h2 \ (aligned)

$hal2 $loc _ha \ {align} heap align (with NOPs)
                .2%XSL @getHeap $h2 \ {align heap}
  .4%SWP          %MOD \ {heap%align}
  $hal2 .2%JMPL @hpad $h2

\ haN: {align} heap align N byte.
#2 $_ha
$loc ha2   #2$L0   .2%JMPL @_ha $h2
$loc ha4   #4$L0   .2%JMPL @_ha $h2


\ Assert checks a condition or panics with an error
\ ex: <some check> @E_myError assert
$hal2 $loc assertNot \ {failIfTrue errCode}
                  %SWP
  %NOT            %SWP \ fallthrough (aligned)
$loc assert    \ {failIfFalse errCode}
  @D_assert$L0     %DVFT
  %RET \ (unaligned)

$hal2 $loc tAssert
        .2%LIT @E_test $h2
  $hal2 %JMPL @assert $h2

$loc tAssertNot     .4%NOT $hal2 .2%JMPL @tAssert,

$ha2 $loc tAssertEq \ {a b}
   @ERR_DATA_INT2$L0  .1%SRGL @c_errValTy$h2
   %NOP               .4%SRGL @c_errVal2$h2 \ b {a}
   %DUP               .4%SRGL @c_errVal1$h2 \ a {a}
   %NOP               .4%FTGL @c_errVal2$h2 \ {a b}
  .4%EQ  $hal2 .2%JMPL @tAssert,

\ **********
\ * [6] Core functions and macros
\ These are the bread and butter of spor assembly needed to create the fngi compiler.
\ They define and call functions, check the type of dictionary entries,
\ provide local dictionary support, etc.
\
\ fn $xsl  <token>                : compile execute small function
\ fn $xl   <token>                : compile execute large function
\ fn $jmpl <token>                : compile jump to small function
\ fn L1 / L2 / L4  [U] -> []      : compile 1 / 2 / 4 byte literal.
\ fn xCatch [... &fn]             : execute a large function and catch error.
\ fn reteq [a b]                  : return immediately if a == b (%NEQ %RETZ)
\
\ fn FN <name>                    : declare a large function
\ fn SFN <name>                   : declare a small function
\ fn PRE                          : make function "pre" (run after next token)
\ fn SMART                        : make function "smart" (always instant)
\ fn INSTANT                      : require function to be instant
\
\ fn $c1 [instr]                  : INSTANT to compile instr when executed
\ fn toRef [metaRef] -> [ref]     : strip meta byte from metaRef (top byte)
\ fn $toMeta [metaRef] -> [meta]  : INSTANT to convert to a meta byte
\ fn isTypes [&metaRef] -> [U]    : tells whether a &metaRef is not a constant.
\ fn isTyFn [metaRef] -> [U1]     : metaRef is a fn
\ fn isFnLarge [metaRef] -> [U1]  : metaRef is a large fn (has locals)
\ fn isFnPre     ...
\ fn isFnNormal  ...
\ fn isFnInstant  ...
\ fn isFnSmart  ...
\ fn isFnSmartI  ...                : "smart instant", only at runtime
\ fn isTyLocal [metaRef -> U1]      : is a local offset
\ fn isTyLocalInput [metaRef -> U1] : is a local offset input
\ fn isTyGlobal [metaRef -> U1]     : is a global variable
\
\ fn toMod [ref -> [mod]          : get the "module" (upper byte of U4)
\ fn curMod [ -> mod]             : get the module of the last dict entry
\ fn isCurMod [ref -> mod]        : get whether ref is curMod
\
\ fn panic [errCode]              : instantly panic with error
\ fn unreach []                   : instantly panic with E_unreach
\ fn assertWsEmpty []             : assert the working stack is empty (E_wsEmpty)
\ fn assertNoInstant [asInstant]  : used by SMART to assert not called with $
\ fn assertCurMod [ref]           : assert ref is cur mod
\ fn assertNotNull [&r]           : E_null if r is NULL
\ fn assertTyped [&metaRef]       :
\ fn assertFnSmall [metaRef]      : assert fn is small (no locals)
\ fn assertFnLarge [metaRef]      : assert fn is large (has locals)
\
\ fn getWsLen [ -> U4]            : get working stack length
\ fn c_updateRKey [ -> &metaRef]  : update rKey=dictHeap and return it
\ fn ldictBuf / ldictArgs / ldictHeap : interface directly with local dict
\ fn ldictSet / ldictGet / ldictGetK  : set/get/get-ref of local dict key
\ fn c_keySetTyped [&metaRef]         : make a key non-global
\ fn c_makeTy <token> [<dictArgs> meta] : make token be typed meta
\ fn c_dictSetMeta [<dictArgs> meta:U1 &metaRef] : update dict key meta

$hal4 $loc toRef \ {metaRef} -> {ref}
  .4%LIT @REF_MASK $h4 %BAND %RET

$hal2 $loc _j2 \ {ref instr} compile jmpInstr to 2 byte ref
  $hal2 .2%XSL @hal2 $h2    \ enforce proper alignment
  $hal2 .2%XSL @h1 $h2      \ compile instr {metaRef}
  $hal2 .2%XSL @toRef $h2   \ {ref}
  $hal2 .2%JMPL @h2 $h2     \ compile addr

$hal2 $loc _xsl \ $_xsl <token> : compile unchecked xsl
  $hal2 .2%XSL @dictGet $h2 \ {key}
  .1%LIT @XSL2 $h1  \ push .2%XSL instr
  $hal2 .2%JMPL @_j2 $h2

$hal2 $loc _jmpl \ $_jmpl <token>: compile unchecked jmpl
  $_xsl dictGet             \ {key}
  .1%LIT @JMPL2 $h1 \ push .2%JMPL instr
  $hal2 .2%JMPL @_j2 $h2

$hal2 $loc L1 \ {U1} compile 1 byte literal
  .1%LIT  @SZ1 @LIT ^BOR  $h1 \ push .1%LIT instr
  $_xsl h1 \ compile it
  $_jmpl h1

\ INSTANT PRE $c1: {instr:U1}
\ Compiles code so that when executed the instr will be compiled.
$hal2 $loc c1
  $_xsl L1    \ compile the instr literal itself
  \ compile xsl to h1
  $hal2 .2%LIT @h1 $h2
  $hal2 .2%LIT @XSL2 $h2
  $_jmpl _j2

$hal2 $loc L2 \ {U1} compile 2 byte literal
  $_xsl hal2 \ enforce proper alignment
  @SZ2 @LIT  ^BOR  $c1  \ compile .2%LIT instr
  $_jmpl h2  \ compile the 2 byte literal

$hal2 $loc L4 \ {U1} compile 4 byte literal
  $_xsl hal4 \ enforce proper alignment
  @SZ4 @LIT  ^BOR  $c1 \ compile .4%LIT
  $_jmpl h4  \ compile the 4 byte literal

$loc toMeta \ INSTANT {metaRef} -> {meta}
  @SLIT #18 ^BOR $c1  @SHR $c1  %RET
$loc keyMeta \ INSTANT {&key -> meta}
  @INC4 $c1   @SZ1 @FT ^BOR $c1 %RET

$loc isTyped  \ {&key} dict value is a constant
  %DUP @E_cNoKey$L2 $_xsl assert \ no key if &key=NULL
  %INC4 .1%FT @KEY_HAS_TY$L1 %BAND %RET

\ These take {&key} and tell information about it
$loc isTyFn      $toMeta  @META_TY_MASK$L1 %BAND  @TY_FN$L1  %EQ %RET
$loc KEYisTyFn      $keyMeta  @META_TY_MASK$L1 %BAND  @TY_FN$L1  %EQ %RET
$loc isFnLarge   $toMeta  @TY_FN_LARGE$L1 %BAND %RET
$loc KEYisFnLarge   $keyMeta  @TY_FN_LARGE$L1 %BAND %RET
$loc assertNotNull @E_null$L2 $_jmpl assert

$loc assertTyped \ [&metaRef]
  %DUP @E_cKey$L2 $_xsl assert \ assert key was found
  $_xsl isTyped @E_cNotType $L2 $_jmpl assert
$loc assertFn   $_xsl isTyFn  @E_cNotFn $L2  $_jmpl assert \ [metaRef] -> []
$loc KEYassertFn   $_xsl KEYisTyFn  @E_cNotFn $L2  $_jmpl assert \ [&key] -> []

$loc assertFnSmall \ [metaRef]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $_jmpl assertNot
$loc KEYassertFnSmall \ [metaRef]
  %DUP $_xsl KEYassertFn
  $_xsl KEYisFnLarge  @E_cIsX $L2  $_jmpl assertNot

$loc assertFnLarge \ [metaRef]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $_jmpl assert

$hal4 $loc toMod @MOD_MASK $L4 %BAND %RET \ {ref} -> {mod}
$loc isSameMod \ {metaRef metaRef} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET

$hal2 $loc curMod   .2%FTGL @c_rKey$h2 .4%FT  $_jmpl toMod \ [] -> [mod]
$hal2 $loc isCurMod $_xsl toMod  $_xsl curMod %EQ %RET        \ [ref] -> [isCurMod]
$hal2 $loc assertCurMod  $_xsl isCurMod  @E_cMod$L2  $_jmpl assert

$loc _jSetup \ [&metaRef] -> [metaRef]: checked jmp setup
  %DUP $_xsl assertTyped
  .4%FT \ {metaRef}
  %DUP $_xsl assertFnSmall
  %DUP $_jmpl assertCurMod

$hal2 $loc  assertNoInstant @E_cCompOnly$L2 $_jmpl assertNot   \ {asInstant} -> {}

$loc xsl \ $xsl <token> : compile .2%xsl
  $_xsl dictGetK $_xsl _jSetup \ {metaRef}
  .1%LIT @XSL2 $h1  \ get .2%XSL instr
  $_jmpl _j2

$loc xl \ $xl <token> : compile .2%xl
  $_xsl dictGet \ {metaRef}
  %DUP $_xsl assertFnLarge
  %DUP $_xsl assertCurMod
  @SZ2 @XL   ^BOR  $L1  $_jmpl _j2

$loc jmpl  \ $jmpl <token> : compile jmpl2
  $_xsl dictGetK $_xsl _jSetup \ {metaRef}
  @JMPL2$L1  $_jmpl _j2

$hal2 $loc c_updateRKey \ [] -> [&metaRef] update and return current key
        .4%FTGL @c_dictBuf$h2  \ dict.buf
  $hal2 .2%FTGL @c_dictHeap$h2 \ dict.heap
  .4%ADD \ {&newKey}
  %DUP $hal2 .4%SRGL @c_rKey$h2 \ rKey=newKey
  %RET \ return &metaRef (newKey)

$loc metaSet \ {metaRef meta:U1} -> U4 : apply meta to metaRef
  #18 $L0  %SHL  \ make meta be upper byte
  %BOR %RET

$loc rMetaSet \ {&metaRef meta:U1} -> U4 : apply meta to &metaRef
  %OVR .4%FT %SWP \ {&metaRef metaRef meta}
  $_xsl metaSet   \ {&metaRef newMetaRef}
  %SWP .4%SR %RET

$loc c_dictDumpEntry  @D_dictDump$L0 %DVFT %RET \ {<dictArgs> &metaRef}

$loc c_keySetTyped \ {&metaRef} -> []
  %INC4 %DUP \ {&len &len}
  .1%FT @KEY_HAS_TY$L1 %BOR \ {&len tyKeyLen}
  %SWP .1%SR %RET            \ update tyKeyLen

$loc c_dictSetMeta \ {<dictArgs> meta:U1 &metaRef -> } update dict key's meta.
  %SWP %OVR \ {<dictArgs> &metaRef meta &metRef}
  %DUP $_xsl c_keySetTyped \ make key "typed" {<dictArgs> &metaRef meta &metaRef}
  %SWP $_xsl rMetaSet \ {<dictArgs> &metaRef}
  $_jmpl c_dictDumpEntry

$hal2 $loc _declFn \ [<dictArgs> meta]
  @TY_FN$L1 %BOR \ {<dictArgs> meta}
  $_xsl c_updateRKey \ {<dictArgs> meta &metaRef}
  $_xsl loc
  $_jmpl c_dictSetMeta

$hal2 $loc SFN  \ SMART $SFN <token>: define location of small function
  $_xsl assertNoInstant $_xsl dictArgs #0$L0         $_jmpl _declFn

$ha2 $loc FN  \ SMART $FN <token>: define location of function with locals
  $_xsl assertNoInstant $_xsl dictArgs @TY_FN_LARGE$L1 $_jmpl _declFn

$ha2
$loc SMART \ {} modify current function to be smart
  %DRP .4%FTGL @c_rKey$h2  @TY_FN_SMART$L1   $_jmpl rMetaSet

$ha2
$loc INSTANT \ {} modify current function to be smart
  %DRP .4%FTGL @c_rKey$h2  @TY_FN_INSTANT$L1 $_jmpl rMetaSet

\ Backfill the fn meta
$loc c_makeTy \ {<dictArgs> meta} make an existing symbol a type.
  $_xsl dictGetK   \ {meta &metaRef}
  $_jmpl c_dictSetMeta

#0 \ manually insert asInstant=False (since compiler doesn't know it's smart yet)
$FN c_makeFn \ {meta} <token>: set meta for token to be a small function.
  #1$h1  \ locals: 0=meta:U1
  .1%SRLL#0$h1  $_xsl dictArgs .1%FTLL#0$h1 \ {<dictArgs> meta}
  @TY_FN$L1 %BOR  $_jmpl c_makeTy

$ha2 $loc PRE %DRP .4%FTGL @c_rKey$h2  @TY_FN_PRE$L1   $_jmpl rMetaSet

@TY_FN_SMART        $c_makeFn SFN
@TY_FN_SMART        $c_makeFn FN
@TY_FN_SMART        $c_makeFn SMART
@TY_FN_SMART        $c_makeFn INSTANT
@TY_FN_SMART        $c_makeFn PRE
#0                  $c_makeFn xsl
@TY_FN_INSTANT      $c_makeFn jmpl
@TY_FN_PRE            $c_makeFn L0
@TY_FN_PRE            $c_makeFn L1
@TY_FN_PRE            $c_makeFn L2
@TY_FN_PRE            $c_makeFn L4

#0                    $c_makeFn loc
@TY_FN_PRE            $c_makeFn h1
@TY_FN_PRE            $c_makeFn h2
@TY_FN_PRE            $c_makeFn h4
@TY_FN_PRE @TY_FN_INSTANT ^BOR $c_makeFn c1
@TY_FN_PRE            $c_makeFn dictSet
#0                    $c_makeFn dictGet
#0                    $c_makeFn dictGetK
#0                    $c_makeFn dictArgs
#0                    $c_makeFn getHeap
#0                    $c_makeFn getTopHeap
@TY_FN_PRE            $c_makeFn setHeap
@TY_FN_PRE            $c_makeFn hpad
#0                    $c_makeFn hal2
#0                    $c_makeFn hal4
#0                    $c_makeFn ha2
#0                    $c_makeFn ha4
@TY_FN_PRE            $c_makeFn metaSet
@TY_FN_PRE            $c_makeFn toRef
@TY_FN_PRE            $c_makeFn toMod
@TY_FN_PRE @TY_FN_INSTANT ^ADD $c_makeFn toMeta
#0                    $c_makeFn curMod
@TY_FN_PRE            $c_makeFn isTyped
@TY_FN_PRE            $c_makeFn isTyFn
@TY_FN_PRE            $c_makeFn isSameMod
@TY_FN_PRE            $c_makeFn isCurMod
@TY_FN_PRE            $c_makeFn isFnLarge
#0                    $c_makeFn c_updateRKey
@TY_FN_PRE            $c_makeFn c_dictDumpEntry
@TY_FN_PRE            $c_makeFn c_keySetTyped
@TY_FN_PRE            $c_makeFn c_dictSetMeta
#0                    $c_makeFn c_makeTy

@TY_FN_PRE            $c_makeFn assert
@TY_FN_PRE            $c_makeFn assertNot
@TY_FN_PRE            $c_makeFn assertNotNull
@TY_FN_PRE            $c_makeFn tAssert
@TY_FN_PRE            $c_makeFn tAssertNot
@TY_FN_PRE            $c_makeFn tAssertEq
@TY_FN_PRE            $c_makeFn assertFn
@TY_FN_PRE            $c_makeFn assertFnSmall
@TY_FN_PRE            $c_makeFn assertFnLarge
@TY_FN_PRE            $c_makeFn assertCurMod
@TY_FN_PRE            $c_makeFn assertTyped
#0                    $c_makeFn assertNoInstant


$SFN isFnPre     $PRE $toMeta  @TY_FN_PRE$L1 %BAND %RET
$SFN isFnNormal  $PRE $toMeta  @TY_FN_TY_MASK$L1 %BAND  @TY_FN_NORMAL$L1  %EQ %RET
$SFN isFnInstant $PRE $toMeta  @TY_FN_TY_MASK$L1 %BAND  @TY_FN_INSTANT$L1  %EQ %RET
$SFN isFnSmart   $PRE $toMeta  @TY_FN_TY_MASK$L1 %BAND  @TY_FN_SMART$L1  %EQ %RET
$SFN isFnSmartI  $PRE $toMeta  @TY_FN_TY_MASK$L1 %BAND  @TY_FN_SMART_I$L1  %EQ %RET
$SFN isTyLocal   $PRE $toMeta  @META_TY_MASK$L1 %BAND  @TY_LOCAL$L1  %EQ %RET
$SFN isLocalInput $PRE $toMeta  @TY_LOCAL_INPUT$L1 %BAND %RET
$SFN isTyGlobal  $PRE $toMeta  @META_TY_MASK$L1 %BAND  @TY_GLOBAL$L1  %EQ %RET
$SFN assertTyLocal $PRE $xsl isTyLocal  @E_cNotLocal$L2 $jmpl assert
$SFN assertTyGlobal $PRE $xsl isTyGlobal  @E_cNotGlobal$L2 $jmpl assert

$SFN getWsLen      @D_wslen$L0  %DVFT %RET
$SFN xsCatch $PRE  @D_xCatch$L0 %DVFT %RET
$SFN c_scan        @D_scan$L0   %DVFT %RET
$SFN panic   $PRE #0$L0 %SWP  $jmpl assert \ {errCode}: panic with errCode
$SFN unreach @E_unreach$L2 $jmpl panic \ {}: assert unreachable code
$SFN assertWsEmpty   $xsl getWsLen  @E_wsEmpty $L2  $jmpl assertNot
$assertWsEmpty

\ Update the harness with the new dictionary
$SFN c_dictDump       $xsl dictArgs @D_dictDump$L0 %DVSR %RET \ {}
$c_dictDump

$SFN ldictBuf \ {} -> {ldict.buf:APtr}
  $hal2 .4%FTGL @c_dictBuf$h2
  $hal2 .2%FTGL @c_dictHeap$h2
  %ADD %RET

$SFN ldictArgs \ {} -> dictArgs
  $xsl ldictBuf
  @c_dictLHeap$L2  \ &ldict.heap
  #1$L0 \ isLocal=TRUE
  %RET
$SFN ldictHeap $xsl ldictArgs %DRP .2%FT %ADD %RET \ {} -> ldictHeap

$SFN _ldict $xsl c_scan $jmpl ldictArgs
$SFN ldictGet   $xsl _ldict @D_dict$L0  %DVFT %RET
$SFN ldictSet   $PRE $xsl _ldict @D_dict$L0  %DVSR %RET
$SFN ldictGetK  $xsl _ldict @D_rdict$L0 %DVFT %RET
$SFN retz  $PRE $SMART $xsl assertNoInstant @RETZ$c1 %RET
$SFN reteq $PRE $SMART $xsl assertNoInstant @NEQ$c1 @RETZ$c1 %RET
$SFN retif $PRE $SMART $xsl assertNoInstant @NOT$c1 @RETZ$c1 %RET


\ **********
\ * [7] ASM Flow Control
\ - `$IF ... $END` for if blocks
\ - `$LOOP ... $BREAK0 ... $AGAIN $BREAK_END` for infiinte loops with breaks.
\
\ All flow control pushes the current heap on the WS, then END/AGAIN correctly
\ stores/jmps the heap where they are called from.

$SFN IF  $PRE $SMART $xsl assertNoInstant \ {} -> {&jmpTo} : start an if block
  @SZ1 @JZL  ^BOR  $c1 \ compile .1%JZL instr
  $xsl getHeap \ {&jmpTo} push &jmpTo location to stack
  #0$L0  $xsl h1 \ compile 0 (jump pad)
  %RET

$SFN assertLt128  $PRE #80 $L1 %LT_U   @E_cJmpL1 $L2   $jmpl assert

$SFN _END
  %DUP          \ {&jmpTo &jmpTo}
  $xsl getHeap  \ {&jmpTo &jmpTo heap}
  %SWP %SUB     \ {&jmpTo (heap-&jmpTo)}
  %DUP $xsl assertLt128
  %SWP .1%SR %RET \ store at location after start (1 byte literal)
$SFN END $SMART $xsl assertNoInstant $jmpl _END  \ {&jmpTo} -> {} : end of IF or BREAK0

$SFN ELSE $SMART $xsl assertNoInstant \ {&ifNotJmpTo} -> {&elseBlockJmpTo}
  @JMPL $c1         \ (end IF) compile unconditional jmp to end of ELSE
  $xsl getHeap %SWP \ {&elseBlockJmpTo &ifNotJmpTo}
  #0$L0 $xsl h1     \ compile jmp lit for &elseBlockJmpTo
  $jmpl _END        \ end of IF block (beginning of ELSE)

\ $LOOP l0 ... $BREAK0 b0 ... $AGAIN l0  $BREAK_END b0
$SFN LOOP   $SMART $xsl assertNoInstant $xsl getHeap  $jmpl ldictSet
$SFN BREAK0 $PRE $SMART   $xsl IF $jmpl ldictSet
$SFN BREAK_IF  $PRE $SMART @NOT$c1  $jmpl BREAK0 \ break if true
$SFN BREAK_EQ  $PRE $SMART @NEQ$c1  $jmpl BREAK0 \ break if equal
$SFN BREAK_NEQ $PRE $SMART @EQ$c1  $jmpl BREAK0 \ break if equal
$SFN AGAIN $SMART $xsl assertNoInstant
  @JMPL $c1  \ compile jmp
  $xsl getHeap  \ {heap}
  $xsl ldictGet \ {heap &loopTo}
  %SUB     \ {heap-&loopTo}
  %DUP $xsl assertLt128
  %NEG          \ make negative for backwards jmp
  $jmpl h1      \ compile as jmp offset

$SFN END_BREAK $SMART $xsl assertNoInstant $xsl ldictGet $jmpl _END

$SFN END_N $PRE $SMART $xsl assertNoInstant \ {...(N &jmpTo) numJmpTo}
  $LOOP l0 %DUP %RETZ
    %SWP #0$L0 $xsl _END
    %DEC \ dec numJmpTo
  $AGAIN l0

\ **********
\ * [8] Scanning and Alignment Utilities
\ Reading, peeking and szI alignment
\
\ fn align [aptr sz -> aptr]      : align aptr with sz bytes
\ fn align4 [aptr -> aptr]        : align aptr with 4 bytes
\ fn alignSzI [aptr szI -> aptr]  : align aptr with szI bytes
\ fn haN [szI]                    : align the heap to szI
\ fn halN [szI]                   : align the heap for a literal to szI
\ fn hN [U4 szI]                  : write a value of szI to heap (no align)
\ fn szToSzI [U4 -> SzI]          : convert number of bytes to SzI
\ fn szIToSz [SzI -> U1]          : convert szI to number of bytes
\ fn szILowerToSzI [U1 -> SzI]    : convert lower bit SzI (used in meta)
\
\ fn anyDictGetR [-> &metaRef isFromLocal] : any ref to current token.
\ fn c_read [ -> numRead]         : attempt to read bytes from src.
\ fn c_readNew [ -> numRead]      : clear token buf and read bytes.
\ fn c_scanNoEof []               : scan and assert not EOF.
\ fn c_peekChr [ -> c]            : peek at the next character.
\ fn c_countChr [c -> count]      : count and consume matching characters
\ fn c_clearToken []              : shift buffer to clear current token
\
\ fn assertSzI [szI]              : assert that szI is valid
\ fn c_isEof [ -> bool]           : return whether there was a token scanned
\ fn c_assertToken []             : assert there is a token
\ fn c_assertNoEof [numRead]      : assert that numRead > 0 (E_eof)

$SFN szILowerToSzI $PRE \ convert a szI in lower bits to a proper szI
  #3$L0 %BAND #4$L0 %SHL %RET

$SFN _metaRefSzI $PRE $toMeta $jmpl szILowerToSzI \ {metaRef} -> {szI}

$SFN szIToSz $PRE \ {szI} -> {sz}
  %DUP @SZ1$L1 %EQ $IF  %DRP #1$L0 %RET  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP #2$L0 %RET  $END
       @SZ4$L1 %EQ $IF       #4$L0 %RET  $END
  @E_cSz$L2 $xsl panic

$SFN haN  $PRE \ {szI} align heap
  $xsl szIToSz $_jmpl _ha
$SFN halN $PRE \ {szI} align heap for literal to szI
  $xsl szIToSz $_jmpl _hal

$SFN hN $PRE \ {value szI} write a value of szI to heap
  %DUP @SZ1$L1 %EQ $IF  %DRP $jmpl h1  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP $jmpl h2  $END
  %DUP @SZ4$L1 %EQ $IF  %DRP $jmpl h4  $END
  @E_cSz$L2 $xsl panic

$SFN joinSzTyMeta $PRE #4$L0 %SHR %BOR %RET \ {tyMask szI} -> {tyMask}

$SFN assertSzI $PRE \ {szI}
  %DUP #CF$L1 %BAND @E_cSz$L2 $xsl assertNot \ non-sz bits empty
  #4$L0 %SHR #3$L1 %LT_U @E_cSz$L2 $jmpl assert \ sz bits < 3

$SFN szToSzI $PRE \ [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L0 %EQ $IF  %DRP @SZ1 $L1 %RET  $END
  %DUP #2$L0 %EQ $IF  %DRP @SZ2 $L1 %RET  $END
       #4$L0 %EQ $IF  %DRP @SZ4 $L1 %RET  $END
  @E_cSz$L2 $xsl panic

$FN align $PRE \ {aptr sz -> aptr}: align aptr with sz bytes
  #1 $h1 \ locals [sz:U1]
  .1%SRLL#0$h1 \ cache sz
  %DUP \ {aptr aptr}
  .1%FTLL#0$h1 %MOD \ {aptr aptr%sz}
  %DUP $IF
    .1%FTLL#0$h1 %SWP %SUB \ {aptr (sz - aptr%sz)}
    %ADD %RET \ aptr + (sz - aptr%sz)
  $END
  %DRP %RET

$SFN align4 $PRE #4$L0 $xl align %RET \ {addr -> aligned4Addr}
$SFN alignSzI $PRE $xsl szIToSz  $xl align  %RET \ {addr szI -> addrAlignedSzI}

$SFN null2 #0$L0 %DUP %RET

$SFN ftSzI \ {&addr szI}
  %DUP @SZ1$L1 %EQ $IF %DRP .1%FT %RET $END
  %DUP @SZ2$L1 %EQ $IF %DRP .2%FT %RET $END
       @SZ4$L1 %EQ $IF      .4%FT %RET $END
  @E_cSz$L2 $jmpl panic

$SFN srSzI \ {value &addr szI}
  %DUP @SZ1$L1 %EQ $IF %DRP .1%SR %RET $END
  %DUP @SZ2$L1 %EQ $IF %DRP .2%SR %RET $END
       @SZ4$L1 %EQ $IF      .4%SR %RET $END
  @E_cSz$L2 $jmpl panic

$SFN memCmp   $PRE  @D_memCmp$L0 %DVFT %RET  \ {&a &b len -> cmp}
$SFN memClear $PRE  #0$L0 %SWP \ !fallthrough! {dst len}     "dst = 0"
$SFN memSet   $PRE  @D_memSet$L0 %DVFT %RET  \ {dst v len}   "dst = v"
$SFN memMove  $PRE  @D_memSet$L0 %DVSR %RET  \ {dst src len} "dst = src"
$SFN c_scanEol     @D_scan$L0   %DVSR %RET      $hal2
$SFN c_isEof .1%FTGL@c_tokenLen$h2 %NOT %RET    $hal2
$SFN c_assertToken .1%FTGL@c_tokenLen$h2 @E_cNeedToken$L2 $jmpl assert
$SFN c_assertNoEof $PRE @E_eof$L2 $jmpl assertNot \ {numRead}

$SFN c_scanNoEof
  $xsl c_scan
  $xsl c_isEof
  @E_eof$L2 $jmpl assertNot

$SFN c_peekChr \ {} -> {c} peek at a character
  $xsl c_scan
  $xsl c_isEof $IF  #0$L0 %RET  $END
  $hal2 .4%FTGL@c_tokenBuf$h2 .1%FT \ {c}
  #0$L0 $hal2 .1%SRGL@c_tokenLen$h2 %RET \ reset scanner for next scan

$SFN c_countChr $PRE \ { chr -> count } count and consume matching chrs
  #0$L0 $LOOP l0 \ {chr count}
    %OVR $xsl c_peekChr %NEQ $IF %SWP %DRP %RET $END
    %INC \ inc count, then inc tokenLen
    $hal2 .1%FT@c_tokenLen$h2 %INC  $hal2 .1%SR@c_tokenLen$h2
  $AGAIN l0


$SFN c_read \ { -> numRead} attempt to read bytes
  #1$L0 @D_read$L0 %DVFT %RET

$SFN c_readNew \ { -> numRead} clear token buf and read bytes
  #0$L0 $hal2 .1%SRGL@c_tokenLen$h2
  #0$L0 $hal2 .1%SRGL@c_tokenSize$h2
  #1$L0 @D_read$L0 %DVFT %RET

$hal2
$SFN c_clearToken \ shift buffer to clear current token
  .4%FTGL@c_tokenBuf$h2                 \ {&tokenBuf}
  %DUP $hal2 .1%FTGL@c_tokenLen$h2 %ADD \ {&tokenBuf &tokenEnd}
  $hal2 .1%FTGL@c_tokenLen$h2           \ {&tokenBuf &tokenEnd tokenLen}
  $jmpl memMove

\ dotMeta bitmask: OOOO OOOO | DL&@ SXRR
\ where O=offset byte, D=done, L=local, &=ref, @=deref S=store
\ R=number of ref/derefs
#80 =DOT_DONE   \ if 1 then all compilation is already done.
#40 =DOT_LOCAL \ 0 = global
#20 =DOT_REF
#10 =DOT_DEREF
#08 =DOT_STORE \ 0 = fetch

$SFN anyDictGetR \ {} -> {&metaRef isFromLocal}
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    @DOT_LOCAL$L1 %RET
  $END %DRP
  $xsl dictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    @FALSE$L0 %RET
  $END @E_cNotType$L2 $xsl panic

\ **********
\ * [9] globals and locals
\ We need a way to define global and local variables, as well as GET, SET and
\ obtain a REF to them.
\
\ fn GET <token>            SMART : compile a FT of token (local or global)
\ fn _SET <token>           SMART : compile a SR of token (local or global)
\ fn REF <token>            SMART : compile a "get ref" of token
\ fn GLOBAL <token> [SzI]   SMART : define a global variable of szI
\ fn LOCAL <token> [SzI]    SMART : define a local variable of szI
\ fn INPUT <token> [SzI]    SMART : define a local input variable of szI
\ fn END_LOCALS             SMART : end locals

$SFN _lSetup $PRE \ {&metaO} -> {metaO} : checked local setup
  %DUP $_xsl assertTyped
  .4%FT %DUP $_jmpl assertTyLocal \ returns metaOffset

$SFN _gSetup $PRE \ {&metaRef} -> {metaRef} : checked global setup
  %DUP $xsl assertTyped
  .4%FT %DUP $jmpl assertTyGlobal

\ {metaRO szInstr szLit instr} compile a literal memory instr.
\   szLit the size of the literal to compile for the instr.
\   metaRO: either a reference or an offset with a conforming meta attached to
\     it (meta at upper byte, szI in lowest 2 bits).
$FN c_instrLitImpl $PRE
  #1 $h1 \ 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 \ store instr          {metaR0 szInstr szLit}
  %DUP $xsl halN \ align for literal   {metaR0 szInstr szLit}
  .1%SRLL #0$h1 \ store szLit          {metaR0 szInstr}
  .1%FTLL #1$h1 %BOR $xsl h1 \ compile (szInstr | instr) {metaRO}
  .1%FTLL #0$h1  $jmpl hN \ compile literal of proper instrSz

\ Compile a get or set instruction.
\ Args:
\   &metaRef: contains the reference (global/local offset) and metadata needed to
\             compile correctly.
\   dotMeta: whether the value to compile is a local or global.
\   localInstrSz localInstr: if isFromLocal: use these as the literal sz and instr.
\   globalInstrSz globalInstr: if NOT isFromLocal: use these as the literal sz and instr.
$FN _getSetImpl $PRE
  #1 $h1 \ locals (see below)
  .1%SRLL#3$h1   .1%SRLL#2$h1 \ 2=globalInstrSz 3=globalInstr
  .1%SRLL#1$h1   .1%SRLL#0$h1 \ 0=localInstrSz 1=localInstr
  \ {&metaRef dotMeta}
  @DOT_LOCAL$L1 %BAND $IF
    $xsl _lSetup %DUP $xsl _metaRefSzI \ {metaRef szInstr}
    .1%FTLL#0$h1 .1%FTLL#1$h1
  $ELSE
    $xsl _gSetup %DUP $xsl _metaRefSzI \ {metaRef szInstr}
    .1%FTLL#2$h1 .1%FTLL#3$h1
  $END
  $xl c_instrLitImpl %RET

\ (create _xxxImpl for fngi to use)
$SFN _getImpl $PRE \ {&metaRef dotMeta}
  @SZ1$L1  @FTLL$L1  \ local sz + instr
  @SZ2$L1  @FTGL$L1  \ global sz + instr
  $xl _getSetImpl %RET

$SFN _setImpl $PRE \ {&metaRef dotMeta}
  @SZ1$L1  @SRLL$L1  \ local sz + instr
  @SZ2$L1  @SRGL$L1  \ global sz + instr
  $xl _getSetImpl %RET

$SFN _refImpl \ {}
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _lSetup \ {metaOffset}
    $xsl toRef %DUP #40$L1 %LT_U @E_cReg$L2 $xsl assert \ {offset}
    @R_LP$L1 %BOR \ {LpOffset}: offset is lower 7 bits
    @RGFT$c1 $jmpl h1  \ compile: %RGFT (@R_LP + offset)$h1
  $END %DRP
  $xsl dictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    $xsl _gSetup  $xsl toRef $jmpl L4 \ write literal directly TODO: use c_lit
  $END @E_cNotType$L2 $xsl panic

\ # DOT (.) Compiler
\ The below is the  initial compiler for below cases:
\   .var            \ variable fetch
\   .var = <token>  \ variable store
\   .&var           \ variable reference (also function)
\   .@var           \ variable dereference
\   .@var = <token> \ variable dereference store
\
\ These are built to be extended by future dot compiler implementations for
\ (i.e.) structs, modules, roles, etc.

$SFN c_dotRefs \ { -> dotMeta } get dot meta for de/refs.
  \ Get the dotMeta for preceeding & or @
  $xsl c_peekChr %DUP #26$L0 %EQ $IF \ Reference (&) case
    $xsl c_countChr %DUP #4$L0 %LT_U @E_cBadRefs$L2 $xsl tAssert
    @DOT_REF$L0 %ADD
  $ELSE
    %DUP #40$L1 %EQ $IF \ Dereference (@) case
      $xsl c_countChr #4$L0 %LT_U @E_cBadRefs$L2 $xsl tAssert
      @DOT_DEREF$L0 %ADD
    $ELSE %DRP #0$L0 \ no meta
    $END
  $END %RET \ {dotMeta}

$SFN c_dotEq \ {&metaRef dotMeta} ".var =" case
  %DUP @DOT_REF$L0 %BAND  @E_cRefEq$L2 $xsl tAssertNot \ "&var =" is invalid
  %DUP @DOT_DEREF$L0 %BAND  $IF \ Deref assignment ".@var ="
    %DUP #3$L0 %BAND \ {&metaRef dotMeta refCount}
    %DUP #2$L0 %GE_U $IF \ if refCount >= 2 we do multple fetches then sr.
      @E_unimpl$L2 $jmpl panic
    $ELSE \ TODO: switch to _setImpl once it can do SROL.
      %DRP $xsl _getImpl @SR$c1 \ a FT_L followed by a SR
    $END
  $ELSE $jmpl _setImpl
  $END

$SFN c_dotDeref \ {&metaRef dotMeta}
  %OVR %SWP $xsl _getImpl \ compile a ft (local or global) {dotMeta}
  #3$L0 %BAND %DEC $LOOP l0 \ {remainingDerefs}
    %DUPN $IF %DRP %RET $END \ return when remaining==0
    @FT $c1 %DEC
  $AGAIN l0



\ {&metaRef dotMeta} implementation of "standard" dot cases including
\ fetch/store of ref or deref
\ $SFN c_dotStd
\   %DUP @DOT_STORE$L1 %BAND $IF \ Handle ".var =" case
\     $jmpl c_dotEq
\   $END
\   %DUP @DOT_REF$L0 %BAND $IF \ case &var
\     \ assert refCount == 1. This is all that will ever be allowed.
\     %DUP #3$L0 %BAND  #1$L0 %EQ  @E_cBadRefs$L2 $xsl tAssert \ {&metaRef dotMeta}
\     @DOT_LOCAL$L1 %BAND $IF \ {&metaRef}
\       $xsl _lSetup \ {metaOffset}
\       $xsl toRef %DUP #40$L1 %LT_U @E_cReg$L2 $xsl assert \ {offset}
\       @R_LP$L1 %BOR \ {LpOffset}: offset is lower 7 bits
\       @RGFT$c1 $jmpl h1  \ compile: %RGFT (@R_LP + offset)$h1
\     $END
\     $xsl _gSetup  $xsl toRef $jmpl L4 \ write literal directly TODO: use c_lit
\   $END
\   %DUP @DOT_DEREF$L0 %BAND $IF $jmpl c_dotDeref \ case @@@var $END
\   %DRP $jmpl _getImpl \ standard get



\ $SFN c_dot
\   $xsl anyDictGetR %SWP .4%SRLL #0$h1 \ {dotMeta isFromLocal} cache &metaRef
\   %ADD %RET
\ $xsl c_peekChr #3D$L0 %EQ $IF  \ '='


$SFN REF  $SMART
  $IF  $xsl dictGetK $xsl _gSetup $jmpl toRef  $END
  $xsl c_scan $jmpl _refImpl

$SFN GET  $SMART
  $IF  $xsl dictGetK $xsl _gSetup %DUP $xsl _metaRefSzI \ {metaRef szInstr}
       %SWP $xsl toRef %SWP $jmpl ftSzI   $END
  $xsl c_scan $xsl anyDictGetR $jmpl _getImpl

$SFN _SET $SMART $xsl assertNoInstant $xsl c_scan $xsl anyDictGetR $jmpl _setImpl

$FN c_makeGlobal $PRE \ {szI} <token>: set meta for token to be a global.
  #1$h1 \ locals 0=szI:u1
  .1%SRLL#0$h1  $xsl dictArgs .1%FTLL#0$h1 \ {<dictArgs> locals}
  #4$L0 %SHR  @TY_GLOBAL$L1  %BOR  $_jmpl c_makeTy

@SZA $c_makeGlobal heap
@SZA $c_makeGlobal topHeap
@SZA $c_makeGlobal topMem
@SZ4 $c_makeGlobal err
@SZ4 $c_makeGlobal c_state
@SZ4 $c_makeGlobal testIdx
@SZ2 $c_makeGlobal sysLogLvl
@SZ2 $c_makeGlobal usrLogLvl
@SZA $c_makeGlobal c_dictBuf
@SZ2 $c_makeGlobal c_dictHeap
@SZ2 $c_makeGlobal c_dictEnd
@SZ2 $c_makeGlobal c_dictLHeap
@SZA $c_makeGlobal c_tokenBuf
@SZ1 $c_makeGlobal c_tokenLen
@SZ1 $c_makeGlobal c_tokenSize
@SZ1 $c_makeGlobal c_tokenGroup
@SZ1 $c_makeGlobal c_errValTy     \ [U1]
@SZ2 $c_makeGlobal c_dataASz      \ [U2]
@SZ2 $c_makeGlobal c_dataBSz      \ [U2]
@SZ4 $c_makeGlobal c_errVal1      \ [U4]
@SZ4 $c_makeGlobal c_errVal2      \ [U4]
@SZ4 $c_makeGlobal c_msg          \ [APtr]
@SZ4 $c_makeGlobal BA_kernel      \ [BlockAllocator]

@SZ4 $c_makeGlobal c_rKey
@SZ4 $c_makeGlobal c_rLKey
@SZ4 $c_makeGlobal c_gheap
@SZ2 $c_makeGlobal c_localOffset

$FN GLOBAL \ <value> <szI> $GLOBAL <token>: define a global variable of sz
  #2$h1  $SMART $xsl assertNoInstant  \ locals 0=szI 1=meta 4=&metaRef
  %DUP $xsl assertSzI \ {value szI}
  .1%SRLL#0$h1 \ set szI {value}
  $xsl c_updateRKey \ {value &metaRef}
  $xsl loc \ initialize dictionary entry \ {value &metaRef}
  $GET c_gheap .1%FTLL#0$h1  $xsl alignSzI \ {value &metaRef alignedGHeap}
  %DUP $_SET c_gheap \ update gheap to aligned value {value &metaRef alignedGHeap}
  %OVR .4%SR         \ set dictionary value to alignedGHeap {value &metaRef}

  @TY_GLOBAL$L1  .1%FTLL#0$h1   $xsl joinSzTyMeta \ {value &metaRef meta}
  .1%SRLL#1$h1 .4%SRLL#4$h1 \ {value}
  $xsl dictArgs  .1%FTLL#1$h1 .4%FTLL#4$h1 \ {value <dictArgs> meta &metaRef}
  $xsl c_dictSetMeta \ update key ty {value}
  $GET c_gheap .1%FTLL#0$h1  $xsl srSzI \ store global value
  $GET c_gheap .1%FTLL#0$h1 $xsl szIToSz %ADD $_SET c_gheap \ gheap += sz
  %RET

\ **********
\ * Local Variables
$SFN c_updateRLKey \ [] -> [&metaRef] update and return current local key
  $GET c_dictBuf   $GET c_dictHeap  %ADD \ ldict.buf
  $GET c_dictLHeap                        \ ldict.heap
  %ADD \ {&newLkey}
  %DUP $_SET c_rLKey  %RET

\ implement LOCAL or INPUT. Mostly just updating ldict key and globals.
$ha2 $FN _localImpl $PRE \ {szI:U1 meta:U1}
  #2$h1 \ locals: 0=szI  1=meta  4=&metaRef

  \ assert current function is valid
  .4%FTGL @c_rKey$h2 %DUP $xsl assertTyped .4%FT \ {szI meta fnMetaRef}
    $xsl assertFnLarge \ {szI meta}

  .1%SRLL#1$h1 \ 1=meta {szI}
  %DUP  $xsl assertSzI \ {szI}
  %DUP  .1%SRLL#0$h1 \ cache szI {szI}
  @TY_LOCAL$L1  .1%FTLL#1$h1 %BOR \ update full meta {szI meta}
  %SWP  $xsl joinSzTyMeta  \ {meta}
  $GET c_localOffset \ {meta loff}
  .1%FTLL#0$h1  $xsl alignSzI \ align local offset {meta loff}
  \ c_localOffset = offset + sz
  %DUP  .1%FTLL#0$h1 $xsl szIToSz %ADD  $_SET c_localOffset \ {meta loff}
  $xsl c_updateRLKey \ {meta loff &metaRef}
  %SWP $xsl ldictSet  \ set to localOffset {meta &metaRef}
  .4%SRLL#4$h1 .1%SRLL#1$h1  $xsl dictArgs  .1%FTLL#1$h1 .4%FTLL#4$h1
  $jmpl c_dictSetMeta

\ #<szI> $LOCAL myLocal: declare a local variable of sz
\ This stores the offset and sz for lRef, lGet and lSet to use.
$SFN LOCAL $SMART $xsl assertNoInstant  #0             $L0 $xl _localImpl %RET
$SFN INPUT $SMART $xsl assertNoInstant  @TY_LOCAL_INPUT$L1 $xl _localImpl %RET

$SFN Dict_keyLen   $PRE %INC4 .1%FT  #3F$L1 %BAND %RET            \ {&key} -> {len:U1}
$SFN Dict_keySz    $PRE $xsl Dict_keyLen #5$L0 %ADD $jmpl align4  \ {&key} -> {sz:U1}
$SFN Dict_nextKey  $PRE %DUP $xsl Dict_keySz %ADD %RET            \ {&key} -> {&key}

\ {&key} -> {} recursive function to compile INPUTs
\ Inputs are "compiled" (i.e. a SRLL is compiled) in reverse order.
\ This maps them well to the conventional stack nomenclature.
$FN _compileInputs $PRE
  #1$h1 \ locals 0=&key:APtr
  %DUP  .4%SRLL#0$h1 \ {&key}
  $xsl ldictHeap $reteq \ return if key=ldictHeap
  .4%FTLL#0$h1  $xsl Dict_nextKey  $xl _compileInputs \ get the next key and recurse {}
  .4%FTLL#0$h1  %DUP $xsl isTyped %SWP .4%FT \ {hasTy metaRef}
  %DUP $xsl isLocalInput %SWP \ {hasTy isLocal metaRef}
  $xsl isLocalInput %LAND %LAND %RETZ \ {} check (hasTy and isLocal and isLocalInput)
  .4%FTLL#0$h1  .4%FT  %DUP $xsl _metaRefSzI \ {metaRef szInstr}
  @SZ1$L1 @SRLL$L1 $xl c_instrLitImpl
  %RET

\ - Updates the number of slots for the FN
\ - compiles SRLL for each INPUT in reverse order.
$SFN END_LOCALS  $SMART $xsl assertNoInstant
  $GET c_localOffset #4$L0 $xl align
  #2$L0 %SHR $xsl h1 \ update number of slots
  $xsl ldictBuf $xl _compileInputs %RET

\ **********
\ * [10] Zoa strings and logging zoab
\ See ./harness.md for design reasoning.
\
\ fn $loc <name> |zoa string literal|    : define a zoa string
\
\ fn com [len &raw]               : communicate raw data with harness
\ fn comDone []                   : trigger com done (flush)
\ fn comzStart []                 : zoab start
\ fn comzU4 [U4]                  : com zoab U4 (big endian)
\ fn comzData [len &raw join]     : send zoab data with join bit
\ fn comzArr [len join]           : start an array of len and join
\ fn comzLogStart [lvl extraLen]  : start a log arr of data len exraLen
\ fn print [len &raw]             : print raw data to the user (LOG_USER)
\ fn _printz [&z]                 : print zoab bytes (<=63) to the user
\ fn TODO: not finished

\ |zoa string literal|
\ Creates a zoa string in the heap.
$SFN |
  $SMART $xsl assertNoInstant
  $xsl getHeap
  \ maxLen: (topHeap - heap)
  %DUP $xsl getTopHeap %SWP %SUB
  @D_zoa$L0 %DVFT $jmpl setHeap

$FN c_logAll $PRE \ {&writeStruct len &buf } Write raw data to log output
  $END_LOCALS \ no locals, but used in XW.
  %DRP
  $LOOP l0
    %OVR %OVR
    @D_com$L0 %DVFT
    %NOT $IF %DRP %DRP %RET $END
  $AGAIN l0

$SFN ft4BE $PRE \ {&a -> U4} fetch4 unaligned big-endian
  %DUP%INC %DUP%INC %DUP%INC .1%FT \ {&a &a+1 &a+2                 a@3 }
  %SWP .1%FT #08$L0%SHL %ADD       \ {&a &a+1            (a@2<<8 + a@3)}
  %SWP .1%FT #10$L0%SHL %ADD       \ {&a       (a@1<<16 + a@2<<8 + a@3)}
  %SWP .1%FT #18$L0%SHL %ADD       \ {a@0<<24 + a@1<<16 + a@2<<8 + a@3 }
  %RET


$SFN com $PRE @D_com$L0 %DVSR %RET \ {len &raw} communicate directly
$SFN comDone  @D_comDone$L0 %DVFT %RET

$loc LOG_ZOAB_START  #80$h1 #03$h1
$SFN comzStart  #2$L0 @LOG_ZOAB_START$L4  @D_com$L0 %DVSR %RET
$SFN comzU4     $PRE @D_comZoab$L0 %DVFT %RET \ {U4}
$SFN comzData   $PRE @D_comZoab$L0 %DVSR %RET \ {len &raw join}

$loc TODO #0$h1
$FN comzArr  $PRE \ {len join}
  @SZ1 $LOCAL b0 $END_LOCALS \ b0 is used as an array for com

  \ $IF @ZOAB_JOIN$L1 $ELSE #0$L0 $END %SWP \ join -> joinTy       {joinTy len}
  \ %DUP #40$L1 %LT_U @E_cZoab$L2 $xsl assert \ assert len <= 0x3F {joinTy len}
  \ %BOR $_SET b0
  \ #33$L0 $REF b0 $jmpl com \ send via com

  %DRP \ ignore join TODO
  @ZOAB_ARR$L1 %BOR \ len->arrLen
  @TODO$L2 .1%SR \ store len @TODO
  #1$L0 @TODO$L2 $jmpl com \ send via com

$SFN comzLogStart  $PRE \ {lvl extraLen}  extraLen is in addition to sending the lvl
  $xsl comzStart \ TODO: check return code once it's added
  %INC @FALSE$L0 $xl comzArr
  $jmpl comzU4 \ send lvl

$SFN print  $PRE \ {len &raw}: print data to user
  @LOG_USER$L1 #1$L0 $xsl comzLogStart
  @FALSE$L0 $jmpl comzData

$SFN _printz  $PRE \ {&z}: print zoab bytes to user. (single segment)
  %DUP .1%FT %DUP #40$L1 %LT_U @E_cZoab$L2 $xsl assert \ {len}
  %SWP %INC  $xsl print 
  $jmpl comDone

$assertWsEmpty

\ **********
\ * [11] Fngi compile loop
\ The fngi compile loop is implemented in spore. In addition to the compile loop itself,
\ this section implements several essential parsing functions that can be used
\ in other areas of fngi.
\
\ Globals:
\   global c_compFn: a function reference used to compile individual tokens.
\
\ fn spor <token>                 : compile token as spor.
\ fn ( <...> )                    : ( compiles tokens until )
\ fn $ <token>                    : execute token asInstant
\ fn ret [U4]                     : compile %RET
\ fn _   fn ,   fn ;              : syntax helpers that do nothing.
\
\ fn betweenIncl [value a b -> bool]  : return whether value between [a,b]
\ fn charToInt [c -> U8]          : convert ascii -> hex
\ fn null2 [ -> NULL NULL]        : return two null values
\
\ c_fngi                          : fngi compile loop.
\ fn fngiSingle [ -> ...]         : starting (base) c_compFn
\ fn c_single [asInstant -> ...]  : compile/execute a single token (c_compFn).
\ fn c_fn [metaRef]               : compile a function (small or large)
\ fn execute [metaRef]            : execute a function (small or large)
\ fn parseNumber [ -> value isNum]: parse token as number
\ fn lit [U4]                     : compile literal
\ fn xSzI [metaRef -> szI]        : return szI of the fn
\
\ fn c_charNext [ -> c]           : read next character (WARN: AFTER tokenLen)
\ fn c_charNextEsc [ -> c unkEsc] : read an escapeable character (string).
\ fn c_updateCompFn [newComp -> prevComp] : update c_compFn + ret old
\ fn c_number <number> -> [value isNum]   : parse next token (parseNumber).

$FN betweenIncl $PRE \ {value a b} -> a <= value <= b
  @SZ4 $INPUT b   $END_LOCALS \ {value a}
  %OVR %SWP \ {value value a}
  \ if (value<a) return FALSE;
  %LT_U $IF %DRP @FALSE$L0 %RET $END
  $GET b %SWP \ {b value}
  %LT_U %NOT %RET \ return not(b<value)

$SFN charToInt $PRE \ {c} -> {U8}
  \ '0' - '9'
  %DUP #30$L0 #39$L0 $xl betweenIncl $IF #30$L0 %SUB %RET $END
  \ 'A' - 'Z'
  %DUP #41$L1 #5A$L1 $xl betweenIncl $IF #41$L1 %SUB #A$L0 %ADD %RET $END
  \ 'a' - 'z'
  %DUP #61$L1 #7A$L1 $xl betweenIncl $IF #61$L1 %SUB #A$L0 %ADD %RET $END
  %DRP #FF$L1 %RET

\ {} -> {c}: read next character from AFTER tokenLen.
\ Increments tokenLen. This is destructive to token, use with care.
$SFN c_charNext
  \ IF(GET c_tokenLen >= GET c_tokenSize)
  $GET c_tokenLen  $GET c_tokenSize %GE_U $IF
    $xsl c_readNew  $xsl c_assertNoEof
  $END
  $GET c_tokenBuf  $GET c_tokenLen  %ADD .1%FT
  $GET c_tokenLen %INC  $_SET c_tokenLen %RET

\ {} -> {char unknownEscape} read a character that can be escaped.
$SFN c_readCharEsc
  $xsl c_charNext \ {char}
  %DUP #5C$L1 %NEQ $IF @FALSE$L0 %RET $END \ if(c != '\\') ret;
  \ c is an escape character: \
  %DRP $xsl c_charNext
  %DUP #74$L1 %EQ $IF %DRP #09$L0 @FALSE$L0 %RET $END \ \t: tab
  %DUP #6E$L1 %EQ $IF %DRP #0A$L0 @FALSE$L0 %RET $END \ \n: newline
  %DUP #20$L1 %EQ $IF %DRP #20$L0 @FALSE$L0 %RET $END \ \ : space (raw)
  %DUP #73$L1 %EQ $IF %DRP #20$L0 @FALSE$L0 %RET $END \ \s: space (explicit)
  %DUP #78$L1 %EQ $IF \ \xHH
    \ charToInt(c_charNext) << 8 + charToInt(c_charNext)
    %DRP $xsl c_charNext  $xsl charToInt #8$L0  %SHL
    $xsl c_charNext       $xsl charToInt %ADD
    \ assertNot(dup < inc(0xFF), E_cStr)
    %DUP #FF$L1 %INC %LT_U  @E_cStr$L2  $xsl assertNot
    @FALSE$L0 %RET
  $END
  @TRUE$L0 %RET \ just return the character as-is but unknownEscape=true

$FN c_parseNumber \ {} -> {value isNumber}
  @SZ4 $LOCAL value
  @SZ1 $LOCAL i
  @SZ1 $LOCAL base
  $END_LOCALS
  #A$L0 $_SET base
  #0$L0 $_SET value
  #0$L0 $_SET i

  \ Get correct base
  $GET c_tokenBuf .1%FT #30$L0 %EQ $IF \ if c0 == '0' {}
    $GET c_tokenBuf %INC .1%FT %DUP \ {c1 c1} if there was a char

    \ First handle 0c: character literal
    #63$L1 %EQ  $IF \ if .tokenBuf@1=='c' {c1}
      \ set tokenLen=2 to treat anything after 0c as next character.
      %DRP #2$L0 $_SET c_tokenLen
      $xsl c_readCharEsc  @E_cUnknownEsc$L2 $xsl assertNot
      @TRUE$L0 %RET
    $END

    %DUP \ {c1 c1}
    #62$L1 %EQ  $IF \ if .tokenBuf@1=='b' {c1}
      #2$L0  $_SET base  #2$L0 $_SET i
    $END \ {c1}
    #78$L1 %EQ  $IF \ if .tokenBuf@1=='x' {}
      #10$L0 $_SET base  #2$L0 $_SET i
    $END
  $END

  $LOOP l0
    $GET i  $GET c_tokenLen $BREAK_EQ b0
    $GET c_tokenBuf $GET i %ADD .1%FT \ {c}
    $xsl charToInt \ {v}
    \ return {0 0} if not integer character
    %DUP $GET base %GE_U $IF  @FALSE$L0 %RET  $END

    $GET base  $GET value %MUL \ {base * value}
    %ADD $_SET value \ value = v + value*10
    $GET i %INC $_SET i \ i += 1
  $AGAIN l0  $END_BREAK b0

  $GET i %NOT $IF  #0$L0 @FALSE$L0 %RET  $END \ no token
  $GET value @TRUE$L0 %RET

$assertWsEmpty

$SFN lit  $PRE \ {U4} compile literal
  %DUP #40$L1 %LT_U        $IF  $jmpl L0  $END
  %DUP #FF$L1 %INC %LT_U   $IF  $jmpl L1  $END
  %DUP #FFFF$L2 %INC %LT_U $IF  $jmpl L2  $END
                                $jmpl L4

\ {asInstant value:U4} -> {?instantVal}: compile proper sized literal
\ if asInstant=true the value is left on the stack.
$SFN c_lit
  %SWP %NOT %RETZ \ if instant, leave on stack
  $jmpl lit

$SFN xSzI $PRE \ {metaRef} -> {szI}: return the size requirement of the X instr
   $xsl isCurMod $IF  @SZ2$L1 %RET  $END  @SZ4$L1 %RET

$SFN c_fn $PRE \ {metaRef}: compile a function
  %DUP $xsl assertFn \ {metaRef}
  %DUP $xsl xSzI     \ {metaRef szLit}
  %OVR $xsl isFnLarge  $IF @XL$L1 $ELSE @XSL$L1 $END \ {metaRef instrSzI instr}
  %OVR %SWP \ {metaRef instrSzI litSzI instr} instr & lit are same sz
  $xl c_instrLitImpl %RET

$SFN execute \ {metaRef} -> {...}: execute a function
  %DUP $xsl toRef %SWP \ {ref metaRef}
  $xsl isFnLarge  $IF .4%XW %RET $END
  .4%JMPW

$SFN _compConstant $PRE \ {asInstant} -> {asInstant metaRefFn[nullable]}
  $xl c_parseNumber \ {asInstant value isNumber}
  $IF \ {asInstant value}
    $xsl c_lit $jmpl null2
  $END %DRP \ {asInstant}
  $xsl c_isEof $IF  $jmpl null2  $END

  \ Handle local dictionary. Only constants allowed here.
  $xsl ldictArgs  @D_rdict$L0 %DVFT %DUP  $IF
    %DUP $xsl isTyped  @E_cNotFnOrConst$L2 $xsl assertNot
    .4%FT $xsl c_lit  $jmpl null2
  $END %DRP

  $xsl dictArgs  @D_rdict$L0 %DVFT \ {asInstant &metaRef}

  \ Constant
  %DUP  $xsl isTyped %NOT $IF
    .4%FT $xsl c_lit $jmpl null2
  $END

  \ Must be a function
  .4%FT %DUP $jmpl assertFn \ {asInstant metaRef}

$assertWsEmpty

\ {asInstant metaRefFn} -> {metaRefFn} check fn type and update asInstant
$SFN _compFnAsInstant $PRE
  %DUP $xsl isFnNormal $IF
    %SWP $IF @TY_FN_INSTANT$L1 $ELSE #0$L0 $END \ {metaRefFn newFnTy}
    $jmpl metaSet
  $END \ {asInstant metaRefFn}
  %SWP %DRP %RET \ not normal = leave alone

#0 @SZ4 $GLOBAL c_compFn \ must be a large fn.

$SFN c_updateCompFn $PRE \ {newCompFnMetaRef} -> {prevCompFnRef}
  $xsl toRef
  $GET c_compFn %SWP $_SET c_compFn %RET


\ {asInstant} -> {}: compile a single token.
\ This is the primary function that all compilation steps (besides spor
\ compilation) reduce to.
$FN c_single $PRE
  @SZA $LOCAL metaRef $END_LOCALS

  \ Handle constants
  $xsl _compConstant \ {asInstant metaRefFn[nullable]}
  \ If metaRefFn=null then already compiled, drop asInstant and metaRefFn
  %DUP %NOT $IF  %DRP %DRP %RET  $END \ {asInstant metaRefFn}

  %SWP %OVR  \ {metaRefFn asInstant metaRefFn}
  $xsl isFnInstant $IF \ {metaRef asInstant}
    @E_cReqInstant$L2 $xsl assert \ assert asInstant
  $ELSE
    \ if asInstant=TRUE and ty was SMART, this will make it SMART_I, else INSTANT
    \ if asInstant=FALSE this will not change metaRef
    $IF @TY_FN_INSTANT$L1  $xsl metaSet $END
  $END \ {metaRef}

  \ if pre, recursively call fngiSingle (compile next token first)
  %DUP $xsl isFnPre $IF
    $_SET metaRef  $GET c_compFn .4%XW  $GET metaRef
  $END

  %DUP $xsl isFnSmart $IF
    @FALSE$L0 %SWP $jmpl execute \ smart: not instant. asInstant=FALSE
  $END %DUP $xsl isFnSmartI $IF
    @TRUE$L0 %SWP $jmpl execute \ smart: instant. asInstant=TRUE
  $END %DUP $xsl isFnInstant $IF
    $jmpl execute \ regular instant. Just call immediately.
  $END $jmpl c_fn \ otherwise compile the function.

$FN fngiSingle \ base c_compFn for fngi tokens.
  $END_LOCALS \ not really any locals (but this is used as a pointer)
  $xsl c_scan $GET c_tokenLen %RETZ
  @FALSE$L0 $xl c_single %RET

@fngiSingle $c_updateCompFn ^DRP

$SFN c_number $xsl c_scan $xl c_parseNumber %RET \ compile next token as number.

$SFN (  $SMART%DRP  \ parens ()
  $xsl c_assertToken
  $xsl c_peekChr #29$L0 %EQ $IF  $jmpl c_scan  $END \ return if we hit ")"
  $LOOP l0
    $GET c_compFn .4%XW
    $xsl c_assertToken
    $xsl c_peekChr #29$L0 %EQ $IF  $jmpl c_scan  $END \ return if we hit ")"
  $AGAIN l0

$FN _spor
  @SZA $LOCAL compFn $END_LOCALS
  @_spor$L2  $xsl c_updateCompFn $_SET compFn \ update c_compFn and cache
  $xsl c_scanNoEof
  @D_comp$L0  %DVFT \ compile next token as spor asm
  $GET compFn $_SET c_compFn %RET

$SFN spor $SMART $xsl assertNoInstant $xl _spor %RET \ compile as assembly

$FN _instant \ used in $ to make next token/s instant.
  @SZA $LOCAL compFn $END_LOCALS
  @_instant$L2  $xsl c_updateCompFn $_SET compFn \ update c_compFn and cache
  $xsl c_scanNoEof
  @TRUE$L0 $xl c_single  \ compile next token as INSTANT
  $GET compFn $_SET c_compFn %RET

$SFN $ $SMART $xsl assertNoInstant $xl _instant %RET \ make instant

$FN _comment \ used in \ to make next token ignored (comment)
  @SZA $LOCAL compFn $END_LOCALS
  @_comment$L2  $xsl c_updateCompFn $_SET compFn
  $xsl c_scanNoEof
  \ Execute an open paren, else ignore
  $GET c_tokenBuf .1%FT #28$L0 %EQ $IF @TRUE$L0 $xl c_single $END
  $GET compFn $_SET c_compFn %RET \ scan and ignore

\ {-> c} peek at the char after current token.
$SFN c_peekNoScan
  $GET c_tokenLen  $GET c_tokenSize %GE_U $IF
    \ ensure a char exists, if not return
    $xsl c_read $GET c_tokenSize $GET c_tokenLen %SUB %RETZ
  $END
  $GET c_tokenBuf $GET c_tokenLen %ADD .1%FT %RET

\ Comment, of which there are three forms.
\    \        : a line comment
\    \foo     : an inline comment, commenting out one token
\    \( ... ) : a block comment
$SFN \ $SMART %DRP
  \ Line comment if '\' is followed by space or newline
  $xsl c_peekNoScan %DUP #20$L0 %EQ %SWP #A$L0 %EQ %LOR
  $IF $jmpl c_scanEol $END   $xl _comment %RET \ else token comment

$SFN ret $PRE $SMART $xsl assertNoInstant @RET $c1 %RET \ ret 4, or just ret;

\ These do nothing and are used for more readable code.
$SFN _ $SMART%DRP %RET
$SFN , $SMART%DRP %RET
$SFN ; $SMART%DRP %RET

$SFN c_fngi \ fngi compile loop
  $LOOP l0
    $GET c_tokenSize %RETZ \ exit on EOF
    $GET c_compFn .4%XW
  $AGAIN l0

$c_dictDump
