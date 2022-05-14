\  |00322998|L0027|@158D {0...      72C4|        1} [JMPL  U2] +++ "a" ((_setImpl   0x1527)) 

\ This file bootstraps spor from the native (i.e. C) implementation into
\ a more full-featured language with helpful macros.
\
\ Note: this file requires the compiler to pass the current heap value on the
\ stack so that it can define it's first function (_h), which it then
\ uses to define the other functions.
\
\ # Table of Contents
\ Search for these headings to find information
\
\ [0] Documented Core Structs and Fn Types
\ [1] Instructions: contains definition of spore assembly instructions and
\    documentation.
\    [1.a] Operations: Special
\    [1.b] Operations: One Inp -> One Out
\    [1.c] Operations: Two Inp -> One Out
\    [1.d] Small Literal [0x40 - 0x80)
\    [1.e] Sizes: SZ1, SZ2, SZ4, SZA
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
\ [8] xx, jmpl, Scanning and Alignment
\ [9] globals and locals
\ [10] Zoa strings and logging zoab
\ [11] Fngi compile loop
\
\ **********
\ * [0] Documented Core Structs and Fn Types
\
\ There are a few "core structs" and associated "core function types". These allow
\ cohesion and interoperability among various APIs in spor/fngi.
\
\ Structs:
\   struct Slc [ref: &uty; len: U2]          : a slice of data
\   struct Buf [ref: &uty; len: U2; cap: U2] : buffer (len and total capacity)
\   struct Sll [next: AP]                    : singly linked list
\
\ Fn Types:
\   fn wlkr [ref: &uty -> ref: AP, done:Bool]: function walker, used with wlk
\   fn wlk  [ref: &uty, w: wlkr -> AP]       : walk a walker. Call until done,
\   fn sllwlkr [ref: &Sll -> done:Bool]      : Sll version of walker.
\   fn sllwlk  [ref: &Sll, w:sllwlkr -> AP]  : walk an Sll walker, ret done ref.
\
\ **********
\ * [1] Instructions: these are constants that can be used directly by: % ^
\ Spor uses 8 bit instructions with the following bit layout (S=size bit):
\   00XX XXXX: operation
\   01XX XXXX: small literal [0x00 - 0x3F]
\   10SS XXXX: jmp
\   11SS XXXX: mem

\ # [1.a] Operations: Special
#00 #0=NOP   \ { -> }     no operation
#01 #0=RETZ  \ Return if zero
#02 #0=RET   \ Return
#03 #0=SWP   \ {l r -> r l} swap
#04 #0=DRP   \ {l -> }    drop
#05 #0=OVR   \ {l r -> }  drop 2
#06 #0=DUP   \ {l -> l l} duplicate
#07 #0=DUPN  \ {l -> l l==0} DUP then NOT
#08 #0=DVFT  \ Device Operation Fetch
#09 #0=DVSR  \ Device Operation Store
#0A #0=RGFT  \ {-> v}  Register Fetch
#0B #0=RGSR  \ {v}     Register Store

\ # [1.b] Operations: One Inp -> One Out
#10 #0=INC   \ {l+1}  increment 1
#11 #0=INC2  \ {l+2}  increment 2
#12 #0=INC4  \ {l+4}  increment 4
#13 #0=DEC   \ {l-4}  decrement 1
#14 #0=INV   \ {~l}   Bitwise Inversion
#15 #0=NEG   \ {-l}   Negate (2's compliment)
#16 #0=NOT   \ {l==0} Logical NOT
#17 #0=CI1   \ {ISz}  Convert I1 to ISz
#18 #0=CI2   \ {ISz}  Convert I2 to ISz
\ future: leading 0's, trailing 0's, count of 1's
\ Some single-arg extension commands might be:
\ (7) floating point abs, negative, ceil, floor, trunc, nearest, and sqrt
\ (1) i -> f conversion
\ (1) f -> i conversion

\ # [1.c] Operations: Two Inp -> One Out
#20 #0=ADD   \ {l +  r } add
#21 #0=SUB   \ {l -  r } subtract
#22 #0=MOD   \ {l %  r } integer modulo (remainder)
#23 #0=SHL   \ {l << r } bit shift left
#24 #0=SHR   \ {l >> r } bit shift right
#25 #0=MSK  \ {l &  r } bitwise and
#26 #0=JN    \ {l |  r } bitwise or
#27 #0=XOR   \ {l ^  r } bitwise xor
#28 #0=AND  \ {l && r } logical and
#29 #0=OR    \ {l || r } logical or
#2A #0=EQ    \ {l == r } equal
#2B #0=NEQ   \ {l != r } not equal
#2C #0=GE_U  \ {l >= r } unsigned greater than or equal
#2D #0=LT_U  \ {l <  r } unsigned less than
#2E #0=GE_S  \ {l >= r } signed greater than or equal
#2F #0=LT_S  \ {l <  r } signed less than

#30 #0=MUL   \ {l *  r } multiplication
#31 #0=DIV_U \ {l / r  } unsigned division
#32 #0=DIV_S \ {l / r  } signed division
\ Double-arg extension commands might be:
\ floating point: add,sub,mul,div,ge,lt

\ # [1.d] Small Literal [0x40 - 0x80)
#40 #0=SLIT

\ # [1.e] Sizes (note: SZA defined after `select`)
#00 #0=SZ1
#10 #0=SZ2
#20 #0=SZ4

\ # [1.f] Jmp
\
\ Jumps can be to either a literal (L) or to an item on the working stack (W).
\ - XL means "execute large" and is a call to a function with locals.
\ - XS is an "execute small" and means that the function has no local stack.
\ - XLL/XLW will execute a function that has a local stack. The size of the
\   local stack is stored in the first bye (shifted by APO2) at the function's
\   address, which  are loaded by the execute instr and stored in the highest
\   byte in the  callstack (which RET uses to shrink the local stack on return).

\   Jmp      Description
#80 #0=JMPL  \ Jmp to Literal
#81 #0=JMPW  \ Jmp to WS
#82 #0=JZL   \ Jmp to Literal if store==0
#83 #0=JTBL  \ Jump to Table index using size=Literal
#84 #0=XLL   \ Execute Literal (mPtr)
#85 #0=XLW   \ Execute WS (aPtr)
#86 #0=XSL   \ Execute Small Literal (no LS update)
#87 #0=XSW   \ Execute Small WS (no LS update)

\ JZL and JMPL for SZ=1
\ For SZ=1 they jump to the 1 byte signed offset from the location
\ of the operation (NOT the location of the literal).

\ # [1.g] Mem|Store              |Description
#C0 #0=FT    \ {addr} -> {value}  |FeTch value from addr
#C1 #0=FTO   \ {addr} -> {value}  |FeTch value from addr + U1 literal offset
#C2 #0=FTLL  \ {} -> {local}      |FeTch from LP + U1 literal offset
#C3 #0=FTGL  \ {} -> {global}     |FeTch from GB + U2 literal offset
#C4 #0=SR    \ {value addr} -> {} |Store value at addr
#C5 #0=SRO   \ {value addr} -> {} |Store value at addr + U1 literal offset
#C6 #0=SRLL  \ {value} -> {}      |StoRe value at LP + U1 literal offset
#C7 #0=SRGL  \ {value} -> {}      |StoRe value at GB + U2 literal offset
#C8 #0=LIT   \ {} -> {literal}    |Literal (U1, U2 or U4)

\ Common instr+szs
@SZ2 @XSL  ^JN   #0=XSL2
@SZ2 @JMPL ^JN   #0=JMPL2

\ **********
\ * [2] Registers and Device Operations: RGXX|DVXX w/ 1 byte literal

\ The RG 1 byte literal has the following byte format. Note: FT will return the
\ register value + offset, SR will store the value + offset in the register.
\   1OOO OOOO: (R_LP) local stack pointer with 7bit offset (O)
\   0XXX XXXR: register. X is currently undefined.
#80 #0=R_LP \ local stack pointer
#00 #0=R_EP \ execution pointer, SR will panic
#01 #0=R_GB \ global base pointer

\ The DV 1 byte literal select the operation
#00 #0=D_read   \ read from src, filling up tokenBuf
#01 #0=D_scan   \ FT: scan next word (tokenBuf)  SR: line comment
#02 #0=D_dict   \ [(SR-only)value &dict] FT=get SR=set dict key=tokenBuf
#03 #0=D_dictK  \ [&dict] FT=get reference to val  SR=forget including key
#05 #0=D_comp   \ compile (assemble) the token in tokenBuf
#06 #0=D_assert \ error if != 0
#07 #0=D_wslen  \ get working stack length (in slots)
#08 #0=D_cslen  \ get call stack lengh (in slots)
\ {-> err} D_xCatch executes large function from WS but catches a panic.
\ The errCode is returned (or 0 if no error).
\ Note: caches and restores ep, call stack and local stack state. Working stack
\ is cleared besides the returned err.
#09 #0=D_xCatch
#0A #0=D_memSet  \ {dst v len} "dst = v [len]". FT: memset, SR: memmove
#0B #0=D_memCmp  \ {&a &b len} -> I32: <0 if a<b; >0 if a>b; 0 if a==b
#0C #0=D_com     \ {&msg len} -> {ioResult}: write to debug stream
#0D #0=D_zoa     \ {} -> {} parse zoa to heap
#0E #0=D_dictDump\ {<dict-args> [&entry]} dump a dictionary FT=entry SR=full
#0F #0=D_comZoab \ FT{U4}  SR{len &data join}
#10 #0=D_comDone \ signifies end of one com. On linux this is a flush.

\ **********
\ * [3] Constants
#0  #0=FALSE
#1  #0=TRUE
#4  #0=ASIZE \ size of an absolute pointer
@ASIZE ^INC #0=DICT_OLEN \ dict name len offset
#30 #0=SZ_MASK \ size bit mask (for instr and meta)

\ * [3.a] Dict Ty Bits (meta byte):  TTXX XXXX T=TY_MASK
#C0 #0=META_TY_MASK \ upper three bits determine type
#00 #0=TY_CONST   \ constant, the value is used directly
#40 #0=TY_FN      \ function, can be called and has an fnMeta
#80 #0=TY_VAR     \ variable (local, global, struct field, etc). Has varMeta
#C0 #0=TY_DICT    \ a "dictionary" type which has dictMeta.
#FF_FFFF #0=REF_MASK
#FF_0000 #0=MOD_MASK

\ FN meta bits [TTPL FF--] L=locals F=fnTy P=pre
#20 #0=TY_FN_PRE     \ Compile next token first. Creates pre-order arguments.
#10 #0=TY_FN_LARGE   \ large, has locals (called with XLL or XLW)

#0C #0=TY_FN_TY_MASK \ 4 function types
#00 #0=TY_FN_NORMAL  \ Normally compiled, can use $ to make NOW
#04 #0=TY_FN_NOW     \ Required to be run as NOW (must use $)
#08 #0=TY_FN_SYN     \ (syntactical) always run now (knowing asNow)
#0C #0=TY_FN_INLINE  \ Inline function, copies bytes when compiled.

@TY_FN@TY_FN_PRE^JN #0=_FP \ Meta for function with args

\ Local meta bits  [TTTI RRSS] I=input R=ref S=szI
\ Local meta bits  [TTSS RR-I] I=input R=ref S=szI
\ Global meta bits [TTSS RR--]         R=ref S=szI
#01 #0=TY_VAR_INPUT
#0C #0=TY_VAR_REF

\ * [3.b] Zoab
#C0 #0=ZOAB_TY   \ bitmask: all types
#80 #0=ZOAB_JOIN \ bitmask: join type
#40 #0=ZOAB_ARR  \ bitmask: arr type
#C0 #0=ZOAB_PTR  \ equality: next 4 bytes are a pointer.

\ * [3.c] Log Levels
#00 #0=LOG_SILENT

\ Log Levels
#10 #0=LOG_USER
#1F #0=LOG_TRACE
#17 #0=LOG_DEBUG
#13 #0=LOG_INFO
#11 #0=LOG_WARN
#10 #0=LOG_CRIT

\ Language Level (builtin) Logs
#27 #0=LOG_INSTR
#23 #0=LOG_EXECUTE
#21 #0=LOG_ASM
#20 #0=LOG_COMPILER

\ * [3.d] Errors
\ [E000 - E100): built-in errors.
\  E100: device-specific hardware errors
\ [E200-E800): reserved
\  E800+: application errors
\  AXXX_EXXX: test case assertion error.

#0     #0=E_ok      \ no error
#E000  #0=E_general \ general errors [E000-E010)
#E010  #0=E_io      \ IO error class
#E0A0  #0=E_asm     \ assembly error class (cause in asm).
#E0C0  #0=E_comp    \ compiler error class (cause in comp).
#A000  #0=E_test    \ [AXXX] (assert) test case error.

#E001  #0=E_intern  \ internal (undefined) error
#E002  #0=E_undef   \ undefined error
#E003  #0=E_unreach \ unreachable code
#E004  #0=E_todo    \ executed incomplete (to do) code
#E005  #0=E_wsEmpty \ the WS was expected empty
#E006  #0=E_unimpl  \ unimplemented error

#E0A1  #0=E_null    \ null access
#E0A2  #0=E_oob     \ out of bounds access
#E0A3  #0=E_stkUnd  \ Stack underflow
#E0A4  #0=E_stkOvr  \ Stack overflow
#E0A5  #0=E_align2  \ access off 2byte allign
#E0A6  #0=E_align4  \ access off 4byte align
#E0A7  #0=E_divZero \ divide by zero

#E0C1  #0=E_cInstr  \ invalid instr
#E0C2  #0=E_cToken  \ token invalid
#E0C3  #0=E_cTLen   \ token invalid
#E0C4  #0=E_cKey    \ key already exists
#E0C5  #0=E_cNoKey  \ dict key not found
#E0C6  #0=E_cHex    \ non-hex number
#E0C7  #0=E_cSz     \ invalid Sz selected
#E0C8  #0=E_cSzPtr  \ invalid Sz for aptr
#E0C9  #0=E_cRet    \ invalid RET
#E0CA  #0=E_cDblSr  \ Double store
#E0CB  #0=E_cDevOp  \ device op not impl
#E0CC  #0=E_DictOvr \ dict overflow
#E0CD  #0=E_cXHasL  \ small-execute to fn w/locals
#E0CE  #0=E_cXNoL   \ large-execute to fn wo/locals
#E0CF  #0=E_cErr    \ D_assert err code invalid
#E0D0  #0=E_cKeyLen \ Key len too large
#E0D1  #0=E_cReg    \ Register error
#E0D2  #0=E_cStr    \ Str invalid

#E0E0  #0=E_cNotGlobal \ using a non-global as global
#E0E1  #0=E_cIsX       \ using an XS for an X
#E0E2  #0=E_cIsXS      \ using an X for an XS
#E0E3  #0=E_cJmpL1     \ JMP1 over too much space
#E0E4  #0=E_cNotFn
#E0E5  #0=E_cNotFnLarge
#E0E6  #0=E_cMod       \ different modules
#E0E7  #0=E_cLSz       \ literal sz
#E0E9  #0=E_cNotType
#E0EA  #0=E_cNotLocal
#E0EB  #0=E_cNotVar
#E0EC  #0=E_cNotFnOrConst
#E0ED  #0=E_eof
#E0EE  #0=E_cUnclosed   \ unclosed paren/brace/etc
#E0EF  #0=E_cReqNow     \ fn is NOW but no '$' used
#E0EF  #0=E_cNoNow      \ fn is SYN and requires no $ used.
#E0F0  #0=E_cUnknownEsc \ unknown character escape
#E0F1  #0=E_cZoab       \ Zoab invalid
#E0F2  #0=E_cNeedToken  \ token not present
#E0F3  #0=E_cNeedNumber \ number not present
#E0F4  #0=E_cBadRefs    \ too many de/refs
#E0F5  #0=E_cRefEq      \ .&var = not allowed.
#E0F6  #0=E_cInlineLarge \ inline fn is large
#E0F7  #0=E_cColon      \ Expect : after function
#E0F8  #0=E_cFnSyn      \ Invalid use of SYN function

#E0B0  #0=E_iBlock      \ invalid block index
#E0B1  #0=E_ptrBlk      \ invalid block ptr
#E0B2  #0=E_aaPo2       \ invalid po2


#00  #0=ERR_DATA_NONE
#01  #0=ERR_DATA_INT1
#02  #0=ERR_DATA_DATA1
#03  #0=ERR_DATA_INT2
#04  #0=ERR_DATA_DATA2

\ **********
\ * [4] Globals: many of these must be the same as in spor.c

@TY_FN=_h  .A%FTGL #4.2,  %RET \ { -> heap} get the heap

$_h @_FP=select \ {a b s -> a|b} a if s else b
  .1%JZL #3.1, %DRP %RET \ if(s) ret a
  %SWP %DRP %RET         \ ret b
@INC2 @INC4  @ASIZE #2 ^EQ  $select #0=INCA
@SZ2 @SZ4    @ASIZE #2 ^EQ  $select #0=SZA
#1   #2      @ASIZE #2 ^EQ  $select #0=APO2

#0000_0004 @TY_VAR@SZA^JN=heap
#0000_0008 @TY_VAR@SZA^JN=topHeap
#0000_000C @TY_VAR@SZA^JN=topMem
#0000_0010 @TY_VAR@SZ2^JN=err
#0000_0012 @TY_VAR@SZ2^JN=c_state  \ U2
\ #0000_0014 #0=_unimpl1
\ #0000_0018 #0=_unimpl2
#0000_001C @TY_VAR@SZ2^JN=sysLogLvl
#0000_001E @TY_VAR@SZ2^JN=usrLogLvl

\ Dictionary (Kernel and Local) Structs
#0000_0020 @TY_VAR@SZA^JN=c_kdictRef   \ U4
#0000_0024 @TY_VAR@SZ2^JN=c_kdictLen   \ U2
#0000_0026 @TY_VAR@SZ2^JN=c_kdictCap   \ U2
#0000_0028 @TY_VAR@SZA^JN=c_ldictRef   \ U4
#0000_002C @TY_VAR@SZ2^JN=c_ldictLen   \ U2
#0000_002E @TY_VAR@SZ2^JN=c_ldictCap   \ U2

\ TokenBuf Struct
#0000_0030 @TY_VAR@SZA^JN=c_tokenBuf   \ [APtr] TokenBuf struct
#0000_0034 @TY_VAR@SZ2^JN=c_tokenLen   \ [U2] length of token
#0000_0036 @TY_VAR@SZ2^JN=c_tokenSize  \ [U2] characters buffered
#0000_0038 @TY_VAR@SZ1^JN=c_tokenGroup \ [U1] token group

\ Global Error Variables
#0000_003C @TY_VAR@SZ1^JN=c_errValTy     \ [U1] + 3align
#0000_0040 @TY_VAR@SZ2^JN=c_dataASz      \ [U2]
#0000_0042 @TY_VAR@SZ2^JN=c_dataBSz      \ [U2]
#0000_0044 @TY_VAR@SZA^JN=c_errVal1      \ [U4]
#0000_0048 @TY_VAR@SZA^JN=c_errVal2      \ [U4]
#0000_004C @TY_VAR@SZA^JN=c_msg          \ [APtr]

\ Block allocator (12 bytes, see fngi.fn)
#0000_0050 #0=BA_kernel

\ Global Compiler Variables
#0000_005C @TY_VAR@SZA^JN=c_gkey         \ [U4] current kdict &key
#0000_0060 @TY_VAR@SZA^JN=c_lkey         \ [U4] current ldict &key
#0000_0064 @TY_VAR@SZA^JN=c_gheap        \ [U4] global heap
#0000_0068 @TY_VAR@SZ2^JN=c_localOffset  \ [U2] Local Offset (for local var setup)
#0000_006C @TY_VAR@SZA^JN=c_compFn       \ [UA] current compiler function

\ **********
\ * [5] Bootstrap Macros
\ These macros must be defined in pure ASM. They build on eachother
\ to make the syntax much more readable.
\
\   fn select [a b s -> a|b]      : a if s else b
\   fn h1 [U1] -> []              : push 1 byte to heap
\   fn h2 [U2] -> []              : push 2 byte to heap
\   fn h4 [U4] -> []              : push 4 byte to heap
\   fn L0 [U1] -> []              : compile a small literal [#0 - #3F]
\   fn $kdictGet <key>  [-> U4]   : get kernel dictionary key's value
\   fn $kdictGetK <key> [-> APtr] : get kernel dictionary &key (reference)
\   fn $loc <token> []            : set token to the current heap location
\
\ Assertions: these panic with the supplied errCode if cond is not met.
\   assert [cond errCode]
\   assertNot [cond errCode]
\
\ Test Assertions: these panic with E_test if the cond is not met.
\   tAssert, tAssertNot, tAssertEq


$_h @_FP=h1  \ h1: {val:1} push 1bytes from stack to heap
  .A%FTGL @heap.2, .1%SR    \ store 1 byte value at heap
  .A%FTGL @heap.2,  %INC  .A%SRGL @heap.2, \ heap=heap+1
  %RET

$_h @_FP=L0   \ L0: compile a small literal (unchecked)
  .1%LIT  #3F, %MSK \ truncated to bottom 6 bits
  .1%LIT  @SLIT,
  %JN     .2%JMPL @h1, \ made into SLIT instr and stored.

$_h @_FP=srBE2 \ {val addr} store a value at addr encoded BE2 (big-endian 2)
  %OVR #48.1, %SHR \ {val addr val>>8} note: #48 is SLIT(8)
  %OVR .1%SR       \ store upper byte {val addr}
  %INC .1%SR %RET  \ {} store lower byte

$_h @_FP=srBE4 \ {val addr} store a value at addr encoded BE4 (big-endian 4)
  %OVR #50.1, %SHR \ {val addr val>>16} note: #50 is SLIT(16)
  %OVR .2%XSL @srBE2,   \ handle the large bytes (1 & 2)
  %INC2 .2%JMPL @srBE2, \ small bytes            (3 & 4)

$_h @_FP=h2  \ h2: {val:2} push 2bytes from stack to heap
  .A%FTGL @heap.2, .2%XSL @srBE2.2, \ store value at heap
  .A%FTGL @heap.2, \ {heap}
  %INC2   .A%SRGL   @heap.2,   \ heap=heap+2
  %RET

$_h @_FP=h4  \ h4: {val:4} push 4bytes from stack to heap
  .A%FTGL @heap.2, .2%XSL @srBE4.2, \ store value at heap
  .A%FTGL @heap.2,  %INC4     .A%SRGL @heap.2, \ heap=heap+4
  %RET

$_h @TY_FN=kdictArgs \ [ -> &kdict] args for dict.
  %RGFT@R_GB$h1 .2%LIT @c_kdictRef$h2 %ADD %RET

$_h @TY_FN=_dict
  @D_scan$L0  %DVFT .2%JMPL @kdictArgs$h2

\ TODO: remove
$_h @TY_FN=kdictSet \ kdictSet <token> {meta}: Set "global" dictionary to next token.
  .2%XSL @_dict$h2  @D_dict$L0   %DVSR  \ set dict key
  .A%FTGL @c_kdictRef$h2  .2%FTGL @c_kdictLen$h2
    %ADD  .A%SRGL @c_ldictRef$h2 \ ldictRef = kdictRef + kdictLen
  #0$L0 .2%SRGL @c_ldictLen$h2   \ ldictLen = 0
  .2%FTGL @c_kdictCap$h2  .2%FTGL @c_kdictLen$h2
    %SUB  .2%SRGL @c_ldictCap$h2 \ ldictCap = kdictCap - kdictLen
  #0$L0 .2%SRGL @c_localOffset$h2 %RET

$_h @TY_FN=kdictGet \ kdictGet: Get the value of the next token.
  .2%XSL @_dict$h2   @D_dict$L0   %DVFT  %RET

$_h @TY_FN=kdictGetK \ kdictGetK: Get the &key of the next token.
  .2%XSL @_dict$h2   @D_dictK$L0   %DVFT  %RET

$_h @TY_FN=loc \ {meta} $loc <name>: define location
  .A%FTGL @heap$h2 %SWP \ {heap meta}
  .2%XSL @_dict $h2  @D_dict$L0   %DVSR  \ set dict key
  .A%FTGL @c_kdictRef$h2  .2%FTGL @c_kdictLen$h2
    %ADD  .A%SRGL @c_ldictRef$h2 \ ldictRef = kdictRef + kdictLen
  #0$L0 .2%SRGL @c_ldictLen$h2   \ ldictLen = 0
  .2%FTGL @c_kdictCap$h2  .2%FTGL @c_kdictLen$h2
    %SUB  .2%SRGL @c_ldictCap$h2 \ ldictCap = kdictCap - kdictLen
  #0$L0 .2%SRGL @c_localOffset$h2 %RET


\ Assert checks a condition or panics with an error
\ ex: <some check> @E_myError assert
@_FP@TY_FN_INLINE^JN $loc assert #2$h1 @D_assert$L0 %DVFT %RET  \ note: inline function
@_FP$loc assertNoNow  .2%LIT @E_cNoNow$h2 \ [fallthrough]
@_FP$loc assertNot    %SWP %NOT %SWP  @D_assert$L0 %DVFT %RET
@_FP$loc tAssert      .2%LIT @E_test$h2 @D_assert$L0 %DVFT %RET
@_FP$loc tAssertNot   %NOT .2%JMPL @tAssert,
@_FP$loc assertLt128   .1%LIT #80$h1 %LT_U  .2%LIT @E_cJmpL1$h2 @D_assert$L0 %DVFT %RET
@_FP$loc tAssertEq \ {a b}
   @ERR_DATA_INT2$L0  .1%SRGL @c_errValTy$h2
        .A%SRGL @c_errVal2$h2 \ b {a}
   %DUP .A%SRGL @c_errVal1$h2 \ a {a}
        .A%FTGL @c_errVal2$h2 \ {a b}
  %EQ   .2%JMPL @tAssert,

\ **********
\ * [6] Core functions and macros
\ These are the bread and butter of spor assembly needed to create the fngi compiler.
\ They define and call functions, check the type of dictionary entries,
\ provide local dictionary support, etc.
\
\ fn $_xsl <token> / $_jmp <token>: compile an XSL or JMPL to key
\ fn L1 / L2 / L4 / LA  [U] -> [] : compile 1 / 2 / 4 / ASIZE byte literal.
\ fn xCatch [... &fn]             : execute a large function and catch error.
\ fn retz  [a]                    : return immediately if not a  (     %RETZ)
\ fn retif [a]                    : return immediately if a      (%NOT %RETZ)
\ fn reteq [a b]                  : return immediately if a == b (%NEQ %RETZ)
\
\ fn FN <name>                    : declare a function
\ fn PRE                          : make function "pre" (run after next token)
\ fn SYN                          : make function "syn" (syntax, always now)
\ fn NOW                          : require function to be "now" (use $)
\ fn LARGE                        : make function large (has locals)
\
\ fn $c1 [instr]                  : NOW to compile instr when executed
\ fn keyMeta [&key] -> [meta]     : INLINE get meta of key
\ fn isTyConst   [&key] -> [U]    : &key has a type (is not const)
\ fn isTyFn      [&key] -> [U1]   : &key is a fn
\ fn isFnLarge   [&key] -> [U1]   : &key is a large fn (has locals)
\ fn isFnPre     ...
\ fn isFnNormal  ...
\ fn isFnNow     ...
\ fn isFnSyn     ...
\ fn isTyVar [&key -> U1]      : is a local offset
\ fn isTyVarInput [&key -> U1] : is a local offset input
\
\ fn toMod [ref -> mod]           : get the "module" (upper byte of U4)
\ fn curMod [ -> mod]             : get the module of the last dict entry
\ fn isCurMod [ref -> mod]        : get whether ref is curMod
\
\ fn panic [errCode]              : instantly panic with error
\ fn unreach []                   : instantly panic with E_unreach
\ fn assertWsEmpty []             : assert the working stack is empty (E_wsEmpty)
\ fn assertNoNow [asNow]          : used by SYN to assert not called with $
\ fn assertCurMod [ref]           : assert ref is cur mod
\ fn assertNotNull [&r]           : E_null if r is NULL
\ fn assertTyped [&key]           :
\ fn assertFnSmall [&key]         : assert fn is small (no locals)
\ fn assertFnLarge [&key]         : assert fn is large (has locals)
\
\ fn c_updateGkey [ -> &key]      : update gkey=dictLen and return it
\ fn ldictRef / ldictArgs / ldictLen  : interface directly with local dict
\ fn ldictSet / ldictGet / ldictGetK  : set/get/get-ref of local dict key
\ fn c_dictSetMeta [<dictArgs> meta:U1 &key] : update dict key meta
\
\ Note: any SYN function must be prefixed with asNow (typically #0)
\ since it will not be tagged as SYN until c_makeFn.

@_FP$loc _j2 \ {ref instr} compile jmpInstr to 2 byte ref
  .2%XSL @h1 $h2      \ compile instr {ref}
  .2%JMPL @h2 $h2     \ compile addr

@TY_FN$loc _xsl \ $_xsl <token> : compile unchecked xsl
  .2%XSL @kdictGet $h2 \ {key}
  .1%LIT @XSL2 $h1  \ push .2%XSL instr
  .2%JMPL @_j2 $h2

@TY_FN$loc _jmp \ $_jmp <token>: compile unchecked jmpl
  $_xsl kdictGet             \ {key}
  .1%LIT @JMPL2 $h1 \ push .2%JMPL instr
  .2%JMPL @_j2 $h2

@_FP$loc L1 \ {U1} compile 1 byte literal
  .1%LIT  @SZ1 @LIT ^JN   $h1 \ push .1%LIT instr
  $_xsl h1 \ compile it
  $_jmp h1

\ NOW PRE $c1: {instr:U1}
\ Compiles code so that when executed the instr will be compiled.
@_FP@TY_FN_NOW^JN$loc c1
  $_xsl L1    \ compile the instr literal itself
  \ compile xsl to h1
  .2%LIT @h1 $h2
  .2%LIT @XSL2 $h2
  $_jmp _j2

@_FP$loc L2 \ {U2} compile 2 byte literal
  @SZ2 @LIT  ^JN   $c1  \ compile .2%LIT instr
  $_jmp h2  \ compile the 2 byte literal

@_FP$loc L4 \ {U4} compile 4 byte literal
  @SZ4 @LIT  ^JN   $c1 \ compile .4%LIT
  $_jmp h4  \ compile the 4 byte literal

@L2 @L4    @ASIZE #2 ^EQ  $select @_FP=LA \ {UA} comipile ASIZE literal

@_FP@TY_FN_INLINE^JN $loc keyMeta \ {&key -> meta} get key's meta.
  #2$h1 %INCA .1%FT %RET \ Note: direct inline function. Two bytes

\ These take {&key} and tell information about it
@_FP$loc isTyConst   $keyMeta  @META_TY_MASK$L1 %MSK  @TY_CONST$L1 %EQ %RET
@_FP$loc isTyFn      $keyMeta  @META_TY_MASK$L1 %MSK  @TY_FN$L1  %EQ %RET
@_FP$loc isFnLarge   $keyMeta  @TY_FN_LARGE$L1 %MSK %RET
@_FP$loc assertNotNull @E_null$L2 $assert %RET
@_FP$loc assertFn   $_xsl isTyFn  @E_cNotFn $L2  $assert %RET \ [&key] -> []
@TY_FN$loc assertWsEmpty   @D_wslen$L0 %DVFT  @E_wsEmpty $L2  $_jmp assertNot
$assertWsEmpty

@_FP$loc assertFnSmall \ [&key]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $_jmp assertNot

@_FP$loc assertFnLarge \ [&key]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $assert %RET

@_FP$loc toMod @MOD_MASK $L4 %MSK %RET \ {ref} -> {mod}
@_FP$loc isSameMod \ {ref ref} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET

@_FP$loc curMod   .2%FTGL @c_gkey$h2 .A%FT  $_jmp toMod \ [] -> [mod]
@_FP$loc isCurMod $_xsl toMod  $_xsl curMod %EQ %RET     \ [ref] -> [isCurMod]
@_FP$loc assertCurMod  $_xsl isCurMod  @E_cMod$L2  $assert %RET

@_FP$loc _jSetup \ [&key] -> [ref]: checked jmp setup
  %DUP $_xsl assertFnSmall
  .A%FT %DUP $_jmp assertCurMod \ {ref}

@TY_FN$loc c_updateGkey \ [] -> [&key] update and return current key
  .A%FTGL @c_kdictRef$h2 \ dict.buf
  .2%FTGL @c_kdictLen$h2 \ dict.heap
  %ADD \ {&newKey}
  %DUP .A%SRGL @c_gkey$h2 \ gkey=newKey
  %RET \ return &key
\ FIXME: remove literal 0
@TY_FN$loc locK $_xsl c_updateGkey %SWP $_jmp loc \ { <token> meta -> &key} def loc, ret &key

@_FP$loc c_keyJnMeta \ {&key meta:U1} -> U4 : apply meta to &key
  %OVR %INCA \ {... &key newmeta &meta} note: #0 for unregistered SYN
  .1%FT %JN    \ {&key newMeta}
  %SWP %INCA .1%SR \ update meta
  %RET

@_FP$loc c_dictSetMeta \ {<dictArgs> meta:U1 &key} update dict key's meta.
  %SWP %OVR \ {<dictArgs> &key meta &key}
  %SWP $_xsl c_keyJnMeta \ {<dictArgs> &key}
  @D_dictDump$L0 %DVFT %RET \ dict dump entry

\ END: used for INLINE, IF/ELSE and BREAK0
@_FP $loc _END \ {&addr heapDiff} addr is where to store (heap-heapDiff)
  .A%FTGL @heap$h2             \ {&addr heapDiff heap}
  %SWP %SUB  %DUP $_xsl assertLt128 \ {heapDiff (heap-heapDiff)}
  %SWP .1%SR %RET \ store at &addr (1 byte literal)
@TY_FN@TY_FN_SYN^JN $loc END_INLINE $_xsl assertNoNow %DUP %INC $_jmp _END

\ example: $FN <token> $SYN $LARGE: declare a function with attributes.
@TY_FN@TY_FN_SYN^JN$loc FN
  $_xsl assertNoNow $_xsl assertWsEmpty @TY_FN$L1 $_xsl locK %DRP %RET
@TY_FN@TY_FN_SYN^JN$loc SYN     %DRP .A%FTGL @c_gkey$h2  @TY_FN_SYN$L1   $_jmp c_keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc NOW     %DRP .A%FTGL @c_gkey$h2  @TY_FN_NOW$L1   $_jmp c_keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc LARGE   %DRP .A%FTGL @c_gkey$h2  @TY_FN_LARGE$L1 $_jmp c_keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc PRE %DRP .A%FTGL @c_gkey$h2  @TY_FN_PRE$L1   $_jmp c_keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc INLINE  %DRP
  .A%FTGL @heap$h2 #0$L0 $_xsl h1 \ put heap on stack and write 0 to heap
  .A%FTGL @c_gkey$h2  @TY_FN_INLINE$L1 $_jmp c_keyJnMeta \ set meta as TY_FN_INLINE

$assertWsEmpty

$FN isFnPre     $PRE $keyMeta  @TY_FN_PRE$L1     %MSK %RET
$FN isVarInput  $PRE $keyMeta  @TY_VAR_INPUT$L1  %MSK %RET
$FN isFnNormal  $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NORMAL$L1 %EQ %RET
$FN isFnNow     $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NOW$L1    %EQ %RET
$FN isFnSyn     $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_SYN$L1    %EQ %RET
$FN isFnInline  $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_INLINE$L1 %EQ %RET
$FN isTyVar     $PRE $keyMeta  @META_TY_MASK$L1  %MSK  @TY_VAR$L1       %EQ %RET
$FN assertTyVar $PRE $_xsl isTyVar  @E_cNotLocal$L2 $assert %RET

$FN c_scan       $INLINE @D_scan$L0 %DVFT $END_INLINE %RET
$FN panic   $PRE $INLINE #0$L0 %SWP  $assert $END_INLINE \ {errCode}: panic with errCode
$FN unreach      @E_unreach$L2 $panic \ {}: assert unreachable code
$FN unimplIfTrue $PRE @E_unimpl$L2 $assert %RET \ {}: if true raise unimpl
$FN tAssertKeyMeta $PRE %SWP $keyMeta %SWP $_jmp tAssertEq \ {&key meta}
$FN assertSzI $PRE \ {szI}
  %DUP #CF$L1 %MSK @E_cSz$L2 $_xsl assertNot \ non-sz bits empty
  #4$L0 %SHR #3$L1 %LT_U @E_cSz$L2 $assert %RET \ sz bits < 3

$FN ftoN $NOW  \ {offset szI} compile FTO szI w/offset
  %DUP $_xsl assertSzI  @FTO$L1 %ADD $_xsl h1 $_jmp h1

\ Update the harness with the new dictionary
$FN c_dictDump       $_xsl kdictArgs @D_dictDump$L0 %DVSR %RET \ {}
$c_dictDump

$FN ldictArgs \ {} -> dictArgs
  @c_ldictRef$L2 %RET \ TODO: add R_GB to it.

$FN _ldict $c_scan $_jmp ldictArgs
$FN ldictGet   $_xsl _ldict @D_dict$L0  %DVFT %RET
$FN ldictSet   $PRE #0$L0 $_xsl _ldict @D_dict$L0  %DVSR %RET
$FN ldictGetK  $_xsl _ldict @D_dictK$L0 %DVFT %RET
$FN retz       $PRE $INLINE      %RETZ $END_INLINE %RET
$FN reteq      $PRE $INLINE %NEQ %RETZ $END_INLINE %RET
$FN retif      $PRE $INLINE %NOT %RETZ $END_INLINE %RET

\ **********
\ * [7] ASM (initial) Flow Control
\ Flow control either pushes the current heap on the WS or uses a local
\ constant. END/AGAIN uses this heap-val/local to do the right thing.
\
\   if/else: $IF ... $ELSE ... $END
\   loop:    $LOOP <l0> ... $BREAK0 <b0> ... $AGAIN <l0> $BREAK_END <b0>

$FN IF $SYN $PRE $_xsl assertNoNow \ {} -> {&jmpTo} : start an if block
  @SZ1 @JZL  ^JN   $c1 \ compile .1%JZL instr
  .A%FTGL @heap$h2 \ {&jmpTo} push &jmpTo location to stack
  #0$L0  $_xsl h1 \ compile 0 (jump pad)
  %RET

@TY_FN@TY_FN_SYN^JN $loc END $_xsl assertNoNow %DUP $_jmp _END

$FN ELSE $SYN $_xsl assertNoNow \ {&ifNotJmpTo} -> {&elseBlockJmpTo}
  @JMPL $c1         \ (end IF) compile unconditional jmp to end of ELSE
  .A%FTGL @heap$h2 %SWP \ {&elseBlockJmpTo &ifNotJmpTo}
  #0$L0 $_xsl h1     \ compile jmp lit for &elseBlockJmpTo
  %DUP $_jmp _END   \ end of IF block (beginning of ELSE)

\ $LOOP l0 ... $BREAK0 b0 ... $AGAIN l0  $BREAK_END b0
$FN LOOP   $SYN $_xsl assertNoNow .A%FTGL @heap$h2  $_jmp ldictSet
$FN BREAK0 $PRE $SYN   $_xsl IF $_jmp ldictSet
$FN BREAK_IF  $PRE $SYN @NOT$c1  $_jmp BREAK0 \ break if true
$FN BREAK_EQ  $PRE $SYN @NEQ$c1  $_jmp BREAK0 \ break if equal
$FN BREAK_NEQ $PRE $SYN @EQ$c1  $_jmp BREAK0 \ break if equal
$FN AGAIN $SYN $_xsl assertNoNow
  @JMPL $c1  \ compile jmp
  .A%FTGL @heap$h2  \ {heap}
  $_xsl ldictGet \ {heap &loopTo}
  %SUB     \ {heap-&loopTo}
  %DUP $_xsl assertLt128
  %NEG          \ make negative for backwards jmp
  $_jmp h1      \ compile as jmp offset

$FN END_BREAK $SYN $_xsl assertNoNow $_xsl ldictGet %DUP $_jmp _END

\ **********
\ * [8] xx, jmpl, Scanning and Alignment Utilities
\ Reading, peeking and szI alignment
\
\ fn xx:<token> [...]             : compile an execute to a token
\ fn jmp:<token> [...]            : compile an jmp to a token
\ fn align [aptr sz -> aptr]      : align aptr with sz bytes
\ fn align4 [aptr -> aptr]        : align aptr with 4 bytes
\ fn alignSzI [aptr szI -> aptr]  : align aptr with szI bytes
\ fn hN [U4 szI]                  : write a value of szI to heap (no align)
\ fn szToSzI [U4 -> SzI]          : convert number of bytes to SzI
\ fn szIToSz [SzI -> U1]          : convert szI to number of bytes
\
\ fn dictK [-> &key isFromLocal]  : any ref to current token.
\ fn c_read [ -> numRead]         : attempt to read bytes from src.
\ fn c_readNew [ -> numRead]      : clear token buf and read bytes.
\ fn c_scanNoEof []               : scan and assert not EOF.
\ fn c_peekChr [ -> c]            : peek at the next character.
\ fn c_clearToken []              : shift buffer to clear current token
\
\ fn assertSzI [szI]              : assert that szI is valid
\ fn c_isEof [ -> bool]           : return whether there was a token scanned
\ fn c_assertToken []             : assert there is a token
\ fn c_assertNoEof [numRead]      : assert that numRead > 0 (E_eof)

$FN keySzI $PRE $keyMeta @SZ_MASK$L1 %MSK %RET \ {&key -> szI}

$FN szIToSz $PRE \ {szI} -> {sz}
  %DUP @SZ1$L1 %EQ $IF  %DRP #1$L0 %RET  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP #2$L0 %RET  $END
       @SZ4$L1 %EQ $IF       #4$L0 %RET  $END
  @E_cSz$L2 $panic

$FN hN $PRE \ {value szI} write a value of szI to heap
  %DUP @SZ1$L1 %EQ $IF  %DRP $_jmp h1  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP $_jmp h2  $END
  %DUP @SZ4$L1 %EQ $IF  %DRP $_jmp h4  $END
  @E_cSz$L2 $panic

$FN szToSzI $PRE \ [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L0 %EQ $IF  %DRP @SZ1 $L1 %RET  $END
  %DUP #2$L0 %EQ $IF  %DRP @SZ2 $L1 %RET  $END
       #4$L0 %EQ $IF  %DRP @SZ4 $L1 %RET  $END
  @E_cSz$L2 $panic

$FN reqAlign $PRE \ {sz -> sz}: get required alignment
  %DUP @ASIZE$L0 %DEC %LT_U $retif  %DRP @ASIZE$L0 %RET

$FN align $LARGE $PRE \ {aptr sz -> aptr}: align aptr with sz bytes
  #1 $h1 \ locals [sz:U1]
  .1%SRLL#0$h1 \ cache sz
  %DUP \ {aptr aptr}
  .1%FTLL#0$h1 %MOD \ {aptr aptr%sz}
  %DUP $IF
    .1%FTLL#0$h1 %SWP %SUB \ {aptr (sz - aptr%sz)}
    %ADD %RET \ aptr + (sz - aptr%sz)
  $END
  %DRP %RET

$FN alignA   $PRE $_xsl reqAlign .2%XLL @align$h2 %RET  \ {aptr sz -> aptr}: align to SZA
$FN align4   $PRE #4$L0 .2%XLL @align$h2 %RET          \ {addr -> aligned4Addr}
$FN alignSzI $PRE $_xsl szIToSz  .2%XLL @align$h2 %RET \ {addr szI -> addrAlignedSzI}

$FN ftSzI \ {&addr szI}
  %DUP @SZ1$L1 %EQ $IF %DRP .1%FT %RET $END
  %DUP @SZ2$L1 %EQ $IF %DRP .2%FT %RET $END
       @SZ4$L1 %EQ $IF      .4%FT %RET $END
  @E_cSz$L2 $panic

$FN srSzI \ {value &addr szI}
  %DUP @SZ1$L1 %EQ $IF %DRP .1%SR %RET $END
  %DUP @SZ2$L1 %EQ $IF %DRP .2%SR %RET $END
       @SZ4$L1 %EQ $IF      .4%SR %RET $END
  @E_cSz$L2 $panic

\ TODO: need to make isNotEof INLINE function. Possibly get rid of all isEof uses?
$FN c_isEof .2%FTGL@c_tokenLen$h2 %NOT %RET
$FN c_assertToken .2%FTGL@c_tokenLen$h2 @E_cNeedToken$L2 $assert %RET
$FN c_assertNoEof $PRE @E_eof$L2 $assert %RET \ {numRead}

$FN c_scanNoEof
  $c_scan
  $_xsl c_isEof
  @E_eof$L2 $_jmp assertNot

$FN c_peekChr \ {} -> {c} peek at a character
  $c_scan
  $_xsl c_isEof $IF  #0$L0 %RET  $END
  .A%FTGL@c_tokenBuf$h2 .1%FT \ {c}
  #0$L0 .2%SRGL@c_tokenLen$h2 %RET \ reset scanner for next scan

$FN c_read $INLINE #1$L0 @D_read$L0 %DVFT $END_INLINE %RET \ { -> numRead}
$FN c_readNew \ { -> numRead} clear token buf and read bytes
  #0$L0 .2%SRGL@c_tokenLen$h2
  #0$L0 .2%SRGL@c_tokenSize$h2
  $c_read %RET

$FN c_clearToken \ shift buffer to clear current token
  .A%FTGL@c_tokenBuf$h2                 \ {&tokenBuf}
  %DUP .2%FTGL@c_tokenLen$h2 %ADD \ {&tokenBuf &tokenEnd}
  .2%FTGL@c_tokenLen$h2           \ {&tokenBuf &tokenEnd tokenLen}
  @D_memSet$L0 %DVSR %RET  \ memMove

$FN dictK \ {} -> {&key isFromLocal}
  $_xsl ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF
    @TRUE$L1 %RET
  $END %DRP
  $_xsl kdictArgs  @D_dictK$L0 %DVFT %DUP  $IF
    @FALSE$L0 %RET
  $END @E_cNotType$L2 $panic

$FN c_updateLkey \ [] -> [&key] update and return current local key
  .A%FTGL @c_ldictRef$h2 \ dict.buf
  .2%FTGL @c_ldictLen$h2 \ dict.heap
  %ADD \ {&newLKey}
  %DUP .A%SRGL @c_lkey$h2 \ gkey=newKey
  %RET \ return &key

$FN xSzI $PRE \ {&key} -> {szI}: return the size requirement of the X instr
   .A%FT $_xsl isCurMod $IF  @SZ2$L1 %RET  $END  @SZA$L1 %RET

\ {&key szInstr szLit instr} compile a literal memory instr.
\   szLit the size of the literal to compile for the instr.
\   oRef: either a reference or an offset
$FN c_instrLitImpl $PRE $LARGE
  #1 $h1 \ 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 \ var instr          {&key szInstr szLit}
  .1%SRLL #0$h1 \ var szLit          {&key szInstr}
  .1%FTLL #1$h1 %JN  $_xsl h1 \ compile (szInstr | instr) {&key}
  .A%FT \ {oRef} offset or reference
  .1%FTLL #0$h1  $_jmp hN \ compile literal of proper instrSz

$FN c_fn $PRE \ {&key}: compile a function of any type
  %DUP $_xsl assertFn \ {&key}
  %DUP $_xsl isFnInline $IF \ Inline compilation {&key}
    %DUP $_xsl isFnLarge @E_cInlineLarge$L2 $_xsl assertNot
    .A%FT .A%FTGL @heap$h2 %OVR \ {&inlineFn &heap &inlineFn}
    %DUP %INC %SWP .1%FT \ {&inlineFn &heap &inlineFn+1 inlineLen}
    @D_memSet$L0 %DVSR  \ memMove {&inlineFn}
    .1%FT .A%FTGL @heap$h2 %ADD .A%SRGL @heap$h2 %RET \ update heap+inlineLen
  $END
  %DUP $_xsl xSzI     \ {&key szLit}
  %OVR $_xsl isFnLarge  $IF @XLL$L1 $ELSE @XSL$L1 $END \ {&key instrSzI instr}
  %OVR %SWP \ {&key instrSzI litSzI instr} instr & lit are same sz
  .2%XLL @c_instrLitImpl$h2 %RET

$FN execute \ {&key} -> {...}: execute a dictionary key
  %DUP $_xsl isFnInline $IF \ if inline, assert not large and jmp to addr+1
    %DUP $_xsl isFnLarge @E_cInlineLarge$L2 $_xsl assertNot  .A%FT %INC .A%JMPW
  $END
  %DUP $_xsl isFnLarge  $IF .A%FT .A%XLW %RET $END
  .A%FT .A%JMPW

$FN colon \ consume a colon token as syntactic surgar, i.e. xx:foo
  $c_scan .2%FTGL@c_tokenLen$h2 #1$L0 %EQ @E_cColon$L2 $assert \ assert len=1
  .A%FTGL@c_tokenBuf$h2 .1%FT  #3A$L0 %EQ @E_cColon$L2 $assert \ assert ":"
  %RET

$FN _xxPre $LARGE
  #1$h1 $_xsl assertNoNow \ locals 0=&key
  $_xsl colon $_xsl kdictGetK %DUP .A%SRLL #0$h1 \ {&key}
  \ if fn is (PRE or SYN) and a compFn exists, compile next token.
  %DUP $_xsl isFnPre %SWP $_xsl isFnSyn %OR .A%FTGL @c_compFn$h2 %AND $IF
    .A%FTGL @c_compFn$h2 .4%XLW
  $END .A%FTLL #0$h1 %RET

$FN xx $SYN .2%XLL @_xxPre$h2 $_jmp c_fn
$FN jmp $SYN
  $xx:_xxPre %DUP $xx:isFnLarge @E_cXHasL$L2 $xx:assertNot
  .A%FT @JMPL2$L1 $_jmp _j2

\ **********
\ * [9] Globals and Locals
\ We need a way to define global and local variables, as well as GET, SET and
\ obtain a REF to them.
\
\ fn GET <token>            SYN : compile a FT of token (local or global)
\ fn SET <token>            SYN : compile a SR of token (local or global)
\ fn REF <token>            SYN : compile a "get ref" of token
\ fn declVar                    : declare a local/gloabl variable
\ fn declEnd                    : end locals declaration

$FN declG \ [<token> -> &key isLocal] create global and return it.
  @TY_VAR$L1 $xx:locK
  @FALSE$L0 %RET \ {&key isLocal=FALSE}

$FN declL \ [<token> -> &key isLocal] create local and return it.
  $xx:c_updateLkey \ {&key}
  #0$L0 $xx:ldictSet \ initialize lDict token (to heap for now)
  %DUP @TY_VAR$L1 $xx:c_keyJnMeta \ {&key} update meta as local
  @TRUE$L0 %RET \ {&key isLocal=TRUE}

$FN declVar $LARGE $PRE \ {&key isLocal meta szBytes} declare a variable (global or local)
  #1$h1 .1%SRLL #0$h1  .1%SRLL #1$h1   .1%SRLL #2$h1 \ Locals 0=szBytes 1=meta 2=isLocal
  %DUP .1%FTLL #1$h1 $xx:c_keyJnMeta \ update key meta {&key}
  .1%FTLL #2$h1 $IF \ if(isLocal) {&key}
    .2%FTGL @c_localOffset$h2  .1%FTLL #0$h1  $xx:alignA \ {&key offsetAligned}
    %DUP .1%FTLL #0$h1 %ADD .2%SRGL @c_localOffset$h2 \ update c_localOffset {...}
  $ELSE
    .A%FTGL @c_gheap$h2        .1%FTLL #0$h1  $xx:alignA \ {&key gheapAligned}
    %DUP .1%FTLL #0$h1 %ADD .A%SRGL @c_gheap$h2       \ update gheap {...}
  $END %SWP .A%SR %RET \ update key's value

\ Compile a get or set instruction.
\ Args:
\   &key: key to compile.
\   dotMeta: whether the value to compile is a local or global.
\   localInstrSz localInstr: if isFromLocal: use these as the literal sz and instr.
\   globalInstrSz globalInstr: if NOT isFromLocal: use these as the literal sz and instr.
$FN _getSetImpl $PRE $LARGE
  #1 $h1 \ locals (see below)
  .1%SRLL#3$h1   .1%SRLL#2$h1 \ 2=globalInstrSz 3=globalInstr
  .1%SRLL#1$h1   .1%SRLL#0$h1 \ 0=localInstrSz 1=localInstr
  \ {&key dotMeta}
  %OVR $xx:assertTyVar $IF \ {&key}
        %DUP $xx:keySzI .1%FTLL#0$h1 .1%FTLL#1$h1
  $ELSE %DUP $xx:keySzI .1%FTLL#2$h1 .1%FTLL#3$h1 $END
  $xx:c_instrLitImpl %RET

\ (create _xxxImpl for fngi to use)
$FN _getImpl $PRE \ {&key dotMeta}
  @SZ1$L1  @FTLL$L1  \ local sz + instr
  @SZ2$L1  @FTGL$L1  \ global sz + instr
  $xx:_getSetImpl %RET

$FN _setImpl $PRE \ {&key dotMeta}
  @SZ1$L1  @SRLL$L1  \ local sz + instr
  @SZ2$L1  @SRGL$L1  \ global sz + instr
  $xx:_getSetImpl %RET

$FN gRef $NOW \ [<token> -> &gref] get token's global reference
  $xx:kdictGetK  %DUP $xx:assertTyVar .A%FT %RGFT @R_GB$h1 %ADD %RET

$FN _refImpl \ {}
  $xx:ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {&key}
    %DUP $xx:assertTyVar \ {&key}
    .A%FT %DUP #40$L1 %LT_U @E_cReg$L2 $assert \ {offset}
    @R_LP$L1 %JN  \ {LpOffset}: offset is lower 7 bits
    @RGFT$c1 $jmp:h1  \ compile: %RGFT (@R_LP + offset)$h1
  $END %DRP
  $xx:kdictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {&key}
    %DUP $xx:assertTyVar  .A%FT $jmp:LA \ write literal directly TODO: use c_lit
  $END @E_cNotType$L2 $panic

$FN REF  $SYN
  $IF \ asNow: we can get &fn or &global
    $xx:kdictGetK \ next: assert(isTyVar or isTyFn)
      %DUP $xx:isTyVar %OVR $xx:isTyFn %OR
      @E_cNotType$L2 $assert
    .A%FT %RET
  $END \ else: we can get &local or &global
  $c_scan $jmp:_refImpl

$FN GET  $SYN
  $IF  $xx:kdictGetK %DUP $xx:assertTyVar %DUP $xx:keySzI \ {&key szInstr}
       %SWP .A%FT %SWP $jmp:ftSzI   $END
  $c_scan $xx:dictK $jmp:_getImpl

$FN SET $SYN
  $xx:assertNoNow $c_scan $xx:dictK $jmp:_setImpl

\ **********
\ * Local Variables

\ All of these take &key and output len (U1 len), sz (total key size), nextKey (&key)
$FN Dict_keyLen  $PRE @DICT_OLEN$L0 %ADD .1%FT #3F$L1 %MSK %RET
$FN Dict_keySz   $PRE $xx:Dict_keyLen @DICT_OLEN^INC$L0 %ADD $jmp:align4
$FN Dict_nextKey $PRE %DUP $xx:Dict_keySz %ADD %RET

\ {&key} -> {} recursive function to compile INPUTs
\ Inputs are "compiled" (i.e. a SRLL is compiled) in reverse order.
\ This maps them well to the conventional stack nomenclature.
$FN _compileInputs $PRE $LARGE
  #1$h1 \ locals 0=&key:APtr
  %DUP  .A%SRLL#0$h1 \ {&key} var key
  .A%FTGL @c_ldictRef$h2 .2%FTGL @c_ldictLen$h2 %ADD \ {&key &ldictEnd}
    $reteq \ return if key=ldictEnd
  .A%FTLL#0$h1  $xx:Dict_nextKey  $xx:_compileInputs \ get the next key and recurse {}
  .A%FTLL#0$h1  %DUP $xx:isTyVar %SWP \ {isTyVar &key}
  $xx:isVarInput %AND %RETZ \ {} all of (isTyVar isVarInput)
  .A%FTLL#0$h1  %DUP $xx:keySzI \ {&key szInstr}
  @SZ1$L1 @SRLL$L1 $xx:c_instrLitImpl %RET

\ End locals declaration. Update FN slots and LARGE.
\ Compiles SRLL for each TY_VAR_INPUT, in reverse order.
$FN declEnd
  $GET c_localOffset $IF \ if localOffset: update fn to large
    $GET c_gkey @TY_FN_LARGE$L0 $xx:c_keyJnMeta
  $END
  $GET c_localOffset #4$L0 $xx:align
  @APO2$L0 %SHR $xx:h1 \ update number of slots
  $GET c_ldictRef  $xx:_compileInputs
  %RET

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
\ fn $ <token>                    : execute token asNow
\ fn ret [U4]                     : compile %RET
\ fn _   fn ,   fn ;              : syntax helpers that do nothing.
\
\ fn betweenIncl [value a b -> bool]  : return whether value between [a,b]
\ fn charToInt [c -> U8]          : convert ascii -> hex
\
\ c_fngi                          : fngi compile loop.
\ fn fngiSingle [ -> ...]         : starting (base) c_compFn
\ fn c_single [asNow -> ...]  : compile/execute a single token (c_compFn).
\ fn c_fn [&key]                  : compile a function (small or large)
\ fn execute [&key]               : execute a function (small or large)
\ fn parseNumber [ -> value isNum]: parse token as number
\ fn lit [U4]                     : compile literal
\ fn xSzI [&key -> szI]        : return szI of the fn
\
\ fn c_charNext [ -> c]           : read next character (WARN: AFTER tokenLen)
\ fn c_charNextEsc [ -> c unkEsc] : read an escapeable character (string).
\ fn c_updateCompFn [newComp -> prevComp] : update c_compFn + ret old
\ fn c_number <number> -> [value isNum]   : parse next token (parseNumber).

$FN betweenIncl $PRE \ {value a b} -> a <= value <= b
  $declL b  @SZA@TY_VAR_INPUT^JN  @ASIZE $declVar
  $declEnd \ {value a}
  %OVR %SWP \ {value value a}
  \ if (value<a) return FALSE;
  %LT_U $IF %DRP @FALSE$L0 %RET $END
  $GET b %SWP \ {b value}
  %LT_U %NOT %RET \ return not(b<value)

$FN charToInt $PRE \ {c} -> {U8}
  \ '0' - '9'
  %DUP #30$L0 #39$L0 $xx:betweenIncl $IF #30$L0 %SUB %RET $END
  \ 'A' - 'Z'
  %DUP #41$L1 #5A$L1 $xx:betweenIncl $IF #41$L1 %SUB #A$L0 %ADD %RET $END
  \ 'a' - 'z'
  %DUP #61$L1 #7A$L1 $xx:betweenIncl $IF #61$L1 %SUB #A$L0 %ADD %RET $END
  %DRP #FF$L1 %RET

\ {} -> {c}: read next character from AFTER tokenLen.
\ Increments tokenLen. This is destructive to token, use with care.
$FN c_charNext
  $GET c_tokenLen  $GET c_tokenSize %GE_U $IF
    $xx:c_readNew  $xx:c_assertNoEof
  $END
  $GET c_tokenBuf  $GET c_tokenLen  %ADD .1%FT
  $GET c_tokenLen %INC  $SET c_tokenLen %RET

\ {} -> {char unknownEscape} read a character that can be escaped.
$FN c_readCharEsc
  $xx:c_charNext \ {char}
  %DUP #5C$L1 %NEQ $IF @FALSE$L0 %RET $END \ if(c != '\\') ret;
  \ c is an escape character: \
  %DRP $xx:c_charNext
  %DUP #5C$L1 %EQ $IF             @FALSE$L0 %RET $END \ \\: escape
  %DUP #74$L1 %EQ $IF %DRP #09$L0 @FALSE$L0 %RET $END \ \t: tab
  %DUP #6E$L1 %EQ $IF %DRP #0A$L0 @FALSE$L0 %RET $END \ \n: newline
  %DUP #20$L1 %EQ $IF %DRP #20$L0 @FALSE$L0 %RET $END \ \ : space
  %DUP #78$L1 %EQ $IF \ \xHH
    \ charToInt(c_charNext) << 8 + charToInt(c_charNext)
    %DRP $xx:c_charNext  $xx:charToInt #8$L0  %SHL
    $xx:c_charNext       $xx:charToInt %ADD
    \ assertNot(dup < inc(0xFF), E_cStr)
    %DUP #FF$L1 %INC %LT_U  @E_cStr$L2  $xx:assertNot
    @FALSE$L0 %RET
  $END
  @TRUE$L0 %RET \ just return the character as-is but unknownEscape=true

$FN numBase $PRE \ {c -> base} get number base from char
  %DUP #63$L1 %EQ $IF %DRP #FE$L1 %RET $END \ c -> character
  %DUP #62$L1 %EQ $IF %DRP #02$L0 %RET $END \ b -> binary
       #78$L1 %EQ $IF      #10$L0 %RET $END \ x -> hex
  #0$L0 %RET \ unknown/use default

$FN c_parseBase \ {i base -> value isNumber}
  $declL i      @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL base   @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL value  @SZA                  @ASIZE $declVar
  $declEnd
  $GET base #FE$L1 %EQ $IF \ if special 'character' base
      $GET i $SET c_tokenLen \ readCharEsc is off END of tokenLen
      $xx:c_readCharEsc  @E_cUnknownEsc$L2 $xx:assertNot  @TRUE$L0 %RET
  $END #0$L0 $SET value
  $LOOP l0
    $GET i  $GET c_tokenLen $BREAK_EQ b0
    $GET c_tokenBuf $GET i %ADD .1%FT \ {c}
    $xx:charToInt \ {v}
    \ return {0 0} if not integer character
    %DUP $GET base %GE_U $IF  @FALSE$L0 %RET  $END

    $GET base  $GET value %MUL \ {base * value}
    %ADD $SET value \ value = v + value*10
    $GET i %INC $SET i \ i += 1
  $AGAIN l0  $END_BREAK b0

  $GET i %NOT $IF  #0$L0 @FALSE$L0 %RET  $END \ no token
  $GET value @TRUE$L0 %RET

$FN c_parseNumber \ {} -> {value isNumber}
  #0$L0 #A$L0 \ {i=0 base=0xA}. Next: if (token len>=2 and starts with '0')
  $GET c_tokenLen #2$L0 %GE_U $GET c_tokenBuf .1%FT #30$L0 %EQ %AND $IF
    %DRP $GET c_tokenBuf %INC .1%FT $xx:numBase %DUPN $IF \ {i base}
      %DRP #A$L0 \ base was = 0, just set back to 10
    $ELSE %SWP %INC2 %SWP $END \ else i+=2
  $END
  $xx:c_parseBase %RET

$FN lit  $PRE \ {U4} compile literal
  %DUP #40$L1 %LT_U        $IF  $jmp:L0  $END
  %DUP #FF$L1 %INC %LT_U   $IF  $jmp:L1  $END
  %DUP #FFFF$L2 %INC %LT_U $IF  $jmp:L2  $END
  $jmp:L4

\ {asNow value:U4} -> {?nowVal}: compile proper sized literal
\ if asNow=true the value is left on the stack.
$FN c_lit
  %SWP %NOT %RETZ \ if now, leave on stack
  $jmp:lit


$FN _compConstant $PRE \ {asNow} -> {&keyFn[nullable]}
  $xx:c_parseNumber \ {asNow value isNumber}
  $IF  $xx:c_lit #0$L0 %RET  $END %DRP \ {asNow}
  $xx:c_isEof $IF  %DRP #0$L0 %RET  $END \ {asNow}

  \ Handle local dictionary. Only constants allowed here.
  $xx:ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {asNow &key}
    %DUP $xx:isTyConst  @E_cNotFnOrConst$L2 $assert
    .A%FT $xx:c_lit  #0$L0 %RET
  $END %DRP \ {asNow}
  $xx:kdictArgs  @D_dictK$L0 %DVFT \ {asNow &key}
  %DUP $xx:isTyConst $IF \ Constant case {asNow &key}
    .A%FT $xx:c_lit #0$L0 %RET
  $END %SWP %DRP %DUP $jmp:assertFn \ {&key}

$assertWsEmpty

\ declare c_compFn = 0
#0  $gRef c_compFn  .4^SR

$FN c_updateCompFn $PRE \ {&newCompFn -> &prevCompFn}
  $GET c_compFn %SWP $SET c_compFn %RET

\ {asNow} -> {}: compile a single token.
\ This is the primary function that all compilation steps (besides spor
\ compilation) reduce to.
$FN c_single $PRE
  $declL asNow @SZ1 #1     $declVar
  $declL key   @SZA @ASIZE $declVar $declEnd
  \ Handle constants, return if it compiled the token.
  %DUP $SET asNow $xx:_compConstant %DUP $SET key %RETZ
  $GET key $xx:isFnPre $IF $GET c_compFn .A%XLW $END \ recurse for PRE
  $GET key $xx:isFnSyn $IF $GET asNow $GET key $jmp:execute    $END
  $GET key $xx:isFnNow $IF $GET asNow @E_cReqNow$L2 $assert $END
  $GET key $GET asNow $IF  $jmp:execute  $END  $jmp:c_fn

$FN c_number $c_scan $xx:c_parseNumber %RET \ compile next token as number.

$FN (  $SYN%DRP  \ parens ()
  $xx:c_assertToken
  $xx:c_peekChr #29$L0 %EQ $IF  $c_scan %RET  $END \ return if we hit ")"
  $LOOP l0
    $GET c_compFn .A%XLW
    $xx:c_assertToken
    $xx:c_peekChr #29$L0 %EQ $IF  $c_scan %RET  $END \ return if we hit ")"
  $AGAIN l0

$FN _spor
  $declL compFn  @SZA  @ASIZE $declVar $declEnd
  @_spor$L2  $xx:c_updateCompFn $SET compFn \ update c_compFn and cache
  $xx:c_scanNoEof
  @D_comp$L0  %DVFT \ compile next token as spor asm
  $GET compFn $SET c_compFn %RET

$FN spor $SYN $xx:assertNoNow $xx:_spor %RET \ compile as assembly

$FN c_now \ used in $ to make next token/s run NOW.
  $declL compFn  @SZA  @ASIZE $declVar $declEnd
  @c_now$L2  $xx:c_updateCompFn $SET compFn \ update c_compFn and cache
  $xx:c_scanNoEof
  @TRUE$L0 $xx:c_single  \ compile next token as NOW
  $GET compFn $SET c_compFn %RET

$FN $ $SYN $xx:assertNoNow $xx:c_now %RET \ make NOW

$FN _comment \ used in \ to make next token ignored (comment)
  $declL compFn  @SZA  @ASIZE $declVar $declEnd
  @_comment$L2  $xx:c_updateCompFn $SET compFn
  $xx:c_scanNoEof
  \ Execute an open paren, else ignore
  $GET c_tokenBuf .1%FT #28$L0 %EQ $IF @TRUE$L0 $xx:c_single $END
  $GET compFn $SET c_compFn %RET

\ {-> c} peek at the char after current token.
$FN c_peekNoScan
  $GET c_tokenLen  $GET c_tokenSize %GE_U $IF
    $c_read $xx:c_assertNoEof \ ensure a char exists
  $END
  $GET c_tokenBuf $GET c_tokenLen %ADD .1%FT %RET

\ Comment, of which there are three forms.
\    \        : a line comment
\    \foo     : an inline comment, commenting out one token
\    \( ... ) : a block comment
$FN \
  $SYN %NOP %DRP
  \ Line comment if '\' is followed by space or newline
  $xx:c_peekNoScan #20$L0 %EQ
  $IF @D_scan$L0 %DVSR %RET $END \ scanEol
  $xx:_comment %RET \ else token comment

$FN ret $PRE $SYN $xx:assertNoNow @RET $c1 %RET \ ret 4, or just ret;

\ These do nothing and are used for more readable code.
$FN _ $SYN%DRP %RET  $FN , $SYN%DRP %RET  $FN ; $SYN%DRP %RET
$FN -> $SYN%DRP %RET

$FN c_fngi \ fngi compile loop
  $LOOP l0
    $GET c_tokenSize %RETZ \ exit on EOF
    $GET c_compFn .A%XLW
  $AGAIN l0

$FN fngiSingle \ base c_compFn for fngi tokens.
  #0$h1 $LARGE \ not really any locals (but called with XLW)
  $c_scan $GET c_tokenLen %RETZ
  @FALSE$L0 $xx:c_single %RET

@fngiSingle $c_updateCompFn ^DRP

$c_dictDump
