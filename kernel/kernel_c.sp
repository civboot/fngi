\ Kernel Constants
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

\ Values put on stack by kernel-compiler
    #0=SZA   \ SZ2 or SZ4 depending on arch
    #0=ASIZE \ 2 or 4 depending on arch
    #0=CODE_HEAP_START \ start of code heap

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

\ # [1.e] Sizes
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
#0A #0=D_memSet   \ {dst v len} "dst = v [len]". FT: memset, SR: memmove
#0B #0=D_memCmp   \ {&a &b len} -> I32: <0 if a<b; >0 if a>b; 0 if a==b
#0C #0=D_com      \ {&msg len} -> {ioResult}: write to debug stream
#0D #0=D_zoa      \ {} -> {} parse zoa to heap
#0E #0=D_dictDump \ {<dict-args> [&entry]} dump a dictionary FT=entry SR=full
#0F #0=D_comZoab  \ FT{U4}  SR{len &data join}
#10 #0=D_comDone  \ signifies end of one com. On linux this is a flush.
#11 #0=D_block     \ {... &rooti &ba} block allocator. FT=alloc, SR=free
#12 #0=D_bump      \ {size &bba} bump allocator. FT=alloc, SR=allocUnaligned

\ **********
\ * [3] Constants
#0  #0=FALSE
#1  #0=TRUE
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
#E0F9  #0=E_newBlock    \ Require a NEW_BLOCK for code.
#E0FA  #0=E_OOM         \ Out Of Memory

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


#0000_0004 @TY_VAR@SZA^JN=heap
#0000_0008 @TY_VAR@SZA^JN=topHeap
#0000_000C @TY_VAR@SZA^JN=topMem
#0000_0010 @TY_VAR@SZ2^JN=err
#0000_0012 @TY_VAR@SZ2^JN=c_state  \ U2
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

\ Block Bump Arena
#0000_0050 @TY_VAR@SZA^JN=kBBA        \ also kBBA.ba
#0000_0054 @TY_VAR@SZ1^JN=kBBA_rooti
#0000_0056 @TY_VAR@SZ2^JN=kBBA_len
#0000_0058 @TY_VAR@SZ2^JN=kBBA_cap

\ Global Compiler Variables
#0000_005C @TY_VAR@SZA^JN=c_gkey         \ [U4] current kdict &key
#0000_0060 @TY_VAR@SZA^JN=c_lkey         \ [U4] current ldict &key
#0000_0064 @TY_VAR@SZA^JN=c_gheap        \ [U4] global heap
#0000_0068 @TY_VAR@SZ2^JN=c_localOffset  \ [U2] Local Offset (for local var setup)
#0000_006C @TY_VAR@SZA^JN=c_compFn       \ [UA] current compiler function

@CODE_HEAP_START @TY_FN=_h  \ { -> heap} get the code heap
  .1%FTGL@kBBA_rooti.2,         \ {rooti} get index block in use
  @SLIT#0B^JN.1,  %SHL          \ {rooti<<12} convert to &block
  .2%FTGL@kBBA_len, %ADD %RET   \ {&block+len} return the current "heap"

$_h @_FP@TY_FN_INLINE^JN =assert #2.1, \ {cond errCode} assert cond or panic
  @SLIT@D_assert^JN.1 %DVFT %RET

$_h @_FP=kbump \ {size -> &data} bump some memory from kernel BBA
  .2%LIT @kBBA, \ {size &bba}
  @D_bump@SLIT^JN.1, %DVSR  \ call D_bumpUnaligned {leftoverSize &leftover &data}
  %SWP %DRP %SWP %NOT \ {&data (not leftoverSize)} next: assert no leftover and data
  .2%LIT@E_newBlock, $assert  %DUP .2%LIT@E_OOM, $assert %RET

$_betterh @_FP=h1   #1@SLIT^JN.1,  .2%XSL @kbump,  .1%SR %RET
\ $_betterh @_FP=kh2   #2 @SLIT ^JN .1,  .2%XSL @kbump,  .2%SRBE %RET
\ $_betterh @_FP=kh4   #4 @SLIT ^JN .1,  .2%XSL @kbump,  .4%SRBE %RET
