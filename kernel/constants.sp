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
\    [1.d] Sizes: SZ1, SZ2, SZ4, SZA
\    [1.e] Mem: fetch, store, locals, globals
\    [1.f] Jmp: jumping, execution, tables
\    [1.g] Small Literal [0x40 - 0x80)
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
\   01SS XXXX: mem
\   10SS XXXX: jmp
\   11XX XXXX: small literal value [0x00 - 0x3F]
#40 #0=I_MEM
#80 #0=I_JMP

\ Values put on stack by kernel-compiler
    #0=SZA   \ SZ2 or SZ4 depending on arch
    #0=ASIZE \ 2 or 4 depending on arch
    #0=CODE_HEAP_START \ start of code heap

\ # [1.a] Operations: Special
#00 #0=NOP   \ {}  no operation
#01 #0=RETZ  \ {l} return if zero
#02 #0=RET   \ {}  return
#03 #0=YLD   \ {} yield control to another fiber
#04 #0=SWP   \ {l r -> r l}    swap
#05 #0=DRP   \ {l   -> }       drop
#06 #0=OVR   \ {l r -> l r l}  over
#07 #0=DUP   \ {l   -> l l}    duplicate
#08 #0=DUPN  \ {l   -> l l==0} DUP then NOT
#09 #0=DV    \ Device Operation
#0A #0=RG    \ {-> v}  Register
#0F #0=END   \ not actual instr, used in tests.

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

\ # [1.d] Sizes
#00 #0=SZ1
#10 #0=SZ2
#20 #0=SZ4

\ # [1.e] Mem|Store              |Description
#40 #0=FT    \ {addr} -> {value}  |FeTch value from addr
#41 #0=FTBE  \ {addr} -> {value}  |FeTch value from addr (big endian)
#42 #0=FTO   \ {addr} -> {value}  |FeTch value from addr + U1 literal offset
#43 #0=FTLL  \ {} -> {local}      |FeTch from LP + U1 literal offset
#44 #0=FTGL  \ {} -> {global}     |FeTch from GB + U2 literal offset
#45 #0=SR    \ {value addr} -> {} |Store value at addr
#46 #0=SRBE  \ {value addr} -> {} |Store value at addr (big endian)
#47 #0=SRO   \ {value addr} -> {} |Store value at addr + U1 literal offset
#48 #0=SRLL  \ {value} -> {}      |StoRe value at LP + U1 literal offset
#49 #0=SRGL  \ {value} -> {}      |StoRe value at GB + U2 literal offset
#4A #0=LIT   \ {} -> {literal}    |Literal (U1, U2 or U4)

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

\ Common instr+szs
@SZ2 @XSL  ^JN   #0=XSL2
@SZ2 @JMPL ^JN   #0=JMPL2

\ # [1.g] Small Literal [0xC0 - 0xFF]
#C0 #0=SLIT

\ JZL and JMPL for SZ=1
\ For SZ=1 they jump to the 1 byte signed offset from the location
\ of the operation (NOT the location of the literal).

\ **********
\ * [2] Registers and Device Operations: RGXX|DVXX w/ 1 byte literal

\ Device Operations, single byte literal consumed by DV instr
\
\ DV is the primary mechanism to communicate complex logic with the kernel.
\ Many of these consume a pointer to a role of {&methods &data}. If the
\ "methods" pointer in that role is null, then the kernel uses a native
\ implementation. For instance, for FRole (&File &FileMethods=NULL) the data
\ will be an index into the kernel's file manager.
\

#00 #0=D_cede     \ {} cede, allowing another thread to run
#01 #0=D_assert   \ {chk errCode} if(not chk) panic(errCode)
#02 #0=D_catch    \ {&xlw -> errCode} execute xlw returning err
#03 #0=D_memset   \ {&dst v:U1 len} set dst to v
#04 #0=D_memcmp   \ {&a &b len -> cmp} compare a and b
#05 #0=D_memmove  \ {&dst &src len} dst = src [of len]
#06 #0=D_bump     \ {size aligned &bba -> &mem} bump allocate size
#07 #0=D_log      \ { ... len lvl} log len integers to com
#08 #0=D_file     \ {method:U1 f:FRole} run a file method with kernel support
#09 #0=D_comp     \ {...} run a compiler method (see below)
#0A #0=D_dict     \ {slc root:&DNode-> &DNode cmp} perform dict_find

\ D_comp is a toolbox of compiler functionality which has to be implemented
\ in the kernel anyway. Allowing it to be usable by spor reduces the complexity
\ of bootstrapping considerably.
#00 #0=D_comp_heap    \ {-> heap} get the current heap (depends on cstate C_PUB)
#01 #0=D_comp_last    \ {-> &DNode} last dictionary node modified
#02 #0=D_comp_wsLen   \ {-> wsLen} get working stack length
#03 #0=D_comp_block   \ {} start new block
#04 #0=D_comp_dGet    \ {-> &DNode} get src.b[0:src.plc] from any "base" dict
#05 #0=D_comp_dAdd    \ {v m2} set current (priv or pub) dictionary to v + meta
#06 #0=D_comp_read1   \ {} read (at least) single byte in src
#07 #0=D_comp_readEol \ {} read src until EOL (for comments), incrementing b.len
#08 #0=D_comp_scan    \ {} "scan" token into start of buffer. Sets b.len.

\ The RG 1 byte literal has the following byte format. Note: FT will return the
\ register value + offset, SR will store the value + offset in the register.
\   1OOO OOOO: (R_LP) local stack pointer with 7bit offset (O)
\   0XXX XXXR: register. X is currently undefined.
#80 #0=R_LP \ local stack pointer
#00 #0=R_EP \ execution pointer, SR will panic
#01 #0=R_GB \ global base pointer


\ **********
\ * [3] Constants
\ Token Groups
#0  #0=T_NUM
#1  #0=T_HEX
#2  #0=T_ALPHA
#3  #0=T_SINGLE
#4  #0=T_SYMBOL
#5  #0=T_WHITE

#30 #0=SZ_MASK \ size bit mask (for instr and meta)

#0040 #0=C_PUB        \ G_cstate AND meta0: store as public (function data)
#4000 #0=C_PUB_NAME   \ G_cstate: make next name public
#2000 #0=C_EXPECT_ERR \ G_cstate: expeting error (for testing)

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

\ \ * [3.b] Zoab
\ #C0 #0=ZOAB_TY   \ bitmask: all types
\ #80 #0=ZOAB_JOIN \ bitmask: join type
\ #40 #0=ZOAB_ARR  \ bitmask: arr type
\ #C0 #0=ZOAB_PTR  \ equality: next 4 bytes are a pointer.
\ 
\ \ * [3.c] Log Levels
#10 #0=LOG_USER
#1F #0=LOG_TRACE
#17 #0=LOG_DEBUG
#13 #0=LOG_INFO
#11 #0=LOG_WARN
#10 #0=LOG_CRIT

\ \ Language Level (builtin) Logs
\ #27 #0=LOG_INSTR
\ #23 #0=LOG_EXECUTE
\ #21 #0=LOG_ASM
\ #20 #0=LOG_COMPILER
