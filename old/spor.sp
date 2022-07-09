\ This file bootstraps spor from the native (i.e. C) implementation into
\ a more full-featured language with helpful macros. It depends on
\ kernel/kernel_c.sp for constants.
\
\ # Table of Contents
\ Search for these headings to find information
\
\ [1 ... 4] see kernel/kernel_c.sp
\ [5] Bootstrap Macros: necessary functionality for the rest of the language.
\ [6] Core functions and macros: bread and butter of spor assembly
\ [7] ASM Flow Control (IF, LOOP, etc)
\ [8] xx, jmpl, Scanning and Alignment
\ [9] globals and locals
\ [10] Zoa strings and logging zoab
\ [11] Fngi compile loop


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

$_h @_FP=select \ {a b s -> a|b} a if s else b
  .1%JZL #3.1, %DRP %RET \ if(s) ret a
  %SWP %DRP %RET         \ ret b

$_h @_FP=h1  \ h1: {val:1} push 1bytes from stack to heap
  .R%FTGL @heap.2, .1%SR    \ store 1 byte value at heap
  .R%FTGL @heap.2,  %INC  .R%SRGL @heap.2, \ heap=heap+1
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
  .R%FTGL @heap.2, .2%XSL @srBE2.2, \ store value at heap
  .R%FTGL @heap.2, \ {heap}
  %INC2   .R%SRGL   @heap.2,   \ heap=heap+2
  %RET

$_h @_FP=h4  \ h4: {val:4} push 4bytes from stack to heap
  .R%FTGL @heap.2, .2%XSL @srBE4.2, \ store value at heap
  .R%FTGL @heap.2,  %INC4     .R%SRGL @heap.2, \ heap=heap+4
  %RET

$_h @TY_FN=kdictArgs \ [ -> &kdict] args for dict.
  %RGFT@R_GB$h1 .2%LIT @kdictRef$h2 %ADD %RET

$_h @TY_FN=_dict
  @D_scan$L0  %DVFT .2%JMPL @kdictArgs$h2

\ TODO: remove
$_h @TY_FN=kdictSet \ kdictSet <token> {meta}: Set "global" dictionary to next token.
  .2%XSL @_dict$h2  @D_dict$L0   %DVSR  \ set dict key
  .R%FTGL @kdictRef$h2  .2%FTGL @kdictLen$h2
    %ADD  .R%SRGL @ldictRef$h2 \ ldictRef = kdictRef + kdictLen
  #0$L0 .2%SRGL @ldictLen$h2   \ ldictLen = 0
  .2%FTGL @kdictCap$h2  .2%FTGL @kdictLen$h2
    %SUB  .2%SRGL @ldictCap$h2 \ ldictCap = kdictCap - kdictLen
  #0$L0 .2%SRGL @localOffset$h2 %RET

$_h @TY_FN=kdictGet \ kdictGet: Get the value of the next token.
  .2%XSL @_dict$h2   @D_dict$L0   %DVFT  %RET

\ MIGRATE: use dictRef instead
$_h @TY_FN=kdictGetK \ kdictGetK: Get the &key of the next token.
  .2%XSL @_dict$h2   @D_dictK$L0   %DVFT  %RET

$_h @TY_FN=loc \ {meta} $loc <name>: define location
  .R%FTGL @heap$h2 %SWP \ {heap meta}
  .2%XSL @_dict $h2  @D_dict$L0   %DVSR  \ set dict key
  .R%FTGL @kdictRef$h2  .2%FTGL @kdictLen$h2
    %ADD  .R%SRGL @ldictRef$h2 \ ldictRef = kdictRef + kdictLen
  #0$L0 .2%SRGL @ldictLen$h2   \ ldictLen = 0
  .2%FTGL @kdictCap$h2  .2%FTGL @kdictLen$h2
    %SUB  .2%SRGL @ldictCap$h2 \ ldictCap = kdictCap - kdictLen
  #0$L0 .2%SRGL @localOffset$h2 %RET


\ Assert checks a condition or panics with an error
\ ex: <some check> @E_myError assert
@_FP@TY_FN_INLINE^JN $loc assert #2$h1 @D_assert$L0 %DVFT %RET  \ note: inline function
@_FP$loc assertNoNow  .2%LIT @E_cNoNow$h2 \ [fallthrough]
@_FP$loc assertNot    %SWP %NOT %SWP  @D_assert$L0 %DVFT %RET
@_FP$loc tAssert      .2%LIT @E_test$h2 @D_assert$L0 %DVFT %RET
@_FP$loc tAssertNot   %NOT .2%JMPL @tAssert,
@_FP$loc assertLt128   .1%LIT #80$h1 %LT_U  .2%LIT @E_cJmpL1$h2 @D_assert$L0 %DVFT %RET
@_FP$loc tAssertEq \ {a b}
   @ERR_DATA_INT2$L0  .1%SRGL @errValTy$h2
        .R%SRGL @errVal2$h2 \ b {a}
   %DUP .R%SRGL @errVal1$h2 \ a {a}
        .R%FTGL @errVal2$h2 \ {a b}
  %EQ   .2%JMPL @tAssert,

\ **********
\ * [6] Core functions and macros
\ These are the bread and butter of spor assembly needed to create the fngi compiler.
\ They define and call functions, check the type of dictionary entries,
\ provide local dictionary support, etc.
\
\ fn $_xsl <token> / $_jmp <token>: compile an XSL or JMPL to key
\ fn L1 / L2 / L4 / LA  [U] -> [] : compile 1 / 2 / 4 / RSIZE byte literal.
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
\ fn updateGkey [ -> &key]      : update gkey=dictLen and return it
\ fn ldictRef / ldictArgs / ldictLen  : interface directly with local dict
\ fn ldictSet / ldictGet / ldictGetK  : set/get/get-ref of local dict key
\
\ Note: any SYN function must be prefixed with asNow (typically #0)
\ since it will not be tagged as SYN until makeFn.

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

@L2 @L4    @RSIZE #2 ^EQ  $select @_FP=LA \ {UA} comipile RSIZE literal

@_FP@TY_FN_INLINE^JN $loc keyMeta \ {&key -> meta} get key's meta.
  #2$h1 %INCA .1%FT %RET \ Note: direct inline function. Two bytes

\ These take {&key} and tell information about it
@_FP$loc isTyConst   $keyMeta  @META_TY_MASK$L1 %MSK  @TY_CONST$L1 %EQ %RET
@_FP$loc isTyFn      $keyMeta  @META_TY_MASK$L1 %MSK  @TY_FN$L1  %EQ %RET
@_FP$loc isFnLarge   $keyMeta  @TY_FN_LARGE$L1 %MSK %RET
@_FP$loc assertNotNull @E_null$L2 $_jmp assert
@_FP$loc assertFn   $_xsl isTyFn  @E_cNotFn $L2  $_jmp assert \ [&key] -> []
@TY_FN$loc assertWsEmpty   @D_wslen$L0 %DVFT  @E_wsEmpty $L2  $_jmp assertNot
$assertWsEmpty

@_FP$loc assertFnSmall \ [&key]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $_jmp assertNot

@_FP$loc assertFnLarge \ [&key]
  %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX $L2  $_jmp assert

@_FP$loc toMod @MOD_MASK $L4 %MSK %RET \ {ref} -> {mod}
@_FP$loc isSameMod \ {ref ref} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET

@_FP$loc curMod   .2%FTGL @gkey$h2 .R%FT  $_jmp toMod \ [] -> [mod]
@_FP$loc isCurMod $_xsl toMod  $_xsl curMod %EQ %RET     \ [ref] -> [isCurMod]
@_FP$loc assertCurMod  $_xsl isCurMod  @E_cMod$L2  $_jmp assert

@_FP$loc _jSetup \ [&key] -> [ref]: checked jmp setup
  %DUP $_xsl assertFnSmall
  .R%FT %DUP $_jmp assertCurMod \ {ref}

@TY_FN$loc updateGkey \ [] -> [&key] update and return current key
  .R%FTGL @kdictRef$h2 \ dict.buf
  .2%FTGL @kdictLen$h2 \ dict.heap
  %ADD \ {&newKey}
  %DUP .R%SRGL @gkey$h2 \ gkey=newKey
  %RET \ return &key
\ FIXME: remove literal 0
@TY_FN$loc locK $_xsl updateGkey %SWP $_jmp loc \ { <token> meta -> &key} def loc, ret &key

@_FP$loc keyJnMeta \ {&key meta:U1} -> U4 : apply meta to &key
  %OVR %INCA \ {... &key newmeta &meta} note: #0 for unregistered SYN
  .1%FT %JN    \ {&key newMeta}
  %SWP %INCA .1%SR \ update meta
  %RET

@_FP$loc dictSetMeta \ {<dictArgs> meta:U1 &key} update dict key's meta.
  %SWP %OVR \ {<dictArgs> &key meta &key}
  %SWP $_xsl keyJnMeta \ {<dictArgs> &key}
  @D_dictDump$L0 %DVFT %RET \ dict dump entry

\ END: used for INLINE, IF/ELSE and BREAK0
@_FP $loc _END \ {&addr heapDiff} addr is where to store (heap-heapDiff)
  .R%FTGL @heap$h2             \ {&addr heapDiff heap}
  %SWP %SUB  %DUP $_xsl assertLt128 \ {heapDiff (heap-heapDiff)}
  %SWP .1%SR %RET \ store at &addr (1 byte literal)
@TY_FN@TY_FN_SYN^JN $loc END_INLINE $_xsl assertNoNow %DUP %INC $_jmp _END

\ example: $FN <token> $SYN $LARGE: declare a function with attributes.
@TY_FN@TY_FN_SYN^JN$loc FN
  $_xsl assertNoNow $_xsl assertWsEmpty @TY_FN$L1 $_xsl locK %DRP %RET
@TY_FN@TY_FN_SYN^JN$loc SYN     %DRP .R%FTGL @gkey$h2  @TY_FN_SYN$L1   $_jmp keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc NOW     %DRP .R%FTGL @gkey$h2  @TY_FN_NOW$L1   $_jmp keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc LARGE   %DRP .R%FTGL @gkey$h2  @TY_FN_LARGE$L1 $_jmp keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc PRE %DRP .R%FTGL @gkey$h2  @TY_FN_PRE$L1   $_jmp keyJnMeta
@TY_FN@TY_FN_SYN^JN$loc INLINE  %DRP
  .R%FTGL @heap$h2 #0$L0 $_xsl h1 \ put heap on stack and write 0 to heap
  .R%FTGL @gkey$h2  @TY_FN_INLINE$L1 $_jmp keyJnMeta \ set meta as TY_FN_INLINE

$assertWsEmpty

$FN isFnPre     $PRE $keyMeta  @TY_FN_PRE$L1     %MSK %RET
$FN isVarInput  $PRE $keyMeta  @TY_VAR_INPUT$L1  %MSK %RET
$FN isFnNormal  $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NORMAL$L1 %EQ %RET
$FN isFnNow     $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NOW$L1    %EQ %RET
$FN isFnSyn     $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_SYN$L1    %EQ %RET
$FN isFnInline  $PRE $keyMeta  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_INLINE$L1 %EQ %RET
$FN isTyVar     $PRE $keyMeta  @META_TY_MASK$L1  %MSK  @TY_VAR$L1       %EQ %RET
$FN assertTyVar $PRE $_xsl isTyVar  @E_cNotLocal$L2 $_jmp assert

$FN scan       $INLINE @D_scan$L0 %DVFT $END_INLINE %RET
$FN panic   $PRE $INLINE #0$L0 %SWP  $_xsl assert $END_INLINE \ {errCode}: panic with errCode
$FN unreach      @E_unreach$L2 $_xsl panic \ {}: assert unreachable code
$FN unimplIfTrue $PRE @E_unimpl$L2 $_jmp assert \ {}: if true raise unimpl
$FN tAssertKeyMeta $PRE %SWP $keyMeta %SWP $_jmp tAssertEq \ {&key meta}
$FN assertSzI $PRE \ {szI}
  %DUP #CF$L1 %MSK @E_sz$L2 $_xsl assertNot \ non-sz bits empty
  #4$L0 %SHR #3$L1 %LT_U @E_sz$L2 $_jmp assert \ sz bits < 3

$FN ftoN $NOW  \ {offset szI} compile FTO szI w/offset
  %DUP $_xsl assertSzI  @FTO$L1 %ADD $_xsl h1 $_jmp h1

\ Update the harness with the new dictionary
$FN dictDump       $_xsl kdictArgs @D_dictDump$L0 %DVSR %RET \ {}
$dictDump

$FN ldictArgs \ {} -> dictArgs
  @ldictRef$L2 %RET \ TODO: add R_GB to it.

$FN _ldict $scan $_jmp ldictArgs
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
  .R%FTGL @heap$h2 \ {&jmpTo} push &jmpTo location to stack
  #0$L0  $_xsl h1 \ compile 0 (jump pad)
  %RET

@TY_FN@TY_FN_SYN^JN $loc END $_xsl assertNoNow %DUP $_jmp _END

$FN ELSE $SYN $_xsl assertNoNow \ {&ifNotJmpTo} -> {&elseBlockJmpTo}
  @JMPL $c1         \ (end IF) compile unconditional jmp to end of ELSE
  .R%FTGL @heap$h2 %SWP \ {&elseBlockJmpTo &ifNotJmpTo}
  #0$L0 $_xsl h1     \ compile jmp lit for &elseBlockJmpTo
  %DUP $_jmp _END   \ end of IF block (beginning of ELSE)

\ $LOOP l0 ... $BREAK0 b0 ... $AGAIN l0  $BREAK_END b0
$FN LOOP   $SYN $_xsl assertNoNow .R%FTGL @heap$h2  $_jmp ldictSet
$FN BREAK0 $PRE $SYN   $_xsl IF $_jmp ldictSet
$FN BREAK_IF  $PRE $SYN @NOT$c1  $_jmp BREAK0 \ break if true
$FN BREAK_EQ  $PRE $SYN @NEQ$c1  $_jmp BREAK0 \ break if equal
$FN BREAK_NEQ $PRE $SYN @EQ$c1  $_jmp BREAK0 \ break if equal
$FN AGAIN $SYN $_xsl assertNoNow
  @JMPL $c1  \ compile jmp
  .R%FTGL @heap$h2  \ {heap}
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
\ fn read [ -> numRead]         : attempt to read bytes from src.
\ fn readNew [ -> numRead]      : clear token buf and read bytes.
\ fn scanNoEof []               : scan and assert not EOF.
\ fn peekChr [ -> c]            : peek at the next character.
\
\ fn assertSzI [szI]              : assert that szI is valid
\ fn isEof [ -> bool]           : return whether there was a token scanned
\ fn assertToken []             : assert there is a token
\ fn assertNoEof [numRead]      : assert that numRead > 0 (E_eof)

$FN keySzI $PRE $keyMeta @SZ_MASK$L1 %MSK %RET \ {&key -> szI}

$FN szIToSz $PRE \ {szI} -> {sz}
  %DUP @SZ1$L1 %EQ $IF  %DRP #1$L0 %RET  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP #2$L0 %RET  $END
       @SZ4$L1 %EQ $IF       #4$L0 %RET  $END
  @E_sz$L2 $_xsl panic

$FN hN $PRE \ {value szI} write a value of szI to heap
  %DUP @SZ1$L1 %EQ $IF  %DRP $_jmp h1  $END
  %DUP @SZ2$L1 %EQ $IF  %DRP $_jmp h2  $END
  %DUP @SZ4$L1 %EQ $IF  %DRP $_jmp h4  $END
  @E_sz$L2 $_xsl panic

$FN szToSzI $PRE \ [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L0 %EQ $IF  %DRP @SZ1 $L1 %RET  $END
  %DUP #2$L0 %EQ $IF  %DRP @SZ2 $L1 %RET  $END
       #4$L0 %EQ $IF  %DRP @SZ4 $L1 %RET  $END
  @E_sz$L2 $_xsl panic

$FN reqAlign $PRE \ {sz -> sz}: get required alignment
  %DUP @RSIZE$L0 %DEC %LT_U $retif  %DRP @RSIZE$L0 %RET

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
  @E_sz$L2 $_xsl panic

$FN srSzI \ {value &addr szI}
  %DUP @SZ1$L1 %EQ $IF %DRP .1%SR %RET $END
  %DUP @SZ2$L1 %EQ $IF %DRP .2%SR %RET $END
       @SZ4$L1 %EQ $IF      .4%SR %RET $END
  @E_sz$L2 $_xsl panic

\ TODO: need to make isNotEof INLINE function. Possibly get rid of all isEof uses?
$FN isEof .2%FTGL@tokenPlc$h2 %NOT %RET
$FN assertToken .2%FTGL@tokenPlc$h2 @E_cNeedToken$L2 $_jmp assert
$FN assertNoEof $PRE @E_eof$L2 $_jmp assert \ {numRead}

$FN scanNoEof
  $scan
  $_xsl isEof
  @E_eof$L2 $_jmp assertNot

$FN peekChr \ {} -> {c} peek at a character
  $scan
  $_xsl isEof $IF  #0$L0 %RET  $END
  .R%FTGL@tokenDat$h2 .1%FT \ {c}
  #0$L0 .2%SRGL@tokenPlc$h2 %RET \ reset scanner for next scan

$FN read $INLINE #1$L0 @D_read$L0 %DVFT $END_INLINE %RET \ { -> numRead}
$FN readNew \ { -> numRead} clear token buf and read bytes
  #0$L0 .2%SRGL@tokenPlc$h2
  #0$L0 .2%SRGL@tokenSize$h2
  $read %RET

\ PORTING: just use dictRef/D_comp_dGet instead.
$FN dictK \ {} -> {&key isFromLocal}
  $_xsl ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF
    @TRUE$L1 %RET
  $END %DRP
  $_xsl kdictArgs  @D_dictK$L0 %DVFT %DUP  $IF
    @FALSE$L0 %RET
  $END @E_cNotType$L2 $_xsl panic

\ PORTING: not needed, just use D_comp_last
$FN updateLkey \ [] -> [&key] update and return current local key
  .R%FTGL @ldictRef$h2 \ dict.buf
  .2%FTGL @ldictLen$h2 \ dict.heap
  %ADD \ {&newLKey}
  %DUP .R%SRGL @lkey$h2 \ gkey=newKey
  %RET \ return &key

$FN xSzI $PRE \ {&key} -> {szI}: return the size requirement of the X instr
   .R%FT $_xsl isCurMod $IF  @SZ2$L1 %RET  $END  @SZA$L1 %RET

\ {&key szInstr szLit instr} compile a literal memory instr.
\   szLit the size of the literal to compile for the instr.
\   oRef: either a reference or an offset
$FN instrLitImpl $PRE $LARGE
  #1 $h1 \ 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 \ var instr          {&key szInstr szLit}
  .1%SRLL #0$h1 \ var szLit          {&key szInstr}
  .1%FTLL #1$h1 %JN  $_xsl h1 \ compile (szInstr | instr) {&key}
  .R%FT \ {oRef} offset or reference
  .1%FTLL #0$h1  $_jmp hN \ compile literal of proper instrSz

$FN c_fn $PRE \ {&key}: compile a function of any type
  %DUP $_xsl assertFn \ {&key}
  %DUP $_xsl isFnInline $IF \ Inline compilation {&key}
    %DUP $_xsl isFnLarge @E_cInlineLarge$L2 $_xsl assertNot
    .R%FT .R%FTGL @heap$h2 %OVR \ {&inlineFn &heap &inlineFn}
    %DUP %INC %SWP .1%FT \ {&inlineFn &heap &inlineFn+1 inlineLen}
    @D_memSet$L0 %DVSR  \ memMove {&inlineFn}
    .1%FT .R%FTGL @heap$h2 %ADD .R%SRGL @heap$h2 %RET \ update heap+inlineLen
  $END
  %DUP $_xsl xSzI     \ {&key szLit}
  %OVR $_xsl isFnLarge  $IF @XLL$L1 $ELSE @XSL$L1 $END \ {&key instrSzI instr}
  %OVR %SWP \ {&key instrSzI litSzI instr} instr & lit are same sz
  .2%XLL @instrLitImpl$h2 %RET

$FN execute \ {&key} -> {...}: execute a dictionary key
  %DUP $_xsl isFnInline $IF \ if inline, assert not large and jmp to addr+1
    %DUP $_xsl isFnLarge @E_cInlineLarge$L2 $_xsl assertNot  .R%FT %INC .R%JMPW
  $END
  %DUP $_xsl isFnLarge  $IF .R%FT .R%XLW %RET $END
  .R%FT .R%JMPW

$FN colon \ consume a colon token as syntactic surgar, i.e. xx:foo
  $scan .2%FTGL@tokenPlc$h2 #1$L0 %EQ @E_cColon$L2 $_xsl assert \ assert len=1
  .R%FTGL@tokenDat$h2 .1%FT  #3A$L0 %EQ @E_cColon$L2 $_xsl assert \ assert ":"
  %RET

$FN _xxPre $LARGE
  #1$h1 $_xsl assertNoNow \ locals 0=&key
  $_xsl colon $_xsl kdictGetK %DUP .R%SRLL #0$h1 \ {&key}
  \ if fn is (PRE or SYN) and a compFn exists, compile next token.
  %DUP $_xsl isFnPre %SWP $_xsl isFnSyn %OR .R%FTGL @compFn$h2 %AND $IF
    .R%FTGL @compFn$h2 .4%XLW
  $END .R%FTLL #0$h1 %RET

$FN xx $SYN .2%XLL @_xxPre$h2 $_jmp c_fn
$FN jmp $SYN
  $xx:_xxPre %DUP $xx:isFnLarge @E_cXHasL$L2 $xx:assertNot
  .R%FT @JMPL2$L1 $_jmp _j2

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
  $xx:updateLkey \ {&key}
  #0$L0 $xx:ldictSet \ initialize lDict token (to heap for now)
  %DUP @TY_VAR$L1 $xx:keyJnMeta \ {&key} update meta as local
  @TRUE$L0 %RET \ {&key isLocal=TRUE}

$FN declVar $LARGE $PRE \ {&key isLocal meta szBytes} declare a variable (global or local)
  #1$h1 .1%SRLL #0$h1  .1%SRLL #1$h1   .1%SRLL #2$h1 \ Locals 0=szBytes 1=meta 2=isLocal
  %DUP .1%FTLL #1$h1 $xx:keyJnMeta \ update key meta {&key}
  .1%FTLL #2$h1 $IF \ if(isLocal) {&key}
    .2%FTGL @localOffset$h2  .1%FTLL #0$h1  $xx:alignA \ {&key offsetAligned}
    %DUP .1%FTLL #0$h1 %ADD .2%SRGL @localOffset$h2 \ update localOffset {...}
  $ELSE
    .R%FTGL @gheap$h2        .1%FTLL #0$h1  $xx:alignA \ {&key gheapAligned}
    %DUP .1%FTLL #0$h1 %ADD .R%SRGL @gheap$h2       \ update gheap {...}
  $END %SWP .R%SR %RET \ update key's value

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
  $xx:instrLitImpl %RET

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
  $xx:kdictGetK  %DUP $xx:assertTyVar .R%FT %RGFT @R_GB$h1 %ADD %RET

$FN _refImpl \ {}
  $xx:ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {&key}
    %DUP $xx:assertTyVar \ {&key}
    .R%FT %DUP #40$L1 %LT_U @E_cReg$L2 $_xsl assert \ {offset}
    @R_LP$L1 %JN  \ {LpOffset}: offset is lower 7 bits
    @RGFT$c1 $jmp:h1  \ compile: %RGFT (@R_LP + offset)$h1
  $END %DRP
  $xx:kdictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {&key}
    %DUP $xx:assertTyVar  .R%FT $jmp:LA \ write literal directly TODO: use lit
  $END @E_cNotType$L2 $_xsl panic

$FN REF  $SYN
  $IF \ asNow: we can get &fn or &global
    $xx:kdictGetK \ next: assert(isTyVar or isTyFn)
      %DUP $xx:isTyVar %OVR $xx:isTyFn %OR
      @E_cNotType$L2 $_xsl assert
    .R%FT %RET
  $END \ else: we can get &local or &global
  $scan $jmp:_refImpl

$FN GET  $SYN
  $IF  $xx:kdictGetK %DUP $xx:assertTyVar %DUP $xx:keySzI \ {&key szInstr}
       %SWP .R%FT %SWP $jmp:ftSzI   $END
  $scan $xx:dictK $jmp:_getImpl

$FN SET $SYN
  $xx:assertNoNow $scan $xx:dictK $jmp:_setImpl

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
  %DUP  .R%SRLL#0$h1 \ {&key} var key
  .R%FTGL @ldictRef$h2 .2%FTGL @ldictLen$h2 %ADD \ {&key &ldictEnd}
    $reteq \ return if key=ldictEnd
  .R%FTLL#0$h1  $xx:Dict_nextKey  $xx:_compileInputs \ get the next key and recurse {}
  .R%FTLL#0$h1  %DUP $xx:isTyVar %SWP \ {isTyVar &key}
  $xx:isVarInput %AND %RETZ \ {} all of (isTyVar isVarInput)
  .R%FTLL#0$h1  %DUP $xx:keySzI \ {&key szInstr}
  @SZ1$L1 @SRLL$L1 $xx:instrLitImpl %RET

\ End locals declaration. Update FN slots and LARGE.
\ Compiles SRLL for each TY_VAR_INPUT, in reverse order.
$FN declEnd
  $GET localOffset $IF \ if localOffset: update fn to large
    $GET gkey @TY_FN_LARGE$L0 $xx:keyJnMeta
  $END
  $GET localOffset #4$L0 $xx:align
  @APO2$L0 %SHR $xx:h1 \ update number of slots
  $GET ldictRef  $xx:_compileInputs
  %RET

\ **********
\ * [11] Fngi compile loop
\ The fngi compile loop is implemented in spore. In addition to the compile loop itself,
\ this section implements several essential parsing functions that can be used
\ in other areas of fngi.
\
\ Globals:
\   global compFn: a function reference used to compile individual tokens.
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
\ fngi                          : fngi compile loop.
\ fn fngiSingle [ -> ...]         : starting (base) compFn
\ fn single [asNow -> ...]  : compile/execute a single token (compFn).
\ fn fn [&key]                  : compile a function (small or large)
\ fn execute [&key]               : execute a function (small or large)
\ fn parseNumber [ -> value isNum]: parse token as number
\ fn lit [U4]                     : compile literal
\ fn xSzI [&key -> szI]        : return szI of the fn
\
\ fn charNext [ -> c]           : read next character (WARN: AFTER tokenPlc)
\ fn charNextEsc [ -> c unkEsc] : read an escapeable character (string).
\ fn updateCompFn [newComp -> prevComp] : update compFn + ret old
\ fn number <number> -> [value isNum]   : parse next token (parseNumber).

$FN betweenIncl $PRE \ {value a b} -> a <= value <= b
  $declL b  @SZA@TY_VAR_INPUT^JN  @RSIZE $declVar
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

\ {} -> {c}: read next character from AFTER tokenPlc.
\ Increments tokenPlc. This is destructive to token, use with care.
$FN charNext
  $GET tokenPlc  $GET tokenSize %GE_U $IF
    $xx:readNew  $xx:assertNoEof
  $END
  $GET tokenDat  $GET tokenPlc  %ADD .1%FT
  $GET tokenPlc %INC  $SET tokenPlc %RET

\ {} -> {char unknownEscape} read a character that can be escaped.
$FN readCharEsc
  $xx:charNext \ {char}
  %DUP #5C$L1 %NEQ $IF @FALSE$L0 %RET $END \ if(c != '\\') ret;
  \ c is an escape character: \
  %DRP $xx:charNext
  %DUP #5C$L1 %EQ $IF             @FALSE$L0 %RET $END \ \\: escape
  %DUP #74$L1 %EQ $IF %DRP #09$L0 @FALSE$L0 %RET $END \ \t: tab
  %DUP #6E$L1 %EQ $IF %DRP #0A$L0 @FALSE$L0 %RET $END \ \n: newline
  %DUP #20$L1 %EQ $IF %DRP #20$L0 @FALSE$L0 %RET $END \ \ : space
  %DUP #78$L1 %EQ $IF \ \xHH
    \ charToInt(charNext) << 8 + charToInt(charNext)
    %DRP $xx:charNext  $xx:charToInt #8$L0  %SHL
    $xx:charNext       $xx:charToInt %ADD
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

$FN parseBase \ {<token> i base -> value isNumber}
  $declL i      @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL base   @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL value  @SZA                  @RSIZE $declVar
  $declEnd
  $GET base #FE$L1 %EQ $IF \ if special 'character' base
      $GET i $SET tokenPlc \ readCharEsc is off END of tokenPlc
      $xx:readCharEsc  @E_cUnknownEsc$L2 $xx:assertNot  @TRUE$L0 %RET
  $END #0$L0 $SET value
  $LOOP l0
    $GET i  $GET tokenPlc $BREAK_EQ b0
    $GET tokenDat $GET i %ADD .1%FT \ {c}
    $xx:charToInt \ {v}
    \ return {0 0} if not integer character
    %DUP $GET base %GE_U $IF  @FALSE$L0 %RET  $END

    $GET base  $GET value %MUL \ {base * value}
    %ADD $SET value \ value = v + value*10
    $GET i %INC $SET i \ i += 1
  $AGAIN l0  $END_BREAK b0

  $GET i %NOT $IF  #0$L0 @FALSE$L0 %RET  $END \ no token
  $GET value @TRUE$L0 %RET

$FN parseNumber \ {} -> {value isNumber}
  #0$L0 #A$L0 \ {i=0 base=0xA}. Next: if (token len>=2 and starts with '0')
  $GET tokenPlc #2$L0 %GE_U $GET tokenDat .1%FT #30$L0 %EQ %AND $IF
    %DRP $GET tokenDat %INC .1%FT $xx:numBase %DUPN $IF \ {i base}
      %DRP #A$L0 \ base was = 0, just set back to 10
    $ELSE %SWP %INC2 %SWP $END \ else i+=2
  $END
  $xx:parseBase %RET

$FN lit  $PRE \ {U4} compile literal
  %DUP #40$L1 %LT_U        $IF  $jmp:L0  $END
  %DUP #FF$L1 %INC %LT_U   $IF  $jmp:L1  $END
  %DUP #FFFF$L2 %INC %LT_U $IF  $jmp:L2  $END
  $jmp:L4

\ {asNow value:U4} -> {?nowVal}: compile proper sized literal
\ if asNow=true the value is left on the stack.
$FN lit
  %SWP %NOT %RETZ \ if now, leave on stack
  $jmp:lit


$FN _compConstant $PRE \ {asNow} -> {&keyFn[nullable]}
  $xx:parseNumber \ {asNow value isNumber}
  $IF  $xx:lit #0$L0 %RET  $END %DRP \ {asNow}
  $xx:isEof $IF  %DRP #0$L0 %RET  $END \ {asNow}

  \ Handle local dictionary. Only constants allowed here.
  $xx:ldictArgs  @D_dictK$L0 %DVFT %DUP  $IF \ {asNow &key}
    %DUP $xx:isTyConst  @E_cNotFnOrConst$L2 $_xsl assert
    .R%FT $xx:lit  #0$L0 %RET
  $END %DRP \ {asNow}
  $xx:kdictArgs  @D_dictK$L0 %DVFT \ {asNow &key}
  %DUP $xx:isTyConst $IF \ Constant case {asNow &key}
    .R%FT $xx:lit #0$L0 %RET
  $END %SWP %DRP %DUP $jmp:assertFn \ {&key}

$assertWsEmpty

\ declare compFn = 0
#0  $gRef compFn  .4^SR

$FN updateCompFn $PRE \ {&newCompFn -> &prevCompFn}
  $GET compFn %SWP $SET compFn %RET

\ {asNow} -> {}: compile a single token.
\ This is the primary function that all compilation steps (besides spor
\ compilation) reduce to.
$FN single $PRE
  $declL asNow @SZ1 #1     $declVar
  $declL key   @SZA @RSIZE $declVar $declEnd
  \ Handle constants, return if it compiled the token.
  %DUP $SET asNow $xx:_compConstant %DUP $SET key %RETZ
  $GET key $xx:isFnPre $IF $GET compFn .R%XLW $END \ recurse for PRE
  $GET key $xx:isFnSyn $IF $GET asNow $GET key $jmp:execute    $END
  $GET key $xx:isFnNow $IF $GET asNow @E_cReqNow$L2 $_xsl assert $END
  $GET key $GET asNow $IF  $jmp:execute  $END  $jmp:fn

$FN number $scan $xx:parseNumber %RET \ compile next token as number.

$FN (  $SYN%DRP  \ parens ()
  $xx:assertToken
  $xx:peekChr #29$L0 %EQ $IF  $scan %RET  $END \ return if we hit ")"
  $LOOP l0
    $GET compFn .R%XLW
    $xx:assertToken
    $xx:peekChr #29$L0 %EQ $IF  $scan %RET  $END \ return if we hit ")"
  $AGAIN l0

$FN _spor
  $declL compFn  @SZA  @RSIZE $declVar $declEnd
  @_spor$L2  $xx:updateCompFn $SET compFn \ update compFn and cache
  $xx:scanNoEof
  @D_comp$L0  %DVFT \ compile next token as spor asm
  $GET compFn $SET compFn %RET

$FN spor $SYN $xx:assertNoNow $xx:_spor %RET \ compile as assembly

$FN now \ used in $ to make next token/s run NOW.
  $declL compFn  @SZA  @RSIZE $declVar $declEnd
  @now$L2  $xx:updateCompFn $SET compFn \ update compFn and cache
  $xx:scanNoEof
  @TRUE$L0 $xx:single  \ compile next token as NOW
  $GET compFn $SET compFn %RET

$FN $ $SYN $xx:assertNoNow $xx:now %RET \ make NOW

$FN _comment \ used in \ to make next token ignored (comment)
  $declL compFn  @SZA  @RSIZE $declVar $declEnd
  @_comment$L2  $xx:updateCompFn $SET compFn
  $xx:scanNoEof
  \ Execute an open paren, else ignore
  $GET tokenDat .1%FT #28$L0 %EQ $IF @TRUE$L0 $xx:single $END
  $GET compFn $SET compFn %RET

\ {-> c} peek at the char after current token.
$FN peekNoScan
  $GET tokenPlc  $GET tokenSize %GE_U $IF
    $read $xx:assertNoEof \ ensure a char exists
  $END
  $GET tokenDatokenDatokenDatokenDat $GET tokenPlc %ADD .1%FT %RET

\ Comment, of which there are three forms.
\    \        : a line comment
\    \foo     : an inline comment, commenting out one token
\    \( ... ) : a block comment
$FN \
  $SYN %NOP %DRP
  \ Line comment if '\' is followed by space or newline
  $xx:peekNoScan #20$L0 %EQ
  $IF @D_scan$L0 %DVSR %RET $END \ scanEol
  $xx:_comment %RET \ else token comment

$FN ret $PRE $SYN $xx:assertNoNow @RET $c1 %RET \ ret 4, or just ret;

\ These do nothing and are used for more readable code.
$FN _ $SYN%DRP %RET  $FN , $SYN%DRP %RET  $FN ; $SYN%DRP %RET
$FN -> $SYN%DRP %RET

$FN fngi \ fngi compile loop
  $LOOP l0
    $GET tokenSize %RETZ \ exit on EOF
    $GET compFn .R%XLW
  $AGAIN l0

$FN fngiSingle \ base compFn for fngi tokens.
  #0$h1 $LARGE \ not really any locals (but called with XLW)
  $scan $GET tokenPlc %RETZ
  @FALSE$L0 $xx:single %RET

@fngiSingle $updateCompFn ^DRP

$dictDump
