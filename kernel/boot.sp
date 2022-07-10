\ Bootstrapping fngi.
\
\ Included by kernel: constants.sp, errors.sp, globals.sp
\
\ This file bootstraps fngi from spor assembly implemented natively (i.e. in C).
\
\ # Table of Contents
\ Search for these headings to find information
\
\ [1] Bootstrap Macros: necessary functionality for the rest of the language.
\ [2] Core functions and macros: bread and butter of spor assembly
\ [3] ASM Flow Control (IF, LOOP, etc)
\ [4] xx, jmpl, Scanning and Alignment
\ [5] globals and locals
\ [6] Zoa strings and logging zoab
\ [7] Fngi compile loop

\ **********
\ * [1] Core functions and macros
\
\ Storage Locations: these allow ultra-minimalism for embedded systems
\   fn pub                          : store next name in public storage + dict
\   fn STORE_PUB / STORE_PRIV       : switch storage to public / private
\
\ Assertions: these panic with the supplied errCode if cond is not met.
\   fn assert [cond errCode]
\   fn assertNot [cond errCode]
\
\ Test Assertions: these panic with E_test if the cond is not met.
\   fn tAssert, tAssertNot, tAssertEq
\
\ Internal functions: these are used to create other compiler functions
\   fn bump [size align -> &mem]    : bump memory from storage
\   fn select [a b s -> a|b]        : a if s else b
\   fn h1 [U1] -> []                : push 1 byte to heap
\   fn h2 [U2] -> []                : push 2 byte to heap (big endian)
\   fn L0 [U1] -> []                : compile a small literal  [#0 - #3F]
\   fn L1 [U1] -> []                : compile a 1 byte literal [#0 - #FF]
\   fn L2 [U1] -> []                : compile a 2 byte literal [#0 - #FFFF]
\   fn scan     [<token>]           : scan the next token in src
\   fn $dictRef [<token> &r -> &n]  : get node of token, root can be null.
\   fn $dictAdd [<token> v m2 -> &D]: set dict@token
\
\ Inline functions:
\   fn xCatch [... &fn]       : execute a large function and catch error.
\   fn retIfNot  [a]          : return immediately if not a  (     %RETZ)
\   fn retIf [a]              : return immediately if a      (%NOT %RETZ)
\   fn reteq [a b]            : return immediately if a == b (%NEQ %RETZ)


@TY_FN@TY_FN_SYN^JN :pub %DRP \ make next name public
  .2%LIT@C_PUB_NAME, %FTGL@G_cstate, %JN  %SRGL@G_cstate, %RET

$pub @TY_FN@TY_FN_NOW^JN :STORE_PUB \ switch to using public storage.
  .2%LIT@C_PUB,      %FTGL@G_cstate, %JN  %SRGL@G_cstate, %RET
$pub @TY_FN@TY_FN_NOW^JN :STORE_PRIV \ switch to using private storage.
  .2%LIT@C_PUB^INV, %FTGL@G_cstate, %MSK %SRGL@G_cstate, %RET

$STORE_PRIV
@_FP@TY_FN_INLINE^JN :BBA_bump \ {size align &BBA -> &dat}: bump memory from BBA
  .1#3, @SLIT@BBAm_bump    ^JN, %DV@DV_bba, %RET
@_FP@TY_FN_INLINE^JN :BBA_newBlock \ {&BBA}: create new block
  .1#3, @SLIT@BBAm_newBlock^JN, %DV@DV_bba, %RET
@_FP@TY_FN_INLINE^JN :BBA_drop \ {&BBA}: drop BBA (return all blocks)
  .1#3, @SLIT@BBAm_drop    ^JN, %DV@DV_bba, %RET

$STORE_PUB
$pub @TY_FN@TY_FN_NOW^JN :NEW_BLOCK_PUB \ start new public block
  .R%FTGL@G_bbaPub.2,  $BBA_newBlock %RET
$pub @TY_FN@TY_FN_NOW^JN :NEW_BLOCK_PRIV \ start new private block
  .R%FTGL@G_bbaPriv.2, $BBA_newBlock %RET

\ Using DV instr, we define a way to make assertions for tests.
     @_FP@TY_FN_INLINE^JN :dv_log    .1#2, %DV@DV_log,    %RET
$pub @_FP@TY_FN_INLINE^JN  :assertEq .1#2, %DV@DV_assert, %RET \ {l r err}
$pub @_FP :assertNot %SLIT %SWP $assertEq %RET \ { l err} assert l == 0
$pub @_FP :assert %SWP %NOT %SWP %SLIT %SWP $assertEq %RET \ { l err} assert l != 0
$pub @_FP :assertNotNull .2%LIT@E_null, .2%JMPL@assert,
$pub @_FP :tAssertEq     .2%LIT@E_test, $assertEq %RET \ {chk} test assert
$pub @_FP :tAssertNot       %SLIT .2%JMPL@tAssertEq, \ tAssertEq(_, 0)
$pub @_FP :tAssert
  %DUP .1%LIT#A0, %LIT#2, %LIT#10, $dv_log
  %NOT %SLIT .2%JMPL@tAssertEq, \ tAssertEq(not _, 0)
#55 #54^INC $tAssertEq     #0    $tAssertNot    #1 $tAssert
#0 #1       $assertNot     #1 #1 $assert        #1 $assertNotNull

$STORE_PRIV \ Note: everything until STORE_PUB is in the private space.

@TY_FN :getCState .2%FTGL@G_cstate, %RET  \ for assertions
$getCState@C_PUB^MSK $tAssertNot

@TY_FN :wsLen      .1@SLIT@DV_comp_wsLen^JN, %DV@DV_comp, %RET
@TY_FN :assertNoWs .2%XSL@wsLen,  .2%JMPL@tAssertNot,
$assertNoWs

$STORE_PUB  $getCState@C_PUB^MSK $tAssert
$pub @_FP@TY_FN_INLINE^JN :bump \ {size align -> &dat} bump some memory
  .1#4, @SLIT, $BBA_bump %RET \ {&U1} Note: #3 is size of INLINE

@_FP :h1 \ {U1}: store 1 byte on heap (unchecked)
  .1@SLIT#1^JN, @SLIT,  \ {U1 size=1 aligned=false}
  $bump .1%SR %RET

@_FP :L0   \ {U1} compile a small literal (unchecked)
  .1%LIT#3F, %MSK   \ {U1} truncated to bottom 6 bits
  .1%LIT@SLIT, %JN  \ {jn(U1, SLIT) get full instr
  .2%JMPL@h1, \ made into SLIT instr and stored.

$STORE_PRIV
@_FP :answer #30$L0 #12$L0 %ADD %RET \ { -> 0x42}
$answer #42 $tAssertEq
$STORE_PUB   $assertNoWs

$pub@_FP :h2 #2$L0 #0$L0 $bump .2%SRBE %RET \ {U2} store on heap (BE)
$pub@_FP :h4 #4$L0 #0$L0 $bump .4%SRBE %RET \ {U4} store on heap (BE)

$pub@_FP :L1   \ {U1} compile a 1 byte literal (unchecked)
  .1%LIT @SZ1@LIT^JN$h1 .2%XSL@h1, \ {U1} compile .1%LIT instr
  .2%JMPL@h1, \ {} then compile the U1

$pub@_FP :L2   \ {U1} compile a 2 byte literal (unchecked)
  .1%LIT @SZ2@LIT^JN$h1 .2%XSL@h1, \ {U2} compile .2%LIT instr
  .2%JMPL@h2, \ {} then compile the U2

$STORE_PRIV
@_FP :answer2 #42$L1 #4242$L2 %RET
$answer2  #4242 $tAssertEq  #42 $tAssertEq

$STORE_PUB   $assertNoWs
@_FP@TY_FN_INLINE^JN :dnodeLast #3$h1 @DV_comp_last$L0 %DV@DV_comp$h1 %RET
@_FP@TY_FN_INLINE^JN :scan      #3$h1 @DV_comp_scan$L0 %DV@DV_comp$h1 %RET

@_FP :dictRef   $scan @DV_comp_dGet$L0 %DV@DV_comp$h1 %RET \ {&root -> &Node}
@_FP :dictAdd   $scan @DV_comp_dAdd$L0 %DV@DV_comp$h1 %RET \ {m v -> &Node}

$STORE_PRIV       $NEW_BLOCK_PRIV
@_FP@TY_FN_INLINE^JN :d_mGet   #2$h1 .2%FTO@DN_m$h1 %RET \ {&DNode -> m}
@_FP@TY_FN_INLINE^JN :d_mSet   #2$h1 .2%SRO@DN_m$h1 %RET \ {m &DNode}
@_FP@TY_FN_INLINE^JN :d_vGet   #2$h1 .R%FTO@DN_v $h1 %RET \ {&DNode -> v}
@_FP@TY_FN_INLINE^JN :d_vSet   #2$h1 .R%SRO@DN_v $h1 %RET \ {v &DNode}

@_FP :tDictRef #0$L0 .2%XSL@dictRef,  %RET \ get dict ref for testing
@_FP :assertDictV \ {<token> v} assert the value of token
  #0$L0 .2%XSL@dictRef, $d_vGet .2%JMPL@tAssertEq,
@_FP :assertDictM \ {<token> m} assert the meta of token
  #0$L0 .2%XSL@dictRef, $d_mGet .2%JMPL@tAssertEq,

#42 #0 $dictAdd answerV   ^DRP    #42 $assertDictV answerV

$STORE_PUB   $assertNoWs
@_FP@TY_FN_INLINE^JN :catch     #2$h1 %DV@DV_catch$h1 %RET
@_FP@TY_FN_INLINE^JN :retIfNot  #1$h1           %RETZ %RET
@_FP@TY_FN_INLINE^JN :reteq     #2$h1     %NEQ  %RETZ %RET
@_FP@TY_FN_INLINE^JN :retIf     #2$h1     %NOT  %RETZ %RET
@_FP@TY_FN_INLINE^JN :retLt     #2$h1     %GE_U %RETZ %RET
@_FP@TY_FN_INLINE^JN :retGe     #2$h1     %LT_U %RETZ %RET
@_FP@TY_FN_INLINE^JN :gt_u      #2$h1     %SWP %LT_U %RET
@_FP@TY_FN_INLINE^JN :le_u      #2$h1     %SWP %GE_U %RET

\ * [2] Spor syntax
\ These are core spor syntax helpers, many of which will also be used to
\ build equivalent fngi syntax.
\
\   fn FN <name>              : declare a function
\   fn PRE                    : make function "pre" (run after next token)
\   fn INLINE                 : make function "pre" (run after next token)
\   fn SYN                    : make function "syn" (syntax, always now)
\   fn NOW                    : require function to be "now" (use $)
\   fn LARGE                  : make function large (has locals)
\
\ There are also many helpers, like
\   fn isFnPre, isFnNormal

$STORE_PRIV

@_FP :_j2 .2%XSL@h1, .2%JMPL@h2, \ {ref instr} compile a context switch

@TY_FN :_xsl \ $_xsl <token> : compile unchecked xsl
  #0$L0 .2%XSL@dictRef, $d_vGet    .1%LIT@XSL2,   .2%JMPL@_j2,

@TY_FN :_jmp \ $_jmp <token> : compile unchecked jmpl
  #0$L0 .2%XSL@dictRef, $d_vGet    .1%LIT@JMPL2,  .2%JMPL@_j2,

@_FP@TY_FN_NOW^JN :c1 \ {instr} compile "compile instr" into function
  $_xsl L1
  @h1$L2 @XSL2$L2 $_jmp _j2

@TY_FN :callAnswer  $_xsl answer %RET     $callAnswer #42 $tAssertEq
@TY_FN :jmpAnswer   $_jmp answer          $jmpAnswer  #42 $tAssertEq

\ These take {&DNode} and return or assert information about it
$STORE_PUB      $assertNoWs
@_FP :isTyLocal   $d_mGet  @C_LOCAL$L2 %MSK  %RET
@_FP :isTyConst   $d_mGet  @META_TY_MASK$L1 %MSK  @TY_CONST$L1 %EQ %RET
@_FP :isTyFn      $d_mGet  @META_TY_MASK$L1 %MSK  @TY_FN$L1  %EQ %RET
@_FP :isFnLarge   $d_mGet  @TY_FN_LARGE$L1 %MSK %RET
@_FP :assertFn    $_xsl isTyFn @E_cNotFn$L2 $_jmp assert
@_FP :assertFnSmall %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX$L2  $_jmp assertNot
@_FP :assertFnLarge %DUP $_xsl assertFn
  $_xsl isFnLarge  @E_cIsX$L2  $_jmp assert

@_FP :toMod #FFFF$L2 #10$L0 %SHL %MSK %RET

@_FP :isSameMod \ {ref ref} -> {sameMod}
  $_xsl toMod  %SWP  $_xsl toMod  %EQ %RET
@_FP :curMod   $dnodeLast  $d_vGet  $_jmp toMod    \ [] -> [mod]
@_FP :isCurMod $_xsl toMod  $_xsl curMod %EQ %RET  \ [ref] -> [isCurMod]
@_FP :assertCurMod  $_xsl isCurMod  @E_cMod$L2  $_jmp assert

      #42   $isCurMod $tAssert
#1234_5678  $isCurMod $tAssertNot

@_FP :_jSetup \ [&DNode] -> [ref]: checked jmp setup
  %DUP $_xsl assertFnSmall  $d_vGet %DUP $_jmp assertCurMod \ {ref}

\ TODO: rename d_mJn and swap args
@_FP :keyJnMeta \ {&DNode m -> &DNode} : apply meta to key
  %OVR $d_mGet %JN \ {&DNode mNew}
  %SWP $d_mSet %RET

$pub @_FP@TY_FN_INLINE^JN :heap #3$h1 @DV_comp_heap$L0 %DV@DV_comp$h1 %RET
$pub @_FP :notNow  @E_cNoNow$L2  $_jmp assertNot  \ {now}
@TY_FN :_implFnTy \ {meta asNow}
  %SWP $_xsl notNow \ {meta}
  .2%FTGL@G_metaNext$h2  %JN  .2%SRGL@G_metaNext$h2  %RET

$pub @TY_FN@TY_FN_SYN^JN :syn    @TY_FN_SYN   $L1 $_jmp _implFnTy
$pub @TY_FN@TY_FN_SYN^JN :now    @TY_FN_NOW   $L1 $_jmp _implFnTy
     @TY_FN@TY_FN_SYN^JN :pre    @TY_FN_PRE   $L1 $_jmp _implFnTy
$pub @TY_FN@TY_FN_SYN^JN :inline @TY_FN_INLINE$L1 $_jmp _implFnTy
     @TY_FN@TY_FN_SYN^JN :large  @TY_FN_LARGE $L1 $_jmp _implFnTy

@TY_FN :clearLocals
  %GR@G_bbaLocal$h2 $BBA_drop   #0$L0 .R%SRGL@G_dictLocal$h2 \ drop everything
  %GR@G_bbaLocal$h2 $BBA_newBlock    #0$L0 .1%SRGL@G_localOffset$h2 %RET

\ example: $syn $large $FN <token>: declare a function with attributes.
@TY_FN@TY_FN_SYN^JN :FN   $_xsl notNow \ {}
  #0$L0 @TY_FN$L1 .2%FTGL@G_metaNext$h2 %JN $_xsl dictAdd \ new dict with metaNext
  %DUP .R%SRGL@G_curFn$h2 \ update currently compiling fn
  $heap %SWP $d_vSet \ dnodeLast.v = heap
  #0$L0 .2%SRGL@G_metaNext$h2 \ clear metaNext
  $_xsl clearLocals $_jmp assertNoWs
$assertNoWs

\ These tell something about a &DNode. Type: [&DNode -> bool]
$pre $FN isFnPre     $d_mGet  @TY_FN_PRE$L1     %MSK %RET
$pre $FN isVarInput  $d_mGet  @TY_VAR_INPUT$L1  %MSK %RET
$pre $FN isFnNormal  $d_mGet  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NORMAL$L1 %EQ %RET
$pre $FN isFnNow     $d_mGet  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_NOW$L1    %EQ %RET
$pre $FN isFnSyn     $d_mGet  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_SYN$L1    %EQ %RET
$pre $FN isFnInline  $d_mGet  @TY_FN_TY_MASK$L1 %MSK  @TY_FN_INLINE$L1 %EQ %RET
$pre $FN isTyVar     $d_mGet  @META_TY_MASK$L1  %MSK  @TY_VAR$L1       %EQ %RET
$pre $FN assertTyVar $_xsl isTyVar  @E_cNotVar$L2 $_jmp assert

$tDictRef assert    ^DUP $isFnPre $tAssert $isFnNormal $tAssert
$tDictRef assertEq  $isFnInline $tAssert

$pub $pre $FN panic   #0$L0 $_jmp assert \ {err} panic immediately
$pub      $FN unreach @E_unreach$L2 $_jmp panic \ unreachable code
$pub $pre $FN unimplIfTrue @E_unimpl$L2 $_jmp assert \ {chk}: if true raise unimpl
$pre $FN assertSzI \ {szI}
  %DUP #CF$L1 %MSK @E_sz$L2 $_xsl assertNot \ non-sz bits empty
  #4$L0 %SHR #3$L1 %LT_U @E_sz$L2 $_jmp assert \ sz bits < 3

$now $pre $FN ftoN \ {offset szI} compile FTO szI w/offset
  %DUP $_xsl assertSzI  @FTO$L1 %ADD  $_xsl h1 $_jmp h1

\ **********
\ * [7] ASM (initial) Flow Control
\ Flow control either pushes the current heap on the WS or uses a local
\ constant. END/AGAIN uses this heap-val/local to do the right thing.
\
\   if/else: $IF ... $ELSE ... $END
\   loop:    $LOOP <l0> ... $BREAK0 <b0> ... $AGAIN <l0> $BREAK_END <b0>

\ @_FP :assertJmpL1  #80$L1 %LT_U  @E_cJmpL1$L2 $_jmp assert
$FN assertJmpL1 #80$L1 %LT_U @E_cJmpL1$L2 $_jmp assert \ {&jmpTo} assert valid

\ Switch to non/local storage
$FN storeNonLocal @C_LOCAL^INV$L2 .2%FTGL@G_cstate$h2 %MSK .2%SRGL@G_cstate$h2 %RET
$FN storeLocal    @C_LOCAL$L2     .2%FTGL@G_cstate$h2 %JN  .2%SRGL@G_cstate$h2 %RET
$FN ldictRef $_xsl storeLocal #0$L0 $_xsl dictRef $_jmp storeNonLocal \ {->&Node}
$FN ldictAdd $_xsl storeLocal       $_xsl dictAdd $_jmp storeNonLocal \ {m v->&Node}

$pub $pre $syn $FN IF $_xsl notNow \ {} -> {&jmpTo} : start an if block
  @SZ1@JZL^JN $c1 \ compile .1%JZL instr
  $heap #0$L0 $_jmp h1  \ compile 0 (jump pad)

$FN _END \ {&jmpTo heapSub} jmpTo is where to store (heap-heapSub)
  $heap \ {&jmpTo heapSub heap}
  %SWP %SUB   %DUP $_xsl assertJmpL1 \ {heapSub (heap-heapSub)}
  %SWP .1%SR %RET \ store at &jmpTo (1 byte literal)

$syn $FN END $_xsl notNow %DUP $_jmp _END

$syn $FN ELSE $_xsl notNow \ {&ifNotJmpTo} -> {&elseBlockJmpTo}
  @JMPL $c1       \ (end IF) compile unconditional jmp to end of ELSE
  $heap %SWP      \ {&elseBlockJmpTo &ifNotJmpTo}
  #0$L0 $_xsl h1  \ compile jmp lit for &elseBlockJmpTo
  %DUP $_jmp _END \ end of IF block (beginning of ELSE)

\ $LOOP l0 ... $BREAK0 b0 ... $AGAIN l0  $BREAK_END b0
     $syn $FN LOOP   $_xsl notNow  $heap  #0$L0 $_xsl ldictAdd %DRP %RET
$pre $syn $FN BREAK0                 $_xsl IF  #0$L0 $_xsl ldictAdd %DRP %RET
$syn $FN ENDV_BREAK  $_xsl notNow $_xsl ldictRef $d_vGet %DUP $_jmp _END
$pre $syn $FN BREAK_IF  @NOT$c1  $_jmp BREAK0 \ break if true
$pre $syn $FN BREAK_EQ  @NEQ$c1  $_jmp BREAK0 \ break if equal
$pre $syn $FN BREAK_NEQ @EQ$c1   $_jmp BREAK0 \ break if not equal

$syn $FN AGAIN $_xsl notNow
  @JMPL$c1  \ {} compile jmp
  $heap $_xsl ldictRef $d_vGet  %SUB %DUP  $_xsl assertJmpL1 \ {heap-&loopTo}
  %NEG $_jmp h1  \ compile negative backwards jump to jmp offset

$STORE_PRIV
$FN testIf \ converts 1->4 else: 13
  #1$L0 %EQ  $IF   #4$L0  %RET
             $END  #13$L0 %RET
#1 $testIf      #4  $tAssertEq
#2 $testIf      #13 $tAssertEq

$FN testIfElse
  #1$L0 %EQ $IF #4$L0 $ELSE #13$L0 $END
  #2$L0 %ADD %RET
#1 $testIfElse  #6 $tAssertEq
#2 $testIfElse  #15 $tAssertEq

$STORE_PUB $assertNoWs

\ **********
\ * [8] xx, jmpl, Scanning and Alignment Utilities
\ Reading, peeking and szI alignment
\
\ fn xx:<token> [...]             : compile an execute to a token
\ fn jmp:<token> [...]            : compile an jmp to a token
\ fn align [aptr sz -> aptr]      : align aptr with sz bytes
\ fn alignSzI [aptr szI -> aptr]  : align aptr with szI bytes
\ fn heapSzI [U4 szI]             : write a value of szI to heap (no align)
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

$pre $FN L4   @SZ4@LIT^JN $c1  $_jmp h4
$pre $FN L \ {v} compile literal of appropriate size
  %DUP #40$L1        %LT_U $IF  $_jmp L0  $END
  %DUP #100$L2       %LT_U $IF  $_jmp L1  $END
  %DUP #FFFF$L2 %INC %LT_U $IF  $_jmp L2  $END   $_jmp L4

$pre $FN keySzI $d_mGet @SZ_MASK$L %MSK %RET \ {&dnode -> szI}

$pub $pre $FN szIToSz \ {szI} -> {sz}
  %DUP @SZ1$L %EQ $IF  %DRP #1$L %RET  $END
  %DUP @SZ2$L %EQ $IF  %DRP #2$L %RET  $END
       @SZ4$L %EQ $IF       #4$L %RET  $END
  @E_sz$L $_jmp panic

$pub $pre $FN heapSzI \ {value szI} write a value of szI to heap
  %DUP @SZ1$L %EQ $IF  %DRP $_jmp h1  $END
  %DUP @SZ2$L %EQ $IF  %DRP $_jmp h2  $END
  %DUP @SZ4$L %EQ $IF  %DRP $_jmp h4  $END
  @E_sz$L $_jmp panic

$pub $pre $FN szToSzI \ [sz] -> [SzI] convert sz to szI (instr)
  %DUP #1$L %EQ $IF  %DRP @SZ1 $L %RET  $END
  %DUP #2$L %EQ $IF  %DRP @SZ2 $L %RET  $END
       #4$L %EQ $IF  %DRP @SZ4 $L %RET  $END
  @E_sz$L $_jmp panic

$pub $pre $FN reqAlign \ {sz -> sz}: get required alignment
  %DUP @RSIZE^DEC$L %LT_U $retIf  %DRP @RSIZE$L %RET

$pub $large $pre $FN align \ {aptr sz -> aptr}: align aptr with sz bytes
  @RSIZE$h1 \ locals [sz:U1]
  .1%SRLL#0$h1 \ cache sz
  %DUP \ {aptr aptr}
  .1%FTLL#0$h1 %MOD \ {aptr aptr%sz}
  %DUP $IF
    .1%FTLL#0$h1 %SWP %SUB \ {aptr (sz - aptr%sz)}
    %ADD %RET \ aptr + (sz - aptr%sz)
  $END
  %DRP %RET


$pub$pre $FN alignR   #4$L .2%XLL@align$h2 %RET           \ {ref -> aptr}
$pub$pre $FN alignReq $_xsl reqAlign .2%XLL @align$h2 %RET \ {ref sz -> ref}
$pub     $FN alignSzI $_xsl szIToSz  .2%XLL @align$h2 %RET \ {ref szI -> ref}

$pre $FN ftSzI \ {&addr szI}
  %DUP @SZ1$L %EQ $IF %DRP .1%FT %RET $END
  %DUP @SZ2$L %EQ $IF %DRP .2%FT %RET $END
       @SZ4$L %EQ $IF      .4%FT %RET $END
  @E_sz$L $_xsl panic

$pre $FN srSzI \ {value &addr szI}
  %DUP @SZ1$L %EQ $IF %DRP .1%SR %RET $END
  %DUP @SZ2$L %EQ $IF %DRP .2%SR %RET $END
       @SZ4$L %EQ $IF      .4%SR %RET $END
  @E_sz$L $_xsl panic

$pre $FN xSzI \ {&DNode -> szI}: size requirement of calling DNode
  $d_vGet $_xsl isCurMod $IF  @SZ2$L %RET  $END  @SZR$L %RET

$STORE_PRIV
\ Test XSW, JMPW and XLW
$FN testXsw  @answer$L %XSW %RET      $testXsw   #42 $tAssertEq
$FN testJmpw @answer$L %JMPW          $testJmpw  #42 $tAssertEq
$large $FN answerL #0$h1  #42$L %RET  $answerL   #42 $tAssertEq
$FN testXlw  @answerL$L %XLW %RET     $testXlw   #42 $tAssertEq

\ Test alignment functions
#0 $reqAlign  #0 $tAssertEq     #1 $reqAlign  #1 $tAssertEq
#2 $reqAlign  #2 $tAssertEq     #3 $reqAlign  #4 $tAssertEq
#4 $reqAlign  #4 $tAssertEq     #5 $reqAlign  #4 $tAssertEq
#20 #1 $alignReq #20 $tAssertEq   #21 #1  $alignReq #21 $tAssertEq
#20 #2 $alignReq #20 $tAssertEq   #21 #2  $alignReq #22 $tAssertEq
#21 #4 $alignReq #24 $tAssertEq   #21 #11 $alignReq #24 $tAssertEq

$STORE_PRIV
\ Inline helper functions for src and token
$inline $FN read #3$h1 @DV_comp_read1$L %DV@DV_comp$h1 %RET \ { -> numRead}
$inline $FN tokenPlc    #3$h1 .2%FTGL@G_src@Fs_plc^ADD$h2 %RET
$inline $FN tokenPlcSet #3$h1 .2%SRGL@G_src@Fs_plc^ADD$h2 %RET
$inline $FN tokenLen    #3$h1 .2%FTGL@G_src@Fs_buf^ADD@Buf_len^ADD$h2 %RET
$inline $FN tokenLenSet #3$h1 .2%SRGL@G_src@Fs_buf^ADD@Buf_len^ADD$h2 %RET
$inline $FN tokenDat #3$h1 .2%FTGL@G_src@Fs_buf^ADD@Buf_dat^ADD$h2 %RET

$STORE_PUB
     $FN isEof       $tokenPlc %NOT %RET
     $FN assertToken $tokenPlc @E_cNeedToken$L $_jmp assert
$pre $FN assertNoEof @E_eof$L $_jmp assert \ {numRead}

$FN scanNoEof  $scan  $_xsl isEof  @E_eof$L $_jmp assertNot

$FN peekChr \ {} -> {c} peek at a character
  $scan   $_xsl isEof $IF  #0$L %RET  $END
  $tokenDat .1%FT \ {c}
  #0$L $tokenPlcSet %RET \ reset scanner for next scan

$FN readNew \ { -> numRead} clear token buf and read bytes
  #0$L $tokenPlcSet  #0$L $tokenLenSet  $read  %RET

\ {&key instrSzI litSzI instr} compile a literal memory instr.
\   szLit the size of the literal to compile for the instr.
$pre $large $FN instrLitImpl
  @RSIZE$h1 \ 1 slot [szLit:U1 instr:U1]
  .1%SRLL #1$h1 \ var instr          {&key szInstr szLit}
  .1%SRLL #0$h1 \ var szLit          {&key szInstr}
  .1%FTLL #1$h1 %JN  $_xsl h1 \ compile jn(szInstr, instr) {&key}
  $d_vGet  .1%FTLL#0$h1  $_jmp heapSzI \ compile literal of proper instrSz

$pre $FN c_fn \ {&key}: compile a function of any type
  %DUP $_xsl assertFn \ {&key}
  %DUP $_xsl isFnInline $IF \ Inline compilation {&key}
    %DUP #F0$L  #2$L #10$L $dv_log
    %DUP $_xsl isFnLarge @E_cInlineLarge$L $_xsl assertNot
    $d_vGet  $heap %SWP      \ {&heap &inlineFn}
    %DUP %INC %SWP .1%FT     \ {&heap &inlineFn+1 inlineLen}
    %DUP @FALSE$L $bump %DRP \ {&heap &inlineFn+1 inlineLen} update heap
    %DV@DV_memMove$h1 %RET   \ {&inlineFn}
  $END
  %DUP $_xsl xSzI     \ {&key szLit}
  %OVR $_xsl isFnLarge  $IF @XLL$L $ELSE @XSL$L $END \ {&key instrSzI instr}
  %OVR %SWP \ {&key instrSzI litSzI instr} instr & lit are same sz
  .2%XLL @instrLitImpl$h2 %RET

$FN colon \ consume a colon token as syntactic surgar, i.e. xx:foo
  $scan $tokenPlc #1$L  %EQ @E_cColon$L $_xsl assert \ assert len=1
  $tokenDat .1%FT #3A$L %EQ @E_cColon$L $_jmp assert \ assert ":"

$FN colonRef $_xsl colon  #0$L $_jmp dictRef

$large $FN _xxPre \ { -> &node} prepare for execute or jmp
  @RSIZE$h1 $_xsl notNow \ locals 0=&key
  $_xsl colonRef  %DUP .R%SRLL#0$h1 \ {&key}
  %DUP @E_cNoKey$L $_xsl assert
  \ if fn is (PRE or SYN) and compFn exists, compile next token.
  %DUP $_xsl isFnPre %SWP $_xsl isFnSyn %OR .R%FTGL@G_compFn$h2 %AND $IF
    .R%FTGL@G_compFn$h2 %XLW
  $END
  .R%FTLL #0$h1 %RET

$syn $FN xx .2%XLL@_xxPre$h2 $_jmp c_fn
$syn $FN jmp
  $xx:_xxPre %DUP $xx:isFnLarge @E_cXHasL$L $xx:assertNot
  $d_vGet @JMPL2$L $_jmp _j2

\ **********
\ * [9] Globals and Locals
\
\ fn GET <token>            SYN : compile a FT of token (local or global)
\ fn SET <token>            SYN : compile a SR of token (local or global)
\ fn REF <token>            SYN : compile a "get ref" of token
\ fn declG | declL  <token>     : begins a global or local declaration
\ fn declVar                    : declare a local/gloabl variable
\ fn declEnd                    : end locals declaration


\ **********
\   * [9.a] Define locals
\ This allows us to declar globals and locals. declEnd (handling actually writing local data)
\ will not happen until later.
$STORE_PUB
$FN declG #0$L %DUP $xx:dictAdd  #0$L %RET  \ [<token> -> &key isLocal=false]
$FN declL #0$L %DUP $xx:ldictAdd #1$L %RET  \ [<token> -> &key isLocal=true]

$large $pre $FN declVar \ {&Node isLocal meta szBytes} declare a variable
  \ Locals          0=szBytes     1=meta         2=isLocal
  @RSIZE^DUP^ADD$h1 .1%SRLL#0$h1  .1%SRLL#1$h1   .1%SRLL#2$h1 \ {&Node}
  %DUP @TY_VAR$L  $xx:keyJnMeta \ {&Node} update meta to TY_VAR

  %DUP .1%FTLL#0$h1 @RSIZE$L %GE_U \ {&Node &Node (szBytes > RSIZE)}
  $IF   @SZR$L
  $ELSE .1%FTLL#0$h1 $xx:szToSzI $END $xx:keyJnMeta \ {&Node}

  .1%FTLL#2$h1 $IF \ if(isLocal)
    %DUP @C_LOCAL$L $xx:keyJnMeta \ {&Node} add C_LOCAL to meta
    .2%FTGL@G_localOffset$h2  .1%FTLL#0$h1  $xx:alignReq \ {&Node offsetAligned}
    %DUP .1%FTLL#0$h1 %ADD .2%SRGL@G_localOffset$h2    \ {..} update localOffset

  $ELSE
    .R%FTGL@G_glen$h2       .1%FTLL#0$h1  $xx:alignReq \ {&Node gheapAligned}
    %DUP .1%FTLL#0$h1 %ADD  .R%SRGL @G_glen$h2       \ {..} update gheap

  $END
  %OVR .1%FTLL#1$h1 $xx:keyJnMeta \ update Node meta {&Node}

  %SWP $d_vSet %RET \ update Node's value

\ Test that locals are declared with proper alignment
$clearLocals
$declL a  #0 #2 $declVar         @a #0 $tAssertEq
$declL b  #0 #1 $declVar         @b #2 $tAssertEq
$declL c  #0 #4 $declVar         @c #4 $tAssertEq

\ **********
\   * [9.b] work with variables
\ These compile GET, SET and REF to work with globals and locals.

\ Compile a get or set instruction.
\ Args:
\   &key: key to compile.
\   localInstrSz  localInstr:  if isTyLocal: use these as the literal sz and instr.
\   globalInstrSz globalInstr: if not isTyLocal: use these as the literal sz and instr.
$pre $large $FN _getSetImpl
  @RSIZE^DUP^ADD$h1 \ locals (see below)
  .1%SRLL#3$h1   .1%SRLL#2$h1 \ 2=globalInstrSz 3=globalInstr
  .1%SRLL#1$h1   .1%SRLL#0$h1 \ 0=localInstrSz 1=localInstr   {&key}
  %DUP @E_cNoKey$L $xx:assert
  %DUP $xx:assertTyVar %DUP $xx:isTyLocal $IF \ {&key}
        %DUP $xx:keySzI .1%FTLL#0$h1 .1%FTLL#1$h1      \ local
  $ELSE %DUP $xx:keySzI .1%FTLL#2$h1 .1%FTLL#3$h1 $END \ global
  $xx:instrLitImpl %RET

\ (create _xxxImpl for fngi to use)
$pre $FN _getImpl \ {&key}
  @SZ1$L  @FTLL$L  \ local  {sz, instr}
  @SZ2$L  @FTGL$L  \ global {sz, instr}
  $xx:_getSetImpl %RET

$pre $FN _setImpl \ {&key}
  @SZ1$L  @SRLL$L  \ local  {sz, instr}
  @SZ2$L  @SRGL$L  \ global {sz, instr}
  $xx:_getSetImpl %RET

$pre $FN _getSetNow \ {&key -> ref szI}
  %DUP $xx:keySzI %SWP   \ {szI &key}
  $d_vGet %GR#0$h1 %ADD \ {szI ref}
  %SWP %RET

$STORE_PRIV
$syn $FN GET
  #0$L $xx:dictRef %SWP $IF $xx:_getSetNow $jmp:ftSzI $END
  $jmp:_getImpl

$syn $FN SET
  #0$L $xx:dictRef %SWP $IF $xx:_getSetNow $jmp:srSzI $END
  $jmp:_setImpl

$syn $FN REF
  #0$L $xx:dictRef %SWP $IF $d_vGet %GR#0$h2 %ADD %RET $END
  %DUP $xx:assertTyVar %DUP $xx:isTyLocal $IF \ {&key}
          #0$L @SZ1$L @LR$L
  $ELSE   #0$L @SZ2$L @GR$L $END
  $xx:instrLitImpl %RET

$large $FN testSetRefGet \ [-> 0x42 0x42]
  @RSIZE$h1 $declL a  #0  @RSIZE $declVar
  #42$L  $SET a   $REF a .R%FT   $GET a  %RET
$testSetRefGet  #42 $tAssertEq  #42 $tAssertEq

$STORE_PUB $assertNoWs

\ **********
\   * [9.c] Compiling Locals
\ Compile (i.e. write to local stack) inputs in the reverse order they were
\ declared. Since each input increments the local offset, this means we need to
\ compile the largest offset first, then largest less than that, etc. The last
\ offset will always be 0.
\
\ We walk the entire locals BST for every input. We are really not worried
\ about the fact that this has bad time complexity O(L * I), since there can
\ only be ~8 inputs and 8 more locals -- meaning this is bounded to be
\ guaranateed fast.
\
\ struct Context [   \ This is the struct shape for &Context
\   node: &Node,  \ The current node with value < maxAllowed
\   max: U1,      \ the maximum value allowed
\ ]

$STORE_PRIV
$inline $FN Ctx_node    #2$h1 .R%FTO#0$h1     \ [&Context -> &Node]
$inline $FN Ctx_nodeSet #2$h1 .R%SRO#0$h1     \ [&Node &Context]
$inline $FN Ctx_max     #2$h1 .R%FTO@RSIZE$h1 \ [&Context -> maxAllowed]
$inline $FN Ctx_maxSet  #2$h1 .R%SRO@RSIZE$h1 \ [maxAllowed &Context]
$STORE_PUB $assertNoWs

$large $FN _walker \ [&context &Node]
  @RSIZE@RSIZE^ADD$h1
  $declL ctx  #0 @RSIZE $declVar
  $declL node #0 @RSIZE $declVar   $SET node $SET ctx
  $GET node$xx:isTyVar $retIfNot   $GET node$xx:isVarInput $retIfNot \ type check
  $GET node$d_vGet $GET ctx$Ctx_max %LT_U $retIfNot\(offset < max)
  $GET ctx$Ctx_node %NOT  $IF \ null case
    $GET node  $GET ctx$Ctx_nodeSet %RET
  $END
  $GET ctx$Ctx_node$d_vGet  $GET node$d_vGet $retGe \ retGe(cur,offset)
  $GET node $GET ctx$Ctx_nodeSet %RET \ .ctx.node = node

\ {&context &Node]]
\ Walk the BST, calling fn on each node
$large $FN bstWalk
  @RSIZE@RSIZE^ADD$h1
  $declL ctx  #0 @RSIZE $declVar
  $declL node #0 @RSIZE $declVar
  %SWP $SET ctx  %DUP  $SET node $retIfNot
  \\ perform DFS preorder
  $GET ctx $GET node  $xx:_walker
  $GET ctx $GET node  .R%FTO@DN_l$h1  $xx:bstWalk
  $GET ctx $GET node  .R%FTO@DN_r$h1  $xx:bstWalk  %RET
  %RET

$large $FN compileInputs
  @RSIZE@RSIZE^ADD$h1 \ local: Context struct
  $declL ctx_node #0 @RSIZE $declVar
  $declL ctx_max  #0 @RSIZE $declVar  #FF$L $SET ctx_max
  $LOOP l0
    $GET ctx_max %RETZ   #0$L $SET ctx_node
    $REF ctx_node $GET G_dictLocal $xx:bstWalk
    $GET ctx_node $retIfNot   $GET ctx_node$d_vGet  $SET ctx_max
    $GET ctx_node $xx:_setImpl
  $AGAIN l0
  %RET

$STORE_PRIV
$FN declEnd \ Compile local inputs update locals space for large fn.
  $GET G_localOffset $retIfNot               \ noop if no locals
  $GET G_localOffset $xx:alignR $xx:h1       \ store locals space
  $xx:compileInputs
  $GET G_curFn @TY_FN_LARGE$L $jmp:keyJnMeta \ update curFn to large

$STORE_PRIV
$pre $FN setNow #1$L $jmp:SET \ TODO: test this and get it to work
$pre $FN setLogLvlSys .2%SRGL@G_logLvlSys$h2 %RET
$FN getDictLocal $GET G_dictLocal %RET \ TODO: use getNow

$pre $FN testDeclEnd \ { a b c -> a - c + b }
  $declL b  @TY_VAR_INPUT  @RSIZE $declVar
  $declL c  @TY_VAR_INPUT  @RSIZE $declVar $declEnd
  $GET c %SUB $GET b %ADD %RET

@_FP@TY_FN_LARGE^JN $assertDictM testDeclEnd

#10 #4 #8 $testDeclEnd   #C $tAssertEq


\ **********
\ * [11] Fngi compile loop
\ The fngi compile loop is implemented in spore. In addition to the compile loop itself,
\ this section implements several essential parsing functions that can be used
\ in other areas of fngi.
\
\ Globals:
\   global compFn: a function reference used to compile individual tokens.
\
\ fn ( <...> )                    : ( compiles tokens until )
\ fn $ <token>                    : execute token asNow
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
$STORE_PUB $assertNoWs

\ **********
\   * [11.a] Parse Numbers and String Characters

$pre $FN betweenIncl \ {value a b} -> a <= value <= b
  $declL b  @TY_VAR_INPUT  @RSIZE  $declVar $declEnd \ {value a}
  %OVR %SWP \ {value value a}
  \ if (value<a) return FALSE;
  %LT_U $IF %DRP @FALSE$L %RET $END
  $GET b %SWP \ {b value}
  %LT_U %NOT %RET \ return not(b<value)
#1 #0 #3 $betweenIncl $tAssert
#0 #0 #3 $betweenIncl $tAssert
#3 #0 #3 $betweenIncl $tAssert

$pre $FN charToInt \ {c} -> {U8}
  \ '0' - '9'
  %DUP #30$L #39$L $xx:betweenIncl $IF #30$L %SUB %RET $END
  \ 'A' - 'Z'
  %DUP #41$L #5A$L $xx:betweenIncl $IF #41$L %SUB #A$L %ADD %RET $END
  \ 'a' - 'z'
  %DUP #61$L #7A$L $xx:betweenIncl $IF #61$L %SUB #A$L %ADD %RET $END
  %DRP #FF$L %RET
#30 $charToInt  #0 $tAssertEq  #39 $charToInt  #9 $tAssertEq  \ '0', '9'
#41 $charToInt  #A $tAssertEq  #46 $charToInt  #F $tAssertEq  \ 'A', 'F'
#61 $charToInt  #A $tAssertEq  #66 $charToInt  #F $tAssertEq  \ 'a', 'f'
#2C $charToInt  #FF $tAssertEq  \ ','

\ {} -> {c}: read next character from AFTER tokenPlc.
\ Increments tokenPlc. This is destructive to token, use with care.
$FN charNext
  $tokenPlc  $tokenLen %GE_U $IF
    $xx:readNew  $xx:assertNoEof
  $END
  $tokenDat $tokenPlc  %ADD .1%FT
  $tokenPlc %INC  $tokenPlcSet %RET
$charNext* #2A $tAssertEq   $charNext  #20 $tAssertEq \ '*', ' '

\ {} -> {char unknownEscape} read a character that can be escaped.
$FN readCharEsc
  $xx:charNext \ {char}
  %DUP #5C$L %NEQ $IF @FALSE$L %RET $END \ if(c != '\\') ret;
  \ c is an escape character: \
  %DRP $xx:charNext
  %DUP #5C$L %EQ $IF             @FALSE$L %RET $END \ \\: escape
  %DUP #74$L %EQ $IF %DRP #09$L @FALSE$L %RET $END \ \t: tab
  %DUP #6E$L %EQ $IF %DRP #0A$L @FALSE$L %RET $END \ \n: newline
  %DUP #20$L %EQ $IF %DRP #20$L @FALSE$L %RET $END \ \ : space
  %DUP #78$L %EQ $IF \ \xHH
    \ charToInt(charNext) << 8 + charToInt(charNext)
    %DRP $xx:charNext  $xx:charToInt #8$L  %SHL
    $xx:charNext       $xx:charToInt %ADD
    \ assertNot(dup < inc(0xFF), E_cStr)
    %DUP #FF$L %INC %LT_U  @E_cStr$L  $xx:assertNot
    @FALSE$L %RET
  $END
  @TRUE$L %RET \ just return the character as-is but unknownEscape=true
$readCharEsc*  $tAssertNot #2A $tAssertEq    \ '*'
$readCharEsc\n $tAssertNot #0A $tAssertEq    \ '\n'
$readCharEsc\W $tAssert    #57 $tAssertEq    \ unknownEscape and 'W'

$pre $FN numBase \ {c -> base} get number base from char
  %DUP #63$L %EQ $IF %DRP #FE$L %RET $END \ c -> character
  %DUP #62$L %EQ $IF %DRP #02$L %RET $END \ b -> binary
       #78$L %EQ $IF      #10$L %RET $END \ x -> hex
  #0$L %RET \ unknown/use default
#63 $numBase #FE $tAssertEq   #62 $numBase #02 $tAssertEq

$FN parseBase \ {i base -> value isNumber} parse from token
  $declL i      @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL base   @SZ1@TY_VAR_INPUT^JN  #1     $declVar
  $declL value  @SZR                  @RSIZE $declVar
  $declEnd
  $GET base #FE$L %EQ $IF \ if special 'character' base
      $GET i $tokenPlcSet \ readCharEsc is off END of tokenPlc
      $xx:readCharEsc  @E_cUnknownEsc$L $xx:assertNot  @TRUE$L %RET
  $END #0$L $SET value
  $LOOP l0
    $GET i  $tokenPlc $BREAK_EQ b0
    $tokenDat $GET i %ADD .1%FT \ {c}
    $xx:charToInt \ {v}
    \ return {0 0} if not integer character
    %DUP $GET base %GE_U $IF  @FALSE$L %RET  $END

    $GET base  $GET value %MUL \ {base * value}
    %ADD $SET value \ value = v + value*10
    $GET i %INC $SET i \ i += 1
  $AGAIN l0  $ENDV_BREAK b0

  $GET i %NOT $IF  #0$L @FALSE$L %RET  $END \ no token
  $GET value @TRUE$L %RET

$FN parseNumber \ {} -> {value isNumber} parse token as number
  #0$L #A$L \ {i=0 base=0xA}. Next: if (token len>=2 and starts with '0')
  $tokenPlc #2$L %GE_U $tokenDat .1%FT #30$L %EQ %AND $IF
    %DRP $tokenDat %INC .1%FT $xx:numBase %DUPN $IF \ {i base}
      %DRP #A$L \ base was = 0, just set back to 10
    $ELSE %SWP %INC2 %SWP $END \ else i+=2
  $END
  $xx:parseBase %RET

$FN number $scan $xx:parseNumber %RET \ {<token> -> value isNumber}

$number 1234     $tAssert    #4D2   $tAssertEq
$number 0x1234   $tAssert    #1234  $tAssertEq
$number 0xF00FA  $tAssert    #F00FA $tAssertEq
$number 0b11100  $tAssert    #1C    $tAssertEq
$number 0b11102  $tAssertNot ^DRP \ not valid binary
$number notNum   $tAssertNot ^DRP \ not a number

\ **********
\   * [11.b] Compile Tokens
$NEW_BLOCK_PRIV

$FN lit \ {asNow value:U4 -> ?nowVal}: compile literal respecting asNow
  %SWP $retIf $jmp:L \ if now leave on stack, else compile

$pre $FN _compConstant \ {asNow} -> {&keyFn[nullable]}
  %DUP #C0$L  #2$L #10$L $dv_log
  $xx:parseNumber \ {asNow value isNumber}
  %DUP #C1$L  #2$L #10$L $dv_log
  $IF  $xx:lit #0$L %RET  $END  %DRP  \ {asNow}
  $xx:isEof $IF  %DRP #0$L %RET  $END \ {asNow}
  #0$L @DV_comp_dGet$L %DV@DV_comp$h1 \ {asNow &node}
  %DUP @E_cNoKey$L $xx:assert
  %DUP $xx:isTyConst  #C3$L  #2$L #10$L $dv_log

  %DUP $xx:isTyConst $IF \ {asNow &node}
    $d_vGet  $xx:lit  #0$L %RET
  $END \ {asNow &node}
  %DUP $xx:isTyLocal  @E_cLocalNotConst$L $xx:assertNot
  %SWP %DRP %DUP $jmp:assertFn \ {-> &key}

$FN execute \ {&key} -> {...}: execute a dictionary key
  %DUP $xx:isFnInline $IF \ if inline, assert not large and jmp to addr+1
    %DUP $xx:isFnLarge @E_cInlineLarge$L $xx:assertNot
    $d_vGet %INC %JMPW
  $END
  %DUP %DUP $d_vGet #E2$L  #3$L #10$L $dv_log
  %DUP $xx:isFnLarge  $IF $d_vGet %XLW %RET $END
  $d_vGet %JMPW

$STORE_PRIV
$FN executeIt #0$L $xx:dictRef $jmp:execute
$executeIt answer  #42 $tAssertEq


$STORE_PUB

\ {asNow} -> {}: compile a single token.
\ This is the primary function that all compilation steps (besides spor
\ compilation) reduce to.
$pre $FN single
  $declL asNow #0 #1     $declVar
  $declL node  #0 @RSIZE $declVar $declEnd
  %DUP #01$L  #2$L #10$L $dv_log
  \ Handle constants, return if it compiled the token.
  %DUP $SET asNow $xx:_compConstant %DUP $SET node %RETZ

  $GET node $xx:isFnPre $IF
    #08$L  #1$L #10$L $dv_log
    $GET G_compFn %XLW
    #09$L  #1$L #10$L $dv_log
  $END \ recurse for PRE
  $GET node $xx:isFnSyn $IF
    #02$L  #1$L #10$L $dv_log
    $GET asNow $GET node $jmp:execute    $END
  $GET node $xx:isFnNow $IF
    #03$L  #1$L #10$L $dv_log
    $GET asNow @E_cReqNow$L $xx:assert $END
  $GET node $GET asNow $IF
    #04$L  #1$L #10$L $dv_log
    $xx:execute
    #05$L  #1$L #10$L $dv_log
    %RET

    $END  $jmp:c_fn

$pre $FN updateCompFn \ {&newCompFn -> &prevCompFn}
  $GET G_compFn %SWP $SET G_compFn %RET

$FN _now \ used in $ to make next token/s run NOW.
  $declL compFn  #0  @RSIZE $declVar $declEnd
  @_now$L $xx:updateCompFn $SET compFn \ update G_compFn and cache
  $xx:scanNoEof
  #70$L #1$L #10$L $dv_log
  @TRUE$L $xx:single  \ compile next token as NOW
  $GET compFn $SET G_compFn %RET

$pub$syn $FN $ $xx:notNow $xx:_now %RET \ make NOW

$pub$syn $FN (  %DRP  \ parens ()
  $LOOP l0
    #100$L #1$L #10$L $dv_log
    $xx:assertToken
    $xx:peekChr #29$L %EQ $IF  $scan %RET  $END \ return if we hit ")"
    $GET G_compFn
    %DUP #101$L #2$L #10$L $dv_log
    %XLW
  $AGAIN l0

\ {-> c} peek at the char after current token.
$FN peekNoScan
  $tokenPlc  $tokenLen %GE_U $IF
    $read $xx:assertNoEof \ ensure a char exists
  $END
  $tokenDat $tokenPlc %ADD .1%FT %RET

$FN _comment \ used in '\' to make next token ignored (i.e. a comment)
  $declL compFn  #0  @RSIZE $declVar $declEnd
  @_comment$L  $xx:updateCompFn $SET compFn
  $xx:scanNoEof \ ignore token, unless it's an open parenthesis
  $tokenDat .1%FT #28$L %EQ $IF
    @TRUE$L $xx:single
  $END
  $GET compFn $SET G_compFn %RET

\ Comment, of which there are three forms.
\    \        : a line comment
\    \foo     : an inline comment, commenting out one token
\    \( ... ) : a block comment
$syn $FN \  %DRP
  \ line comment if '\' is followed by space
  $xx:peekNoScan %DUP #A$L %EQ $IF %DRP %RET $END \ newline
  #20$L %EQ $IF @DV_comp_readEol$L %DV@DV_comp$h1 %RET $END
  $xx:_comment %RET \ else token comment
$\ line comment
$\token_comment
\ $\(block comment)
\ $\()  $\(  )

\ These do nothing and are used for more readable code.
$syn $FN _ %DRP %RET   $syn $FN , %DRP %RET   $syn $FN ; %DRP %RET
$syn $FN -> %DRP %RET


$FN fngi \ fngi compile loop
  $LOOP l0
    $tokenLen #E0$L  #2$L #10$L $dv_log
    $tokenLen %RETZ \ exit on EOF
    $GET G_compFn #E1$L  #2$L #10$L $dv_log
    $GET G_compFn %XLW
    #EF$L  #1$L #10$L $dv_log
  $AGAIN l0

$large $FN fngiSingle \ base compFn for fngi tokens.
  #0$h1 \ not really any locals (but called with XLW)
  $scan $tokenPlc
  %RETZ
  @FALSE$L $xx:single
  %RET


\ **********
\ * [12] Core fngi functions
$NEW_BLOCK_PUB

\ Stack operators. These are /not/ PRE since they directly modify the stack.
$pub      $inline $FN swp   #1$h1 %SWP    %RET
$pub      $inline $FN drp   #1$h1 %DRP    %RET
$pub      $inline $FN ovr   #1$h1 %OVR    %RET
$pub      $inline $FN dup   #1$h1 %DUP    %RET
$pub      $inline $FN dupn  #1$h1 %DUPN   %RET

\ Standard operators that use PRE syntax. Either "a <op> b" or simply "<op> b"
$pub $pre $inline $FN ret   #1$h1 %RET
$pub $pre $inline $FN inc   #1$h1 %INC    %RET
$pub $pre $inline $FN inc2  #1$h1 %INC2   %RET
$pub $pre $inline $FN inc4  #1$h1 %INC4   %RET
$pub $pre $inline $FN dec   #1$h1 %DEC    %RET
$pub $pre $inline $FN inv   #1$h1 %INV    %RET
$pub $pre $inline $FN neg   #1$h1 %NEG    %RET
$pub $pre $inline $FN not   #1$h1 %NOT    %RET
$pub $pre $inline $FN i1to4 #1$h1 %CI1    %RET
$pub $pre $inline $FN i2to4 #1$h1 %CI2    %RET
$pub $pre $inline $FN +     #1$h1 %ADD    %RET
$pub $pre $inline $FN -     #1$h1 %SUB    %RET
$pub $pre $inline $FN %     #1$h1 %MOD    %RET
$pub $pre $inline $FN <<    #1$h1 %SHL    %RET
$pub $pre $inline $FN >>    #1$h1 %SHR    %RET
$pub $pre $inline $FN msk   #1$h1 %MSK    %RET
$pub $pre $inline $FN jn    #1$h1 %JN     %RET
$pub $pre $inline $FN xor   #1$h1 %XOR    %RET
$pub $pre $inline $FN and   #1$h1 %AND    %RET
$pub $pre $inline $FN or    #1$h1 %OR     %RET
$pub $pre $inline $FN ==    #1$h1 %EQ     %RET
$pub $pre $inline $FN !=    #1$h1 %NEQ    %RET
$pub $pre $inline $FN >=    #1$h1 %GE_U   %RET
$pub $pre $inline $FN <     #1$h1 %LT_U   %RET
$pub $pre $inline $FN *     #1$h1 %MUL    %RET
$pub $pre $inline $FN /     #1$h1 %DIV_U  %RET

\ ftN(addr): fetch a value of sz N from address.
$pub $pre $inline $FN ft1   #1$h1 .1%FT   %RET
$pub $pre $inline $FN ft2   #1$h1 .2%FT   %RET
$pub $pre $inline $FN ft4   #1$h1 .4%FT   %RET

\ srN(value, addr): store a value of sz N to address.
$pub $pre $inline $FN sr1   #1$h1 .1%SR   %RET
$pub $pre $inline $FN sr2   #1$h1 .2%SR   %RET
$pub $pre $inline $FN sr4   #1$h1 .4%SR   %RET

\ Switch to fngi syntax and test some basics
$STORE_PRIV $assertNoWs
@fngiSingle $updateCompFn ^DRP $fngi
$tAssert 1;  $tAssert(1);   $tAssertNot(FALSE);
$tAssertEq(0x42, 0x42);
pre FN testFngiSyntax \ [a -> 0x42 + a] just make sure some syntax works
  $declVar(declL a, TY_VAR_INPUT, 2) $declVar(declL out, 0, 2) $declEnd
  GET a + answer -> SET out;   ret(GET out);
$tAssertEq(0x46, testFngiSyntax(4));


$STORE_PUB $assertNoWs
