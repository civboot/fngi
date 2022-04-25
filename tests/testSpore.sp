$assertWsEmpty

\ Test assert
#0              $tAssertNot
#1              $tAssert

\ Test Select
#3 #42 #1 $select  #3  $tAssertEq
#3 #42 #0 $select  #42 $tAssertEq
$assertWsEmpty

\ Test operations
#400 #1 ^ADD    #401 $tAssertEq
#400 #1 ^SUB    #3FF $tAssertEq
#400 #1 ^SHR    #200 $tAssertEq
#400 #1 ^SHL    #800 $tAssertEq

\ Test dictionary
#444 $gdictSet foo
$gdictGet foo   #444 $tAssertEq
#123455 $gdictSet foo2
$gdictGet foo2 #123455 $tAssertEq


\ Test loc
$loc twelve #12 $L0 %RET
$twelve #12 $tAssertEq

\ Test L0-4
$loc lits #5 $L0  #12345 $L1  #12345 $L2   #12345 $L4 %RET
$lits
#12345  $tAssertEq
#2345   $tAssertEq
#45     $tAssertEq
#5     $tAssertEq

\ Test meta
$gdictGetK keyMeta @TY_FN @TY_FN_PRE @TY_FN_SYN ^JN ^JN $tAssertKeyMeta

\ Test xsl
$FN myXs #22 $L0 %RET
$myXs #22 $tAssertEq

$FN callMyXs $xsl myXs %RET
$callMyXs #22 $tAssertEq

$c_updateGkey ^DRP

\ assert @gkey == @(&dict.buf + &dict.heap)
  @c_gkey @REF_MASK ^MSK .4^FT
  @c_gdictRef @REF_MASK ^MSK .4^FT   @c_gdictLen @REF_MASK ^MSK .2^FT   .4^ADD
  $tAssertEq


\ Test functions
$assertWsEmpty

\ Test core macros

$FN one   #1 $L0 %RET
$FN add   %ADD %RET

$FN test_xslJmpl $xsl one   $xsl one   $jmpl add
$test_xslJmpl  #2 $tAssertEq

$gdictGetK FN      $isFnSyn   $tAssert
$gdictGetK IF      $isTyFn    $tAssert
$gdictGetK IF      $isFnSyn   $tAssert
$gdictGetK assert  $isFnNow   $tAssertNot

\ Test control flow
$FN testIf \ converts 1->10 else: 42
  #1 $L0 %EQ  $IF   #4 $L0 %RET
              $END  #13 $L0 %RET
#1 $testIf      #4 $tAssertEq
#2 $testIf      #13 $tAssertEq

$FN testIfElse
  #1$L0 %EQ $IF #4$L0 $ELSE #13$L0 $END
  #2$L0 %ADD %RET
#1 $testIfElse  #6 $tAssertEq
#2 $testIfElse  #15 $tAssertEq


$FN min $LARGE \ [a b] -> [min]
  #1 $h1 \ one local, b
  .4%SRLL #0 $h1
  %DUP \ {a a}
  .4%FTLL #0 $h1 \ {a a b}
  %GE_U %RETZ               \ if(!(a >= b)) ret a
  %DRP .4%FTLL #0 $h1 %RET  \ ret b

#1 #2    $min   #1 $tAssertEq
#42 #333 $min   #42 $tAssertEq

\ Testing Vars
#0 $reqAlign  #0 $tAssertEq   #1 $reqAlign  #1 $tAssertEq
#2 $reqAlign  #2 $tAssertEq   #3 $reqAlign  #4 $tAssertEq
#4 $reqAlign  #4 $tAssertEq   #5 $reqAlign  #4 $tAssertEq

#20 #1 $alignA #20 $tAssertEq   #21 #1  $alignA #21 $tAssertEq
#20 #2 $alignA #20 $tAssertEq   #21 #2  $alignA #22 $tAssertEq
#21 #4 $alignA #24 $tAssertEq   #21 #11 $alignA #24 $tAssertEq

$FN testDecl $LARGE
  $declL a
    ^OVR^OVR $tAssert   \ assert(isLocal)
    ^DUP $isTyped $tAssert
    ^INCA .1^FT @TY_VAR $tAssertEq
    ^OVR^OVR  @SZA@TY_VAR_INPUT^JN  @ASIZE $declVar
    ^DRP  ^INCA .1^FT  @TY_VAR@SZA^JN @TY_VAR_INPUT^JN $tAssertEq

@SZ1 $assertSzI
@SZ2 $assertSzI
@SZ4 $assertSzI
\ #30  $assertSzI \ expect failure.
\ #01  $assertSzI \ expect failure.

$declG myG1  @SZ4 #4 $declVar
  #12345 $gRef myG1   .4^SR

@myG1 #FF_FFFF ^MSK .4^FT  #12345 $tAssertEq

$FN myG1Ref  $REF myG1 %RET
$myG1Ref  @myG1 #FF_FFFF ^MSK  $tAssertEq

$FN myG1Get  $GET myG1 %RET
$myG1Get  #12345 $tAssertEq

$FN myG1Set  $_SET myG1 %RET
#6789F $myG1Set   $myG1Get  #6789F $tAssertEq

\ *****
\ * Testing Locals
\ test ldict
#12 =shadowed
$FN notARealFn %RET \ updates ldict
@shadowed #12 $tAssertEq

#45 $ldictSet shadowed
$ldictGet shadowed #45 $tAssertEq
@shadowed #12 $tAssertEq

$FN fooLocals
  $declL myLocal  @SZ2  #2 $declVar
  $ldictGetK myLocal ^INCA .1^FT
  @TY_VAR @SZ2 ^JN $tAssertEq

\ test R_LP
$FN getLp %RGFT @R_LP$h1  %RET
$getLp ^DUP #FFF0 $tAssertEq  =lsTop

$FN getLpWLocal #1$h1 $LARGE  %RGFT @R_LP$h1  %RET
$getLpWLocal @lsTop #4 ^SUB $tAssertEq

\ test local variables
$FN useLocal
  $declL a  @SZ2  #2 $declVar $declEnd

  #12345$L4 $_SET a
  $GET a
  %RET

$useLocal #2345 $tAssertEq

$FN badMultiply \ {a b -- a*b} uses loop to implement multiply
  $declL a  @SZ2@TY_VAR_INPUT^JN  #2 $declVar
  $declL b  @SZ2@TY_VAR_INPUT^JN  #2 $declVar
  $declEnd

  #0$L0 \ out = 0
  $LOOP l0
    \ if(!b) break
    $GET b $BREAK0 b0
    \ out = out + a
    $GET a  %ADD
    \ b = b - 1
    $GET b  %DEC  $_SET b
  $AGAIN l0  $END_BREAK b0
  %RET

#0 #0 $badMultiply   #0   $tAssertEq
#5 #1 $badMultiply   #5   $tAssertEq
#5 #0 $badMultiply   #0   $tAssertEq
#8 #8 $badMultiply   #40  $tAssertEq
#5 #5 $badMultiply   #19  $tAssertEq

$assertWsEmpty
$c_number 1234     $tAssert   #4D2   $tAssertEq
$c_number 0x1234   $tAssert   #1234  $tAssertEq
$c_number 0xF00FA  $tAssert   #F00FA $tAssertEq
$c_number 0b11100  $tAssert   #1C    $tAssertEq
$c_number 0b11102  $tAssertNot      ^DRP \ not valid binary
$c_number notNum   $tAssertNot      ^DRP \ not a number

\ characters
$c_number 0cA   $tAssert   #41 $tAssertEq
$c_number 0ca   $tAssert   #61 $tAssertEq
$c_number 0cb   $tAssert   #62 $tAssertEq
$c_number 0cc   $tAssert   #63 $tAssertEq
$c_number 0c\t  $tAssert   #09 $tAssertEq
$c_number 0c\n  $tAssert   #0A $tAssertEq
$c_number 0c    $tAssert   #20 $tAssertEq
$c_number 0c\   $tAssert   #20 $tAssertEq
$c_number 0c\s  $tAssert   #20 $tAssertEq

\ Test FTO and SRO
$declG gStruct  @SZ4 #4 $declVar
$declG gStruct1 @SZ1 #1 $declVar
  #1234  $gRef gStruct  .4^SR
  #67    $gRef gStruct1 .1^SR

$FN testFTSROffset
  $declL r  @SZ4  #4 $declVar $declEnd
  $REF gStruct $_SET r
  $GET r  .4%FTO #0$h1    #1234$L2 $xsl tAssertEq
  $GET r  #0 @SZ4 $ftoN   #1234$L2 $xsl tAssertEq
  $GET r  #4 @SZ1 $ftoN   #67$L2 $xsl tAssertEq
  %RET
$assertWsEmpty   $testFTSROffset

\ Test compiler internals

@TRUE #123 $c_lit #123 $tAssertEq  $assertWsEmpty

$FN two $declEnd #2$L0 %RET \ {} -> 1

$FN test_c_fn   $gdictGetK one $c_fn %RET
$test_c_fn #1 $tAssertEq
$assertWsEmpty

$FN test_c_fnTwo   $gdictGetK two $c_fn %RET
$test_c_fnTwo #2 $tAssertEq

$gdictGetK test_c_fnTwo $execute
$gdictGetK test_c_fn    $execute
  ^ADD #3 $tAssertEq

$FN test_c_compFnExists $GET c_compFn %RET
$test_c_compFnExists  @fngiSingle $tAssertEq

\ Test essential functions

$c_peekChr $one  #1 $tAssertEq  #24 $tAssertEq \ 0x24 = '$'

$FN comp1 .4%XW %RET

$FN testFngiSingleNum @fngiSingle $comp1 12 %RET
$testFngiSingleNum #C $tAssertEq

$FN testFngiSingleOne @fngiSingle $comp1 two %RET
$testFngiSingleOne #2 $tAssertEq

$FN withLocals
  $declL s4  @SZ4  #4 $declVar
  $declL s1  @SZ1  #1 $declVar
  $declEnd
  %RET

@withLocals .1^FT  #2 $tAssertEq

#1 #0 #3 $betweenIncl $tAssert
#0 #0 #3 $betweenIncl $tAssert
#3 #0 #3 $betweenIncl $tAssert

\ A bit of fngi syntax
$() \ does nothing
$(
  \inlineComment
  \ line comment
  \( block comment )
)

$( FN hi 32 one spor%RET )
$hi
  #1 $tAssertEq
  #20 $tAssertEq

$FN add1 $PRE %INC %RET \ {a} -> {a+1}
$( FN three ret(add1(2)) )
$three  #3 $tAssertEq

$loc sporMsg  $| Hello world from testSpor.sp!\n|
$assertWsEmpty

@sporMsg $_printz
$assertWsEmpty


$FN failRecursively2 \ {n} -> {n-1}
  %DUP %NOT $IF
    %DRP
    #1$L0 #7$L0  $xsl tAssertEq
    %RET
  $END
  %DEC $xsl failRecursively2

$FN failRecursively \ {n} -> {n-1}
  $declL n  @SZ4@TY_VAR_INPUT^JN  #4 $declVar
  $declEnd
  $GET n %NOT $IF
    #5$L0 $xsl failRecursively2
    %RET
  $END
  $GET n %DEC $xl failRecursively

$assertWsEmpty
$c_dictDump

\ #5 $failRecursively

