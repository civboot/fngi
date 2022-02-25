// Test assert
#0              $tAssertNot
#1              $tAssert

// Test operations
#400 #1 ^ADD    #401 $tAssertEq
#400 #1 ^SUB    #3FF $tAssertEq
#400 #1 ^SHR    #200 $tAssertEq
#400 #1 ^SHL    #800 $tAssertEq

// Test dictionary
#444 $dictSet foo
$dictGet foo   #444 $tAssertEq
#123455 $dictSet foo2
$dictGet foo2 #123455 $tAssertEq

// Test loc
$loc twelve #12 $L0 %RET
$twelve #12 $tAssertEq

// Test L0-4
$loc lits #5 $L0  #12345 $L1  #12345 $L2   #12345 $L4 %RET
$lits
#12345  $tAssertEq
#2345   $tAssertEq
#45     $tAssertEq
#5     $tAssertEq

// Test xsl

$SFN myXs #22 $L0 %RET
$myXs #22 $tAssertEq

$SFN callMyXs $xsl myXs %RET
$callMyXs #22 $tAssertEq

// Test rKeyMeta
#12 #1 $metaSet  #100_0012 $tAssertEq

$c_updateRKey ^DRP

// assert @rKey == @(&dict.buf + &dict.heap)
  @c_rKey @REF_MASK ^BAND .4^FT
  @c_dictBuf @REF_MASK ^BAND .4^FT   @c_dictHeap @REF_MASK ^BAND .2^FT   .4^ADD
  $tAssertEq


// Test functions
$assertWsEmpty

// Test core macros

$SFN one   #1 $L0 %RET
$SFN add   %ADD %RET

$SFN test_xslJmpl $xsl one   $xsl one   $jmpl add
$test_xslJmpl  #2 $tAssertEq

$dictGet SFN     $isFnSmart $tAssert
$dictGet FN      $isFnSmart $tAssert
$dictGet IF      $isTyFn      $tAssert
$dictGet IF      $isFnSmart $tAssert
$dictGet assert  $isFnInstant $tAssertNot

// Test control flow
$SFN testIf // converts 1->10 else: 42
  #1 $L0 %EQ  $IF   #4 $L0 %RET
              $END  #13 $L0 %RET
#1 $testIf      #4 $tAssertEq
#2 $testIf      #13 $tAssertEq

$SFN testIfElse
  #1$L0 %EQ $IF #4$L0 $ELSE #13$L0 $END
  #2$L0 %ADD %RET
#1 $testIfElse  #6 $tAssertEq
#2 $testIfElse  #15 $tAssertEq


$FN min // [a b] -> [min]
  #1 $h1 // one local, b
  .4%SRLL #0 $h1
  %DUP // {a a}
  .4%FTLL #0 $h1 // {a a b}
  %GE_U %RETZ               // if(!(a >= b)) ret a
  %DRP .4%FTLL #0 $h1 %RET  // ret b

#1 #2    $min   #1 $tAssertEq
#42 #333 $min   #42 $tAssertEq

$ha2 #1 $h1 // misalign heap
@SZ4 $halN #1 $h1 #12345 $h4 // use them.

// Testing Gloabls
#12345 @SZ4 $GLOBAL myG1   $assertWsEmpty
@myG1 #FF_FFFF ^BAND .4^FT  #12345 $tAssertEq

$SFN myG1Ref  $REF myG1 %RET
$myG1Ref  @myG1 #FF_FFFF ^BAND  $tAssertEq

$SFN myG1Get  $GET myG1 %RET
$myG1Get  #12345 $tAssertEq

$SFN myG1Set  $_SET myG1 %RET
#6789F $myG1Set   $myG1Get  #6789F $tAssertEq

// *****
// * Testing Locals
// test ldict
#12 =shadowed
$SFN notARealFn %RET // updates ldict
@shadowed #12 $tAssertEq

#45 $ldictSet shadowed
$ldictGet shadowed #45 $tAssertEq
@shadowed #12 $tAssertEq

$FN fooLocals
  @SZ2 $LOCAL myLocal
  $ldictGet myLocal
  @TY_LOCAL  @SZ2 #4 ^SHR  ^BOR  #18 ^SHL
  $tAssertEq

// test R_LP
$SFN getLp %RGFT @R_LP$h1  %RET
$getLp ^DUP #8000 $tAssertEq  =lsTop

$FN getLpWLocal #1$h1  %RGFT @R_LP$h1  %RET // uses locals
$getLpWLocal @lsTop #4 ^SUB $tAssertEq

// test local variables
$FN useLocal
  @SZ2 $LOCAL a   $END_LOCALS
  #12345$L4 $_SET a
  $GET a
  %RET

$useLocal #2345 $tAssertEq

$FN badMultiply // {a b -- a*b} uses loop to implement multiply
  @SZ2 $INPUT a
  @SZ2 $INPUT b
  $END_LOCALS

  #0$L0 // out = 0
  $LOOP l0
    // if(!b) break
    $GET b $BREAK0 b0
    // out = out + a
    $GET a  %ADD
    // b = b - 1
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
$c_number 0b11102  $tAssertNot      ^DRP // not valid binary
$c_number notNum   $tAssertNot      ^DRP // not a number

// characters
$c_number 0cA   $tAssert   #41 $tAssertEq
$c_number 0ca   $tAssert   #61 $tAssertEq
$c_number 0cb   $tAssert   #62 $tAssertEq
$c_number 0cc   $tAssert   #63 $tAssertEq
$c_number 0c\t  $tAssert   #09 $tAssertEq
$c_number 0c\n  $tAssert   #0A $tAssertEq
$c_number 0c    $tAssert   #20 $tAssertEq
$c_number 0c\   $tAssert   #20 $tAssertEq
$c_number 0c\s  $tAssert   #20 $tAssertEq

// Test compiler internals

@TRUE #123 $c_lit #123 $tAssertEq  $assertWsEmpty

$FN two $END_LOCALS #2$L0 %RET // {} -> 1

$SFN test_c_fn   @one $c_fn %RET
$test_c_fn #1 $tAssertEq
$assertWsEmpty

$SFN test_c_fnTwo   @two $c_fn %RET
$test_c_fnTwo #2 $tAssertEq

@test_c_fnTwo $execute
@test_c_fn $execute
  ^ADD #3 $tAssertEq

$SFN test_c_compFnExists $GET c_compFn %RET
$test_c_compFnExists  @fngiSingle $toRef $tAssertEq

// Test essential functions

$c_peekChr $one  #1 $tAssertEq  #24 $tAssertEq // 0x24 = '$'

$SFN comp1 $xsl toRef .4%XW %RET

$SFN testFngiSingleNum @fngiSingle $toRef $comp1 12 %RET
$testFngiSingleNum #C $tAssertEq

$SFN testFngiSingleOne @fngiSingle $comp1 two %RET
$testFngiSingleOne #2 $tAssertEq

$FN withLocals
  @SZ4 $LOCAL s4 @SZ1 $LOCAL s1 $END_LOCALS
  %RET

@withLocals $toRef .1^FT  #2 $tAssertEq

#1 #0 #3 $between $tAssert
#0 #0 #3 $between $tAssert
#3 #0 #3 $between $tAssert

$() // does nothing
$( SFN hi 32 one spor%RET )
$hi
  #1 $tAssertEq
  #20 $tAssertEq

$SFN add1 $PRE %INC %RET // {a} -> {a+1}
$( SFN three ret(add1(2)) )
$three  #3 $tAssertEq

$assertWsEmpty
