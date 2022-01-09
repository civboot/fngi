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
  @c_rKey @REF_MASK ^AND .4^FT
  @c_dictBuf @REF_MASK ^AND .4^FT   @c_dictHeap @REF_MASK ^AND .2^FT   .4^ADD
  $tAssertEq


// Test functions
$assertWsEmpty

// Test core macros

$SFN one   #1 $L0 %RET
$SFN add   %ADD %RET

$SFN test_xslJmpl $xsl one   $xsl one   $jmpl add
$test_xslJmpl  #2 $tAssertEq

$dictGet SFN     $isFnInstant $tAssert
$dictGet FN      $isFnInstant $tAssert
$dictGet IF      $isTyFn      $tAssert
$dictGet IF      $isFnInstant $tAssert
$dictGet assert  $isFnInstant $tAssertNot

// Test control flow
.4 $loc testIf // converts 1->10 else: 42
  #1 $L0 %EQ  $IF   #4 $L0 %RET
              $END  #13 $L0 %RET

#1 $testIf      #4 $tAssertEq
#2 $testIf      #13 $tAssertEq

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
@myG1 #FF_FFFF ^AND .4^FT  #12345 $tAssertEq

$SFN myG1Ref  $REF myG1 %RET
$myG1Ref  @myG1 #FF_FFFF ^AND  $tAssertEq

$SFN myG1Get  $GET myG1 %RET
$myG1Get  #12345 $tAssertEq

$SFN myG1Set  $SET myG1 %RET
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

@SZ2 $LOCAL myLocal
  $ldictGet myLocal
  @TY_LOCAL  @SZ2 #4 ^SHR  ^OR  #18 ^SHL
  $tAssertEq

// test R_LP
$SFN getLp %RGFT @R_LP$h1  %RET
$getLp ^DUP #8000 $tAssertEq  =lsTop

$FN getLpWLocal #1$h1  %RGFT @R_LP$h1  %RET // uses locals
$getLpWLocal @lsTop #4 ^SUB $tAssertEq

// test local variables
$FN useLocal
  @SZ2 $LOCAL a   $END_LOCALS
  #12345$L4 $SET a
  $GET a
  %RET

$useLocal #2345 $tAssertEq

$FN badMultiply // {a b -- a*b} uses loop to implement multiply
  @SZ2 $LOCAL b
  @SZ2 $LOCAL a
  $END_LOCALS
  $SET b
  $SET a

  #0$L0 // out = 0
  $LOOP
    // if(!b) break
    $GET b $BREAK0
    // out = out + a
    $GET a  %ADD
    // b = b - 1
    $GET b  %DEC  $SET b
  $AGAIN
  $END // end break
  %RET

#0 #0 $badMultiply   #0   $tAssertEq
#5 #1 $badMultiply   #5   $tAssertEq
#5 #0 $badMultiply   #0   $tAssertEq
#8 #8 $badMultiply   #40  $tAssertEq
#5 #5 $badMultiply   #19  $tAssertEq

$assertWsEmpty

// #200 #2 $XX alignSz  #200 $tAssertEq
// #201 #2 $XX alignSz  #202 $tAssertEq
// #201 #4 $XX alignSz  #204 $tAssertEq
// 
// 
// // Test fetching/storing
// $hAlign4
// $loc testVal #12345 $h4
// .4 @testVal FT^  #12345 $tAssertEq
// .4 @testVal #2 SR^   @testVal FT^  #2 $tAssertEq
// 
// // Test dict commands
// #0 =dictVal @dictVal    $tAssertNot
// 
// // Test catch
// $loc failNow NOP; NOP;  LIT #0 $mem_jmpl tAssert
// 
// $assertWsEmpty #1 #2 // these will be discarded
// @failNow $c_xsCatch    @E_test $tAssertEq
// $assertWsEmpty
// 
// 
// / #0
// / .4 @cAllowPanicMask  @state FT^OR^ @state SWP^SR^
// / $tAssert
