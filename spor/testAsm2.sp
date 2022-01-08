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

$c_sfn myXs #22 $L0 %RET
$myXs #22 $tAssertEq

$c_sfn callMyXs $xsl myXs %RET
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

$c_sfn one   #1 $L0 %RET
$c_sfn add   %ADD %RET

$c_sfn test_xslJmpl $xsl one   $xsl one   $jmpl add
$test_xslJmpl  #2 $tAssertEq

// Test control flow
.4 $loc testIf // converts 1->10 else: 42
  #1 $L0 %EQ  $c_if   #4 $L0 %RET
              $c_end  #13 $L0 %RET

#1 $testIf      #4 $tAssertEq
#2 $testIf      #13 $tAssertEq

#1 #2    $min   #1 $tAssertEq
#42 #333 $min   #42 $tAssertEq

$ha2 #1 $h1 // misalign heap
@SZ4 $halN #1 $h1 #12345 $h4 // use them.

// Testing Gloabls
#12345 @SZ4 $c_global myG1   $assertWsEmpty
@myG1 #FF_FFFF ^AND .4^FT  #12345 $tAssertEq

$c_sfn myG1Ref  $gRef myG1 %RET
$myG1Ref  @myG1 #FF_FFFF ^AND  $tAssertEq

$c_sfn myG1Get  $gGet myG1 %RET
$myG1Get  #12345 $tAssertEq

$c_sfn myG1Set  $gSet myG1 %RET
#6789F $myG1Set   $myG1Get  #6789F $tAssertEq

// *****
// * Testing Locals
// test ldict
#12 =shadowed
$c_sfn notARealFn %RET // updates ldict
@shadowed #12 $tAssertEq

#45 $ldictSet shadowed
$ldictGet shadowed #45 $tAssertEq
@shadowed #12 $tAssertEq

@SZ2 $c_local myLocal
  $ldictGet myLocal
  @TY_LOCAL  @SZ2 #4 ^SHR  ^OR  #18 ^SHL
  $tAssertEq

// test R_LP
$c_sfn getLp %RGFT @R_LP$h1  %RET
$getLp ^DUP #8000 $tAssertEq  =lsTop

$c_fn getLpWLocal #1$h1  %RGFT @R_LP$h1  %RET // uses locals
$getLpWLocal @lsTop #4 ^SUB $tAssertEq

// test local variables
$c_fn useLocal
  @SZ2 $c_local a
  $c_localEnd
  #12345$L4 $lSet a
  $lGet a
  %RET

$useLocal #2345 $tAssertEq






// $loc badMultiply // {a b -- a*b} uses loop to implement multiply
//   #2 $decl_lo b
//   #2 $decl_lo a
//   $end_lo
//   .2 $setl b
//   .2 $setl a
// 
//   ZERO; // out = 0
//   $c_loop
//     // if(!b) break
//     .2 FTLL $lo b $mem_c_break0
//     // out = out + l2
//     .2 ADD $getl a
//     // l0 = l0 - 1
//     .2 DEC $getl b   .2 $setl b
//   $c_again
//   $c_end // for break
//   RET;
// 
// #0 #0 $XX badMultiply   #0   $tAssertEq
// #5 #1 $XX badMultiply   #5   $tAssertEq
// #5 #0 $XX badMultiply   #0   $tAssertEq
// #8 #8 $XX badMultiply   #40  $tAssertEq
// #5 #5 $XX badMultiply   #19  $tAssertEq

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
