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

// Test rKeyMeta
$c_updateRKey

// assert @rKey == @(&dict.buf + &dict.heap)
  @c_rKey .4^FT
  @c_dictBuf .4^FT @c_dictHeap .2^FT .4^ADD
  $tAssertEq


@heap .4^FT =twelve #12 $_L0 %RET

$twelve #12 $tAssertEq



//$assertWsEmpty
// 
// // Test core macros
// $loc test_h2 .4 LIT RET; #1234_5678 $h2
// $test_h2 #5678 $tAssertEq
// 
// .4 $loc one   LIT RET; #1 $h2
// .4 $loc add   ADD RET;
// 
// $loc test_xslJmpl $xsl one   $xsl one   $jmpl add
// $test_xslJmpl  #2 $tAssertEq
// 
// $loc test_mem_xslJmpl
//   LIT #5 $mem_xsl one // {5, 1}
//   ADD LIT #10 $mem_jmpl add // add({5, 1+10})
// $test_mem_xslJmpl   #16 $tAssertEq $assertWsEmpty
// 
// $loc test_l  $L#420        $retL@DUP
// $test_l  SWP^  #420 $tAssertEq  @DUP $tAssertEq
// 
// $loc test_LIT4 .4 LIT4 RET; #1234 $h2 #5678 $h2
// $test_LIT4 #1234_5678 $tAssertEq
// 
// $loc test_HL4 .4 LIT4 RET; #1234_5678 $hL4
// $test_HL4 #1234_5678 $tAssertEq
// 
// // Test control flow
// .4 $loc testIF // converts 1->10 else: 42
//   LIT EQ #1 $mem_IF  $retL #10
//            $END  $retL #42
// #1 $testIF      #10 $tAssertEq
// #2 $testIF      #42 $tAssertEq
// 
// 
// #200 #2 $XX alignSz  #200 $tAssertEq
// #201 #2 $XX alignSz  #202 $tAssertEq
// #201 #4 $XX alignSz  #204 $tAssertEq
// 
// $loc badMultiply // {a b -- a*b} uses loop to implement multiply
//   #2 $decl_lo b
//   #2 $decl_lo a
//   $end_lo
//   .2 $setl b
//   .2 $setl a
// 
//   ZERO; // out = 0
//   $LOOP
//     // if(!b) break
//     .2 FTLL $lo b $mem_BREAK0
//     // out = out + l2
//     .2 ADD $getl a
//     // l0 = l0 - 1
//     .2 DEC $getl b   .2 $setl b
//   $AGAIN
//   $END // for BREAK0
//   RET;
// 
// #0 #0 $XX badMultiply   #0   $tAssertEq
// #5 #1 $XX badMultiply   #5   $tAssertEq
// #5 #0 $XX badMultiply   #0   $tAssertEq
// #8 #8 $XX badMultiply   #40  $tAssertEq
// #5 #5 $XX badMultiply   #19  $tAssertEq
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
