// Tests for boot.sa
#1 $tAssert  $assertWsEmpty
#100 #100 $tAssertEq $assertWsEmpty
#100 #101 $tAssertNe


$c_floc testingIf // converts 1->10 else: 42 \n"
  LIT EQ JZL; #1 $h2 $jloc j_to
     LIT RET; #10 $h2
  $jset j_to
  LIT RET; #42 $h2
$c_fnClean
$assertWsEmpty

#1 $testingIf   #10 EQ^ $tAssert $assertWsEmpty
#123 $testingIf #42 EQ^ $tAssert $assertWsEmpty

$c_floc fib // calculate the nth fibronacci
  DUP; LIT LT_U JZL; #2 $h2 $jloc j_notOne // if < 2: return 0
    DRP; LIT RET; #1 $h2
  $jset j_notOne
  DUP;  LIT SUB; #1 $h2  @fib $c_UCAL
  SWP;  LIT SUB; #2 $h2  @fib $c_UCAL
  ADD RET;
$c_fnClean

#0 $fib #1 $tAssertEq   #1 $fib #1 $tAssertEq   #2 $fib #2 $tAssertEq
#3 $fib #3 $tAssertEq   #4 $fib #5 $tAssertEq   #5 $fib #8 $tAssertEq
