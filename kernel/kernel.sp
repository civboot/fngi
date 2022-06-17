\ The fngi kernel.
\
\ Included by kernel: constants.sp, errors.sp, globals.sp
\ Kernel passes values: {heap} and has PUB and PUB_NAME set.

\ `pub` is used to put the name of the next item in the public dictionary;
\ by default all names go in the private dictionary.
@TY_FN@TY_FN_SYN^JN:pub %DRP \ make next name public
  .2%LIT@C_PUB_NAME, %FTGL@G_cstate, %JN  %SRGL@G_cstate, %RET

\ STORE_PRV is used to store both code and names in the "private" space,
\ which will be dropped at the end of the kernel instantiation.
$pub @TY_FN@TY_FN_NOW^JN:STORE_PUB \ switch to using public storage.
  .2%LIT@C_PUB,      %FTGL@G_cstate, %JN  %SRGL@G_cstate, %RET
$pub @TY_FN@TY_FN_NOW^JN:STORE_PRIV \ switch to using private storage.
  .2%LIT@C_PUB^INV, %FTGL@G_cstate, %MSK %SRGL@G_cstate, %RET

\ Using DV instr, we define a way to make assertions for tests.
$pub @_FP@TY_FN_INLINE^JN  :assertEq .1#2, %DV@D_assert, %RET \ {l r err}
$pub @_FP:tAssertEq     .2%LIT@E_test, $assertEq %RET \ {chk} test assert
$pub @_FP:tAssertNot       %SLIT .2%JMPL@tAssertEq, \ tAssertEq(_, 0)
$pub @_FP:tAssert     %NOT %SLIT .2%JMPL@tAssertEq, \ tAssertEq(not _, 0)
#55 #54^INC $tAssertEq      #0 $tAssertNot    #1 $tAssert

$STORE_PRIV \ Note: everything until STORE_PUB is in the private space.

@TY_FN:getCState .2%FTGL@G_cstate, %RET  \ for assertions
$getCState@C_PUB^MSK $tAssertNot

@TY_FN:wsLen      .1@SLIT@D_comp_wsLen^JN, %DV@D_comp, %RET
@TY_FN:assertNoWs .2%XSL@wsLen,  .2%JMPL@tAssertNot,
$assertNoWs

$STORE_PUB  $getCState@C_PUB^MSK $tAssert
$pub @_FP@TY_FN_INLINE^JN:bump \ {size align} bump some memory
  .1#3, @SLIT#1^JN, %DV@D_comp, %RET \ Note: #3 is size of INLINE

@_FP:h1 \ {U1}: store 1 byte on heap (unchecked)
  .1@SLIT#1^JN, @SLIT, $bump .1%SR %RET

\ @_FP:L0   \ {U1} compile a small literal (unchecked)
\   .1%LIT#3F, %MSK   \ {U1} truncated to bottom 6 bits
\   .1%LIT@SLIT, %JN  \ {jn(U1, SLIT) get full instr
\   .2%JMPL@h1, \ made into SLIT instr and stored.
\ 
\ $STORE_PRIV
\ @_FP:answer
\   #30$L0
\   #12$L0
\   %ADD
\   %RET
\ 
\ $answer
\ #42$tAssertEq
\ $STORE_PUB

$assertNoWs



\ $pub@_FP:h2 #2$L0 #0$L0 $bump .2%SRBE %RET \ {U2} store on heap (BE)
\ $pub@_FP:h4 #4$L0 #0$L0 $bump .4%SRBE %RET \ {U4} store on heap (BE)
\ 
\ $pub@_FP:L1   \ {U1} compile a 1 byte literal (unchecked)
\   .1%LIT @SZ1@LIT^JN$h1 .2%XSL@h1, \ {U1} compile .1%LIT instr
\   .2%JMPL@h1, \ {} then compile the U1
\ 
\ $pub@_FP:L2   \ {U1} compile a 2 byte literal (unchecked)
\   .1%LIT @SZ2@LIT^JN$h1 .2%XSL@h1, \ {U2} compile .2%LIT instr
\   .2%JMPL@h2, \ {} then compile the U2
\ 
\ \ Scan the token
\ @_FP@TY_FN_INLINE^JN:scan #3$h1 @D_comp_scan$L0 %DV@D_comp$h1 %RET

\ \ {<token> -> value} get token's dictionary value
\ @_FP:kdictGet $scan @D_comp_dGet$L0 %DV@D_comp$h1 %RET
\ 
\ \ {<token> -> value} get token's dictionary value
\ @_FP:kdictRef $scan @G_src
\ 
