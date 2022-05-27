\ The fngi kernel.
\
\ Note: this file requires constants.sp to be compiled first.

@CODE_HEAP_START @TY_FN=_h  \ { -> heap} get the code heap
  .1%FTGL@kBBA_rooti.2,         \ {rooti} get index block in use
  @SLIT#0B^JN.1,  %SHL          \ {rooti<<12} convert to &block
  .2%FTGL@kBBA_len, %ADD %RET   \ {&block+len} return the current "heap"

$_h @_FP@TY_FN_INLINE^JN =assert #2.1, \ {cond errCode} assert cond or panic
  @SLIT@D_assert^JN.1 %DVFT %RET

$_h @_FP=kbump \ {size -> &data} bump some memory from kernel BBA
  .2%LIT @kBBA, \ {size &bba}
  @D_bump@SLIT^JN.1, %DVSR  \ call D_bumpUnaligned {leftoverSize &leftover &data}
  %SWP %DRP %SWP %NOT \ {&data (not leftoverSize)} next: assert no leftover and data
  .2%LIT@E_newBlock, $assert  %DUP .2%LIT@E_OOM, $assert %RET

$_betterh @_FP=h1   #1@SLIT^JN.1,  .2%XSL @kbump,  .1%SR %RET
\ $_betterh @_FP=kh2   #2 @SLIT ^JN .1,  .2%XSL @kbump,  .2%SRBE %RET
\ $_betterh @_FP=kh4   #4 @SLIT ^JN .1,  .2%XSL @kbump,  .4%SRBE %RET
