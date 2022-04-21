# Code Dump: where code goes to die

## fetch/store local offset

This was replaced by FTO and SRO


In C:
```
FTLO: case SzI2 + FTLO:
    l = fetch(mem, LS_SP + popLit(SzI1), SzIA); // &local
    return WS_PUSH(fetch(mem, l + popLit(SzI1), szI)); // fetch offset
    GOTO_SZ(FTLO, SzI1)
    GOTO_SZ(FTLO, SzI4)
SRLO: case SzI2 + SRLO:
    l = fetch(mem, LS_SP + popLit(SzI1), SzIA); // &local
    return store(mem, l + popLit(SzI1), WS_POP(), szI);
    GOTO_SZ(SRLO, SzI1)
    GOTO_SZ(SRLO, SzI4)
```

In spor test:
```
\ Test FTLO

#1234 @SZ4 $GLOBAL gStruct
#67    @SZ1 $GLOBAL gStruct1
$dictGetK gStruct @SZ4 $GLOBAL gStructR  \ &gStruct

$FN testFTSRLocalOffset \ () -> ()
  @SZ4 $LOCAL _a \ just to test that local offset works
  @SZ4 $LOCAL r $END_LOCALS

  \ Test fetch local offset
  $REF gStruct $_SET r
  .4%FTLO $ldictGet r $h1 #0$h1    #1234$L2 $xsl tAssertEq
  .1%FTLO $ldictGet r $h1 #4$h1    #67$L2 $xsl tAssertEq

  \ Test store local offset
  #4242$L2 .4%SRLO $ldictGet r $h1 #0$h1
  .4%FTLO $ldictGet r $h1 #0$h1    #4242$L2 $xsl tAssertEq

  #15$L2   .1%SRLO $ldictGet r $h1 #4$h1
  .1%FTLO $ldictGet r $h1 #4$h1    #15$L2 $xsl tAssertEq
  %RET
$testFTSRLocalOffset

```

# Dot
```
\ # Dot Compiler (.compiler)
\ The below is the initial compiler for below cases:
\   .var               \ variable fetch
\   .var = <token>     \ variable store
\   .@var              \ variable dereference
\   .@var = <token>    \ variable dereference store
\   .&var              \ variable/function reference
\
\ These are built to be extended by future dot compiler implementations for
\ (i.e.) structs, modules, roles, etc.
\ note: @4:1 [= <token>] syntax is a separate word (not .compiler)


\ fn c_countChr [c -> count]      : count and consume matching characters
$SFN c_countChr $PRE \ { chr -> count } count and consume matching chrs
:wa
  #0$L0 $LOOP l0 \ {chr count}
    %OVR $xsl c_peekChr \ {chr count chr c}
    %NEQ $IF \ {chr count}
      %SWP %DRP %RET $END
    %INC \ inc count. Next line inc tokenLen
    .1%FTGL@c_tokenLen$h2 %INC  .1%SRGL@c_tokenLen$h2
  $AGAIN l0

$SFN c_dotRefs \ { -> dotMeta } get dot meta for de/refs.
  \ Get the dotMeta for preceeding & or @
  $xsl c_peekChr %DUP #26$L0 %EQ $IF \ Reference (&) case
    $xsl c_countChr %DUP #4$L0 %LT_U @E_cBadRefs$L2 $xsl assert
    @DOT_REF$L0 %ADD
  $ELSE %DUP #40$L1 %EQ $IF \ Dereference (@) case
      $xsl c_countChr %DUP #4$L0 %LT_U @E_cBadRefs$L2 $xsl assert
      @DOT_DEREF$L0 %ADD
  $ELSE %DRP #0$L0 \ no meta
  $END $END %RET \ {dotMeta}


#26 $c_countChr &&& #3 $tAssertEq
#26 $c_countChr     #0 $tAssertEq
$c_dotRefs &&       #2 @DOT_REF   ^ADD $tAssertEq
$c_dotRefs @@       #2 @DOT_DEREF ^ADD $tAssertEq
$c_dotRefs          #0                 $tAssertEq
```
