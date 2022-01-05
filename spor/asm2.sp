// This file creates some essential assembly utilities including

// **********
// * Essential macros:
// These macros must be defined in pure ASM. They build on eachother
// to make the syntax much more readable.
//
// h1 [U1] -> []                : push 1 byte to heap
// h2 [U2] -> []                : push 2 byte to heap
// h4 [U4] -> []                : push 4 byte to heap
// _L0 [U1] -> []               : compile a small literal (<64), unchecked
// $dictSet <key> [U4] -> []    : set a dictionary key to value
// $dictGet <key> [] -> [U4]    : get dictionary key's value
// $loc <token> [] -> []        : set current heap location to <token>
// hpad [U4] -> []              : pad heap with NOPs
// hal2 [] -> []                : align heap for 2 byte literal
// hal4 [] -> []                : align heap for 4 byte literal
// ha2 [] -> []                 : align heap to 2 bytes
// ha4 [] -> []                 : align heap to 4 bytes
//
// Assertions: these panic with the supplied errCode if cond is not met.
// assert [cond errCode]
// assertNot [cond errCode]
//
// Test Assertions: these panic with E_test if the cond is not met.
// tAssert, tAssertNot, tAssertEq, tAssertNe

@heap .4^FT =h1  // h1: {val:1} push 1bytes from stack to heap
  .1@heap @SLIT ^OR,  .4%FT // fetch heap {val, heap}
  .1%SR                        // store 1 byte value at heap
  // fetch heap and INC
                      .1@heap @SLIT ^OR,
  .4%FT               .4%INC // {heap+1}
  .1@heap @SLIT ^OR,  .4%SR  // heap=heap+1
  %RET                  %NOP // (aligned)

@heap .4^FT =_L0  // _L0: compile a small literal (unchecked)
  .1%LIT    @SLIT,
  .1%OR    .2%XSL @h1,
  %RET // (unaligned)

@heap .4^FT =h2  // h2: {val:2} push 2bytes from stack to heap
            @heap$_L0
  .4%FT       .2%SR          // store 2 byte value at heap
  @heap$_L0   .4%FT          // {heap}
  .4%INC2     %SRML .2@heap, // heap=heap+2
  %RET // (unaligned)

@heap .4^FT =h4  // h4: {val:4} push 4bytes from stack to heap
            @heap$_L0
  %FT       .4%SR           // store 4 byte value at heap
  @heap$_L0   %FT           // {heap}
  .4%INC4     %SRML .2@heap, // heap=heap+4
  %RET // (unaligned)

@heap .4^FT =_dict // setup for dict.
  // Scan next token
            @D_scan$_L0
  %DVFT
  // put {dict.buf &dict.heap} on stack
            .4%FTML @c_dictBuf $h2
  %NOP      .2%LIT @c_dictHeap $h2
  %RET // (unaligned)

@heap .4^FT =dictSet // dictSet: Set "standard" dictionary to next token.
            .2%XSL @_dict $h2
  @D_dict$_L0  %DVSR // device dict SR
  %RET // (unaligned)

@heap .4^FT =dictGet // dictGet: Get the value of the next token.
            .2%XSL @_dict $h2
  @D_dict$_L0  %DVFT
  %RET // (unaligned)

@heap .4^FT =loc // $loc <name>: define a location
            @heap$_L0
  %FT       .2%XSL @dictSet $h2
  %RET // (unaligned)

$loc getHeap     %FTML @heap $h2      %RET // (unaligned)
$loc setHeap     %SRML @heap $h2      %RET // (unaligned)
$loc getTopHeap  %FTML @topHeap $h2   %RET // (unaligned)
$loc setTopHeap  %SRML @topHeap $h2   %RET // (unaligned)

%NOP // (aligned)
$loc hpad // {pad} write pad bytes to heap.
  // WHILE(pad) we write NOP to heap
  @heap ^FT // c-stk{loopStart}
    .4%DUP        .2%JZL // if(pad == 0) breakTo
      .4@heap ^FT ^SWP #0 $h2 // c-stk{breakTo loopStart}
    @NOP$_L0      .2%XSL @h1 $h2 // write a noop
    .4%DEC        .2%JMPL    $h2 // DEC and jmp to loopStart
  .4@heap ^FT ^SWP .2^SR // update breakTo spot
  %DRP           %RET // (aligned)

$loc _hal // {align} heap align (for) literal
  .4%DUP        .2%XSL @getHeap $h2 // {align align heap}
  .4%SWP        .4%MOD // {align heap%align}
  // pad = (align-1) - heap%align
  .4%SWP          %DEC // {heap%align align-1}
  .4%SWP          %SUB
  .4%NOP        .2%JMPL @hpad $h2

// halN: heap align (for) N-byte literal.
$loc hal2   #2$_L0         .2%JMPL @_hal $h2 // (aligned)
$loc hal4   #4$_L0         .2%JMPL @_hal $h2 // (aligned)

// Assert checks a condition or panics with an error
// ex: <some check> @E_myError assert
$hal2 $loc assertNot // {failIfTrue errCode}
                  %SWP
  %NOT            %SWP // fallthrough (aligned)
$loc assert    // {failIfFalse errCode}
  @D_assert$_L0    %DVFT
  %RET // (unaligned)

$hal2 $loc tAssert
        .2%LIT @E_test $h2
  $hal2 %JMPL @assert $h2

$loc tAssertNot     .4%NOT $hal2 .2%JMPL @tAssert,
$loc tAssertEq      .4%EQ  $hal2 .2%JMPL @tAssert,
$loc tAssertNe      .4%NEQ $hal2 .2%JMPL @tAssert,

$hal2 $loc _ha // {align} heap align (with NOPs)
                .2%XSL @getHeap $h2 // {align heap}
  .4%SWP        .4%MOD // {heap%align}
  $hal2 .2%JMPL @hpad $h2

// haN: heap align N byte.
#2 $_ha
$loc ha2   #2$_L0  .2%JMPL @_ha $h2
$loc ha4   #4$_L0  .2%JMPL @_ha $h2

$loc assertFn // {metaRef}
  @IS_FN $_L0 .1%LIT #18 $h1 %SHL %AND
  $hal2 .2%LIT @E_cNotFn $h2
  $hal2 .2%JMPL @assert $h2

$loc assertXs // {metaRef}
  %DUP $hal2 .2%XSL @assertFn $h2
  @FN_LOCALS $_L0 .1%LIT #18 $h1 %SHL %AND
  $hal2 .2%LIT @E_cIsX $h2
  $hal2 .2%JMPL @assertNot $h2

$loc assertSameMod // {metaRef}
  $hal4 .4%LIT #FF_0000 $h4 %AND // {mod}
  $hal2 .2%XSL @getHeap $h2
  $hal4 .4%LIT #FF_0000 $h4 %AND // {mod heapMod}
  %EQ
  $hal2 .2%LIT @E_cMod $h2
  $hal2 .2%JMPL @assert $h2

$loc _xslComp // {metaRef}
  $hal2 .2%XSL @hal2 $h2    // enforce proper alignment
  .1%LIT @SZ2 @XSL ^OR $h1  // get .2%XSL instr
  $hal2 .2%XSL @h1 $h2      // compile it
  $hal4 .4%LIT #FF_FFFF $h4 %AND // {addr} remove meta
  $hal2 .2%JMPL @h2 $h2 // compile addr

$loc _xsl // unchecked xsl
  $hal2 .2%XSL @dictGet $h2 // {metaRef}
  $hal2 .2%JMPL @_xslComp $h2

$loc xsl // $xsl <token> : compile xsl2
  $_xsl dictGet // {metaRef}
  // Assert correct type and module
  %DUP $_xsl assertXs
  %DUP $_xsl assertSameMod
  $_xsl _xslComp %RET

$hal2 $loc c_updateRKey // &key contains the current dict key.
        .4%FTML @c_dictBuf $h2  // dict.buf
  $hal2 .2%FTML @c_dictHeap $h2 // dict.heap
  .4%ADD // {&newKey}
  $hal2 .4%SRML @c_rKey $h2 // rKey=newKey
  %RET

.4$loc metaSet // {U4 mask:U1} -> U4 : apply meta mask to U4 value
  #18 $_L0 %SHL           // make mask be upper byte
  %OR %RET

$loc c_setRKeyMeta // {mask:U1} mask current key's 8bit meta
  $hal2 .4%FTML @c_rKey $h2 .4 %FT // fetch key {mask key}
  %SWP $hal2 .2%XSL @metaSet $h2   // apply mask
  $hal2 .4%FTML @c_rKey $h2 .4 %SR // update key
  %RET

$hal2 $loc c_sfn // $c_sfn <token>: define location of small function
            .2%XSL @c_updateRKey $h2
  $hal2 .2%XSL @loc $h2

  // set rKey as fn type
  @IS_FN$_L0     .2%XSL @c_setRKeyMeta $h2
  %RET

$hal2 $loc c_fn // $c_fn <token>: define location of function with locals
            .2%XSL @c_sfn $h2
  // set rKey as FN_LOCALS
  @FN_LOCALS$_L0     .2%XSL @c_setRKeyMeta $h2
  $ha2
  // clear locals by setting localDict.heap=dict.heap (start of localDict.buf)
  #0$_L0       .2%SRML @c_localOffset $h2  // zero localDict.offset
  #0$_L0       .4%SRML @c_dictLHeap $h2    // zero localDict.heap
  %RET

$c_fn makeSmallFn
  $hal2 .2%XSL 

// // **********
// // * ASM Flow Control
// // - `$IF1 ... $END1` for if blocks
// // - `$LOOP ... $BREAK0 ... $AGAIN $END` for infiinte loops with breaks.
// //
// // All flow control pushes the current heap on the WS, then END/AGAIN correctly
// // stores/jmps the heap where they are called from.
// 
// $ha2 $loc IF1
//   $hal2  .2%XSL @getHeap $h2  // {start} push start location to stack
//   .1%LIT @Sz1 @JZL ^ADD $h1   // .1%JZL literal
//   $hal2  .2%XSL @h1 $h2       // compile it
//   #0$_L0 $hal2 .2%XSL @h1 $h2 // compile 0 (jump pad)
//   %RET
// 
// $loc assertLt128 // {U4}
//   .1%LIT #10 $h1 %LT_U
//   $hal2 .2%LIT @E_cJmpL1 $h2
//   $hal2 .2%XSL @assert
//   %RET
// 
// $loc END1 // {start}
//   .4%DUP // {start start}
//   $hal2  .2%XSL @getHeap $h2 // {start start heap}
//   %SWP %SUB                           // {start (heap-start)}
//   %DUP $hal2 .2%XSL @assertLt128 $h2  // assert that it is < 0x10
//   // store at location after start (1 byte literal)
//   %SWP %INC .1%SR %RET
// 
// // $LOOP ... $BREAK0 ... $AGAIN $END
// $c_fn LOOP   $hal2 .2%XSL @getHeap $h2 // push location for AGAIN
// $c_fn BREAK0 $xsl IF   SWP RET;
// $c_fn mem_BREAK0 $xsl mem_IF   SWP RET;
// $c_fn AGAIN
//   LIT4 XSL; @JMPL $hL4  @c_compMaskInstr $h2 // compile with JMPL
//   $jmpl h2 // compile location to jmp to
// 
// // **********
// // * Compiler helpers
// // These are core compiler helpers for compiling literals, jumps and
// // (unchecked) executes.
// //
// // L [U4] -> []                : compile a literal of the correct size
// 
// $loc szInstr // [sz] -> [instr] : convert a sz in bytes to instr bits
// 
// $loc L // [U4] : compile a literal. Automatically handles size and align.
// 
// $hal4 $loc keyRef // {metaRef} -> {ref}
//         .4%LIT #FF_FFFF $h4
//   .4%AND  %RET
// 
// $hal2 $loc _xsl // $_xsl <token>: compile an .2%XSL to token.
// 
// 
// // **********
// // * Defining and executing functions.
// //
// // Functions have meta bits in the dictionary set, in order to distinguish that
// // they are functions. Confusing between an XSW and an XW is an easy but fatal
// // mistake. These help define functions and do the type checking when compiling
// // functions.
// 
// 
// $ha2 $locX metaChk // {metaRef mask:U1} -> bool: return true if meta bit/s are set.
//   #1 .2, // 1 local variable. [mask]
//   .4#18 $_L0 %SHL    // {metaRef mask} make mask be upper byte
//   %DUP %SRLL #0$h1 // cache .mask {metaRef mask}
//   %AND %FTLL #0$h1 %EQ // (mask AND metaRef) == mask
//   %RET
// 
// $ha2 $locX metaAssert // {metaRef mask:U1 errCode}
//   #1 .2, // 1 local variable. [errCode]
//   .4%SRLL #0$h1 // store errCode
//   $hal2 .2%XL @metaChk $h2
//   .4%FTLL #0$h1  $hal2 .2%XSL @assert $h2 // assert
//   %RET
// 
// .4
// $locXs getSz       @D_sz$_L0         %DVFT %RET
// $locXs setSz       @D_sz$_L0         %DVSR %RET
// $locXs getWsLen    @D_wslen$_L0      %DVFT %RET
// $locXs c_xsCatch   @D_xsCatch$_L0    %DVFT %RET
// $locXs c_scan      @D_scan$_L0       %DVFT %RET
// 
// $locXs c_compNext // scan and compile
//   @D_scan$_L0    %DVFT
//   @D_comp$_L0    %DVSR
//   %RET
// 
// $hal2 $locXs panic // {errCode} panic with errCode
//             #0 $_L0
//   .4%SWP      .2%JMPL @assert$h2  // Panic with an error code
//   %RET
// 
// 
// **********
// * ASM Flow Control
// - `$IF ... $END` for if blocks
// - `$LOOP ... $BREAK0 ... $AGAIN $END` for infiinte loops with breaks.
//
// All flow control pushes the current heap on the WS, then END/AGAIN correctly
// stores/jmps the heap where they are called from.


// .4 $c_fn IF
//   @JZL $h1
//   LIT4 XSL; @JZL $hL4  @c_compMaskInstr $h2 // compile with JZL
//   $xsl getHeap // push location for END
//   LIT #1BAD $mem_jmpl h2 // compile 1BAD to jmp location

// .4 $c_fn mem_IF
//   LIT4 XSL; @JZL $hL4  @c_compMaskInstr $h2 // compile with JZL
//   $xsl h2 // compile literal
//   $xsl getHeap // push location for END
//   LIT #1BAD $mem_jmpl h2 // compile 1BAD to jmp location
// 
// .4 $c_fn END $xsl getHeap   .2 SR RET;
// 


// **********
// * Core macros:
// These are used for executing code with combined instructions and literals,
// as well as setting up forward jumps.
//
// These are combined with the current instruction, using whatever size it is.
//   Ex: `ADD $xsl foo`         -> `ADD XSL; @foo $h2`
// The mem_* variants also include a 2byte literal for use with an arbitrary mem:
//   Ex: `ADD LIT #321 $mem_xsl foo` -> `LIT ADD XSL; #321 $h2 @foo $h2`
//
// Additionally, `$L` or `$retL` can be used to only push literals. They are
// smart and use LIT or LIT4 depending on the size of the value. Note: they
// ignore current instr since LIT4 is an operation.
//   Ex: $L@myVar   $L#1234_5678   $retL#3

// $c_fn topU4And // {v:U4 mask:U1} mask upper 8bits of U4
//                 #18$_L0  
//   .4 %SHL       %AND
//   %RET // (unaligned)

// TODO: don't use this one, tried to do it w/out .4LIT
// $c_fn chkXNoLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//                   .4%DUP // {metaFnAddr, metaFnAddr}
//   @IS_LARGE_FN$_L0     %XSL @topU4And $h2 // {metaFnAddr isLocal}
//   %NOP            .4%LIT @E_cXHasL $h2
//   %NOP            .4%LIT @assertNot $h2 // {metaFnAddr}
//   .1 %LIT           #FF $h1
//   #10               %SHL
//   %NOP            .2%LIT #FFFF $h2
//   %AND            %RET

// $c_fn chkXNoLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//   .4 DUP; // {metaFnAddr, metaFnAddr}
//   .4 LIT XSL; @IS_LARGE_FN $h2  @topU4And $h2 // {metaFnAddr isLocal}
//   .4 LIT XSL; @E_cXHasL $h2  @assertNot $h2  // {metaFnAddr}
//   .4 LIT4; #FF_FFFF $hL4
//   .4 AND RET;
// 
// .4 $c_fn xsl // compile instr with XSL, see docs on core macros.
//   LIT4 XSL; @XSL $hL4  @c_compMaskInstr $h2 // compile instr with XSL
//   XSL; @dictGet $h2 // get next token's ptr
//   XSL; @chkXNoLocals $h2 // check that it's valid
//   JMPL; @h2 $h2 // put on stack
// 
// .4 $c_fn jmpl // compile instr with JMPL, see docs on core macros.
//   LIT4 XSL; @JMPL $hL4  @c_compMaskInstr $h2 // compile with JMPL
//   $xsl dictGet // get next token's ptr
//   $xsl chkXNoLocals // check that it's valid
//   JMPL; @h2 $h2 // put on stack
// 
// .4 $c_fn mem_xsl // compile mem+instr with XSL, see docs on core macros.
//   LIT4 XSL; @XSL $hL4 @c_compMaskInstr $h2 // compile w/XSL
//   $xsl h2 // compile lit
//   $xsl dictGet // get next token value
//   $xsl chkXNoLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// .4 $c_fn mem_jmpl // compile mem+instr with JMPL, see docs on core macros.
//   LIT4 XSL; @JMPL $hL4 @c_compMaskInstr $h2 // compile w/JMPL
//   $xsl h2 // compile literal
//   $xsl dictGet // get next token value
//   $xsl chkXNoLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// $c_fn chkXLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//   .4 DUP; // {metaFnAddr metaFnAddr}
//   .4 LIT @IS_LARGE_FN $mem_xsl topU4And // {metaFnAddr isLocal}
//   .4 LIT @E_cXNoL $mem_xsl assert // {metaFnAddr}
//   LIT4; #FF_FFFF $hL4 // {metaFnAddr fnAddrMask}
//   AND RET;
// 
// .4 $c_fn xl // compile instr with XL, see docs on core macros.
//   LIT4 XSL; @XL $hL4 @c_compMaskInstr $h2 // compile w/XL
//   $xsl dictGet // get next token value
//   $xsl chkXLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// .4 $c_fn mem_xl // compile mem+instr with XL, see docs on core macros.
//   LIT4 XSL; @XL $hL4 @c_compMaskInstr $h2 // compile w/XL
//   $xsl h2 // compile lit
//   $xsl dictGet // get next token value
//   $xsl chkXLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// // **********
// // * Literals
// // $L @myValue allows for easier pushing of literals. It automatically handles
// // 2 or 4 byte literals.
// 
// .4 $c_fn L // compile next value as literal
//   $xsl c_assembleNext // assemble next token (i.e. @myVal)
//   // Check if it is >= #10000 and use LIT4, else LIT
//   DUP;  LIT INC; #FFFF $h2  GE_U $IF
//     @Sz4 @LIT4 ADD^  LIT $mem_xsl h2 // compile {.4 LIT4}
//     $jmpl hL4 // compile 4byte literal value.
//   $END
//     @Sz2 @LIT ADD^   LIT $mem_xsl h2 // compile {.2 LIT}
//     $jmpl h2 // compile 2byte literal value.
// 
// .4 $c_fn retL // compile next value as literal and return
//   $xsl c_assembleNext // assemble next token (i.e. @myVal)
//   // Check if it is >= #10000 and use LIT4, else LIT
//   DUP;  LIT INC; #FFFF $h2   GE_U $IF
//     @Sz4 @LIT4 @RET ADD^ADD^  LIT $mem_xsl h2 // compile {.4 LIT4 RET}
//     $jmpl hL4 // compile 4byte literal value.
//   $END
//     @Sz2 @LIT @RET ADD^ADD^   LIT $mem_xsl h2 // compile {.2 LIT RET}
//     $jmpl h2 // compile 2byte literal value.
// 
// 
// // **********
// // * Utilities
// 
// // $XX: instantly execute non-small function.
// // Ex: $XX foo, where foo has local variables.
// $c_fn XX $xsl dictGet  $xsl chkXLocals  XW; RET;
// 
// // Get next token dict reference
// $c_fn dictGetR
//   $xsl c_scan
//   .4 LIT DVFT RET; @D_rdict $h2
// 
// .4 $c_fn hAlign4 // align heap
//   $xsl getHeap  DUP;
//   LIT MOD; #4 $h2 // {heap heap%4}
//   DUP $IF
//     SUB;
//     ADD LIT #4 $mem_jmpl setHeap // heap = heap - (heap%4) + 4
//   $END  DRP2 RET;
// 
// .4
// 
// $c_fn assertWsEmpty  $xsl getWsLen  $L @E_wsEmpty  $jmpl assertNot
// 
// // **********
// // * Local Variables
// // Local variables are simply offsets that are fetch/store using FTLL and SRLL
// // They are defined like:
// // $c_fn foo
// //   #4 $decl_lo a
// //   #2 $decl_lo b
// //   $end_lo
// //   SRLL ZERO; $lo a $h2 // a = 0
// 
// $c_fn c_setRKeyFnMeta // {mask:U1} mask upper 8bits (fn meta) of current key's value
//   .4 LIT SHL; #18 $h2 // mask=mask << 24
//   .4 FTML FT; @c_rKey $h2 // double fetch {mask key.value}
//   .4 OR; // { maskedValue }
//   FTML SWP; @c_rKey $h2 // { &key newValue }
//   .4 SR RET;
// 
// $c_fn _lDict
//   .4 LIT DVFT    ; .2@D_scan,
//   // localDict.buf = dict.buf + dict.heap
//   .4 LIT4; @c_dictBuf $hL4  .4 FT;
//   .4 LIT4; @c_dictHeap $hL4 .2 FT;
//   ADD;
//   .4 LIT4 RET; @c_dictLHeap $hL4    // &localDict.heap
// 
// $c_fn lDictSet $xsl _lDict   .4 LIT DVSR RET; @D_dict $h2 // {value}
// $c_fn lo $xsl _lDict   .4 LIT DVFT RET; @D_dict $h2 // {-> value}
// 
// $c_fn alignSz // {value sz -> (aligned)} align value by sz bytes
//   @IS_LARGE_FN $c_setRKeyFnMeta
//   #1 $h2 // locals (4bytes)
//   .2 SRLL; #0 $h2 // l0=sz
//   .4 DUP; FTLL MOD; #0 $h2 // {value value%sz}
//   DUP $IF
//     SUB;
//     FTLL ADD RET; #0 $h2 // return: value - value%sz + sz
//   $END   DRP RET;        // return: value
// 
// // {sz}: Declare a local variable offset.
// // Ex: `#4 $decl_lo foo` declares local variable of sz=4
// //
// // It is the job of the programmer to ensure they are (aligned).
// $c_fn decl_lo
//   .4 DUP; // {sz sz}
//   .2 FTML SWP @c_localOffset $mem_xl alignSz // {sz alignedOffset}
//   .4 DUP $xsl lDictSet // set next token to alignedOffset {sz alignedOffset}
//   .2 ADD SRML RET; @c_localOffset $h2 // add together and update vLocalOffset
// 
// $c_fn end_lo // use the current declared local offsets to define the locals size
//   LIT @IS_LARGE_FN $mem_xsl c_setRKeyFnMeta // make it a locals fn
//   .2 FTML; @c_localOffset $h2
//   LIT #4 $mem_xl alignSz
//   SHR LIT #2 $mem_jmpl h2 // shift right 2 (divide by 4) and store.
// 
// $c_fn getl // get a local variable
//   LIT4 XSL; @FTLL $hL4 @c_compMaskInstr $h2
//   $xsl lo // get next local token value (offset)
//   JMPL; @h2 $h2 // put on stack (literal)
// 
// $c_fn setl // set a local variable
//   LIT4 XSL; @SRLL $hL4 @c_compMaskInstr $h2
//   $xsl lo // get next local token value (offset)
//   JMPL; @h2 $h2 // put on stack (literal)
// 
// $assertWsEmpty
