// This file creates some essential assembly utilities including

// **********
// * Essential macros:
// These macros must be defined in pure ASM. They build on eachother
// to make the syntax much more readable.

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
  %FT       .2%SR           // store 2 byte value at heap
  @heap$_L0   %FT           // {heap}
  .4%INC2     %SRML .2@heap, // heap=heap+2
  %RET // (unaligned)

@heap .4^FT =h4  // h4: {val:4} push 4bytes from stack to heap
            @heap$_L0
  %FT       .4%SR           // store 4 byte value at heap
  @heap$_L0   %FT           // {heap}
  .4%INC4     %SRML .2@heap, // heap=heap+4
  %RET // (unaligned)

@heap .4^FT =getHeap     %FTML @heap $h2      %RET // (unaligned)
@heap .4^FT =setHeap     %SRML @heap $h2      %RET // (unaligned)
@heap .4^FT =getTopHeap  %FTML @topHeap $h2   %RET // (unaligned)
@heap .4^FT =setTopHeap  %SRML @topHeap $h2   %RET // (unaligned)

%NOP // aligned
@heap .4^FT =hpad // {pad} write pad bytes to heap.
  // WHILE(pad) we write NOP to heap
  @heap ^FT // c-stk{loopStart}
    .4%DUP        .2%JZL // if(pad == 0) breakTo
      .4@heap ^FT ^SWP #0 $h2 // c-stk{breakTo loopStart}
    @NOP$_L0      .2%XSL @h1 $h2 // write a noop
    .4%DEC        .2%JMPL    $h2 // DEC and jmp to loopStart
  .4@heap ^FT ^SWP .2^SR // update breakTo spot
  %DRP           %RET // (aligned)

@heap .4^FT =_hal // {align} heap align (for) literal
  .4%DUP        .2%XSL @getHeap $h2 // {align align heap}
  .4%SWP        .4%MOD // {align heap%align}
  // pad = (align-1) - heap%align
  .4%SWP          %DEC // {heap%align align-1}
  .4%SWP          %SUB
  .4%NOP        .2%JMPL @hpad $h2

// halN: heap align (for) N-byte literal.
@heap .4^FT =hal2   #2$_L0         .2%JMPL @_hal $h2 // (aligned)
@heap .4^FT =hal4   #4$_L0         .2%JMPL @_hal $h2 // (aligned)


// Assert checks a condition or panics with an error
// ex: <some check> @E_myError assert
$hal2
@heap .4^FT =assertNot // {failIfTrue errCode}
                  %SWP
  %NOT            %SWP // fallthrough (aligned)
@heap .4^FT =assert    // {failIfFalse errCode}
  @D_assert$_L0    %DVFT
  %RET // (unaligned)

$hal2
@heap .4^FT =tAssert
        .2%LIT @E_test $h2
  $hal2 %JMPL @assert $h2

@heap .4^FT =tAssertNot     .4%NOT $hal2 .2%JMPL @tAssert,
@heap .4^FT =tAssertEq      .4%EQ  $hal2 .2%JMPL @tAssert,
@heap .4^FT =tAssertNe      .4%NEQ $hal2 .2%JMPL @tAssert,

$hal2
@heap .4^FT =_ha // {align} heap align (with NOPs)
                .2%XSL @getHeap $h2 // {align heap}
  .4%SWP        .4%MOD // {heap%align}
  $hal2 .2%JMPL @hpad $h2


// haN: heap align N byte.
#2 $_ha
@heap .4^FT =ha2   #2$_L0  .2%JMPL @_ha $h2
@heap .4^FT =ha4   #4$_L0  .2%JMPL @_ha $h2

$hal2
@heap .4^FT =_dict // setup for dict
            .4%FTML@c_dictBuf $h2  // dict.buf
  %NOP      .2%LIT @c_dictHeap $h2 // &dict.heap
  %RET

@heap .4^FT =dictSet // dictSet: Set "standard" dictionary to next token.
  @D_scan$_L0 %DVFT // scan next token
  $hal2 .2%XSL@_dict $h2 // select "main" dictionary
  @D_dict$_L0  %DVSR
  %RET

@heap .4^FT =dictGet // dictGet: Get the value of the next token.
  @D_scan$_L0 %DVFT
  $hal2 .2%XSL @_dict $h2
  @D_dict$_L0  %DVFT
  %RET

$hal2
@heap .4^FT =c_updateRKey
        .4%FTML @c_dictBuf $h2  // dict.buf
  $hal2 .2%FTML @c_dictHeap $h2 // dict.heap
  .4%ADD // {&newKey}
  $hal2 .4%SRML @c_rKey $h2 // rKey=newKey
  %RET

@heap .4^FT =c_setRKeyMeta // {mask:U1} mask current key's 8bit meta
  #18 $_L0 .4%SHL           // make mask for upper byte
  $hal2 .4%FTML @c_rKey $h2 .4 %FT // {mask key.meta}
  %OR // updated {key.meta}
  $hal2 .4%FTML @c_rKey $h2 .4 %SR // key.meta = (mask<<24) | key.meta
  %RET

$hal2
// Define $c_fn for cleaner function/etc names.
// This creates a name and sets up for local variables.
@heap .4^FT =c_fn
            .2%XSL @c_updateRKey $h2

  // Set dict[nextToken] = heap
  @heap$_L0 .4%FT   $hal2 .2%XSL @dictSet $h2

  // set rKey as fn type
  @IS_FN$_L0     .2%XSL @c_setRKeyMeta $h2

  $ha2
  // clear locals by setting localDict.heap=dict.heap (start of localDict.buf)
  #0$_L0       .2%SRML @c_localOffset $h2  // zero localDict.offset
  #0$_L0       .4%SRML @c_dictLHeap $h2    // zero localDict.heap
  %RET

.4
$c_fn getSz       @D_sz$_L0         %DVFT %RET
// $c_fn setSz       @D_sz$_L0         %DVSR %RET // (unaligned)
// $c_fn getWsLen    @D_wslen$_L0      %DVFT %RET // (aligned)
// $c_fn c_xsCatch   @D_xsCatch$_L0    %DVFT %RET // (unaligned)
// $c_fn c_scan      @D_scan$_L0       %DVFT %RET // (aligned)
// $c_fn c_assemble  @D_assemble$_L0   %DVSR %RET // (unaligned)
// 
// %NOP // (aligned)
// $c_fn c_assembleNext // scan and assemble
//   @D_scan$_L0    %DVFT
//   @D_comp$_L0    %DVSR
//   %RET // (unaligned)
// 
// 
// $c_fn panic
//                   #0$_L0  
//   %SWP            %JMPL @assert$h2  // Panic with an error code
//   %RET // (unaligned)
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
// // $LOOP ... $BREAK0 ... $AGAIN $END
// $c_fn LOOP   $jmpl getHeap // push location for AGAIN
// $c_fn BREAK0 $xsl IF   SWP RET;
// $c_fn mem_BREAK0 $xsl mem_IF   SWP RET;
// $c_fn AGAIN
//   LIT4 XSL; @JMPL $hL4  @c_compMaskInstr $h2 // compile with JMPL
//   $jmpl h2 // compile location to jmp to


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
