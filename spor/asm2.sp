// This file creates some essential assembly utilities including

// **********
// * Essential macros:
// These macros must be defined in pure ASM. They build on eachother
// to make the syntax much more readable.

@heap .4^FT =h1  // h1: {val:1} push 1bytes from stack to heap
  .1@heap @ULIT ^ADD, .4%FT // fetch heap {val, heap}
  .4%SWP      .1%SR         // store 1 byte value at heap
  .1@heap @ULIT ^OR,  .4%FT // fetch heap {heap}
  .4%INC      %SRML.2@heap, // heap=heap+1
  %RET        %NOP // aligned

@heap .4^FT =ulit  // ulit: compile a micro literal
  .1%LIT    @ULIT .1,
  .1%ADD    .2%XSL@h1 .2,
  %RET      %NOP // aligned

// 
// @heap .4^FT =h2  // h2: {val:2} push 2bytes from stack to heap
//   @heap$ulit  %FT           // fetch heap {val, heap}
//   .4%SWP    .2%SR             // store 2 byte value at heap
//   @heap$ulit  %FT           // fetch heap {heap}
//   .4%INC2     %SRML.2@heap, // heap=heap+2
//   %RET        %NOP // aligned
// 
// @heap .4^FT =h4  // h4: {val:4} push 4bytes from stack to heap
//   @heap$ulit  %FT           // fetch heap {val, heap}
//   .4%SWP    .4%SR         // store 4 byte value at heap
//   @heap$ulit  %FT           // fetch heap {heap}
//   .4%INC4     %SRML  .2@heap, // heap=heap+2
//   %RET // unaligned
// 
// @heap .4^FT =_dict // setup for dict
//             .4%FTML@c_dictBuf $h2  // dict.buf
//   %NOP      .2%LIT @c_dictHeap $h2 // &dict.heap
//   %RET // unaligned
// 
// @heap .4^FT =dictSet // dctSet: Set "standard" dictionary to next token.
//                 @D_scan$ulit
//   %DVF        .2%XSL@_dict $h2
//   @D_scan$ulit  %DVS
//   %RET // unaligned
// 
// @heap .4^FT =c_setRKeyMeta // {mask:U1} mask current key's 8bit meta
//               .4%FTML @c_rKey $h2 // fetch {mask &key.meta}
//   .4%INC4     .1%FT   // fetch 1 byte {mask meta}
//   .1%OR       .4%FTML @c_rKey $h2
//   .4%INC4
//   .1%SR       %RET // aligned
// 
// %NOP // unaligned
// @heap .4^FT =locSetup // update rKey
//               .4%FTML @c_dictBuf $h2 // dict.buf
//   .2%NOP        %FTML @c_dictHeap $h2 // dict.heap
//   .4%ADD      .4%SRML @c_rKey $h2
//   %RET // unaligned
// 
// // Define $loc for cleaner function/etc names.
// // This creates a name and sets up for local variables.
// @heap .4^FT =loc  // rename locFn or something
//               .4%XSL @locSetup $h2
// 
//   // Set dict[nextToken] = heap
//   @heap$ulit    %FT
//   .4%NOP        %XSL @dictSet $h2
// 
//   // // set rKey as fn type
//   @IS_FN$ulit   %XSL @c_setRKeyMeta $h2
// 
//   // clear locals by setting localDict.heap=dict.heap (start of localDict.buf)
//   #0$ulit     .2%SRML @c_localOffset $h2  // zero localDict.offset
//   #0$ulit     .2%SRML @c_dictLHeap $h2    // zero localDict.heap
//   %RET          %NOP // aligned
// 
// $loc dictGet   // { -> v}
//   @D_scan$ulit    %DVF
//   %NOP          .4%XSL @_dict $h2
//   @D_dict$ulit    %DVF
//   %RET  // unaligned
// 
// .4
// $loc getHeap     %FTML @heap $h2      %RET // unaligned
// $loc setHeap     %SRML @heap $h2      %RET // unaligned
// $loc getTopHeap  %FTML @topHeap $h2   %RET // unaligned
// $loc setTopHeap  %SRML @topHeap $h2   %RET // unaligned
// $loc getSz       @D_sz$ulit       %DVF %RET // aligned
// $loc setSz       @D_sz$ulit       %DVS %RET // unaligned
// $loc getWsLen    @D_wslen$ulit    %DVF %RET // aligned
// $loc c_xsCatch   @D_xsCatch$ulit  %DVF %RET // unaligned
// $loc c_scan      @D_scan$ulit     %DVF %RET // aligned
// $loc c_assemble  @D_assemble$ulit %DVS %RET // unaligned
// 
// %NOP // aligned
// $loc c_assembleNext // scan and assemble
//   @D_scan$ulit  %DVF
//   @D_comp$ulit  %DVS
//   %RET // unaligned
// 
// // Assert checks a condition or panics with an error
// // ex: <some check> @E_myError assert
// $loc assertNot
//   %SWP
//   %NOT            %SWP // fallthrough (aligned)
// $loc assert
//   @D_assert$ulit  %DVF
//   %RET // unaligned
// 
// $loc panic
//                   #0$ulit
//   %SWP            %JMPL @assert$h2  // Panic with an error code
//   %RET // unaligned
// 
// @heap .4^FT =hma2 // {} heap mis-align 2
//                 .2%XSL @getHeap $h2 // {heap}
//   // IF(NOT heap % 2)
//   #2 $ulit      .4%MOD
//   .4%DUP          %NOT
//     %NOP        .2%JZ @heap ^FT #0 $h2 // keep this location on the stk
//     .4%SUB        #4 $ulit
//     .4%ADD        %JMPL @setHeap $h2
//   .4@heap ^FT ^SWP
//   %DRP2           %RET // aligned


// **********
// * ASM Flow Control
// - `$IF ... $END` for if blocks
// - `$LOOP ... $BREAK0 ... $AGAIN $END` for infiinte loops with breaks.
//
// All flow control pushes the current heap on the WS, then END/AGAIN correctly
// stores/jmps the heap where they are called from.


// .4 $loc IF
//   @JZL $h1
//   LIT4 XSL; @JZL $hL4  @c_compMaskInstr $h2 // compile with JZL
//   $xsl getHeap // push location for END
//   LIT #1BAD $mem_jmpl h2 // compile 1BAD to jmp location

// .4 $loc mem_IF
//   LIT4 XSL; @JZL $hL4  @c_compMaskInstr $h2 // compile with JZL
//   $xsl h2 // compile literal
//   $xsl getHeap // push location for END
//   LIT #1BAD $mem_jmpl h2 // compile 1BAD to jmp location
// 
// .4 $loc END $xsl getHeap   .2 SR RET;
// 
// // $LOOP ... $BREAK0 ... $AGAIN $END
// $loc LOOP   $jmpl getHeap // push location for AGAIN
// $loc BREAK0 $xsl IF   SWP RET;
// $loc mem_BREAK0 $xsl mem_IF   SWP RET;
// $loc AGAIN
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

$loc topU4And // {v:U4 mask:U1} mask upper 8bits of U4
                #18$ulit
  .4 %SHL       %AND
  %RET // unaligned

// TODO: don't use this one, tried to do it w/out .4LIT
// $loc chkXNoLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//                   .4%DUP // {metaFnAddr, metaFnAddr}
//   @IS_LARGE_FN$ulit   %XSL @topU4And $h2 // {metaFnAddr isLocal}
//   %NOP            .4%LIT @E_cXHasL $h2
//   %NOP            .4%LIT @assertNot $h2 // {metaFnAddr}
//   .1 %LIT           #FF $h1
//   #10               %SHL
//   %NOP            .2%LIT #FFFF $h2
//   %AND            %RET

// $loc chkXNoLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//   .4 DUP; // {metaFnAddr, metaFnAddr}
//   .4 LIT XSL; @IS_LARGE_FN $h2  @topU4And $h2 // {metaFnAddr isLocal}
//   .4 LIT XSL; @E_cXHasL $h2  @assertNot $h2  // {metaFnAddr}
//   .4 LIT4; #FF_FFFF $hL4
//   .4 AND RET;
// 
// .4 $loc xsl // compile instr with XSL, see docs on core macros.
//   LIT4 XSL; @XSL $hL4  @c_compMaskInstr $h2 // compile instr with XSL
//   XSL; @dictGet $h2 // get next token's ptr
//   XSL; @chkXNoLocals $h2 // check that it's valid
//   JMPL; @h2 $h2 // put on stack
// 
// .4 $loc jmpl // compile instr with JMPL, see docs on core macros.
//   LIT4 XSL; @JMPL $hL4  @c_compMaskInstr $h2 // compile with JMPL
//   $xsl dictGet // get next token's ptr
//   $xsl chkXNoLocals // check that it's valid
//   JMPL; @h2 $h2 // put on stack
// 
// .4 $loc mem_xsl // compile mem+instr with XSL, see docs on core macros.
//   LIT4 XSL; @XSL $hL4 @c_compMaskInstr $h2 // compile w/XSL
//   $xsl h2 // compile lit
//   $xsl dictGet // get next token value
//   $xsl chkXNoLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// .4 $loc mem_jmpl // compile mem+instr with JMPL, see docs on core macros.
//   LIT4 XSL; @JMPL $hL4 @c_compMaskInstr $h2 // compile w/JMPL
//   $xsl h2 // compile literal
//   $xsl dictGet // get next token value
//   $xsl chkXNoLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// $loc chkXLocals // {metaFnAddr -> fnAddr}
//   // Assert is not local
//   .4 DUP; // {metaFnAddr metaFnAddr}
//   .4 LIT @IS_LARGE_FN $mem_xsl topU4And // {metaFnAddr isLocal}
//   .4 LIT @E_cXNoL $mem_xsl assert // {metaFnAddr}
//   LIT4; #FF_FFFF $hL4 // {metaFnAddr fnAddrMask}
//   AND RET;
// 
// .4 $loc xl // compile instr with XL, see docs on core macros.
//   LIT4 XSL; @XL $hL4 @c_compMaskInstr $h2 // compile w/XL
//   $xsl dictGet // get next token value
//   $xsl chkXLocals // check that it's valid
//   $jmpl h2 // compile it
// 
// .4 $loc mem_xl // compile mem+instr with XL, see docs on core macros.
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
// .4 $loc L // compile next value as literal
//   $xsl c_assembleNext // assemble next token (i.e. @myVal)
//   // Check if it is >= #10000 and use LIT4, else LIT
//   DUP;  LIT INC; #FFFF $h2  GE_U $IF
//     @Sz4 @LIT4 ADD^  LIT $mem_xsl h2 // compile {.4 LIT4}
//     $jmpl hL4 // compile 4byte literal value.
//   $END
//     @Sz2 @LIT ADD^   LIT $mem_xsl h2 // compile {.2 LIT}
//     $jmpl h2 // compile 2byte literal value.
// 
// .4 $loc retL // compile next value as literal and return
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
// $loc XX $xsl dictGet  $xsl chkXLocals  XW; RET;
// 
// // Get next token dict reference
// $loc dictGetR
//   $xsl c_scan
//   .4 LIT DVF RET; @D_rdict $h2
// 
// .4 $loc hAlign4 // align heap
//   $xsl getHeap  DUP;
//   LIT MOD; #4 $h2 // {heap heap%4}
//   DUP $IF
//     SUB;
//     ADD LIT #4 $mem_jmpl setHeap // heap = heap - (heap%4) + 4
//   $END  DRP2 RET;
// 
// .4
// 
// $loc assertWsEmpty  $xsl getWsLen  $L @E_wsEmpty  $jmpl assertNot
// $loc tAssert        LIT @E_test $mem_jmpl assert
// $loc tAssertNot     LIT @E_test $mem_jmpl assertNot
// $loc tAssertEq      .4 EQ  $jmpl tAssert
// $loc tAssertNe      .4 NEQ $jmpl tAssert
// 
// // **********
// // * Local Variables
// // Local variables are simply offsets that are fetch/store using FTLL and SRLL
// // They are defined like:
// // $loc foo
// //   #4 $decl_lo a
// //   #2 $decl_lo b
// //   $end_lo
// //   SRLL ZERO; $lo a $h2 // a = 0
// 
// $loc c_setRKeyFnMeta // {mask:U1} mask upper 8bits (fn meta) of current key's value
//   .4 LIT SHL; #18 $h2 // mask=mask << 24
//   .4 FTML FT; @c_rKey $h2 // double fetch {mask key.value}
//   .4 OR; // { maskedValue }
//   FTML SWP; @c_rKey $h2 // { &key newValue }
//   .4 SR RET;
// 
// $loc _lDict
//   .4 LIT DVF    ; .2@D_scan,
//   // localDict.buf = dict.buf + dict.heap
//   .4 LIT4; @c_dictBuf $hL4  .4 FT;
//   .4 LIT4; @c_dictHeap $hL4 .2 FT;
//   ADD;
//   .4 LIT4 RET; @c_dictLHeap $hL4    // &localDict.heap
// 
// $loc lDictSet $xsl _lDict   .4 LIT DVS RET; @D_dict $h2 // {value}
// $loc lo $xsl _lDict   .4 LIT DVF RET; @D_dict $h2 // {-> value}
// 
// $loc alignSz // {value sz -> aligned} align value by sz bytes
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
// // It is the job of the programmer to ensure they are aligned.
// $loc decl_lo
//   .4 DUP; // {sz sz}
//   .2 FTML SWP @c_localOffset $mem_xl alignSz // {sz alignedOffset}
//   .4 DUP $xsl lDictSet // set next token to alignedOffset {sz alignedOffset}
//   .2 ADD SRML RET; @c_localOffset $h2 // add together and update vLocalOffset
// 
// $loc end_lo // use the current declared local offsets to define the locals size
//   LIT @IS_LARGE_FN $mem_xsl c_setRKeyFnMeta // make it a locals fn
//   .2 FTML; @c_localOffset $h2
//   LIT #4 $mem_xl alignSz
//   SHR LIT #2 $mem_jmpl h2 // shift right 2 (divide by 4) and store.
// 
// $loc getl // get a local variable
//   LIT4 XSL; @FTLL $hL4 @c_compMaskInstr $h2
//   $xsl lo // get next local token value (offset)
//   JMPL; @h2 $h2 // put on stack (literal)
// 
// $loc setl // set a local variable
//   LIT4 XSL; @SRLL $hL4 @c_compMaskInstr $h2
//   $xsl lo // get next local token value (offset)
//   JMPL; @h2 $h2 // put on stack (literal)
// 
// $assertWsEmpty
