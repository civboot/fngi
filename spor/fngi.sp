// Bootstrap outselves outside of spor assembly and into fngi.
// Requires: spor.sa

// ******************
// * Compiler:

// // 4 byte globals
// $hAlign4
// $loc rFN_PTR_MASK .4 #FFFF_FFFF_FFFF,
// $loc rI_LIT       .4 @XSW @LIT ADD^,
// $loc rI_XSW_LIT   .4 @XSW @LIT ADD^,
// $loc rI_XW_LIT    .4 @XW  @LIT ADD^,
// 
// // State Globals
// $loc STATE .2 #0,   // compiler state ` global-bits | fn-meta-bits `
// $loc XSTATE .2 #0, // Execution state of a fn
// 
// // 16bit [global-bits | fn-meta-bits] Masks
// #0001 =LCL_MASK // 1=has local stack
// #0002 =PRE_MASK // 1=is prefix
// #0004 =TNT_MASK // 1=is insTaNT
// #0008 =ALI_MASK // 1=always instant ($ may alter behavior)
// 
// #0100 =EXEC_MASK // 1=everything is always instant.
// #0200 =QUIT_MASK // 1=quit compLoop.
// 
// .2
// $loc c_stateClr // {mask}
//   NOT; FTML AND; @STATE $h2
//   SRML RET; @STATE $h2
// $loc c_stateSet // {mask}
//   FTML AND; @STATE $h2
//   SRML RET; @STATE $h2
// 
// $loc c_toMeta   LIT SHR RET; #18 $h2 // shift to make just 8bit meta
// $loc c_maskPre   LIT AND RET; @PRE_MASK $h2
// $loc c_maskIns // masks for ALI (always instant) and TNT
//   @ALI_MASK $c_LIT;
//   LIT OR; @TNT_MASK $h2
//   AND RET;
// $loc c_maskLcl   LIT AND RET; @LCL_MASK $h2
// 
// $loc c_maskExec  LIT AND RET; @EXEC_MASK $h2
// $loc c_maskQuit  LIT AND RET; @QUIT_MASK $h2
// 
// // Used by ALI (always instant) functions to know whether they've
// // been asked to be "super" instant. Supper instant happens
// // when `$` flips ALI's TNT bit or compiler state is EXEC
// $loc c_stateSuperIns
//   @STATE $c_FTML2   @c_maskExec $c_XSW
//   @XSTATE $c_FTML2  @c_maskIns $c_XSW
//   AND RET;
// 
// $hAlign4
// $loc c_rFN #0 .4, // dict ref of currently compiling fn.
// $loc c_fnDictHeap #0 .4, // dict heap after defining current fn.
// 
// // ******************
// // * Core Syntax: define fns for the core syntax tokens.
// $assertWsEmpty
// 
// // usage: $floc <fn-name>
// // Set location of fngi fn. This is ultra-basic. The syntax is still assembly.
// $loc c_floc
//   #00FF $c_LIT  @c_stateClr $c_XSW // clear toggle bits
//   .2 SRML ZERO; @XSTATE $h2       // clear XSTATE
//   @loc $c_XSW // set location of next token
//   .4 LIT DVF; @D_rdict $h2 // get dictionary reference of current token
//   SRML; @c_rFN $h2 // store to c_rFN
// 
//   @c_dictHeap $c_FTML2
//   .2 SRML RET; @c_fnDictHeap $h2 // update fnDictHeap
// 
// $c_floc c_fnClean
//   @assertWsEmpty $c_XSW
//   @c_fnDictHeap $c_FTML2 // fetch dict heap after current fn
//   .2 SRML RET; @c_dictHeap $h2 // store to dictHeap (forget keys after fn)
// 
// // {8bit mask} apply an 8-bit mask to the c_rFN (current fn ref)
// $c_floc c_fnMask
//   LIT SHL; #18 $h2 // shift mask left by 0x18 (24)
//   FTML FT; @c_rFN $h2 // double fetch of rFN (get metaFn)
//   SRML OR RET; @c_rFN $h2 // bitwise-or mask and store.
// 
// $c_floc INSTANT          @TNT_MASK $c_LIT  @c_fnMask $c_JMPW // make INSTANT
//                 $INSTANT // make INSTANT an instant fn
// $c_floc ALI     $INSTANT @ALI_MASK $c_LIT  @c_fnMask $c_JMPW // make ALI
// $c_floc PRE     $INSTANT @PRE_MASK $c_LIT  @c_fnMask $c_JMPW // make PRE
// $c_floc $       $INSTANT @TNT_MASK $c_LIT  @c_stateSet $c_JMPW  // toggle next INSTANT
// $c_floc '       $INSTANT @PRE_MASK $c_LIT  @c_stateSet $c_JMPW  // toggle next PRE
// $c_floc %%      $INSTANT @c_scan $c_XSW  .4 LIT DVF RET; @D_comp $h2 // asm compile
// $c_floc ASM_BEG          @QUIT_MASK $c_LIT  @c_stateSet $c_JMPW // begin assembly syntax
// 
// // Note: this actually isn't a comment. It's defining comments for fngi.
// $c_floc //      $INSTANT .4 LIT DVF RET; @D_comp $h2 // asm compile
// 
// $loc c_compMaskInstr // {maskInstr:4}
//   @getSz $c_XSW  SWP; // cache sz {sz maskInstr}
//   .4 LIT DVS; @D_instr $h2 // use mask on instr
//   .4 LIT DVF; @D_instr $h2 // get instr
//   @h2 $c_XSW // compile instr to heap
// 
//   // Clear current instr but with cached size
//   .4 FTML ADD; @rI_CLR $h2
//   LIT DVS RET; @D_instr $h2
// 
// $c_floc #       $ALI // Literal Value (hex constant)
//   .4 LIT DVF; @D_comp $h2 // assemble the '#' token with number.
//   // If we are "super instant" simply return to keep number on stack.
//   @c_stateSuperIns $c_XSW  NOT JZL; $jloc j_ret
//   FTML; @rI_LIT $h2   @c_compMaskInstr $c_XSW // compile TNT instr
//   @h2 $c_XSW // Compile the literal value.
//   $jset j_ret   RET
// $c_fnClean   $c_dRef j_ret $tAssertNot // assert j_ret is deleted
// 
// 
// 
// // Force execute a metaFn
// $loc c_forceExec // {metaFn}
//   .4 DUP;  @c_toMeta $c_XSW  @c_maskLcl $c_XSW // get meta
//   SWP;  .4 FTML AND; @rFN_PTR_MASK $h2 SWP; // get masked fn ptr
//   JZL; $jloc j_call  .4 XW; RET;
//   $jset j_call       .4 XSW; RET;
// $dForget j_call
// 
// // Force compile a metaFn to a value.
// $loc c_forceComp // {metaFn}
//   // TODO: make sure token is in same module.
// 
//   // Check if Fn is LCL or not
//   .4 DUP;  @c_toMeta $c_XSW  @c_maskLcl $c_XSW
//   JZL; $jloc j_cnl
//     @rI_XW_LIT $c_FTML4 // (has locals) fetch XW mask+instr
//     LIT JMPW; $jloc j_end
//   $jset j_cnl
//     .4 FTML; @rI_XSW_LIT $h2 // fetch XSW mask+instr
//   $jset j_end
//   @c_compMaskInstr $c_XSW  // mask and compile them mask+instr
//   @h2 $c_JMPW // Compile the fn ptr.
// $dForget j_cnl
// 
// // Core Spore compiler function. Compiles the metaFn passed to it as a fngi
// // function, checking for PRE, TNT, ALI and LCL bits appropriately.
// $loc c_compMetaFn // {metaFn} compile the meta fn
//   // Check the meta for PRE. XOR with preBit to toggle
//   .4 DUP;  @c_toMeta $c_XSW  @c_maskPre $c_XSW
//   @STATE $c_FTML2    @c_maskPre $c_XSW
//   @PRE_MASK $c_LIT   @c_stateClr $c_XSW // clear state.PRE
// 
//   XOR JZL; $jloc j_to // Checking for PRE.
//     // Pre: simply compile next token.
//     // Note: this is a forward call (c_comp not yet defined)
//     LIT XSW; $jloc j_c_comp
//   $jset j_to
// 
//   // Check the meta for TNT or EXEC mode
//   .4 DUP;  @c_toMeta $c_XSW   @c_maskIns $c_XSW
//   @STATE $c_FTML2             @c_maskIns $c_XSW
//   // xor to toggle. We store it in XSTATE so an ALI fn can selected state.
//   XOR; SRML DUP; @XSTATE $h2
// 
//   // also check for state.EXEC which overrides both
//   @STATE $c_FTML2             @c_maskExec $c_XSW
// 
//   @PRE_MASK $c_LIT  @c_stateClr $c_XSW // clear state.TNT
//   LOR JZL; $jloc j_to
//     @c_forceExec $c_JMPW // instant: forge execute
//   $jset j_to
//     @c_forceComp $c_JMPW // not instant: force compile
// $jset j_c_comp    // update location of j_c_comp = c_comp (here)
// $dForget j_to     // forget j_to and j_c_comp
// 
// $loc c_comp
//   // Load dict ref and check it
//   .4 LIT DVF; @D_scan $h2
//   // TODO: check if token is "#"
// 
//   .4 LIT DVF; @D_rdict $h2 // {rValue}
//   // Make sure token exists
//   .4 DUP;  @E_DictDNE $c_LIT  @assert $c_XSW
//   FT; // fetch it {metaFn}
//   @c_compMetaFn $c_JMPW
// 
// $loc ASM_END // fngi compilation loop. Calling will end ASM syntax.
//   @QUIT_MASK $c_LIT  @c_stateClr $c_XSW
//   $loc j_loop
//     @c_comp $c_XSW
//     @STATE $c_FTML2   @c_maskQuit $c_XSW
//     JZL; $jloc j_quit
//     LIT JMPW; @j_loop $h2
//   $jset j_quit RET;
// $dForget j_loop
// 
// 
// $assertWsEmpty
// @EXEC_MASK $c_stateSet