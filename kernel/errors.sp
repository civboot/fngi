\ * [3.d] Errors
\ [E000 - E100): built-in errors.
\  E100: device-specific hardware errors
\ [E200-E800): reserved
\  E800+: application errors
\  AXXX_EXXX: test case assertion error.

#0     #0=E_ok      \ no error
#E000  #0=E_general \ general errors [E000-E010)
#E010  #0=E_io      \ IO error class
#E0A0  #0=E_asm     \ assembly error class (cause in asm).
#E0C0  #0=E_comp    \ compiler error class (cause in comp).
#A000  #0=E_test    \ [AXXX] (assert) test case error.

#E001  #0=E_intern  \ internal (undefined) error
#E002  #0=E_undef   \ undefined error
#E003  #0=E_unreach \ unreachable code
#E004  #0=E_todo    \ executed incomplete (to do) code
#E005  #0=E_wsEmpty \ the WS was expected empty
#E006  #0=E_unimpl  \ unimplemented error
#E007  #0=E_dv      \ unknown device op

#E0A1  #0=E_null    \ null access
#E0A2  #0=E_oob     \ out of bounds access
#E0A3  #0=E_stkUnd  \ Stack underflow
#E0A4  #0=E_stkOvr  \ Stack overflow
#E0A5  #0=E_align2  \ access off 2byte allign
#E0A6  #0=E_align4  \ access off 4byte align
#E0A7  #0=E_divZero \ divide by zero
#E0A8  #0=E_oom     \ out of memory
#E0A9  #0=E_xlSz    \ invalid XL sz

#E0C1  #0=E_cInstr  \ invalid instr
#E0C2  #0=E_cToken  \ token invalid
#E0C3  #0=E_cTLen   \ token invalid
#E0C4  #0=E_cKey    \ key already exists
#E0C5  #0=E_cNoKey  \ dict key not found
#E0C6  #0=E_cHex    \ non-hex number
#E0C7  #0=E_sz      \ invalid Sz selected
#E0C8  #0=E_cSzPtr  \ invalid Sz for aptr
#E0C9  #0=E_cRet    \ invalid RET
#E0CA  #0=E_cDblSr  \ Double store
#E0CB  #0=E_cDevOp  \ device op not impl
#E0CC  #0=E_DictOvr \ dict overflow
#E0CD  #0=E_cXHasL  \ small-execute to fn w/locals
#E0CE  #0=E_cXNoL   \ large-execute to fn wo/locals
#E0CF  #0=E_cErr    \ D_assert err code invalid
#E0D0  #0=E_cKeyLen \ Key len too large
#E0D1  #0=E_cReg    \ Register error
#E0D2  #0=E_cStr    \ Str invalid

#E0E0  #0=E_cNotGlobal \ using a non-global as global
#E0E1  #0=E_cIsX       \ using an XS for an X
#E0E2  #0=E_cIsXS      \ using an X for an XS
#E0E3  #0=E_cJmpL1     \ JMP1 over too much space
#E0E4  #0=E_cNotFn
#E0E5  #0=E_cNotFnLarge
#E0E6  #0=E_cMod       \ different modules
#E0E7  #0=E_cLSz       \ literal sz
#E0E9  #0=E_cNotType
#E0EA  #0=E_cNotLocal
#E0EB  #0=E_cNotVar
#E0EC  #0=E_cNotFnOrConst
#E0ED  #0=E_eof
#E0EE  #0=E_cUnclosed   \ unclosed paren/brace/etc
#E0EF  #0=E_cReqNow     \ fn is NOW but no '$' used
#E0EF  #0=E_cNoNow      \ fn is SYN and requires no $ used.
#E0F0  #0=E_cUnknownEsc \ unknown character escape
#E0F1  #0=E_cZoab       \ Zoab invalid
#E0F2  #0=E_cNeedToken  \ token not present
#E0F3  #0=E_cNeedNumber \ number not present
#E0F4  #0=E_cBadRefs    \ too many de/refs
#E0F5  #0=E_cRefEq      \ .&var = not allowed.
#E0F6  #0=E_cInlineLarge \ inline fn is large
#E0F7  #0=E_cColon      \ Expect : after function
#E0F8  #0=E_cFnSyn      \ Invalid use of SYN function
#E0F9  #0=E_newBlock    \ Require a NEW_BLOCK for code.

#E0B0  #0=E_iBlock      \ invalid block index
#E0B1  #0=E_ptrBlk      \ invalid block ptr
#E0B2  #0=E_aaPo2       \ invalid po2

#00  #0=ERR_DATA_NONE
#01  #0=ERR_DATA_INT1
#02  #0=ERR_DATA_DATA1
#03  #0=ERR_DATA_INT2
#04  #0=ERR_DATA_DATA2
