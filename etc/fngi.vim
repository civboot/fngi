" Vim syntax file
" Language: Spore ASM
" Maintainer: Rett Berg
" Latest Revision: 12 Nov 2021

if exists("b:current_syntax")
  finish
endif

syn match elAlpha    '[_a-zA-Z0-9]\+' nextgroup=elInstr
syn keyword elInstr
      \ NOP   RETZ  RET   SWP   DRP   OVR   DUP   DUPN  DVFT  DVSR
      \ RGFT  RGSR  FT    SR
      \ INC   INC2  INC4  DEC   INV   NEG   NOT   CI1   CI2
      \ ADD   SUB   MOD   SHL   SHR   AND   OR    XOR   LAND  LOR
      \ EQ    NEQ   GE_U  LT_U  GE_S  LT_S
      \ MUL   DIV_U DIV_S
      \ SLIT  SZ1   SZ2   SZ4   SZA
      \ NOJ   JMPL  JMPW  JZL   JTBL  XL    XW    XSL   XSW
      \ WS    LIT   FTLL  FTGL  FTOL  SRLL  SRGL  SROL
      \ if else loop while break ret reteq
      \ nextgroup=elSymbol

syn match elSymbol    '[^()%$'._a-zA-Z0-9]' nextgroup=elSpecial
syn match elSpecial   '[()%$'.]' nextgroup=elDecimal
syn match elDecimal    '_0-9' nextgroup=elHex
syn match elHex       '0x_0-9a-fA-F' nextgroup=elBin
syn match elBin       '0b01' nextgroup=elInstant
syn match elInstant   '$[_0-9a-zA-Z]\+' nextgroup=elInstrDot
syn match elCommentLn '//.*$' nextgroup=elCommentBlock
syn match elCommentBlk '/\*.*\*/' nextgroup=elStrRaw
syn match elStrRaw     '".*"' nextGroup=elStrEsc
syn match elStrEsc     '\\".*\\"'

let b:current_syntax = "sporeASM"

hi def link elInstr       PreProc
hi def link elSymbol      Type
hi def link elSpecial     Keyword
hi def link elDecimal     Constant
hi def link elHex         Constant
hi def link elBin         Constant
hi def link elInstant     Macro
hi def link elCommentLn   Comment
hi def link elCommentBlk  Comment
hi def link elStrEsc      String
hi def link elStrRaw      String
