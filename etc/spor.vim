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
      \ INC   INC2  INC4  INCA  DEC   INV   NEG   NOT   CI1   CI2
      \ ADD   SUB   MOD   SHL   SHR   MSK   JN    XOR   AND   OR
      \ EQ    NEQ   GE_U  LT_U  GE_S  LT_S
      \ MUL   DIV_U DIV_S
      \ SLIT  SZ1   SZ2   SZ4   SZA
      \ NOJ   JMPL  JMPW  JZL   JTBL  XLL   XLW   XSL   XSW
      \ WS    LIT   FTLL  FTGL  FTOL  SRLL  SRGL  SROL
      \ nextgroup=elSymbol

syn match elSymbol    '[^!'()_a-zA-Z0-9.]' nextgroup=elSpecial
syn match elSpecial   '[!'()]' nextgroup=elNum
syn match elNum       '#[_0-9a-fA-F]\+' nextgroup=elNow
syn match elNow       '$[_0-9a-zA-Z]\+' nextgroup=elInstrDot
syn match elInstrDot  '[.][0-4A]\+'         nextgroup=elComment
syn match elComment   '\\.*$'

let b:current_syntax = "sporeASM"

hi def link elInstr       PreProc
hi def link elInstrSC     PreProc
hi def link elInstrDot    PreProc
hi def link elSymbol      Type
hi def link elSpecial     Keyword
hi def link elNum         Constant
hi def link elNow         Macro
hi def link elComment     Comment
