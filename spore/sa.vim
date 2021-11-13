" Vim syntax file
" Language: Spore ASM
" Maintainer: Rett Berg
" Latest Revision: 12 Nov 2021

if exists("b:current_syntax")
  finish
endif


syn match elAlpha    '[_a-zA-Z0-9]\+' nextGroup=elInstr
syn keyword elInstr
      \ NOJ   JZ    JTBL  JMP   CALL  CNL   RET
      \ WS    IMWS  FTLI  FTMI  FTOI  SRLI  SRMI  SROI
      \ Sz1   Sz2   Sz4   SzA
      \ NOP   SWP   DRP   DRP2  DUP   DUPN  DVL   DVS   RGL   RGS   FT    SR
      \ INC   INC2  INC4  INV   NEG   NOT   CI1   CI2
      \ ADD   SUB   MOD   SHL   SHR   AND   OR    XOR   LAND  LOR
      \ EQ    NEQ   GE_U  LT_U  GE_S  LT_S
      \ MUL   DIV_U DIV_S ;
      \ nextgroup=elInstrEnd
syn match elInstrEnd ';' nextGroup=elSymbol

syn match elSymbol   '[^!'.()_a-zA-Z0-9]' nextgroup=elSpecial
syn match elSpecial   '[!'.()]' nextgroup=elNum
syn match elNum       '#[_0-9a-fA-F]\+' nextgroup=elImm
syn match elImm       '$[_0-9a-zA-Z]\+' nextgroup=elComment
syn match elComment   '/.*$'

let b:current_syntax = "sa"

hi def link elInstr     PreProc
hi def link elInstrEnd  PreProc
hi def link elSymbol    Type
hi def link elSpecial   Constant
hi def link elNum       Constant
hi def link elImm       Constant
hi def link elComment   Comment
