" Vim syntax file
" Language: Spore ASM
" Maintainer: Rett Berg
" Latest Revision: 12 Nov 2021

if exists("b:current_syntax")
  finish
endif

syn match elAlpha    '[_a-zA-Z0-9]\+' nextgroup=elInstr
syn keyword elInstr
      \ NOP   RETZ  RET   SWP   DRP   OVR   DUP   DUPN  DV
      \ RG    LR    GR    YLD   FT    FTO   SR    SRO
      \ INC   INC2  INC4  INCA  DEC   INV   NEG   NOT   CI1   CI2
      \ ADD   SUB   MOD   SHL   SHR   MSK   JN    XOR   AND   OR
      \ EQ    NEQ   GE_U  LT_U  GE_S  LT_S
      \ MUL   DIV_U DIV_S
      \ SLIT  SZ1   SZ2   SZ4   SZA
      \ NOJ   JMPL  JMPW  JZL   JTBL  XLL   XLW   XSL   SLC   XSW
      \ WS    LIT   FTLL  FTGL  FTOL  SRLL  SRGL  SROL
      \ if else loop while break ret retIfNot reteq retIf retLt retGe
      \ xlw xsw
      \ jn msk xor or and inv neg not
      \ ge_s gt_s le_s lt_s
      \ nextgroup=elSymbol

syn match elSymbol    '[^()%$'._a-zA-Z0-9]' nextgroup=elSpecial
syn match elSpecial   '[()%$'.]' nextgroup=elDecimal
syn match elDecimal    '_0-9' nextgroup=elHex
syn match elHex       '0x_0-9a-fA-F' nextgroup=elBin
syn match elBin       '0b01' nextgroup=elNow
syn match elNow       '$[_0-9a-zA-Z]\+' nextgroup=elCommentLn
syn match elCommentToken '\\\w\+' nextgroup=elCommentLine
syn match elCommentLn '\\\(\ .*\)\?$' nextgroup=elCommentBlk
syn match elCommentBlk '\\[(].\{-}[)]' nextgroup=elStrRaw
syn match elStrRaw     '|.*|'

let b:current_syntax = "sporeASM"

hi def link elInstr         PreProc
hi def link elSymbol        Type
hi def link elSpecial       Keyword
hi def link elDecimal       Constant
hi def link elHex           Constant
hi def link elBin           Constant
hi def link elNow           Macro
hi def link elCommentLn     Comment
hi def link elCommentToken  Comment
hi def link elCommentBlk    Comment
hi def link elStrRaw        String
