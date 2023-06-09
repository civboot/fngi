" Vim syntax file
" Language: Spore ASM
" Maintainer: Rett Berg
" Latest Revision: 12 Nov 2021

if exists("b:current_syntax")
  finish
endif

syn match elAlpha    '[_a-zA-Z0-9]\+' nextgroup=elKey
syn keyword elKey
      \ ret retIfNot reteq retIf retLt retGe
      \ fn do stk unty NULL Any declared
      \ loop while break cont
      \ if elif else
      \ global const
      \ mod use struct role impl meth absmeth
      \ jn msk xor or and inv neg not
      \ drp ovr swp
      \ ge_s gt_s le_s lt_s
      \ nextgroup=elSymbol

syn match elSymbol    '[^()%#'._a-zA-Z0-9]' nextgroup=elSpecial
syn match elSpecial   '[()%#'.]' nextgroup=elDecimal
syn match elDecimal    '_0-9' nextgroup=elHex
syn match elHex       '0x_0-9a-fA-F' nextgroup=elBin
syn match elBin       '0b01' nextgroup=elImm
syn match elImm       'imm#[_0-9a-zA-Z]\+' nextgroup=elCommentLn
syn match elCommentToken '\\\w\+' nextgroup=elCommentLine
syn match elCommentLn '\\\(\ .*\)\?$' nextgroup=elCommentBlk
syn match elCommentBlk '\\[(].\{-}[)]' nextgroup=elStrRaw
syn match elStrRaw     '|.*|'

let b:current_syntax = "fngi"

hi def link elKey           PreProc
hi def link elSymbol        Type
hi def link elSpecial       Keyword
hi def link elDecimal       Constant
hi def link elHex           Constant
hi def link elBin           Constant
hi def link elImm           Macro
hi def link elCommentLn     Comment
hi def link elCommentToken  Comment
hi def link elCommentBlk    Comment
hi def link elStrRaw        String
