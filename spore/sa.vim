" Vim syntax file
" Language: Spore ASM
" Maintainer: Rett Berg
" Latest Revision: 12 Nov 2021

if exists("b:current_syntax")
  finish
endif

syn match elAlpha    '[_a-zA-Z0-9]\+' nextGroup=elSymbol
syn match elSymbol   '[^!'$.()_a-zA-Z0-9]' nextgroup=elSpecial
syn match elSpecial   '[!'$.()]' nextgroup=elNum
syn match elNum       '#[_0-9a-fA-F]\+' nextgroup=elComment
syn match elComment   '/.*$'

let b:current_syntax = "sa"

hi def link elSymbol  Type
hi def link elSpecial Constant
hi def link elNum     Constant
hi def link elComment Comment
