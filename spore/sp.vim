" Vim syntax file
" Language: Spore
" Maintainer: Rett Berg
" Latest Revision: 14 Nov 2021

if exists("b:current_syntax")
  finish
endif


syn match elAlpha    '[_a-zA-Z0-9]\+' nextGroup=elSingleAsm
syn match elSingleAsm '%%[^\s]\+'     nextGroup=elSymbol
syn match elSymbol    '[^!'()_a-zA-Z0-9.]' nextgroup=elSpecial
syn match elSpecial   '[!'()]' nextgroup=elNum
syn match elNum       '#[_0-9a-fA-F]\+' nextgroup=elInstant
syn match elInstant   '$[_0-9a-zA-Z]\+' nextgroup=elComment
syn match elComment   '/.*$'

let b:current_syntax = "spore"

hi def link elSingleAsm   PreProc
hi def link elSymbol      Type
hi def link elSpecial     Constant
hi def link elNum         Constant
hi def link elInstant     Constant
hi def link elComment     Comment
