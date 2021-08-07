
from .imports import *
from enum import Enum, auto as enumAuto

Ptr = U16
USize = U16

UXN_OPTIONS = (
# The below block copied directly from uxn.c
# Copyright (c) 2021 Devine Lu Linvega
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE.
    """
    op_brk, op_lit, op_nop, op_pop, op_dup, op_swp, op_ovr, op_rot,
    op_equ, op_neq, op_gth, op_lth, op_jmp, op_jnz, op_jsr, op_sth,
    op_pek, op_pok, op_ldr, op_str, op_lda, op_sta, op_dei, op_deo,
    op_add, op_sub, op_mul, op_div, op_and, op_ora, op_eor, op_sft,
    """
    # 16-bit
    + """
    op_brk16, op_lit16, op_nop16, op_pop16, op_dup16, op_swp16, op_ovr16, op_rot16
    op_equ16, op_neq16, op_gth16, op_lth16, op_jmp16, op_jnz16, op_jsr16, op_sth16,
    op_pek16, op_pok16, op_ldr16, op_str16, op_lda16, op_sta16, op_dei16, op_deo16,
    op_add16, op_sub16, op_mul16, op_div16, op_and16, op_ora16, op_eor16, op_sft16
    """
).replace(',', '')

uxn = Enum("uxn", UXN_OPTIONS)

def test_uxn():
    assert uxn.op_brk.value == 1
    assert uxn.op_lit.value == 2


# op_pek, op_pok, op_ldr, op_str, op_lda, op_sta, op_dei, op_deo,
# op_add, op_sub, op_mul, op_div, op_and, op_ora, op_eor, op_sft,
# /* 16-bit */
# op_brk,   op_lit16, op_nop,   op_pop16, op_dup16, op_swp16, op_ovr16, op_rot16,
# op_equ16, op_neq16, op_gth16, op_lth16, op_jmp16, op_jnz16, op_jsr16, op_sth16, 
# op_pek16, op_pok16, op_ldr16, op_str16, op_lda16, op_sta16, op_dei16, op_deo16, 
# op_add16, op_sub16, op_mul16, op_div16, op_and16, op_ora16, op_eor16, op_sft16


# Name Lookup Dict (for testing)
uxnName = {
}


# Code Lookup Dict (for testing)
uxnCode = {
}

