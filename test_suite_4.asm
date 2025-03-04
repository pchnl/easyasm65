!macro start_test_expect_expr .pass {
    jsr init_symbol_table

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    sta err_code
    sta asm_flags

    lda #.pass
    sta pass
    jsr init_pass
}

!macro create_undefined_symbol_for_test .name, .namelength {
    lda #<.name
    sta bas_ptr
    lda #>.name
    sta bas_ptr+1
    ldx #.namelength
    jsr find_or_add_symbol
}

!macro set_symbol_for_test .name, .namelength, .val {
    +create_undefined_symbol_for_test .name, .namelength
    lda #<.val
    ldx #>.val
    ldy #^.val
    ldz #<(.val >>> 24)
    jsr set_symbol_value
}

!macro test_assemble_pc_assign .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .epc, .edefined {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1

    ldx #0
    stx tok_pos
    stx stmt_tokpos
    stx err_code
    stx asm_flags
    stx program_counter
    stx program_counter+1
    jsr assemble_pc_assign

!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    jsr assert_cc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
    +assert_mem_eq_word program_counter, .epc, test_msg_wrong_pc

    !if .edefined {
        lda asm_flags
        and #F_ASM_PC_DEFINED
        +assert_ne test_msg_expected_defined_pc
    } else {
        lda asm_flags
        and #F_ASM_PC_DEFINED
        +assert_eq test_msg_expected_undefined_pc
    }
}

    +test_end
}

test_assemble_pc_assign_tb_1: !byte 0, $ff
test_assemble_pc_assign_tb_2: !byte tk_multiply, 0, tk_equal, 2, tk_number_literal, 0, $dd, $cc, $00, $00, 0, $ff
test_assemble_pc_assign_tb_3: !byte tk_multiply, 0, tk_equal, 2, tk_label_or_reg, 4, 5, 0, $ff
test_assemble_pc_assign_tb_4: !byte tk_multiply, 0, tk_equal, 2, tk_number_literal, 0, $dd, $cc, $bb, $aa, 0, $ff
test_assemble_pc_assign_tb_end:
test_assemble_pc_assign_line_1: !pet "* = label",0

!macro assemble_label_for_test .tokbuf, .tokbufend, .lineaddr {
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1

    ldx #0
    stx tok_pos
    stx stmt_tokpos
    jsr assemble_label
}

!macro test_assemble_label .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .ez, .edefined, .evalue, .ezero {
    +test_start .tnum

    +assemble_label_for_test .tokbuf, .tokbufend, .lineaddr

!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    jsr assert_cc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
    cpz #.ez
    +assert_eq test_msg_wrong_z

    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    ldx .tokbuf + 1
    ldy .tokbuf + 2
    jsr find_or_add_label
    jsr get_symbol_value

    !if .edefined {
        +assert_q_eq_32 .evalue, test_msg_wrong_value

        ldz #3
        lda [attic_ptr],z
        and #F_SYMTBL_DEFINED
        +assert_ne test_msg_expected_defined_value

        !if .ezero {
            ldz #3
            lda [attic_ptr],z
            and #F_SYMTBL_LEADZERO
            +assert_ne test_msg_expected_leading_zero_value
        }
    } else {
        ldz #3
        lda [attic_ptr],z
        and #F_SYMTBL_DEFINED
        +assert_eq test_msg_expected_undefined_value
    }

}

    +test_end
}

test_assemble_label_tb_1: !byte 0, $ff
test_assemble_label_tb_2: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_number_literal, 8, $dd, $cc, $00, $00, 0, $ff
test_assemble_label_tb_3: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_number_literal_leading_zero, 8, $dd, $cc, $00, $00, 0, $ff
test_assemble_label_tb_4: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_label_or_reg, 8, 6, 0, $ff
test_assemble_label_tb_5: !byte tk_label_or_reg, 0, 5, 0, $ff
test_assemble_label_tb_6: !byte tk_label_or_reg, 0, 5, 1, 6, 0, $ff
test_assemble_label_tb_7: !byte tk_label_or_reg, 0, 7, tk_equal, 8, tk_label_or_reg, 10, 6, 0, $ff
test_assemble_label_tb_8: !byte tk_label_or_reg, 0, 7, 0, $ff
test_assemble_label_tb_end:
test_assemble_label_line_1: !pet "label = label2",0
test_assemble_label_line_2: !pet "@cheapo = label2",0

tee_line_1: !pet "label",0

!macro test_expect_addressing_expr .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos {
    +test_start .tnum

    ; TODO: add tests for forced16 and forced8 modes
    jsr init_forced16
    lda asm_flags
    and #!(F_ASM_FORCE_MASK | F_ASM_AREL_MASK)
    sta asm_flags

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3
    lda #<(.str - 4)
    sta line_addr
    lda #>(.str - 4)
    sta line_addr+1
    jsr tokenize
    lda err_code
    beq +
    +print_fail_line test_msg_tokenize
    brk
+
    ldz #3
    stz tok_pos
    jsr expect_addressing_expr
!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte err_code, .eerror, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .eerror_pos, test_msg_wrong_err_pos
} else {
    jsr assert_cc
    cpx #<.emode
    +assert_eq test_msg_wrong_mode
    cpy #>.emode
    +assert_eq test_msg_wrong_mode
    +assert_mem_eq_32 expr_result, .eresult, test_msg_wrong_result
    +assert_mem_eq_byte expr_flags, .eflags, test_msg_wrong_flags
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
}
    +test_end
}

; Syntax errors
test_expect_addressing_expr_21: !pet "lda # : inx",0
test_expect_addressing_expr_22: !pet "lda $fe,w",0
test_expect_addressing_expr_23: !pet "lda $fffe,w",0
test_expect_addressing_expr_24: !pet "lda ($fe),w",0
test_expect_addressing_expr_25: !pet "lda [$fe],w",0
test_expect_addressing_expr_26: !pet "lda lda",0
test_expect_addressing_expr_27: !pet "lda (lda,x)",0
test_expect_addressing_expr_28: !pet "lda ($fe x)",0
test_expect_addressing_expr_29: !pet "lda ($fe,y)",0
test_expect_addressing_expr_30: !pet "lda ($fe,x",0
test_expect_addressing_expr_31: !pet "lda (4,sp",0
test_expect_addressing_expr_32: !pet "lda (4,sp)",0
test_expect_addressing_expr_33: !pet "lda (4,sp),",0
test_expect_addressing_expr_34: !pet "lda (4,sp),x",0
test_expect_addressing_expr_35: !pet "inx iny",0
; Other errors
test_expect_addressing_expr_36: !pet "lda [$fffe],z",0  ; err_value_out_of_range

!macro test_forced16 .tnum, .pc, .do_add, .efound {
    +test_start .tnum

    lda #<.pc
    sta program_counter
    lda #>.pc
    sta program_counter+1
    lda asm_flags
    ora #F_ASM_PC_DEFINED
    sta asm_flags
    !if .do_add {
        jsr add_forced16
    }
    jsr find_forced16
    !if .efound {
        jsr assert_cs
    } else {
        jsr assert_cc
    }

    +test_end
}


!macro test_expect_expr .tnum, .tname, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eresult, .eflags {
    +test_start .tnum

    +print_chr chr_spc
    +print_strlit .tname

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1

    ldx #0
    stx tok_pos
    jsr expect_expr

!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
} else {
    jsr assert_cc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos

    !if .eflags != F_EXPR_UNDEFINED {
        +assert_mem_eq_32 expr_result, .eresult, test_msg_wrong_result
    }

    +assert_mem_eq_byte expr_flags, .eflags, test_msg_wrong_flags
}
    +test_end
}


!macro copy_seglist_for_test .start, .end {
    ldx #.end-.start
    dex
-   lda .start,x
    sta tokbuf,x
    dex
    bpl -
}

; Z=1 list is on tokbuf, Z=0 list is not on tokbuf
!macro compare_seglist_for_test .estart, .eend {
    ldx #.eend-.estart
    dex
-   lda .estart,x
    cmp tokbuf,x
    bne +
    dex
    bpl -
    lda #0
+
}

!macro test_compare_segments_for_sort .tnum, .start, .end, .apos, .bpos, .ec {
    +test_start .tnum
    +copy_seglist_for_test .start, .end
    ldx #.apos
    ldy #.bpos
    jsr compare_segments_for_sort
    !if .ec {
        jsr assert_cs
    } else {
        jsr assert_cc
    }
    +test_end
}

!macro test_swap_segments_for_sort .tnum, .start, .end, .apos, .bpos, .estart, .eend {
    +test_start .tnum
    +copy_seglist_for_test .start, .end
    ldx #.apos
    ldy #.bpos
    jsr swap_segments_for_sort
    +compare_seglist_for_test .estart, .eend
    beq +
    +debug_print "... fail: seglist not on tokbuf"
    brk
+
    +test_end
}

!macro test_add_segment_entry_to_strbuf_for_merge .tnum, .start, .end, .pos {
    +test_start .tnum
    lda #0
    sta line_pos
    +copy_seglist_for_test .start, .end
    ldy #.pos
    jsr add_segment_entry_to_strbuf_for_merge
    ldq strbuf
    cpq tokbuf+.pos
    beq +
    +debug_print "... fail: wrong value"
    brk
+
    +test_end
}

!macro test_merge_segments_for_sort .tnum, .start, .end, .startpos, .midpos, .endpos, .estart, .eend {
    +test_start .tnum
    +copy_seglist_for_test .start, .end
    lda #.startpos
    ldx #.midpos
    ldy #.endpos
    jsr merge_segments_for_sort
    +compare_seglist_for_test .estart, .eend
    beq +
    +debug_print "... fail: seglist not on tokbuf"
    brk
+
    +test_end
}

!macro test_sort_segment_list .tnum, .start, .end, .estart, .eend {
    +test_start .tnum

    lda #$ff
    ldx #0
-   sta tokbuf,x
    sta strbuf,x
    inx
    bne -

    +copy_seglist_for_test .start, .end
    ldx #0
    ldy #.end-.start
    jsr sort_segment_list
    +compare_seglist_for_test .estart, .eend
    beq +
    +debug_print "... fail: seglist not on tokbuf"
    brk
+
    +test_end
}

test_pcs: !word $0010, $0020, $0030, $0040, $0050, $0060
test_seglist_sorted: !32 test_pcs, test_pcs+2, test_pcs+4, test_pcs+6, test_pcs+8, test_pcs+10
test_seglist_sorted_end:
test_seglist_one_swap: !32 test_pcs, test_pcs+6, test_pcs+4, test_pcs+2, test_pcs+8, test_pcs+10
test_seglist_one_swap_end:
test_seglist_merge: !32 test_pcs, test_pcs+4, test_pcs+10, test_pcs+2, test_pcs+6, test_pcs+8
test_seglist_merge_end:
test_seglist_reversed: !32 test_pcs+10, test_pcs+8, test_pcs+6, test_pcs+4, test_pcs+2, test_pcs
test_seglist_reversed_end:
test_seglist_mixed_6: !32 test_pcs+6, test_pcs+2, test_pcs+8, test_pcs+10, test_pcs, test_pcs+4
test_seglist_mixed_6_end:
test_seglist_mixed_2: !32 test_pcs+6, test_pcs+2
test_seglist_mixed_2_end:
test_seglist_mixed_2_result: !32 test_pcs+2, test_pcs+6
test_seglist_mixed_2_result_end:
test_seglist_mixed_3: !32 test_pcs+6, test_pcs+2, test_pcs+8
test_seglist_mixed_3_end:
test_seglist_mixed_3_result: !32 test_pcs+2, test_pcs+6, test_pcs+8
test_seglist_mixed_3_result_end:

!macro test_assemble_line .tnum, .line_addr, .ec, .eerr, .epos {
    +test_start .tnum

    jsr init_symbol_table
    jsr init_segment_table
    lda #0
    sta pass
    sta err_code
    jsr init_pass

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3
    lda #<(.line_addr)  ; (test data will provide bytes 0-3 this time)
    sta line_addr
    lda #>(.line_addr)
    sta line_addr+1

    ldx #$bb
    ldy #$aa
    jsr set_pc

    jsr assemble_line

!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_err_pos
} else {
    jsr assert_cc
}

    +test_end
}

test_assemble_line_1: !pet 1,2,3,4,"; comment", 0
test_assemble_line_2: !pet 1,2,3,4,"* = $c000 ; comment",0
test_assemble_line_3: !pet 1,2,3,4,"label  ; comment",0
test_assemble_line_4: !pet 1,2,3,4,"label = 12345 ; comment",0
test_assemble_line_5: !pet 1,2,3,4,"  lda #7  ; comment",0
test_assemble_line_6: !pet 1,2,3,4,"label lda #7 ; comment",0
test_assemble_line_7: !pet 1,2,3,4,"label: lda #7 ; comment",0
test_assemble_line_8: !pet 1,2,3,4," inx : iny : inz ",0

test_assemble_line_9: !pet 1,2,3,4," inx iny inz ",0
test_assemble_line_10: !pet 1,2,3,4,"*=$$$",0
test_assemble_line_11: !pet 1,2,3,4,"= $c000",0
test_assemble_line_12: !pet 1,2,3,4,"label1 label2",0

test_assemble_line_13: !byte 0,0,0,0,0


!macro set_pc_for_test .addr {
    ldx #<.addr
    ldy #>.addr
    jsr set_pc
}

!macro test_assemble_instruction .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len {
    +test_start .tnum

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3
    lda #<(.str - 4)
    sta line_addr
    lda #>(.str - 4)
    sta line_addr+1
    jsr tokenize
    lda err_code
    beq +
    +print_strlit_line "...test error: tokenize failed"
    brk
+
    jsr init_symbol_table
    jsr init_segment_table
    jsr init_forced16

    ldx #0
    stx pass
    stx tok_pos
    stx stmt_tokpos
    stx err_code
    stx asm_flags
    +set_pc_for_test $0010
    jsr assemble_instruction

!if .ec {
    jsr assert_cs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    jsr assert_cc
    +assert_mem_eq_word program_counter, .epc, test_msg_wrong_pc

    lda #<.ebytes
    sta code_ptr
    lda #>.ebytes
    sta code_ptr+1
    ldx #0
    ldz #.ebytes_len-1
    jsr strbuf_cmp_code_ptr
    cmp #0
    +assert_eq test_msg_wrong_strbuf
}

    +test_end
}

ai_1: !pet "inx",0
ai_1_bytes: !byte $e8
ai_1_bytes_end:
ai_2: !pet "lda #7",0
ai_2_bytes: !byte $a9, $07
ai_2_bytes_end:
ai_3: !pet "lda [$fc],z",0
ai_3_bytes: !byte $ea, $b2, $fc
ai_3_bytes_end:
ai_4: !pet "sta $fc",0
ai_4_bytes: !byte $85, $fc
ai_4_bytes_end:
ai_5: !pet "sta $1600",0
ai_5_bytes: !byte $8d, $00, $16
ai_5_bytes_end:
ai_6: !pet "sta ($fc),y",0
ai_6_bytes: !byte $91, $fc
ai_6_bytes_end:
ai_7: !pet "sta [$fc]",0
ai_7_bytes: !byte $ea, $92, $fc
ai_7_bytes_end:
ai_8: !pet "sta ([$fc]),y",0
ai_8_bytes: !byte $91, $fc
ai_8_bytes_end:
ai_9: !pet "stq $fc",0
ai_9_bytes: !byte $42, $42, $85, $fc
ai_9_bytes_end:
ai_10: !pet "stq $1600",0
ai_10_bytes: !byte $42, $42, $8d, $00, $16
ai_10_bytes_end:
ai_11: !pet "stq ($fc)",0
ai_11_bytes: !byte $42, $42, $92, $fc
ai_11_bytes_end:
ai_12: !pet "stq [$fc]",0
ai_12_bytes: !byte $42, $42, $ea, $92, $fc
ai_12_bytes_end:
ai_13: !pet "sta+2 $fc",0
ai_13_bytes: !byte $8d, $fc, 00
ai_13_bytes_end:
ai_14: !pet "sta+1 $00fc",0
ai_14_bytes: !byte $85, $fc
ai_14_bytes_end:

; (Test sets PC to $0010.)
ai_15: !pet "bra $0010",0
ai_15_bytes: !byte $80, $fe
ai_15_bytes_end:
ai_16: !pet "bra $0008",0
ai_16_bytes: !byte $80, $fc
ai_16_bytes_end:
ai_17: !pet "bra $0013",0
ai_17_bytes: !byte $80, $01
ai_17_bytes_end:
ai_18: !pet "bra $C013",0  ; out of range error
ai_19: !pet "lbra $0010",0
ai_19_bytes: !byte $83, $fe, $ff
ai_19_bytes_end:
ai_20: !pet "lbra $0008",0
ai_20_bytes: !byte $83, $f6, $ff
ai_20_bytes_end:
ai_21: !pet "lbra $0013",0
ai_21_bytes: !byte $83, $01, $00
ai_21_bytes_end:
ai_22: !pet "lbra $C013",0  ; no error
ai_22_bytes: !byte $83, $01, $c0
ai_22_bytes_end:
ai_23: !pet "lbra $1C013",0  ; out of range error
ai_24: !pet "bbs0 $fc,$0010",0
ai_24_bytes: !byte $8f, $fc, $fe
ai_24_bytes_end:
ai_25: !pet "adc $fc,y",0  ; coerce to 16-bit
ai_25_bytes: !byte $79, $fc
ai_25_bytes_end:
ai_26: !pet "ldx $fc,y",0  ; do not coerce to 16-bit
ai_26_bytes: !byte $b6, $fc
ai_26_bytes_end:
ai_27: !pet "ldx $00fc,y",0  ; 16-bit
ai_27_bytes: !byte $be, $fc
ai_27_bytes_end:


!macro test_tokenize .tnum, .str, .etokbuf, .etokbuf_end, .eerror, .eerror_pos {
    +test_start .tnum

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3
    lda #<(.str - 4)
    sta line_addr
    lda #>(.str - 4)
    sta line_addr+1
    jsr tokenize

!if .eerror {
    +assert_mem_eq_byte err_code, .eerror, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .eerror_pos, test_msg_wrong_err_pos
} else {
    ldx #.etokbuf_end-.etokbuf
-   dex
    lda .etokbuf,x
    cmp tokbuf,x
    bne +
    cpx #0
    beq ++
    bra -
+
    brk
++
}

    +test_end
}

test_tokenize_1: !pet 0
test_tokenize_1e: !byte 0, $ff
test_tokenize_2: !pet "    ; comment only",0
test_tokenize_2e: !byte 0, $ff
test_tokenize_3: !pet "\"string literal\"",0
test_tokenize_3e: !byte tk_string_literal, 4, 14, 0, $ff
test_tokenize_4: !pet "12345",0
test_tokenize_4e:
    !byte tk_number_literal, 4
    !32 12345
    !byte 0, $ff
test_tokenize_5: !pet "$DeAdBeEf",0
test_tokenize_5e:
    !byte tk_number_literal, 4
    !32 $deadbeef
    !byte 0, $ff
test_tokenize_6: !pet "tZa",0
test_tokenize_6e: !byte mnemonic_tza, 4, 0, 0, $ff
test_tokenize_7: !pet "!wOrD",0
test_tokenize_7e: !byte po_word, 4, 0, $ff
test_tokenize_8: !pet "xOr",0
test_tokenize_8e: !byte tk_label_or_reg, 4, 3, 0, $ff
test_tokenize_9: !pet ">>>",0
test_tokenize_9e: !byte tk_lsr, 4, 0, $ff
test_tokenize_10: !pet "label",0
test_tokenize_10e: !byte tk_label_or_reg, 4, 5, 0, $ff
test_tokenize_11: !pet "@label",0
test_tokenize_11e: !byte tk_label_or_reg, 4, 6, 0, $ff
test_tokenize_12: !pet "label: lda (45, sp), y  ; comment",0
test_tokenize_12e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_colon, 9
    !byte mnemonic_lda, 11, 0
    !byte tk_lparen, 15
    !byte tk_number_literal, 16
    !32 45
    !byte tk_comma, 18
    !byte tk_label_or_reg, 20, 2
    !byte tk_rparen, 22
    !byte tk_comma, 23
    !byte tk_label_or_reg, 25, 1
    !byte 0, $ff
test_tokenize_13: !pet "label = *+4",0
test_tokenize_13e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_equal, 10
    !byte tk_multiply, 12
    !byte tk_pluses, 13, 1
    !byte tk_number_literal, 14
    !32 4
    !byte 0, $ff
test_tokenize_14: !pet "* = $d000",0
test_tokenize_14e:
    !byte tk_multiply, 4
    !byte tk_equal, 6
    !byte tk_number_literal, 8
    !32 $d000
    !byte 0, $ff
test_tokenize_15: !pet "!to \"lda\", runnable",0
test_tokenize_15e:
    !byte po_to, 4
    !byte tk_string_literal, 8, 3
    !byte tk_comma, 13
    !byte tk_label_or_reg, 15, 8
    !byte 0, $ff
test_tokenize_16: !pet "lda $000a",0
test_tokenize_16e:
    !byte mnemonic_lda, 4, 0
    !byte tk_number_literal_leading_zero, 8
    !32 $000a
    !byte 0, $ff
test_tokenize_17: !pet "!warn $cc | $9d",0
test_tokenize_17e:
    !byte po_warn, 4
    !byte tk_number_literal, 10
    !32 $cc
    !byte tk_pipe2, 14
    !byte tk_number_literal, 16
    !32 $9d
    !byte 0, $ff
test_tokenize_18: !pet "!warn $cc ", 220, " $9d",0  ; other pipe
test_tokenize_18e:
    !byte po_warn, 4
    !byte tk_number_literal, 10
    !32 $cc
    !byte tk_pipe, 14
    !byte tk_number_literal, 16
    !32 $9d
    !byte 0, $ff
test_tokenize_last:

test_tokenize_error_1: !pet "$$$",0



run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-expect-addressing-expr"

    +test_expect_addressing_expr $15, test_expect_addressing_expr_21, 1, 0, 0, 0, 0, err_syntax, 6+4
    ; TODO: this test isn't tokenizing correctly? can't repro in real life
    ; +test_expect_addressing_expr $16, test_expect_addressing_expr_22, 1, 0, 0, 0, 0, err_syntax, 8+4
    +test_expect_addressing_expr $17, test_expect_addressing_expr_23, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $18, test_expect_addressing_expr_24, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $19, test_expect_addressing_expr_25, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $1a, test_expect_addressing_expr_26, 1, 0, 0, 0, 0, err_syntax, 4+4
    +test_expect_addressing_expr $1b, test_expect_addressing_expr_27, 1, 0, 0, 0, 0, err_syntax, 5+4
    +test_expect_addressing_expr $1c, test_expect_addressing_expr_28, 1, 0, 0, 0, 0, err_syntax, 9+4
    +test_expect_addressing_expr $1d, test_expect_addressing_expr_29, 1, 0, 0, 0, 0, err_syntax, 9+4
    +test_expect_addressing_expr $1e, test_expect_addressing_expr_30, 1, 0, 0, 0, 0, err_syntax, $ff ; end of line
    +test_expect_addressing_expr $1f, test_expect_addressing_expr_31, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $20, test_expect_addressing_expr_32, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $21, test_expect_addressing_expr_33, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $22, test_expect_addressing_expr_34, 1, 0, 0, 0, 0, err_syntax, 11+4
    +test_expect_addressing_expr $23, test_expect_addressing_expr_35, 1, 0, 0, 0, 0, err_syntax, 4+4
    +test_expect_addressing_expr $24, test_expect_addressing_expr_36, 1, 0, 0, 0, 0, err_value_out_of_range, 5+4


    +print_chr chr_cr
    +print_strlit_line "test-assemble-pc"
    +start_test_expect_expr 0
    +test_assemble_pc_assign $01, test_assemble_pc_assign_tb_1, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_line_1, 1, 0, 0, 0, 0
    +test_assemble_pc_assign $02, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_line_1, 0, 10, 0, $0000ccdd, 1

    +start_test_expect_expr 0
    +test_assemble_pc_assign $03, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 1, 0, err_pc_undef, 0, 0

    +start_test_expect_expr 0
    +set_symbol_for_test test_assemble_pc_assign_line_1 + 4, 5, $0000aabb
    +test_assemble_pc_assign $04, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 0, 7, 0, $0000aabb, 1

    +start_test_expect_expr 0
    +test_assemble_pc_assign $05, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_tb_end, test_assemble_pc_assign_line_1, 1, 0, err_value_out_of_range, 0, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-assemble-label"
    +start_test_expect_expr 0

    ; .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .ez, .edefined, .evalue, .ezero
    +test_assemble_label $01, test_assemble_label_tb_1, test_assemble_label_tb_2, test_assemble_label_line_1, 1, 0, 0, 0, 0, 0, 0
    +start_test_expect_expr 0
    +test_assemble_label $02, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 0

    +start_test_expect_expr 0
    +test_assemble_label $03, test_assemble_label_tb_3, test_assemble_label_tb_4, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 1

    +start_test_expect_expr 0
    +test_assemble_label $04, test_assemble_label_tb_4, test_assemble_label_tb_5, test_assemble_label_line_1, 0, 8, 0, 0, 0, 0, 0

    +start_test_expect_expr 0
    jsr init_pass
    +test_assemble_label $05, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 1, 0, err_pc_undef, 0, 0, 0, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $06, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $07, test_assemble_label_tb_6, test_assemble_label_tb_7, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr 0
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_assemble_label $08, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 1, 0, err_already_defined, 0, 0, 0, 0

    +start_test_expect_expr 0
    +set_symbol_for_test test_assemble_label_line_2+10, 6, 98765
    +test_assemble_label $09, test_assemble_label_tb_7, test_assemble_label_tb_8, test_assemble_label_line_2, 1, 0, err_label_assign_global_only, 0, 0, 0, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +assemble_label_for_test test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1
    +test_assemble_label $0A, test_assemble_label_tb_8, test_assemble_label_tb_end, test_assemble_label_line_2, 0, 3, 0, 1, 1, $aabb, 0

    +print_chr chr_cr
    +print_strlit_line "test-forced16"
    jsr init_forced16
    +test_forced16 $01, $aabb, 0, 0
    jsr init_forced16
    +test_forced16 $02, $aabb, 1, 1
    +test_forced16 $03, $bbcc, 1, 1
    +test_forced16 $04, $ccdd, 1, 1
    +test_forced16 $05, $aabb, 0, 1
    +test_forced16 $06, $bbcc, 0, 1
    +test_forced16 $07, $ccdd, 0, 1
    jsr init_forced16
    +test_forced16 $08, $aabb, 0, 0
    +test_forced16 $09, $bbcc, 0, 0
    +test_forced16 $0A, $ccdd, 0, 0

    +print_chr chr_cr
    +print_strlit_line "segment sorting"

    +test_compare_segments_for_sort $01, test_seglist_sorted, test_seglist_sorted_end, 0, 4, 0
    +test_compare_segments_for_sort $02, test_seglist_sorted, test_seglist_sorted_end, 4, 16, 0
    +test_compare_segments_for_sort $03, test_seglist_sorted, test_seglist_sorted_end, 8, 0, 1
    +test_compare_segments_for_sort $04, test_seglist_sorted, test_seglist_sorted_end, 20, 20, 1

    +test_swap_segments_for_sort $05, test_seglist_one_swap, test_seglist_one_swap_end, 4, 12, test_seglist_sorted, test_seglist_sorted_end

    +test_add_segment_entry_to_strbuf_for_merge $06, test_seglist_sorted, test_seglist_sorted_end, 8

    +test_merge_segments_for_sort $07, test_seglist_merge, test_seglist_merge_end, 0, 12, 24, test_seglist_sorted, test_seglist_sorted_end

    +test_sort_segment_list $08, test_seglist_sorted, test_seglist_sorted_end, test_seglist_sorted, test_seglist_sorted_end
    +test_sort_segment_list $09, test_seglist_one_swap, test_seglist_one_swap_end, test_seglist_sorted, test_seglist_sorted_end
    +test_sort_segment_list $0A, test_seglist_merge, test_seglist_merge_end, test_seglist_sorted, test_seglist_sorted_end
    +test_sort_segment_list $0B, test_seglist_reversed, test_seglist_reversed_end, test_seglist_sorted, test_seglist_sorted_end
    +test_sort_segment_list $0C, test_seglist_mixed_6, test_seglist_mixed_6_end, test_seglist_sorted, test_seglist_sorted_end

    +test_sort_segment_list $0D, test_seglist_mixed_2, test_seglist_mixed_2_end, test_seglist_mixed_2_result, test_seglist_mixed_2_result_end
    +test_sort_segment_list $0E, test_seglist_mixed_3, test_seglist_mixed_3_end, test_seglist_mixed_3_result, test_seglist_mixed_3_result_end

    +print_chr chr_cr
    +print_strlit_line "test-assemble-line"
    ; .tnum, .line_addr, .ec, .eerr, .epos
    +test_assemble_line $01, test_assemble_line_1, 0, 0, 0
    +test_assemble_line $02, test_assemble_line_2, 0, 0, 0
    +test_assemble_line $03, test_assemble_line_3, 0, 0, 0
    +test_assemble_line $04, test_assemble_line_4, 0, 0, 0
    +test_assemble_line $05, test_assemble_line_5, 0, 0, 0
    +test_assemble_line $06, test_assemble_line_6, 0, 0, 0
    +test_assemble_line $07, test_assemble_line_7, 0, 0, 0
    +test_assemble_line $08, test_assemble_line_8, 0, 0, 0
    +test_assemble_line $09, test_assemble_line_9, 1, err_syntax, 5+4
    +test_assemble_line $0A, test_assemble_line_10, 1, err_syntax, 2+4
    +test_assemble_line $0B, test_assemble_line_11, 1, err_syntax, 0+4
    +test_assemble_line $0C, test_assemble_line_12, 1, err_syntax, 0+4
    +test_assemble_line $0D, test_assemble_line_13, 1, 0, 0+4

    ; TODO: assert segment bytes for successful statements

    +print_chr chr_cr
    +print_strlit_line "test-assemble-instruction"
    ; .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len
    +test_assemble_instruction $01, ai_1, 0, 0, $0011, ai_1_bytes, ai_1_bytes_end-ai_1_bytes
    +test_assemble_instruction $02, ai_2, 0, 0, $0012, ai_2_bytes, ai_2_bytes_end-ai_2_bytes
    +test_assemble_instruction $03, ai_3, 0, 0, $0013, ai_3_bytes, ai_3_bytes_end-ai_3_bytes
    +test_assemble_instruction $04, ai_4, 0, 0, $0012, ai_4_bytes, ai_4_bytes_end-ai_4_bytes
    +test_assemble_instruction $05, ai_5, 0, 0, $0013, ai_5_bytes, ai_5_bytes_end-ai_5_bytes
    +test_assemble_instruction $06, ai_6, 0, 0, $0012, ai_6_bytes, ai_6_bytes_end-ai_6_bytes
    +test_assemble_instruction $07, ai_7, 0, 0, $0013, ai_7_bytes, ai_7_bytes_end-ai_7_bytes
    +test_assemble_instruction $08, ai_8, 0, 0, $0012, ai_8_bytes, ai_8_bytes_end-ai_8_bytes
    +test_assemble_instruction $09, ai_9, 0, 0, $0014, ai_9_bytes, ai_9_bytes_end-ai_9_bytes
    +test_assemble_instruction $0a, ai_10, 0, 0, $0015, ai_10_bytes, ai_10_bytes_end-ai_10_bytes
    +test_assemble_instruction $0b, ai_11, 0, 0, $0014, ai_11_bytes, ai_11_bytes_end-ai_11_bytes
    +test_assemble_instruction $0c, ai_12, 0, 0, $0015, ai_12_bytes, ai_12_bytes_end-ai_12_bytes
    +test_assemble_instruction $0d, ai_13, 0, 0, $0013, ai_13_bytes, ai_13_bytes_end-ai_13_bytes
    +test_assemble_instruction $0e, ai_14, 0, 0, $0012, ai_14_bytes, ai_14_bytes_end-ai_14_bytes

    +test_assemble_instruction $0f, ai_15, 0, 0, $0012, ai_15_bytes, ai_15_bytes_end-ai_15_bytes
    +test_assemble_instruction $10, ai_16, 0, 0, $0012, ai_16_bytes, ai_16_bytes_end-ai_16_bytes
    +test_assemble_instruction $11, ai_17, 0, 0, $0012, ai_17_bytes, ai_17_bytes_end-ai_17_bytes
    +test_assemble_instruction $12, ai_18, 1, err_value_out_of_range, 0, 0, 0
    +test_assemble_instruction $13, ai_19, 0, 0, $0013, ai_19_bytes, ai_19_bytes_end-ai_19_bytes
    +test_assemble_instruction $14, ai_20, 0, 0, $0013, ai_20_bytes, ai_20_bytes_end-ai_20_bytes
    +test_assemble_instruction $15, ai_21, 0, 0, $0013, ai_21_bytes, ai_21_bytes_end-ai_21_bytes
    +test_assemble_instruction $16, ai_22, 0, 0, $0013, ai_22_bytes, ai_22_bytes_end-ai_22_bytes
    +test_assemble_instruction $17, ai_23, 1, err_value_out_of_range, 0, 0, 0
    +test_assemble_instruction $18, ai_24, 0, 0, $0013, ai_24_bytes, ai_24_bytes_end-ai_24_bytes
    +test_assemble_instruction $19, ai_25, 0, 0, $0013, ai_25_bytes, ai_25_bytes_end-ai_25_bytes
    +test_assemble_instruction $1A, ai_26, 0, 0, $0012, ai_26_bytes, ai_26_bytes_end-ai_26_bytes
    +test_assemble_instruction $1B, ai_27, 0, 0, $0013, ai_27_bytes, ai_27_bytes_end-ai_27_bytes

    +print_chr chr_cr
    +print_strlit_line "tokenize"
    +test_tokenize $01, test_tokenize_1, test_tokenize_1e, test_tokenize_2, 0, 0
    +test_tokenize $02, test_tokenize_2, test_tokenize_2e, test_tokenize_3, 0, 0
    +test_tokenize $03, test_tokenize_3, test_tokenize_3e, test_tokenize_4, 0, 0
    +test_tokenize $04, test_tokenize_4, test_tokenize_4e, test_tokenize_5, 0, 0
    +test_tokenize $05, test_tokenize_5, test_tokenize_5e, test_tokenize_6, 0, 0
    +test_tokenize $06, test_tokenize_6, test_tokenize_6e, test_tokenize_7, 0, 0
    +test_tokenize $07, test_tokenize_7, test_tokenize_7e, test_tokenize_8, 0, 0
    +test_tokenize $08, test_tokenize_8, test_tokenize_8e, test_tokenize_9, 0, 0
    +test_tokenize $09, test_tokenize_9, test_tokenize_9e, test_tokenize_10, 0, 0
    +test_tokenize $0A, test_tokenize_10, test_tokenize_10e, test_tokenize_11, 0, 0
    +test_tokenize $0B, test_tokenize_11, test_tokenize_11e, test_tokenize_12, 0, 0
    +test_tokenize $0C, test_tokenize_error_1, 0, 0, 1, 4
    +test_tokenize $0D, test_tokenize_12, test_tokenize_12e, test_tokenize_13, 0, 0
    +test_tokenize $0E, test_tokenize_13, test_tokenize_13e, test_tokenize_14, 0, 0
    +test_tokenize $0F, test_tokenize_14, test_tokenize_14e, test_tokenize_15, 0, 0
    +test_tokenize $10, test_tokenize_15, test_tokenize_15e, test_tokenize_16, 0, 0
    +test_tokenize $11, test_tokenize_16, test_tokenize_16e, test_tokenize_17, 0, 0
    +test_tokenize $12, test_tokenize_17, test_tokenize_17e, test_tokenize_18, 0, 0
    +test_tokenize $13, test_tokenize_18, test_tokenize_18e, test_tokenize_last, 0, 0


    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
