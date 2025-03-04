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
    +print_strlit_line "-- all tests passed --"
    rts
