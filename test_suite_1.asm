!macro test_is_letter .tnum, .in_a, .ec {
    +test_start .tnum

    lda #.in_a
    jsr is_letter
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

!macro test_is_secondary_ident_char .tnum, .in_a, .ec {
    +test_start .tnum

    lda #.in_a
    jsr is_secondary_ident_char
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

!macro test_strbuf_to_lowercase .tnum, .instraddr, .outstraddr {
    +test_start .tnum
    +test_copy_to_strbuf .instraddr
    jsr strbuf_to_lowercase
    +test_match_strbuf .outstraddr
    beq +
    brk
+
    +test_end
}

test_strbuf_to_lowercase_2_in:   !pet "ABC",0
test_strbuf_to_lowercase_3_in:   !pet "aBc",0
test_strbuf_to_lowercase_1_out:  !pet "abc",0

!macro test_strbuf_cmp_code_ptr .tnum, .astraddr, .bstraddr, .maxlen, .ea, .ex {
    +test_start .tnum
    +test_copy_to_strbuf .astraddr
    lda #<.bstraddr
    sta code_ptr
    lda #>.bstraddr
    sta code_ptr+1
    ldz #.maxlen
    ldx #0
    jsr strbuf_cmp_code_ptr
    cmp #.ea
    beq +
    brk
+   cpx #.ex
    beq +
    brk
+
    +test_end
}

test_strbuf_cmp_code_ptr_abc:     !pet "abc",0
test_strbuf_cmp_code_ptr_acb:     !pet "acb",0
test_strbuf_cmp_code_ptr_abcd:     !pet "abcd",0

!macro test_accept_whitespace_and_comment .tnum, .basptr, .pos, .epos, .ezero {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$00
    sta bas_ptr+2
    sta bas_ptr+3
    ldz #.pos
    stz line_pos
    jsr accept_whitespace_and_comment
!if .ezero {
    beq +
    brk
+
}
    lda line_pos
    taz
    cpz #.epos
    beq +
    brk
+
    +test_end
}

test_accept_whitespace_and_comment_empty_line:
    !word +
    !word 12345
    !pet 0
+   !word 0

test_accept_whitespace_and_comment_comment:
    !word +
    !word 12345
    !pet "   ; comment ; with ; semicolons", 0
+   !word 0
test_accept_whitespace_and_comment_comment_length = len("   ; comment ; with ; semicolons")

test_accept_whitespace_and_comment_space_then_stuff:
    !word +
    !word 12345
    !pet "   stuff", 0
+   !word 0

test_accept_whitespace_and_comment_other_spaces:
    !word +
    !word 12345
    !pet "  ", chr_shiftspc, chr_tab, "  stuff", 0
+   !word 0


!macro test_accept_ident .tnum, .basptr, .c, .ec, .ez {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$00
    sta bas_ptr+2
    ldz #0
!if .c {
    sec
} else {
    clc
}
    jsr accept_ident
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}
    cpz #.ez
    beq +
    brk
+
    +test_end
}

test_accept_ident_1: !pet "label  ",0
test_accept_ident_2: !pet "label", chr_backarrow, "12", chr_megaat, "3  ",0
test_accept_ident_3: !pet "0label",0
test_accept_ident_4: !pet "!label",0

!macro test_accept_literal .tnum, .lineaddr, .ec, .eval, .epos, .ezeroflag {
    +test_start .tnum
    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    lda #$00
    sta bas_ptr+2
    ldz #0
    stz line_pos
    jsr accept_literal

!if .ec {
    bcs +
    brk
+   lda #<.eval
    ldx #>.eval
    ldy #^.eval
    ldz #<(.eval >>> 24)
    cpq expr_result
    beq +
    ldq expr_result
    brk
+   lda line_pos
    cmp #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+
}

    lda expr_flags
    and #F_EXPR_FORCE16
!if .ezeroflag {
    bne +
    brk
+
} else {
    beq +
    brk
+
}

    +test_end
}

test_accept_literal_1: !pet "'x'",0
test_accept_literal_2: !pet "'''",0
test_accept_literal_3: !pet "$0",0
test_accept_literal_4: !pet "$1",0
test_accept_literal_5: !pet "$f",0
test_accept_literal_6: !pet "$F",0
test_accept_literal_7: !pet "$deadbeef",0
test_accept_literal_8: !pet "$DEadbeEF",0
test_accept_literal_9: !pet "$000a",0
test_accept_literal_10: !pet "$1000",0
test_accept_literal_11: !pet "%0",0
test_accept_literal_12: !pet "%1",0
test_accept_literal_13: !pet "%0101",0
test_accept_literal_14: !pet "%1010",0
test_accept_literal_15: !pet "%.#.#",0
test_accept_literal_16: !pet "%#.#.",0
test_accept_literal_17: !pet "%....##..####....##..####....####",0
test_accept_literal_18: !pet "0",0
test_accept_literal_19: !pet "9",0
test_accept_literal_20: !pet "1234567890",0
test_accept_literal_21: !pet "0123",0
test_accept_literal_22: !pet "$z",0
test_accept_literal_23: !pet "%z",0
test_accept_literal_24: !pet "a",0
test_accept_literal_25: !pet "$Fg",0
test_accept_literal_26: !pet "%12",0
test_accept_literal_27: !pet "56a",0
test_accept_literal_28: !pet 0

!macro test_find_in_token_list .tnum, .str, .pos, .word_boundary, .ec, .eentry, .epos {
    +test_start .tnum

    lda #<mnemonics
    sta code_ptr
    lda #>mnemonics
    sta code_ptr+1

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
!if .word_boundary {
    sec
} else {
    clc
}
    lda #1  ; mnemonics start at 1
    jsr find_in_token_list
!if .ec {
    bcs +
    brk
+   cpy #.eentry
    beq +
    brk
+   cpx #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

test_find_in_token_list_1: !pet "adc  ",0
test_find_in_token_list_2: !pet "adcq  ",0
test_find_in_token_list_3: !pet "tza  ",0
test_find_in_token_list_4: !pet "zzz  ",0
test_find_in_token_list_5: !pet "adcz  ",0
test_find_in_token_list_6: !pet "adc#  ",0
test_find_in_token_list_7: !pet "#adc#  ",0
test_find_in_token_list_8: !pet "adc+1  ",0
test_find_in_token_list_9: !pet "adc+2  ",0

!macro test_tokenize_mnemonic .tnum, .str, .pos, .ec, .etoken, .epos, .eflags {
    +test_start .tnum

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
    stx line_pos
    jsr tokenize_mnemonic
!if .ec {
    jsr assert_cs
    cpx #.etoken
    +assert_eq test_msg_wrong_result
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_result
} else {
    jsr assert_cc
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_err_pos
}

    +test_end
}


!macro test_expect_keyword .tnum, .tokbuf, .tokbufend, .lineaddr, .kw, .ec, .etokpos {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1
    ldx #0
    stx tok_pos
    ldx #<.kw
    ldy #>.kw
    jsr expect_keyword
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}
    +test_end
}

test_expect_keyword_1: !byte 0, $ff
test_expect_keyword_2: !byte tk_label_or_reg, 2, 3, 0, $ff
test_expect_keyword_end:
test_expect_keyword_line_1: !pet "5 xor 7",0
test_expect_keyword_line_2: !pet "5 XoR 7",0
test_expect_keyword_line_3: !pet "5 ror 7",0

!macro test_expect_oppop .tnum, .isop, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
!if .isop {
    jsr expect_opcode
} else {
    jsr expect_pseudoop
}
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   cmp #.ea
    beq +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
!if .isop {
    cpy #.ey
    beq +
    brk
+
}
}

    +test_end
}
!macro test_expect_opcode .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey {
    +test_expect_oppop .tnum, 1, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey
}
!macro test_expect_pseudoop .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea {
    +test_expect_oppop .tnum, 0, .tokbuf, .tokbufend, .ec, .etokpos, .ea, 0
}

test_expect_oppop_1: !byte 0, $ff
test_expect_oppop_2: !byte 1, 4, 0, 0, $ff
test_expect_oppop_3: !byte po_to, 4, 0, $ff
test_expect_oppop_4: !byte 1, 4, F_ASM_FORCE16, 0, $ff
test_expect_oppop_end:

!macro test_expect_literal .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .eresult, .eflags {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    lda #0
    sta expr_flags
    jsr expect_literal

!if .ec {
    jsr assert_cs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    jsr assert_cc
    lda expr_result
    cmp #<.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+1
    cmp #>.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+2
    cmp #^.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+3
    cmp #<(.eresult >>> 24)
    +assert_eq test_msg_wrong_result
    lda expr_flags
    cmp #.eflags
    +assert_eq test_msg_wrong_flags
}

    +test_end
}

test_expect_literal_1: !byte 0, $ff
test_expect_literal_2: !byte tk_number_literal, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_3: !byte tk_number_literal_leading_zero, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_end:


!macro test_expect_pms .tnum, .tokbuf, .tokbufend, .ec, .etok, .elen {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_pluses_or_minuses

!if .ec {
    jsr assert_cs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    jsr assert_cc
    cmp #.etok
    +assert_eq test_msg_wrong_result
    cpy #.elen
    +assert_eq test_msg_wrong_value
}

    +test_end
}

!macro test_expect_p_or_m .tnum, .tokbuf, .tokbufend, .ec, .etok {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_single_plus_or_minus

!if .ec {
    jsr assert_cs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    jsr assert_cc
    cmp #.etok
    +assert_eq test_msg_wrong_result
}

    +test_end
}

!macro test_expect_m .tnum, .tokbuf, .tokbufend, .ec {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_single_minus

!if .ec {
    jsr assert_cs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    jsr assert_cc
}

    +test_end
}

test_expect_pms_1: !byte 0, $ff
test_expect_pms_2: !byte tk_pluses, 0, 1, 0, $ff
test_expect_pms_3: !byte tk_minuses, 0, 1, 0, $ff
test_expect_pms_4: !byte tk_pluses, 0, 3, 0, $ff
test_expect_pms_5: !byte tk_minuses, 0, 3, 0, $ff
test_expect_pms_end


test_symbol_table:
    ; label
    !byte <attic_symbol_names, >attic_symbol_names, ^attic_symbol_names
    !byte F_SYMTBL_DEFINED
    !32 12345
    ; Alpha
    !byte <(attic_symbol_names+6), >(attic_symbol_names+6), ^(attic_symbol_names+6)
    !byte F_SYMTBL_DEFINED
    !32 23456
    ; Alph
    !byte <(attic_symbol_names+12), >(attic_symbol_names+12), ^(attic_symbol_names+12)
    !byte F_SYMTBL_DEFINED
    !32 34567
    ; Beta
    !byte <(attic_symbol_names+17), >(attic_symbol_names+17), ^(attic_symbol_names+17)
    !byte F_SYMTBL_DEFINED
    !32 45678
    ; BetaZ
    !byte <(attic_symbol_names+22), >(attic_symbol_names+22), ^(attic_symbol_names+22)
    !byte F_SYMTBL_DEFINED
    !32 56789
    ; GAMMA
    !byte <(attic_symbol_names+28), >(attic_symbol_names+28), ^(attic_symbol_names+28)
    !byte F_SYMTBL_DEFINED
    !32 99999
    ; (END)
    !byte 0,0,0,0,0,0,0,0
test_symbol_table_end:
test_symbol_table_last_addr = test_symbol_table_end-test_symbol_table+attic_symbol_table-SYMTBL_ENTRY_SIZE
test_symbol_names:
    !pet "label",0
    !pet "Alpha",0
    !pet "Alph",0
    !pet "Beta",0
    !pet "BetaZ",0
    !pet "GAMMA",0
test_symbol_names_end:

test_set_up_symbol_data:
    jsr init_symbol_table

    lda #<test_symbol_table
    sta code_ptr
    lda #>test_symbol_table
    sta code_ptr+1
    lda #<attic_symbol_table
    ldx #>attic_symbol_table
    ldy #^attic_symbol_table
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names
    bcc -
    lda code_ptr
    cmp #<test_symbol_names
    bcc -

    lda #<test_symbol_names
    sta code_ptr
    lda #>test_symbol_names
    sta code_ptr+1
    lda #<attic_symbol_names
    ldx #>attic_symbol_names
    ldy #^attic_symbol_names
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names_end
    bcc -
    lda code_ptr
    cmp #<test_symbol_names_end
    bcc -
    lda attic_ptr
    sta symtbl_next_name
    lda attic_ptr+1
    sta symtbl_next_name+1
    lda attic_ptr+2
    sta symtbl_next_name+2
    rts

!macro test_find_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    ldx #.length
    jsr find_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    brk
+
    lda #<test_symbol_table_last_addr
    ldx #>test_symbol_table_last_addr
    ldy #^test_symbol_table_last_addr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
}

    +test_end
}

test_find_symbol_1: !pet "label = 999",0
test_find_symbol_2: !pet "Alpha",0
test_find_symbol_3: !pet "Alph",0
test_find_symbol_4: !pet "Beta",0
test_find_symbol_5: !pet "BetaZ",0
test_find_symbol_6: !pet "GAMMA",0
test_find_symbol_7: !pet "GAMMB",0

!macro test_find_or_add_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    brk
+
    ; TODO: confirm out of memory conditions
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
    ; TODO: test symtbl_next_name has advanced
}

    +test_end
}

!macro test_get_symbol_value .tnum, .str, .length, .ec, .eq {
    +test_start .tnum

    jsr test_set_up_symbol_data

    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    jsr get_symbol_value

!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+   cmp #<.eq
    beq +
    brk
+   cpx #>.eq
    beq +
    brk
+   cpy #^.eq
    beq +
    brk
+   cpz #<(.eq >>> 24)
    beq +
    brk
+
}

    +test_end
}

!macro test_set_symbol_value .tnum, .str, .length, .q {
    +test_start .tnum

    jsr test_set_up_symbol_data

    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #0
    sta bas_ptr+2
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    lda #<.q
    ldx #>.q
    ldy #^.q
    ldz #<(.q >>> 24)
    jsr set_symbol_value

    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    bne +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #^.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<(.q >>> 24)
    beq +
    brk
+
    +test_end
}

!macro test_do_assemble_bytes .x {
!if .x > 0 {
    ldx #.x-1
-   txa
    sta strbuf,x
    dex
    bpl -
}
    ldx #.x
    jsr assemble_bytes
}

!macro test_assemble_bytes .tnum, .pass, .pc, .x, .ec, .epc {
    +test_start .tnum
    jsr init_segment_table
    lda #.pass
    sta pass
    jsr init_pass
!if .pc {
    ldx #<.pc
    ldy #>.pc
    jsr set_pc
}
    +test_do_assemble_bytes .x

!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

!if .epc {
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
}

!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr
    ldz #0
    lda [attic_ptr],z
    cmp #<.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.x
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.x
    beq +
    brk
+   inz

    ldy #.x
    ldx #0
-   txa
    cmp [attic_ptr],z
    beq +
    brk
+
    inz
    inx
    dey
    bne -
}

    +test_end
}

!macro test_assemble_bytes_twice .tnum, .pass, .pc1, .pc2, .x, .epc, .edata, .edataend {
    +test_start .tnum
    jsr init_segment_table
    lda #.pass
    sta pass
    jsr init_pass
    ldx #<.pc1
    ldy #>.pc1
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    ldx #<.pc2
    ldy #>.pc2
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr

    ldx #.edataend-.edata-1
-   txa
    taz
    lda .edata,x
    cmp [attic_ptr],z
    beq +
    brk
+
    dex
    bpl -
}
    +test_end
}

test_assemble_bytes_twice_1:
    !word $c000
    !word 10
    !byte 0, 1, 2, 3, 4, 0, 1, 2, 3, 4
test_assemble_bytes_twice_2:
    !word $c000
    !word 5
    !byte 0, 1, 2, 3, 4
    !word $d000
    !word 5
    !byte 0, 1, 2, 3, 4
test_assemble_bytes_twice_2_end:


!macro test_expect_token .tnum, .a, .tokbuf, .tokbufend, .ec, .etokpos {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #.a
    ldx #0
    stx tok_pos
    jsr expect_token
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_token_1: !byte 0, $ff
test_expect_token_2: !byte 1, 4, 0, $ff
test_expect_token_end:

!macro test_expect_label .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ex, .ey {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_label
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   cpx #.ex
    beq +
    brk
+   cpy #.ey
    beq +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_label_1: !byte 0, $ff
test_expect_label_2: !byte tk_label_or_reg, 99, 5, 0, $ff
test_expect_label_end:


; Input: X=segment count
; Output: Initialized segment table with X segments,
;   with starting addresses of $0i00 and legnth of 3,
;   created in descending order i=X to 1,
;   e.g. $0300, $0200, $0100. If X=0, no segments
;   are created.
set_up_segtable_for_test:
    phx
    jsr init_segment_table

    ; Three bytes in strbuf
    lda #1
    ldx #0
    sta strbuf,x
    inc
    inx
    sta strbuf,x
    inc
    inx
    sta strbuf,x

    ply
-   cpy #0
    beq +
    sty program_counter+1
    lda #0
    sta program_counter
    lda asm_flags
    ora #F_ASM_PC_DEFINED
    and #<(!F_ASM_SRC_TO_BUF)
    sta asm_flags
    phy
    ldx #3
    jsr assemble_bytes
    ply
    dey
    bra -

+   rts

; Call this immediately after set_up_segtable_for_test
; to create a segment that overlaps an existing one
set_up_overlapping_segment_for_test:
    lda #$03
    sta program_counter+1
    lda #$01
    sta program_counter
    lda asm_flags
    ora #F_ASM_PC_DEFINED
    and #<(!F_ASM_SRC_TO_BUF)
    sta asm_flags
    ldx #3
    jsr assemble_bytes
    rts


!macro create_test_segment_tables .count {
    ldx #.count
    jsr set_up_segtable_for_test
}

!macro test_segment_traversal .tnum, .count {
    +test_start .tnum

    +create_test_segment_tables .count
    lda #0
    sta bas_ptr  ; repurposes bas_ptr.0 as a segment counter
    jsr start_segment_traversal
-   jsr is_end_segment_traversal
    bcs +
    inc bas_ptr
    jsr next_segment_traversal
    bra -
+   lda bas_ptr
    cmp #.count
    +assert_eq test_msg_wrong_result

    +test_end
}

!macro test_segment_overlap .tnum, .start, .length, .c, .set_expr_result, .ec {
    +test_start .tnum

    +create_test_segment_tables 5

    !if .set_expr_result {
        jsr start_segment_traversal
        jsr next_segment_traversal
        jsr next_segment_traversal
        ldq current_segment
        stq expr_result
    } else {
        lda #0
        sta expr_result
        sta expr_result+1
        sta expr_result+2
        sta expr_result+3
    }

    lda #<.start
    ldx #>.start
    ldy #<.length
    ldz #>.length
    !if .c { sec } else { clc }
    jsr does_a_segment_overlap
    !if .ec {
        jsr assert_cs
    } else {
        jsr assert_cc
    }

    +test_end
}

!macro test_segment_overlap_any .tnum, .yesoverlap {
    +test_start .tnum
    +create_test_segment_tables 5
    !if .yesoverlap {
        jsr set_up_overlapping_segment_for_test
    }
    jsr do_any_segments_overlap
    !if .yesoverlap {
        jsr assert_cs
    } else {
        jsr assert_cc
    }
    +test_end
}


run_test_suite_cmd:

    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "is-letter"
    +test_is_letter $01, 'A', 1
    +test_is_letter $02, 'B', 1
    +test_is_letter $03, 'Z', 1
    +test_is_letter $04, 'a', 1
    +test_is_letter $05, 'b', 1
    +test_is_letter $06, 'z', 1
    +test_is_letter $07, 193, 1
    +test_is_letter $08, 194, 1
    +test_is_letter $09, 218, 1
    +test_is_letter $0A, '@', 0
    +test_is_letter $0B, '0', 0
    +test_is_letter $0C, '9', 0
    +test_is_letter $0D, ']', 0
    +test_is_letter $0E, chr_backarrow, 0
    +test_is_letter $0F, chr_megaat, 0
    +test_is_letter $10, $e1, 0

    +print_chr chr_cr
    +print_strlit_line "is-secondary-ident-char"
    +test_is_secondary_ident_char $01, 'A', 1
    +test_is_secondary_ident_char $02, 'B', 1
    +test_is_secondary_ident_char $03, 'Z', 1
    +test_is_secondary_ident_char $04, 'a', 1
    +test_is_secondary_ident_char $05, 'b', 1
    +test_is_secondary_ident_char $06, 'z', 1
    +test_is_secondary_ident_char $07, '0', 1
    +test_is_secondary_ident_char $08, '9', 1
    +test_is_secondary_ident_char $09, 193, 1
    +test_is_secondary_ident_char $0A, 194, 1
    +test_is_secondary_ident_char $0B, 218, 1
    +test_is_secondary_ident_char $0C, chr_backarrow, 1
    +test_is_secondary_ident_char $0D, chr_megaat, 1
    +test_is_secondary_ident_char $0E, '@', 0
    +test_is_secondary_ident_char $0F, ']', 0
    +test_is_secondary_ident_char $10, $e1, 0
    +test_is_secondary_ident_char $11, '.', 1

    +print_chr chr_cr
    +print_strlit_line "strbuf-to-lowercase"
    +test_strbuf_to_lowercase $01, test_strbuf_to_lowercase_1_out, test_strbuf_to_lowercase_1_out
    +test_strbuf_to_lowercase $02, test_strbuf_to_lowercase_2_in, test_strbuf_to_lowercase_1_out
    +test_strbuf_to_lowercase $03, test_strbuf_to_lowercase_3_in, test_strbuf_to_lowercase_1_out

    +print_chr chr_cr
    +print_strlit_line "strbuf-cmp-code-ptr"
    +test_strbuf_cmp_code_ptr $01, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abc, 3, $00, 3
    +test_strbuf_cmp_code_ptr $02, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abc, 6, $00, 3
    +test_strbuf_cmp_code_ptr $03, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_acb, 3, $ff, 1
    +test_strbuf_cmp_code_ptr $04, test_strbuf_cmp_code_ptr_acb, test_strbuf_cmp_code_ptr_abc, 3, $01, 1
    +test_strbuf_cmp_code_ptr $05, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abcd, 4, $ff, 3
    +test_strbuf_cmp_code_ptr $06, test_strbuf_cmp_code_ptr_abcd, test_strbuf_cmp_code_ptr_abc, 4, $01, 3

    +print_chr chr_cr
    +print_strlit_line "accept-whitespace-and-comment"
    +test_accept_whitespace_and_comment $01, test_accept_whitespace_and_comment_empty_line, 4, 4, 1
    +test_accept_whitespace_and_comment $02, test_accept_whitespace_and_comment_comment, 4, 4+test_accept_whitespace_and_comment_comment_length, 1
    +test_accept_whitespace_and_comment $03, test_accept_whitespace_and_comment_space_then_stuff, 4, 7, 0
    +test_accept_whitespace_and_comment $04, test_accept_whitespace_and_comment_other_spaces, 4, 10, 0

    +print_chr chr_cr
    +print_strlit_line "accept-ident"
    +test_accept_ident $01, test_accept_ident_1, 0, 1, 5
    +test_accept_ident $02, test_accept_ident_2, 0, 1, 10
    +test_accept_ident $03, test_accept_ident_3, 0, 0, 0
    +test_accept_ident $04, test_accept_ident_4, 0, 0, 0
    +test_accept_ident $05, test_accept_ident_4, 1, 1, 6

    +print_chr chr_cr
    +print_strlit_line "accept-literal"
    +test_accept_literal $01, test_accept_literal_1, 1, 88, 3, 0
    +test_accept_literal $02, test_accept_literal_2, 1, '\'', 3, 0
    +test_accept_literal $03, test_accept_literal_3, 1, $0, 2, 1
    +test_accept_literal $04, test_accept_literal_4, 1, $1, 2, 0
    +test_accept_literal $05, test_accept_literal_5, 1, $f, 2, 0
    +test_accept_literal $06, test_accept_literal_6, 1, $F, 2, 0
    +test_accept_literal $07, test_accept_literal_7, 1, $deadbeef, 9, 0
    +test_accept_literal $08, test_accept_literal_8, 1, $DEadbeEF, 9, 0
    +test_accept_literal $09, test_accept_literal_9, 1, $000a, 5, 1
    +test_accept_literal $0a, test_accept_literal_10, 1, $1000, 5, 0
    +test_accept_literal $0b, test_accept_literal_11, 1, %0000, 2, 0
    +test_accept_literal $0c, test_accept_literal_12, 1, %0001, 2, 0
    +test_accept_literal $0d, test_accept_literal_13, 1, %0101, 5, 0
    +test_accept_literal $0e, test_accept_literal_14, 1, %1010, 5, 0
    +test_accept_literal $0f, test_accept_literal_15, 1, %.#.#, 5, 0
    +test_accept_literal $10, test_accept_literal_16, 1, %#.#., 5, 0
    +test_accept_literal $11, test_accept_literal_17, 1, %....##..####....##..####....####, 33, 0
    +test_accept_literal $12, test_accept_literal_18, 1, 0, 1, 1
    +test_accept_literal $13, test_accept_literal_19, 1, 9, 1, 0
    +test_accept_literal $14, test_accept_literal_20, 1, 1234567890, 10, 0
    +test_accept_literal $15, test_accept_literal_21, 1, 0123, 4, 1
    +test_accept_literal $16, test_accept_literal_22, 0, 0, 0, 0
    +test_accept_literal $17, test_accept_literal_23, 0, 0, 0, 0
    +test_accept_literal $18, test_accept_literal_24, 0, 0, 0, 0
    +test_accept_literal $19, test_accept_literal_25, 1, $F, 2, 0
    +test_accept_literal $1a, test_accept_literal_26, 1, %0001, 2, 0
    +test_accept_literal $1b, test_accept_literal_27, 1, 56, 2, 0
    +test_accept_literal $1c, test_accept_literal_28, 0, 0, 0, 0

    +print_chr chr_cr
    +print_strlit_line "find-in-token-list"
    +test_find_in_token_list $01, test_find_in_token_list_1, 0, 1, 1, mnemonic_adc, 3
    +test_find_in_token_list $02, test_find_in_token_list_2, 0, 1, 1, mnemonic_adcq, 4
    +test_find_in_token_list $03, test_find_in_token_list_3, 0, 1, 1, mnemonic_tza, 3
    +test_find_in_token_list $04, test_find_in_token_list_4, 0, 1, 0, mnemonic_adc, 0
    +test_find_in_token_list $05, test_find_in_token_list_5, 0, 0, 1, mnemonic_adc, 3
    +test_find_in_token_list $06, test_find_in_token_list_5, 0, 1, 0, mnemonic_adc, 0
    +test_find_in_token_list $07, test_find_in_token_list_6, 0, 0, 1, mnemonic_adc, 3
    +test_find_in_token_list $08, test_find_in_token_list_6, 0, 1, 1, mnemonic_adc, 3
    +test_find_in_token_list $09, test_find_in_token_list_7, 1, 1, 1, mnemonic_adc, 4

    +print_chr chr_cr
    +print_strlit_line "tokenize-mnemonic"
    ; .tnum, .str, .pos, .ec, .etoken, .epos, .eflags
    +test_tokenize_mnemonic $01, test_find_in_token_list_1, 0, 1, mnemonic_adc, 3, 0
    +test_tokenize_mnemonic $02, test_find_in_token_list_2, 0, 1, mnemonic_adcq, 4, 0
    +test_tokenize_mnemonic $03, test_find_in_token_list_3, 0, 1, mnemonic_tza, 3, 0
    +test_tokenize_mnemonic $04, test_find_in_token_list_4, 0, 0, 0, 0, 0
    +test_tokenize_mnemonic $05, test_find_in_token_list_5, 0, 0, 0, 0, 0
    +test_tokenize_mnemonic $06, test_find_in_token_list_6, 0, 1, mnemonic_adc, 3, 0
    +test_tokenize_mnemonic $07, test_find_in_token_list_7, 1, 1, mnemonic_adc, 4, 0
    +test_tokenize_mnemonic $08, test_find_in_token_list_8, 0, 1, mnemonic_adc, 5, F_ASM_FORCE8
    +test_tokenize_mnemonic $09, test_find_in_token_list_9, 0, 1, mnemonic_adc, 5, F_ASM_FORCE16

    +print_chr chr_cr
    +print_strlit_line "test-expect-keyword"
    +test_expect_keyword $01, test_expect_keyword_1, test_expect_keyword_2, test_expect_keyword_line_1, kw_xor, 1, 0
    +test_expect_keyword $02, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_1, kw_xor, 0, 3
    +test_expect_keyword $03, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_2, kw_xor, 0, 3
    +test_expect_keyword $04, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_3, kw_xor, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-opcode"
    ; .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea
    +test_expect_opcode $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0, 0
    +test_expect_opcode $02, test_expect_oppop_2, test_expect_oppop_3, 0, 3, 1, 0
    +test_expect_opcode $03, test_expect_oppop_3, test_expect_oppop_4, 1, 0, 0, 0
    +test_expect_opcode $04, test_expect_oppop_4, test_expect_oppop_end, 0, 3, 1, F_ASM_FORCE16

    +print_chr chr_cr
    +print_strlit_line "test-expect-pseudoop"
    +test_expect_pseudoop $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0
    +test_expect_pseudoop $02, test_expect_oppop_2, test_expect_oppop_3, 1, 0, 0
    +test_expect_pseudoop $03, test_expect_oppop_3, test_expect_oppop_end, 0, 2, po_to

    +print_chr chr_cr
    +print_strlit_line "test-expect-literal"
    +test_expect_literal $01, test_expect_literal_1, test_expect_literal_2, 1, 0, 0, 0
    +test_expect_literal $02, test_expect_literal_2, test_expect_literal_3, 0, 6, $aabbccdd, 0
    +test_expect_literal $03, test_expect_literal_3, test_expect_literal_end, 0, 6, $aabbccdd, F_EXPR_FORCE16

    +print_chr chr_cr
    +print_strlit_line "test-expect-pluses-or-minuses"
    +test_expect_pms $01, test_expect_pms_1, test_expect_pms_2, 1, 0, 0
    +test_expect_pms $02, test_expect_pms_2, test_expect_pms_3, 0, tk_pluses, 1
    +test_expect_pms $03, test_expect_pms_3, test_expect_pms_4, 0, tk_minuses, 1
    +test_expect_p_or_m $04, test_expect_pms_2, test_expect_pms_3, 0, tk_pluses
    +test_expect_p_or_m $05, test_expect_pms_3, test_expect_pms_4, 0, tk_minuses
    +test_expect_p_or_m $06, test_expect_pms_4, test_expect_pms_5, 1, 0
    +test_expect_p_or_m $07, test_expect_pms_5, test_expect_pms_end, 1, 0
    +test_expect_m $08, test_expect_pms_1, test_expect_pms_2, 1
    +test_expect_m $09, test_expect_pms_2, test_expect_pms_3, 1
    +test_expect_m $0A, test_expect_pms_3, test_expect_pms_4, 0
    +test_expect_m $0B, test_expect_pms_4, test_expect_pms_5, 1
    +test_expect_m $0C, test_expect_pms_5, test_expect_pms_end, 1

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-find-symbol"
    +test_find_symbol $01, test_find_symbol_1, 5, 0, attic_symbol_table+(8*0)
    +test_find_symbol $02, test_find_symbol_2, 5, 0, attic_symbol_table+(8*1)
    +test_find_symbol $03, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_symbol $04, test_find_symbol_4, 4, 0, attic_symbol_table+(8*3)
    +test_find_symbol $05, test_find_symbol_5, 5, 0, attic_symbol_table+(8*4)
    +test_find_symbol $06, test_find_symbol_6, 5, 0, attic_symbol_table+(8*5)
    +test_find_symbol $07, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-find-or-add-symbol"
    +test_find_or_add_symbol $01, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_or_add_symbol $02, test_find_symbol_7, 5, 0, attic_symbol_table+(8*6)

    +print_chr chr_cr
    +print_strlit_line "test-get-symbol-value"
    +test_get_symbol_value $01, test_find_symbol_2, 5, 0, 23456
    +test_get_symbol_value $02, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-set-symbol-value"
    +test_set_symbol_value $01, test_find_symbol_2, 5, 98765
    +test_set_symbol_value $02, test_find_symbol_7, 5, 87654

    +print_chr chr_cr
    +print_strlit_line "test-assemble-bytes"
    +test_assemble_bytes $01, 0, 0, 1, 1, 0  ; undefined PC is error
    +test_assemble_bytes $02, 0, $c000, 0, 0, 0  ; zero length is ok
    +test_assemble_bytes $03, 0, $c000, 5, 0, $c005
    +test_assemble_bytes $04, $ff, $c000, 5, 0, $c005
    +test_assemble_bytes_twice $05, 0, $c000, $c005, 5, $c00a, 0, 0
    +test_assemble_bytes_twice $06, 0, $c000, $d000, 5, $d005, 0, 0
    +test_assemble_bytes_twice $07, $ff, $c000, $c005, 5, $c00a, test_assemble_bytes_twice_1, test_assemble_bytes_twice_2
    +test_assemble_bytes_twice $08, $ff, $c000, $d000, 5, $d005, test_assemble_bytes_twice_2, test_assemble_bytes_twice_2_end

    +print_chr chr_cr
    +print_strlit_line "test-expect-token"
    +test_expect_token $01, 1, test_expect_token_1, test_expect_token_2, 1, 0
    +test_expect_token $02, 1, test_expect_token_2, test_expect_token_end, 0, 2
    +test_expect_token $03, 4, test_expect_token_2, test_expect_token_end, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-label"
    +test_expect_label $01, test_expect_label_1, test_expect_label_2, 1, 0, 0, 0
    +test_expect_label $02, test_expect_label_2, test_expect_label_end, 0, 3, 99, 5

    +print_chr chr_cr
    +print_strlit_line "test-segment-traversal"
    +test_segment_traversal $01, 0
    +test_segment_traversal $02, 1
    +test_segment_traversal $03, 5

    +print_chr chr_cr
    +print_strlit_line "test-segment-overlap"

    ; $0100 cmp $0080 -> Z=0 C=1
    ; lda #$00 : sta expr_a : lda #$01 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2
    ; $007F cmp $0080 -> Z=0 C=0
    ; lda #$7F : sta expr_a : lda #$00 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2
    ; $0080 cmp $0080 -> Z=1 C=1
    ; lda #$80 : sta expr_a : lda #$00 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2

    ; Five test segments: 0100-0102, ..., 0500-0502
    ; .start, .length, .c, .set_expr_result, .ec
    +test_segment_overlap $01, $0204, $00ef, 0, 0, 0
    +test_segment_overlap $02, $0202, $00ef, 0, 0, 1
    +test_segment_overlap $03, $0204, $00fd, 0, 0, 1
    +test_segment_overlap $04, $0302, $00ef, 0, 1, 1
    +test_segment_overlap $05, $0302, $00ef, 1, 1, 0
    +test_segment_overlap $06, $0302, $00ef, 1, 0, 1
    +test_segment_overlap $07, $01ff, $00ef, 0, 0, 1
    +test_segment_overlap_any $08, 0
    +test_segment_overlap_any $09, 1


    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
