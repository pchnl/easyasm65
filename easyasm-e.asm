; EasyAsm, an assembler for the MEGA65
; Copyright © 2024  Dan Sanderson
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;
; ---------------------------------------------------------
; easyasm-e : Dispatch routines
; ---------------------------------------------------------

!cpu m65

; EasyAsm uses page $1E00-$1EFF for dispatch code and variable space.
; Set EXPECTED_END_OF_DISPATCH according to BP variables declared in
; easyasm.asm. (On error, it may be able to raise this limit, just
; check easyasm.asm first.)
; EXPECTED_END_OF_DISPATCH = $1e94
EXPECTED_END_OF_DISPATCH = $1eff

kernal_base_page = $00
easyasm_base_page = $1e

; Attic map
; (Make sure this is consistent with easyasm.asm.)
attic_start         = $08700000
attic_easyasm_stash = attic_start + $2000        ; 0.2000-0.D6FF
; (Gap: 0.D700-1.1FFF = $4900 = 18.25 KB)
attic_source_stash  = attic_start + $12000         ; 1.2000-1.D6FF
basic_memory_size   = $d700 - $2000

dmajobs        = $58000
prog_mem_dirty = $1eff


* = $1e00

    ; $1E00: Launch menu.
    lda #$00
    ldx #$00

    ; $1E04: Run menu option A, argument X.
    pha

    ; Only stash source if source memory is "clean."
    lda prog_mem_dirty
    bne +
    jsr stash_src
+   lda #1
    sta prog_mem_dirty

    jsr sys_to_easyasm
    pla
    jsr $2000     ; EasyAsm dispatch
    lda #1
    jsr from_attic_to_src  ; Restore source.
    lda #0
    sta prog_mem_dirty
    lda #kernal_base_page
    tab
    rts

execute_user_program:
    ; $1E27: Execute user's program.
    ; A/X = address
    ; Segments built and segment DMA list installed
    pha
    lda #kernal_base_page
    tab
    pla
    sta $fe
    stx $ff
    jsr do_segment_dma
    jsr ($fe)
    jsr sys_to_easyasm
    rts

do_segment_dma:
    lda #^dmajobs
    sta $d702
    lda #dmajobs >> 24
    sta $d704
    lda #>dmajobs
    sta $d701
    lda #<dmajobs
    sta $d705
    rts

sys_to_easyasm:
    lda #0
    jsr from_attic_to_src
    lda #easyasm_base_page
    tab
    rts

from_attic_to_src:
    ; Input: A: $00=EasyAsm $01=Stashed source
    sta src_dma+11
    lda #0
    sta src_dma+3
    sta src_dma+14
    lda #$87
    sta src_dma+1

do_src_dma:
    lda #0
    sta $d704
    lda #^src_dma
    sta $d702
    lda #>src_dma
    sta $d701
    lda #<src_dma
    sta $d705
    rts

stash_src:
    lda #1
    sta src_dma+14
    lda #0
    sta src_dma+1
    sta src_dma+11
    lda #$87
    sta src_dma+3
    bra do_src_dma

src_dma:
    !byte $80, $87       ; src mb  = $(0)87
    !byte $81, $00       ; dest mb = $(0)00
    !byte $0b, $00
    !byte $00            ; copy
    !byte <basic_memory_size, >basic_memory_size  ; size
    !byte $00, $20, $00  ; src  $(087)02000
    !byte $00, $20, $00  ; dest $(000)02000
    !byte $00, $00, $00


!if * > EXPECTED_END_OF_DISPATCH {
    !error "EasyAsm dispatch code ends at address ", *
}
