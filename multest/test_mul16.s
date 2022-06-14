; mul16
; 16x16=32 bit unsigned multiply

.p816

.export test_name : far
.export test_run
.export test_ref

.importzp test_count
.importzp test_in0
.importzp test_in1
.importzp test_out

.segment "HEADNAME"
.byte "MULTEST mul16        "

.segment "MAIN"
test_name: .asciiz "Multest mul16"

.segment "LOPRG"

; unsigned 16-bit multiply, 32-bit result
; Written by 93143: https://forums.nesdev.org/viewtopic.php?p=280089#p280089
test_run: ; mul16a x mul16b = mul16ab, clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	ldx z:test_in0+0
	stx $4202
	ldy z:test_in1+0
	sty $4203         ; a0 x b0 (A)
	ldx z:test_in1+1
	stz z:test_out+2
	lda $4216
	stx $4203         ; a0 x b1 (B)
	sta z:test_out+0   ; 00AA
	ldx z:test_in0+1
	lda $4216
	stx $4202
	sty $4203         ; a1 x b0 (C)
	clc
	adc z:test_out+1   ; 00AA + 0BB0 (can't set carry because high byte was 0)
	ldy z:test_in1+1
	adc $4216
	sty $4203         ; a1 x b1 (D)
	sta z:test_out+1   ; 00AA + 0BB0 + 0CC0
	lda z:test_out+2
	bcc :+
	adc #$00FF        ; if carry, increment top byte
:
	adc $4216
	sta z:test_out+2   ; 00AA + 0BB0 + 0CC0 + DD00
	rts

; reference: shift+add multiply
test_ref:
	.a16
	.i8
	lda z:test_in0
	lsr
	sta z:test_out+0
	lda #0
	ldx #16
@loop:
	bcc :+
		clc
		adc z:test_in1
	:
	ror
	ror z:test_out+0
	dex
	bne @loop
	sta z:test_out+2
	rts
