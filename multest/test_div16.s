; div16
; 16/16=16+16 bit unsigned divide

.p816

.export test_name : far
.export test_run
.export test_ref

.importzp test_count
.importzp test_in0
.importzp test_in1
.importzp test_out

.segment "HEADNAME"
.byte "MULTEST div16        "

.segment "MAIN"
test_name: .asciiz "Multest div16"

.segment "LOPRG"

; unsigned 16-bit divide, 2 x 16-bit result
; Based on routine by psychopathicteen: https://wiki.superfamicom.org/16-bit-multiplication-and-division/
; test_in0 / test_in1 = test_out+2
; test_in0 % test_in1 = test_out+0
test_run: ; clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	lda z:test_in0
	sta z:test_out+0 ; temp3
	stz z:test_out+2 ; temp4
	lda z:test_in1
	cmp #$0100
	bcs @full
	; divisor is 8-bit, just use hardware divide directly
	tax
	lda z:test_in0
	sta $4204
	stx $4206
	nop
	nop
	nop
	nop
	nop
	nop
	lda f:$4214 ; f for +1 wait cycle
	sta z:test_out+2
	lda $4216
	sta z:test_out+0
	rts
@full:
	; right shift temp numerator (test_out+0) and divisor (A) until an 8-bit divisor is achieved
	; A = test_in1
	:
		lsr
		adc #0 ; divisor rounds up
		lsr z:test_out+0 ; numerator rounds down
		cmp #$0100
		bcs :-
	; do approximate division in hardware (guaranteed <= the desired quotient by rounding)
	tax
	lda z:test_out+0
	sta $4204
	stx $4206
	nop
	nop
	nop
	nop
	nop
	nop
	lda f:$4214 ; f for +1 wait cycle
	sta z:test_out+2 ; approximate quotient (<= real quotient), guaranteed < 256
	; hardware multiply ~quotient * divisor to calculate the result of our approximation
	tax
	stx $4202
	ldx z:test_in1+0
	stx $4203
	ldx z:test_in1+1
	nop
	lda $4216 ; ~quotient * divisor-low
	stx $4203
	sta z:test_out+0 ; low byte of result
	xba
	clc
	adc $4216 ; (+ ~quotient * divisor-low) << 8
	tax
	stx z:test_out+1 ; high byte of result
	lda z:test_in0
	sec
	sbc z:test_out+0
	; A = remainder: numerator - (~quotient * divisor)
	; test_out+2 = ~quotient
	:
		cmp z:test_in1
		bcc :+ ; if remainder > divisor: increment quotient, subtract divisor from remainder (repeat as needed)
		sbc z:test_in1
		inc z:test_out+2
		bra :-
	:
	; test_out+2 is now real quotient
	sta z:test_out+0 ; A is now real remainder
	rts

; reference: shift+sub divide
test_ref:
	.a16
	.i8
	lda z:test_in0
	asl
	sta z:test_out+2
	lda #0
	ldx #16
@loop:
	rol
	cmp z:test_in1
	bcc :+
		sbc z:test_in1
	:
	rol z:test_out+2
	dex
	bne @loop
	sta z:test_out+0 ; remainder
	rts
