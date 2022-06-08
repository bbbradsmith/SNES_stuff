; multest multiply routine test
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

VRAM_NMT = $0000
VRAM_CHR = $2000

;
; Header
;

.segment "HEADER"
.byte "MULTEST              "
.byte $31 ; map mode (HiROM, FastROM)
.byte $00 ; cartridge type (ROM only)
.byte $08 ; 2mbit (256k)
.byte $00 ; RAM size
.byte $01 ; destination code (north america)
.byte $00 ; dev ID (not $33 to prevent indication of v3 header)
.byte $00 ; mask ROM version
.word $0000 ; checksum
.word $FFFF ; checksum complement
; native vectors
.assert (.loword(*)=$FFE0), error, "native vectors misplaced"
.word $FFFF
.word $FFFF
.word .loword(vector_irq) ; COP
.word .loword(vector_irq) ; BRK
.word .loword(vector_irq) ; ABORT
.word .loword(vector_nmi)
.word .loword(vector_reset)
.word .loword(vector_irq)
; emulation vectors
.assert (.loword(*)=$FFF0), error, "emulation vectors misplaced"
.word $FFFF
.word $FFFF
.word .loword(vector_irq) ; COP
.word .loword(vector_irq) ; BRK
.word .loword(vector_irq) ; ABORT
.word .loword(vector_nmi)
.word .loword(vector_reset)
.word .loword(vector_irq)

;
; RAM
;

.segment "ZEROPAGE" ; < $100
gamepad:      .res 2
lastpad:      .res 2
newpad:       .res 2
nmi_ready:    .res 1
nmi_count:    .res 1
ptr:           .res 3

mul16a:       .res 2 ; 16-bit multiply terms and 32-bit product
mul16b:       .res 2
mul16ab:      .res 4

testa:        .res 2
testb:        .res 2
testc:        .res 2
temp:         .res 4

.segment "LORAM" ; <$2000
.segment "HIRAM" ; $FE bank < $8000

new_hdma: .res 16 * 8 ; HDMA channel settings to apply at next update
nmi_hdma: .res 16 * 8 ; HDMA channel settings current

.segment "RODATA" ; for data needed by code

.import __RODATA_LOAD__
.import __RODATA_SIZE__
.assert (^__RODATA_LOAD__) = (^(__RODATA_LOAD__ + __RODATA_SIZE__)), error, "RODATA cannot cross a bank"

BANK_RODATA = ^__RODATA_LOAD__

;
; START stub for bank $00 dispatch
;

.segment "START"

vector_irq:
	; not used
	rti

vector_nmi:
	jml nmi

vector_reset:
	sei
	clc
	xce ; disable 6502 emulation
	stz $4200 ; NMI off
	jml reset

;
; LOPRG stuff for the first bank (mapped at $C00000)
;

.segment "LOPRG"

nmi:
	; save registers
	rep #$30
	.a16
	.i16
	pha
	phx
	phy
	phb
	phd
	sep #$30
	.a8
	.i8
	lda #$00
	pha
	plb ; set data bank $00 for hardware access
	lda z:nmi_ready ; no pending update
	bne :+
		jmp @exit
	:
	cmp #2 ; 2 = begin blanking
	bne :+
		lda #$8F
		sta $2100 ; force blanking on
		stz z:nmi_ready
		jmp @exit
	:
	; force blanking off
	lda #$0F
	sta $2100
	stz z:nmi_ready
	; next field
@exit:
	; count frames
	inc z:nmi_count
	; restore registers
	rep #$30
	.a16
	.i16
	pld
	plb
	ply
	plx
	pla
	rti
	.a8
	.i8

reset:
	rep #$30
	.a16
	.i16
	ldx #$01FF
	txs ; setup stack
	jml :+ ; setup PC
:
	lda #$0000
	tcd ; set DP to zero page
	; "standard" init
	sep #$30
	.a8
	.i8
	lda #$8f
	sta $2100
	stz $2101
	stz $2102
	stz $2103
	stz $2105
	stz $2106
	stz $2107
	stz $2108
	stz $2109
	stz $210A
	stz $210B
	stz $210C
	stz $210D
	stz $210D
	stz $210E
	stz $210E
	stz $210F
	stz $210F
	stz $2110
	stz $2110
	stz $2111
	stz $2111
	stz $2112
	stz $2112
	stz $2113
	stz $2113
	stz $2114
	stz $2114
	lda #$80
	sta $2115
	stz $2116
	stz $2117
	stz $211A
	stz $211B
	lda #$01
	sta $211B
	stz $211C
	stz $211C
	stz $211D
	stz $211D
	stz $211E
	lda #$01
	sta $211E
	stz $211F
	stz $211F
	stz $2120
	stz $2120
	stz $2121
	stz $2123
	stz $2124
	stz $2125
	stz $2126
	stz $2127
	stz $2128
	stz $2129
	stz $212A
	stz $212B
	stz $212C
	stz $212D
	stz $212E
	stz $212F
	lda #$30
	sta $2130
	stz $2131
	lda #$E0
	sta $2132
	stz $2133
	stz $4200
	lda #$FF
	sta $4201
	stz $4202
	stz $4203
	stz $4204
	stz $4205
	stz $4206
	stz $4207
	stz $4208
	stz $4209
	stz $420A
	stz $420B
	stz $420C
	;stz $420D ; SlowROM
	lda #1
	sta $420D ; FastROM
	; clear RAM
	lda #0
	sta f:$7E0000 ; 0 byte to pattern-fill with MVN
	sta f:$7F0000
	rep #$30
	.a16
	.i16
	phb
	ldx #0
	ldy #1
	lda #$10000-2
	mvn #$7E,#$7E
	ldx #0
	ldy #1
	lda #$10000-2
	mvn #$7F,#$7F
	plb
	sep #$30
	.a8
	.i8
	; clear vram
	stz $2116 ; VMADD $0000
	stz $2117
	lda #%00001001 ; no increment, 2-register
	sta $4300
	lda #$18 ; $2118 VMDATA
	sta $4301
	lda #<byte00
	sta $4302
	lda #>byte00
	sta $4303
	lda #^byte00
	sta $4304
	stz $4305
	stz $4306 ; 64k bytes
	lda #$01
	sta $420B ; DMA
	; copy tiles
	lda #<VRAM_CHR
	sta $2116
	lda #>VRAM_CHR
	sta $2117
	lda #%00000001 ; 2-register
	sta $4300
	lda #<chr_test
	sta $4302
	lda #>chr_test
	sta $4303
	lda #^chr_test
	sta $4304
	lda #<CHR_TEST_SIZE
	sta $4305
	lda #>CHR_TEST_SIZE
	sta $4306
	lda #$01
	sta $420B ; DMA
	; load palettes
	stz $2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta $4300
	lda #$22 ; $2122 CGDATA
	sta $4301
	ldx #16
	:
		lda #<palette
		sta $4302
		lda #>palette
		sta $4303
		lda #^palette
		sta $4304
		lda #32
		sta $4305
		stz $4306 ; 32 bytes
		lda #$01
		sta $420B ; DMA
		dex
		bne :-
	; setup PPU addresses
	lda #((>VRAM_NMT) & $FC)
	sta $2107 ; BG1SC nametable, 1-screen
	lda #(VRAM_CHR >> 12)
	sta $210B ; BG12NBA
	lda #$01
	sta $212C ; TM BG1 main-screen
	; increment on $2118 for easy NMT updates
	stz $2115
	; begin
	jmp run

;
; common stuff
;

.a16
.i8

render_on:
	ldx #1
	stx z:nmi_ready
render_wait:
	wai
	:
		ldx z:nmi_ready
		bne :-
	rts

render_off:
	ldx #2
	stx z:nmi_ready
	bra render_wait

print_hex8: ; A = hex value, clobber X
	pha
	lsr
	lsr
	lsr
	lsr
	and #$0F
	ora #$10
	tax
	stx $2118
	pla
	and #$0F
	ora #$10
	tax
	stx $2118
	rts

print_hex16:
	pha
	xba
	jsr print_hex8
	pla
	jmp print_hex8

print_ptr:
	ldy #0
:
	lda [ptr], Y
	and #$00FF
	beq :+
	tax
	stx $2118
	iny
	bne :-
:
	rts

.macro LATCHXY x_, y_
	lda #(VRAM_NMT+(x_)+(32*(y_)))
	sta $2116
.endmacro

.macro PRINTSTR string_
	.pushseg
	.rodata
	.local pxystr
	pxystr: .asciiz string_
	.popseg
	lda #.loword(pxystr)
	sta z:ptr+0
	ldx #^pxystr
	stx z:ptr+2
	jsr print_ptr
.endmacro

.macro PRINTXY x_, y_, string_
	LATCHXY (x_), (y_)
	PRINTSTR string_
.endmacro


;
; main
;

run:
	.a8
	.i8
	; enable NMI and auto-joypad
	lda $4210 ; RDNMI
	lda #$81
	sta $4200 ; NMITIMEN
	rep #$20
	sep #$10
	.a16
	.i8
	; TEST
	PRINTXY 5, 5, "Multest"
	; to jump into more relevant tests earlier:
	;lda #5081
	;sta z:testa
@loop:
	jsr render_on
	; read controllers
	lda z:gamepad
	sta z:lastpad
	; wait for auto-read to finish
	:
		lda $4212 ; HBVJOY (+RDIO)
		and #1
		bne :-
	lda $4218 ; JOY1
	sta z:gamepad
	eor z:lastpad
	and z:gamepad
	sta z:newpad
	jsr test
	; pass when testc returns to 0000
	lda z:testc
	;cmp #2 ; to test passing
	beq @pass
	jmp @loop
@pass:
	jsr render_off
	PRINTXY 5, 10, "Pass!"
	jsr render_on
:
	jmp :-

;
; test
;

test:
	jsr render_off
	LATCHXY 5,8
	lda z:testc
	jsr print_hex16
	PRINTSTR " ("
	lda z:testa
	jsr print_hex16
	PRINTSTR ")"
	jsr render_on
@loop:
	; mul16 to be tested
	lda z:testa
	sta z:mul16a
	lda z:testb
	sta z:mul16b
	jsr mul16
	lda z:mul16ab+0
	sta z:temp+0
	lda z:mul16ab+2
	sta z:temp+2
	; reference implementation
	lda z:testa
	sta z:mul16a
	lda z:testb
	sta z:mul16b
	jsr mul16
	; to test results
	;jsr render_off
	;jsr print_result
	;jsr render_on
	; compare
	;inc z:temp+0 ; to test fail
	lda z:mul16ab+0
	cmp z:temp+0
	bne @fail
	lda z:mul16ab+2
	cmp z:temp+2
	bne @fail
	; next test
	inc z:testb
	bne @loop
	lda z:testa
	clc
	adc #5081 ; a prime number just to avoid cycling in order
	sta z:testa
	inc z:testc
	rts
@fail:
	jsr render_off
	PRINTXY 5, 10, "Fail!"
	jsr print_result
	jsr render_on
:
	jmp :-

print_result:
	LATCHXY 5, 12
	lda z:testa
	jsr print_hex16
	PRINTSTR " x "
	lda z:testb
	jsr print_hex16
	LATCHXY 5,13
	lda z:temp+2
	jsr print_hex16
	lda z:temp+0
	jsr print_hex16
	PRINTSTR " != "
	lda z:mul16ab+2
	jsr print_hex16
	lda z:mul16ab+0
	jsr print_hex16
	rts

; unsigned 16-bit multiply, 32-bit result
; Written by 93143: https://forums.nesdev.org/viewtopic.php?p=280007#p280007
mul16: ; mul16a x mul16b = mul16ab, clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	ldx z:mul16a+0
	stx $4202
	ldy z:mul16b+0
	sty $4203         ; a0 x b0 (A)
	ldy z:mul16b+1
	stz z:mul16ab+2
	lda $4216
	sta z:mul16ab+0   ; 00AA
	sty $4203         ; a0 x b1 (B)
	ldx z:mul16a+1
	ldy z:mul16b+0
	lda $4216
	stx $4202
	sty $4203         ; a1 x b0 (C)
	clc
	adc z:mul16ab+1   ; 00AA + 0BB0 (can't set carry because high byte was 0)
	ldy z:mul16b+1
	adc $4216
	sty $4203         ; a1 x b1 (D)
	sta z:mul16ab+1   ; 00AA + 0BB0 + 0CC0
	lda z:mul16ab+2
	bcc :+
	adc #$00FF        ; if carry, increment top byte
:
	adc $4216
	sta z:mul16ab+2   ; 00AA + 0BB0 + 0CC0 + DD00
	rts

; reference shift+add multiply
rmul16:
	.a16
	.i8
	lda #0
	ldx #16
	lsr z:mul16a
@loop:
	bcc :+
		clc
		adc z:mul16b
	:
	ror
	ror z:mul16ab+0
	dex
	bne @loop
	sta z:mul16ab+2
	rts

;
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

; if the assert happens, add some padding or an align to keep these from crossing a bank
.macro BANKCHECK label_
	.assert (^(*-1))=(^(label_)), error, "BANKCHECK fail."
.endmacro

.align $10000

chr_test: .incbin "test.chr"
CHR_TEST_SIZE = * - chr_test
BANKCHECK chr_test

; for clearing
byte00: .byte $00

palette: ; 4 greys + some rainbow for test
	.word  0+( 0<<5)+( 0<<10)
	.word 13+(13<<5)+(13<<10)
	.word 22+(22<<5)+(22<<10)
	.word 31+(31<<5)+(31<<10)
	.repeat 12, I
		.byte (I+4) | ((I+4)<<4), (I+4) | ((I+4)<<4)
	.endrepeat
