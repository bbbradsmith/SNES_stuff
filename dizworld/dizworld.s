; dizworld Mode 7 demo
;
; Demonstrating 4 practical uses of Mode 7
;  Select - Toggle aspect ratio correction
;  A - Rotate background around a pivot point. (Suitable for bosses.)
;      D-pad to scroll.
;      L/R to scale.
;      Start to pause the spin.
;  B - Rotate around player. (Overhead level).
;      D-pad to rotate or move.
;      L/R to scale.
;  X - Tilted view.
;      L/R to adjust tilt.
;      D-pad to move.
;  Y - Flying view.
;      D-pad to rotate or move.
;      L/R to raise/lower.
;      Start to toggle horizon glow.
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

;
; Header
;

.segment "HEADER"
.byte "DIZWORLD MODE 7 DEMO "
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
mode:         .res 1

pause:        .res 1 ; pauses animation
aspect:       .res 1 ; 8:7 aspect correction
angle:        .res 1 ; for spinning modes
scale:        .res 2 ; for uniform scale
scale2:       .res 4 ; for separate axis scale
posx:         .res 6 ; position for some modes with subpixel precision
posy:         .res 6
player_tile:  .res 1 ; player sprite tile

cosa:         .res 2 ; sincos result
sina:         .res 2
math_a:       .res 4 ; multiply/divide/math input terms 16 or 32-bit
math_b:       .res 4
math_p:       .res 8 ; product/quotient
math_r:       .res 8 ; remainder
temp:         .res 16

det_r:        .res 4 ; storage for 1 / AD-BC (16.16f)
texelx:       .res 2 ; input/result for coordinate transforms
texely:       .res 2
screenx:      .res 2
screeny:      .res 2

nmi_bgmode:   .res 1
nmi_hofs:     .res 2
nmi_vofs:     .res 2
nmi_m7t:      .res 8
nmi_m7x:      .res 2
nmi_m7y:      .res 2

new_hdma_en:  .res 1 ; HDMA channel enable at next update
nmi_hdma_en:  .res 1 ; HDMA channel enable currently

oamp_i:       .res 2 ; OAM printing
oamp_x:       .res 1
oamp_y:       .res 1

.segment "LORAM" ; <$2000

oam:      .res 512+32

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
		stz z:nmi_hdma_en ; disable HDMA
		stz z:nmi_ready
		jmp @exit
	:
	; update
	; 1. copy new HDMA settings
	rep #$30
	.a16
	.i16
	phb
	ldx #.loword(new_hdma)
	ldy #.loword(nmi_hdma)
	lda #(16*8)-1
	mvn #^new_hdma,#^nmi_hdma
	plb
	sep #$20
	.a8
	lda z:new_hdma_en
	sta z:nmi_hdma_en
	; 2. apply other settings
	lda z:nmi_bgmode
	sta $2105 ; BGMODE
	lda z:nmi_hofs+0
	sta $210D ; BG1HOFS / M7HOFS
	lda z:nmi_hofs+1
	sta $210D
	lda z:nmi_vofs+0
	sta $210E ; BG1VOFS / M7VOFS
	lda z:nmi_vofs+1
	sta $210E
	lda z:nmi_m7t+0
	sta $211B ; M7A
	lda z:nmi_m7t+1
	sta $211B
	lda z:nmi_m7t+2
	sta $211C ; M7B
	lda z:nmi_m7t+3
	sta $211C
	lda z:nmi_m7t+4
	sta $211D ; M7C
	lda z:nmi_m7t+5
	sta $211D
	lda z:nmi_m7t+6
	sta $211E ; M7D
	lda z:nmi_m7t+7
	sta $211E
	lda z:nmi_m7x+0
	sta $211F ; M7E
	lda z:nmi_m7x+1
	sta $211F
	lda z:nmi_m7y+0
	sta $2120 ; M7F
	lda z:nmi_m7y+1
	sta $2120
	; 3. OAM DMA
	stz $2102
	stz $2103
	lda #%00000010 ; increment, 1-regiter (x2)
	sta $4300
	lda #$04 ; $2104
	sta $4301
	lda #<oam
	sta $4302
	lda #>oam
	sta $4303
	lda #^oam
	sta $4304
	lda #<(512+32)
	sta $4305
	lda #>(512+32)
	sta $4306
	lda #$01
	sta $420B ; DMA
	; force blanking off
	lda #$0F
	sta $2100
	stz z:nmi_ready
	; next field
@exit:
	; copy HDMA settings and execute
	rep #$30
	.a16
	.i16
	phb
	ldx #.loword(nmi_hdma)
	ldy #$4300
	lda #(16*8)-1
	mvn #^nmi_hdma,#^004300
	plb
	sep #$20
	.a8
	lda z:nmi_hdma_en
	sta $420C
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
	; load vram
	stz $2116 ; VMADD $0000
	stz $2117
	lda #%00000001 ; 2-register
	sta $4300
	lda #$18 ; $2118 VMDATA
	sta $4301
	lda #<bin_vram
	sta $4302
	lda #>bin_vram
	sta $4303
	lda #^bin_vram
	sta $4304
	stz $4305
	stz $4306 ; 64k bytes
	lda #$01
	sta $420B ; DMA
	; load palettes
	stz $2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta $4300
	lda #$22 ; $2122 CGDATA
	sta $4301
	lda #<pal_bg
	sta $4302
	lda #>pal_bg
	sta $4303
	lda #^pal_bg
	sta $4304
	stz $4305
	lda #>256
	sta $4306 ; 256 bytes
	lda #$01
	sta $420B ; DMA
	lda #<pal_fg
	sta $4302
	lda #>pal_fg
	sta $4303
	lda #^pal_fg
	sta $4304
	stz $4305
	lda #>256
	sta $4306 ; 256 bytes
	lda #$01
	sta $420B ;DMA
	; setup OAM
	lda #$E0
	sta a:oam+0
	rep #$30
	.a16
	.i16
	ldx #.loword(oam+0)
	ldy #.loword(oam+1)
	lda #(512-2)
	mvn #^oam,#^oam ; fill with E0
	sep #$30
	.a8
	.i8
	lda #$AA
	sta oam+512 ; first 4 sprites are 32x32
	; setup PPU addresses
	lda #((>VRAM_NMT_SKY) & $FC)
	sta $2107 ; BG1SC nametable, 1-screen
	lda #(VRAM_CHR_SKY >> 12)
	sta $210B ; BG12NBA
	lda #((VRAM_CHR_FG >> 13) | $20)
	sta $2101 ; OBJSEL 8x8 + 32x32 sprites
	lda #$11
	sta $212C ; TM OBJ + BG1 main-screen
	stz $212D ; TS empty sub-screen
	; begin
	jmp run

;
; main loop
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
	jsr oamp_start
	; reset mode 7 transform matrix to identity (1,0) (0,1)
	ldx #$01
	stx nmi_m7t+1
	stx nmi_m7t+7
	; TODO eventually set default mode Y?
	jsr set_mode_b
@loop:
	; post a render update
	jsr oamp_finish
	ldx #1
	stx z:nmi_ready
	wai
	:
		ldx z:nmi_ready
		bne :-
	jsr oamp_start
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
	; pausing
	lda z:newpad
	and #$1000
	beq :+
		lda z:pause
		eor #1
		tax
		stx z:pause
	:
	; aspect ratio correction
	lda z:newpad
	and #$2000
	beq :+
		lda z:aspect
		eor #1
		tax
		stx z:aspect
	:
	; switch mode
	lda z:newpad
	and #$0080 ; A
	bne @set_mode_a
	lda z:newpad
	and #$8000 ; B
	bne @set_mode_b
	lda z:newpad
	and #$0040 ; X
	bne @set_mode_x
	lda z:newpad
	and #$4000 ; Y
	bne @set_mode_y
	; dispatch mode
	ldx z:mode
	beq @mode_a
	cpx #1
	beq @mode_b
	cpx #2
	beq @mode_x
	cpx #3
	beq @mode_y
	; fallthrough?
	ldx #0
	stx z:mode
@mode_a:
	jsr mode_a
	jmp @loop
@mode_b:
	jsr mode_b
	jmp @loop
@mode_x:
	jsr mode_x
	jmp @loop
@mode_y:
	jsr mode_y
	jmp @loop
@set_mode_a:
	ldx #0
	stx z:mode
	jsr set_mode_a
	jmp @loop
@set_mode_b:
	ldx #1
	stx z:mode
	jsr set_mode_b
	jmp @loop
@set_mode_x:
	ldx #2
	stx z:mode
	jsr set_mode_x
	jmp @loop
@set_mode_y:
	ldx #3
	stx z:mode
	jsr set_mode_y
	jmp @loop

;
; OAM sprite
;

oam_sprite_clear: ; move 4 sprites offscreen
	.i8
	ldx #$E0
	stx a:oam+1+( 0)
	stx a:oam+1+( 4)
	stx a:oam+1+( 8)
	stx a:oam+1+(12)
	rts

oam_sprite: ; A = tile, X = OAM start index (x4), screenx,screeny location
	.a16
	tay
	lda z:screenx
	sec
	sbc #16
	sta z:temp+0
	lda z:screeny
	sec
	sbc #32
	sta z:temp+2
	php
	sep #$20
	.a8
	tya
	sta a:oam+2, X
	lda z:screenx+1
	bne @skip
	lda z:screeny+1
	bne @skip
	lda z:screeny+0
	cmp #$E0
	bcs @skip
	lda z:temp+0
	sta a:oam+0, X
	lda z:temp+2
	sta a:oam+1, X
	lda #$30 ; high priority, palette 0, no flip
	sta a:oam+3, X
	; transfer X high bit to high table
	txa
	lsr
	lsr
	pha
	and #3
	tax ; X = 0-3 shifts needed
	pla
	lsr
	lsr
	tay ; Y = high oam entry
	lda a:oam+512, Y
	and f:@hoam_mask, X
	sta a:oam+512, Y
	lda z:temp+1
	and #1 ; high bit of adjusted x
	ora #2 ; 32x32 sprite size
	cpx #0
	beq :++
	:
		asl
		asl
		dex
		bne :-
	:
	ora a:oam+512, Y
	sta a:oam+512, Y
	plp
	rts
@skip:
	lda #$E0
	sta oam+1, X
	plp
	rts
@hoam_mask:
.byte %11111100
.byte %11110011
.byte %11001111
.byte %00111111

;
; OAM printing
;

OAMP_X = 24 ; starting position for OAM printing
OAMP_Y = 168
OAMP_I = 4*4 ; skip first 4 sprites

oamp_start:
	.a16
	.i8
	lda #OAMP_I
	sta z:oamp_i
	ldx #OAMP_X
	stx z:oamp_x
	ldx #OAMP_Y
	stx z:oamp_y
	rts

oamp_finish:
	php
	sep #$20
	rep #$10
	.a8
	.i16
	ldx z:oamp_i
	lda #$E0
	:
		sta a:oam+1, X
		inx
		inx
		inx
		inx
		cpx #512
		bcc :-
	plp

oamp_return: ; carriage return
	.a16
	.i8
	ldx #OAMP_X
	stx z:oamp_x
	lda #0
	ldx z:oamp_y
	txa
	clc
	adc #8
	tax
	stx z:oamp_y
	rts

oamp_char_:
	.a8
	.i16
	ldx z:oamp_i
	sta a:oam+2, X
	lda z:oamp_y
	sta a:oam+1, X
	lda z:oamp_x
	sta a:oam+0, X
	clc
	adc #8
	sta z:oamp_x
	lda #$30 ; priority 3
	sta a:oam+3, X
	inx
	inx
	inx
	inx
	stx z:oamp_i
	rts

oamp_space: ; skips a space
	.a16
	.i8
	ldx z:oamp_x
	txa
	clc
	adc #8
	tax
	stx z:oamp_x
	rts

oamp_alpha: ; A = ascii capital letter to print
	php
	sep #$20
	rep #$10
	.a8
	.i16
	clc
	adc #$CA-'A'
	jsr oamp_char_
	plp
	rts

oamp_alpha_space: ; alpha + space
	jsr oamp_alpha
	jmp oamp_space

oamp_hex8: ; A = value to print in hex
	php
	sep #$20
	rep #$10
	.a8
	.i16
	pha
	lsr
	lsr
	lsr
	lsr
	ora #$C0
	jsr oamp_char_
	pla
	and #$0F
	ora #$C0
	jsr oamp_char_
	plp
	rts

oamp_hex16: ; A = 16-bit value to print in hex
	.a16
	pha
	xba
	jsr oamp_hex8
	pla
	jmp oamp_hex8

oamp_hex16_space:
	jsr oamp_hex16
	jmp oamp_space

;
; Math
;

; unsigned 16-bit multiply, 32-bit result
; Written by 93143: https://forums.nesdev.org/viewtopic.php?p=280089#p280089
umul16: ; math_a x math_b = math_p, clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	ldx z:math_a+0
	stx $4202
	ldy z:math_b+0
	sty $4203         ; a0 x b0 (A)
	ldx z:math_b+1
	stz z:math_p+2
	lda $4216
	stx $4203         ; a0 x b1 (B)
	sta z:math_p+0    ; 00AA
	ldx z:math_a+1
	lda $4216
	stx $4202
	sty $4203         ; a1 x b0 (C)
	clc
	adc z:math_p+1    ; 00AA + 0BB0 (can't set carry because high byte was 0)
	ldy z:math_b+1
	adc $4216
	sty $4203         ; a1 x b1 (D)
	sta z:math_p+1    ; 00AA + 0BB0 + 0CC0
	lda z:math_p+2
	bcc :+
	adc #$00FF        ; if carry, increment top byte
:
	adc $4216
	sta z:math_p+2    ; 00AA + 0BB0 + 0CC0 + DD00
	rts

; 16-bit multiply, truncated 16-bit result (sign-agnostic)
mul16t:
	.a16
	.i8
	ldx z:math_a+0
	stx $4202
	ldy z:math_b+0
	sty $4203         ; a0 x b0 (A)
	ldx z:math_b+1
	nop
	lda $4216
	stx $4203         ; a0 x b1 (B)
	sta z:math_p+0    ; AA
	ldx z:math_a+1
	lda $4216
	stx $4202
	sty $4203         ; a1 x b0 (C)
	clc
	adc z:math_p+1    ; AA + B0
	clc
	adc $4216         ; AA + B0 + C0
	tax
	stx z:math_p+1
	lda z:math_p+0
	rts

; signed 16-bit multiply, 32-bit result, clobbers A/X/Y
smul16:
	.a16
	.i8
	jsr umul16
	; A = math_p+2
	; X = math_a+1
	; Y = math_b+1
	cpx #$80
	bcc :+
		;sec
		sbc z:math_b ; sign extend math_a: (-1 << 16) x math_b
	:
	cpy #$80
	bcc :+
		;sec
		sbc z:math_a ; sign extend math_b: (-1 << 16) x math_a
	:
	sta z:math_p+2
	rts

smul16f: ; smul16 but returning the middle 16-bit value as A (i.e. 8.8 fixed point multiply)
	.a16
	.i8
	jsr smul16
	lda z:math_p+1
	rts

smul32f_16f: ; a = 24.8 fixed, b = 8.8 fixed, clobbers: math_a/math_b
	.a16
	.i8
	lda z:math_a+0
	sta z:math_p+4
	lda z:math_a+2
	sta z:math_p+6 ; p+4 = a
	lda z:math_b+0
	sta z:math_r+4 ; r+4 = b
	cmp #$8000
	bcs :+
		lda #0
		bra :++
	:
		lda #$FFFF
	:
	sta z:math_r+6 ; sign extended
	; 32-bit multiply from 3 x 16-bit multiply
	jsr umul16     ; a0 x b0 (A)
	lda z:math_p+0
	sta z:math_r+0
	lda z:math_p+2
	sta z:math_r+2 ; r+0 = 00AA
	lda z:math_r+6
	sta z:math_b+0
	jsr mul16t     ; a0 x b1 (B)
	clc
	adc z:math_r+2
	sta z:math_r+2 ; r+0 = AAAA + BB00
	lda z:math_p+6
	sta z:math_a+0
	lda z:math_r+4
	sta z:math_b+0
	jsr mul16t     ; a1 x b0 (C)
	clc
	adc z:math_r+2 ; r+0 = AAAA + BB00 + CC00
	sta z:math_p+2
	lda z:math_r+0
	sta z:math_p+0
	lda z:math_p+2 ; result in upper bits
	rts

smul32ft: ; a = 24.8 fixed, b = 16.16 fixed, 40-bit result in math_r, returns top 16 bits, clobbers temp
	.a16
	.i8
	; sign extend and copy to temp
	lda z:math_a+0
	sta z:temp+0
	lda z:math_a+2
	sta z:temp+2
	stz z:temp+4
	cmp #$8000
	bcc :+
		lda #$FFFF
		sta z:temp+4
	:
	lda z:math_b+0
	sta z:temp+8
	lda z:math_b+2
	sta z:temp+10
	stz z:temp+12
	cmp #$8000
	bcc :+
		lda #$FFFF
		sta z:temp+12
	:
	; 40-bit multiply (temporary result in r)
	jsr umul16 ; a0 x b0 (A)
	lda z:math_p+0
	sta z:math_r+0
	lda z:math_p+2
	sta z:math_r+2
	stz z:math_r+4 ; 0AAAA
	lda z:temp+10
	sta z:math_b
	jsr umul16 ; a0 x b1 (B)
	lda z:math_p+0
	clc
	adc z:math_r+2
	sta z:math_r+2
	lda z:math_p+2
	adc z:math_r+4
	sta z:math_r+4 ; 0AAAA + BBB00
	lda z:temp+2
	sta z:math_a
	lda z:temp+8
	sta z:math_b
	jsr umul16 ; a1 x b0 (C)
	lda z:math_p+0
	clc
	adc z:math_r+2
	sta z:math_r+2
	lda z:math_p+2
	adc z:math_r+4 ; 0AAAA + BBB00 + CCC00
	; 3 8x8 multiplies for the top byte
	; A is now temporary r+4
	ldx z:temp+0
	stx $4202
	ldx z:temp+12
	stx $4203 ; a0 x b2 (D)
	ldx z:temp+2
	clc
	adc $4216      ; 0AAAA + BBB00 + CCC00 + D0000
	stx $4202
	ldx z:temp+10
	stx $4203 ; a1 x b1 (E)
	ldx z:temp+4
	clc
	adc $4216     ; 0AAAA + BBB00 + CCC00 + D0000 + E0000
	stx $4202
	ldx z:temp+8
	stx $4203 ; a2 x b0 (F)
	nop
	nop
	clc
	adc $4216     ; 0AAAA + BBB00 + CCC00 + D0000 + E0000 + F0000
	sta z:math_r+4
	lda z:math_r+3 ; return top 16 bits
	rts

; 16-bit / 16-bit division, 16 + 16 result
; math_a / math_b = math_p
; math_a % math_b = math_r
; clobbers A/X
; This could potentially be hardware-accelerated for ~2x speedup:
;   See: https://github.com/bbbradsmith/SNES_stuff/blob/main/multest/test_div16.s
udiv16:
	.a16
	.i8
	lda z:math_a
	asl
	sta z:math_p
	lda #0
	ldx #16
@loop:
	rol
	cmp z:math_b
	bcc :+
		sbc z:math_b
	:
	rol z:math_p
	dex
	bne @loop
	sta z:math_r
	rts

; 32-bit / 32-bit division, 32 + 32 result
; math_a / math_b = math_p
; math_a % math_b = math_r
; clobbers A/X
; Is there a way to do this faster with the hardware divider?
udiv32:
	.a16
	.i8
	lda z:math_a+0
	asl
	sta z:math_p+0
	lda z:math_a+2
	rol
	sta z:math_p+2
	stz z:math_r+2 ; A used temporarily as low word of r
	lda #0
	ldx #32
@loop:
	rol
	rol z:math_r+2
	cmp z:math_b+0
	pha
	lda z:math_r+2
	sbc z:math_b+2
	bcc :+
		sta z:math_r+2
		pla
		sbc z:math_b+0
		sec
		bra :++
	:
		pla
	:
	rol z:math_p+0
	rol z:math_p+2
	dex
	bne @loop
	sta z:math_r+0
	rts

; fixed point reciprocal, clobbers A/X/Y/math_a/math_b/math_p/math_r
recip16f: ; A = fixed point number, result in A
	sta z:math_b+0
	stz z:math_b+2
	cmp #$8000
	bcs :+
		stz z:temp+0
		ldy #0
		bra :++
	:
		lda #$FFFF
		eor z:math_b+0
		inc
		sta z:math_b+0
		ldy #1
	:
	; numerator: 1.0 << 16
	stz z:math_a+0
	lda #(1*256)
	sta z:math_a+2
	jsr udiv32 ; A<<16
	cpy #1
	bne :+
		lda #0
		sec
		sbc z:math_p+0
		sta z:math_p+0
		lda #0
		sbc z:math_p+2
		sta z:math_p+2
	:
	lda z:math_p+1
	rts

sign: ; A = value, returns either 0 or $FFFF, preserves flags
	.a16
	php
	cmp #$8000
	bcs :+
		lda #0
		plp
		rts
	:
		lda #$FFFF
		plp
		rts
	;

sincos: ; A = angle 0-255, result in cosa/sina, clobbers A/X
	.a16
	.i8
	rep #$10
	.i16
	asl
	tax
	lda f:sincos_table, X
	sta z:cosa
	txa
	clc
	adc #(192*2) ; sin(x) = cos(x + 3/4 turn)
	and #(256*2)-1
	tax
	lda f:sincos_table, X
	sta z:sina
	sep #$10
	.i8
	rts

sincos_table:
.word $0100,$0100,$0100,$00FF,$00FF,$00FE,$00FD,$00FC,$00FB,$00FA,$00F8,$00F7,$00F5,$00F3,$00F1,$00EF
.word $00ED,$00EA,$00E7,$00E5,$00E2,$00DF,$00DC,$00D8,$00D5,$00D1,$00CE,$00CA,$00C6,$00C2,$00BE,$00B9
.word $00B5,$00B1,$00AC,$00A7,$00A2,$009D,$0098,$0093,$008E,$0089,$0084,$007E,$0079,$0073,$006D,$0068
.word $0062,$005C,$0056,$0050,$004A,$0044,$003E,$0038,$0032,$002C,$0026,$001F,$0019,$0013,$000D,$0006
.word $0000,$FFFA,$FFF3,$FFED,$FFE7,$FFE1,$FFDA,$FFD4,$FFCE,$FFC8,$FFC2,$FFBC,$FFB6,$FFB0,$FFAA,$FFA4
.word $FF9E,$FF98,$FF93,$FF8D,$FF87,$FF82,$FF7C,$FF77,$FF72,$FF6D,$FF68,$FF63,$FF5E,$FF59,$FF54,$FF4F
.word $FF4B,$FF47,$FF42,$FF3E,$FF3A,$FF36,$FF32,$FF2F,$FF2B,$FF28,$FF24,$FF21,$FF1E,$FF1B,$FF19,$FF16
.word $FF13,$FF11,$FF0F,$FF0D,$FF0B,$FF09,$FF08,$FF06,$FF05,$FF04,$FF03,$FF02,$FF01,$FF01,$FF00,$FF00
.word $FF00,$FF00,$FF00,$FF01,$FF01,$FF02,$FF03,$FF04,$FF05,$FF06,$FF08,$FF09,$FF0B,$FF0D,$FF0F,$FF11
.word $FF13,$FF16,$FF19,$FF1B,$FF1E,$FF21,$FF24,$FF28,$FF2B,$FF2F,$FF32,$FF36,$FF3A,$FF3E,$FF42,$FF47
.word $FF4B,$FF4F,$FF54,$FF59,$FF5E,$FF63,$FF68,$FF6D,$FF72,$FF77,$FF7C,$FF82,$FF87,$FF8D,$FF93,$FF98
.word $FF9E,$FFA4,$FFAA,$FFB0,$FFB6,$FFBC,$FFC2,$FFC8,$FFCE,$FFD4,$FFDA,$FFE1,$FFE7,$FFED,$FFF3,$FFFA
.word $0000,$0006,$000D,$0013,$0019,$001F,$0026,$002C,$0032,$0038,$003E,$0044,$004A,$0050,$0056,$005C
.word $0062,$0068,$006D,$0073,$0079,$007E,$0084,$0089,$008E,$0093,$0098,$009D,$00A2,$00A7,$00AC,$00B1
.word $00B5,$00B9,$00BE,$00C2,$00C6,$00CA,$00CE,$00D1,$00D5,$00D8,$00DC,$00DF,$00E2,$00E5,$00E7,$00EA
.word $00ED,$00EF,$00F1,$00F3,$00F5,$00F7,$00F8,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF,$00FF,$0100,$0100
; python generator:
;import math
;vt = [round(256*math.cos(i*math.pi/128)) for i in range(256)]
;for y in range(16):
;    s = ".word "
;    for x in range(16):
;        s += "$%04X" % (vt[x+(y*16)] & 0xFFFF)
;        if x < 15: s += ","
;    print(s)

;
; Mode 7 calculations
;

;
; A,B,C,D = M7A,B,C,D "matrix"
; Sx,Sy = screen coordinate (0-255,0-223) "screen"
; Ox,Oy = M7HOFS,M7VOFS "offset"
; Px,Py = M7X,M7Y "pivot"
; Tx,Ty = texel coordinate
; Mx,My = horizontal texel scale, vertical texel scale
;
; The official texel lookup:
;
;   [A B]   [Sx+Ox-Px]   [ Px ]   [ Tx ]
;   [   ] x [        ] + [    ] = [    ]
;   [C D]   [Sy+Oy-Py]   [ Py ]   [ Ty ]
; 
;   Tx = A (Sx + Ox - Px) + B (Sy + Oy - Py) + Px
;   Ty = C (Sx + Ox - Px) + D (Sy + Oy - Py) + Py
;
; Texel to screen:
;
;   Sx = Px - Ox + (D(Tx-Px)-B(Ty-Py)) / (AD-BC)
;   Sy = Py - Oy + (A(Ty-Py)-C(Tx-Px)) / (AD-BC)
;
; Calculating offset when you know where on-screen a texel should appear,
; which is the same as texel-to-screen but with Ox,Oy and Sx,Sy swapped:
;
;   Ox = Px - Sx + (D(Tx-Px)-B(Ty-Py)) / (AD-BC)
;   Oy = Py - Sy + (A(Ty-Py)-C(Tx-Px)) / (AD-BC)
;
; Because we are only using rotation + scale for ABCD, the determinant (AD-BC) is more simply:
;
;   AD - BC = Mx cos * My cos + Mx sin * My sin
;           = (Mx * My)(cos^2 + sin^2)
;           = Mx * My
;

;
; In general in these examples calculating texel-to-screen (and the reciprocal determinant det_r) are time-intensive operations.
; In most practical cases these can be simplified or avoided. For example:
;  - Scale of 1 means det_r=1 and can be skipped entirely.
;  - Fixed or limited scaling could have a constant det_r, or looked up from a small table.
;  - In mode B, since Px,Py is mid-screen, Tx-Px,Ty-Py is always low for on-screen sprites. With appropriate culling texel_to_scale can be done at lower precision.
;

; 1 for higher precision determinant reciprocal (more accurate under scaling)
;   adds about 10 more hardware multiplies to texel_to_screen (11 vs 9 scanlines?)
DETR40 = 1

; recalculate det_r = 1 / (AD-BC) = Mx * My
; used by texel_to_screen
calc_det_r:
	lda z:scale2+0
	sta z:math_a
	lda z:scale2+2
	sta z:math_b
	jsr smul16f
	jsr recip16f
	lda z:math_p+0
	sta z:det_r+0
	lda z:math_p+2
	sta z:det_r+2
	rts

texel_to_screen: ; input: texelx,texely output screenx,screeny (requires det_r)
	lda z:texelx
	sec
	sbc z:nmi_m7x
	pha ; Tx-Px 16u
	sta z:math_a
	lda z:nmi_m7t+4 ; C
	sta z:math_b
	jsr smul16
	lda z:math_p+0
	sta z:temp+0 ; C(Tx-Px) 24.8f
	lda z:math_p+2
	sta z:temp+2
	lda z:texely
	sec
	sbc z:nmi_m7y
	pha ; Ty-Py 16u
	sta z:math_a
	lda z:nmi_m7t+0 ; A
	sta z:math_b
	jsr smul16
	lda z:math_p+0
	sec
	sbc z:temp+0 ; A(Ty-Py)-C(Tx-Px) 24.8f
	sta z:math_a+0
	lda z:math_p+2
	sbc z:temp+2
	sta z:math_a+2
	.if DETR40
		lda z:det_r+0 ; 16.16f
		sta z:math_b+0
		lda z:det_r+2
		sta z:math_b+2
		jsr smul32ft ; (A(Ty-Py)-C(Tx-Px)) / (AD-BC) 16u
	.else
		lda z:det_r+1 ; 8.8f
		sta z:math_b
		jsr smul32f_16f ; 16u
	.endif
	clc
	adc z:nmi_m7y ; Py + (A(Ty-Py)-C(Tx-Px)) / (AD-BC)
	sec
	sbc z:nmi_vofs ; Py - Oy + (A(Ty-Py)-C(Tx-Px)) / (AD-BC)
	sta z:screeny
	pla ; Ty-Py 16u
	sta z:math_a
	lda z:nmi_m7t+2 ; B
	sta z:math_b
	jsr smul16
	lda z:math_p+0
	sta z:temp+0 ; B(Ty-Py) 24.8f
	lda z:math_p+2
	sta z:temp+2
	pla ; Tx-Px 16u
	sta z:math_a
	lda z:nmi_m7t+6 ;D
	sta z:math_b
	jsr smul16
	lda z:math_p+0
	sec
	sbc z:temp+0 ; D(Tx-Px)-B(Ty-Py) 24.8f
	sta z:math_a+0
	lda z:math_p+2
	sbc z:temp+2
	sta z:math_a+2
	.if DETR40
		lda z:det_r+0 ; 16.16f
		sta z:math_b+0
		lda z:det_r+2
		sta z:math_b+2
		jsr smul32ft ; (D(Tx-Px)-B(Ty-Py)) / (AD-BC) 16u
	.else
		lda z:det_r+1 ; 8.8f
		sta z:math_b
		jsr smul32f_16f ; 16u
	.endif
	clc
	adc z:nmi_m7x ; Px + (D(Tx-Px)-B(Ty-Py)) / (AD-BC)
	sec
	sbc z:nmi_hofs ; Px - Ox + (D(Tx-Px)-B(Ty-Py)) / (AD-BC)
	sta z:screenx
	rts

simple_scroll: ; UDLR = hofs/vofs
	.a16
	lda z:gamepad
	and #$0100 ; right
	beq :+
		inc z:nmi_hofs
	:
	lda z:gamepad
	and #$0200 ; left
	beq :+
		dec z:nmi_hofs
	:
	lda z:gamepad
	and #$0400 ; down
	beq :+
		inc z:nmi_vofs
	:
	lda z:gamepad
	and #$0800 ; up
	beq :+
		dec z:nmi_vofs
	:
	rts

simple_rot_scale: ; LR shoulder = scale adjust, build ABCD from angle/scale
	.a16
	.i8
	; scale adjust
	lda z:gamepad
	and #$0020 ; L shoulder
	beq :+
		inc z:scale
	:
	lda z:gamepad
	and #$0010 ; R shoulder
	beq :+
		dec z:scale
	:
	; look up angle vectors
	lda #0
	ldx z:angle
	txa
	jsr sincos
	; apply uniform scale
	lda z:scale
	sta z:scale2+0 ; copy here for stats
	sta z:scale2+2
	sta z:math_a
	lda z:cosa
	sta z:math_b
	jsr smul16f
	sta z:cosa
	lda z:scale
	sta z:math_a
	lda z:sina
	sta z:math_b
	jsr smul16f
	sta z:sina
	; clockwise (map-space) rotation matrix
	lda z:cosa
	sta z:nmi_m7t+0 ; A = cos
	sta z:nmi_m7t+6 ; D = cos
	lda z:sina
	sta z:nmi_m7t+2 ; B = sin
	eor #$FFFF
	inc
	sta z:nmi_m7t+4 ; C = -sin
	; aspect ratio correction if select is pressed
	ldx z:aspect
	beq :+
		lda #(256*8/7)
		sta z:math_a
		lda z:nmi_m7t+0
		sta z:math_b
		jsr smul16f
		sta z:nmi_m7t+0 ; A *= 8/7
		lda #(256*8/7)
		sta z:math_a
		lda z:nmi_m7t+4
		sta z:math_b
		jsr smul16f
		sta z:nmi_m7t+4 ; C *= 8/7
	:
	rts

print_stats:
	.a16
	.i8
	; ABCD
	lda #'A'
	jsr oamp_alpha
	lda #'B'
	jsr oamp_alpha_space
	lda z:nmi_m7t+0
	jsr oamp_hex16_space
	lda z:nmi_m7t+2
	jsr oamp_hex16
	jsr oamp_return
	lda #'C'
	jsr oamp_alpha
	lda #'D'
	jsr oamp_alpha_space
	lda z:nmi_m7t+4
	jsr oamp_hex16_space
	lda z:nmi_m7t+6
	jsr oamp_hex16
	jsr oamp_return
	; Px,Py
	jsr oamp_space
	lda #'P'
	jsr oamp_alpha_space
	lda z:nmi_m7x
	jsr oamp_hex16_space
	lda z:nmi_m7y
	jsr oamp_hex16
	jsr oamp_return
	; Ox,Oy
	jsr oamp_space
	lda #'O'
	jsr oamp_alpha_space
	lda z:nmi_hofs
	jsr oamp_hex16_space
	lda z:nmi_vofs
	jsr oamp_hex16
	jsr oamp_return
	; Mx,My
	jsr oamp_space
	lda #'M'
	jsr oamp_alpha_space
	lda z:scale+0
	jsr oamp_hex16_space
	lda z:scale+2
	jsr oamp_hex16
	jmp oamp_return

;
; Mode A test "Overhead, simple"
; - Map spins around a fixed point
; - Player moves over the rotated map
; - L/R apply scale
;
; The basic concept here is just to pin Px,Py in the middle of something you want to rotate/scale,
; and then scroll the screen to move it with hofs/vofs just a normal scroll offset relative to Px,Py.
;

; pivot point for centre of the spin
MODE_A_PX = 152
MODE_A_PY = 120

; texel coordinate of screen-sprite
MODE_A_TX = 280 ; entrance to forest
MODE_A_TY = 115
;MODE_A_TX = 807 ; tip of 7 in "MODE 7" (accuracy declines with distance from Px)
;MODE_A_TY = 645

set_mode_a:
	.a16
	.i8
	ldx #7
	stx z:nmi_bgmode
	ldx #0
	stx z:new_hdma_en
	stx z:angle
	lda #$0100
	sta z:scale
	lda #MODE_A_PX
	sta z:nmi_m7x
	lda #MODE_A_PY
	sta z:nmi_m7y
	stz z:nmi_hofs
	stz z:nmi_vofs
	jsr oam_sprite_clear
mode_a:
	; spin
	lda #0
	ldx z:angle
	ldy z:pause
	bne :+
		inx
		stx z:angle
	:
	jsr simple_rot_scale
	jsr simple_scroll
	; sprite pinned to tilemap
	jsr calc_det_r
	lda #MODE_A_TX
	sta z:texelx
	lda #MODE_A_TY
	sta z:texely
	jsr texel_to_screen
	ldx #0
	lda #$8C ; arrow
	jsr oam_sprite
	; stats
	jmp print_stats

;
; Mode B test "Overhead, first person"
; - Map rotates around the player
; - Player faces "up", and controls spin
; - L/R apply scale
;
; In this mode Px,Py is pinned to an on-screen location, letting us rotate/scale around it.
; and making movements relative to it. Ox,Oy is easy to calculate (see below),
; and moving up/down is accomplished by adding B/D to our Px,Py.
; (Similarly moving left/right could be done with A/C.)
;

MODE_B_SX = 128
MODE_B_SY = 112

set_mode_b:
	.a16
	.i8
	ldx #7
	stx z:nmi_bgmode
	ldx #0
	stx new_hdma_en
	stx z:angle
	lda #$0100
	sta z:scale
	; start at the centre of the spiral
	stz z:posx+0
	stz z:posy+0
	lda #MODE_A_PX
	sta z:posx+2
	lda #MODE_A_PY
	sta z:posy+2
	stz z:posx+4
	stz z:posy+4
	jsr oam_sprite_clear
	ldx #$08
	stx z:player_tile
mode_b:
	; rotate with left/right
	ldx z:angle
	lda z:gamepad
	and #$0200 ; left
	beq :+
		ldy #$00
		sty z:player_tile
		inx
	:
	lda z:gamepad
	and #$0100 ; right
	beq :+
		ldy #$04
		sty z:player_tile
		dex
	:
	stx z:angle
	; scale with L/R, generate rotation matrix
	jsr simple_rot_scale
	; up/down moves player
	lda z:gamepad
	and #$0400 ; down
	beq :+
		ldy #$0C
		sty z:player_tile
		; X += B
		lda z:nmi_m7t + 2 ; B
		pha
		clc
		adc z:posx+1
		sta z:posx+1
		pla
		jsr sign
		adc z:posx+3
		sta z:posx+3
		; Y += D
		lda z:nmi_m7t + 6 ; D
		pha
		clc
		adc z:posy+1
		sta z:posy+1
		pla
		jsr sign
		adc z:posy+3
		sta z:posy+3
	:
	lda z:gamepad
	and #$0800 ; up
	beq :+
		ldy #$08
		sty z:player_tile
		; X -= B
		lda #0
		sec
		sbc z:nmi_m7t + 2 ; B
		pha
		clc
		adc z:posx+1
		sta z:posx+1
		pla
		jsr sign
		adc z:posx+3
		sta z:posx+3
		; Y -= D
		lda #0
		sec
		sbc z:nmi_m7t + 6 ; D
		pha
		clc
		adc z:posy+1
		sta z:posy+1
		pla
		jsr sign
		adc z:posy+3
		sta z:posy+3
	:
	; posx/posy is Px/Py
	lda z:posx+2
	sta z:nmi_m7x
	lda z:posy+2
	sta z:nmi_m7y
	; calculate hofs/vofs to put Px/Py at desired centre (equivalent to texel_to_screen with screen position in place of vofs/hofs)
	; This would be a "generic" way to calculate it via texel_to_screen:
	;	lda #MODE_B_SX
	;	sta z:nmi_hofs
	;	lda #MODE_B_SY
	;	sta z:nmi_vofs
	;	lda z:nmi_m7x
	;	sta z:texelx
	;	lda z:nmi_m7y
	;	sta z:texely
	;	jsr texel_to_screen
	;	lda z:screenx
	;	sta z:nmi_hofs
	;	lda z:screeny
	;	sta z:nmi_vofs
	; However, because Tx,Ty = Px,Py the offset equation simplifies dramatically:
	;   Ox = Px - Sx + (D(Tx-Px)-B(Ty-Py)) / (AD-BC)
	;      = Px - Sx
	;   Oy = Py - Sy + (A(Ty-Py)-C(Tx-Px)) / (AD-BC)
	;      = Py - Sy
	lda z:nmi_m7x
	sec
	sbc #MODE_B_SX
	sta z:nmi_hofs
	lda z:nmi_m7y
	sec
	sbc #MODE_B_SY
	sta z:nmi_vofs
	; player sprite
	lda #MODE_B_SX
	sta z:screenx
	lda #MODE_B_SY
	sta z:screeny
	lda player_tile
	ldx #0
	jsr oam_sprite
	; sprite pinned to tilemap
	jsr calc_det_r
	lda #MODE_A_TX
	sta z:texelx
	lda #MODE_A_TY
	sta z:texely
	jsr texel_to_screen
	ldx #4
	lda #$8C ; arrow
	jsr oam_sprite
	; stats
	jmp print_stats

;
; Mode X test "Tilted plane"
; - Map appears with perspective tilt, 1:1 at player centre
; - Player moves only orthogonally
; - L/R adjusts tilt amount?
;

set_mode_x:
	.a16
	.i8
	ldx #7
	ldx #3 ; mode 3 HACK
	stx z:nmi_bgmode
	ldx #0
	stx new_hdma_en
	jsr oam_sprite_clear
mode_x:
	jmp simple_scroll

;
; Mode Y test "Flying"
; - Mode 1 clouds at top, fixed colour horizon fades
; - Map rotates around the player
;

set_mode_y:
	.a16
	.i8
	ldx #1
	stx z:nmi_bgmode
	ldx #0
	stx new_hdma_en
	jsr oam_sprite_clear
mode_y:
	jsr simple_scroll ; TODO
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

bin_vram: ; 64k block to initialize vram in one shot
; mode 7 block (nmt+chr interleaved), 32k at VRAM 0
md7_bg: .incbin "bg.md7"
; sprites, 16k aligned
.align $4000
chr_fg: .incbin "fg.chr"
; mode 1 sky chr, 4k aligned
.align $1000
chr_sky: .incbin "sky.chr"
; mode 1 sky nmt, 2k aligned
.align $800
nmt_sky: .incbin "sky.nmt"
BANKCHECK bin_vram

VRAM_CHR_FG  = (chr_fg -bin_vram)>>1
VRAM_CHR_SKY = (chr_sky-bin_vram)>>1
VRAM_NMT_SKY = (nmt_sky-bin_vram)>>1

.align $10000 ; next bank

; palettes
pal_bg: .incbin "bg.pal"
pal_fg: .incbin "fg.pal"
BANKCHECK pal_bg

; for clearing
byte00: .byte $00
byteE0: .byte $E0
byteAA: .byte $AA
