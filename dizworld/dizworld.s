; dizworld Mode 7 demo
;
; Demonstrating 4 practical uses of Mode 7
;  Select - Toggle aspect ratio correction
;  Start - Toggle stats / pause spin
;  A - Rotate background around a pivot point. (Suitable for bosses.)
;      D-pad to scroll.
;      L/R to scale.
;      Start to pause the spin.
;  B - Rotate around player. (Overhead level.)
;      D-pad to rotate or move.
;      L/R to scale.
;  X - Tilted view.
;      L/R to adjust tilt.
;      D-pad to move.
;  Y - Flying view. (RPG world map, or racing game.)
;      D-pad to rotate or move.
;      L/R to raise/lower.
;      Select to reset position. (Aspect ratio adjustment does not apply to this mode.)
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

;
; =============================================================================
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
; =============================================================================
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
height:       .res 1 ; player height
tilt:         .res 1 ; mode X tilt
tilt_last:    .res 1

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
nmi_cgwsel:   .res 1
nmi_cgadsub:  .res 1
nmi_bg2hofs:  .res 2
nmi_bg2vofs:  .res 2
nmi_tm:       .res 1

new_hdma_en:  .res 1 ; HDMA channel enable at next update
nmi_hdma_en:  .res 1 ; HDMA channel enable currently

oamp_i:       .res 2 ; OAM printing
oamp_x:       .res 1
oamp_y:       .res 1

; perspective
; inputs
pv_buffer:    .res 1 ; 0/1 selects double buffer
pv_l0:        .res 1 ; first scanline
pv_l1:        .res 1 ; last scanline + 1
pv_s0:        .res 2 ; horizontal texel distance at l0
pv_s1:        .res 2 ; horizontal texel distance at l1
pv_sh:        .res 2 ; vertical texel distance from l0 to l1, sh=0 to copy s0 scale for efficiency: (s0*(l1-l0)/256)
pv_interp:    .res 1 ; interpolate every X lines, 0,1=1x (no interpolation, 2=2x, 4=4x, other values invalid
pv_wrap:      .res 1 ; 0 if no wrapping, 1 if wrapping (does not affect PPU wrapping)
; temporaries
pv_zr:        .res 2 ; interpolated 1/Z
pv_zr_inc:    .res 2 ; zr increment per line
pv_sh_:       .res 2 ; =pv_sh, or if pv_sh=0 then computed value
pv_scale:     .res 4 ; 8-bit scale of a/b/c/d
pv_negate:    .res 1 ; negation of a/b/c/d
pv_interps:   .res 2 ; interpolate * 4 for stride increment

.segment "LORAM" ; <$2000

oam:          .res 512+32

.segment "HIRAM" ; $FE bank < $8000

new_hdma:     .res 16 * 8 ; HDMA channel settings to apply at next update
nmi_hdma:     .res 16 * 8 ; HDMA channel settings current

; HDMA double-buffer for perspective
.align 256 ; (alignment not needed but is easier to debug)
pv_hdma_ab0:  .res 1024 ; Mode 7 matrix AB
pv_hdma_cd0:  .res 1024 ; Mode 7 matrix CD
pv_hdma_bgm0: .res 16 ; background mode
pv_hdma_tm0:  .res 16 ; background enable
pv_hdma_abi0: .res 16 ; indirection for AB
pv_hdma_cdi0: .res 16 ; indirection for CD
pv_hdma_col0: .res 16 ; fixed colour for horizon fade (indirect)
.align 256 ; (not needed)
pv_hdma_ab1:  .res 1024
pv_hdma_cd1:  .res 1024
pv_hdma_bgm1: .res 16
pv_hdma_tm1:  .res 16
pv_hdma_abi1: .res 16
pv_hdma_cdi1: .res 16
pv_hdma_col1: .res 16
PV_HDMA_STRIDE = pv_hdma_ab1 - pv_hdma_ab0

.segment "RODATA" ; for data needed by code

.import __RODATA_LOAD__
.import __RODATA_SIZE__
.assert (^__RODATA_LOAD__) = (^(__RODATA_LOAD__ + __RODATA_SIZE__)), error, "RODATA cannot cross a bank"

BANK_RODATA = ^__RODATA_LOAD__

;
; =============================================================================
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
	lda #0
	phk ; K = 0
	plb ; DB = 0
	sta a:$4200 ; NMI off
	jml reset

;
; =============================================================================
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
; mode 1 sky chr, 8k aligned
.align $2000
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

;
; =============================================================================
; LOPRG stuff for the first bank (mapped at $C00000)
;

; used for all code in this demo program
.segment "LOPRG"

;
; =============================================================================
; NMI
;

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
		sta a:$2100 ; force blanking on
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
	sta a:$2105 ; BGMODE
	lda z:nmi_hofs+0
	sta a:$210D ; BG1HOFS / M7HOFS
	lda z:nmi_hofs+1
	sta a:$210D
	lda z:nmi_vofs+0
	sta a:$210E ; BG1VOFS / M7VOFS
	lda z:nmi_vofs+1
	sta a:$210E
	lda z:nmi_m7t+0
	sta a:$211B ; M7A
	lda z:nmi_m7t+1
	sta a:$211B
	lda z:nmi_m7t+2
	sta a:$211C ; M7B
	lda z:nmi_m7t+3
	sta a:$211C
	lda z:nmi_m7t+4
	sta a:$211D ; M7C
	lda z:nmi_m7t+5
	sta a:$211D
	lda z:nmi_m7t+6
	sta a:$211E ; M7D
	lda z:nmi_m7t+7
	sta a:$211E
	lda z:nmi_m7x+0
	sta a:$211F ; M7E
	lda z:nmi_m7x+1
	sta a:$211F
	lda z:nmi_m7y+0
	sta a:$2120 ; M7F
	lda z:nmi_m7y+1
	sta a:$2120
	lda z:nmi_cgwsel
	sta a:$2130 ; CGWSEL
	lda z:nmi_cgadsub
	sta a:$2131 ; CGADSUB
	lda z:nmi_bg2hofs+0
	sta a:$210F ; BG2HOFS
	lda z:nmi_bg2hofs+1
	sta a:$210F
	lda z:nmi_bg2vofs+0
	sta a:$2110 ; BG2VOFS
	lda z:nmi_bg2vofs+1
	sta a:$2110
	lda z:nmi_tm
	sta a:$212C ; TM
	; 3. OAM DMA
	stz a:$2102
	stz a:$2103
	lda #%00000010 ; increment, 1-regiter (x2)
	sta a:$4300
	lda #$04 ; $2104
	sta a:$4301
	lda #<oam
	sta a:$4302
	lda #>oam
	sta a:$4303
	lda #^oam
	sta a:$4304
	lda #<(512+32)
	sta a:$4305
	lda #>(512+32)
	sta a:$4306
	lda #$01
	sta a:$420B ; DMA
	; force blanking off
	lda #$0F
	sta a:$2100
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
	sta a:$420C
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

;
; =============================================================================
; Startup
;

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
	sta a:$2100
	stz a:$2101
	stz a:$2102
	stz a:$2103
	stz a:$2105
	stz a:$2106
	stz a:$2107
	stz a:$2108
	stz a:$2109
	stz a:$210A
	stz a:$210B
	stz a:$210C
	stz a:$210D
	stz a:$210D
	stz a:$210E
	stz a:$210E
	stz a:$210F
	stz a:$210F
	stz a:$2110
	stz a:$2110
	stz a:$2111
	stz a:$2111
	stz a:$2112
	stz a:$2112
	stz a:$2113
	stz a:$2113
	stz a:$2114
	stz a:$2114
	lda #$80
	sta a:$2115
	stz a:$2116
	stz a:$2117
	stz a:$211A
	stz a:$211B
	lda #$01
	sta a:$211B
	stz a:$211C
	stz a:$211C
	stz a:$211D
	stz a:$211D
	stz a:$211E
	lda #$01
	sta a:$211E
	stz a:$211F
	stz a:$211F
	stz a:$2120
	stz a:$2120
	stz a:$2121
	stz a:$2123
	stz a:$2124
	stz a:$2125
	stz a:$2126
	stz a:$2127
	stz a:$2128
	stz a:$2129
	stz a:$212A
	stz a:$212B
	stz a:$212C
	stz a:$212D
	stz a:$212E
	stz a:$212F
	lda #$30
	sta a:$2130
	stz a:$2131
	lda #$E0
	sta a:$2132
	stz a:$2133
	stz a:$4200
	lda #$FF
	sta a:$4201
	stz a:$4202
	stz a:$4203
	stz a:$4204
	stz a:$4205
	stz a:$4206
	stz a:$4207
	stz a:$4208
	stz a:$4209
	stz a:$420A
	stz a:$420B
	stz a:$420C
	;stz a:$420D ; SlowROM
	lda #1
	sta a:$420D ; FastROM
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
	sta a:$4300
	lda #$18 ; $2118 VMDATA
	sta a:$4301
	lda #<bin_vram
	sta a:$4302
	lda #>bin_vram
	sta a:$4303
	lda #^bin_vram
	sta a:$4304
	stz a:$4305
	stz a:$4306 ; 64k bytes
	lda #$01
	sta a:$420B ; DMA
	; load palettes
	stz a:$2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta a:$4300
	lda #$22 ; $2122 CGDATA
	sta a:$4301
	lda #<pal_bg
	sta a:$4302
	lda #>pal_bg
	sta a:$4303
	lda #^pal_bg
	sta a:$4304
	stz a:$4305
	lda #>256
	sta a:$4306 ; 256 bytes
	lda #$01
	sta a:$420B ; DMA
	lda #<pal_fg
	sta a:$4302
	lda #>pal_fg
	sta a:$4303
	lda #^pal_fg
	sta a:$4304
	stz a:$4305
	lda #>256
	sta a:$4306 ; 256 bytes
	lda #$01
	sta a:$420B ;DMA
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
	sta a:$2108 ; BG2SC nametable, 1-screen
	lda #(VRAM_CHR_SKY >> 12) << 4
	sta a:$210B ; BG12NBA
	lda #((VRAM_CHR_FG >> 13) | $20)
	sta a:$2101 ; OBJSEL 8x8 + 32x32 sprites
	lda #$11
	sta z:nmi_tm ; TM OBJ + BG1 main-screen
	stz a:$212D ; TS empty sub-screen
	; begin
	jmp run

;
; =============================================================================
; main loop
;

run:
	.a8
	.i8
	; enable NMI and auto-joypad
	lda a:$4210 ; RDNMI
	lda #$81
	sta a:$4200 ; NMITIMEN
	rep #$20
	sep #$10
	.a16
	.i8
	jsr oamp_start
	jmp @set_mode_y
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
		lda a:$4212 ; HBVJOY (+RDIO)
		and #1
		bne :-
	lda a:$4218 ; JOY1
	sta z:gamepad
	eor z:lastpad
	and z:gamepad
	sta z:newpad
	; pausing
	lda z:newpad
	and #$1000 ; start
	beq :+
		lda z:pause
		eor #1
		tax
		stx z:pause
	:
	; aspect ratio correction
	lda z:newpad
	and #$2000 ; select
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
; =============================================================================
; OAM sprites
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
	sta z:temp+4
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
	ora z:temp+5 ; high bit of tile
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
; =============================================================================
; OAM printing
;

; summary:
;  .a16/.i8 assumed for most functions
;
;  oamp_start - begin printing
;  oamp_finish - finish printing
;  oamp_return - carriage return
;  oamp_space - space
;  oamp_alpha ; A = ascii capital letter to print (a/i any)
;  oamp_alpha_space ; alpha + space
;  oamp_hex8 ; A = value to print in hex (a/i any)
;  oamp_hex16 ; A = 16-bit value to print in hex
;  oamp_hex16_space

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
; =============================================================================
; Math
;

; summary of available subroutines:
;   inputs:  math_a, math_b, A register
;   outputs: math_p, math_r, A register
;   values are unsigned (u16), signed (s16), either (16) or fixed point (8.8)
;   .a16 .i8 DB=0 assumed
;
; umul16:         u16 a *    u16 b =  u32 p                clobbers A/X/Y                 ~620 clocks
; smul16_u8:      s16 a *     u8 b =  s24 p (A=msw)        clobbers A/X/Y                 ~570 clocks
; smul16:         s16 a *    s16 b =   32 p                clobbers A/X/Y                 ~770 clocks
; mul16t:          16 a *     16 b =   16 A/p (truncated)  clobbers A/X/Y                 ~500 clocks
; smul16f:       s8.8 a *   s8.8 b = s8.8 A = s16.16 p     clobbers A/X/Y                 ~920 clocks
; smul32f_16f:  s24.8 a *   s8.8 b = s8.8 A =  s8.24 p     clobbers A/X/Y,a,b,r           ~2570 clocks (1.8 scanlines)
; smul32ft:     s24.8 a * s16.16 b = s8.8 A = s16.24 r     clobbers A/X/Y,a,b,p,temp0-13  ~3600 clocks (2.6 scanlines)
;
; udiv32:         u32 a /    u32 b =  u32 p    % u32 r     clobbers A/X (DB any)          ~12400 clocks (9 scanlines)
; sdiv32          s32 a /    s32 b =  s32 p    % u32 r     clobbers A/X/Y (DB any)        ~14000 clocks (10 scanlines)
; recip16f:           1 /   s8.8 A = s8.8 A = s16.16 p     clobbers A/X,a,b (DB any)      ~12450 clocks (9 scanlines)
;
; sign:         A = value, returns either 0 or $FFFF       preserves flags (.i8/.i16 allowed, DB any)
; sincos:       A = angle 0-255, result in cosa/sina       clobbers A/X (i8/.i16 allowed, DB any)

; unsigned 16-bit multiply, 32-bit result
; Written by 93143: https://forums.nesdev.org/viewtopic.php?p=280089#p280089
umul16: ; math_a x math_b = math_p, clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	ldx z:math_a+0
	stx a:$4202
	ldy z:math_b+0
	sty a:$4203       ; a0 x b0 (A)
	ldx z:math_b+1
	stz z:math_p+2
	lda a:$4216
	stx a:$4203       ; a0 x b1 (B)
	sta z:math_p+0    ; 00AA
	ldx z:math_a+1
	lda a:$4216
	stx a:$4202
	sty a:$4203       ; a1 x b0 (C)
	clc
	adc z:math_p+1    ; 00AA + 0BB0 (can't set carry because high byte was 0)
	ldy z:math_b+1
	adc a:$4216
	sty a:$4203       ; a1 x b1 (D)
	sta z:math_p+1    ; 00AA + 0BB0 + 0CC0
	lda z:math_p+2
	bcc :+
	adc #$00FF        ; if carry, increment top byte
:
	adc a:$4216
	sta z:math_p+2    ; 00AA + 0BB0 + 0CC0 + DD00
	rts

; signed 16-bit x 8-bit multiply, 24-bit result, returns high 16
smul16_u8: ; math_a x math_b = math_p, clobbers A/X/Y
	; DB = 0
	.a16
	.i8
	ldx z:math_b
	stx a:$4202
	ldy z:math_a+0
	sty a:$4203       ; b x a0 (A)
	ldx z:math_a+1
	stz z:math_p+2
	lda a:$4216
	stx a:$4203       ; b x a1 (B)
	sta z:math_p+0    ; 0AA
	lda z:math_p+1
	clc
	adc a:$4216       ; 0AA + BB0
	sta z:math_p+1
	cpx #$80
	bcc :+ ; if sign bit, must mutiply b by sign extend
		ldx #$FF
		stx a:4203
		clc
		lda z:math_p+2
		adc a:$4216   ; 0AA + BB0 + C00
		sta z:math_p+2
		lda z:math_p+1
	:
	rts

; signed 16-bit multiply, 32-bit result, clobbers A/X/Y
smul16:
	; DB = 0
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

; 16-bit multiply, truncated 16-bit result (sign-agnostic), clobbers A/X/Y
mul16t:
	; DB = 0
	.a16
	.i8
	ldx z:math_a+0
	stx a:$4202
	ldy z:math_b+0
	sty a:$4203       ; a0 x b0 (A)
	ldx z:math_b+1
	nop
	lda a:$4216
	stx a:$4203       ; a0 x b1 (B)
	sta z:math_p+0    ; AA
	ldx z:math_a+1
	lda a:$4216
	stx a:$4202
	sty a:$4203       ; a1 x b0 (C)
	clc
	adc z:math_p+1    ; AA + B0
	clc
	adc a:$4216       ; AA + B0 + C0
	tax
	stx z:math_p+1
	lda z:math_p+0
	rts

smul16f: ; smul16 but returning the middle 16-bit value as A (i.e. 8.8 fixed point multiply)
	; DB = 0
	.a16
	.i8
	jsr smul16
	lda z:math_p+1
	rts

smul32f_16f: ; a = 24.8 fixed, b = 8.8 fixed, result in A = 8.8, clobbers: math_a/math_b/math_r
	; DB = 0
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

smul32ft: ; a = 24.8 fixed, b = 16.16 fixed, 16.24 result in math_r, returns 8.8 in A, clobbers math_a/b/p, temp0-13
	; DB = 0
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
	stx a:$4202
	ldx z:temp+12
	stx a:$4203 ; a0 x b2 (D)
	ldx z:temp+2
	clc
	adc a:$4216      ; 0AAAA + BBB00 + CCC00 + D0000
	stx a:$4202
	ldx z:temp+10
	stx a:$4203 ; a1 x b1 (E)
	ldx z:temp+4
	clc
	adc a:$4216     ; 0AAAA + BBB00 + CCC00 + D0000 + E0000
	stx a:$4202
	ldx z:temp+8
	stx a:$4203 ; a2 x b0 (F)
	nop
	nop
	clc
	adc a:$4216     ; 0AAAA + BBB00 + CCC00 + D0000 + E0000 + F0000
	sta z:math_r+4
	lda z:math_r+3 ; return top 16 bits
	rts

; 32-bit / 32-bit division, 32 + 32 result
; math_a / math_b = math_p
; math_a % math_b = math_r
; clobbers A/X
udiv32:
	; DB = any
	.a16
	.i8
	lda z:math_a+0
	asl
	sta z:math_p+0
	lda z:math_a+2
	rol
	sta z:math_p+2
	stz z:math_r+2 ; A is used temporarily as low word of r
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
	; Optimization notes:
	;   This routine is very simple and very slow.
	;   We try to do it only a few times per frame, but it's still pretty hefty.
	;   There is likely a way to decompose the operation to use the 16/8 hardware divider,
	;   but I have not yet discovered one.

sdiv32: ; 32-bit/32-bit signed division, 32+32 result, math_a / math_b = math_p & math_r, clobbers A/X/Y
	ldy #0 ; y=1 marks an inverted result
	lda z:math_a+2
	bpl :+
		iny
		lda z:math_a+0
		eor #$FFFF
		clc
		adc #1
		sta z:math_a+0
		lda z:math_a+2
		eor #$FFFF
		adc #0
		sta z:math_a+2
	:
	lda z:math_b+2
	bpl :+
		iny
		lda z:math_b+0
		eor #$FFFF
		clc
		adc #1
		sta z:math_b+0
		lda z:math_b+2
		eor #$FFFF
		adc #0
		sta z:math_b+2
	:
	jsr udiv32
	cpy #1
	bne :+
		lda z:math_p+0
		eor #$FFFF
		clc
		adc #1
		sta z:math_p+0
		lda z:math_p+2
		eor #$FFFF
		adc #0
		sta z:math_b+2
	:
	rts

; fixed point reciprocal, clobbers A/X/Y/math_a/math_b/math_p/math_r
recip16f: ; A = fixed point number, result in A
	; DB = any
	sta z:math_b+0
	stz z:math_b+2
	cmp #$8000
	bcs :+
		ldy #0
		bra :++
	:
		lda #$FFFF
		eor z:math_b+0
		inc
		sta z:math_b+0
		ldy #1 ; Y indicates negate at end
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
	; Optimization notes:
	;   See udiv32 notes above.
	;   If not using the p+0 byte, this could be done a little faster with a udiv24,
	;   but this demo needs p+0 for all uses.

sign: ; A = value, returns either 0 or $FFFF, preserves flags
	.a16
	;.i any
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
	;.i any
	php
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
	plp
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
; =============================================================================
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
; Screen to texel (official formula):
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
; In general in these examples calculating texel_to_screen (and the reciprocal determinant det_r) are time-intensive operations.
; The provided texel_to_screen is very generic, and with precision good enough for most purposes.
; In most practical cases these can be simplified or avoided. For example:
;  - Scale of 1 means det_r=1 and can be skipped entirely.
;  - Fixed or limited scaling could have a constant det_r, or looked up from a small table.
;  - If Px,Py is always mid-screen, Tx-Px,Ty-Py will be low for on-screen sprites.
;    With appropriate culling of distant objects, texel_to_scale can be done at lower precision.
;  - ABCD could be replaced by versions pre-scaled with 1/(AD-BC), avoiding its separate application.
;  - Object positions might be stored in an alternative way (camera-relative, polar coordinates, etc.) that does not need as much transformation.
;

; 1 for higher precision determinant reciprocal (more accurate under scaling)
;   adds about 10 more hardware multiplies to texel_to_screen (11 vs 9 scanlines?)
DETR40 = 1

; recalculate det_r = 1 / (AD-BC) = 1 / (Mx * My)
; used by texel_to_screen
calc_det_r:
	lda z:scale2+0 ; Mx 8.8f
	sta z:math_a
	lda z:scale2+2 ; My 8.8f
	sta z:math_b
	jsr smul16f ; Mx * My 8.8f
	jsr recip16f
	lda z:math_p+0 ; 1 / (Mx * My) 16.16f
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
		lda #(256*8/7)
		sta z:math_a
		lda z:scale2+0
		sta z:math_b
		jsr smul16f
		sta z:scale2+0 ; Mx *= 8/7
	:
	rts

;
; =============================================================================
; Perspective View
;

; Configuration and performance:
;   There are 3 versions of the perspective generator which take increasing CPU time:
;     1. angle=0:  No rotation of the view.
;     2. pv_sh=0:  Vertical view scale is locked to horizontal view scale.
;     3. pv_sh!=0: Full calculation, vertical view scale is independent of horizontal.
;   Interpolation can be applied on every 2nd, or 3 of 4 lines:
;     1. pv_interp=0/1: no interpolation:      ~1.2 scanlines per line, ~0.9 if sh=0, ~0.7 if angle=0
;     2. pv_interp=2:   interpolate odd lines, ~0.9 scanlines per line, ~0.7 if sh=0, ~0.6 if angle=0
;     3. pv_interp=4:   interpolate 3/4 lines, ~0.7 scanlines per line, ~0.6 if sh=0, ~0.6 if angle=0
;
; Interpolation can be used to reduce CPU time. 2x tends to look fine,
; though 4x starts to amplify precision errors a little bit unpleasantly
; (i.e. rippling across specific scanlines), especially near the bottom.
;
; With sh=0, it will behave as if sh = (s0 * (l1-l0)) / 256
; This means if s0 has N texels per pixel horizontally, sh will have N texels per pixel vertically.
; The resulting view usually has a fairly natural looking scal. A square appears square, locally speaking.
; Many games without DSP (e.g. F-Zero, Final Fantasy VI) accepted this compromise,
; trading independent vertical scale away for faster computation.

; indirect TM tables to swap BG1 and BG2 (both with OBJ)
pv_tm1: .byte $11
pv_tm2: .byte $12

; indirect COLDATA tables for fade
pv_fade_table0: ; black for top of screen
.byte $E0
pv_fade_table1:
.byte $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7 ; 16 lines at bottom of sky
.byte $FC, $FA, $F7, $F4, $F1, $EE, $EC, $EA, $E8, $E6, $E5, $E4, $E3, $E2, $E1, $E0 ; 16 lines at top of ground

pv_ztable: ; 12 bit (1<<15)/z lookup
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; 0000
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; 0020
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; 0040
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; 0060
.byte $FF,$FE,$FC,$FA,$F8,$F6,$F5,$F3,$F1,$EF,$ED,$EC,$EA,$E8,$E7,$E5,$E4,$E2,$E0,$DF,$DD,$DC,$DA,$D9,$D8,$D6,$D5,$D3,$D2,$D1,$CF,$CE ; 0080
.byte $CD,$CC,$CA,$C9,$C8,$C7,$C5,$C4,$C3,$C2,$C1,$C0,$BF,$BD,$BC,$BB,$BA,$B9,$B8,$B7,$B6,$B5,$B4,$B3,$B2,$B1,$B0,$AF,$AE,$AD,$AC,$AC ; 00A0
.byte $AB,$AA,$A9,$A8,$A7,$A6,$A5,$A5,$A4,$A3,$A2,$A1,$A1,$A0,$9F,$9E,$9E,$9D,$9C,$9B,$9B,$9A,$99,$98,$98,$97,$96,$96,$95,$94,$94,$93 ; 00C0
.byte $92,$92,$91,$90,$90,$8F,$8E,$8E,$8D,$8D,$8C,$8B,$8B,$8A,$8A,$89,$89,$88,$87,$87,$86,$86,$85,$85,$84,$84,$83,$83,$82,$82,$81,$81 ; 00E0
.byte $80,$80,$7F,$7F,$7E,$7E,$7D,$7D,$7C,$7C,$7B,$7B,$7A,$7A,$79,$79,$78,$78,$78,$77,$77,$76,$76,$75,$75,$75,$74,$74,$73,$73,$73,$72 ; 0100
.byte $72,$71,$71,$71,$70,$70,$6F,$6F,$6F,$6E,$6E,$6E,$6D,$6D,$6D,$6C,$6C,$6B,$6B,$6B,$6A,$6A,$6A,$69,$69,$69,$68,$68,$68,$67,$67,$67 ; 0120
.byte $66,$66,$66,$65,$65,$65,$65,$64,$64,$64,$63,$63,$63,$62,$62,$62,$62,$61,$61,$61,$60,$60,$60,$60,$5F,$5F,$5F,$5E,$5E,$5E,$5E,$5D ; 0140
.byte $5D,$5D,$5D,$5C,$5C,$5C,$5C,$5B,$5B,$5B,$5B,$5A,$5A,$5A,$5A,$59,$59,$59,$59,$58,$58,$58,$58,$57,$57,$57,$57,$56,$56,$56,$56,$56 ; 0160
.byte $55,$55,$55,$55,$54,$54,$54,$54,$54,$53,$53,$53,$53,$53,$52,$52,$52,$52,$52,$51,$51,$51,$51,$51,$50,$50,$50,$50,$50,$4F,$4F,$4F ; 0180
.byte $4F,$4F,$4E,$4E,$4E,$4E,$4E,$4D,$4D,$4D,$4D,$4D,$4D,$4C,$4C,$4C,$4C,$4C,$4C,$4B,$4B,$4B,$4B,$4B,$4A,$4A,$4A,$4A,$4A,$4A,$49,$49 ; 01A0
.byte $49,$49,$49,$49,$48,$48,$48,$48,$48,$48,$48,$47,$47,$47,$47,$47,$47,$46,$46,$46,$46,$46,$46,$46,$45,$45,$45,$45,$45,$45,$45,$44 ; 01C0
.byte $44,$44,$44,$44,$44,$44,$43,$43,$43,$43,$43,$43,$43,$42,$42,$42,$42,$42,$42,$42,$42,$41,$41,$41,$41,$41,$41,$41,$41,$40,$40,$40 ; 01E0
.byte $40,$40,$40,$40,$40,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3C,$3C ; 0200
.byte $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$39,$39,$39,$39,$39,$39 ; 0220
.byte $39,$39,$39,$39,$38,$38,$38,$38,$38,$38,$38,$38,$38,$38,$38,$37,$37,$37,$37,$37,$37,$37,$37,$37,$37,$37,$36,$36,$36,$36,$36,$36 ; 0240
.byte $36,$36,$36,$36,$36,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$34,$34,$34,$34,$34,$34,$34,$34,$34,$34,$34,$34,$33,$33,$33 ; 0260
.byte $33,$33,$33,$33,$33,$33,$33,$33,$33,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ; 0280
.byte $31,$31,$31,$31,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F ; 02A0
.byte $2F,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D,$2D ; 02C0
.byte $2D,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B ; 02E0
.byte $2B,$2B,$2B,$2B,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$2A,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29 ; 0300
.byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$27,$27 ; 0320
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26 ; 0340
.byte $26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25 ; 0360
.byte $25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$23,$23,$23,$23 ; 0380
.byte $23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$23,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22 ; 03A0
.byte $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21 ; 03C0
.byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; 03E0
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F ; 0400
.byte $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E ; 0420
.byte $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D ; 0440
.byte $1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1C,$1C ; 0460
.byte $1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C ; 0480
.byte $1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B ; 04A0
.byte $1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A ; 04C0
.byte $1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A ; 04E0
.byte $1A,$1A,$1A,$1A,$1A,$1A,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19 ; 0500
.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$18,$18,$18,$18,$18,$18 ; 0520
.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18 ; 0540
.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17 ; 0560
.byte $17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17 ; 0580
.byte $17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16 ; 05A0
.byte $16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16 ; 05C0
.byte $16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15 ; 05E0
.byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15 ; 0600
.byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$14 ; 0620
.byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14 ; 0640
.byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14 ; 0660
.byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13 ; 0680
.byte $13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13 ; 06A0
.byte $13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13 ; 06C0
.byte $13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12 ; 06E0
.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12 ; 0700
.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12 ; 0720
.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11 ; 0740
.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11 ; 0760
.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11 ; 0780
.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11 ; 07A0
.byte $11,$11,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10 ; 07C0
.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10 ; 07E0
.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10 ; 0800
.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10 ; 0820
.byte $10,$10,$10,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F ; 0840
.byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F ; 0860
.byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F ; 0880
.byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F ; 08A0
.byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E ; 08C0
.byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E ; 08E0
.byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E ; 0900
.byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E ; 0920
.byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E ; 0940
.byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0D,$0D,$0D,$0D ; 0960
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D ; 0980
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D ; 09A0
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D ; 09C0
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D ; 09E0
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D ; 0A00
.byte $0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0C,$0C ; 0A20
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0A40
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0A60
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0A80
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0AA0
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0AC0
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0AE0
.byte $0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C ; 0B00
.byte $0C,$0C,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0B20
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0B40
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0B60
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0B80
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0BA0
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0BC0
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0BE0
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B ; 0C00
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0C20
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0C40
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0C60
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0C80
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0CA0
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0CC0
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0CE0
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0D00
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0D20
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A ; 0D40
.byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$09,$09,$09,$09,$09,$09 ; 0D60
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0D80
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0DA0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0DC0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0DE0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0E00
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0E20
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0E40
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0E60
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0E80
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0EA0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0EC0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09 ; 0EE0
.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0F00
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0F20
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0F40
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0F60
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0F80
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0FA0
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0FC0
.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08 ; 0FE0
; This table is 12-bit (4kb) but could be made smaller at a precision tradeoff.
; An 8-bit implementation was tried using the hardware div: (1<<11) / (zr>>8))
; This looked too imprecise and there was excessive "rippling", especially in the distance.
; I think as low as 10-bits (1kb) is still fairly good, and beyond 12-bit didn't seem to offer any useful additional precision.
; If using a smaller table, compensate by adding lsr(s) to the zr shift in @abcd_pv_line.
;
; python generator:
;def ztable(bits,width=32):
;    s = "pv_ztable: ; %d bit (1<<%d)/z lookup" % (bits,3+bits)
;    for i in range(0,1<<(bits)):
;        v = 0xFF
;        if i > 0:
;            v = min(0xFF,round((1 << (3+bits)) / i))
;        if (i % width) == 0:
;            s += "\n.byte "
;        s += "$%02X" % v
;        if (i % width) != width-1:
;            s += ","
;        else:
;            s += " ; %04X" % (i+1-width)
;    print(s)

pv_buffer_x: ; sets X to 0 or PV_HDMA_STRIDE to select the needed buffer
	.a8
	.i16
	lda z:pv_buffer
	beq :+
		ldx #PV_HDMA_STRIDE
		rts
	:
		ldx #0
		rts
	;

; rebuild the perspective HDMA tables (only needed if pv input variables or angle change, moving the origin only does not require a rebuild)
pv_rebuild:
	php
	phb
	sep #$20
	rep #$10
	.a8
	.i16
	lda #$7E
	pha
	plb
	; 1. flip the double buffer
	; =========================
	lda z:pv_buffer
	eor #1
	sta z:pv_buffer
	; 2. calculate BG mode table + TM table (pv_hdma_bgm, pv_hdma_tm)
	; ========================================
	jsr pv_buffer_x
	stx z:temp
	ldy z:temp ; X = Y = pv_buffer_x
	lda z:pv_l0
	beq @bgm_end
	dec ; need to set on scanline before L0
	beq @bgm_end
	sta z:temp
	@bgm_mode: ; use nmi_bgmode until L0
		cmp #128
		bcc :+
			lda #128
		:
		sta a:pv_hdma_bgm0+0, X
		sta a:pv_hdma_tm0+0, Y
		eor #$FF
		sec
		adc z:temp
		sta z:temp
		lda z:nmi_bgmode
		sta a:pv_hdma_bgm0+1, X
		lda #<pv_tm2
		sta a:pv_hdma_tm0+1, Y
		lda #>pv_tm2
		sta a:pv_hdma_tm0+2, Y
		inx
		inx
		iny
		iny
		iny
		lda z:temp
		bne @bgm_mode
	@bgm_end:
	; set mode 7 at L0
	lda #1
	sta a:pv_hdma_bgm0+0, X
	lda #7
	sta a:pv_hdma_bgm0+1, X
	stz a:pv_hdma_bgm0+2, X ; end of table
	; set BG2 at L0
	lda #1
	sta a:pv_hdma_tm0+0, Y
	lda #<pv_tm1
	sta a:pv_hdma_tm0+1, Y
	lda #>pv_tm1
	sta a:pv_hdma_tm0+2, Y
	lda #0
	sta a:pv_hdma_tm0+3, Y ; end of table
	; 3. calculate TM table and color fade (indirect tables: pv_hdma_tm, pv_hdma_col)
	; =====================================================
	jsr pv_buffer_x
	lda z:pv_l0
	sec
	sbc #(16+1)
	sta z:temp+0
	stz z:temp+1
	@col_fade: ; black until L0-16
		cmp #128
		bcc :+
			lda #128
		:
		sta a:pv_hdma_col0+0, X
		eor #$FF
		sec
		adc z:temp+0
		sta z:temp+0
		lda #<pv_fade_table0
		sta a:pv_hdma_col0+1, X
		lda #>pv_fade_table0
		sta a:pv_hdma_col0+2, X
		inx
		inx
		inx
		lda z:temp+0
		bne @col_fade
	; 32 lines from fade table
	lda #$80 | 32
	sta a:pv_hdma_col0+0, X
	lda #<pv_fade_table1
	sta a:pv_hdma_col0+1, X
	lda #>pv_fade_table1
	sta a:pv_hdma_col0+2, X
	stz a:pv_hdma_col0+3, X ; end of table
	; 4. calculate ABCD
	; =================
	; overview of calculation:
	;   Interpolating from S0 to S1 texel scales, with perspective correction,
	;   meaning that for Z proportional to S0/S1, 1/Z interpolates linearly down the screen.
	;   So: 1/Z is interpolated, and used to recover the corrected Z.
	;   Finally, it's multiplied by the rotation matrix, and the vertical values receive a relative scaling.
	;   Various shifts are applied to make the fixed point precision practical.
	;   Acceptable ranges are set by the fixed point precision. These could be adjusted to trade precision for more/less range:
	;   - S0/S1 should be <1024: 2.6 precision goes from 0 to 4x-1 scale
	;   - SH scale (SA) should be less than <2x S0 scale: 1.7 precision goes from 0 to 2x-1 relative scale.
	;     If SH=0 then SA will be forced to 1, allowing more efficient computation but SH will automatically have the same texel scale as S0.
	;   - L0<L1, L1<254 (L1 should probably always be 224)
	;
	; setup:
	;   ZR0 = (1<<21)/S0              ; 11.21 / 8.8 (S0) = 19.13, truncated to 3.13
	;   ZR1 = (1<<21)/S1
	;   SA = (256 * SH) / (S0 * (L1 - L0)) ; pre-combined with rotation cos/sin at 1.7
	; per line:
	;   zr = >lerp(ZR0,ZR1)>>4        ; 3.9 (truncated from 3.13)
	;   z = <((1<<15)/zr)             ; 1.15 / 3.9 = 10.6, clamped to 2.6
	;   a = z *  cos(angle)      >> 5 ; 2.6 * 1.7 (cos>>1)    = 3.13 >> 5 = 8.8
	;   b = z *  sin(angle) * SA >> 5 ; 2.6 * 1.7 (SA*sin>>1) = 3.13 >> 5 = 8.8
	;   c = z * -sin(angle)      >> 5
	;   d = z *  cos(angle) * SA >> 5
	;
	; Setup
	; -----
	rep #$20
	sep #$10
	.a16
	.i8
	phb
	ldy #0
	phy
	plb ; data bank 0 for hardware multiply/divide access
	; calculate ZR0 = (1<<21)/S0
	lda #(1<<21)>>16
	stz z:math_a+0
	sta a:math_a+2
	lda z:pv_s0
	sta a:math_b+0
	stz a:math_b+2
	jsr udiv32
	; result should fit in 16-bits, clamp to 0001-FFFF
	lda a:math_p+2
	beq :+
		lda #$FFFF
		sta a:math_p+0
	:
	lda a:math_p+0
	bne :+
		inc
	:
	sta z:pv_zr
	; calculate ZR1 = (1<<21)/S1
	lda z:pv_s1
	sta a:math_b+0
	jsr udiv32
	lda a:math_p+2
	beq :+
		lda #$FFFF
		sta a:math_p+0
	:
	lda a:math_p+0
	bne :+
		inc
	:
	; calculate (ZR1 - ZR0) / (L1 - L0) for interpolation increment
	ldy #0
	sec
	sbc z:pv_zr
	bcs :+
		eor #$FFFF
		inc
		iny
	: ; Y = negate
	sta f:$004204 ; WRDIVH:WRDIVL = abs(ZR1 - ZR0)
	sep #$20
	.a8
	lda z:pv_interp
	cmp #2
	beq :+
	cmp #4
	beq :+
		lda #1 ; otherwise default to 1
	:
	asl
	asl
	sta z:pv_interps+0 ; interps = interp * 4 (stride for increment)
	stz z:pv_interps+1 ; high byte zero for use with 16-bit registers
	lda z:pv_l1
	sec
	sbc z:pv_l0
	sta f:$004206 ; WRDIVB = (L1 - L0), result in 12 cycles + far load
	sta z:temp+0 ; temp+0 = L1-L0 = scanline count
	ldx z:pv_interp
	cpx #4
	bne :+
		lsr
		lsr
		bra :++
	:
	cpx #2
	bne :+
		lsr
	:
	inc
	sta z:temp+1 ; temp+1 = (L1-L0 / INTERPOLATE)+1 = un-interpolated scanline count + 1
	rep #$20
	.a16
	; result is ready
	lda f:$004214 ; RDDIVH:RDDIVL = abs(ZR1 - ZR0) / (L1 - L0)
	ldx z:pv_interp
	cpx #4
	bne :+
		asl
		asl
		bra :++
	:
	cpx #2
	bne :+
		asl
	:
	cpy #0
	beq :+ ; negate if needed
		eor #$FFFF
		inc
	:
	sta z:pv_zr_inc ; per-line increment * interpolation factor
	; calculate SA
	lda z:pv_s0
	sta z:math_a
	lda #0
	ldx z:temp+0 ; L1-L0 = scanline count
	txa
	sta z:math_b
	jsr umul16
	lda z:math_p+0
	sta z:math_b+0
	lda z:math_p+2
	sta z:math_b+2 ; b = S0 * (L1-L0)
	stz z:math_a+0
	lda z:pv_sh
	beq :+
		sta z:pv_sh_
		sta z:math_a+2 ; a = (SH * 256) << 8
		jsr udiv32
		lda z:math_p+0
		bra :++
	:
		lda #1<<8 ; SH = 0 means: SA = 1
		lda z:math_b+1
		sta z:pv_sh_ ; computed SH = (S0 * (L1-L0)) / 256
	:
	sta z:math_a ; SA = (SH * 256) / (S0 * (L1-L0))
	; fetch sincos for rotation matrix
	lda #0
	ldx z:angle
	txa
	jsr sincos
	; store m7t matrix (replaced by HDMA but used for other purposes like pv_texel_to_screen, and player moevement)
	lda z:cosa
	sta z:nmi_m7t+0 ; A = cos
	sta z:nmi_m7t+6 ; D = cos
	lda z:sina
	sta z:nmi_m7t+2 ; B = sin
	eor #$FFFF
	inc
	sta z:nmi_m7t+4 ; C = -sin
	; check for negations, take abs of cosa/sina
	; want: cos sin -sin cos, keep track of flips to recover from abs by negating afterwards
	ldx #0
	lda z:cosa
	bpl :+
		eor #$FFFF
		inc
		sta z:cosa
		ldx #%1001
	:
	stx z:pv_negate
	lda z:sina
	bmi :+
		lda #%0100
		bra :++
	:
		eor #$FFFF
		inc
		sta z:sina
		lda #%0010
	:
	eor z:pv_negate
	tax
	stx z:pv_negate
	; generate scale (convert 8.8 cosa/sina to 1.7, prescale vertical by SA)
	lda cosa
	sta z:math_b
	lsr
	tax
	stx z:pv_scale+0 ; scale A = cos / 2
	lda z:pv_sh
	beq :++
		jsr umul16
		lda z:math_p+1
		lsr
		cmp #$0100
		bcc :+ ; clamp at $FF
			lda #$00FF
		:
		tax
	:
	stx z:pv_scale+3 ; scale D = SA * cos / 2
	lda sina
	sta z:math_b
	lsr
	tax
	stx z:pv_scale+2 ; scale C = sin / 2
	lda z:pv_sh
	beq :++
		jsr umul16
		lda z:math_p+1
		lsr
		cmp #$0100
		bcc :+ ; clamp at $FF
			lda #$00FF
		:
		tax
	:
	stx z:pv_scale+1 ; scale B = SA * sin / 2
	plb ; return to RAM data bank
	; generate HDMA indirection buffers
	; ---------------------------------
	sep #$20
	rep #$10
	.a8
	.i16
	jsr pv_buffer_x
	stx z:temp+4 ; pv_buffer_x
	stx z:temp+6 ; pv_buffer_x
	rep #$20
	.a16
	lda z:pv_l0
	and #$00FF
	beq @abcdi_head_end
	dec ;set on scanline before L0
	beq @abcdi_head_end
	sta z:temp+2 ; L0 
	@abcdi_head:
		cmp #128
		bcc :+
			lda #128
		:
		sta a:pv_hdma_abi0+0, X
		sta a:pv_hdma_cdi0+0, X
		eor #$FF
		sec
		adc z:temp+2
		and #$00FF
		sta z:temp+2
		; these could be skipped, as the values won't display anyway
		lda #.loword(pv_hdma_ab0)
		clc
		adc z:temp+4
		sta a:pv_hdma_abi0+1, X
		lda #.loword(pv_hdma_cd0)
		clc
		adc z:temp+4
		sta a:pv_hdma_cdi0+1, X
		; next group
		inx
		inx
		inx
		lda z:temp+2
		bne @abcdi_head
	@abcdi_head_end:
	lda z:temp+0 ; L1-L0 (scanline count)
	and #$00FF
	sta z:temp+2
	@abcdi_body:
		cmp #127 ; repeat mode maxes at 127
		bcc :+
			lda #127
		:
		eor #$80 ; repeat mode
		sta a:pv_hdma_abi0+0, X
		sta a:pv_hdma_cdi0+0, X
		eor #$7F
		sec
		adc z:temp+2
		and #$00FF
		sta z:temp+2
		lda #.loword(pv_hdma_ab0)
		clc
		adc z:temp+4
		sta a:pv_hdma_abi0+1, X
		lda #.loword(pv_hdma_cd0)
		clc
		adc z:temp+4
		sta a:pv_hdma_cdi0+1, X
		inx
		inx
		inx
		lda z:temp+2
		beq @abcdi_body_end
		lda z:temp+4 ; if split 127 + rest, add the offset
		clc
		adc #(127*4)
		sta z:temp+4
		lda z:temp+2
		bra @abcdi_body
	@abcdi_body_end:
	stz a:pv_hdma_abi0+0, X ; end of table
	stz a:pv_hdma_cdi0+0, X
	; Generate scanlines with perspective correction
	; ---------------------------------------------------
	.a16
	.i16
	phb
	sep #$10
	.i8
	ldx #0
	phx
	plb ; DB = 0 for absolute writes to hardware
	; reuse math result ZP as long pointers
	ldx #$7E
	stx z:math_a+2
	stx z:math_b+2
	stx z:math_p+2
	stx z:math_r+2
	lda #.loword(pv_hdma_ab0+0)
	sta z:math_a+0
	lda #.loword(pv_hdma_ab0+2)
	sta z:math_b+0
	lda #.loword(pv_hdma_cd0+0)
	sta z:math_p+0
	lda #.loword(pv_hdma_cd0+2)
	sta z:math_r+0
	rep #$10
	.i16
	; temp+1 =  un-interpolated scanline count
	; temp+6/7 = pv_buffer_x
	ldy z:temp+6
	lda z:temp+1
	and #$00FF
	sta z:temp+2 ; temp+2/3 = countdown
	lda z:pv_negate
	and #$000F
	sta z:temp+4 ; temp+4/5 = negate
	; choose 1 of 3 variations:
	ldx z:pv_scale+1
	bne :+
		lda z:pv_negate
		and #%1001
		bne :+
		; if b=0 (assume c=0) angle is either 0, 180
		; if a/d are not negated, angle=0
		jsr pv_abcd_lines_angle0_ ; only calculates a/d, fills 
		bra :+++
	:
	sep #$20
	.a8
	txa
	cmp z:pv_scale+2
	bne :+
		lda z:pv_scale+0
		cmp z:pv_scale+3
		bne :+
		; if b==c and a==d then SA=1
		rep #$20
		.a16
		jsr pv_abcd_lines_sa1_
		bra :++
	:
		rep #$20
		.a16
		jsr pv_abcd_lines_full_
	:
	plb ; DB = $7E
	; Generate linear interpolation, apply negation
	; ----------------------------------------------------------------
	; ~750 clocks per line
	.a16
	.i16
	lda z:pv_interps
	cmp #(2*4)
	bcc @interpolate_end
		ldx z:temp+6 ; pv_buffer_x
		lda z:temp+1 ; un-interpolated scanline count
		and #$0FF
		beq @interpolate_end
		dec
		beq @interpolate_end
		sta z:temp+2 ; temp+2/3 = countdown
		lda z:pv_interps
		cmp #(4*4)
		beq :+
			jsr pv_interpolate_2x_
			bra :++
		:
			jsr pv_interpolate_4x_
		:
	@interpolate_end:
	sep #$20
	rep #$10
	.a8
	.i16
	; 5. set HDMA tables for next frame
	; =================================
	lda #$1F
	sta z:new_hdma_en ; enable HDMA (0,1,2,3,4)
	stz a:new_hdma+(0*16)+0 ; bgm: 1 byte transfer
	lda #$40
	sta a:new_hdma+(1*16)+0 ; tm: 1 byte transfer, indirect
	lda #$43
	sta a:new_hdma+(2*16)+0 ; AB: 4 byte transfer, indirect
	sta a:new_hdma+(3*16)+0 ; CD: 4 byte transfer, indirect
	lda #$40
	sta a:new_hdma+(4*16)+0 ; col: 1 byte transfer, indirect
	lda #$05
	sta a:new_hdma+(0*16)+1 ; bgm: $2105 BGMODE
	lda #$2C
	sta a:new_hdma+(1*16)+1 ; tm: $212C TM
	lda #$1B
	sta a:new_hdma+(2*16)+1 ; AB: $211B M7A
	lda #$1D
	sta a:new_hdma+(3*16)+1 ; CD: $211D M7C
	lda #$32
	sta a:new_hdma+(4*16)+1 ; col: $3132 COLDATA
	lda #$7E
	sta a:new_hdma+(0*16)+4 ; bank
	sta a:new_hdma+(1*16)+4
	sta a:new_hdma+(2*16)+4
	sta a:new_hdma+(3*16)+4
	sta a:new_hdma+(4*16)+4
	lda #^pv_tm1
	sta a:new_hdma+(1*16)+7 ; indirect bank
	lda #$7E
	sta a:new_hdma+(2*16)+7
	sta a:new_hdma+(3*16)+7
	lda #^pv_fade_table0
	sta a:new_hdma+(4*16)+7
	jsr pv_buffer_x
	stx z:temp
	rep #$20
	.a16
	lda #.loword(pv_hdma_bgm0)
	clc
	adc z:temp
	sta a:new_hdma+(0*16)+2
	lda #.loword(pv_hdma_tm0)
	clc
	adc z:temp
	sta a:new_hdma+(1*16)+2
	lda #.loword(pv_hdma_abi0)
	clc
	adc z:temp
	sta a:new_hdma+(2*16)+2
	lda #.loword(pv_hdma_cdi0)
	clc
	adc z:temp
	sta a:new_hdma+(3*16)+2
	lda #.loword(pv_hdma_col0)
	clc
	adc z:temp
	sta a:new_hdma+(4*16)+2
	; restore register sizes, data bank, and return
	plb
	plp
	rts

pv_abcd_lines_full_: ; full perspective with independent horizontal/vertical scale: ~1670 clocks per line
	; temp+2/3 = un-interpolated scanline count
	; temp+4/5 = pv_negate
	; Y = pv_buffer_x
	.a16
	.i16
	; perspective divide: lerp(zr) ; z = (1<15)/(zr>>4)
	; using a table wider than 8 bits instead of the hardware 16/8 divide allows a more precise result
	lda z:pv_zr
	lsr
	lsr
	lsr
	lsr
	tax ; X = 12-bit zr for ztable lookup
	lda f:pv_ztable, X
	sta a:$4202 ; WRMPYA = z (spurious write to $4303)
	; scale a
	ldx z:pv_scale+0
	stx a:$4203 ; WRMPYB = scale a (spurious write to $4304)
		; while waiting for the result: lerp(zr)
		lda z:pv_zr
		clc
		adc z:pv_zr_inc
		sta z:pv_zr ; zr += linear interpolation increment for next line
	lda a:$4216 ; RDMPYH:RDMPYL = z * a
	lsr
	lsr
	lsr
	lsr
	lsr
	; scale b
	ldx z:pv_scale+1
	stx a:$4203
		; negate and store a while waiting
		lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
		sta [math_a], Y ; pv_hdma_ab0+0
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	; scale c
	ldx z:pv_scale+2
	stx a:$4203
		; store b
		lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
		sta [math_b], Y ; pv_hdma_ab0+2
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	; scale d
	ldx z:pv_scale+3
	stx a:$4203
		; store c
		lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
		sta [math_p], Y ; pv_hdma_cd0+0
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr z:temp+4
	bcc :+
		eor #$FFFF
		inc
	:
	sta [math_r], Y ; pv_hdma_cd0+2
	; reload negate and do next
	lda z:pv_negate
	and #$000F
	sta z:temp+4
	tya
	clc
	adc z:pv_interps
	tay
	dec z:temp+2
	beq :+
	jmp pv_abcd_lines_full_
:
	rts
	; Optimization notes:
	; 1. The negation test (lsr temp+4, bcc) could be eliminated by creating 4 permuations of this routine.
	;    I did not want to complicate this example with 4 copies of the same code, but it would save a few cycles.
	; 2. If the arrays were placed in LoRAM area (<$2000) and this code was placed in a LoROM area of memory
	;    (e.g. banks $80-BF if HiROM), we could use absolute addressing for both the hardware multiplier
	;    and our output array at the same time. Instead I used far addressing for the array and DB=0
	;    so that the arrays can go anywhere in RAM, and the code can run from both LoROM and HiROM.
	; 3. Instead of Z*A at 8x8=16 multiply, could perhaps use 8x16=16 with two hardware multiplies.
	;    This would eliminate the 5 LSRs (i.e. scale would be 1.10 instead of 1.7, with its top 5 bits = 0),
	;    offsetting the added time for a second hardware multiply. (FF6 does something like this.)
	;    Not sure if extra bits of accuracy for A would help in any significant way, though.
	;    I think the 8-bit Z result is the real bottleneck for precision.
	;    Alternatively you could use a 16-bit Z result table (3.10 with its top 5 bits = 0)?
	;    If both were 16-bit precision could definitely improve, at the expense of performance.

pv_abcd_lines_sa1_: ; SA=1 means d=a and c=-b: ~1210 clocks per line
	; temp+2/3 = un-interpolated scanline count
	; temp+4/5 = pv_negate
	; Y = pv_buffer_x
	.a16
	.i16
	lda z:pv_zr
	lsr
	lsr
	lsr
	lsr
	tax
	lda f:pv_ztable, X
	sta a:$4202
	; scale a/d
	ldx z:pv_scale+0
	stx a:$4203
		lda z:pv_zr
		clc
		adc z:pv_zr_inc
		sta z:pv_zr
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	; scale b/c
	ldx z:pv_scale+1
	stx a:$4203
		; negate and store a while waiting
		lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
		sta [math_a], Y ; pv_hdma_ab0+0
		sta [math_r], Y ; pv_hdma_cd0+2 d=a
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr z:temp+4
	bcc :+
		sta [math_p], Y ; pv_hdma_cd0+0
		eor #$FFFF
		inc
		sta [math_b], Y ; pv_hdma_ab0+2 b=-c
		bra :++
	:
		sta [math_b], Y ; pv_hdma_ab0+2
		eor #$FFFF
		inc
		sta [math_p], Y ; pv_hdma_cd0+0 c=-b
	:
	; next
	lda z:pv_negate
	and #$000F
	sta z:temp+4
	tya
	clc
	adc z:pv_interps
	tay
	dec z:temp+2
	bne pv_abcd_lines_sa1_
	rts

pv_abcd_lines_angle0_: ; angle 0 means a/d are positive and b=c=0: ~970 clocks per line
	; temp+2/3 = un-interpolated scanline count
	; Y = pv_buffer_x
	.a16
	.i16
	lda z:pv_zr
	lsr
	lsr
	lsr
	lsr
	tax
	lda f:pv_ztable, X
	sta a:$4202
	; scale a
	ldx z:pv_scale+0
	stx a:$4203
		lda z:pv_zr
		clc
		adc z:pv_zr_inc
		sta z:pv_zr
	lda a:$4216
	; scale d
	ldx z:pv_scale+3
	stx a:$4203
		; scale and store a while waiting
		lsr
		lsr
		lsr
		lsr
		lsr
		sta [math_a], Y ; pv_hdma_ab0+0
	lda a:$4216
	lsr
	lsr
	lsr
	lsr
	lsr
	sta [math_r], Y ; pv_hdma_cd0+2
	; b/c = 0
	lda #0
	sta [math_b], Y
	sta [math_p], Y
	; next
	tya
	clc
	adc z:pv_interps
	tay
	dec z:temp+2
	bne pv_abcd_lines_angle0_
	rts

pv_interpolate_4x_: ; interpolate from every 4th line to every 2nd line
	.a16
	.i16
	; x = pv_buffer_x
	; temp+2/3 = lines to interpolate
	lda temp+2
	pha
	phx
	:
		lda a:pv_hdma_ab0+0,    X
		clc
		adc a:pv_hdma_ab0+0+16, X
		ror
		sta a:pv_hdma_ab0+0+ 8, X
		lda a:pv_hdma_ab0+2,    X
		clc
		adc a:pv_hdma_ab0+2+16, X
		ror
		sta a:pv_hdma_ab0+2+ 8, X
		lda a:pv_hdma_cd0+0,    X
		clc
		adc a:pv_hdma_cd0+0+16, X
		ror
		sta a:pv_hdma_cd0+0+ 8, X
		lda a:pv_hdma_cd0+2,    X
		clc
		adc a:pv_hdma_cd0+2+16, X
		ror
		sta a:pv_hdma_cd0+2+ 8, X
		txa
		clc
		adc #16
		tax
		dec z:temp+2
		bne :-
	plx
	pla
	asl
	sta temp+2 ; reload counter for twice as many lines at 2x interpolation
	;jmp pv_interpolate_2x_
pv_interpolate_2x_: ; interpolate from every 2nd line to every line
	.a16
	.i16
	; x = pv_buffer_x
	; temp+2/3 = lines to interpolate
	:
		lda a:pv_hdma_ab0+0,   X
		clc
		adc a:pv_hdma_ab0+0+8, X
		ror
		sta a:pv_hdma_ab0+0+4, X
		lda a:pv_hdma_ab0+2,   X
		clc
		adc a:pv_hdma_ab0+2+8, X
		ror
		sta a:pv_hdma_ab0+2+4, X
		lda a:pv_hdma_cd0+0,   X
		clc
		adc a:pv_hdma_cd0+0+8, X
		ror
		sta a:pv_hdma_cd0+0+4, X
		lda a:pv_hdma_cd0+2,   X
		clc
		adc a:pv_hdma_cd0+2+8, X
		ror
		sta a:pv_hdma_cd0+2+4, X
		txa
		clc
		adc #8
		tax
		dec z:temp+2
		bne :-
	rts

pv_set_origin: ; Y = scanline to place posx/posy on the centre of
	; Call this only after pv_rebuild has brought the perspective tables up to date.
	; If you don't rotate or change the perspective, this can be reused many times to change the origin without having to use pv_rebuild again.
	.a16
	.i8
	sty z:temp+0 ; temp+0 = target scanline
	tya
	sec
	sbc z:pv_l0
	and #$00FF
	asl
	asl
	sta z:temp+2 ; temp+2/3 = index to line
	sep #$20
	rep #$10
	.a8
	.i16
	lda z:pv_l1
	sec
	sbc z:temp+0
	sta z:math_b ; math_b = scanlines above bottom
	jsr pv_buffer_x ; X = index to pv buffers
	rep #$20
	.a16
	txa
	clc
	adc z:temp+2
	tax ; X = index of target scanline in pv buffers
	lda f:pv_hdma_ab0+2, X
	sta z:math_a ; math_a = b coefficient
	lda f:pv_hdma_cd0+2, X
	pha ; store for a moment
	sep #$10
	.i8
	jsr smul16_u8
	clc
	adc z:posx+2
	sta z:nmi_m7x ; ox = posx + (scanlines * b)
	sec
	sbc #128
	sta z:nmi_hofs ; ox - 128
	pla
	sta z:math_a ; math_a = d coefficient
	jsr smul16_u8
	clc
	adc z:posy+2
	sta z:nmi_m7y ; oy = posy + (scanlines * d)
	lda z:pv_l1
	and #$00FF
	eor #$FFFF
	sec
	adc z:nmi_m7y
	sta z:nmi_vofs ; oy - L1
	; scroll sky to meet L0 and pan with angle
	lda z:angle
	asl
	asl
	eor #$FFFF
	and #$03FF
	sta z:nmi_bg2hofs
	lda z:pv_l0
	eor #$FFFF
	sec
	adc #240
	sta z:nmi_bg2vofs
	rts

pv_texel_to_screen: ; input: texelx,texely output screenx,screeny (pv_rebuild must be up to date)
	.a16
	.i8
	; 1. translate to origin-relative position
	lda z:texelx
	sec
	sbc z:nmi_m7x
	sta z:screenx
	lda z:texely
	sec
	sbc z:nmi_m7y
	sta z:screeny
	; 2. project into the rotated frustum
	ldx z:pv_scale+1
	bne @rotate
	lda z:pv_negate
	and #%1001
	beq @rotate_end ; if b=0, assume c=0, and if a/d are not negated, angle=0
	@rotate:
		lda z:screenx
		sta z:math_a
		lda z:nmi_m7t+0
		sta z:math_b
		jsr smul16f ; X*A
		pha ; X*A
		lda z:nmi_m7t+2
		sta z:math_b
		jsr smul16f ; X*B
		pha ; X*B, X*A
		lda z:screeny
		sta z:math_a
		lda z:nmi_m7t+6
		sta z:math_b
		jsr smul16 ; Y*D
		pla
		clc
		adc z:math_p+1
		sta z:screeny ; sy = X*B + Y*D
		lda z:nmi_m7t+4
		sta z:math_b
		jsr smul16 ; Y*C
		pla
		clc
		adc z:math_p+1
		sta z:screenx ; sx = X*A + Y*C
	@rotate_end:
	; translate Y to move 0 to the top of the screen, rather than the origin at the bottom
	lda z:screeny
	clc
	adc z:pv_sh_
	sta z:screeny
	; 3. try wrapping if outside the rectangular bounds of the frustum (S0 wide, SH tall)
	ldx z:pv_wrap
	beq @wrap_end
		lda z:screenx
		bmi @wrap_xm
			lda z:pv_s0
			lsr
			cmp z:screenx
			bcs @wrap_x_end ; s0/2 >= X, already in range
			lda z:screenx
			sec
			sbc #1024
			sta z:screenx ; try X-1024
			bra @wrap_x_end
		@wrap_xm:
			lda z:pv_s0
			lsr
			eor #$FFFF
			;inc
			cmp z:screenx
			bcc @wrap_x_end ; -s0/2 <= X, already in range
			lda z:screenx
			clc
			adc #1024
			sta z:screenx ; try X+1024
			;bra @wrap_y
		@wrap_x_end:
		lda z:screeny
		bmi @wrap_ym
			cmp z:pv_sh_
			bcc @wrap_end
			lda z:screeny
			sec
			sbc #1024
			sta z:screeny ; try Y-1024
			bra @wrap_end
		@wrap_ym:
			; <0 is above the horizon
			;lda z:screeny
			clc
			adc #1024
			sta z:screeny ; try Y+1024
			;bra @wrap_end
		;
	@wrap_end:
	; 4. transform Y to scanline
	;
	; Interpolating Y from 0 to SH with perspective correction:
	;   t = (scanline - L0) / (L1-L0)
	;   Y = lerp(0/S0,SH/S1,t) / lerp(1/S0,1/S1,t)
	; Inverting this calculation to find scanline from Y:
	;   scanline = (Y * S1 * (L1-L0)) / ((S0 * SH) - Y * (S0 - S1))
	;
	lda z:screeny
	sta z:math_a
	lda z:pv_s1
	sta z:math_b
	jsr smul16
	lda z:math_p+0
	sta z:math_a+0
	lda z:math_p+2
	sta z:math_a+2 ; Y * S1
	lda z:pv_l1
	sec
	sbc z:pv_l0
	and #$00FF
	sta z:math_b
	jsr smul32f_16f
	lda z:math_p+0
	sta z:temp+0
	lda z:math_p+2
	sta z:temp+2 ; Y * S1 * (L1-L0)
	lda z:pv_s0
	sta z:math_a
	lda z:pv_sh_
	sta z:math_b
	jsr umul16
	lda z:math_p+0
	sta z:temp+4
	lda z:math_p+2
	sta z:temp+6 ; S0 * SH
	lda z:screeny
	sta z:math_a
	lda z:pv_s0
	sec
	sbc z:pv_s1
	sta z:math_b
	jsr smul16 ; Y * (S0 - S1)
	lda z:temp+4
	sec
	sbc z:math_p+0
	sta z:math_b+0
	lda z:temp+6
	sbc z:math_p+2
	sta z:math_b+2 ; (S0 * SH) - Y * (S0 - S1)
	lda z:temp+0
	sta z:math_a+0
	lda z:temp+2
	sta z:math_a+2
	jsr sdiv32
	lda z:math_p+0
	bpl :+
	@offscreen: ; just return a very large screeny to indicate offscreen
		lda #$5FFF
		sta z:screeny
		rts
	:
	lda z:pv_l0
	and #$00FF
	clc
	adc z:math_p+0
	sta z:screeny
	lda z:pv_l1
	and #$00FF
	dec
	cmp z:screeny
	bcc @offscreen
	; screeny is now correct
	; math_p+0 = screeny - L0
	; TODO how do we apply the appropriate X scale?
	; HACK
	lda #160
	sta z:screenx
	rts


;
; =============================================================================
; Utilities
;

colmath_off:
	.i8
	ldx #0
	stx z:nmi_cgwsel
	stx z:nmi_cgadsub
	rts

print_stats_common:
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
	rts

print_stats_simple:
	.a16
	.i8
	ldx z:pause
	bne :+
		rts
	:
	jsr print_stats_common
	; Mx,My
	jsr oamp_space
	lda #'M'
	jsr oamp_alpha_space
	lda z:scale+0
	jsr oamp_hex16_space
	lda z:scale+2
	jsr oamp_hex16
	jmp oamp_return

print_stats_pv:
	.a16
	.i8
	ldx z:pause
	bne :+
		rts
	:
	jsr print_stats_common
	; S0,S1,SH
	jsr oamp_space
	lda #'S'
	jsr oamp_alpha_space
	lda z:pv_s0
	jsr oamp_hex16_space
	lda z:pv_s1
	jsr oamp_hex16_space
	lda z:pv_sh_
	jsr oamp_hex16
	jmp oamp_return

;
; =============================================================================
; Mode A test "Overhead, simple"
; - Map spins around a fixed point
; - Player moves over the rotated map
; - L/R apply scale
;
; The basic concept here is just to pin Px,Py in the middle of something you want to rotate/scale,
; and then scroll the screen to move it with hofs/vofs just a normal scroll offset relative to Px,Py.
;
; Useful for the "mode 7 background as a big boss sprite" idiom.
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
	jsr colmath_off
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
	lda #$008C ; arrow
	jsr oam_sprite
	; stats
	jmp print_stats_simple

;
; =============================================================================
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
; Similar to Contra 3 overhead stages.
;

MODE_B_SX = 128
MODE_B_SY = 112

set_mode_b:
	.a16
	.i8
	jsr colmath_off
	ldx #7
	stx z:nmi_bgmode
	ldx #0
	stx z:new_hdma_en
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
	lda z:player_tile
	and #$00FF
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
	lda #$008C ; arrow
	jsr oam_sprite
	; stats
	jmp print_stats_simple

;
; =============================================================================
; Mode X test "Tilted plane"
; - Map appears with perspective tilt
; - Player moves only orthogonally
; - L/R adjusts tilt amount
;
; A little bit simpler than the full rotating perspective plane (mode Y).
; Without rotation, the computation is less expensive, and only needs to be updated when you change the tilt.
;

MODE_X_SX = 128
MODE_X_SY = 112

set_mode_x:
	.a16
	.i8
	jsr colmath_off
	ldx #7
	stx z:nmi_bgmode
	ldx #0
	stx z:new_hdma_en
	stx z:angle
	stx z:pv_interp
	ldx #64
	stx z:tilt
	inx
	stx z:tilt_last ; cause rebuild
	stz z:posx+2
	stz z:posy+2
	ldx #$00
	stx z:player_tile ; face left
	ldx #1
	stx z:pv_wrap ; wrapping world
	jsr oam_sprite_clear
mode_x:
	lda z:gamepad
	and #$0200 ; left
	beq :+
		ldy #$00
		sty z:player_tile
		dec z:posx+2
	:
	lda z:gamepad
	and #$0100 ; right
	beq :+
		ldy #$04
		sty z:player_tile
		inc z:posx+2
	:
	lda z:gamepad
	and #$0800 ; up
	beq :+
		ldy #$08
		sty z:player_tile
		dec z:posy+2
	:
	lda z:gamepad
	and #$0400 ; down
	beq :+
		ldy #$0C
		sty z:player_tile
		inc z:posy+2
	:
	; wrap position
	lda z:posx+2
	and #$03FF
	sta z:posx+2
	lda z:posy+2
	and #$03FF
	sta z:posy+2
	; L/R = adjust tilt
	ldx z:tilt
	lda z:gamepad
	and #$0010 ; R for tilt more
	beq :+
		cpx #196
		bcs :+
		inx
	:
	lda z:gamepad
	and #$0020 ; L for tilt less
	beq :+
		cpx #1
		bcc :+
		dex
	:
	stx z:tilt
	; rebuild perspective if tilt changed
	lda z:newpad
	and #$2000
	bne :+ ; select changes aspect ratio adjustment
	ldx z:tilt
	cpx z:tilt_last
	beq @tilt_end
	:
		ldx #0
		stx z:pv_l0
		ldx #224
		stx z:pv_l1
		lda z:tilt
		and #$00FF
		asl
		asl
		clc
		adc #256
		ldx z:aspect
		beq :+ ; 8/7 aspect ratio adjustment
			sta z:math_a
			lda #(256*8)/7
			sta z:math_b
			jsr umul16
			lda z:math_p+1
		:
		sta z:pv_s0
		lda z:tilt
		and #$00FF
		eor #$FFFF
		sec
		adc #256
		ldx z:aspect
		beq :+ ; 8/7 aspect ratio adjustment
			sta z:math_a
			lda #(256*8)/7
			sta z:math_b
			jsr umul16
			lda z:math_p+1
		:
		sta z:pv_s1
		lda z:tilt
		and #$00FF
		clc
		adc #224
		sta z:pv_sh
		jsr pv_rebuild
		ldx z:tilt
		stx z:tilt_last
	@tilt_end:
	ldy #MODE_X_SY ; place player focus on SY
	jsr pv_set_origin
	; player sprite
	lda #MODE_B_SX
	sta z:screenx
	lda #MODE_B_SY
	sta z:screeny
	lda z:player_tile
	and #$00FF
	ldx #0
	jsr oam_sprite
	; demonstrate texel to screen mapping
	lda #MODE_A_TX
	sta z:texelx
	lda #MODE_A_TY
	sta z:texely
	jsr pv_texel_to_screen
	ldx #4
	lda #$8C ; arrow
	jsr oam_sprite
	; print stats
	jmp print_stats_pv

;
; =============================================================================
; Mode Y test "Flying"
; - Mode 1 clouds at top, fixed colour horizon fades
; - Map rotates around the player
; - L/R raises/lowers view
;

; focus location on ground
MODE_Y_SX = 128
MODE_Y_SY = 168
; minimum height of bird above the ground (height is 0-128 + this)
MODE_Y_HEIGHT_BASE = 16

set_mode_y:
	.a16
	.i8
	; colormath
	ldx #$00
	stx z:nmi_cgwsel ; fixed colour
	ldx #$23
	stx z:nmi_cgadsub ; enable additive blend on BG1 +BG2 + backdrop
	ldx #1
	stx z:nmi_bgmode
	stx z:pv_wrap
	ldy #$80
	sty z:player_tile ; facing down
	ldy #64
	sty z:height ; halfway up
mode_y:
	; L/R = up/down
	ldx z:height
	lda z:gamepad
	and #$0010 ; R for up
	beq :+
		cpx #127
		bcs :+
		inx
	:
	lda z:gamepad
	and #$0020 ; L for down
	beq :+
		cpx #1
		bcc :+
		dex
	:
	stx z:height
	; rotate with left/right
	ldx z:angle
	lda z:gamepad
	and #$0200 ; left
	beq :+
		inx
	:
	lda z:gamepad
	and #$0100 ; right
	beq :+
		dex
	:
	stx z:angle
	; generate perspective
	; --------------------
	; set horizon
	lda z:height
	and #$00FF
	lsr
	clc
	adc #32
	tax
	sta z:pv_l0 ; l0 = 32+(height/2)  [32-96]
	ldx #224
	stx z:pv_l1
	; set view scale
	lda z:height
	and #$00FF
	asl
	clc
	adc #384
	sta z:pv_s0 ; 384 + (height*2)    [384-640]
	lda z:height
	and #$00FF
	lsr
	adc #64
	sta z:pv_s1 ; 64 + (height/2)     [64-128]
	lda #0
	sta z:pv_sh ; dependent-vertical scale
	ldx #2
	stx z:pv_interp ; 2x interpolation
	jsr pv_rebuild
	; up/down moves player (depends on generated rotation matrix)
	lda z:gamepad
	and #$0400 ; down
	beq :+
		ldy #$80
		sty z:player_tile
		; X += B * 2
		lda z:nmi_m7t + 2 ; B
		asl
		pha
		clc
		adc z:posx+1
		sta z:posx+1
		pla
		jsr sign
		adc z:posx+3
		and #$0003 ; wrap to 0-1023
		sta z:posx+3
		; Y += D * 2
		lda z:nmi_m7t + 6 ; D
		asl
		pha
		clc
		adc z:posy+1
		sta z:posy+1
		pla
		jsr sign
		adc z:posy+3
		and #$0003
		sta z:posy+3
	:
	lda z:gamepad
	and #$0800 ; up
	beq :+
		ldy #$40
		sty z:player_tile
		; X -= B * 2
		lda #0
		sec
		sbc z:nmi_m7t + 2 ; B
		asl
		pha
		clc
		adc z:posx+1
		sta z:posx+1
		pla
		jsr sign
		adc z:posx+3
		and #$0003
		sta z:posx+3
		; Y -= D * 2
		lda #0
		sec
		sbc z:nmi_m7t + 6 ; D
		asl
		pha
		clc
		adc z:posy+1
		sta z:posy+1
		pla
		jsr sign
		adc z:posy+3
		and #$0003
		sta z:posy+3
	:
	; select to reset position (since aspect ratio can't do anything in sh=0)
	lda z:newpad
	and #$2000 ; select
	beq :+
		stz z:posx+0
		stz z:posx+2
		stz z:posx+4
		stz z:posy+0
		stz z:posy+2
		stz z:posy+4
		ldx #0
		stx z:angle
	:
	; set origin
	ldy #MODE_Y_SY ; place focus at centre of scanline SY
	jsr pv_set_origin
	; animate and draw sprites and stats
	; ----------------------------------
	jsr oam_sprite_clear
	; draw flying bird
	lda #MODE_Y_SX
	sta z:screenx
	lda #MODE_Y_SY - MODE_Y_HEIGHT_BASE
	sec
	sbc z:height
	and #$00FF
	sta z:screeny
	ldx #0
	; animate bird 0,4,8,4 pattern
	lda z:nmi_count
	lsr
	and #$000C ; +4 every 8 frames
	cmp #$000C
	bcc :+
		lda #$0004
	:
	ora z:player_tile
	and #$00FF
	jsr oam_sprite
	; demonstrate texel to screen mapping
	lda #MODE_A_TX
	sta z:texelx
	lda #MODE_A_TY
	sta z:texely
	jsr pv_texel_to_screen
	ldx #4
	lda #$8C ; arrow
	jsr oam_sprite
	; shadow at focus
	lda #MODE_Y_SX
	sta z:screenx
	lda #MODE_Y_SY
	sta z:screeny
	lda z:height
	lsr
	lsr
	lsr
	and #$000C
	ora #$0100 ; shadow sprite selected by height
	ldx #8
	jsr oam_sprite
	; stats
	jmp print_stats_pv
