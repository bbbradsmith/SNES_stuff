; =============================================================================
; =============================================================================
; 16-bit precision test:
;   This version has been modified to use 16-bit precision for Z * A
;   instead of truncating them both to 8 bits. This does improve precision
;   somewhat, though is a lot more computationally expensive.
;   This version is not optimized, but it would still be significantly slower
;   even if it was. I've turned interpolation off too, just to see what it looks
;   like with the best precision it can use. There is some noticeable benefit
;   here, and it might be a reasonable tradeoff if you don't mind running at
;   30 fps instead of 60. In particular the extra precision for SA allows a
;   wider range than SH. This is very noticeable in how the X demo tilt looks
;   in this version vs. the 8-bit original.
;
;   See the github change history to get an idea of how this was applied.
; =============================================================================
; =============================================================================

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

nmi_bgmode:   .res 1 ; hardware settings applied when nmi_ready
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
pv_scale:     .res 8 ; 8.8 scale of a/b/c/d
pv_negate:    .res 1 ; negation of a/b/c/d
pv_interps:   .res 2 ; interpolate * 4 for stride increment
pv_16:        .res 14 ; tempoararies for 16-bit calculation test

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
; Overview:
;   Parameters
;     angle (u8) - determines the direction of view, specified in 256ths of a circle
;     pv_l0 (u8) - the vertical line of output picture where the view begins (horizon), 0 for top of screen
;     pv_l1 (u8) - the vertical line where the view stops (usually 224)
;     pv_s0 (u16) - the horizontal width of the view in texels on the texure map at L0 (256 = 1:1 scale, max < 1024)
;     pv_s1 (u16) - the horizontal width at L1, S1 should be <= S0
;     pv_sh (u16) - the vertical height of the view in texels on the texture map, use 0 for "automatic" (fast), L1-L0 = 1:1 scale
;     pv_interp (u8) - values of 2 or 4 give 2x or 4x vertical interpolation (faster calculation, less accurate), other values disable interpolation
;     pv_wrap (u8) - 1 if pv_texel_to_world coordinates need to wrap
;
; L0 to L1 defines the area of the screen to be covered by the perspective effect.
; S0, S1, and SH are the top, bottom, and height of a trapezoid view frustum, this is an area on the 1024x1024 mode 7 texture map that will be visible in this perspective
;
; Calculation overview:
;   Creating a correct perspective involves a "Z" factor which is proportional to your distance from a point on the plane.
;   The scale of Z is arbitrary as long as the Z for the bottom is the right ratio to the Z at the top.
;   For our perspective, Z at the top is proportional to S0, and Z at the bottom is proportional to S1.
;   Across the vertical space of the picture, Z does not interpolate linearly. However, its reciprocal 1/Z does.
;   We can linearly interpolate this reciprocal 1/Z and then invert it to recover a perspective correct Z for each line.
;     Useful reference: https://en.wikipedia.org/wiki/Texture_mapping#Perspective_correctness
;
;   This calculation is used to generate a set of scalings of the Mode 7 ABCD matrix for each line of the picture, to be updated by HDMA.
;   Setup:
;     ZR0 = 1/S0
;     ZR1 = 1/S1
;     SA = (256 * SH) / (S0 * (L1 - L0)), or SA = 1 if automatic (SH=0)
;   Per-line:
;     zr = lerp(ZR0,ZR1,(line-L0)/(L1-L0))
;     z = 1 / zr
;     a = z *  cos(angle)
;     b = z *  sin(angle) * SA
;     c = z * -sin(angle)
;     d = z *  cos(angle) * SA
;
;   Note that the above is an idealized version of the computation. It might be usable as-is on a CPU with floating point operations,
;   but the actual SNES computation will involved fixed point numbers with carefully chosen scales. More details in the code below.
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

pv_ztable: ; 12 bit (1<<17)/z lookup
.word $FFFF,$FFFF,$FFFF,$AAAB,$8000,$6666,$5555,$4925,$4000,$38E4,$3333,$2E8C,$2AAB,$2762,$2492,$2222,$2000,$1E1E,$1C72,$1AF3,$199A,$1862,$1746,$1643,$1555,$147B,$13B1,$12F7,$1249,$11A8,$1111,$1084 ; 0000
.word $1000,$0F84,$0F0F,$0EA1,$0E39,$0DD6,$0D79,$0D21,$0CCD,$0C7D,$0C31,$0BE8,$0BA3,$0B61,$0B21,$0AE5,$0AAB,$0A73,$0A3D,$0A0A,$09D9,$09A9,$097B,$094F,$0925,$08FC,$08D4,$08AE,$0889,$0865,$0842,$0821 ; 0020
.word $0800,$07E0,$07C2,$07A4,$0788,$076C,$0750,$0736,$071C,$0704,$06EB,$06D4,$06BD,$06A6,$0690,$067B,$0666,$0652,$063E,$062B,$0618,$0606,$05F4,$05E3,$05D1,$05C1,$05B0,$05A0,$0591,$0581,$0572,$0564 ; 0040
.word $0555,$0547,$0539,$052C,$051F,$0512,$0505,$04F9,$04EC,$04E0,$04D5,$04C9,$04BE,$04B2,$04A8,$049D,$0492,$0488,$047E,$0474,$046A,$0460,$0457,$044D,$0444,$043B,$0432,$042A,$0421,$0419,$0410,$0408 ; 0060
.word $0400,$03F8,$03F0,$03E9,$03E1,$03DA,$03D2,$03CB,$03C4,$03BD,$03B6,$03AF,$03A8,$03A2,$039B,$0395,$038E,$0388,$0382,$037C,$0376,$0370,$036A,$0364,$035E,$0359,$0353,$034E,$0348,$0343,$033E,$0338 ; 0080
.word $0333,$032E,$0329,$0324,$031F,$031A,$0316,$0311,$030C,$0308,$0303,$02FF,$02FA,$02F6,$02F1,$02ED,$02E9,$02E5,$02E0,$02DC,$02D8,$02D4,$02D0,$02CC,$02C8,$02C4,$02C1,$02BD,$02B9,$02B6,$02B2,$02AE ; 00A0
.word $02AB,$02A7,$02A4,$02A0,$029D,$0299,$0296,$0293,$028F,$028C,$0289,$0286,$0283,$027F,$027C,$0279,$0276,$0273,$0270,$026D,$026A,$0267,$0264,$0262,$025F,$025C,$0259,$0257,$0254,$0251,$024E,$024C ; 00C0
.word $0249,$0247,$0244,$0241,$023F,$023C,$023A,$0237,$0235,$0233,$0230,$022E,$022B,$0229,$0227,$0224,$0222,$0220,$021E,$021B,$0219,$0217,$0215,$0213,$0211,$020E,$020C,$020A,$0208,$0206,$0204,$0202 ; 00E0
.word $0200,$01FE,$01FC,$01FA,$01F8,$01F6,$01F4,$01F2,$01F0,$01EF,$01ED,$01EB,$01E9,$01E7,$01E5,$01E4,$01E2,$01E0,$01DE,$01DD,$01DB,$01D9,$01D7,$01D6,$01D4,$01D2,$01D1,$01CF,$01CE,$01CC,$01CA,$01C9 ; 0100
.word $01C7,$01C6,$01C4,$01C2,$01C1,$01BF,$01BE,$01BC,$01BB,$01B9,$01B8,$01B6,$01B5,$01B3,$01B2,$01B1,$01AF,$01AE,$01AC,$01AB,$01AA,$01A8,$01A7,$01A5,$01A4,$01A3,$01A1,$01A0,$019F,$019D,$019C,$019B ; 0120
.word $019A,$0198,$0197,$0196,$0195,$0193,$0192,$0191,$0190,$018E,$018D,$018C,$018B,$018A,$0188,$0187,$0186,$0185,$0184,$0183,$0182,$0180,$017F,$017E,$017D,$017C,$017B,$017A,$0179,$0178,$0176,$0175 ; 0140
.word $0174,$0173,$0172,$0171,$0170,$016F,$016E,$016D,$016C,$016B,$016A,$0169,$0168,$0167,$0166,$0165,$0164,$0163,$0162,$0161,$0160,$015F,$015E,$015E,$015D,$015C,$015B,$015A,$0159,$0158,$0157,$0156 ; 0160
.word $0155,$0154,$0154,$0153,$0152,$0151,$0150,$014F,$014E,$014E,$014D,$014C,$014B,$014A,$0149,$0149,$0148,$0147,$0146,$0145,$0144,$0144,$0143,$0142,$0141,$0140,$0140,$013F,$013E,$013D,$013D,$013C ; 0180
.word $013B,$013A,$013A,$0139,$0138,$0137,$0137,$0136,$0135,$0134,$0134,$0133,$0132,$0132,$0131,$0130,$012F,$012F,$012E,$012D,$012D,$012C,$012B,$012B,$012A,$0129,$0129,$0128,$0127,$0127,$0126,$0125 ; 01A0
.word $0125,$0124,$0123,$0123,$0122,$0121,$0121,$0120,$011F,$011F,$011E,$011E,$011D,$011C,$011C,$011B,$011A,$011A,$0119,$0119,$0118,$0117,$0117,$0116,$0116,$0115,$0115,$0114,$0113,$0113,$0112,$0112 ; 01C0
.word $0111,$0110,$0110,$010F,$010F,$010E,$010E,$010D,$010D,$010C,$010B,$010B,$010A,$010A,$0109,$0109,$0108,$0108,$0107,$0107,$0106,$0106,$0105,$0105,$0104,$0104,$0103,$0103,$0102,$0102,$0101,$0101 ; 01E0
.word $0100,$0100,$00FF,$00FF,$00FE,$00FE,$00FD,$00FD,$00FC,$00FC,$00FB,$00FB,$00FA,$00FA,$00F9,$00F9,$00F8,$00F8,$00F7,$00F7,$00F6,$00F6,$00F5,$00F5,$00F5,$00F4,$00F4,$00F3,$00F3,$00F2,$00F2,$00F1 ; 0200
.word $00F1,$00F0,$00F0,$00F0,$00EF,$00EF,$00EE,$00EE,$00ED,$00ED,$00ED,$00EC,$00EC,$00EB,$00EB,$00EA,$00EA,$00EA,$00E9,$00E9,$00E8,$00E8,$00E8,$00E7,$00E7,$00E6,$00E6,$00E6,$00E5,$00E5,$00E4,$00E4 ; 0220
.word $00E4,$00E3,$00E3,$00E2,$00E2,$00E2,$00E1,$00E1,$00E0,$00E0,$00E0,$00DF,$00DF,$00DF,$00DE,$00DE,$00DD,$00DD,$00DD,$00DC,$00DC,$00DC,$00DB,$00DB,$00DA,$00DA,$00DA,$00D9,$00D9,$00D9,$00D8,$00D8 ; 0240
.word $00D8,$00D7,$00D7,$00D7,$00D6,$00D6,$00D5,$00D5,$00D5,$00D4,$00D4,$00D4,$00D3,$00D3,$00D3,$00D2,$00D2,$00D2,$00D1,$00D1,$00D1,$00D0,$00D0,$00D0,$00CF,$00CF,$00CF,$00CE,$00CE,$00CE,$00CD,$00CD ; 0260
.word $00CD,$00CC,$00CC,$00CC,$00CC,$00CB,$00CB,$00CB,$00CA,$00CA,$00CA,$00C9,$00C9,$00C9,$00C8,$00C8,$00C8,$00C8,$00C7,$00C7,$00C7,$00C6,$00C6,$00C6,$00C5,$00C5,$00C5,$00C5,$00C4,$00C4,$00C4,$00C3 ; 0280
.word $00C3,$00C3,$00C2,$00C2,$00C2,$00C2,$00C1,$00C1,$00C1,$00C0,$00C0,$00C0,$00C0,$00BF,$00BF,$00BF,$00BF,$00BE,$00BE,$00BE,$00BD,$00BD,$00BD,$00BD,$00BC,$00BC,$00BC,$00BC,$00BB,$00BB,$00BB,$00BA ; 02A0
.word $00BA,$00BA,$00BA,$00B9,$00B9,$00B9,$00B9,$00B8,$00B8,$00B8,$00B8,$00B7,$00B7,$00B7,$00B7,$00B6,$00B6,$00B6,$00B6,$00B5,$00B5,$00B5,$00B5,$00B4,$00B4,$00B4,$00B4,$00B3,$00B3,$00B3,$00B3,$00B2 ; 02C0
.word $00B2,$00B2,$00B2,$00B1,$00B1,$00B1,$00B1,$00B0,$00B0,$00B0,$00B0,$00AF,$00AF,$00AF,$00AF,$00AF,$00AE,$00AE,$00AE,$00AE,$00AD,$00AD,$00AD,$00AD,$00AC,$00AC,$00AC,$00AC,$00AC,$00AB,$00AB,$00AB ; 02E0
.word $00AB,$00AA,$00AA,$00AA,$00AA,$00AA,$00A9,$00A9,$00A9,$00A9,$00A8,$00A8,$00A8,$00A8,$00A8,$00A7,$00A7,$00A7,$00A7,$00A7,$00A6,$00A6,$00A6,$00A6,$00A5,$00A5,$00A5,$00A5,$00A5,$00A4,$00A4,$00A4 ; 0300
.word $00A4,$00A4,$00A3,$00A3,$00A3,$00A3,$00A3,$00A2,$00A2,$00A2,$00A2,$00A2,$00A1,$00A1,$00A1,$00A1,$00A1,$00A0,$00A0,$00A0,$00A0,$00A0,$009F,$009F,$009F,$009F,$009F,$009E,$009E,$009E,$009E,$009E ; 0320
.word $009E,$009D,$009D,$009D,$009D,$009D,$009C,$009C,$009C,$009C,$009C,$009B,$009B,$009B,$009B,$009B,$009B,$009A,$009A,$009A,$009A,$009A,$0099,$0099,$0099,$0099,$0099,$0099,$0098,$0098,$0098,$0098 ; 0340
.word $0098,$0098,$0097,$0097,$0097,$0097,$0097,$0096,$0096,$0096,$0096,$0096,$0096,$0095,$0095,$0095,$0095,$0095,$0095,$0094,$0094,$0094,$0094,$0094,$0094,$0093,$0093,$0093,$0093,$0093,$0093,$0092 ; 0360
.word $0092,$0092,$0092,$0092,$0092,$0091,$0091,$0091,$0091,$0091,$0091,$0091,$0090,$0090,$0090,$0090,$0090,$0090,$008F,$008F,$008F,$008F,$008F,$008F,$008E,$008E,$008E,$008E,$008E,$008E,$008E,$008D ; 0380
.word $008D,$008D,$008D,$008D,$008D,$008C,$008C,$008C,$008C,$008C,$008C,$008C,$008B,$008B,$008B,$008B,$008B,$008B,$008B,$008A,$008A,$008A,$008A,$008A,$008A,$008A,$0089,$0089,$0089,$0089,$0089,$0089 ; 03A0
.word $0089,$0088,$0088,$0088,$0088,$0088,$0088,$0088,$0087,$0087,$0087,$0087,$0087,$0087,$0087,$0086,$0086,$0086,$0086,$0086,$0086,$0086,$0085,$0085,$0085,$0085,$0085,$0085,$0085,$0085,$0084,$0084 ; 03C0
.word $0084,$0084,$0084,$0084,$0084,$0083,$0083,$0083,$0083,$0083,$0083,$0083,$0083,$0082,$0082,$0082,$0082,$0082,$0082,$0082,$0082,$0081,$0081,$0081,$0081,$0081,$0081,$0081,$0081,$0080,$0080,$0080 ; 03E0
.word $0080,$0080,$0080,$0080,$0080,$007F,$007F,$007F,$007F,$007F,$007F,$007F,$007F,$007E,$007E,$007E,$007E,$007E,$007E,$007E,$007E,$007D,$007D,$007D,$007D,$007D,$007D,$007D,$007D,$007C,$007C,$007C ; 0400
.word $007C,$007C,$007C,$007C,$007C,$007C,$007B,$007B,$007B,$007B,$007B,$007B,$007B,$007B,$007A,$007A,$007A,$007A,$007A,$007A,$007A,$007A,$007A,$0079,$0079,$0079,$0079,$0079,$0079,$0079,$0079,$0079 ; 0420
.word $0078,$0078,$0078,$0078,$0078,$0078,$0078,$0078,$0078,$0077,$0077,$0077,$0077,$0077,$0077,$0077,$0077,$0077,$0077,$0076,$0076,$0076,$0076,$0076,$0076,$0076,$0076,$0076,$0075,$0075,$0075,$0075 ; 0440
.word $0075,$0075,$0075,$0075,$0075,$0075,$0074,$0074,$0074,$0074,$0074,$0074,$0074,$0074,$0074,$0073,$0073,$0073,$0073,$0073,$0073,$0073,$0073,$0073,$0073,$0072,$0072,$0072,$0072,$0072,$0072,$0072 ; 0460
.word $0072,$0072,$0072,$0071,$0071,$0071,$0071,$0071,$0071,$0071,$0071,$0071,$0071,$0071,$0070,$0070,$0070,$0070,$0070,$0070,$0070,$0070,$0070,$0070,$006F,$006F,$006F,$006F,$006F,$006F,$006F,$006F ; 0480
.word $006F,$006F,$006F,$006E,$006E,$006E,$006E,$006E,$006E,$006E,$006E,$006E,$006E,$006E,$006D,$006D,$006D,$006D,$006D,$006D,$006D,$006D,$006D,$006D,$006D,$006C,$006C,$006C,$006C,$006C,$006C,$006C ; 04A0
.word $006C,$006C,$006C,$006C,$006B,$006B,$006B,$006B,$006B,$006B,$006B,$006B,$006B,$006B,$006B,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$006A,$0069,$0069,$0069,$0069,$0069 ; 04C0
.word $0069,$0069,$0069,$0069,$0069,$0069,$0069,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0068,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0067,$0066 ; 04E0
.word $0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0065,$0064,$0064,$0064,$0064,$0064,$0064,$0064 ; 0500
.word $0064,$0064,$0064,$0064,$0064,$0064,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0063,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062,$0062 ; 0520
.word $0062,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0061,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$0060,$005F,$005F,$005F ; 0540
.word $005F,$005F,$005F,$005F,$005F,$005F,$005F,$005F,$005F,$005F,$005F,$005F,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005E,$005D,$005D,$005D,$005D,$005D,$005D ; 0560
.word $005D,$005D,$005D,$005D,$005D,$005D,$005D,$005D,$005D,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005C,$005B,$005B,$005B,$005B,$005B,$005B,$005B ; 0580
.word $005B,$005B,$005B,$005B,$005B,$005B,$005B,$005B,$005B,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$005A,$0059,$0059,$0059,$0059,$0059,$0059,$0059 ; 05A0
.word $0059,$0059,$0059,$0059,$0059,$0059,$0059,$0059,$0059,$0059,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0058,$0057,$0057,$0057,$0057,$0057,$0057 ; 05C0
.word $0057,$0057,$0057,$0057,$0057,$0057,$0057,$0057,$0057,$0057,$0057,$0057,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0056,$0055,$0055 ; 05E0
.word $0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0055,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054,$0054 ; 0600
.word $0054,$0054,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0053,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052 ; 0620
.word $0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0052,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0051,$0050,$0050,$0050 ; 0640
.word $0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$0050,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F,$004F ; 0660
.word $004F,$004F,$004F,$004F,$004F,$004F,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004E,$004D,$004D,$004D,$004D ; 0680
.word $004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004D,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C ; 06A0
.word $004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004C,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B,$004B ; 06C0
.word $004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$004A,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049 ; 06E0
.word $0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0049,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048 ; 0700
.word $0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0048,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047,$0047 ; 0720
.word $0047,$0047,$0047,$0047,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0046,$0045,$0045 ; 0740
.word $0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0045,$0044,$0044,$0044,$0044,$0044,$0044 ; 0760
.word $0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0044,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043 ; 0780
.word $0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0043,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042 ; 07A0
.word $0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0042,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041 ; 07C0
.word $0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0041,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040 ; 07E0
.word $0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$0040,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F ; 0800
.word $003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003F,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E ; 0820
.word $003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003E,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D ; 0840
.word $003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003D,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C ; 0860
.word $003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003C,$003B,$003B,$003B,$003B,$003B ; 0880
.word $003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B,$003B ; 08A0
.word $003B,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A ; 08C0
.word $003A,$003A,$003A,$003A,$003A,$003A,$003A,$003A,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039 ; 08E0
.word $0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0039,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038 ; 0900
.word $0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0038,$0037,$0037,$0037,$0037,$0037,$0037 ; 0920
.word $0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037,$0037 ; 0940
.word $0037,$0037,$0037,$0037,$0037,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036 ; 0960
.word $0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0036,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035 ; 0980
.word $0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035,$0035 ; 09A0
.word $0035,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034 ; 09C0
.word $0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0034,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033 ; 09E0
.word $0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033,$0033 ; 0A00
.word $0033,$0033,$0033,$0033,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032 ; 0A20
.word $0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0032,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031 ; 0A40
.word $0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031 ; 0A60
.word $0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0031,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030 ; 0A80
.word $0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030 ; 0AA0
.word $0030,$0030,$0030,$0030,$0030,$0030,$0030,$0030,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F ; 0AC0
.word $002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F,$002F ; 0AE0
.word $002F,$002F,$002F,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E ; 0B00
.word $002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E,$002E ; 0B20
.word $002E,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D ; 0B40
.word $002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D,$002D ; 0B60
.word $002D,$002D,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C ; 0B80
.word $002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C,$002C ; 0BA0
.word $002C,$002C,$002C,$002C,$002C,$002C,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B ; 0BC0
.word $002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B ; 0BE0
.word $002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002B,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A ; 0C00
.word $002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A ; 0C20
.word $002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$002A,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029 ; 0C40
.word $0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029 ; 0C60
.word $0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029,$0029 ; 0C80
.word $0029,$0029,$0029,$0029,$0029,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028 ; 0CA0
.word $0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028 ; 0CC0
.word $0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0028,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027 ; 0CE0
.word $0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027 ; 0D00
.word $0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027 ; 0D20
.word $0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0027,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026 ; 0D40
.word $0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026 ; 0D60
.word $0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026 ; 0D80
.word $0026,$0026,$0026,$0026,$0026,$0026,$0026,$0026,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025 ; 0DA0
.word $0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025 ; 0DC0
.word $0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025 ; 0DE0
.word $0025,$0025,$0025,$0025,$0025,$0025,$0025,$0025,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024 ; 0E00
.word $0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024 ; 0E20
.word $0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024 ; 0E40
.word $0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0024,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023 ; 0E60
.word $0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023 ; 0E80
.word $0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023 ; 0EA0
.word $0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0023,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022 ; 0EC0
.word $0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022 ; 0EE0
.word $0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022 ; 0F00
.word $0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022 ; 0F20
.word $0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0022,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021 ; 0F40
.word $0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021 ; 0F60
.word $0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021 ; 0F80
.word $0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021,$0021 ; 0FA0
.word $0021,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020 ; 0FC0
.word $0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020,$0020 ; 0FE0
; This table is 12-bit (8kb) but could be made smaller at a precision tradeoff.
; An 8-bit implementation was tried using the hardware div: (1<<11) / (zr>>8))
; This looked too imprecise and there was excessive "rippling", especially in the distance.
; I think as low as 10-bits (1kb) is still fairly good, and beyond 12-bit didn't seem to offer any useful additional precision.
; If using a smaller table, compensate by adding lsr(s) to the zr shift in @abcd_pv_line.
;
; python generator:
;def ztable(bits,width=32):
;    s = "pv_ztable: ; %d bit (1<<%d)/z lookup" % (bits,5+bits)
;    for i in range(0,1<<(bits)):
;        v = 0xFFFF
;        if i > 0:
;            v = min(0xFFFF,round((1 << (5+bits)) / i))
;        if (i % width) == 0:
;            s += "\n.word "
;        s += "$%04X" % v
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
	; Overview of calculation:
	;   Interpolating from S0 to S1 texel scales, with perspective correction,
	;   meaning that for Z proportional to S0/S1, 1/Z interpolates linearly down the screen.
	;   So: 1/Z is interpolated, and used to recover the corrected Z.
	;   Finally, it's multiplied by the rotation matrix, and the vertical values receive a relative scaling.
	;   Various shifts are applied to make the fixed point precision practical.
	;   Acceptable ranges are set by the fixed point precision. These could be adjusted to trade precision for more/less range:
	;   - S0/S1 should be <1024: 2.6 precision goes from 0 to 4x-1 scale
	;   - L0<L1, L1<254 (L1 should probably always be 224)
	;
	; Setup:
	;   ZR0 = (1<<21)/S0              ; 11.21 / 8.8 (S0) = 19.13, truncated to 3.13
	;   ZR1 = (1<<21)/S1
	;   SA = (256 * SH) / (S0 * (L1 - L0)) ; pre-combined with rotation cos/sin at 8.8
	; Per-line:
	;   zr = >lerp(ZR0,ZR1)>>4        ; 3.9 (truncated from 3.13)
	;   z = <((1<<17)/zr)             ; 1.17 / 3.9 = 10.8, clamped to 8.8
	;   a = z *  cos(angle)           ; 8.8 * 8.8 = 16.16, truncated to 8.8
	;   b = z *  sin(angle) * SA
	;   c = z * -sin(angle)
	;   d = z *  cos(angle) * SA
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
	lda z:cosa
	sta z:math_b
	sta z:pv_scale+0 ; scale A = cos
	lda z:pv_sh
	sta z:math_a
	beq :+
		jsr umul16
		lda z:math_p+1
		bra :++
	:
		lda z:math_b
	:
	sta z:pv_scale+6 ; scale D = SA * cos
	lda z:sina
	sta z:math_b
	sta z:pv_scale+4 ; scale C = sin
	lda z:pv_sh
	beq :+
		jsr umul16
		lda z:math_p+1
		bra :++
	:
		lda z:math_b
	:
	sta z:pv_scale+2 ; scale B = SA * sin
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
	; result ZP as long pointers
	ldx #$7E
	stx z:pv_16+0+2
	stx z:pv_16+3+2
	stx z:pv_16+6+2
	stx z:pv_16+9+2
	lda #.loword(pv_hdma_ab0+0)
	sta z:pv_16+0
	lda #.loword(pv_hdma_ab0+2)
	sta z:pv_16+3
	lda #.loword(pv_hdma_cd0+0)
	sta z:pv_16+6
	lda #.loword(pv_hdma_cd0+2)
	sta z:pv_16+9
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
	; generate lines
	jsr pv_abcd_lines_full_
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
	and #$FFFE
	tax ; X = 12-bit zr for ztable lookup
	lda f:pv_ztable, X
	sta z:math_a
	lda z:pv_zr
	clc
	adc z:pv_zr_inc
	sta z:pv_zr ; zr += linear interpolation increment for next line
	sty z:pv_16+12
	; scale a
	lda z:pv_scale+0
	sta z:math_b
	sep #$10
	.i8
	jsr umul16
	rep #$10
	.i16
	lda z:math_p+1
	lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
	ldy z:pv_16+12
	sta [pv_16+0], Y
	; scale b
	lda z:pv_scale+2
	sta z:math_b
	sep #$10
	.i8
	jsr umul16
	rep #$10
	.i16
	lda z:math_p+1
	lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
	ldy z:pv_16+12
	sta [pv_16+3], Y
	; scale c
	lda z:pv_scale+4
	sta z:math_b
	sep #$10
	.i8
	jsr umul16
	rep #$10
	.i16
	lda z:math_p+1
	lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
	ldy z:pv_16+12
	sta [pv_16+6], Y
	; scale d
	lda z:pv_scale+6
	sta z:math_b
	sep #$10
	.i8
	jsr umul16
	rep #$10
	.i16
	lda z:math_p+1
	lsr z:temp+4
		bcc :+
			eor #$FFFF
			inc
		:
	ldy z:pv_16+12
	sta [pv_16+9], Y
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
	; Optimization note: a custom umul16 that only does a 24-bit result and can work with .i16 would save a lot of time here.

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
	; fall through

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
	; 2. try wrapping if the distance is larger than half the map
	ldx z:pv_wrap
	beq @wrap_end
		lda z:screenx
		bmi @wrap_xm
			cmp #512
			bcc @wrap_x_end
			sec
			sbc #1024
			sta z:screenx ; try X-1024
			bra @wrap_x_end
		@wrap_xm:
			cmp #.loword(-513)
			bcs @wrap_x_end
			clc
			adc #1024
			sta z:screenx ; try X+1024
			;bra @wrap_y
		@wrap_x_end:
		lda z:screeny
		bmi @wrap_ym
			cmp #512
			bcc @wrap_end
			sec
			sbc #1024
			sta z:screeny ; try Y-1024
			bra @wrap_end
		@wrap_ym:
			cmp #.loword(-513)
			bcs @wrap_end
			clc
			adc #1024
			sta z:screeny ; try Y+1024
			;bra @wrap_end
		;
	@wrap_end:
	; 3. project into the rotated frustum
	lda z:pv_scale+2
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
	; 4. transform Y to scanline
	;
	; Interpolating Y from 0 to SH with perspective correction gives the relationship between Y in frustum-texel-space and scanlines:
	;   t = (scanline - L0) / (L1-L0)
	;   Y = lerp(0/S0,SH/S1,t) / lerp(1/S0,1/S1,t)
	; Inverting this calculation to find scanline from Y:
	;   (scanline - L0) = (Y * S1 * (L1-L0)) / ((S0 * SH) - Y * (S0 - S1))
	;
	; X must be rescaled by a factor of S0-to-S1 / 256, interpolating linearly with Y:
	;   screenx = X / lerp(S0/256,S1/256,Y/SH)
	; This becomes a division by the same factor as with Y:
	;   screenx = (X * SH * 256) / ((S0 * SH) - Y * (S0 - S1))
	;
	lda z:screeny
	bmi @offscreen
	cmp z:pv_sh_
	bcc :+
	@offscreen: ; just return a very large screeny to indicate offscreen
		lda #$5FFF
		sta z:screeny
		rts
	:
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
	sta z:temp+4
	sta z:math_b+0
	lda z:temp+6
	sbc z:math_p+2
	sta z:math_b+2 ; (S0 * SH) - Y * (S0 - S1)
	sta z:temp+6
	lda z:temp+0
	sta z:math_a+0
	lda z:temp+2
	sta z:math_a+2
	jsr sdiv32 ; (Y * S1 * (L1-L0)) / ((S0 * SH) - Y * (S0 - S1))
	lda z:pv_l0
	and #$00FF
	clc
	adc z:math_p+0
	sta z:screeny ; screeny is now correct
	; screenx
	lda z:pv_sh_
	sta z:math_a
	lda z:screenx
	sta z:math_b
	jsr smul16 ; X * SH
	stz z:math_a
	lda z:math_p+0
	sta z:math_a+1
	ldx z:math_p+2
	stx z:math_a+3 ; X * SH * 256
	lda z:temp+4
	sta z:math_b+0
	lda z:temp+6
	sta z:math_b+2 ; (S0 * SH) - Y * (S0 - S1)
	jsr sdiv32 ; (X * SH * 256) / ((S0 * SH) - Y * (S0 - S1))
	lda z:math_p+0
	clc
	adc #128
	sta z:screenx
	; NOTE: could probably only do 1 division (1 / the shared denominator) and then 2 multiplies.
	rts
	; Optimization note:
	;   Should probably do 1 division (1 / the shared denominator) and then 2 multiplies, since udiv32/sdiv32 is so slow,
	;   and overlapped hardware multiplies can probably do it faster? This function could probably be made a lot leaner,
	;   but ultimately if we need to do many of these per frame a more efficient alternative route may be needed.
	;   For example: if we only need to rotate, but don't change other parameters, we could create a lookup table with SH entries,
	;   mapping every Y to a screeny, and providing a scaling factor for every X. (e.g. F-Zero doesn't change perspective during a race.)

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
; "Pin" demonstrating a texel to screen mapping
;

; texel coordinate of screen-sprite
PIN_TX = 280 ; entrance to forest
PIN_TY = 115
;PIN_TX = 807 ; tip of 7 in "MODE 7" (accuracy declines with distance from Px)
;PIN_TY = 645

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
	lda #PIN_TX
	sta z:texelx
	lda #PIN_TY
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
	lda #PIN_TX
	sta z:texelx
	lda #PIN_TY
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
; A little bit simpler than the full rotating perspective plane (mode Y below).
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
	ldx #2
	stx z:pv_interp ; 0x interpolation
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
	lda #PIN_TX
	sta z:texelx
	lda #PIN_TY
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
	ldx #0
	stx z:pv_interp ; 0x interpolation
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
	lda #PIN_TX
	sta z:texelx
	lda #PIN_TY
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
