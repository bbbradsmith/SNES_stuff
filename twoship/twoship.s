; twoship mode 5 demo
; press B for mode 1, press A for mode 5
; Select to toggler sprites, Start to pause
; provides a comparison between 256x240p and 512x480i graphics
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

;
; VRAM allocation
;

VRAM_CHR_SHIPS  = $0000 ; - $4000 32k
VRAM_CHR_SPRITE = $4000 ; - $5000 8k
VRAM_CHR_WATER  = $5000 ; - $5800 4k
VRAM_NMT_WATER  = $5800 ; - $5C00 2k
;                 $5C00   - $6000 2k free
VRAM_NMT_SHIPS  = $6000 ; - $7000 8k
;                 $7000   - $8000 8k free

;
; Animation parameters
;

WATER_SPEED = 15
SCROLL0_SPEED = 24
SCROLL1_SPEED = 64
PARTICLE_COUNT = 16

;
; Header
;

.segment "HEADER"
.byte "TWOSHIP MODE 5 DEMO  "
.byte $21 ; map mode (HiROM)
.byte $00 ; cartridge type (ROM only)
.byte $09 ; 4mbit (512k)
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
mode:         .res 1
water:        .res 2
temp:         .res 2
scrollx:      .res 2
scrolly0:     .res 3
scrolly1:     .res 3
gamepad:      .res 2
lastpad:      .res 2
newpad:       .res 2
nmi_ready:    .res 1
seed:         .res 2
spriteon:     .res 1
pause:        .res 1

.segment "LORAM" ; <$2000

.segment "HIRAM" ; $FE bank < $8000
oam: .res (512 + 32)
pt_x0: .res PARTICLE_COUNT ; X position
pt_x1: .res PARTICLE_COUNT
pt_x2: .res PARTICLE_COUNT
pt_y0: .res PARTICLE_COUNT ; Y position
pt_y1: .res PARTICLE_COUNT
pt_f0: .res PARTICLE_COUNT ; animation frame
pt_f1: .res PARTICLE_COUNT
pt_vx0: .res PARTICLE_COUNT ; velocity
pt_vx1: .res PARTICLE_COUNT
pt_vx2: .res PARTICLE_COUNT
pt_vy0: .res PARTICLE_COUNT
pt_vy1: .res PARTICLE_COUNT
pt_vf0: .res PARTICLE_COUNT ; animation speed
pt_vf1: .res PARTICLE_COUNT
pt_c: .res PARTICLE_COUNT ; sprite column

.segment "RODATA" ; for data needed by code

;
; Macros
;

; macros for switching between RODATA and RAM data banks (.a8 assumed)

.import __RODATA_LOAD__
.import __RODATA_SIZE__
.assert (^__RODATA_LOAD__) = (^(__RODATA_LOAD__ + __RODATA_SIZE__)), error, "RODATA cannot cross a bank"

.macro DB_RODATA
	lda #^__RODATA_LOAD__
	pha
	plb
.endmacro

.macro DB_RAM
	lda #$7E
	pha
	plb
.endmacro

.macro DB_ZERO
	lda #0
	pha
	plb
.endmacro

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
	;
	sep #$30
	.a8
	.i8
	DB_ZERO
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
	; regular update
	; OAM DMA
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
	; animate water
	jsr dma_chr_water
	; set scroll
	; NOTE: interlaced scrolls half as fast due to the vertical compression
	;       but hi-res does not do the same horizontally
	lda z:mode
	bne :+
		; vertical (divided by 2)
		lda scrolly0+2
		lsr
		pha
		lda scrolly0+1
		ror
		sta $210E
		pla
		sta $210E ; BG1
		lda scrolly1+2
		lsr
		pha
		lda scrolly1+1
		ror
		sta $2112
		pla
		sta $2112 ; BG3
		; horizontal BG3
		lda scrollx+0
		sta $2111
		lda scrollx+1
		sta $2111
		jmp :++
	:
		; vertical
		lda scrolly0+1
		sta $210E
		lda scrolly0+2
		sta $210E ; BG1
		lda scrolly1+1
		sta $2110
		lda scrolly1+2
		sta $2110 ; BG2
		; horizontal BG2
		lda scrollx+0
		sta $210D
		lda scrollx+1
		sta $210D
		lda scrollx+0
		sta $210F
		lda scrollx+1
		sta $210F
	:
	; horizontal BG1
	lda scrollx+0
	sta $210D
	lda scrollx+1
	sta $210D
	; force blanking off
	lda #$0F
	sta $2100
	stz z:nmi_ready
@exit:
	; restore registers
	rep #$30
	.a16
	.i16
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
	stz $420D
	; clear vram
	stz $2116 ; vram $0000
	stz $2117
	lda #%00001001 ; no increment, 2-register
	sta $4300
	lda #$18 ; $2118
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
	; test palettes
	stz $2121 ; cgram $00
	lda #%00000010 ; increment, 1-register (x2)
	sta $4300
	lda #$22 ; $2122
	sta $4301
	ldx #16
	:
		lda #<pal_test
		sta $4302
		lda #>pal_test
		sta $4303
		lda #^pal_test
		sta $4304
		lda #32
		sta $4305
		stz $4306 ; 32 bytes
		lda #$01
		sta $420B ; DMA
		dex
		bne :-
	; shared water palette (0)
	stz $2121
	lda #<pal_water
	sta $4302
	lda #>pal_water
	sta $4303
	lda #^pal_water
	sta $4304
	lda #8
	sta $4305 ; 8 bytes
	lda #$01
	sta $420B ; DMA
	; shared sprite palette (8)
	lda #128
	sta $2121
	lda #<pal_sprite
	sta $4302
	lda #>pal_sprite
	sta $4303
	lda #^pal_sprite
	sta $4304
	lda #32
	sta $4305 ; 32 bytes
	lda #$01
	sta $420B ; DMA
	; shared sprite CHR
	lda #<VRAM_CHR_SPRITE
	sta $2116
	lda #>VRAM_CHR_SPRITE
	sta $2117
	lda #%00000001 ; increment, 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	lda #<chr_sprite
	sta $4302
	lda #>chr_sprite
	sta $4303
	lda #^chr_sprite
	sta $4304
	stz $4305
	lda #$20
	sta $4306 ; 8k
	lda #$01
	sta $420B ; DMA
	; sprite setup
	lda #(VRAM_CHR_SPRITE >> 13)
	sta $2101 ; 8x8 + 16x16 sprites
	; fill OAM buffer with $F0
	DB_RAM
	lda #$F0
	sta oam+0
	rep #$30
	.a16
	.i16
	ldx #.loword(oam)
	ldy #.loword(oam+1)
	lda #(512+32-2)
	mvn #^oam, #^oam ; fill with $F0
	sep #$30
	.a8
	.i8
	; setup variables
	stz z:mode
	stz z:nmi_ready
	stz z:gamepad+0
	stz z:gamepad+1
	stz z:lastpad+0
	stz z:lastpad+1
	stz z:scrollx+0
	stz z:scrollx+1
	stz z:scrolly0+0
	stz z:scrolly0+1
	stz z:scrolly0+2
	stz z:scrolly1+0
	stz z:scrolly1+1
	stz z:scrolly1+2
	stz z:spriteon
	stz z:pause
	; make sure RNG seed is nonzero
	lda z:seed+0
	ora z:seed+1
	bne :+
		inc z:seed+0
	:
	; randomize particles
	ldx #0
	:
		jsr particle_random
		inx
		cpx #PARTICLE_COUNT
		bcc :-
	; set up mode 1
	DB_ZERO
	jsr reload
	; begin
	jmp run

reload:
	lda z:mode
	bne :+
		lda #$01
		sta $2105 ; BG mode 1
		lda #((>VRAM_NMT_SHIPS) & $FC)
		sta $2107 ; BG1 address, 1-screen
		lda #((>VRAM_NMT_WATER) & $FC)
		sta $2109 ; BG3 address, 1-screen
		lda #(VRAM_CHR_SHIPS >> 12)
		sta $210B ; BG1 CHR address
		lda #(VRAM_CHR_WATER >> 12)
		sta $210C ; BG3 CHR address
		lda #$15
		sta $212C ; OBJ + BG1 + BG3 on main screen
		stz $212d ; sub screen off
		lda #$00
		sta $2133 ; no interlace, no overscan
		jmp :++
	:
		lda #$05
		sta $2105 ; BG mode 5
		lda #((>VRAM_NMT_SHIPS) & $FC) | 2
		sta $2107 ; BG1 address, vertical
		lda #((>VRAM_NMT_WATER) & $FC)
		sta $2108 ; BG2 address, 1-screen
		lda #(VRAM_CHR_SHIPS >> 12) | ((VRAM_CHR_WATER >> 8) & $F0)
		sta $210B ; BG1/BG2 CHR address
		lda #$13
		sta $212C ; OBJ + BG1 + BG2 on main screen
		sta $212D ; same on sub screen (hires)
		lda #$01
		sta $2133 ; interlace BG, no overscan
	:
	jsr dma_nmt_ships
	jsr dma_pal_ships
	jsr dma_chr_ships
	jsr make_nmt_water
	jmp dma_chr_water

dma_nmt_ships:
	lda z:mode
	bne :+
		lda #<nmt_ships1
		sta $4302
		lda #>nmt_ships1
		sta $4303
		lda #^nmt_ships1
		jmp :++
	:
		lda #<nmt_ships5
		sta $4302
		lda #>nmt_ships5
		sta $4303
		lda #^nmt_ships5
		;jmp :+
	:
	sta $4304
	lda #<VRAM_NMT_SHIPS
	sta $2116
	lda #>VRAM_NMT_SHIPS
	sta $2117
	lda #%00000001 ; increment, 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	stz $4305
	lda #$10
	sta $4306 ; 4k
	lda #$01
	sta $420B ; DMA
	rts
	

dma_pal_ships:
	lda z:mode
	bne :+
		lda #<pal_ships1
		sta $4302
		lda #>pal_ships1
		sta $4303
		lda #^pal_ships1
		jmp :++
	:
		lda #<pal_ships5
		sta $4302
		lda #>pal_ships5
		sta $4303
		lda #^pal_ships5
		;jmp :+
	:
	sta $4304
	lda #16
	sta $2121 ; cgram palette (1)
	lda #%00000010 ; increment, 1-register (x2)
	sta $4300
	lda #$22
	sta $4301 ; $2122
	lda #32
	sta $4305
	stz $4306 ; 32 bytes
	lda #$01
	sta $420B ; DMA
	rts

dma_chr_ships:
	lda z:mode
	bne :+
		lda #<chr_ships1
		sta $4302
		lda #>chr_ships1
		sta $4303
		lda #^chr_ships1
		jmp :++
	:
		lda #<chr_ships5
		sta $4302
		lda #>chr_ships5
		sta $4303
		lda #^chr_ships5
		;jmp :+
	:
	sta $4304
	lda #<VRAM_CHR_SHIPS
	sta $2116
	lda #>VRAM_CHR_SHIPS
	sta $2117
	lda #%00000001 ; increment, 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	stz $4305
	lda #$80
	sta $4306 ; 32k
	lda #$01
	sta $420B ; DMA
	rts

make_nmt_water:
	lda #<VRAM_NMT_WATER
	sta $2116
	lda #>VRAM_NMT_WATER
	sta $2117
	ldy #0
@row:
	lda z:mode
	bne :+
		lda #$07
		sta z:temp+0 ; mask X = 0-7
		tya
		jmp :++
	:
		lda #$0E
		sta z:temp+0 ; mask X = 0-7 x 2
		tya
		asl
	:
	asl
	asl
	asl
	sta z:temp+1 ; or Y = row * 8 or 16
	ldx #0
@column:
	lda z:mode
	bne :+
		txa
		jmp :++
	:
		txa
		asl
	:
	and z:temp+0
	ora z:temp+1
	sta $2118
	stz $2119
	inx
	cpx #32
	bcc @column
	iny
	cpy #32
	bcc @row
	rts

dma_chr_water:
	; block offset = water * 4k
	lda z:water+1
	and #15
	asl
	asl
	asl
	asl
	ldx z:mode
	clc
	bne :+
		adc #>chr_water1
		sta $4303
		lda #0
		adc #^chr_water1
		sta $4304
		lda #<chr_water1
		jmp :++
	:
		adc #>chr_water5
		sta $4303
		lda #0
		adc #^chr_water5
		sta $4304
		lda #<chr_water5
		;jmp :+
	:
	sta $4302
	lda #<VRAM_CHR_WATER
	sta $2116
	lda #>VRAM_CHR_WATER
	sta $2117
	lda #%00000001 ; increment, 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	stz $4305
	lda #$10
	sta $4306 ; 4k
	lda #$01
	sta $420B ; DMA
	rts

; simple 16-bit LFSR
prng:
	rep #$20
	.a16
	ldy #8
	lda z:seed
:
	asl
	bcc :+
	eor #$0039
:
	dey
	bne :--
	sta z:seed
	sep #$20
	.a8
	rts

particle_random: ; X = particle
	jsr prng
	sta a:pt_x0, X
	jsr prng
	sta a:pt_x1, X
	stz a:pt_x2, X ; always onscreen
	jsr prng
	sta a:pt_y0, X
	jsr prng
	sta a:pt_y1, X
	jsr prng
	sta a:pt_f0, X
	jsr prng
	and #7
	cmp #6
	bcc :+
		;sec
		sbc #6
	:
	sta a:pt_f1, X
	jsr prng
	and #127
	sta a:pt_vx0, X
	jsr prng
	and #1
	beq :+
		jsr prng
		and #127
		eor #$FF
		sta a:pt_vx0, X
		lda #$FF
	:
	sta a:pt_vx1, X
	sta a:pt_vx2, X
	jsr prng
	and #127
	clc
	adc #32
	sta a:pt_vy0, X
	stz a:pt_vy1, X
	jsr prng
	and #63
	clc
	adc #16
	sta a:pt_vf0, X
	stz a:pt_vf1, X
	jsr prng
	sta a:pt_c, X
	rts

particles_animate:
	phb
	DB_RAM
	ldx #0
@loop:
	; update X
	lda a:pt_x0, X
	clc
	adc a:pt_vx0, X
	sta a:pt_x0, X
	lda a:pt_x1, X
	adc a:pt_vx1, X
	sta a:pt_x1, X
	lda a:pt_x2, X
	adc a:pt_vx2, X
	sta a:pt_x2, X
	lda a:pt_y0, X
	clc
	adc a:pt_vy0, X
	sta a:pt_y0, X
	lda a:pt_y1, X
	adc a:pt_vy1, X
	sta a:pt_y1, X
	cmp #$F0 ; if Y = $F0: randomize and return to top
	bne :+
		jsr particle_random
		lda #$F1
		sta a:pt_y1, X
	:
	; update animation
	lda a:pt_f0, X
	clc
	adc a:pt_vf0, X
	sta a:pt_f0, X
	lda a:pt_f1, X
	adc a:pt_vf1, X
	sta a:pt_f1, X
	sec
	sbc #6 ; wrap at 6
	bcc :+
		sta a:pt_f1, X
	:
	inx
	cpx #PARTICLE_COUNT
	bcc @loop
	plb
	rts

particles_draw:
	phb
	DB_RAM
	ldx #0
	ldy #0
@draw:
	lda a:pt_x1, X
	sta a:oam+0, Y
	lda a:pt_x2, X ; 0 main screen, -1 off the left, otherwise hide
	beq :+
		cmp #$FF
		bne @hide
	:
	lda a:pt_y1, X
	bit z:spriteon
	bpl @show
@hide:
	lda #$F0
@show:
	sta a:oam+1, Y
	lda a:pt_c, X
	and #$06
	sta z:temp ; sprite column: 0 2 4 6
	lda a:pt_f1, X
	asl
	asl
	asl
	asl
	asl
	ora z:temp ; sprite row
	sta a:oam+2, Y
	lda a:pt_c, X ; h flip
	and #$40
	ora #$30 ; high priority
	sta a:oam+3, Y
	iny
	iny
	iny
	iny
	inx
	cpx #PARTICLE_COUNT
	bcc @draw
	ldx #0
	ldy #0
@drawh:
	.repeat 4, I
		lda a:pt_x2+I, X
		ror ; high X
		ror z:temp
		sec ; large sprite
		ror z:temp
	.endrepeat
	lda z:temp
	sta oam+512, Y
	iny
	inx
	inx
	inx
	inx
	cpx #PARTICLE_COUNT
	bcc @drawh
	plb
	rts

;
; main loop
;

run:
	lda #$81
	sta $4200 ; enable NMI and auto-joypad
@loop:
	; post a render update
	lda #1
	sta z:nmi_ready
	wai
	:
		lda z:nmi_ready
		bne :-
	; read controllers
	DB_ZERO
	lda z:gamepad+0
	sta z:lastpad+0
	lda z:gamepad+1
	sta z:lastpad+1
	lda $4218
	sta z:gamepad+0
	eor z:lastpad+0
	and z:gamepad+0
	sta z:newpad+0
	lda $4219
	sta z:gamepad+1
	eor z:lastpad+1
	and z:gamepad+1
	sta z:newpad+1
	; controls
	lda z:newpad+1
	and #$80 ; B
	bne @mode1
	lda z:newpad+1
	and #$40 ; Y
	bne @mode5
	lda z:newpad+1
	and #$20 ; select
	beq :+
		lda z:spriteon
		eor #$FF
		sta z:spriteon
	:
	lda z:newpad+1
	and #$10 ; start
	beq :+
		lda z:pause
		eor #$FF
		sta z:pause
	:
	lda z:newpad+0
	and #$90 ; A/L
	bne @mode5
	lda z:newpad+0
	and #$60 ; X/R
	bne @mode1
	lda z:gamepad+1
	and #$08 ; up
	beq @up_end
		lda z:scrolly0+1
		bne :+
			dec z:scrolly0+2
		:
		dec z:scrolly0+1
		lda z:scrolly1+1
		bne :+
			dec z:scrolly1+2
		:
		dec z:scrolly1+1
	@up_end:
	lda z:gamepad+1
	and #$04 ; down
	beq @down_end
		inc z:scrolly0+1
		bne :+
			inc z:scrolly0+2
		:
		inc z:scrolly1+1
		bne :+
			inc z:scrolly1+2
		:
	@down_end:
	lda z:gamepad+1
	and #$02 ; left
	beq @left_end
		lda z:scrollx+0
		bne :+
			dec z:scrollx+1
		:
		dec z:scrollx+0
	@left_end:
	lda z:gamepad+1
	and #$01 ; right
	beq @right_end
		inc z:scrollx+0
		bne :+
			inc z:scrollx+1
		:
	@right_end:
	jmp @draw
@mode1:
	stz z:mode
	jmp @reload
@mode5:
	lda #1
	sta z:mode
	;jmp @reload
@reload:
	; enter forced blank
	lda #2
	sta z:nmi_ready
	wai
	:
		lda z:nmi_ready
		bne :-
	jsr reload
	jmp @loop
@draw:
	; redraw sprites
	jsr particles_draw
	; don't animate if paused
	lda z:pause
	beq :+
		jmp @loop
	:
	; animate water
	lda z:water+0
	clc
	adc #WATER_SPEED
	sta z:water+0
	bcc :+
		inc z:water+1
	:
	; animate scroll
	lda z:scrolly0+0
	clc
	adc #<SCROLL0_SPEED
	sta z:scrolly0+0
	lda z:scrolly0+1
	adc #>SCROLL0_SPEED
	sta z:scrolly0+1
	lda z:scrolly0+2
	adc #^SCROLL0_SPEED
	sta z:scrolly0+2
	lda z:scrolly1+0
	clc
	adc #<SCROLL1_SPEED
	sta z:scrolly1+0
	lda z:scrolly1+1
	adc #>SCROLL1_SPEED
	sta z:scrolly1+1
	lda z:scrolly1+2
	adc #^SCROLL1_SPEED
	sta z:scrolly1+2
	; animate sprites
	jsr particles_animate
	jmp @loop

;
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

; if the assert happens, add some padding or an align to keep these from crossing a bank
.macro BANKCHECK label_
	.assert (^(*-1))=(^(label_)), error, "BANKCHECK fail."
.endmacro

chr_water5: .incbin "water5.chr"
BANKCHECK chr_water5

chr_water1: .incbin "water1.chr"
BANKCHECK chr_water1

chr_ships5: .incbin "ships5.chr"
chr_ships1: .incbin "ships1.chr"
chr_sprite: .incbin "sprite.chr"

nmt_ships1: .incbin "ships1.nmt"
nmt_ships5: .incbin "ships5.nmt"

pal_ships1: .incbin "ships1.pal"
pal_ships5: .incbin "ships5.pal"
pal_sprite: .incbin "sprite.pal"
pal_water:  .incbin "water.pal"

byte00: .byte $00

pal_test: ; colours suitable for debugging
	.repeat 16, I
		.byte I | (I<<4), I | (I<<4)
	.endrepeat

BANKCHECK chr_ships5
