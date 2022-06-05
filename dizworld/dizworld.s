; dizworld Mode 7 demo
; TODO
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
.byte $21 ; map mode (HiROM)
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

nmi_bgmode:   .res 1
nmi_hofs:     .res 2
nmi_vofs:     .res 2
nmi_m7t:      .res 8
nmi_m7x:      .res 2
nmi_m7y:      .res 2

new_hdma_en:  .res 1 ; HDMA channel enable at next update
nmi_hdma_en:  .res 1 ; HDMA channel enable currently

.segment "LORAM" ; <$2000
.segment "HIRAM" ; $FE bank < $8000

new_hdma: .res 16 * 8 ; HDMA channel settings to apply at next update
nmi_hdma: .res 16 * 8 ; HDMA channel settings current

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
	phd
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
	stz $420D
	; TODO fastROM
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
	; TODO use a16,i8 instead?
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
	; clear OAM
	; TODO RAM OAM instead and get rid of this
	stz $2102 ; OAMADD 0
	stz $2103
	lda #%00001010 ; no-increment, 1-register
	sta $4300
	lda #$04 ; $2104 OAMDATA
	sta $4301
	lda #<byteE0 ; E0 = all sprites offscreen
	sta $4302
	lda #>byteE0
	sta $4303
	lda #^byteE0
	sta $4304
	stz $4305
	lda #>512
	sta $4306 ; 512 bytes
	lda #$01
	sta $420B ; DMA
	lda #<byteAA ; AA = all sprites size = 32, X = onscreen
	sta $4302
	lda #>byteAA
	sta $4303
	lda #^byteAA
	sta $4304
	lda #32
	sta $4305
	stz $4306 ; 32 bytes
	lda #$01
	sta $420B ; DMA
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
	; reset mode 7 transform matrix to identity (1,0) (0,1)
	ldx #$01
	stx nmi_m7t+1
	stx nmi_m7t+7
	jsr set_mode_a
@loop:
	; post a render update
	ldx #1
	stx z:nmi_ready
	wai
	:
		ldx z:nmi_ready
		bne :-
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
	bra @loop
@mode_b:
	jsr mode_b
	bra @loop
@mode_x:
	jsr mode_x
	bra @loop
@mode_y:
	jsr mode_y
	bra @loop
@set_mode_a:
	ldx #0
	stx z:mode
	jsr set_mode_a
	bra @loop
@set_mode_b:
	ldx #1
	stx z:mode
	jsr set_mode_b
	bra @loop
@set_mode_x:
	ldx #2
	stx z:mode
	jsr set_mode_x
	bra @loop
@set_mode_y:
	ldx #3
	stx z:mode
	jsr set_mode_y
	jmp @loop

;
; Common
;

simple_scroll:
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

;
; Mode A test "Overhead, simple"
; - Map spins around a fixed point
; - Player moves over the rotated map
; - L/R apply scale?
;

set_mode_a:
	.a16
	.i8
	ldx #7
	stx z:nmi_bgmode
	ldx #0
	stx new_hdma_en
mode_a:
	; TODO spin around a fixed point
	jmp simple_scroll

;
; Mode B test "Overhead, first person"
; - Map rotates around the player
; - Player faces "up", and controls spin
;

set_mode_b:
	.a16
	.i8
	ldx #7
	ldx #0 ; mode 0 HACK
	stx z:nmi_bgmode
	ldx #0
	stx new_hdma_en
mode_b:
	jsr simple_scroll ; TODO
	rts

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
