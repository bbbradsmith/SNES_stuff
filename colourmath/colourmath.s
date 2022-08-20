; colourmath demo
;
; Cycles through a few colour math demonstrations.
; Press any button to cycle.
;
; 1. add + half 50% blend, "water"
;    Main = BG + OBJ + Water
;    Sub  = BG + OBJ
; 2. add light, "ghost fade 1"
;    Main = OBJ
;    Sub  = BG
; 3. add dark, "ghost fade 2"
;    (Same as 2 with darker ghost palette.)
; 4. subtract, "darkness, some sprites"
;    Main = BG + OBJ
;    Sub  = Darkness
; 5. subtract inverse, "darkness, all sprites"
;    Main = Inverse Darkness
;    Sub  = Inverse BG + Inverse OBJ
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
.byte "COLOUR MATH DEMO     "
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
mode:         .res 1
screenx:      .res 2
screeny:      .res 2
temp:         .res 8
oam_pos:      .res 1
ptr:          .res 4

.segment "LORAM" ; <$2000

oam:          .res 512+32

.segment "HIRAM" ; $FE bank < $8000

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

bin_vram: ; <= 64k block to initialize vram in one shot

; sprite CHR 16k aligned
.align $4000
chr_obj: .incbin "obj.chr"

; tilemap CHR 8k aligned
.align $2000
chr_bg1: .incbin "bg1.chr"
.align $2000
chr_bg2: .incbin "bg2.chr"
.align $2000
chr_bg3: .incbin "bg3.chr"

; tilemaps 2k aligned
.align $800
nmt_bg1: .incbin "bg1.nmt"
.align $800
nmt_bg2: .incbin "bg2.nmt"
.align $800
nmt_bg3: .incbin "bg3.nmt"

BANKCHECK bin_vram

VRAM_CHR_OBJ = (chr_obj-bin_vram)>>1
VRAM_CHR_BG1 = (chr_bg1-bin_vram)>>1
VRAM_CHR_BG2 = (chr_bg2-bin_vram)>>1
VRAM_CHR_BG3 = (chr_bg3-bin_vram)>>1
VRAM_NMT_BG1 = (nmt_bg1-bin_vram)>>1
VRAM_NMT_BG2 = (nmt_bg2-bin_vram)>>1
VRAM_NMT_BG3 = (nmt_bg3-bin_vram)>>1

; palettes
pal: .incbin "pal.pal"
BANKCHECK pal

;
; =============================================================================
; LOPRG stuff for the first bank (mapped at $C00000)
;

; used for all code in this demo program
.segment "LOPRG"

;
; =============================================================================
; NMI / render
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
		stz z:nmi_ready
		jmp @exit
	:
	; OAM DMA
	stz a:$2102
	stz a:$2103
	lda #%00000010 ; increment, 1-register (x2)
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

picture_off:
	ldx #2
	bra :+
picture_on:
	ldx #1
:
	stx z:nmi_ready
	wai
	:
		ldx z:nmi_ready
		bne :-
	rts

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
	lda #<pal
	sta a:$4302
	lda #>pal
	sta a:$4303
	lda #^pal
	sta a:$4304
	stz a:$4305
	lda #>512
	sta a:$4306 ; 512 bytes
	lda #$01
	sta a:$420B ; DMA
	; setup OAM
	lda #$E0
	sta a:oam+0
	lda #$AA
	sta a:oam+512 ; large sprites
	rep #$30
	.a16
	.i16
	phb
	ldx #.loword(oam+0)
	ldy #.loword(oam+1)
	lda #(512-2)
	mvn #^oam,#^oam ; fill with E0
	ldx #.loword(oam+512)
	ldy #.loword(oam+513)
	lda #(64-2)
	mvn #^oam,#^oam ; fill with AA
	plb
	sep #$30
	.a8
	.i8
	; setup PPU addresses
	lda #((>VRAM_NMT_BG1) & $FC)
	sta a:$2107 ; BG1SC nametable, 1-screen
	lda #((>VRAM_NMT_BG2) & $FC)
	sta a:$2108 ; BG2SC nametable, 1-screen
	lda #((>VRAM_NMT_BG3) & $FC)
	sta a:$2109 ; BG3SC nametable, 1-screen
	lda #((VRAM_CHR_BG2 >> 12) << 4) | (VRAM_CHR_BG1 >> 12)
	sta a:$210B ; BG12NBA
	lda #                              (VRAM_CHR_BG3 >> 12)
	sta a:$210C ; BG34NBA
	lda #((VRAM_CHR_OBJ >> 13) | $20)
	sta a:$2101 ; OBJSEL 8x8 + 32x32 sprites
	; begin
	jmp run

;
; =============================================================================
; OAM sprites
;

oam_start:
	.i8
	ldx #0
	stx z:oam_pos
	rts

oam_finish:
	.i8
	php
	sep #$20
	.a8
	lda #$E0
	ldx z:oam_pos
	:
		sta a:oam+1, X
		inx
		inx
		inx
		inx
		bne :-
	plp
	rts

oam_sprite: ; ptr = definition, screenx, screeny
	.a16
	.i8
	ldx z:oam_pos
	ldy #0
	lda [ptr], Y
	and #$00FF
	clc
	adc z:screenx
	sta a:oam+0, X
	iny
	lda [ptr], Y
	and #$00FF
	clc
	adc z:screeny
	sta a:oam+1, X
	iny
	lda [ptr], Y
	sta a:oam+2, X
	lda ptr+0
	clc
	adc #4
	sta ptr+0
	bcc :+
		inc ptr+2
	:
	inx
	inx
	inx
	inx
	stx z:oam_pos
	rts

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
	ldx z:mode
	jmp @load
@loop:
	jsr picture_on
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
	beq @loop
	jsr picture_off
	; load next image
	ldx z:mode
	inx
	cpx #5
	bcc :+
		ldx #0
	:
	stx z:mode
@load:
	sep #$20
	.a8
	; register states
	lda f:table_tm, X
	sta a:$212C ; TM
	lda f:table_ts, X
	sta a:$212D ; TS
	lda f:table_cgadsub, X
	sta a:$2131 ; CGADSUB
	lda #$02
	sta a:$2130 ; CGWSEL subscreen
	lda #$09
	sta a:$2105 ; BGMODE 1, BG3 high priority
	lda #8
	sta a:$210E
	sta a:$210E ; BG1VOFS
	sta a:$2110
	sta a:$2110 ; BG2VOFS
	lda #10
	sta a:$2112
	sta a:$2112 ; BG3VOFS
	; restore or invert BG1/BG2 colours (via attribute table)
	lda #<VRAM_NMT_BG1
	sta a:$2116
	lda #>VRAM_NMT_BG1
	sta a:$2117
	rep #$10
	.i16
	ldy #0
	:
		tyx
		lda f:nmt_bg1+1, X
		sta z:temp
		ldx z:mode
		lda f:table_invert, X
		eor z:temp
		sta a:$2119
		iny
		iny
		cpy #2048
		bcc :-
	lda #<VRAM_NMT_BG2
	sta a:$2116
	lda #>VRAM_NMT_BG2
	sta a:$2117
	rep #$10
	.i16
	ldy #0
	:
		tyx
		lda f:nmt_bg2+1, X
		sta z:temp
		ldx z:mode
		lda f:table_invert, X
		eor z:temp
		sta a:$2119
		iny
		iny
		cpy #2048
		bcc :-
	sep #$10
	.i8
	; CGRAM color 0 inversion
	stz a:$2121 ; CGADD
	lda f:table_invert, X
	beq :+
		lda f:pal + ($40 * 2) + 0
		sta a:$2122
		lda f:pal + ($40 * 2) + 1
		sta a:$2122 ; CGDATA
		bra :++
	:
		stz a:$2122
		stz a:$2122
	:
	; sprites
	rep #$20
	.a16
	lda #0
	txa
	asl
	asl
	tax
	lda f:table_sprites+0, X
	sta z:ptr+0
	lda f:table_sprites+2, X
	sta z:ptr+2
	lda #0
	sta z:screenx
	lda #.loword(-9)
	sta z:screeny
	jsr oam_start
	:
		ldx z:ptr+3
		beq :+
		dex
		stx z:ptr+3
		jsr oam_sprite
		bra :-
	:
	jsr oam_finish
	jmp @loop

table_tm:      .byte $15, $10, $10, $11, $02
table_ts:      .byte $11, $01, $01, $02, $11
table_cgadsub: .byte $7F, $3F, $3F, $BF, $BF
table_invert:  .byte $00, $00, $00, $00, $10
table_sprites:
	.dword table_sprite0 | (2 << 24)
	.dword table_sprite1 | (5 << 24)
	.dword table_sprite2 | (5 << 24)
	.dword table_sprite3 | (4 << 24)
	.dword table_sprite4 | (4 << 24)

table_sprite0:
	.byte  40, 144, $00, $20 | (0<<1)
	.byte 173, 144, $48, $20 | (2<<1)
table_sprite1:
	.byte  75, 144, $04, $20 | (0<<1)
	.byte 185, 144, $44, $20 | (2<<1)
	.byte 126,  74, $80, $20 | (4<<1)
	.byte 116,  78, $88, $20 | (4<<1)
	.byte 101, 146, $84, $20 | (4<<1)
;	.byte 105, 186, $C0, $20 | (4<<1)
table_sprite2:
	.byte  75, 144, $0C, $20 | (0<<1)
	.byte 185, 144, $40, $20 | (2<<1)
	.byte 126,  74, $80, $20 | (6<<1)
	.byte 116,  78, $88, $20 | (6<<1)
	.byte 101, 146, $84, $20 | (6<<1)
;	.byte 105, 186, $C0, $20 | (6<<1)
table_sprite3:
	.byte 106,  80, $08, $20 | (0<<1)
	.byte 169, 144, $4C, $20 | (2<<1)
	.byte  58, 145, $84, $20 | (4<<1)
	.byte 172,  56, $88, $20 | (4<<1)
table_sprite4:
	.byte 106,  80, $08, $20 | (1<<1)
	.byte 169, 144, $4C, $20 | (3<<1)
	.byte  58, 145, $84, $20 | (5<<1)
	.byte 172,  56, $88, $20 | (5<<1)
