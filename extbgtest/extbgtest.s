; extbgtest
;
; Test of Mode 7 EXTBG
; Shows colours and layering of Mode 7 with EXTBG
;
; The row marked 0 uses pixel values from 0-127
; The row marked 1 uses pixel values from 128-255
; The BG palettes for 128+ are darkened to show that BG1 is using them even with EXTBG enabled
;
; Select = toggle direct color
; Start = toggle BG1 or BG2
; A/B/Y/X = select sprite to move
; d-pad = move sprite
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
.byte "EXTBGTEST            "
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
bg1:          .res 1
sprite:       .res 1

.segment "LORAM" ; <$2000

oam:          .res 512+32

.segment "HIRAM" ; $FE bank < $8000

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
	lda #%00001001 ; no-increment, 2-register
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
	; load sprites
	lda #<VRAM_CHR_SPRITE
	sta $2116
	lda #>VRAM_CHR_SPRITE
	sta $2117
	lda #%00000001 ; 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	lda #<chr_sprite
	sta $4302
	lda #>chr_sprite
	sta $4303
	lda #^chr_sprite
	sta $4304
	lda #<CHR_SPRITE_SIZE
	sta $4305
	lda #>CHR_SPRITE_SIZE
	sta $4306
	lda #$01
	sta $420B ; DMA
	; load palettes
	stz $2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta $4300
	lda #$22 ; $2122 CGDATA
	sta $4301
	lda #<pal
	sta $4302
	lda #>pal
	sta $4303
	lda #^pal
	sta $4304
	stz $4305
	lda #>512
	sta $4306 ; 512 bytes
	lda #$01
	sta $420B ; DMA
	; setup PPU
	lda #((VRAM_CHR_SPRITE >> 13) | $A0)
	sta $2101 ; OBJSEL 32x32 + 64x64 sprites
	stz $212D ; TS empty sub-screen
	lda #$07
	sta $2105 ; BGMODE 7
	lda #$40
	sta $2133 ; SETINI EXTBG
	; setup OAM
	; 1. fill with E0
	lda #$E0
	sta a:oam+0
	rep #$30
	.a16
	.i16
	ldx #.loword(oam+0)
	ldy #.loword(oam+1)
	lda #(512-2)
	mvn #^oam,#^oam
	; 2. copy table
	ldx #.loword(oam_setup)
	ldy #.loword(oam)
	lda #OAM_SETUP_SIZE-1
	mvn #^oam_setup,#^oam
	; DB=0
	sep #$30
	.a8
	.i8
	; begin
	jmp run

;
; utility
;

render_on:
	.i8
	ldx #1
	stx z:nmi_ready
render_wait:
	wai
	:
		ldx z:nmi_ready
		bne :-
	rts

render_off:
	.i8
	ldx #2
	stx z:nmi_ready
	bra render_wait

reload_mode:
	.a16
	.i8
	jsr render_off
	ldx z:mode
	bne :+
		ldx #$40
		stx oam+$10+2 ; indexed sprite
		lda #.loword(md7_index)
		sta $4302
		ldx #^md7_index
		bra :++
	:
		ldx #$44
		stx oam+$10+2 ; direct sprite
		lda #.loword(md7_direct)
		sta $4302
		ldx #^md7_direct
	:
	stx $4304
	stz $2116 ; VMADD $0000
	ldx #%00000001 ; 2-register
	stx $4300
	ldx #$18 ; $2118 VMDATA
	stx $4301
	lda #(32*1024)
	sta $4305
	ldx #$01
	stx $420B ; DMA
	ldx z:mode
	stx $2130 ; CGWSEL select direct color
	ldx z:bg1
	bne :+
		lda #$12
		sta $212C ; TM OBJ + BG2 main-screen
		ldx #$4C
		stx oam+$14+2 ; BG2 sprite
		; TODO TM
		bra :++
	:
		lda #$11
		sta $212C ; TM OBJ + BG1 main-screen
		ldx #$48
		stx oam+$14+2 ; BG1 sprite
		; TODO TM
	:
	rts

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
	jsr reload_mode
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
	; input
	lda z:newpad
	and #$2000 ; select switches indexed/direct color
	beq :+
		lda z:mode
		eor #1
		tax
		stx z:mode
		jsr reload_mode
		jmp @loop
	:
	lda z:newpad
	and #$1000 ; start switches BG2 vs. BG1
	beq :+
		lda z:bg1
		eor #1
		tax
		stx z:bg1
		jsr reload_mode
		jmp @loop
	:
	; A/B/Y/X selects 4 sprites for moving
	lda z:gamepad
	and #$0080 ; A
	beq :+
		ldx #0
		stx z:sprite
	:
	lda z:gamepad
	and #$8000 ; B
	beq :+
		ldx #4
		stx z:sprite
	:
	lda z:gamepad
	and #$4000 ; Y
	beq :+
		ldx #8
		stx z:sprite
	:
	lda z:gamepad
	and #$0040 ; X
	beq :+
		ldx #12
		stx z:sprite
	:
	; left/right/up/down moves sprite
	sep #$20
	.a8
	lda z:gamepad+1
	and #$08 ; up
	beq :+
		ldx z:sprite
		dec a:oam+1, X
	:
	lda z:gamepad+1
	and #$04 ; down
	beq :+
		ldx z:sprite
		inc a:oam+1, X
	:
	lda z:gamepad+1
	and #$02 ; left
	beq :+
		ldx z:sprite
		dec a:oam+0, X
	:
	lda z:gamepad+1
	and #$01 ; right
	beq :+
		ldx z:sprite
		inc a:oam+0, X
	:
	rep #$20
	.a16
	jmp @loop

;
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

.align $10000

md7_index:  .incbin "index.md7"
md7_direct: .incbin "direct.md7"

.align $10000

chr_sprite: .incbin "sprite.chr"
CHR_SPRITE_SIZE = *-chr_sprite
VRAM_CHR_SPRITE  = $4000

pal: .incbin "pal.pal"
byte00: .byte $00

oam_setup:
.byte  40, 56, $00, $0E ; priority 0-3 sprites
.byte  88, 56, $04, $1E
.byte 136, 56, $08, $2E
.byte 176, 56, $0C, $3E
.byte  16,100, $40, $3E ; index/direct sprite
.byte 218,100, $4C, $3E ; BG1/BG2 sprite
OAM_SETUP_SIZE = *-oam_setup
