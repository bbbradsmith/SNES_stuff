; elasticity flicker demo
; press B for flicker
; press Y for ditherless 8bpp
; press A for floyd-steinberg 8bpp
; press X for 4:4:4 additive 12bpp
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

;
; VRAM allocation
;

VRAM_CHR0       = $0000
VRAM_CHR1       = $4000
VRAM_NMT        = $7C00

;
; Animation parameters
;

;
; Header
;

.segment "HEADER"
.byte "ELASTICITY FLICKER   "
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
field:        .res 1
gamepad:      .res 2
lastpad:      .res 2
newpad:       .res 2
nmi_ready:    .res 1
ppal0:        .res 4
ppal1:        .res 4
pchr0:        .res 4
pchr1:        .res 4

.segment "LORAM" ; <$2000
.segment "HIRAM" ; $FE bank < $8000

.segment "RODATA" ; for data needed by code

mode_table:
.dword pal0, pal1, chr0, chr1
.dword pal2, pal2, chr2, chr2
.dword pal3, pal3, chr3, chr3
.dword pal4, pal4, chr4, chr5

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
	; palette DMA and CHR switch
	lda z:mode
	cmp #3
	beq :+ ; mode 3 does not switch CHR
	lda z:field
	and #1
	bne :++
	:
		lda #(VRAM_CHR0 >> 12) | (VRAM_CHR1 >> 8)
		sta $210B ; BG1|2 CHR address
		lda z:ppal0+0
		sta $4302
		lda z:ppal0+1
		sta $4303
		lda z:ppal0+2
		jmp :++
	:
		lda #(VRAM_CHR1 >> 12) | (VRAM_CHR1 >> 8)
		sta $210B ; BG1|2 CHR address
		lda z:ppal1+0
		sta $4302
		lda z:ppal1+1
		sta $4303
		lda z:ppal1+2
	:
	sta $4304
	stz $2121 ; cgram index
	lda #%00000010 ; increment, 1-register (x2)
	sta $4300
	lda #$22
	sta $4301 ; $2122
	lda #<512
	sta $4305
	lda #>512
	sta $4306 ; 512 bytes
	lda #$01
	sta $420B ; DMA
	; HDMA 1/2 for scroll split
	lda #%00000010 ; 1-register (x2)
	sta $4310
	lda #$0E ; $210E BG1 scroll Y
	sta $4311
	lda #<hdma_scroll_y
	sta $4312
	lda #>hdma_scroll_y
	sta $4313
	lda #^hdma_scroll_y
	sta $4314
	lda #%00000010 ; 1-register (x2)
	sta $4320
	lda #$10 ; $2110 BG2 scroll Y
	sta $4321
	lda #<hdma_scroll_y
	sta $4322
	lda #>hdma_scroll_y
	sta $4323
	lda #^hdma_scroll_y
	sta $4324
	; begin HDMA
	lda #%00000110
	sta $420C
	; force blanking off
	lda #$0F
	sta $2100
	stz z:nmi_ready
	; next field
	inc z:field
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
	; setup variables
	stz z:field
	stz z:nmi_ready
	stz z:gamepad+0
	stz z:gamepad+1
	stz z:lastpad+0
	stz z:lastpad+1
	lda #3
	sta z:mode
	; begin
	jmp run

reload:
	; load pointers to data
	DB_RODATA
	lda z:mode
	asl
	asl
	asl
	asl
	tay
	ldx #0
	:
		lda mode_table, Y
		sta z:ppal0, X
		iny
		inx
		cpx #16
		bcc :-
	; DMA CHR
	DB_ZERO
	lda #<VRAM_CHR0
	sta $2116
	lda #>VRAM_CHR0
	sta $2117
	lda #%00000001 ; increment, 2-register
	sta $4300
	lda #$18 ; $2118
	sta $4301
	lda pchr0+0
	sta $4302
	lda pchr0+1
	sta $4303
	lda pchr0+2
	sta $4304
	stz $4305
	lda #>(32*1024)
	sta $4306 ; 32k
	lda #$01
	sta $420B ; DMA
	lda #<VRAM_CHR1
	sta $2116
	lda #>VRAM_CHR1
	sta $2117
	lda pchr1+0
	sta $4302
	lda pchr1+1
	sta $4303
	lda pchr1+2
	sta $4304
	stz $4305
	lda #>(32*1024)
	sta $4306 ; 32k
	lda #$01
	sta $420B ; DMA
	; setup tilemap, doing some gymnastics to fit in an area not needed for tiles
	; 000-040 = used for image tile
	; 200-240 = used for image tile
	; 1C0-200 = used for blank tile
	; 3C0-400 = used for blank file
	; 040-1C0 = used for image nametable
	; 240-3C0 = used for blank nametable
	; top nametable has image
	rep #$20
	.a16
	; upper tilemap
	lda #(VRAM_NMT+$040)
	sta $2116
	; 11 rows of image
	ldy #0 ; Y = line
	lda #$00
	@row:
		ldx #3
		jsr @xblanks ; 5 blanks on left
		ldx #11 ; 11 sequential tiles in middle
		@seq:
			sta $2118
			inc
			inc
			pha
			and #$0F ; every 16th tile advances a row
			bne :+
				pla
				clc
				adc #16
				pha
			:
			pla
			dex
			bne @seq
		ldx #18
		jsr @xblanks ; blanks on right
		iny
		cpy #11
		bcc @row
	; 128 bytes of blank tile
	ldx #64
	jsr @xzeros
	; lower tilemap
	lda #(VRAM_NMT+$240)
	sta $2116
	; 11 rows of blank
	ldy #0
	:
		ldx #32
		jsr @xblanks
		iny
		cpy #11
		bcc :-
	; 128 bytes of blank tile
	ldx #64
	jsr @xzeros
	sep #$20
	.a8
	; video registers
	lda #$33
	sta $2105 ; mode 3, 16x16 tiles
	lda #((>VRAM_NMT) & $FC)
	sta $2107 ; BG1 address, 1-screen
	sta $2108 ; BG2 address, 1-screen
	lda #$01
	sta $212C ; BG1 on main screen
	stz $212D ; sub screen off
	stz $2113 ; no interlace, no overscan
	lda #$30
	sta $2130 ; color math off
	lda z:mode
	cmp #3
	bne :+
		lda #$02
		sta $212D ; BG2 on sub screen
		lda #$02
		sta $2130 ; color math on, use subscreen
		lda #$3F
		sta $2131 ; additive color math, all layers
	:
	; set X scroll to 8
	lda #8
	sta $210D
	stz $210D
	sta $210F
	stz $210F
	; set Y scroll to 256+8 (lower half of tilemap + 8 lines for overscan)
	lda #<(256+8)
	sta $210E
	lda #>(256+8)
	sta $210E
	lda #<(256+8)
	sta $2110
	lda #>(256+8)
	sta $2110
	rts
@xblanks:
	.a16
	pha
	lda #$01EE
	:
		sta $2118
		dex
		bne :-
	pla
	rts
	.a8
@xzeros:
	.a16
	:
		stz $2118
		dex
		bne :-
	rts
	.a8

;
; main loop
;

run:
	jsr reload
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
	bne @mode0
	lda z:newpad+1
	and #$40 ; Y
	bne @mode1
	lda z:newpad+0
	and #$80 ; A
	bne @mode2
	lda z:newpad+0
	and #$40 ; X
	bne @mode3
	jmp @loop
@mode0:
	stz z:mode
	jmp @reload
@mode1:
	lda #1
	sta z:mode
	jmp @reload
@mode2:
	lda #2
	sta z:mode
	jmp @reload
@mode3:
	lda #3
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

;
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

; if the assert happens, add some padding or an align to keep these from crossing a bank
.macro BANKCHECK label_
	.assert (^(*-1))=(^(label_)), error, "BANKCHECK fail."
.endmacro

chr0: .incbin "0.chr"
BANKCHECK chr0
chr1: .incbin "1.chr"
BANKCHECK chr1
chr2: .incbin "2.chr"
BANKCHECK chr2
chr3: .incbin "3.chr"
BANKCHECK chr3
chr4: .incbin "4.chr"
BANKCHECK chr4
chr5: .incbin "5.chr"
BANKCHECK chr5

pal0: .incbin "0.pal"
pal1: .incbin "1.pal"
pal2: .incbin "2.pal"
pal3: .incbin "3.pal"
pal4: .incbin "4.pal"

byte00: .byte $00

hdma_scroll_y:
.byte 24-1
.word 36*8 ; blank area of lower tilemap
.byte 127
.word 1*8 ; image area of upper tilemap
.byte 176-127 ; continuing up to 176
.word 1*8
.byte 1
.word (36-25)*8 ; blank area again
.byte 0

BANKCHECK pal0
