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
scroll_x:     .res 2
scroll_y:     .res 2

.segment "LORAM" ; <$2000
.segment "HIRAM" ; $FE bank < $8000

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
	; update
	; TODO
	lda z:scroll_x+0
	sta $210D ; BG1HOFS
	lda z:scroll_x+1
	sta $210D
	lda z:scroll_y+0
	sta $210E ; BG1VOFS
	lda z:scroll_y+1
	sta $210E
	; force blanking off
	lda #$0F
	sta $2100
	stz z:nmi_ready
	; next field
	inc z:nmi_count
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
	; setup variables
	stz z:nmi_ready
	stz z:nmi_count
	stz z:gamepad+0
	stz z:gamepad+1
	stz z:lastpad+0
	stz z:lastpad+1
	stz z:mode
	stz z:scroll_x+0
	stz z:scroll_x+1
	stz z:scroll_y+0
	stz z:scroll_y+1
	; begin
	jmp run

;
; main loop
;

run:
	; TODO (should actually be an HDMA so it can be turned on mid-screen)
	lda #7
	sta $2105 ; BGMODE 7
	; enable NMI and auto-joypad
	lda #$81
	sta $4200
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
	; wait for auto-read to finish
	:
		lda $4212 ; HBVJOY
		and #1
		bne :-
	lda $4218 ; JOY1H
	sta z:gamepad+0
	eor z:lastpad+0
	and z:gamepad+0
	sta z:newpad+0
	lda $4219 ; JOY1L
	sta z:gamepad+1
	eor z:lastpad+1
	and z:gamepad+1
	sta z:newpad+1
	; TODO stuff
	; as a test just toggle mode 1 vs mode 7:
	lda z:newpad+1
	and #$40 ; Y
	beq :+
		lda #1
		sta $2105 ; BGMODE 1
	:
	lda z:newpad+1
	and #$80 ; B
	beq :+
		lda #7
		sta $2105 ; BGMODE 7
	:
	lda z:gamepad+1
	and #$01 ; right
	beq :+
		inc z:scroll_x+0
		bne :+
		inc z:scroll_x+1
	:
	lda z:gamepad+1
	and #$04 ; down
	beq :+
		inc z:scroll_y+0
		bne :+
		inc z:scroll_y+1
	:
	lda z:gamepad+1
	and #$02 ; left
	beq :++
		lda z:scroll_x+0
		bne :+
			dec z:scroll_x+1
		:
		dec z:scroll_x+0
	:
	lda z:gamepad+1
	and #$08 ; up
	beq :++
		lda z:scroll_y+0
		bne :+
			dec z:scroll_y+1 
		:
		dec z:scroll_y+0
	:
	jmp @loop

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
