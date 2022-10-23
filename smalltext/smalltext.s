; smalltext demo
;
; Demonstrates 64x60 character text display.
;
; D-pad to scroll
; L/R to adjust brightness of shadow
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
.byte "SMALLTEXT DEMO       "
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
temp:         .res 1

scrollx:      .res 2
scrolly:      .res 2
grey:         .res 1

.segment "LORAM" ; <$2000

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
text_chr: .incbin "text.chr"

; tilemaps 2k aligned
.align $800
text_nmt: .incbin "text.nmt"

BANKCHECK bin_vram

VRAM_CHR = (text_chr-bin_vram)>>1
VRAM_NMT = (text_nmt-bin_vram)>>1

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
	inc z:nmi_count
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
	lda #2
	sta a:$2121 ; CGADD
	lda z:grey
	asl
	asl
	asl
	asl
	asl
	ora z:grey
	sta a:$2122 ; CGDATA
	lda z:grey
	asl
	asl
	sta z:temp
	lda z:grey
	lsr
	lsr
	lsr
	ora z:temp
	sta a:$2122 ; CGDATA
	lda #34
	sta a:$2121 ; CGADD
	lda z:grey
	asl
	asl
	asl
	asl
	asl
	ora z:grey
	sta a:$2122 ; CGDATA
	lda z:grey
	asl
	asl
	sta z:temp
	lda z:grey
	lsr
	lsr
	lsr
	ora z:temp
	sta a:$2122 ; CGDATA
	lda z:scrollx+0
	sta a:$210F ; BG2HOFS
	lda z:scrollx+1
	sta a:$210F
	lda z:scrolly+0
	sta a:$2110 ; BG2VOFS
	lda z:scrolly+1
	sta a:$2110
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
	; palette
	stz a:$2121 ; CGADD
	stz a:$2122 ; CGCOLOR
	stz a:$2122
	lda #<(31|(31<<5)|(31<<10))
	sta a:$2122
	lda #>(31|(31<<5)|(31<<10))
	sta a:$2122
	lda #32
	sta a:$2121 ; CGADD
	stz a:$2122 ; CGCOLOR
	stz a:$2122
	lda #<(31|(31<<5)|(31<<10))
	sta a:$2122
	lda #>(31|(31<<5)|(31<<10))
	sta a:$2122
	; setup PPU addresses
	lda #((>VRAM_NMT) & $FC) | 2
	sta a:$2108 ; BG2SC nametable, 2-screen tall
	lda #((VRAM_CHR     >> 12) << 4)
	sta a:$210B ; BG12NBA
	; setup PPU state
	lda #%00000010
	sta a:$212C
	sta a:$212D ; TM/TS
	lda #5
	sta a:$2105 ; BGMODE
	lda #%00000101
	sta a:$2133 ; SETINI
	; begin
	jmp run

;
; =============================================================================
; main loop
;

run:
	.a8
	.i8
	lda #15
	sta z:grey
	; enable NMI and auto-joypad
	lda a:$4210 ; RDNMI
	lda #$81
	sta a:$4200 ; NMITIMEN
	rep #$20
	sep #$10
	.a16
	.i8
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
	; scrolling / grey only 8 times per second
	ldx z:nmi_count
	txa
	and #7
	bne @rate_end
	lda z:gamepad
	and #$0100 ; R
	beq :+
		dec z:scrollx
	:
	lda z:gamepad
	and #$0200 ; L
	beq :+
		inc z:scrollx
	:
	lda z:gamepad
	and #$0400 ; D
	beq :+
		dec z:scrolly
	:
	lda z:gamepad
	and #$0800 ; U
	beq :+
		inc z:scrolly
	:
	lda z:gamepad
	and #$0020 ; L-shoulder
	beq :+
		ldx z:grey
		beq :+
		dex
		stx z:grey
	:
	lda z:gamepad
	and #$0010 ; R-shoulder
	beq :+
		ldx z:grey
		cpx #31
		bcs :+
		inx
		stx z:grey
	:
@rate_end:
	jmp @loop ; TODO
