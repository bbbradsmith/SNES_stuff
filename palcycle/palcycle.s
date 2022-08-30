; Palette cycle demo
;
; Press any button to change image
; Press START to halt the cycling
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
.byte "PALETTE CYCLE DEMO   "
.byte $31 ; map mode (HiROM, FastROM)
.byte $00 ; cartridge type (ROM only)
.byte $09 ; 4mbit (512k)
;.byte $0C ; 32mbit (4MB)
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
pause:        .res 1
art:          .res 1
cycle_ptr:    .res 4
temp:         .res 10+32
switch:       .res 1

.segment "LORAM" ; <$2000

MAX_CYCLES = 32

palette:      .res 512
cycle_phase:  .res MAX_CYCLES

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

bin_art:

.incbin "demo.bin"
ART_COUNT = 7
ART_START = 0

;.incbin "ferrari.bin"
;ART_COUNT = 35
;ART_START = $1C

VRAM_NMT_BG1 = $7800

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
	lda z:nmi_ready ; 0=no pending update
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
	; palette DMA
	stz a:$2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta a:$4300
	lda #$22 ; $2122 CGDATA
	sta a:$4301
	lda #<palette
	sta a:$4302
	lda #>palette
	sta a:$4303
	lda #^palette
	sta a:$4304
	stz a:$4305
	lda #>512
	sta a:$4306 ; 512 bytes
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
	; setup PPU addresses
	lda #((>VRAM_NMT_BG1) & $FC)
	sta a:$2107 ; BG1SC nametable, 1-screen
	lda #0
	sta a:$210B ; BG12NBA
	lda #3
	sta a:$2105 ; BGMODE 3
	lda #$01
	sta a:$212C ; TM BG1
	lda #<-1
	sta a:$210E
	sta a:$210E ; BG1VOFS = -1
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
	ldx #ART_START
	stx z:art
	rep #$20
	sep #$10
	.a16
	.i8
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
	lda #0
	; cycle palettes
	ldx z:pause
	bne @cycle_end
		sta z:temp+0 ; index of palette
		:
			jsr cycle_palette ; returns carry when finished, automatically increments temp+0
			bcc :-
		;
	@cycle_end:
	; buttons
	lda z:newpad
	and #$1000
	beq :+
		lda z:pause
		eor #1
		tax
		stx z:pause
	:
	lda z:newpad
	and #%1010010110010000 ; B, SELECT, down, right, A, R
	beq :++
		ldx z:art
		inx
		cpx #ART_COUNT
		bcc :+
			ldx #0
		:
		bra @change
	:
	lda z:newpad
	and #%0100101001100000 ; Y, up, left, X, L
	beq :++
		ldx z:art
		bne :+
			ldx #ART_COUNT
		:
		dex
		bra @change
	:
	bra @loop
@change:
	stx z:art
	jsr picture_off
	; load next image
@load:
	sep #$20
	.a8
	lda z:art
	clc
	adc #^bin_art
	sta z:cycle_ptr+2
	sta a:$4304 ; DMA bank source
	; load vram
	stz a:$2116 ; VMADD $0000
	stz a:$2117
	lda #%00000001 ; 2-register
	sta a:$4300
	lda #$18 ; $2118 VMDATA
	sta a:$4301
	stz a:$4302
	stz a:$4303
	;sta a:$4304 ; set above
	lda #<(256*224)
	sta a:$4305
	lda #>(256*224)
	sta a:$4306 ; 256x224 pixels at 1bpp (packed as CHR tiles)
	lda #$01
	sta a:$420B ; DMA
	; setup nametable
	rep #$30
	.a16
	.i16
	lda #VRAM_NMT_BG1
	sta a:$2116 ; VMADD BG1 NMT
	ldx #0
	:
		stx $2118
		inx
		cpx #1024
		bcc :-
	; copy palette
	lda #(256*224)
	sta z:cycle_ptr+0
	ldy #0
	:
		lda [cycle_ptr], Y
		sta a:palette, Y
		iny
		iny
		cpy #512
		bcc :-
	; set cycle address
	lda #(256*224)+512
	sta z:cycle_ptr+0
	; reset palette cycles
	sep #$30
	.a8
	.i8
	lda #0
	tax
	:
		sta a:cycle_phase, X
		inx
		cpx #MAX_CYCLES
		bcc :-
	rep #$20
	.a16
	.i8
	jmp @loop

cycle_palette: ; temp+0/1 = index
	.a16
	.i8
	lda z:temp+0
	asl
	asl
	tay
	lda [cycle_ptr], Y ; start=0, end=0 means end of list
	bne :+
		sec
		rts
	:
	sta z:temp+2 ; temp+2, temp+3 = start,end index
	iny
	iny
	lda [cycle_ptr], Y
	sta z:temp+4 ; temp+4, temp+5 = increment, reverse
	sep #$20
	.a8
	ldx z:temp+0
	lda z:temp+4
	clc
	adc a:cycle_phase, X
	sta a:cycle_phase, X
	bcs :+
		jmp @check_finish
	:
	rep #$30
	.a16
	.i16
	lda z:temp+2
	and #$00FF
	asl
	sta z:temp+6 ; temp+6/7 = start
	lda z:temp+3
	and #$00FF
	asl
	sta z:temp+8 ; temp+8/9 = end
	lda z:temp+5
	and #$00FF
	beq @forward
	cmp #16
	beq @forward_16
	cmp #17
	bne @reverse
	jmp @switch_tick
@forward:
	ldx z:temp+8
	lda a:palette, X
	sta z:temp+10 ; store the last value
	lda z:temp+8
	clc
	adc #.loword(palette)
	inc
	tay ; dest = last word + 1, to select last byte
	dec
	dec
	tax ; source is dest - 2
	lda z:temp+8
	sec
	sbc z:temp+6
	dec ; count is (end-start)-1
	phb
	mvp #^palette,#^palette
	plb
	ldx z:temp+6
	lda z:temp+10
	sta a:palette, X ; put stored value in first position
	bra @finish
@reverse:
	ldx z:temp+6
	lda a:palette, X
	sta z:temp+10 ; store the first value
	lda z:temp+6
	clc
	adc #.loword(palette)
	tay ; dest
	inc
	inc
	tax ; source is dest + 2
	lda z:temp+8
	sec
	sbc z:temp+6
	dec ; count is (end-start)-1
	phb
	mvn #^palette,#^palette
	plb
	ldx z:temp+8
	lda z:temp+10
	sta a:palette, X ; put stored value in last position
	bra @finish
@forward_16:
	; store the last 16 values
	lda z:temp+8
	sec
	sbc #30
	clc
	adc #.loword(palette)
	tax
	ldy #temp+10
	lda #31
	phb
	mvn #0,#0
	; copy 16 forward
	lda z:temp+8
	clc
	adc #.loword(palette)
	inc
	tay ; dest = last word + 1, to select last byte
	sec
	sbc #32
	tax ; source is dest - 32
	lda z:temp+8
	sec
	sbc z:temp+6
	sec
	sbc #(30+1) ; count is (end-start)-1
	mvp #^palette,#^palette
	; restore the first 16 values
	lda z:temp+6
	clc
	adc #.loword(palette)
	tay
	ldx #temp+10
	lda #31
	mvn #0,#0
	; finish
	plb
	bra @finish
@switch_tick:
	.a16
	.i16
	sep #$30
	.a8
	.i8
	lda z:switch
	eor #1
	sta z:switch
	; fall through
@check_finish:
	.a8
	.i8
	lda z:temp+5
	cmp #17
	bne @finish
	; switch
	ldx z:switch
	lda z:temp+2, X ; start/end are repurposed as a jump index into the list
	dec ; -1 because we will increment before leaving function
	sta z:temp+0
	stz z:temp+1
@finish:
	rep #$20
	sep #$10
	.a16
	.i8
	inc z:temp+0
	clc
	rts
