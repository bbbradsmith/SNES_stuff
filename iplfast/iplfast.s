; IPLFast
;
; Investigates a fast alternative to the SPC IPL ROM loader.
; Loads 32k with IPL and with alternative, reports both frame counts.
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
.byte "IPLFAST              "
.byte $30 ; map mode (LoROM, FastROM)
.byte $00 ; cartridge type (ROM only)
.byte $06 ; 0.5mbit (64k)
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

ptr: .res 2
counter: .res 2
time_ipl: .res 2
time_iplfast: .res 2

.segment "LORAM" ; <$2000

.segment "HIRAM" ; $FE bank < $8000

;
; =============================================================================
; Data
;

.segment "PRG0"

iplfast_spc_bin: .incbin "iplfast.spc.bin"
IPLFAST_SPC_BIN_SIZE = * - iplfast_spc_bin

TRANSFER_TEST_SIZE = 32 * 1024

.segment "PRG1"

chr_test: .incbin "test.chr"
CHR_TEST_SIZE = * - chr_test

palette: ; 4 greys + some rainbow for test
	.word  0+( 0<<5)+( 0<<10)
	.word 13+(13<<5)+(13<<10)
	.word 22+(22<<5)+(22<<10)
	.word 31+(31<<5)+(31<<10)
	.repeat 12, I
		.byte (I+4) | ((I+4)<<4), (I+4) | ((I+4)<<4)
	.endrepeat

; for clearing
byte00: .byte $00

VRAM_NMT = $0000
VRAM_CHR = $2000

;
; =============================================================================
; START stub for bank $00 dispatch
;

.segment "START"

vector_irq:
	; not used
	rti

vector_nmi:
	rep #$30
	inc counter
	rti

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
; PRG stuff for the first bank (mapped at $C00000)
;

; used for all code in this demo program
.segment "PRG0"

;
; Printing
;

print_hex8: ; A = hex value, clobber X
	.a16
	.i8
	pha
	lsr
	lsr
	lsr
	lsr
	and #$0F
	ora #$10
	tax
	stx $2118
	pla
	and #$0F
	ora #$10
	tax
	stx $2118
	rts

print_hex16:
	.a16
	.i8
	pha
	xba
	jsr print_hex8
	pla
	jmp print_hex8

print_ptr:
	.a16
	.i8
	ldy #0
:
	lda (ptr), Y
	and #$00FF
	beq :+
	tax
	stx $2118
	iny
	bne :-
:
	rts

.macro LATCHXY x_, y_
	lda #(VRAM_NMT+(x_)+(32*(y_)))
	sta $2116
.endmacro

.macro PRINTXY x_, y_, string_
	LATCHXY (x_), (y_)
	PRINTSTR string_
.endmacro

;
; IPL Load
;

ipl_load: ; just load data to $300 for timing reference, returns to IPL boot
	.a8
	.i8
	rep #$10
	.i16
	ldy #0
	ldx #$BBAA
:
	cpx $2140
	bne :-
	lda #$01
	sta $2141
	lda #<$0300
	sta $2142
	lda #>$0300
	sta $2143
	lda #$CC
	sta $2140
:
	cmp $2140
	bne :-
	ldx #0
@send_byte:
	lda a:$8000, X ; data doesn't matter for test
	sta $2141
	txa
	sta $2140
:
	cmp $2140
	bne :-
	inx
	cpx #TRANSFER_TEST_SIZE
	bcc @send_byte
	stz $2141
	lda #<$FFC0
	sta $2142
	lda #>$FFC0
	sta $2143
	lda $2140
	inc
	inc
	sta $2140
	sep #$10
	.i8
	rts

;
; Alternative fast load
;

iplfast_load:
	.a8
	.i8
	; 1. load IPL-Fast SPC program to $200
	rep #$10
	.i16
	ldy #0
	ldx #$BBAA
:
	cpx $2140
	bne :-
	lda #$01
	sta $2141
	lda #<$0200
	sta $2142
	lda #>$0200
	sta $2143
	lda #$CC
	sta $2140
:
	cmp $2140
	bne :-
	ldx #0
@send_byte:
	lda iplfast_spc_bin, X
	sta $2141
	txa
	sta $2140
:
	cmp $2140
	bne :-
	inx
	cpx #IPLFAST_SPC_BIN_SIZE
	bcc @send_byte
	stz $2141
	lda #<$0200
	sta $2142
	lda #>$0200
	sta $2143
	lda $2140
	inc
	inc
	sta $2140
	; 2. use IPL-Fast to load data
	; wait for $1990 signature to begin
	ldx #$1990
:
	cpx $2140
	bne :-
	ldx #$0300
	stx $2142 ; address to write
	lda #1
	sta $2140 ; nonzero to port 0 to hold after we release via port 1
	sta $2141 ; nonzero to port 1 to release
:
	cmp $2141 ; port 1 echo will acknowledge
	bne :-
	ldx #0
	ldy #.loword(-1) ; first increment = 0
	; Byte upload like IPL version but with 3 bytes at a time:
	;   1. wait for port 0 = counter
	;   2. then write 3 bytes to ports 1-3
	;   3. increment counter and write to port 0 to send
:
	cmp $2140
	bne :-
	lda a:$8000, X ; data doesn't matter for test
	sta $2141
	inx
	lda a:$8000, X
	sta $2142
	inx
	lda a:$8000, X
	sta $2143
	iny
	tya
	sta $2140
	inx
	cpx #TRANSFER_TEST_SIZE
	bcc :-
:
	cmp $2140
	bne :- ; wait for last acknowledge
	; return execution to IPL
	ldx #$FFC0
	stx $2142
	iny
	iny
	tya
	sta $2140 ; double increment counter to end and return execution
	rts

;
; Tests
;

wait_nmi:
	lda z:counter+0
	:
		cmp z:counter+0
		beq :-
	rts

run:
	.a8
	.i8
	; set data bank to this one
	lda #^*
	pha
	plb
	; enable NMI
	lda #$80
	sta $4200
	; time IPL
	rep #$30
	.a16
	.i16
	jsr wait_nmi
	lda z:counter
	sta z:time_ipl
	sep #$30
	.a8
	.i8
	jsr ipl_load
	rep #$30
	.a16
	.i16
	jsr wait_nmi
	lda z:counter
	sec
	sbc z:time_ipl
	sta z:time_ipl
	; time IPL Fast
	jsr wait_nmi
	lda z:counter
	sta z:time_iplfast
	sep #$30
	.a8
	.i8
	jsr iplfast_load
	rep #$30
	.a16
	.i16
	jsr wait_nmi
	lda z:counter
	sec
	sbc z:time_iplfast
	sta z:time_iplfast
	sep #$10
	.i8
	; print results
	LATCHXY 5, 5
	lda #.loword(@text0)
	sta z:ptr
	jsr print_ptr
	lda #TRANSFER_TEST_SIZE
	jsr print_hex16
	LATCHXY 5, 7
	lda #.loword(@text1)
	sta z:ptr
	jsr print_ptr
	LATCHXY 5, 8
	lda #.loword(@text2)
	sta z:ptr
	jsr print_ptr
	lda z:time_ipl
	jsr print_hex16
	LATCHXY 5, 9
	lda #.loword(@text3)
	sta z:ptr
	jsr print_ptr
	lda z:time_iplfast
	jsr print_hex16
	; display on and infinite loop
	ldx #$0F
	stx $2100
:
	bra :-
@text0: .asciiz "Bytes: $"
@text1: .asciiz "Frames"
@text2: .asciiz "  IPL: $"
@text3: .asciiz " FAST: $"

;
; Main
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
	lda #%00001001 ; no increment, 2-register
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
	; copy tiles
	lda #<VRAM_CHR
	sta $2116
	lda #>VRAM_CHR
	sta $2117
	lda #%00000001 ; 2-register
	sta $4300
	lda #<chr_test
	sta $4302
	lda #>chr_test
	sta $4303
	lda #^chr_test
	sta $4304
	lda #<CHR_TEST_SIZE
	sta $4305
	lda #>CHR_TEST_SIZE
	sta $4306
	lda #$01
	sta $420B ; DMA
	; load palettes
	stz $2121 ; CGADD $00
	lda #%00000010 ; 1-register
	sta $4300
	lda #$22 ; $2122 CGDATA
	sta $4301
	ldx #16
	:
		lda #<palette
		sta $4302
		lda #>palette
		sta $4303
		lda #^palette
		sta $4304
		lda #32
		sta $4305
		stz $4306 ; 32 bytes
		lda #$01
		sta $420B ; DMA
		dex
		bne :-
	; setup PPU addresses
	lda #((>VRAM_NMT) & $FC)
	sta $2107 ; BG1SC nametable, 1-screen
	lda #(VRAM_CHR >> 12)
	sta $210B ; BG12NBA
	lda #$01
	sta $212C ; TM BG1 main-screen
	; increment on $2118 for easy NMT updates
	stz $2115
	; begin
	jmp run
