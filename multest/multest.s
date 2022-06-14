; multest framework for testing calculations
;
; rainwarrior 2022
; http://rainwarrior.ca

.p816
.a8
.i8

; defined in test module
.import test_name : far ; asciiz string for title
.import test_run ; a16/i8 LOPRG code to be tested (input: test_in, output: test_out)
.import test_ref ; a16/i8 LOPRG reference code to be tested against

; accessible to test module
.exportzp test_count
.exportzp test_in0
.exportzp test_in1
.exportzp test_out

;
; Constants
;


TEST_JUMP_IN = 0
PRINT_ALL_RESULTS = 0
TEST_INCREMENT = 5081 ; a prime number just to avoid cycling in order

VRAM_NMT = $0000
VRAM_CHR = $2000

;
; Header
;

; 
.segment "HEADER"
;.byte "MULTEST              " ; in HEADNAME
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
ptr:           .res 3

test_count:   .res 4 ; test framework counter through test cases
testc_in0:    .res 4
testc_in1:    .res 4

test_in0:     .res 4 ; inputs and results for the tests (test may modify)
test_in1:     .res 4
test_out:     .res 4
copy_out:     .res 4

.segment "LORAM" ; <$2000
.segment "HIRAM" ; $FE bank < $8000

new_hdma: .res 16 * 8 ; HDMA channel settings to apply at next update
nmi_hdma: .res 16 * 8 ; HDMA channel settings current

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

;
; common stuff
;

.a16
.i8

render_on:
	ldx #1
	stx z:nmi_ready
render_wait:
	wai
	:
		ldx z:nmi_ready
		bne :-
	rts

render_off:
	ldx #2
	stx z:nmi_ready
	bra render_wait

print_hex8: ; A = hex value, clobber X
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
	pha
	xba
	jsr print_hex8
	pla
	jmp print_hex8

print_ptr:
	ldy #0
:
	lda [ptr], Y
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

.macro PRINTSTR string_
	.pushseg
	.rodata
	.local pxystr
	pxystr: .asciiz string_
	.popseg
	lda #.loword(pxystr)
	sta z:ptr+0
	ldx #^pxystr
	stx z:ptr+2
	jsr print_ptr
.endmacro

.macro PRINTXY x_, y_, string_
	LATCHXY (x_), (y_)
	PRINTSTR string_
.endmacro


;
; main
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
	; print test_name
	LATCHXY 5, 5
	lda #.loword(test_name)
	sta z:ptr+0
	ldx #^test_name
	stx z:ptr+2
	jsr print_ptr
	; to jump into more relevant tests earlier:
	.if TEST_JUMP_IN
		lda #5081
		sta z:testc_in0
	.endif
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
	jsr test_set
	; pass when testc returns to 0000
	lda z:test_count
	;cmp #2 ; to test passing
	beq @pass
	jmp @loop
@pass:
	jsr render_off
	PRINTXY 5, 10, "Pass!"
	jsr render_on
:
	jmp :-

;
; test
;

; run one batch of tests then return (in case we need occasional user input)
test_set:
	; run test
	jsr test_set_run
	; increment
	lda z:testc_in0
	clc
	adc #TEST_INCREMENT
	sta z:testc_in0
	inc z:test_count
	rts

test_set_run: ; run test for testc_in0, for all values of testc_in1
	; display current test_count / testc_in0
	jsr render_off
	LATCHXY 5,8
	lda z:test_count
	jsr print_hex16
	PRINTSTR " ("
	lda z:testc_in0
	jsr print_hex16
	PRINTSTR ")"
	jsr render_on
	; preform tests for all values of tests_in1
	stz z:testc_in1
@loop:
	; inputs to be tested
	lda z:testc_in0
	sta z:test_in0
	lda z:testc_in1
	sta z:test_in1
	jsr test_run
	lda z:test_out+0
	sta z:copy_out+0
	lda z:test_out+2
	sta z:copy_out+2
	; reference implementation
	lda z:testc_in0
	sta z:test_in0
	lda z:testc_in1
	sta z:test_in1
	jsr test_ref
	; to test results:
	.if PRINT_ALL_RESULTS
		jsr render_off
		jsr print_result
		jsr render_on
	.endif
	; compare
	;inc z:copy_out+0 ; to test fail
	lda z:test_out+0
	cmp z:copy_out+0
	bne @fail
	lda z:test_out+2
	cmp z:copy_out+2
	bne @fail
	; next test
	inc z:testc_in1
	bne @loop
	rts
@fail:
	jsr render_off
	PRINTXY 5, 10, "Fail!"
	jsr print_result
	jsr render_on
:
	jmp :-

print_result:
	LATCHXY 5, 12
	lda z:testc_in0
	jsr print_hex16
	PRINTSTR " x "
	lda z:testc_in1
	jsr print_hex16
	LATCHXY 5,13
	lda z:copy_out+2
	jsr print_hex16
	lda z:copy_out+0
	jsr print_hex16
	PRINTSTR " != "
	lda z:test_out+2
	jsr print_hex16
	lda z:test_out+0
	jsr print_hex16
	rts

;
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

; if the assert happens, add some padding or an align to keep these from crossing a bank
.macro BANKCHECK label_
	.assert (^(*-1))=(^(label_)), error, "BANKCHECK fail."
.endmacro

chr_test: .incbin "test.chr"
CHR_TEST_SIZE = * - chr_test
BANKCHECK chr_test

; for clearing
byte00: .byte $00

palette: ; 4 greys + some rainbow for test
	.word  0+( 0<<5)+( 0<<10)
	.word 13+(13<<5)+(13<<10)
	.word 22+(22<<5)+(22<<10)
	.word 31+(31<<5)+(31<<10)
	.repeat 12, I
		.byte (I+4) | ((I+4)<<4), (I+4) | ((I+4)<<4)
	.endrepeat
