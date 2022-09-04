; noise31 demo
;
; Just plays hardware noise at its highest frequence (31)
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
.byte "NOISE31              "
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
; MAIN segment: flat HiROM space for more code, bulk data, etc.
;

.segment "MAIN"

;
; =============================================================================
; LOPRG stuff for the first bank (mapped at $C00000)
;

; used for all code in this demo program
.segment "LOPRG"

bin_spc: .incbin "spc.bin"
BIN_SPC_SIZE = * - bin_spc

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
	sep #$30
	.a8
	.i8
	lda #$8f
	sta a:$2100 ; force blank
	;
	; use BIOS loader to load and run the SPC code
	;
	rep #$10
	.i16
	; wait for warmup signal AABB
	ldy #0
	ldx #$BBAA
:
	cpx $2140 ; APUIO0/1
	bne :-
	lda #$01
	sta $2141 ; APUIO1 not-zero = wait for command setup
	; set write address to $0200
	lda #<$0200
	sta $2142 ; APUIO2
	lda #>$0200
	sta $2143 ; APUIO3
	lda #$CC
	sta $2140 ; APUIO0 CC for transfer program
:
	cmp $2140 ; CC echoed back = ready to go
	bne :-
	ldx #0
@send_byte:
	lda f:bin_spc, X
	sta $2141 ; APUI01 data byte to send
	txa
	sta $2140 ; APUIO0 0 = send first byte, otherwise send incrementing value
:
	cmp $2140 ; APUIO0 wait for echo of incrementing value
	bne :-
	inx
	cpx #BIN_SPC_SIZE
	bcc @send_byte
	; finish and run
	stz $2141 ; APUI01 0 = ready to run
	; set run address to $0200
	lda #<$0200
	sta $2142 ; APUIO2
	lda #>$0200
	sta $2143 ; APUIO3
	lda $2140 ; APUIO0 += 2 to exit data transfer and begin run
	inc
	inc
	sta $2140
	;
	; done, infinite loop
	;
:
	bra :-
