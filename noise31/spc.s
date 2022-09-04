; minimal SPC program to play a loud noise

.memorymap
DEFAULTSLOT 0
SLOTSIZE $FDC0
SLOT 0 $0200
.endme

.rombankmap
BANKSTOTAL 1
BANKSIZE $FDC0
BANKS 1
.endro

.enum $00
.ende

.bank 0 slot 0
.org 0

;
; $0200 entry point and command processing loop
;

start:
	; soft reset DSP
	mov $F2, #$6C ; FLG
	mov $F3, #%11100000 ; soft reset, mute, echo disable
	; set all DSP registers to 0 except $6C
	mov X, #0
-
	cmp X, #$6C
	beq +
		mov $F2, X
		mov $F3, #0
	+
	inc X
	cmp X, #$80
	bcc -
	; apply table of values
	mov X, #0
@reginit:
	mov A, !regs_init+X
	bmi +
	mov $F2, A
	mov A, !regs_init+1+X
	mov $F3, A
	inc X
	inc X
	bra @reginit
+
	; infinite loop
-
	bra -

regs_init:
	.db $6C, %01100000 ; FLG soft-reset off, mute, echo disable
	.db $0C, $7F, $1C, $7F ; MVOL main volume full
	.db $00, $7F, $01, $7F ; VOL maximum voice 0
	.db $07, $7F           ; GAIN maximum voice 0
	.db $3D, %00000001     ; NON noise on voice 0
	.db $4C, %00000001     ; KON voice 0
	.db $6C, %00111111 ; FLG unmute, echo disable, noise pitch highest
	.db $FF
