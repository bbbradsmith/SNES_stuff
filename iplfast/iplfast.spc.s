; IPL alternative that loads 3 bytes per acknowledge

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
; $0200 entry point
;

iplfast:
	; port 1 = 0 from IPL
	; signal ready with $1990 on ports 0+1
	mov $F4, #$90
	mov $F5, #$19
	; wait until port 1 != 0 to begin
-
	cmp $F5, #0
	beq -
	; set write address from port 2+3
	movw YA, $F6
	movw $00, YA
	; echo port 0 to set up
	mov A, $F4
	mov $F4, A
	; echo port 1 to acknowledge
	mov A, $F5
	mov $F5, A
	; begin when port 0 = 0
	mov Y, #0
-
	mov X, $F4
	bne -
@byte:
	cmp X, $F4
	bne @check_end
	mov A, $F5
	mov [$00]+Y, A
	inc Y
	mov A, $F6
	mov [$00]+Y, A
	inc Y
	mov A, $F7
	mov $F4, X ; echo counter at port 0
	inc X
	mov [$00]+Y, A
	inc Y
	bpl @byte ; after 128 bytes, add Y to address
	mov A, Y
	mov Y, #0
	clrc
	addw YA, $00
	movw $00, YA
	mov Y, #0
	bra @byte
@check_end:
	bpl @byte
	cmp X, $F4
	bpl @byte
	; transfer is finished, jump execution to 2+3 address
	movw YA, $F6
	movw $00, YA
	mov X, #0
	jmp [!$0000+X]
