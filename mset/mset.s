;
; mset.s
; SNES Mouse test, by Brad Smith 2019
; adapted for SNES: 2022
; http://rainwarrior.ca
;

.p816
.a8
.i8

.macpack longbranch

; ===
; RAM
; ===

.segment "ZEROPAGE"
_ptr:          .res 2 ; shared pointer for C interface
ppu_post_mode: .res 1
_input:        .res 20
_mouse_index:  .res 1
_mouse0:       .res 1
_mouse1:       .res 1
_mouse2:       .res 1
_mouse3:       .res 1
_mouse4:       .res 1
_mouse_y:      .res 1
_mouse_x:      .res 1

; 4 convenient index values, in 3 variable sizes
_i:    .res 4
_j:    .res 4
_k:    .res 4
_l:    .res 4
_ix = _i
_jx = _j
_kx = _k
_lx = _l
_eix = _i
_ejx = _j
_ekx = _k
_elx = _l

.segment "BSS"
temp: .res 4

.segment "STACK"
ppu_2000:        .res 1
ppu_2001:        .res 1
ppu_2005x:       .res 1
ppu_2005y:       .res 1
_ppu_send_addr:  .res 2
_ppu_send_count: .res 1
_ppu_send:       .res 64
_palette:        .res 32

.segment "OAM"
.align 256
_oam: .res 256

.exportzp _ptr
.exportzp _input;
.exportzp _mouse_index, _mouse0, _mouse1, _mouse2, _mouse3, _mouse4, _mouse_y, _mouse_x
.exportzp _i,_j,_k,_l
.exportzp _ix,_jx,_kx,_lx
.exportzp _eix,_ejx,_ekx,_elx

.export _ppu_send_addr
.export _ppu_send_count
.export _ppu_send
.export _palette
.export _oam

; cc65 temporaries
.importzp tmp1, tmp2, tmp3, tmp4

.enum
	POST_OFF    = 1
	POST_NONE   = 2
	POST_UPDATE = 3
.endenum

; =====
; Tiles
; =====

.export _sprite_chr

.segment "RODATA"
_sprite_chr: .incbin "sprite.chr"

; =========
; Utilities
; =========

.export _mouse_sense
.export _input_setup
.export _input_poll

.segment "CODE"

input_poll_raw:
	; strobe
	ldy #1
	sty $4016
	dey
	sty $4016
	; read 5 bytes each from 4 data lines
input_poll_raw_strobed:
	ldx #0
@poll_byte:
	ldy #8
	:
		lda $4016
		ror
		rol _input+0, X
		ror
		rol _input+2, X
		lda $4017
		ror
		rol _input+1, X
		ror
		rol _input+3, X
		dey
		bne :-
	inx
	inx
	inx
	inx
	cpx #20
	bcc @poll_byte
	rts

_mouse_sense:
	lda #1
	sta $4016
	lda $4016
	lda $4017
	lda #0
	sta $4016
	rts

_input_setup:
	lda #4
	sta _mouse_index ; 4 = no mouse detected
	; if present, mouse needs to be initialized by cycling the sensitivity
	jsr _mouse_sense
	ldx #3 ; count down from 3-0 so that lowest index mouse is used
	@mouse_detect:
		lda #0
		sta temp, X
		stx tmp1
		jsr input_poll_raw
		ldx tmp1
		lda _input+4, X
		and #$0F
		cmp #1
		bne @fail ; missing signature
		@pass:
			lda #1
			sta temp, X ; mouse detected
			stx _mouse_index ; mouse selected
		@fail:
		dex
		cpx #4
		bcc @mouse_detect
	; attempt to cycle to medium sensitivity setting
	lda #4 ; maximum 4 attempts (3 should be sufficient, the 4th is for luck)
	sta tmp1
	:
		jsr _mouse_sense
		jsr _input_poll
		lda _mouse1
		and #$30
		cmp #$10 ; medium setting
		beq :+
		dec tmp1
		bne :-
	:
	rts

_input_poll:
	jsr input_poll_raw
	lda _mouse_index
	cmp #4
	bcc @mouse
	@no_mouse:
		lda #0
		sta _mouse0
		sta _mouse1
		sta _mouse2
		sta _mouse3
		sta _mouse4
		sta _mouse_x
		sta _mouse_y
		rts
	@mouse:
		;lda _mouse_index
		tax
		lda _input+0, X
		sta _mouse0
		lda _input+4, X
		sta _mouse1
		lda _input+8, X
		sta _mouse2
		bpl :+ ; convert signed magnitude to two's complement
			eor #$7F
			clc
			adc #1
		:
		sta _mouse_y
		lda _input+12, X
		sta _mouse3
		bpl :+
			eor #$7F
			clc
			adc #1
		:
		sta _mouse_x
		lda _input+16, X
		sta _mouse4
		rts
	;

; =====================
; NES hardware handling
; =====================

.export _ppu_latch
.export _ppu_direction
.export _ppu_write
.export _ppu_load
.export _ppu_fill
.export _ppu_ctrl
.export _ppu_mask
.export _ppu_scroll_x
.export _ppu_scroll_y
.export _ppu_post
.export _ppu_profile

.import _main
.import popa ; cc65 stack manipulator

.segment "CODE"

_ppu_latch:
	sta $2116 ; VMADDL
	stx $2117 ; VMADDH
	;bit $2002
	;stx $2006
	;sta $2006
	rts

_ppu_direction:
	asl
	asl
	and #%00000100
	pha
	lda ppu_2000
	and #%11111011
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_write:
	sta $2118 ; VMDATAL
	;sta $2007
	rts

_ppu_load: ; X:A = count, _ptr = source
	; count assumed to be multiple of 16, data assumed to be 2bpp CHR, expanding to 4bpp
	stx tmp2
	sta tmp1
@tile:
	ldy #0 ; write 8 words (reverse order because $2118 is increment)
	:
		tya
		tax
		clc
		adc #8
		tay
		lda (_ptr), Y
		sta $2119
		txa
		tay
		lda (_ptr), Y
		sta $2118
		iny
		cpy #8
		bcc :-
	ldy #0 ; 0 fill to skip next 8 words
	:
		stz $2118
		iny
		cpy #8
		bcc :-
	; _ptr += 16
	lda _ptr+0
	clc
	adc #<16
	sta _ptr+0
	lda _ptr+1
	adc #>16
	sta _ptr+1
	; count -= 16
	lda tmp1
	sec
	sbc #<16
	sta tmp1
	lda tmp2
	sbc #>16
	sta tmp2
	bcc :+
	ora tmp1
	bne @tile
:
	rts

_ppu_fill:
	; A:X = count
	; stack = fill value
	sta tmp1
	jsr popa
	ldy tmp1
	cpy #0
	beq @no_partial
@page:
	:
		sta $2118
		dey
		bne :-
@no_partial:
	cpx #0
	beq @done
	dex
	ldy #0
	jmp @page
@done:
	rts

_ppu_ctrl:
	and #%00111000
	pha
	lda ppu_2000
	and #%11000111
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_mask:
	sta ppu_2001
	rts

_ppu_scroll_x:
	; A:X = scroll
	sta ppu_2005x
	txa
	and #1
	pha
	lda ppu_2000
	and #%11111110
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_scroll_y:
	; A:X = scroll
	sta ppu_2005y
	txa
	and #1
	asl
	pha
	lda ppu_2000
	and #%11111101
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_post:
	pha
	jsr snes_ppu_post
	pla
	sta ppu_post_mode
	:
		lda ppu_post_mode
		bne :-
	rts

_ppu_profile:
	;ora ppu_2001
	;sta $2001
	; no SNES equivalent
	rts

nmi:
	.a8
	.i8
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
	jsr snes_nmi
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

irq:
	rti

reset:
	jsr snes_reset
	lda #0 ; clear RAM
	tax
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; wipe OAM
	;ldx #0
	lda #$F0
	:
		sta _oam, X
		inx
		bne :-
	jsr snes_init
	; initialize CC65 and enter main()
	jsr cc65_init
	jsr _main
	jmp ($FFFC)

; ==========
; CC65 setup
; ==========

; simplified version of cc65 libsrc/nes/crt0.s

.import copydata ; cc65 "DATA" segment setup
.importzp sp ; cc65 C stack pointer
.export __STARTUP__: absolute = 1

; stack reservation
CSTACK_SIZE = 128
.segment "CSTACK"
cstack: .res CSTACK_SIZE

.segment "CODE"
cc65_init:
	jsr copydata
	lda #<(cstack + CSTACK_SIZE)
	sta sp+0
	lda #>(cstack + CSTACK_SIZE)
	sta sp+1
	rts

; ==========
; SNES Stuff
; ==========

VRAM_NMT_BG1 = $2000
VRAM_CHR_BG1 = $0000
VRAM_CHR_OBJ = $0000

.segment "SNESRAM"

snes_palette: .res 512
snes_oam: .res 256

.segment "CODE"

.proc snes_reset
	.a8
	.i8
	; Already assumed:
	;   sei
	;   clc
	;   xce ; disable 6502 emulation
	;   stz a:$4200 ; disable NMI/IRQ
	;   rep #$30
	;   ldx #$01FF
	;   txs
	;   sep #$30
	; disable decimal
	cld
	; direct page = 0
	rep #$20
	.a16
	lda #0
	tcd
	sep #$20
	.a8
	; initialize registers $2100-2133 (PPU mostly)
	lda #^@reset_val
	pha
	plb
	ldx #<-1
	@loop_2100:
		inx
		ldy a:@reset_2100, X
		bmi :+
			lda z:_i
			sta a:$2100, Y
			bra @loop_2100
		:
			iny
			beq @end_2100
			lda a:@reset_val-$81, Y
			sta z:_i
			bra @loop_2100
		;
	@end_2100:
	; initialize registers $4201-420D
	lda #$FF
	sta a:$4201 ; I/O port
	ldx #2
	:
		stz a:$4200, X ; multiplier, divider, timers, DMA enable, SlowROM
		inx
		cpx #$0E
		bcc :-
	; set default data bank
	lda #$80
	pha
	plb
	rep #$20
	.a16
	; clear OAM with DMA
	stz a:$2012
	ldx #%00001010 ; 2-to-1, no increment
	stx a:$4300
	ldx #$04 ; $2104 OAMDATA
	stx a:$4301
	lda #.loword(@bin_F0)
	sta a:$4302
	ldx #^*
	stx a:$4304
	sty a:$4305
	ldx #>512
	stx a:$4306 ; 512 bytes $F0
	ldx #1
	stx a:$420B
	lda #.loword(@bin_00)
	sta a:$4302
	lda #32
	sta a:$4305 ; 32 bytes $00
	stx a:$420B
	; clear CGRAM with DMA
	ldy #0
	stx a:$2121 ; CGADD = 0
	ldx #$22 ; $2122 CGDATA
	stx a:$4301
	sty a:$4305
	ldx #>512
	stx a:$4306 ; 512 bytes
	ldx #1
	stx a:$420B
	; clear VRAM with DMA
	stz a:2116 ; VMADD
	ldx #%00001001 ; 2-to-2, no increment
	stx a:$4300
	ldx #$18 ; $2118-9 VMDATA
	stx a:$4301
	stz a:$4305 ; 64k
	ldx #1
	stx a:$420B
	; clear RAM with DMA (outside of NES area only)
	lda #$0800
	sta a:$2181
	ldx #$7E
	stx a:$2183 ; WMADD = $7E800
	ldx #%00001000 ; 1-to-1, no increment
	stx a:$4300
	ldx #$80 ; $2180 WMDATA
	stx a:$4301
	lda #$10000-$800
	sta a:$4305 ; 64k - 2k NES
	ldx #1
	stx $420B
	; WMADD = $7F000
	; $4305 = 0 = 64k
	ldx #1
	stx $420B
	; done
	sep #$20
	.a8
	rts
@reset_val: ;$80 + index = value
	;      $80 $81 $82 $83 $84 $85
	.byte $00,$01,$80,$8F,$30,$E0
@reset_2100: ; SNES book recommended default register values
	.byte $83,$00, $80 ; screen = $8F, switch to $00
	.byte $01,$02,$03,$05,$06 ; sprite settings, graphics mode, mosaic
	.byte $07,$08,$09,$0A,$0B,$0C ; tilemap address
	.byte $0D,$0D,$0E,$0E,$0F,$0F,$10,$10,$11,$11,$12,$12,$13,$13,$14,$14 ; scroll
	.byte $82,$15, $80,$16,$17 ; VRAM increment = $80, VRAM address = $00
	.byte $1A ; mode 7 settings
	.byte $1B, $81,$1B, $80,$1C,$1C,$1D,$1D,$1E, $81,$1E, $80,$1F,$1F,$20,$20 ; mode 7 ABCD = 1001, XY = 00
	.byte $21,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F ; colour / window / mask / logic / TS/SS
	.byte $84,$30, $80,$31, $85,$32 ; color math
	.byte $80,$33 ; screen settings
	.byte $FF ; end
@bin_00:
	.byte $00
@bin_F0:
	.byte $F0
.endproc

.proc snes_init
	.a8
	.i8
	; setup PPU addresses
	lda #((>VRAM_NMT_BG1) & $FC) | $00
	sta a:$2107 ; BG1SC nametable, 1-screen
	lda #(VRAM_CHR_BG1 >> 12)
	sta a:$210B ; BG12NBA
	lda #((VRAM_CHR_OBJ >> 13) | $00)
	sta a:$2101 ; OBJSEL 8x8 + 16x16 sprites
	; global screen settings
	lda #$04
	sta a:$2133 ; SETINI overscan 239 lines mode
	lda #%00010001
	sta a:$212C ; TM OBJ + BG1
	lda #$01
	sta a:$2105 ; BGMODE mode 1
	stz a:$2115 ; VMAIN default increment on $2118
	; begin
	lda #$80 ; NMI on, automatic joypad off (will be turned on by _input_setup)
	sta a:$4200 ; NMITIMEN turn on NMI
	rts
.endproc

; NES to SNES palette conversion
snes_pal0: ; gggrrrrr
	.byte $8C,$60,$24,$08,$0D,$0E,$4C,$68,$A4,$E0,$E0,$C0,$C0,$00,$00,$00
	.byte $D6,$41,$E8,$CE,$B5,$B7,$F6,$31,$6C,$C3,$C0,$C0,$A0,$00,$00,$00
	.byte $FF,$A9,$50,$17,$FE,$FF,$1F,$5E,$B9,$EF,$28,$25,$05,$4A,$00,$00
	.byte $FF,$97,$7A,$5C,$5F,$3F,$5F,$7F,$9D,$BA,$B7,$B5,$B5,$F7,$00,$00
snes_pal1: ; 0bbbbbgg
	.byte $31,$4C,$54,$4C,$34,$0C,$00,$00,$00,$00,$00,$10,$30,$00,$00,$00
	.byte $5A,$75,$7C,$7C,$64,$30,$00,$01,$01,$01,$01,$1D,$4D,$00,$00,$00
	.byte $7F,$7E,$7E,$7E,$7D,$69,$2E,$0A,$02,$02,$0F,$37,$67,$25,$00,$00
	.byte $7F,$7F,$7F,$7F,$7F,$7B,$63,$53,$4B,$47,$53,$63,$77,$5E,$00,$00
snes_oam_convert: ; vhp...cc -> vhPP0cc0 (duplicate+negate priority bit, select OBJ page 0)
	.repeat 256, I
		.byte ((I & $E0) | ((I>>1) & $10) | ((I & $03) << 1) | $00) ^ $30
	.endrepeat

.macro PALETTE_TRANSLATE n_, s_
	ldx _palette+(n_)
	lda snes_pal0, X
	sta snes_palette+((s_)*2)+0
	lda snes_pal1, X
	sta snes_palette+((s_)*2)+1
.endmacro

snes_ppu_post:
	; copy some of the palette
	PALETTE_TRANSLATE  0,  0
	PALETTE_TRANSLATE  0+1,  0+1
	PALETTE_TRANSLATE  0+2,  0+2
	PALETTE_TRANSLATE  0+3,  0+3
	PALETTE_TRANSLATE 16+1,128+1
	PALETTE_TRANSLATE 16+2,128+2
	PALETTE_TRANSLATE 16+3,128+3
	; copy and convert OAM
	ldy #0
	:
		lda _oam+3, Y ; X
		sta snes_oam+0, Y
		lda _oam+0, Y ; Y
		sta snes_oam+1, Y
		lda _oam+1, Y ; tile
		sta snes_oam+2, Y
		ldx _oam+2, Y ; attribute
		lda snes_oam_convert, X
		sta snes_oam+3, Y
		iny
		iny
		iny
		iny
		bne :-
	; TODO
	;wai
	rts

snes_nmi:
	ldx #0
	lda ppu_post_mode
	stx ppu_post_mode ; signal the post is complete (after RTI)
	jeq @end
	cmp #POST_NONE
	jeq @post_none
	cmp #POST_UPDATE
	beq @post_update
	; otherwise POST_OFF
@post_off:
	lda #$8F
	sta a:$2100 ; INIDISP screen off
	jmp @end
@post_update:
	rep #$20
	.a16
	; OAM DMA
	stz a:$2102 ; OAMADD
	ldx #%00000010 ; 1-to-2
	stx a:$4300
	ldx #$04 ; $2104 OAMDATA
	stx a:$4301
	lda #.loword(snes_oam)
	sta a:$4302
	ldx #^snes_oam
	stx a:$4304
	lda #256
	sta a:$4305
	ldx #1
	stx a:$420B ; DMA
	; palette DMAs
	; common setup
	ldx #$22 ; $2122 CGDATA
	stx a:$4301
	ldx #^snes_palette
	stx a:$4304
	stz a:$4305
	; BG1 palettes
	ldx #0
	stx a:$2121 ; CGADD = 0
	lda #.loword(snes_palette)
	sta a:$4302
	ldx #4*2
	stx a:$4305 ; 4 colours
	ldx #1
	stx a:$420B
	; OBJ palettes
	ldx #128
	stx a:$2121 ; CGADD = 128
	lda #.loword(snes_palette+(128*2))
	sta a:$4302
	ldx #4*2
	stx a:$4305 ; 4 colours
	ldx #1
	stx a:$420B
	sep #$20
	.a8
	; nametable update
	lda _ppu_send_addr+0
	sta $2116
	lda _ppu_send_addr+1
	sta $2117
	;lda ppu_2000
	;sta $2000 ; set direction
	ldx #0
	cpx _ppu_send_count
	bcs :++
	:
		lda _ppu_send, X
		sta $2118
		inx
		cpx _ppu_send_count
		bcc :-
	:
	ldx #0
	stx _ppu_send_count
	;lda ppu_2000
	;sta $2000
	lda ppu_2005x
	sta $210D ; BG1HOFS
	lda ppu_2000
	and #1
	sta $210D ; BG1HOFS
	lda ppu_2005y
	sta $210E ; BG1VOFS
	lda ppu_2000
	lsr
	and #1
	sta $210E ; BG1VOFS
	lda #$0F
	sta a:$2100 ; INIDISP screen on
@post_none:
@end:
	rts

.segment "STUB"
reset_stub:
	sei
	clc
	xce
	stz $4200
	rep #$30
	.a16
	.i16
	ldx #$01FF
	txs
	sep #$30
	.a8
	.i8
	jmp reset

.segment "HEADER"

.byte "MSET                 "
.byte $20 ; map mode 0, SlowROM
.byte $00 ; cartridge type (ROM only)
.byte $05 ; 32kb / 0.25 mbit
.byte $00 ; RAM size
.byte $01 ; destination code (north america)
.byte $00 ; dev ID (not $33 indicates v1 header)
.byte $00 ; mask ROM version
.word $0000 ; checksum
.word $FFFF ; checksum complement

; native vectors
.assert (*=$FFE0), error, "native vectors misplaced"
.word $FFFF
.word $FFFF
.word irq ; COP
.word irq ; BRK
.word irq ; ABORT
.word nmi
.word reset_stub
.word irq
; emulation vectors
.assert (*=$FFF0), error, "emulation vectors misplaced"
.word $FFFF
.word $FFFF
.word irq ; COP
.word irq ; BRK
.word irq ; ABORT
.word nmi
.word reset_stub
.word irq
