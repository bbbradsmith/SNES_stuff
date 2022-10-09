//
// ctrltest.c
// Generic NES Controller Test, by Brad Smith 2020
// sdapted for SNES: 2022
// http://rainwarrior.ca
//

// TODO just spit out bits instead, maybe add X/Y/s/S/L/R/U/D to sprites

// for debugging performance
//#define PROFILE() ppu_profile(0x41);
#define PROFILE() {}

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

extern uint8* ptr;
extern uint8 i,j;
#pragma zpsym("ptr")
#pragma zpsym("i")
#pragma zpsym("j")

extern uint8 input[12];
#pragma zpsym("input")

extern uint8 sprite_chr[];
#define SPRITE_CHR_SIZE (16 * 8 * 16)

extern uint8 prng(); // 8-bit random value
extern void input_poll();

extern void ppu_latch(uint16 addr); // write address to PPU latch
extern void ppu_direction(uint8 vertical); // set write increment direction
extern void ppu_write(uint8 value); // write value to $2007
extern void ppu_load(uint16 count); // uploads bytes from ptr to $2007 (clobbers ptr)
extern void ppu_fill(uint8 value, uint16 count); // uploads single value to $2007
extern void ppu_ctrl(uint8 v); // $2000, only bits 4-6 count (tile pages, sprite height), applies at next post
extern void ppu_mask(uint8 v); // $2001, applies at next post
extern void ppu_scroll_x(uint16 x);
extern void ppu_scroll_y(uint16 y);
extern void ppu_post(uint8 mode); // waits for next frame and posts PPU update
extern void ppu_profile(uint8 emphasis); // immediate $2001 write, OR with current mask (use bit 0 for greyscale)

// POST_OFF     turn off rendering
// POST_NONE    turn on, no other updates
// POST_UPDATE  turn on, palette, send
// POST_DOUBLE  turn on, palette, send 64 bytes across 2 nametables
#define POST_OFF    1
#define POST_NONE   2
#define POST_UPDATE 3
#define POST_DOUBLE 4

extern uint16 ppu_send_addr;
extern uint8 ppu_send_count;
extern uint8 ppu_send[64];

extern uint8 palette[32];
extern uint8 oam[256];

void cls() // erase nametables
{
	ppu_latch(0x2000);
	ppu_fill(0xFF,0x1000);
}

uint8 line = 0;
uint8 oam_pos = 0;

void add_sprite(uint8 x, uint8 y, uint8 tile, uint8 attributes)
{
	oam[oam_pos+0] = y-1;
	oam[oam_pos+1] = tile;
	oam[oam_pos+2] = attributes;
	oam[oam_pos+3] = x;
	oam_pos += 4;
}

void add_hex_sprite(uint8 x, uint8 y, uint8 value)
{
	add_sprite(x,y,value>>4,0);
	add_sprite(x+8,y,value&0xF,0);
}

const uint8 heading[] = {
	'B','Y','s','S','u','d','l','r','A','X','l','r','1',' ',
	'B','Y','s','S','u','d','l','r','A','X','l','r','2',' ',
};

void test()
{
	cls();
	
	for (i=0; i<32; i+=4)
	{
		palette[i+0] = 0x0F;
		palette[i+1] = 0x21;
		palette[i+2] = 0x10;
		palette[i+3] = 0x30;
	}
	palette[12+3] = 0x30; // grey background text
	
	ppu_send_addr = 0x2000 + 2 + (2 * 32);
	for (i=0; i<sizeof(heading); ++i) ppu_send[i] = heading[i];
	ppu_send_count = sizeof(heading);
	ppu_post(POST_UPDATE);

	while (1)
	{
		input_poll();
		
		oam_pos = 0;

		add_sprite(1*8,(4+line)*8,0x11,0x40);
		ppu_send_addr = 0x2000 + 2 + ((4+line) * 32);
		ppu_send_count = 24+3;
		for (i=0; i<8; ++i)
		{
			ppu_send[ 0+i] = (input[1] >> (7-i)) & 1;
			ppu_send[14+i] = (input[3] >> (7-i)) & 1;
		}
		for (i=0; i<4; ++i)
		{
			ppu_send[ 8+i] = (input[0] >> (7-i)) & 1;
			ppu_send[22+i] = (input[2] >> (7-i)) & 1;
		}
		ppu_send[12] = input[0] & 0x0F;
		ppu_send[13] = 0xFF;
		ppu_send[26] = input[2] & 0x0F;
		++line; if (line >= 24) line = 0;
		
		while (oam_pos != 0)
		{
			oam[oam_pos+0] = 0xFF;
			oam_pos += 4;
		}

		PROFILE();
		ppu_post(POST_UPDATE);
	}
}

void main()
{
	ppu_latch(0x0000);
	ppu_fill(0x00,8*1024);

	ptr = sprite_chr;
	ppu_latch(0x0000);
	ppu_load(SPRITE_CHR_SIZE);

	ppu_ctrl(0x00);

	ppu_scroll_x(0);
	ppu_scroll_y(0);
	test();

	return;
}
