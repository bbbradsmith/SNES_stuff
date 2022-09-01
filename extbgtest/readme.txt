"Extbgtest"

Shows colours and layering of Mode 7 with EXTBG.

The row marked 0 uses pixel values from 0-127
The row marked 1 uses pixel values from 128-255
The BG palettes for 128+ are darkened to show that BG1 is using them even with EXTBG enabled
The 4 movable sprites are on their numbered layer.

Select = toggle direct color
Start = toggle BG1 or BG2
A/X/Y/B = select sprite 0/1/2/3 to move
d-pad = move sprite


Expected results:
- Indexed BG2
  * Sprites 0,1 in between, 2,3 on top.
  * Layer strips 0 and 1 have identical colours (indicates high bit of palette is forced to 0 on BG2).
- Direct BG2
  * Direct colour disallowed, indexed colours on DC data. (Grey with green, yellow, and cyan streaks.)
- Indexed BG1
  * Sprite 0 behind, 1,2,3 on top.
  * "Layer 1" strip is slightly dim (indicates using full 8bpp palette, not just 7bpp).
- Direct BG1
  * Like indexed BG1 but slightly darker due to reduced range of direct color.


Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
python 3 - gfx.py will rebuild the chr/pal assets
pillow - python image library needed for gfx.py


rainwarrior 2022
http://rainwarrior.ca
