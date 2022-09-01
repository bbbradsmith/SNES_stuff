"Elasticity" flicker colour demo for SNES

This ROM displays 4 versions of an image, press a button to switch:

B - A pair of 3:3:2 images flickering to create a "4:4:3" image.
Y - 256 colours, no-dither.
A - 256 colours, Floyd-Steinberg dithering.
X - A 3:3:2 image added to a 1:1:2 image to create a 4:4:4 image. (default)

All images use Mode 3 in 8bpp indexed colour mode.
Though the flicker version is 3:3:2 quantized, it is not using direct colour,
because its gamut of contrast can be slightly widened with a 256 colour palette.
The "X" mode was suggested by Oziphantom. Since it's clearly better it's now the default.


The image is Umberto Boccioni's Elasticity, 1912 (Public Domain)
https://www.wikiart.org/en/umberto-boccioni/elasticity-1912


Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
python 3 - gfx.py will rebuild the chr/pal assets
pillow - python image library needed for gfx.py


Notes:
https://forums.nesdev.org/viewtopic.php?p=278742



rainwarrior, 2022
http://rainwarrior.ca
