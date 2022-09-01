SNES Colour Math Demo


This is a simple ROM that loads some graphics and display them,
in order to demonstrate a few ways to use SNES colour math.

Simply press any button to cycle through the demo screens.
Use a debugger to inspect them for more information.

Some futher explanation is given at:
  https://snes.nesdev.org/wiki/Color_math#Examples


Modes:

1. add + half 50% blend, "water"
   Main = BG + OBJ + Water
   Sub  = BG + OBJ

2. add light, "ghost fade 1"
   Main = OBJ
   Sub  = BG

3. add dark, "ghost fade 2"
   (Same as 2 with darker ghost palette.)

4. subtract, "darkness, some sprites"
   Main = BG + OBJ
   Sub  = Inverse Darkness

5. subtract inverse, "darkness, all sprites"
   Main = Darkness
   Sub  = Inverse BG + Inverse OBJ


== Build Info ==

Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
python 3 - gfx.py will rebuild the chr/pal/nmt assets (optional)
pillow - python image library needed for gfx.py


== Art ==

This demo incorporates graphic art from OpenGameArt.org under Creative Commons licenses:

* GrafxKid - Mini Fantasy Sprites
  CC0 (Public Domain)
  https://opengameart.org/content/mini-fantasy-sprites

* Surt - Dawn of the Gods
  CC0 (Public Domain)
  https://opengameart.org/content/dawn-of-the-gods


== Author ==

rainwarrior, 2022
http://rainwarrior.ca
