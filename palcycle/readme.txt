SNES 256-Colour Palette Cycling Demo


This is a simple ROM that loads some graphics and display them,
in order to demonstrate 256-colour palette cycling.

Video explanation:
- https://www.youtube.com/watch?v=7LAY4ulp63Y


Ideally I'd like to test this with Mark Ferrari's Living Worlds:
- http://www.effectgames.com/demos/canvascycle/
- http://pixfabrik.com/livingworlds/

However, since it is not public domain art, instead I've used a few generated
images. A few are "plasma" style effects inspired by AcidWarp and Maelstrom
(thanks to lidnariq for the suggestion). The generating programs are in the
art_src/ folder. Links to the inspiring programs:
- http://www.noah.org/acidwarp/
- http://eyecandyarchive.com/Maelstrom%20(MS-DOS)/

This is essentially a "gallery" program. If you have 256-colour images of your
own, try substituting them into the list in gfx.py and rebuild it with your art.


The idea was just to show the SNES in mode 3, which gives it a 256 colour
palette that is well suited to arbitrary palette animation effects. Basically
it can do the same kind of palette techniques that PC demos used to. Maybe it
could be fun to just re-implement AcidWarp as a SNES program...

8bpp images like this take up a lot of VRAM. When covering the full screen like
this: double buffering is not possible, and there is very little room for
sprites or anything else. However, you could address this by reducing the
coverage of the 8bpp area, or even just live with this drawback and do slow
256-colour software rendering, like PC games had to do. The point is kind
of that you can get a lot of animation done with palettes alone, especially
with 256 colours.

Of course, you can do this stuff with 16-colour mode 1 graphics too, which
doesn't have those problems, but with less freedom of range, and the added
attribute region constraints.


Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
python 3 - gfx.py will rebuild the chr/pal assets (optional)
pillow - python image library needed for gfx.py


rainwarrior, 2022
http://rainwarrior.ca
