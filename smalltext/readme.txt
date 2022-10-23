Smalltext Demo


This is a ROM that uses high resolution and interlacing to display a
demonstration of a 64x60 character text display.

D-pad will scroll the text.
L/R will adjust the brightness of the text shadow.


This can look very good in an emulator, or if weaved to 480 lines.
On a CRT vertically-thick characters are needed to reduce the flicker effect,
which can be achieved in this demo by increasing the brightness.

Unfortunately, because high-resolution tiles are 16x8 pixel, we cannot simply
address characters in the nametable, and for this demonstration I built a
custom text image. For arbitrary text, you might have to reserve almost all of
VRAM just for character rendering, and have very slow updates.


The text itself is an excerpt from the public domain short story,
"The Most Dangerous Game" by Richard Connell, 1924.
I had used it earlier in an NES text compression demo, which can be found here:
https://github.com/bbbradsmith/huffmunch/tree/master/danger


rainwarrior, 2022
http://rainwarrior.ca
