"Multest" tests for SNES

This test framework tries all 2^32 combinations of input values.
Value A is cycled in order by incrementing by a large prime value.
Value B is cycles through all 2^16 combinations in order.
There's pretty high confidence after 10 variations or so of value A,
but a complete run is probably about 100 hours.

In the event of failure, will halt and print a failure message with the input
values and bad result. Otherwise prints "Pass!" after it's complete.

== mul16 ==

Test of an algorithm for unsigned 16-bit x 16-bit = 32-bit multiply,
using SNES hardware multiply.

Runs in roughly 0.5 scanlines, vs. 2 scanlines for reference binary multiply.

Algorithm written by 93143 here:
https://forums.nesdev.org/viewtopic.php?p=280007#p280007

== div16 ==

Test of an algorithm for unsigned 16-bit / 16-bit = 16-bit divide + remainder,
partially using SNES hardware divide and multiply.

Runs in roughly 0.2 to 1.0 scanlines, vs. 2 scanlines for reference binary divide.

Algorithm based on psycopathicteen's technique here:
https://wiki.superfamicom.org/16-bit-multiplication-and-division/

== Build Info ==

Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
python 3 - gfx.py will rebuild the chr/pal assets
pillow - python image library needed for gfx.py


rainwarrior, 2022
http://rainwarrior.ca
