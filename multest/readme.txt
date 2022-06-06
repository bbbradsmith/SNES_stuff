"Multest" hardware multiply algorithm test for SNES

Test of an algorithm for unsigned 16-bit x 16-bit = 32-bit multiply ,
sing SNES hardware multiply.

Algorithm written by 93143 here:
https://forums.nesdev.org/viewtopic.php?p=280007#p280007


This test tries all 2^32 combinations of input values.
Value A is cycled in order by incrementing by a large prime value.
Value B is cycles through all 2^16 combinations in order.
There's pretty high confidence after 10 variations or so of value A,
but a complete run is probably about 100 hours.

In the event of failure, will halt and print a failure message with the input
values and bad result. Otherwise prints "Pass!" after it's complete.


Rebuild requirements:
python 3 - gfx.py will rebuild the chr/pal assets
cc65 - put it in a cc65 folder and run build.bat


rainwarrior, 2022
http://rainwarrior.ca
