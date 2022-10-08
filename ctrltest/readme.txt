CTRLTEST
Brad Smith, 2020
Adapted for SNES: 2022

ctrltest.sfc

This is a simple test ROM to check the controller input lines on SNES.

Each frame strobes $4016, then reads 24 bits from each of the 2 data lines
on $4016 and $4017.

The bits are displayed left to right in a big-endian hexadecimal manner:
 800000 = first bit
 400000 = second bit
 200000 = third bit
 100000 = fourth bit
 080000 = fifth bit
 ...
 008000 = ninth bit
 000800 = thirteenth bit
 000100 = sixteenth bit
 000001 = twenty-fourth bit

This is suitable for reading controllers that report up to 24 bits on any of
the data lines. Devices that need longer reports, such as
the SNES mouse are less suitable for these test ROMs, though you still get
the first 24 bits of their report.

Related: MSET test ROM for SNES mouse
https://github.com/bbbradsmith/SNES_stuff/tree/main/mset

NES version of CTRLTEST:
https://forums.nesdev.org/viewtopic.php?f=2&t=19752

Built with cc65


Brad Smith
http://rainwarrior.ca
