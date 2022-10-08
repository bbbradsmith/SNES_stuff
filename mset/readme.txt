mset.nes
SNES Mouse test, by Brad Smith 2019
Updated for SNES: 2012
http://rainwarrior.ca

Plug a SNES mouse (or equivalent) into either SNES port.
On reset, this program will search for a mouse on any of the four appropriate data lines. ($4016 d0/1, $4016 d0/1)
The first one found will be selected as the mouse. (The mouse is any device whose report's second byte ends with %0001)

5 bytes are read each frame.
A record of the last 24 frames is displayed on the left hand side.

The top right displays the last report.
The first and last bytes there are raised to indicate they are less important.
(First byte should be 00, last byte is past the end of the report.)

Just beneath this are two numbers, the first is the mouse port, and the second is the current sensitivity.
Port 0: $4016 d0 (NES)
Port 1: $4017 d0
Port 2: $4016 d1 (Famicom)
Port 3: $4017 d1
Port -: No mouse found.
Click the right mouse button to cycle the sensitivity setting.

Below this are two numbers indicating the lowest and highest X displacement reported,
followed by two more numbers indicating lowest and highest Y displacement reported.
Click the left mouse button to reset the lowest and highest to 0.

In the middle is the current offset displayed visually with a circle over a cross.


Source code requires CC65. (Place it in an adjacent cc65 folder, run build.bat.)
https://cc65.github.io/

build_runtime.bat is used to rebuild runtime.lib (instructions are contained as comments)
preprocessor.bat is used to diagnose preprocessor usage


NES version:
https://forums.nesdev.org/viewtopic.php?p=231608#p231608
