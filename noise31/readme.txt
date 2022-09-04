Noise31


This is a minimal ROM to demonstrate the SNES hardware noise,
and maybe how to get started using the SPC-700.

Run the ROM, and it will play hardware noise on voice 0,
at maximum volume and maximum frequency (31).

This was intended to investigate why the SNES noise seems to have a strong
highpass effect. Discussion here:
  https://forums.nesdev.org/viewtopic.php?p=282889#p282889


== Build Info ==

Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
wla-dx - put it in a wla-dx folder (wla-spc700, wlalink)
python 3 - for checksum (optional)


== Author ==

rainwarrior, 2022
http://rainwarrior.ca
