IPLFast


An experiment trying to speed up the SPC initial program load,
by replacing the loading loop with one that reads 3 bytes per acknowledge.

This seems to have about 150% bandwidth compared to the IPL.


== Build Info ==

Rebuild requirements:
cc65 - put it in a cc65 folder and run build.bat
wla-dx - put it in a wla-dx folder (wla-spc700, wlalink)
python 3 - for checksum (optional)


== Author ==

rainwarrior, 2022
http://rainwarrior.ca
