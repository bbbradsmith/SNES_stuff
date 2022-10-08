@del temp\ctrltest.c.s
@del temp\*.o
@del temp\ctrltest_unstrobe.sfc
@del temp\ctrltest.dbg
@del temp\ctrltest.map

cc65\bin\cc65 -o ctrltest.c.s -O -T -g ctrltest.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o ctrltest.c.o -g ctrltest.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o ctrltest.o -g ctrltest.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o ctrltest_unstrobe.sfc -m ctrltest_unstrobe.map --dbgfile ctrltest_unstrobe.dbg -C ctrltest.cfg ctrltest.o ctrltest.c.o runtime.lib
@IF ERRORLEVEL 1 GOTO error

python checksum.py LOROM ctrltest_unstrobe.sfc

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end