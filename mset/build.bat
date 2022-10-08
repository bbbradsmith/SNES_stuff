@del mset.c.s
@del *.o
@del temp\mset.sfc
@del temp\mset.dbg
@del temp\mset.map

cc65\bin\cc65 -o mset.c.s -O -T -g mset.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o mset.c.o -g mset.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o mset.o -g mset.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o mset.sfc -m mset.map --dbgfile mset.dbg -C mset.cfg mset.o mset.c.o runtime.lib
@IF ERRORLEVEL 1 GOTO error

python checksum.py LOROM mset.sfc

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