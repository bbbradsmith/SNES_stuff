del noise31.o
del noise31.map
del noise31.dbg
del noise31.sfc

wla-dx\wla-spc700 -o spc.o spc.s
@IF ERRORLEVEL 1 GOTO error

wla-dx\wlalink -b spc.link spc.bin 
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 noise31.s -g --large-alignment -o noise31.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o noise31.sfc -C noise31.cfg -m noise31.map --dbgfile noise31.dbg --large-alignment noise31.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM noise31.sfc

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
