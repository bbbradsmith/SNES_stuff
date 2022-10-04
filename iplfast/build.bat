del iplfast.o
del iplfast.map
del iplfast.dbg
del iplfast.sfc

wla-dx\wla-spc700 -o iplfast.spc.o iplfast.spc.s
@IF ERRORLEVEL 1 GOTO error

wla-dx\wlalink -b iplfast.spc.link iplfast.spc.bin 
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 iplfast.s -g --large-alignment -o iplfast.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o iplfast.sfc -C iplfast.cfg -m iplfast.map --dbgfile iplfast.dbg --large-alignment iplfast.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py LOROM iplfast.sfc

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
