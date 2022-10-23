del smalltext.o
del smalltext.map
del smalltext.dbg
del smalltext.sfc

cc65\bin\ca65 smalltext.s -g --large-alignment -o smalltext.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o smalltext.sfc -C smalltext.cfg -m smalltext.map --dbgfile smalltext.dbg --large-alignment smalltext.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM smalltext.sfc

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
