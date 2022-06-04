del dizworld.o
del dizworld.map
del dizworld.dbg
del dizworld.sfc

cc65\bin\ca65 dizworld.s -g --large-alignment -o dizworld.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o dizworld.sfc -C dizworld.cfg -m dizworld.map --dbgfile dizworld.dbg --large-alignment dizworld.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM dizworld.sfc

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
