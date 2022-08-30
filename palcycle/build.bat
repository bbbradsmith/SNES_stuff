del palcycle.o
del palcycle.map
del palcycle.dbg
del palcycle.sfc

cc65\bin\ca65 palcycle.s -g --large-alignment -o palcycle.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o palcycle.sfc -C palcycle.cfg -m palcycle.map --dbgfile palcycle.dbg --large-alignment palcycle.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM palcycle.sfc

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
