del twoship.o
del twoship.map
del twoship.dbg
del twoship.sfc

cc65\bin\ca65 twoship.s -g -o twoship.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o twoship.sfc -C twoship.cfg -m twoship.map --dbgfile twoship.dbg twoship.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM twoship.sfc

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
