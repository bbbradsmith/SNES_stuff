del colourmath.o
del colourmath.map
del colourmath.dbg
del colourmath.sfc

cc65\bin\ca65 colourmath.s -g --large-alignment -o colourmath.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o colourmath.sfc -C colourmath.cfg -m colourmath.map --dbgfile colourmath.dbg --large-alignment colourmath.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM colourmath.sfc

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
