del multest.o
del multest.map
del multest.dbg
del multest.sfc

cc65\bin\ca65 multest.s -g --large-alignment -o multest.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o multest.sfc -C multest.cfg -m multest.map --dbgfile multest.dbg --large-alignment multest.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM multest.sfc

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
