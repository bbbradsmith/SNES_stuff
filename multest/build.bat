del *.o
del *.map
del *.dbg
del *.sfc

REM common test framework

cc65\bin\ca65 multest.s -g --large-alignment -o multest.o
@IF ERRORLEVEL 1 GOTO error

REM individual test source

cc65\bin\ca65 test_mul16.s -g --large-alignment -o test_mul16.o
@IF ERRORLEVEL 1 GOTO error

REM link test ROMs

cc65\bin\ld65 -o multest_mul16.sfc -C multest.cfg -m multest_mul16.map --dbgfile multest_mul16.dbg --large-alignment multest.o test_mul16.o
@IF ERRORLEVEL 1 GOTO error

REM fix checksums

python checksum.py HIROM multest_mul16.sfc

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
