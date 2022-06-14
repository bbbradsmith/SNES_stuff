del *.o
del *.map
del *.dbg
del *.sfc

REM common test framework

cc65\bin\ca65 multest.s -g --large-alignment -o multest.o  || @goto error

REM individual test source

cc65\bin\ca65 test_mul16.s -g -o test_mul16.o  || @goto error
cc65\bin\ca65 test_div16.s -g -o test_div16.o  || @goto error

REM link test ROMs

cc65\bin\ld65 -o multest_mul16.sfc -C multest.cfg -m multest_mul16.map --dbgfile multest_mul16.dbg --large-alignment multest.o test_mul16.o || @goto error
cc65\bin\ld65 -o multest_div16.sfc -C multest.cfg -m multest_div16.map --dbgfile multest_div16.dbg --large-alignment multest.o test_div16.o || @goto error

REM fix checksums

python checksum.py HIROM multest_mul16.sfc
python checksum.py HIROM multest_div16.sfc

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
