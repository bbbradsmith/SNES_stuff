del extbgtest.o
del extbgtest.map
del extbgtest.dbg
del extbgtest.sfc

cc65\bin\ca65 extbgtest.s -g --large-alignment -o extbgtest.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o extbgtest.sfc -C extbgtest.cfg -m extbgtest.map --dbgfile extbgtest.dbg --large-alignment extbgtest.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM extbgtest.sfc

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
