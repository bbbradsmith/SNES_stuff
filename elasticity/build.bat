del elasticity.o
del elasticity.map
del elasticity.dbg
del elasticity.sfc

cc65\bin\ca65 elasticity.s -g -o elasticity.o
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o elasticity.sfc -C elasticity.cfg -m elasticity.map --dbgfile elasticity.dbg elasticity.o
@IF ERRORLEVEL 1 GOTO error

python checksum.py HIROM elasticity.sfc

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
