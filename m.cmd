@echo off
::
::	Make file. Compile EE.PAS to EE.EXE
::

fpc.exe ee_main.pas -b -oee.exe

del *.o
del *.ppu

copy ee.exe ..\bin\ee.exe