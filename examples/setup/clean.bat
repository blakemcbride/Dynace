@echo off
del /Q *.obj 2>NUL
del /Q *.exe 2>NUL
del /Q *.map 2>NUL
del /Q *.aps 2>NUL
del /Q *.pdb 2>NUL
del /Q *.wsp 2>NUL
del /Q *.ilk 2>NUL
del /Q *.bsc 2>NUL
del /Q *.vcp 2>NUL
del /Q *.pch *.sln 2>NUL
del /Q *.sbr 2>NUL
del /Q *.vcw 2>NUL
del /Q generics.* 2>NUL
del /Q class*.c 2>NUL
if exist WinDebug rmdir /S /Q WinDebug
if exist WinRel rmdir /S /Q WinRel
del /Q *.bak 2>NUL
if exist --linux-.--- del --linux-.---

rem From Borland
del /Q *.rws 2>NUL
del /Q *.csm 2>NUL
del /Q *.dsw 2>NUL

rem Unix
del /Q main *.o 2>NUL

rem WATCOM
del /Q *.err 2>NUL
