@echo off
del /Q *.bak 2>NUL
del /Q *.ckp 2>NUL
del /Q *.bk2 2>NUL
del /Q emacsmem 2>NUL
del /Q *.obj *.o curlib.* *.err 2>NUL
if exist --linux-.--- del --linux-.---
