@echo off
..\..\bin\rm -zq *.obj
..\..\bin\rm -zq *.exe
..\..\bin\rm -zq *.map
..\..\bin\rm -zq *.aps
..\..\bin\rm -zq *.pdb
..\..\bin\rm -zq *.wsp
..\..\bin\rm -zq *.ilk
..\..\bin\rm -zq *.bsc
..\..\bin\rm -zq *.vcp
..\..\bin\rm -zq *.pch *.sln
..\..\bin\rm -zq *.sbr
..\..\bin\rm -zq *.vcw
..\..\bin\rm -zq *.map
..\..\bin\rm -zq generics.*
..\..\bin\rm -zq class*.c
..\..\bin\rm -zqr WinDebug
..\..\bin\rm -zqr WinRel
..\..\bin\rm -zq *.~ *.?~ *.??~ *.bak
if exist --linux-.--- del --linux-.---

rem From Borland
..\..\bin\rm -zq *.rws
..\..\bin\rm -zq *.~re
..\..\bin\rm -zq *.csm
..\..\bin\rm -zq *.dsw
..\..\bin\rm -zq *.~de

rem Unix
..\..\bin\rm -zq main *.o

rem WATCOM
..\..\bin\rm -zq *.err
