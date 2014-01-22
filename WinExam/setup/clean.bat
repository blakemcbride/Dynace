@echo off
..\..\bin\rm -zq *.obj
..\..\bin\rm -zq *.exe
..\..\bin\rm -zq *.ex1
..\..\bin\rm -zq *.map
..\..\bin\rm -zq *.res
..\..\bin\rm -zq *.aps
..\..\bin\rm -zq *.pdb
..\..\bin\rm -zq *.wsp
..\..\bin\rm -zq *.ilk
..\..\bin\rm -zq *.mdp
..\..\bin\rm -zq *.ncb
..\..\bin\rm -zq *.idb
..\..\bin\rm -zq *.bsc
..\..\bin\rm -zq *.vcp
..\..\bin\rm -zq *.pch
..\..\bin\rm -zq *.sbr *.opt *.sln
..\..\bin\rm -zq *.vcw
..\..\bin\rm -zq *.map
..\..\bin\rm -zq class1.c
..\..\bin\rm -zq generics.*
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
