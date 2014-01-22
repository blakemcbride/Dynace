@echo off
rm -zq *.bak
rm -zq *.ckp
rm -zq *.bk2
rm -zq emacsmem
rm -zq *.~
rm -zq *.?~
rm -zq *.??~
rm -zq #*.*
rm -zq *.obj *.o curlib.* *.err
if exist --linux-.--- del --linux-.---
rm -zq *.{*
