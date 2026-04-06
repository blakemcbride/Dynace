@echo off

rem  Batch file used to create (from scratch) the base generics.h file
rem  included with Dynace

cd ..\kernel
del /Q generics.* 2>NUL
..\bin\dpp -C -h -i -s *.d
move generics.h ..\include

cd ..\class
..\bin\dpp -C -h -i -g ../include/generics.h -s *.d
move generics.h ..\include

cd ..\threads
..\bin\dpp -C -h -i -g ../include/generics.h -s *.d
move generics.h ..\include

cd ..\generics
