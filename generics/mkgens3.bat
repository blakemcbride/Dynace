@echo off

rem  Batch file used to create (from scratch) the base generics.h file
rem  included with Dynace for C++ inlines

cd ..\kernel
del /Q generics.* 2>NUL
..\bin\dpp -C -S3 -h -i -s *.d
move generics.h ..\include

cd ..\class
..\bin\dpp -C -S3 -h -i -g ../include/generics.h -s *.d
move generics.h ..\include

cd ..\threads
..\bin\dpp -C -S3 -h -i -g ../include/generics.h -s *.d
move generics.h ..\include

cd ..\generics
