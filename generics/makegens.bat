@echo off

rem  Batch file used to create (from scratch) the base generics.h file
rem  included with Dynace

cd ..\kernel
..\bin\rm -q generics.*
..\bin\dpp -C -h -i -s *.d
..\bin\mv generics.h ../include

cd ..\class
..\bin\dpp -C -h -i -g ../include/generics.h -s *.d
..\bin\mv generics.h ../include

cd ..\threads
..\bin\dpp -C -h -i -g ../include/generics.h -s *.d
..\bin\mv generics.h ../include

cd ..\generics
