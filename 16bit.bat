@echo off

rem  The following line must be set to the location of the Dynace system
set dynloc=c:\Dynace

rem  If using the borland compiler correct the following line
set BORLAND_HOME=c:\bc45

rem  The files under bin\dos may be from a separate download.  Be sure to get them.
copy bin\dos\*.* bin

PATH %dynloc%\bin;%path%

set MAKESTARTUP=%dynloc%\utils\startup.mk

set DOS4G=quiet
set DOS16M=:4M

:end

set dynloc=
