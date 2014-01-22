@echo off

rem  The following line must be set to the location of the Dynace system
set dynloc=c:\Dynace

rem  If using the borland compiler correct the following line
set BORLAND_HOME=c:\bc45

PATH %dynloc%\bin;%path%

set MAKESTARTUP=%dynloc%\utils\startup.mk

:end

set dynloc=

pause

