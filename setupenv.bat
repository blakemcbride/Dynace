@echo off

rem  The following line must be set to the location of the Dynace system
set dynloc=%CD%

xcopy /Y bin\win32\*  bin

PATH %dynloc%\bin;%path%

set MAKESTARTUP=%dynloc%\utils\startup.mk

:end

set dynloc=

pause

