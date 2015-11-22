
# DMAKE makefile for Microsoft C 32 & 64 bit

DPP = ..\bin\dpp

DPPOPTS = -C $(STRAT)

TOUCH = ..\bin\touch



all : # setup.dos
%@[
	@echo off

	cd kernel
	echo Entering kernel
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\class
	echo Entering class
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\threads
	echo Entering threads
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\dpp
	echo Entering dpp
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) install
	if errorlevel 1 goto done

	cd ..\generics
	echo Entering generics
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

:done
	cd ..
]

wds-all : all
%@[
	@echo off

	cd Registry
	echo Entering Registry
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\Windows
	echo Entering Windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\ODBC
	echo Entering ODBC
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\Resources
	echo Entering Resources
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\OLE
	echo Entering OLE
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

rem	cd ..\Widgets
rem	echo Entering Widgets
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

rem	cd ..\WordProc
rem	echo Entering WordProc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

rem	cd ..\WDS
rem	echo Entering WDS
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) curlib.nm
rem	if errorlevel 1 goto done

:done
	cd ..
]

all-scratch : setup.dos
%@[
	@echo off

	md lib

rem	copy bin\win32\dpp.exe bin

	cd kernel
	echo Entering kernel
	rem $(TOUCH) *.c
	rem $(TOUCH) ..\include\generics.h
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) NEW=1
	if errorlevel 1 goto done

	cd ..\class
	echo Entering class
	rem $(TOUCH) *.c
	rem $(TOUCH) ..\include\generics.h
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\threads
	echo Entering threads
	rem $(TOUCH) *.c
	rem $(TOUCH) ..\include\generics.h
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\dpp
	echo Entering dpp
	rem $(TOUCH) *.c
	rem $(TOUCH) ..\include\generics.h
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) install
	if errorlevel 1 goto done

	cd ..\generics
	echo Entering generics
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

:done
	cd ..
]

wds-all-scratch : all-scratch
%@[
	@echo off

	cd Windows
	echo Entering Windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) NEW=1
	if errorlevel 1 goto done

	cd ..\Registry
	echo Entering Registry
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\ODBC
	echo Entering ODBC
	$(TOUCH) sqlgrammar.c sqlgrammar.h sqltokens.c 
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\Resources
	echo Entering Resources
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\OLE
	echo Entering OLE
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

rem	cd ..\Widgets
rem	echo Entering Widgets
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

rem	cd ..\WordProc
rem	echo Entering WordProc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

rem	cd ..\WDS
rem	echo Entering WDS
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) curlib.nm
rem	if errorlevel 1 goto done

:done
	cd ..
]

clean : 
%@[
	@echo off

	cd kernel
	echo Entering kernel
	$(MAKE) clean $(MAKEFILE)

	cd ..\class
	echo Entering class
	$(MAKE) clean $(MAKEFILE)

	cd ..\threads
	echo Entering threads
	$(MAKE) clean $(MAKEFILE)

	cd ..\generics
	echo Entering generics
	$(MAKE) clean $(MAKEFILE)

	cd ..\dpp
	echo Entering dpp
	$(MAKE) clean $(MAKEFILE)

	cd ..\examples\setup
	echo Entering examples (takes a while - please wait)
	call cleanall.bat

	cd ..\..
	bin\rm -zq setup.unx setup.dos setup.p9 *.~ *.?~ *.??~ #*.* *.{*
]

wds-clean : clean
%@[
	@echo off

	cd Registry
	echo Entering Registry
	$(MAKE) clean $(MAKEFILE)

	cd ..\Windows
	echo Entering Windows
	$(MAKE) clean $(MAKEFILE)

	cd ..\ODBC
	echo Entering ODBC
	$(MAKE) clean $(MAKEFILE)

	cd ..\Resources
	echo Entering Resources
	$(MAKE) clean $(MAKEFILE

	cd ..\OLE
	echo Entering OLE
	$(MAKE) clean $(MAKEFILE)

rem	cd ..\Widgets
rem	echo Entering Widgets
rem	$(MAKE) clean $(MAKEFILE)

rem	cd ..\WordProc
rem	echo Entering WordProc
rem	$(MAKE) clean $(MAKEFILE)

rem	cd ..\WDS
rem	echo Entering WDS
rem	$(MAKE) clean $(MAKEFILE)

	cd ..\winexam\setup
	echo Entering winexam (takes a while - please wait)
	call realcln.bat
	cd ..\..
]

realclean :
%@[
	@echo off

	cd kernel
	echo Entering kernel
	$(MAKE) realclean $(MAKEFILE)

	cd ..\class
	echo Entering class
	$(MAKE) realclean $(MAKEFILE)

	cd ..\threads
	echo Entering threads
	$(MAKE) realclean $(MAKEFILE)

	cd ..\generics
	echo Entering generics
	$(MAKE) realclean $(MAKEFILE)

	cd ..\dpp
	echo Entering dpp
	$(MAKE) realclean $(MAKEFILE)

	rem cd ..\examples\setup
	rem echo Entering examples (takes a while - please wait)
	rem call cleanall.bat

	cd ..
	bin\rm -zq setup.unx setup.dos setup.p9 *.~ *.?~ *.??~ #*.* *.{*
	bin\rm -zq lib\d*.* bin\dpp bin\dpp.exe 
	bin\rm -zq bin\addcr bin\addcr.exe bin\delcr bin\delcr.exe lib\d*.*
]


wds-realclean : realclean
%@[
	@echo off

	cd Registry
	echo Entering Registry
	$(MAKE) realclean $(MAKEFILE)

	cd ..\Windows
	echo Entering Windows
	$(MAKE) realclean $(MAKEFILE)

	cd ..\ODBC
	echo Entering ODBC
	$(MAKE) realclean $(MAKEFILE)

	cd ..\Resources
	echo Entering Resources
	$(MAKE) realclean $(MAKEFILE)

	cd ..\OLE
	echo Entering OLE
	$(MAKE) realclean $(MAKEFILE)

rem	cd ..\Widgets
rem	echo Entering Widgets
rem	$(MAKE) realclean $(MAKEFILE)

rem	cd ..\WordProc
rem	echo Entering WordProc
rem	$(MAKE) realclean $(MAKEFILE)

rem	cd ..\WDS
rem	echo Entering WDS
rem	$(MAKE) realclean $(MAKEFILE)

	cd ..\winexam\setup
	echo Entering winexam (takes a while - please wait)
	call realcln.bat
	cd ..\..
]

dist:
	bin\rm -zq dynace*.zip
	zip -r dynace README msc.dm makefile.unx makefile.inc change.log
	zip dynace bin\* class\* docs\* dpp\* generics\* lib\*
	zip dynace include\* kernel\* threads\* utils\* Windows\* ODBC\* Resources\*
	zip -r dynace examples\list examples\exam* examples\setup
	zip -r dynace winexam\list winexam\exam* winexam\setup


makegens:
%@[
	@echo off

	cd kernel
	..\bin\rm -zq generics.* 
	$(DPP) $(DPPOPTS) -h -i -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include

	cd ..\class
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include

	cd ..\threads
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include

:done
	cd ..
]

wds-makegens : makegens
%@[
	@echo off

	cd Windows
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

	cd ..\ODBC
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

	cd ..\OLE
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

rem	cd ..\Widgets
rem	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
rem	if errorlevel 1 goto done
rem	..\bin\mv generics.h ../include/generics.h

rem	cd ..\WordProc
rem	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
rem	if errorlevel 1 goto done
rem	..\bin\mv generics.h ../include/generics.h

	cd ..\Java\Dynace
	..\$(DPP) $(DPPOPTS) -h -i -g ../../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\..\bin\mv generics.h ../../include/generics.h
	cd ..

:done
	cd ..
]


newgens : 
%@[
	@echo off

	bin\rm include\generics.h

	cd kernel
	echo Entering kernel
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) reallynewgens
	if errorlevel 1 goto done

	cd ..\class
	echo Entering class
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\threads
	echo Entering threads
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\dpp
	echo Entering dpp
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) generics.c
	if errorlevel 1 goto done

:done
	cd ..
]


wds-newgens : newgens
%@[
	@echo off

	cd Windows
	echo Entering Windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\ODBC
	echo Entering ODBC
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\OLE
	echo Entering OLE
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

rem	cd ..\Widgets
rem	echo Entering Widgets
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done

rem	cd ..\WordProc
rem	echo Entering WordProc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done

	cd ..\Java\Dynace
	echo Entering Java\Dynace
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	cd ..

:done
	cd ..
]


java-build : 
%@[
	@echo off

	cd Java

	cd DLL
	echo Entering Java\DLL
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) -E
	if errorlevel 1 goto done

	cd ..\Dynace
	echo Entering Java\Dynace
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) -E
	if errorlevel 1 goto done

	cd ..\Java
	echo Entering Java\Java
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) -E
	if errorlevel 1 goto done

:done
	cd ..\..
]

java-newgens : 
%@[
	@echo off

	cd Java\Dynace
	echo Entering Java\Dynace
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	cd ..\..
]

java-makegens : 
%@[
	@echo off

	cd Java\Dynace
	echo Entering Java\Dynace
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) makegens
	cd ..\..
]

java-clean : 
%@[
	@echo off

	cd Java

	cd DLL
	echo Entering Java\DLL
	$(MAKE) clean $(MAKEFILE)
	if errorlevel 1 goto done

	cd ..\Dynace
	echo Entering Java\Dynace
	$(MAKE) clean $(MAKEFILE)
	if errorlevel 1 goto done

	cd ..\Java
	echo Entering Java\Java
	$(MAKE) clean $(MAKEFILE)
	if errorlevel 1 goto done

:done
	cd ..\..
]

java-realclean : 
%@[
	@echo off

	cd Java

	cd DLL
	echo Entering Java\DLL
	$(MAKE) realclean $(MAKEFILE)
	if errorlevel 1 goto done

	cd ..\Dynace
	echo Entering Java\Dynace
	$(MAKE) realclean $(MAKEFILE)
	if errorlevel 1 goto done

	cd ..\Java
	echo Entering Java\Java
	$(MAKE) realclean $(MAKEFILE)
	if errorlevel 1 goto done

:done
	cd ..\..
]

setup.dos :
%@[
	bin\touch kernel\*.c
	bin\touch class\*.c
	bin\touch threads\*.c
	bin\touch dpp\*.c
	bin\touch dpp\generics.*
	pause
	bin\touch include\generics.h
]

# The following target is used to convert an SVN checkout into a shippable distribution
# It must only be run immediatly after a co or export
# It creates a system which can boot without DPP, Windows or WINE
bootable :
%@[
	copy /y bin\win32\* bin
	bin\rm -zq include\generics.h
	$(MAKE) $(MAKEFILE) STRAT=-S2 newgens
	bin\touch  ODBC/sqlgrammar.c ODBC/sqlgrammar.h ODBC/sqltokens.c
	del bin\*.exe
]
