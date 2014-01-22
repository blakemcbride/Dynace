
# DMAKE makefile for Symantec C 32 bit

DPP = ..\bin\dpp

DPPOPTS = -C $(STRAT)


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

rem	cd ..\windows
rem	echo Entering windows
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

rem	cd ..\odbc
rem	echo Entering odbc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done

:done
	cd ..
]

all-scratch : # setup.dos
%@[
	@echo off

	md lib

	cd kernel
	echo Entering kernel
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) NEW=1
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

rem	cd ..\windows
rem	echo Entering windows
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) NEW=1
rem	if errorlevel 1 goto done

rem	cd ..\odbc
rem	echo Entering odbc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
rem	if errorlevel 1 goto done


:done
	cd ..
]

clean : 
%@[
	@echo off

	cd kernel
	echo Entering kernel
	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

	cd ..\class
	echo Entering class
	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

	cd ..\threads
	echo Entering threads
	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

	cd ..\generics
	echo Entering generics
	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

	cd ..\dpp
	echo Entering dpp
	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

rem	cd ..\windows
rem	echo Entering windows
rem	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

rem	cd ..\odbc
rem	echo Entering odbc
rem	$(MAKE) $(MAKETARGETS) $(MAKEFILE) $(MAKEMACROS)

	cd ..\examples\setup
	echo Entering examples (takes a while - please wait)
	call cleanall.bat

rem	cd ..\..\winexam\setup
rem	echo Entering winexam (takes a while - please wait)
rem	call realcln.bat

	cd ..\..
	bin\rm -zq setup.unx setup.dos *.~ *.?~ *.??~ #*.* *.{*
]

realclean : clean
	bin\rm -zq lib\d*.* bin\dpp bin\dpp.exe include\wingens.h include\odbcgens.h
	bin\rm -zq bin\addcr bin\addcr.exe bin\delcr bin\delcr.exe lib\d*.*

dist:
	bin\rm -zq dynace*.zip
	zip -r dynace README m32.dm makefile.unx makefile.inc change.log
	zip dynace bin\*.* class\*.* docs\*.* dpp\*.* generics\*.* lib\*.*
	zip dynace include\*.* kernel\*.* threads\*.* utils\*.* windows\*.* odbc\*.*
	zip -r dynace examples\list examples\exam* examples\setup
	zip -r dynace winexam\list winexam\exam* winexam\setup


	

makegens:
%@[
	@echo off

	cd kernel
	rm -zq generics.* 
	$(DPP) $(DPPOPTS) -h -i -s *.d
	if errorlevel 1 goto done
	mv generics.h ../include

	cd ..\class
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	mv generics.h ../include

	cd ..\threads
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	mv generics.h ../include

:done
	cd ..
]




newgens : makegens
%@[
	@echo off

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

rem	cd ..\windows
rem	echo Entering windows
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done

rem	cd ..\odbc
rem	echo Entering odbc
rem	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
rem	if errorlevel 1 goto done


:done
	cd ..
]
