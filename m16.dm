
# DMAKE makefile for Microsoft C 16 bit

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

	cd softprot
	echo Entering softprot
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\windows
	echo Entering windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\odbc
	echo Entering odbc
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\ole
	echo Entering ole
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

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

	cd softprot
	echo Entering softprot
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\windows
	echo Entering windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) NEW=1
	if errorlevel 1 goto done

	cd ..\odbc
	echo Entering odbc
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

	cd ..\ole
	echo Entering ole
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done
	$(MAKE) $(MAKEFILE) $(MAKEMACROS)
	if errorlevel 1 goto done

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
	bin\rm -zq setup.unx setup.dos *.~ *.?~ *.??~ #*.* *.{*
]

wds-clean : clean
%@[
	@echo off

	cd softprot
	echo Entering softprot
	$(MAKE) clean $(MAKEFILE)

	cd ..\windows
	echo Entering windows
	$(MAKE) clean $(MAKEFILE)

	cd ..\odbc
	echo Entering odbc
	$(MAKE) clean $(MAKEFILE)

	cd ..\ole
	echo Entering ole
	$(MAKE) clean $(MAKEFILE)

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

	cd ..\examples\setup
	echo Entering examples (takes a while - please wait)
	call cleanall.bat

	cd ..\..
	bin\rm -zq setup.unx setup.dos *.~ *.?~ *.??~ #*.* *.{*
	bin\rm -zq lib\d*.* bin\dpp 
	bin\rm -zq bin\addcr bin\addcr.exe bin\delcr bin\delcr.exe lib\d*.*
]


wds-realclean : realclean
%@[
	@echo off

	cd softprot
	echo Entering softprot
	$(MAKE) realclean $(MAKEFILE)

	cd ..\windows
	echo Entering windows
	$(MAKE) realclean $(MAKEFILE)

	cd ..\odbc
	echo Entering odbc
	$(MAKE) realclean $(MAKEFILE)

	cd ..\ole
	echo Entering ole
	$(MAKE) realclean $(MAKEFILE)

	cd ..\winexam\setup
	echo Entering winexam (takes a while - please wait)
	call realcln.bat
	cd ..\..
]

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

	cd windows
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

	cd ..\odbc
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

	cd ..\ole
	$(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d
	if errorlevel 1 goto done
	..\bin\mv generics.h ../include/generics.h

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

	cd windows
	echo Entering windows
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\odbc
	echo Entering odbc
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

	cd ..\ole
	echo Entering ole
	$(MAKE) $(MAKEFILE) $(MAKEMACROS) newgens
	if errorlevel 1 goto done

:done
	cd ..
]

