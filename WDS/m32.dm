
#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C 32 bit


TARGET = wds

OTHERLIBS = ..\..\..\Integra\Dynace.Z

C_SRC = main.c LoadDynClasses.c

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

.IF $(ADS)
ADSDIR = $(ADS)
ADSINC = $(ADSDIR)\ads-inc
.ELSE
ADSDIR = ..\..\ADSW.Subversion
ADSINC = $(ADSDIR)\ads-inc
.END

.IF $(BAR)
BARDIR = $(BAR)
.ELSE
BARDIR = $(OTHERLIBS)
.END

.IF $(MZSCHEME)
MZDIR = $(MZSCHEME)
.ELSE
MZDIR = $(OTHERLIBS)
.END

#if 0
#define WINVER	-DWINVER=0x0500
#endif

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -Zi -Fd$(LIBDIR)\dynm32.pdb -D_WINDOWS $(CFEXTRA) $(WINVER)
LFLAGS = /nologo /subsystem:windows /debug /nodefaultlib:libc.lib $(WINVER)
#LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -O2 -D_WINDOWS $(CFEXTRA) $(WINVER)
LFLAGS = /nologo /subsystem:windows /nodefaultlib:libc.lib $(WINVER)
#LFLAGS = /nologo /subsystem:windows
.END

GFLAGS = -I$(INCDIR) -nologo -Oityb1 -Gs -Gy $(CFEXTRA)


.IF $(NATIVE_THREADS)
LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib $(MZDIR)\lib\mzscheme-threads.lib $(BARDIR)\lib\{bar32 zip32}.lib $(ADSDIR)\lib\adscm.lib
.ELSE
LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib $(MZDIR)\lib\mzscheme.lib $(BARDIR)\lib\{bar32 zip32}.lib $(ADSDIR)\lib\adscm.lib $(OTHERLIBS)\lib\{ssleay32 libeay32 sqllite js32}.lib
.END

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 comctl32 advapi32 shell32 odbc32 winmm ole32 wsock32 wininet rpcrt4 mpr msimg32}.lib
#MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 odbc32 winmm ole32}.lib

OBJS = {$(C_SRC:b)}.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


RESOURCES = main.res

.rc.res:
	rc $<

all : curlib.nm $(TARGET)-odbc.exe $(TARGET)-ctree.exe $(TARGET)-both.exe version.exe load.exe load-windows.exe

LoadDynClasses.c : $(INCDIR)\generics.h excludeLoadDynClass.sed excludeDynaceClassRef.sed
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -j Dynace.generics
	sed -f excludeLoadDynClass.sed $@ >tmp.sed
	mv tmp.sed $@
	sed -f excludeDynaceClassRef.sed Dynace.java >tmp.sed
	mv tmp.sed Dynace.java


curlib.nm .LIBRARY : Version.obj versions.obj RTPatch.obj scheme-vargs.obj scheme-bar.obj
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib $(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
	$(BINDIR)\rm -zq $<
	echo Done >$@

$(TARGET)-both.exe : $(OBJS) $(RESOURCES) $(LIBS) scminter-both.obj generics.obj
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2) scminter-both.obj generics.obj\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)

$(TARGET)-odbc.exe : $(OBJS) $(RESOURCES) $(LIBS) scminter-odbc.obj generics.obj
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2) scminter-odbc.obj generics.obj\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)

$(TARGET)-ctree.exe : $(OBJS) $(RESOURCES) $(LIBS) scminter-ctree.obj generics.obj
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2) scminter-ctree.obj generics.obj\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)

version.exe : version.c
	cl -nologo -DTEST version.c
	$(BINDIR)\rm -zq version.obj

load.exe : load.c
	cl -nologo load.c
	$(BINDIR)\rm -zq load.obj

load-windows.exe : load-windows.c
	cl -nologo load-windows.c
	$(BINDIR)\rm -zq load-windows.obj

scheme-vargs.c : makevargs.exe
	makevargs 9 >$@

makevargs.exe : makevargs.c
	cl -nologo $<


$(OBJS) : resource.h


scminter-both.c : generics.h exclude-both.sed
	$(BINDIR)\dpp -g generics.h -L2 $@
	sed -f exclude-both.sed $@ >tmp.sed
	mv tmp.sed $@

scminter-odbc.c : generics.h exclude-ctree.sed
	$(BINDIR)\dpp -g generics.h -L2 $@
	sed -f exclude-ctree.sed $@ >tmp.sed
	mv tmp.sed $@

scminter-ctree.c : generics.h exclude-odbc.sed
	$(BINDIR)\dpp -g generics.h -L2 $@
	sed -f exclude-odbc.sed $@ >tmp.sed
	mv tmp.sed $@

generics.h generics.c :  $(INCDIR)\generics.h $(ADSDIR)\ads-inc\generics.h
	$(BINDIR)\dpp -g $< -h -c

generics.obj : generics.c 
	$(CC) $(GFLAGS) -c generics.c


clean:
	$(BINDIR)\rm -zq scminter-both.c scminter-odbc.c scminter-ctree.c scheme-vargs.c generics.*
	$(BINDIR)\rm -zq LoadDynClasses.c
	$(BINDIR)\rm -zq *.obj
	$(BINDIR)\rm -zq *.exe
	$(BINDIR)\rm -zq *.ex1
#	$(BINDIR)\rm -zq *.res
	$(BINDIR)\rm -zq *.aps
	$(BINDIR)\rm -zq *.pdb
	$(BINDIR)\rm -zq *.wsp
	$(BINDIR)\rm -zq *.ilk
	$(BINDIR)\rm -zq *.bsc
	$(BINDIR)\rm -zq *.vcp
	$(BINDIR)\rm -zq *.pch
	$(BINDIR)\rm -zq *.sbr
	$(BINDIR)\rm -zq *.vcw
	$(BINDIR)\rm -zq *.map
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel
	$(BINDIR)\rm -zq *.idb *.mdp *.ncb
	$(BINDIR)\rm -zq *.dsw *.opt *.plg
	$(BINDIR)\rm -zq *.dsp
	$(BINDIR)\rm -zq *~ *.bak

realclean : clean
	$(BINDIR)\rm -zq *.exe *.res curlib.nm
