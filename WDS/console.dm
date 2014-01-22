
#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C 32 bit


TARGET = cwds


C_SRC = console.c



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include
ADSDIR = d:\dynace.ads

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -Gf -MT
LFLAGS = /nologo /subsystem:console /nodefaultlib:libc.lib /debug
#LFLAGS = /nologo /subsystem:console /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -Gf -MT
LFLAGS = /nologo /subsystem:console /nodefaultlib:libc.lib
#LFLAGS = /nologo /subsystem:console
.END


LIBS = $(LIBDIR)\{dwdsnm dynlcm mzscheme}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 comctl32 advapi32 shell32 odbc32 winmm ole32 wsock32}.lib
#MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 odbc32 winmm ole32}.lib

OBJS = {$(C_SRC:b)}.obj scminter.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


RESOURCES = main.res

.rc.res:
	rc $<

$(TARGET).exe : $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2)\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(OBJS) : resource.h


scminter.c : $(INCDIR)\generics.h exclude.sed
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -L2
	sed -f exclude.sed $@ >tmp.sed
	mv tmp.sed $@
	

clean:
	$(BINDIR)\rm -zq scminter.c
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

