
#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C 32 bit


C_SRC = main.c flash.c jsfl.c



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -Od -D_WINDOWS -MT
LFLAGS = /nologo /subsystem:windows /debug
.IF $(INTEGRA)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -Od -D_WINDOWS -DINTEGRA -MT
.END
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS -MT
LFLAGS = /nologo /subsystem:windows
.IF $(INTEGRA)
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS -DINTEGRA -MT
.END
.END


LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 comctl32 advapi32 shell32 winmm ole32 mpr msimg32 Wininet}.lib

OBJS = {$(C_SRC:b)}.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


RESOURCES = main.res

.rc.res:
	rc /nologo $<

cld.exe : $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2)\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(OBJS) : resource.h

$(OBJS) : ../windows/ctlsave.h


realclean clean:
	$(BINDIR)\rm -zq *.obj
	$(BINDIR)\rm -zq *.exe
	$(BINDIR)\rm -zq *.ex1
	$(BINDIR)\rm -zq *.res
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

