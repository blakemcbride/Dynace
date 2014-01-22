#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C/C++ 32 bit


TARGET = SchemeResource


C_SRC = main.c

DYNACE_PATH=..\..
BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

ADSDIR = ..\..\..\adsw.Subversion
ADSINC = $(ADSDIR)\ads-inc
ADSLIB = $(ADSDIR)\lib

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -Zi -Gf -W3 -MT
LFLAGS = /nologo /subsystem:console /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -O2 -Gf -W3 -MT
LFLAGS = /nologo /subsystem:console
.END

LIBS = $(ADSLIB)\{console32 adscm}.lib $(LIBDIR)\dynlcm.lib

OBJS = {$(C_SRC:b)}.obj

CC = cl


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : $(OBJS) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\n$(LIBS2)\nwinmm.lib\n)


realclean: clean
	$(BINDIR)\rm -zq *.exe


clean:
	$(BINDIR)\rm -zq *.obj *~ *.bak
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

