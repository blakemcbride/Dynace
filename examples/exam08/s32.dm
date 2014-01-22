#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Symantec C/C++ 32 bit


TARGET = main


C_SRC = main.c


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

.IF	$(DEBUG)
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 -g -o-all
LFLAGS = /noignorecase /co
.ELSE
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7
LFLAGS = /noignorecase
.END


LIBS = $(LIBDIR)\dynlcs.lib 


OBJS = {$(C_SRC:b)}.obj

CC = sc


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : $(OBJS) $(LIBS)
	link @$(mktmp $(LFLAGS) $(OBJS2)\n$@\n\n$(LIBS2) winmm.lib;\n)




clean:
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

