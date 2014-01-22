
#  Makefile designed for use by the DMAKE.EXE utility and
#  Symantec C 32 bit


TARGET = main


C_SRC = main.c

DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


CC = sc

.IF	$(DEBUG)
CFLAGS = -I$(INCDIR) -D_WINDOWS -mn -a4 -J -w2 -w7 -g -o-all
LFLAGS = /noignorecase /co
.ELSE
CFLAGS = -I$(INCDIR) -D_WINDOWS -mn -a4 -J -w2 -w7
LFLAGS = /noignorecase
.END


LIBS = $(LIBDIR)\{dwdsns dynlcs}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 winmm}.lib

OBJS = {$(C_SRC:b)}.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


RESOURCES = $(TARGET).res

.rc.res:
	rcc -32 $<

$(TARGET).exe : $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp $(LFLAGS) \n\
	        $(OBJS2)\n$@\n\n$(LIBS2) $(MSLIBS)\n\n$(RESOURCES)\n)


$(OBJS) : resource.h




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
#  Symantec generated files
	$(BINDIR)\rm -zqr s32.bro s32.bpt s32.dpd s32.lnk s32.mak s32.def
	$(BINDIR)\rm -zqr main.rdb

