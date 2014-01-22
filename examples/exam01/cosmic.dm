#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Cosmic MC68HC11 cross compiler version 4.1C



TARGET = main


C_SRC = main.c


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


DEBUG=1

STRAT=-S2

CC = cx6811

.IF $(DEBUG)
CFLAGS = +debug -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
.ELSE
CFLAGS = -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
.END


LIBS = $(LIBDIR)\dynace.h11


OBJS = {$(C_SRC:b)}.o

.c.o:
	$(CC) $(CFLAGS) $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).h11 : $(OBJS) $(LIBS)
	clnk -o $@ -m$*.map $*.lkf


clean:
	$(BINDIR)\rm -zq *.o
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

