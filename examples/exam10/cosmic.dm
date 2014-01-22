#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Cosmic MC68HC11 cross compiler version 4.1C


TARGET = main


C_SRC = main.c


CLASS_SRC = class1.d


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

DEBUG=1

STRAT=-S2

CC = cx6811

.IF	$(DEBUG)
CFLAGS = +debug -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
GFLAGS = $(CFLAGS)
.ELSE
CFLAGS = -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
GFLAGS = $(CFLAGS)
.END


LIBS = $(LIBDIR)\dynace.h11


OBJS = {$(C_SRC:b)}.o {$(CLASS_SRC:b)}.o

.c.o:
	$(CC) $(CFLAGS) $<

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).h11 : generics.1 generics.h generics.o $(OBJS) $(LIBS)
	clnk -o $@ -m$*.map $*.lkf


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -g -c

generics.o : generics.c
	$(CC) $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp $(STRAT) -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp $(STRAT) -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1




clean:
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c
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

