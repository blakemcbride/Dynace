#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C/C++ 32 bit


TARGET = main


C_SRC = main.c


CLASS_SRC = class1.d


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -Zi $(CFEXTRA)
LFLAGS = /nologo /subsystem:console /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy $(CFEXTRA)
LFLAGS = /nologo /subsystem:console
.END


LIBS = $(LIBDIR)\dynlcm.lib 


OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : generics.1 generics.h generics.obj $(OBJS) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\ngenerics.obj\n$(LIBS2)\nwinmm.lib\n)


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1




clean:
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c
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

