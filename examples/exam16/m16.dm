#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C/C++ 16 bit


TARGET = main


C_SRC = main.c


CLASS_SRC = class1.d


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -AL -Gf -WX -Zi -Od -Or
LFLAGS = /NOI /NOE /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -Gf -WX -Oit
LFLAGS = /NOI /NOE /PACKC:65500
.END
GFLAGS = -I$(INCDIR) -AL -nologo -Ozax 

LIBS = $(LIBDIR)\dynldm.lib

OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : generics.1 generics.h generics.obj gens1.obj $(OBJS) $(LIBS)
	link /BATCH @$(mktmp $(OBJS2:t" +\n") generics.obj gens1.obj\n$*.exe\nnul $(LFLAGS)\n$(LIBS2);\n)


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c -M800

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

gens1.obj : gens1.c
	$(CC) -c $(GFLAGS) gens1.c

generics.1 : 
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1



clean:
	rm -zq {$(CLASS_SRC:b)}.c
	rm -zq *.obj
	rm -zq *.exe
	rm -zq *.ex1
	rm -zq *.res
	rm -zq *.aps
	rm -zq *.pdb
	rm -zq *.wsp
	rm -zq *.ilk
	rm -zq *.bsc
	rm -zq *.vcp
	rm -zq *.pch
	rm -zq *.sbr
	rm -zq *.vcw
	rm -zq *.map
	rm -zq generics.* gens1.*
	rm -zqr WinDebug
	rm -zqr WinRel

