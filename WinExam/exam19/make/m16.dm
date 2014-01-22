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
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Zi -Od -Or
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Oit
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500
.END
GFLAGS = -I$(INCDIR) -AL -nologo -Ozax -D_WINDOWS -DWINVER=0x0310


LIBS = $(LIBDIR)\{dwdswm dynldm}.lib

MSLIBS = oldnames llibcew libw oldnames commdlg ddeml odbc ole2


OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : $(TARGET).ex1 $(TARGET).res
	cp $*.ex1 $*.exe
	rc -nologo -31 -k $(TARGET).res $*.exe

$(TARGET).ex1 : generics.1 generics.h generics.obj gens1.obj $(OBJS) $(LIBS)
	link /BATCH @$(mktmp $(OBJS2:t" +\n") generics.obj gens1.obj\n$*.ex1\nnul \
			$(LFLAGS)\n$(LIBS2) $(MSLIBS)\n$(TARGET).def\n)


$(OBJS) : resource.h

$(TARGET).res : $(TARGET).rc
	rc -nologo -r $<


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp -g -c -M700

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
	$(BINDIR)\rm -zq generics.* gens1.*
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel

