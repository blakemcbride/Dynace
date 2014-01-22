
#  The makefile is designed for the DMAKE utility and Symantec C 32 bit



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = sc
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 -g -o-all
.ELSE
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7
.END

GFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 -g -o-all


TARGET = dpp.exe

CLASSES = proto.d istream.d ostream.d token.d arglist.d ostream2.d

CFILES = dpp.c 

OBJS = {$(CFILES:b)}.obj {$(CLASSES:b)}.obj

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -p $<

LIBS = $(LIBDIR)\dynlcs.lib 

# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)

$(TARGET) : generics.1 generics.h generics.obj $(OBJS) 
	link /noignorecase /co @$(mktmp $(OBJS2) generics.obj\n$@\n\n$(LIBS2) winmm.lib;\n)


install : $(TARGET)
	$(BINDIR)\cp $(TARGET) $(BINDIR)

generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c 

generics.obj : generics.c 
	$(CC) $(GFLAGS) -c generics.c

generics.1 newgens:
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASSES:t"\n")\n)
	@echo Done >generics.1


clean realclean:
	$(BINDIR)\rm -zq *.obj *.o *.pdb *.err *.exe
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*



