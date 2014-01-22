
#  Makefile designed for use by the DMAKE.EXE utility and
#  Borland C 32 bit



.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = bcc32
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT = -Od -v
LFLAGS = /c /x /v
.ELSE
OPT  = -O -Z -OtglbWimpv -k- # -Oe doesn't work
LFLAGS = /c /x
.END
COPT = -a4 -d
GOPT  = -O -Z -Ot -k-
CFLAGS = $(COPT) $(OPT) $(WARNINGS) -N
GFLAGS = $(COPT) $(GOPT) $(WARNINGS) -w-rvl

INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include

LIBS = $(LIBDIR)\dynlcb.lib

BLIBS = $(BORLAND_HOME)\lib\{cw32 import32}.lib


BSTARTUP = $(BORLAND_HOME)\lib\c0x32.obj $(BORLAND_HOME)\lib\32bit\wildargs.obj


TARGET = dpp.exe

CLASSES = proto.d istream.d ostream.d token.d arglist.d ostream2.d

CFILES = dpp.c 

OBJS = {$(CFILES:b)}.obj {$(CLASSES:b)}.obj


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
BSTARTUP2 := $(BSTARTUP:s/\/\\/)
BLIBS2    := $(BLIBS:s/\/\\/)
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.c.obj:
	@echo $(CFLAGS) >bcc32.cfg
	@echo $(INC) >>bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg


$(TARGET) : generics.1 generics.h generics.obj $(OBJS) $(LIBS)
	tlink32 @$(mktmp,lnk.rsp  \
		$(LFLAGS) $(BSTARTUP2) +  \n\
		$(OBJS2) generics.obj\n\
		$@ \n\
		\n\
		$(LIBS2) +\n\
		$(BLIBS2) \n)
		
install : $(TARGET)
	$(BINDIR)\cp $(TARGET) $(BINDIR)

generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c

generics.obj : generics.c
	@echo $(GFLAGS) >bcc32.cfg
	@echo $(INC) >>bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg

generics.1 newgens: 
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASSES:t"\n")\n)
	@echo Done >generics.1

clean realclean:
	$(BINDIR)\rm -zq *.obj *.o *.pdb *.err *.exe *.vcp
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*


