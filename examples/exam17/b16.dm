#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Borland C 16 bit


TARGET = main


C_SRC = main.c


CLASS_SRC = class1.d class2.d



.IMPORT .IGNORE : BORLAND_HOME


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


CC = bcc
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT  = -Od -v
LFLAGS = /c /x /v
.ELSE
OPT  = -O -Ot -Z
LFLAGS = /c /x
.END
COPT = -a2 -d 
MM   = -ml
CFLAGS = $(COPT) $(MM) $(OPT) $(WARNINGS) -N
GFLAGS = $(COPT) $(MM) $(OPT) $(WARNINGS) -N- -c -w-rvl

INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include

LIBS = $(LIBDIR)\dynldb.lib

BLIBS = $(BORLAND_HOME)\lib\{emu mathl cl}.lib

BSTARTUP = $(BORLAND_HOME)\lib\c0l.obj


OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj


.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
BSTARTUP2 := $(BSTARTUP:s/\/\\/)
BLIBS2    := $(BLIBS:s/\/\\/)
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.c.obj:
	@echo $(CFLAGS) >turboc.cfg
	@echo $(INC) >>turboc.cfg
	$(CC) -c $<
	@rm -zq turboc.cfg


$(TARGET).exe : generics.1 generics.h generics.obj $(OBJS) $(LIBS)
	tlink @$(mktmp,lnk.rsp  \
		$(LFLAGS) $(BSTARTUP2) +  \n\
		$(OBJS2) generics.obj\n\
		$@ \n\
		\n\
		$(LIBS2) +\n\
		$(BLIBS2) \n)


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c

generics.obj : generics.c
	@echo $(GFLAGS) >turboc.cfg
	@echo $(INC) >>turboc.cfg
	$(CC) -c $<
	@rm -zq turboc.cfg

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
	rm -zq generics.*
	rm -zqr WinDebug
	rm -zqr WinRel
	rm -zq *.rws
	rm -zq *.~re
	rm -zq *.csm
	rm -zq *.dsw
	rm -zq *.~de
