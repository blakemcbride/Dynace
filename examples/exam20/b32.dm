#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Borland C 32 bit

TARGET = main


C_SRC = main.c



.IMPORT .IGNORE : BORLAND_HOME


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


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
CFLAGS = $(COPT) $(OPT) $(WARNINGS) -N

INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include

LIBS = $(LIBDIR)\dynlcb.lib

BLIBS = $(BORLAND_HOME)\lib\{cw32 import32}.lib



BSTARTUP = $(BORLAND_HOME)\lib\c0x32.obj


OBJS = {$(C_SRC:b)}.obj


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
	@rm -zq bcc32.cfg


$(TARGET).exe : $(OBJS) $(LIBS)
	tlink32 @$(mktmp,lnk.rsp  \
		$(LFLAGS) $(BSTARTUP2) +  \n\
		$(OBJS2) \n\
		$@ \n\
		\n\
		$(LIBS2) +\n\
		$(BLIBS2) \n)



clean:
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
	rm -zq generics.*
	rm -zqr WinDebug
	rm -zqr WinRel
	rm -zq *.rws
	rm -zq *.~re
	rm -zq *.csm
	rm -zq *.dsw
	rm -zq *.~de
	rm -zq *.cfg
	rm -zq *.map
