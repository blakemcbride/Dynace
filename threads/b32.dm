
# DMAKE makefile for Borland C 32 bit



.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include




CC = bcc32
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT = -Od -v
.ELSE
OPT  = -O -Z -OtglbWimpv -k- # -Oe kills the threader
.END
COPT = -a4 -d -N-
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
CFLAGS = $(INC) $(COPT) $(OPT) $(WARNINGS) $(CFEXTRA)


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<

.c.obj:
	@echo $(CFLAGS) >bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg



CLASSES = thread.d semaphor.d pipe.d

OBJ = $(CLASSES:s/.d/.obj/) nttimer.obj

curlib.cb .LIBRARY : $(OBJ)
	+tlib $(LIBDIR)\dynlcb.lib /E /C /P32 @$(mktmp,tmp.lnk -+$(?:t" &\n-+")\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynlcb.bak
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
