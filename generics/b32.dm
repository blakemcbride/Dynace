

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
OPT  = -O -Z -OtglbWeimpv -k-
.END
GOPT  = -O -Z -Ot -k-
COPT = -a4 -d -N-
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
GFLAGS = $(INC) $(COPT) $(GOPT) $(WARNINGS) -w-rvl


curlib.cb : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h
	@echo $(GFLAGS) >bcc32.cfg
	$(CC) -c generics.c
	@$(BINDIR)\rm -zq bcc32.cfg
	+tlib $(LIBDIR)\dynlcb.lib /E /C /P32 -+generics.obj
	$(BINDIR)\rm -zq *.obj generics.* $(LIBDIR)\dynlcb.bak
	echo Done >$@


clean:
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
