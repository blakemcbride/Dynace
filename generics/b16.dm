
# DMAKE makefile for Borland C 16 bit



.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = bcc
WARNINGS = -w-par -w-pia -w-pro -w-stu
OPT  = -O -Ot -Z
COPT = -Yo- -Y- -a -d
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
MM   = -ml
GFLAGS = $(INC) $(COPT) $(MM) $(OPT) $(WARNINGS) -N- -w-rvl



curlib.db : $(INCDIR)\generics.h
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -c -h
	@echo $(GFLAGS) >turboc.cfg
	$(CC) -c generics.c
	@$(BINDIR)\rm -zq turboc.cfg
	+tlib $(LIBDIR)\dynldb /E /C /P32 -+generics.obj
	$(BINDIR)\rm -zq *.obj generics.* $(LIBDIR)\dynldb.bak
	echo Done >$@


clean:
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
