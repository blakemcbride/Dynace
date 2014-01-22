
# DMAKE makefile for Borland C 16 bit



.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = bcc
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT  = -Od -v
.ELSE
OPT  = -O -Ot -Z
.END
COPT = -Yo- -Y- -a2 -d
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
MM   = -ml
CFLAGS = $(INC) $(COPT) $(MM) $(OPT) $(WARNINGS) -N- -c $(CFEXTRA)
GFLAGS = $(INC) $(COPT) $(MM) $(OPT) $(WARNINGS) -N- -c



.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<

.c.obj:
	@echo $(CFLAGS) >turboc.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq turboc.cfg



CLASSES = thread.d semaphor.d pipe.d

OBJ = $(CLASSES:s/.d/.obj/) timer.obj


curlib.db .LIBRARY : $(OBJ)
	+tlib $(LIBDIR)\dynldb /E /C /P32 @$(mktmp,tmp.lnk -+$(?:t" &\n-+")\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldb.bak
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*


