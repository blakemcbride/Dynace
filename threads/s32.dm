
# DMAKE makefile for Symantec C 32 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = sc
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 $(CFEXTRA)
.ELSE
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 $(CFEXTRA)
.ENDIF


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<

CLASSES = thread.d semaphor.d pipe.d

OBJ = $(CLASSES:s/.d/.obj/) nttimer.obj

curlib.cs .LIBRARY : $(OBJ)
	lib $(LIBDIR)\dynlcs.lib /n /noi @$(mktmp -+$(?:t" &\n-+");\n)
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h

clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
