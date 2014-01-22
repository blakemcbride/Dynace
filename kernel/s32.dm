
# DMAKE makefile for Symantec C++ 32 bit


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = sc
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 -g -o-all $(CFEXTRA)
.ELSE
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 $(CFEXTRA)
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<


CLASSES = object.d behavior.d metaclas.d class.d method.d genfun.d dynace.d


OBJ = $(CLASSES:s/.d/.obj/) kernel.obj malloc.obj

curlib.cs .LIBRARY : $(OBJ)
.IF	$(NEW)
	lib $(LIBDIR)\dynlcs.lib /c /n /noi @$(mktmp +$(?:t" &\n+");\n)
.ELSE
	lib $(LIBDIR)\dynlcs.lib /n /noi @$(mktmp -+$(?:t" &\n-+");\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

kernel.obj : kernels.h object.iv behavior.iv method.iv genfun.iv

object.obj : object.iv

behavior.obj : kernels.h behavior.iv

genfun.obj : kernels.h genfun.iv

method.obj : method.iv

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -ni -p *.d -h $(INCDIR)\generics.h

reallynewgens:
	$(BINDIR)\dpp $(STRAT) -C -ni -p *.d -h $(INCDIR)\generics.h

clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
