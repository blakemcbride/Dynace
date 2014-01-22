
# DMAKE makefile for DOS using WATCOM C 32

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = wcl386
.IF $(DEBUG)
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -d2 -j $(CFEXTRA)
.ELSE
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j -w4 $(CFEXTRA)
.END

.IF $(CPP)
CFLAGS += -cc++
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


CLASSES = thread.d semaphor.d pipe.d

OBJ = $(CLASSES:s/.d/.obj/) timer.obj

curlib.dw .LIBRARY : $(OBJ)
	wlib -b -c -q $(LIBDIR)\dynldw.lib @$(mktmp,tmp.lnk -+$(?:t"\n-+")\n)
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*

