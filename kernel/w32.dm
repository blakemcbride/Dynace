
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
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<


CLASSES = object.d behavior.d metaclas.d class.d method.d genfun.d dynace.d


OBJ = $(CLASSES:s/.d/.obj/) kernel.obj malloc.obj

#  You will need to substitute jumpto.w with jumpto.w90 if using
#  WATCOM C386 9.0

#  jumpto.w works with WATCOM C/C++32 version 9.5


curlib.dw .LIBRARY : $(OBJ)  jumpto.obj
	wlib -b -c -q $(LIBDIR)\dynldw.lib @$(mktmp,tmp.lnk -+$(?:t"\n-+")\n)
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

jumpto.obj : jumpto\jumpto.w
	$(CC) $(CFLAGS) -c -s $<

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

