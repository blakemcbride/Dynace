
# DMAKE makefile for Microsoft Visual C++ 16 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = cl
.IF $(DEBUG)
CFLAGS = -I. -I$(INCDIR) -AL -WX -nologo -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA) # -Zr
.ELSE
CFLAGS = -I. -I$(INCDIR) -AL -WX -nologo -Oti -Gs -Gf $(CFEXTRA)
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<



CLASSES = object.d behavior.d metaclas.d class.d method.d genfun.d dynace.d


OBJ = $(CLASSES:s/.d/.obj/) kernel.obj

curlib.dm .LIBRARY : $(OBJ) jumpto.obj
	lib /batch /nol /noi /page:32 $(LIBDIR)\dynldm.lib @$(mktmp,tmp.lnk -+$(?:t" &\n-+");\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldm.bak
	echo Done >$@

jumpto.obj : jumpto\jumpto.msc
	cp jumpto\m16.asm jumpto.obj
#	tasm /ml $<

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

