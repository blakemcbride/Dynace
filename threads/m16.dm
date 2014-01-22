
# DMAKE makefile for Microsoft Visual C 16 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = cl
.IF $(DEBUG)
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA)
.ELSE
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Oit -Gs -Gf $(CFEXTRA)
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


CLASSES = thread.d semaphor.d pipe.d

OBJ = $(CLASSES:s/.d/.obj/) timer.obj

curlib.dm .LIBRARY : $(OBJ)
	lib /batch /nol /noi /page:32 $(LIBDIR)\dynldm.lib @$(mktmp,tmp.lnk -+$(?:t" &\n-+");\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldm.bak
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
