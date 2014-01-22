
# DMAKE makefile for Microsoft Visual C 16 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = cl
.IF $(DEBUG)
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or -D_WINDOWS
.ELSE
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Oit -Gs -Gf -D_WINDOWS
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


CLASSES = 

OBJ = $(CLASSES:s/.d/.obj/) regutil.obj

curlib.dm .LIBRARY : $(OBJ)
	lib /batch /nol /noi $(LIBDIR)\dynldm.lib @$(mktmp,tmp.lnk -+$(?:t" &\n-+");\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldm.bak
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d

regutl16.exe : regutil.c
#	cl -nologo -AL -DMAIN $< /link shell.lib /stack:4096
	cl -nologo -AL -DMAIN $< /Fe$@ /link /stack:4096
	$(BINDIR)\rm -zq *.obj 

clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
