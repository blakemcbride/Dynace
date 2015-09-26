
# DMAKE makefile for Microsoft Visual C 32 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END


CC = cl
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -WX -nologo -W3 -Od -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
.ELSE
CFLAGS = -I$(INCDIR) -WX -nologo -W3 -O2 $(CFEXTRA)
.ENDIF


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<

CLASSES = 

OBJ = $(CLASSES:s/.d/.obj/) regutil.obj regutil2.obj

curlib.cm .LIBRARY : $(OBJ)
	lib /nologo /out:$(LIBDIR)\dynlcm.lib $(LIBDIR)\dynlcm.lib @$(mktmp $(?:t"\n")\n)
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d

regutl32.exe : regutil.c
	cl -nologo -Zi -Od -DMAIN $< advapi32.lib user32.lib -Fe$@
	$(BINDIR)\rm -zq *.obj 

clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
