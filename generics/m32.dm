

# DMAKE makefile for Microsoft Visual C 32 bit


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -nologo -Od -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
.ELSE
CFLAGS = -I$(INCDIR) -nologo -Oityb1 -Gs -Gy $(CFEXTRA)
.END

curlib.cm : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h
	$(CC) -c $(CFLAGS) generics.c
	lib /nologo /out:$(LIBDIR)\dynlcm.lib $(LIBDIR)\dynlcm.lib generics.obj
	$(BINDIR)\rm -zq *.obj generics.*
	echo Done >$@

clean:
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
