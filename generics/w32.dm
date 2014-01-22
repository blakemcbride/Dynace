

# DMAKE makefile for NT using WATCOM C 32


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CC = wcl386

CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j -s -w0

curlib.dw : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h
	$(CC) -c $(CFLAGS) generics.c
	wlib -b -c -q $(LIBDIR)\dynldw.lib -+generics.obj
	$(BINDIR)\rm -zq *.obj generics.*
	echo Done >$@


clean:
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
