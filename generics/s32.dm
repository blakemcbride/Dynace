

# DMAKE makefile for Symantec C 32 bit


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CC = sc

.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7 -g -o-all
.ELSE
CFLAGS = -I$(INCDIR) -mn -a4 -J -w2 -w7
.END

curlib.cs : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h
	$(CC) -c $(CFLAGS) generics.c
	lib $(LIBDIR)\dynlcs.lib /n /noi -+generics.obj;
	$(BINDIR)\rm -zq *.obj generics.*
	echo Done >$@

clean:
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
