

# DMAKE makefile for Cosmic MC68HC11 cross compiler version 4.1C


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

DEBUG=1

STRAT=-S2

CC = cx6811

.IF $(DEBUG)
CFLAGS = +debug -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
.ELSE
CFLAGS = -i$(INCDIR) -d__COSMIC__ $(CFEXTRA)
.END

curlib.cm : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h
	$(CC) $(CFLAGS) generics.c
	clib -r $(LIBDIR)\dynace.h11 generics.o
#	$(BINDIR)\rm -zq *.o generics.*
	echo Done >$@

clean:
	$(BINDIR)\rm -zq *.o generics.* *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
