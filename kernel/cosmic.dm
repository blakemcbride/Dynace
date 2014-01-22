
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

.c.o:
	$(CC) $(CFLAGS) $<

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<


CLASSES = object.d behavior.d metaclas.d class.d method.d genfun.d dynace.d


OBJ = $(CLASSES:s/.d/.o/) kernel.o

curlib.cm .LIBRARY : $(OBJ) # jumpto.o
.IF	$(NEW)
	clib -c $(LIBDIR)\dynace.h11 $(OBJ)
.ELSE
	clib -r $(LIBDIR)\dynace.h11 $(OBJ)
.END
#	$(BINDIR)\rm -zq *.o
	echo Done >$@

jumpto.o : jumpto\jumpto.nt3
	$(CC) $(CFLAGS) -c -Tc$<

kernel.o : kernels.h object.iv behavior.iv method.iv genfun.iv

object.o : object.iv

behavior.o : kernels.h behavior.iv

genfun.o : kernels.h genfun.iv

method.o : method.iv

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -ni -p *.d -h $(INCDIR)\generics.h

reallynewgens:
	$(BINDIR)\dpp $(STRAT) -C -ni -p *.d -h $(INCDIR)\generics.h

clean :
	$(BINDIR)\rm -zq *.o generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
