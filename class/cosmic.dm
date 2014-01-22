
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
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


CLASSES = linklist.d link.d linkval.d linkobj.d shortint.d assoc.d \
	lookupky.d set.d dict.d string.d longint.d double.d \
	char.d setseq.d loseq.d strassoc.d strdict.d linkseq.d ushrtint.d \
	date.d pointer.d memalloc.d number.d objassoc.d \
	sequence.d stream.d const.d intassoc.d \
	intdict.d btree.d btreenod.d memory.d


OBJ = $(CLASSES:s/.d/.o/) 

allok.cm .LIBRARY :  $(INCDIR)\generics.h curlib.cm
	echo Done >$@

curlib.cm .LIBRARY : $(OBJ)
	clib -r $(LIBDIR)\dynace.h11 linklist.o link.o linkval.o linkobj.o shortint.o assoc.o
	clib -r $(LIBDIR)\dynace.h11 lookupky.o set.o dict.o string.o longint.o double.o
	clib -r $(LIBDIR)\dynace.h11 char.o setseq.o loseq.o strassoc.o strdict.o linkseq.o ushrtint.o
	clib -r $(LIBDIR)\dynace.h11 date.o pointer.o memalloc.o number.o objassoc.o
	clib -r $(LIBDIR)\dynace.h11 sequence.o stream.o const.o intassoc.o
	clib -r $(LIBDIR)\dynace.h11 intdict.o btree.o btreenod.o memory.o
#	$(BINDIR)\rm -zq *.o
	echo Done >$@

$(INCDIR)\generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

dict.o set.o setseq.o strdict.o intdict.o : set1.h

string.o array.o : memalloc.h

array.o : array2.h

array.o charary.o  shortary.o ushrtary.o longary.o floatary.o \
	dblary.o bitary.o objary.o pntrary.o: array1.h

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h

clean :
	$(BINDIR)\rm -zq *.o generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.* allok.*


