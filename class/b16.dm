
# DMAKE makefile for Borland C 16 bit



.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = bcc
WARNINGS = -w-par -w-pia -w-pro -w-stu
OPT  = -O -Ot -Z
COPT = -Yo- -Y- -a2 -d
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
MM   = -ml
CFLAGS = $(INC) $(COPT) $(MM) $(OPT) $(WARNINGS) -N- $(CFEXTRA)


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<

.c.obj:
	@echo $(CFLAGS) >turboc.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq turboc.cfg


CLASSES = linklist.d link.d linkval.d linkobj.d shortint.d assoc.d \
	lookupky.d set.d dict.d string.d longint.d double.d \
	char.d setseq.d loseq.d strassoc.d strdict.d linkseq.d ushrtint.d \
	date.d pointer.d memalloc.d number.d objassoc.d \
	sequence.d stream.d file.d lowfile.d array.d charary.d \
	const.d shortary.d ushrtary.d intary.d longary.d floatary.d \
	dblary.d bitary.d objary.d pntrary.d numbary.d intassoc.d \
	intdict.d findfile.d btree.d btreenod.d memory.d socket.d \
	crc.d time.d datetime.d property.d


OBJ = $(CLASSES:s/.d/.obj/) 

allok.tc .LIBRARY :  $(INCDIR)\generics.h curlib.tc
	echo Done >$@

curlib.tc .LIBRARY : $(OBJ)
	+tlib $(LIBDIR)\dynldb /E /C /P32 @$(mktmp,tmp.lnk -+$(?:t" &\n-+")\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldb.bak
	echo Done >$@

$(INCDIR)\generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

dict.obj set.obj setseq.obj strdict.obj intdict.obj : set1.h

string.obj array.obj : memalloc.h

array.obj : array2.h

array.obj charary.obj  shortary.obj ushrtary.obj longary.obj floatary.obj \
	dblary.obj bitary.obj objary.obj pntrary.obj : array1.h

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.* allok.*

