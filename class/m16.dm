
# DMAKE makefile for Microsoft Visual C 16 bit

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

MY_SSL = 1

.IF $(MY_SSL)
MY_SSL_CFLAGS = -DMY_SSL -DWIN16
.ELSE
MY_SSL_CFLAGS = 
.END


CC = cl
.IF $(DEBUG)
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Zi $(MY_SSL_CFLAGS) -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA) # -Zr
.ELSE
#CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Oit -Gf $(MY_SSL_CFLAGS) $(CFEXTRA)
CFLAGS = -I. -I$(INCDIR) -AL -nologo -WX -Oit -Gf $(MY_SSL_CFLAGS) $(CFEXTRA)
.END



.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


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


allok.dm .LIBRARY :  $(INCDIR)\generics.h curlib.dm
	echo Done >$@

curlib.dm .LIBRARY : $(OBJ)
	lib /batch /nol /noi /page:32 $(LIBDIR)\dynldm.lib @$(mktmp,tmp.lnk -+$(?:t" &\n-+");\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynldm.bak
	echo Done >$@

$(INCDIR)\generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

dict.obj set.obj setseq.obj strdict.obj intdict.obj : set1.h

string.obj array.obj : memalloc.h

array.obj : array2.h

array.obj charary.obj  shortary.obj ushrtary.obj longary.obj floatary.obj \
	dblary.obj bitary.obj objary.obj pntrary.obj: array1.h

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.* allok.*



