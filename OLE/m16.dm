#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 16 bit


C_SRC = makeguid.c

CLASS_SRC = ComClient.d ComServer.d ComInterface.d ComInstance.d OLEDispatch.d OLEClient.d



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include




.IF $(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA)
GFLAGS =  -AL -I$(INCDIR) -Ozax -nologo -DWINVER=0x0310 -D_WINDOWS
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Oit $(CFEXTRA)
GFLAGS =  -AL -I$(INCDIR) -Ozax -nologo -DWINVER=0x0310 -D_WINDOWS
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500
.END


LIBS = $(LIBDIR)\{dwdswm dynldm}.lib

MSLIBS = {oldnames llibcew libw oldnames commdlg ddeml odbc}.lib


OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.rc.res:
	rc -nologo -r $<

allok.wm .LIBRARY : $(INCDIR)\generics.h curlib.wm
	echo Done >$@


curlib.wm .LIBRARY : $(OBJS)
	lib /batch /noi /nol /page:64 @$(mktmp $(LIBDIR)\\dwdswm\n-+$(^:db:t" &\n-+")\nNUL\n\n)
	echo Done >$@
	$(BINDIR)\rm -zq *.obj $(LIBDIR)/dwdswm.bak


$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) $<

newgens generics.1 : 
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(CLASS_SRC:t"\n")\n)

makegens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -s @$(mktmp $(CLASS_SRC:t"\n")\n)

clean:
	$(BINDIR)\rm -zq *.obj
	$(BINDIR)\rm -zq *.exe
	$(BINDIR)\rm -zq *.ex1
	$(BINDIR)\rm -zq *.res
	$(BINDIR)\rm -zq *.aps
	$(BINDIR)\rm -zq *.pdb
	$(BINDIR)\rm -zq *.wsp
	$(BINDIR)\rm -zq *.ilk
	$(BINDIR)\rm -zq *.bsc
	$(BINDIR)\rm -zq *.vcp
	$(BINDIR)\rm -zq *.pch
	$(BINDIR)\rm -zq *.sbr
	$(BINDIR)\rm -zq *.vcw
	$(BINDIR)\rm -zq *.map *.ncb
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*


realclean: clean
	$(BINDIR)\rm -zq curlib.* allok.* {$(CLASS_SRC:b)}.c generics.c generics.h generics.1

