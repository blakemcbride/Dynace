#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 16 bit


TARGET = client


C_SRC = main.c


CLASS_SRC = client.d


RESOURCES = main.res


BASEDIR = d:\Dynace.401
ADSDIR  = d:\Dynace.ads

BINDIR = $(BASEDIR)\bin
LIBDIR = $(BASEDIR)\lib
INCDIR = $(BASEDIR)\include

ADSINC = $(ADSDIR)\ads-inc
ADSLIB = $(ADSDIR)\lib

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA)
GFLAGS = -AL -I$(INCDIR) -Ozax -nologo -D_WINDOWS
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Oit $(CFEXTRA)
GFLAGS = -AL -I$(INCDIR) -Ozax -nologo -D_WINDOWS
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500
.END



LIBS = $(LIBDIR)\{dwdswm dynldm}.lib $(ADSDIR)\lib\adsdm.lib

MSLIBS = {oldnames llibcew libw oldnames commdlg ddeml ole2 ole2disp}.lib


LOBJS = {$(CLASS_SRC:b)}.obj
OBJS = {$(C_SRC:b)}.obj $(LOBJS)

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.rc.res:
	rc -nologo -r $<

#$(TARGET).exe : $(INCDIR)\generics.h generics.obj $(OBJS) $(RESOURCES) $(LIBS)
#	lib /batch /noi /nol /page:32 @$(mktmp $(LIBDIR)\\dwdswm\n-+$(LOBJS:db:t" &\n-+")\nNUL\n\n)
#	link @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\ngenerics.obj\n\
#		$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(TARGET).exe : $(TARGET).ex1 $(RESOURCES)
	cp $*.ex1 $*.exe
	rc -nologo -31 -k $(RESOURCES) $*.exe

$(TARGET).ex1 : generics.1 generics.h generics.obj $(OBJS) $(LIBS)
#	lib /batch /noi /nol /page:32 @$(mktmp $(LIBDIR)\\dwdswm\n-+$(LOBJS:db:t" &\n-+")\nNUL\n\n)
	link /BATCH @$(mktmp $(OBJS2:t" +\n") generics.obj\n$*.ex1\nnul \
			$(LFLAGS)\n$(LIBS2) $(MSLIBS)\n$(TARGET).def\n)

$(OBJS) : resource.h

client.obj : ../classid.h


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -C -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp -g -c -C # -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h  -p @$(mktmp $(CLASS_SRC:t"\n")\n)

newgens:
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h  -p @$(mktmp $(CLASS_SRC:t"\n")\n)

clean:
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c
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
	$(BINDIR)\rm -zq *.map
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel
	$(BINDIR)\rm -zq *.idb *.mdp *.ncb

