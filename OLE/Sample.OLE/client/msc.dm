#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 32 bit


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
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -Zi -D_WINDOWS -Fd$(LIBDIR)\dynm32.pdb
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -Zi -D_WINDOWS
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -I$(ADSINC) -WX -O2 -D_WINDOWS
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS
LFLAGS = /nologo /subsystem:windows
.END



LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib $(ADSLIB)\adscm.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 winmm ole32 oleaut32 uuid}.lib


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
	rc $<

$(TARGET).exe : $(INCDIR)\generics.h generics.obj $(OBJS) $(RESOURCES) $(LIBS)
#	lib /nologo /out:$(LIBDIR)\dwdsnm.lib $(LIBDIR)\dwdsnm.lib @$(mktmp $(LOBJS:t"\n")\n)
	link @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\ngenerics.obj\n\
		$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(OBJS) : resource.h

client.obj : ../classid.h


#$(INCDIR)\generics.h : $(CLASS_SRC)
#	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n")\n)
#
#generics.c : $(INCDIR)\generics.h
#	$(BINDIR)\dpp -g $(INCDIR)\generics.h -c -C # -Isc windows.h sql.h sqlext.h

generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h generics.h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp -g generics.h -c -C # -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(CLASS_SRC:t"\n")\n)

newgens:
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(CLASS_SRC:t"\n")\n)

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

