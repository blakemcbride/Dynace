#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 32 bit


TARGET = server32


C_SRC = main.c


CLASS_SRC = server.d


RESOURCES = server.res

BASEDIR = d:\Dynace.401
ADSDIR  = d:\Dynace.ads

BINDIR = $(BASEDIR)\bin
LIBDIR = $(BASEDIR)\lib
INCDIR = $(BASEDIR)\include


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -D_WINDOWS -I$(ADSDIR)\ads-inc
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -Zi -D_WINDOWS
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS -I$(ADSDIR)\ads-inc
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS
LFLAGS = /nologo /subsystem:windows
.END



LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib $(ADSDIR)\lib\adscm.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 winmm ole32 oleaut32 uuid}.lib


OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.rc.res:
	rc $<

$(TARGET).exe : generics.1 generics.h generics.obj $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\ngenerics.obj\n\
		$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(OBJS) : resource.h

main.obj : ../classid.h


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -C -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c -C -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1




clean:
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c
	$(BINDIR)\rm -zq *.obj
#	$(BINDIR)\rm -zq *.exe
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

