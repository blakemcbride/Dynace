#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 32 bit


TARGET = main


C_SRC = main.c


CLASS_SRC = iblake.d


RESOURCES = $(TARGET).res


BINDIR = ..\..\bin
LIBDIR = ..\..\lib
INCDIR = ..\..\include


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -D_WINDOWS
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -Zi -D_WINDOWS
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS
LFLAGS = /nologo /subsystem:windows
.END



LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 winmm ole32 uuid}.lib


OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj ..\idl\iblake_i.obj

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


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c -C -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -g $(INCDIR)\odbcgens.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp -g $(INCDIR)\odbcgens.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1




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

