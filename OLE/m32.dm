#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 32 bit



C_SRC = makeguid.c

CLASS_SRC = ComClient.d ComServer.d ComInterface.d ComInstance.d OLEDispatch.d OLEClient.d



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -D_WINDOWS -Od -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS -Zi -Fd$(LIBDIR)\dynm32.pdb
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS
LFLAGS = /nologo /subsystem:windows
.END



LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 odbc32 winmm}.lib


OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -C -g $(INCDIR)\generics.h -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.rc.res:
	rc $<

allok.nm .LIBRARY : $(INCDIR)\generics.h curlib.nm
	echo Done >$@


curlib.nm .LIBRARY : $(OBJS)
.IF  $(NEW)
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.ELSE
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib $(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

main.obj : resource.h


$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))


generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) $<

newgens : 
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

