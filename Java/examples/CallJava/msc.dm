#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C 32 bit

JAVA=C:\j2sdk1.4.2_03
OTHERLIBS = c:\Blake\Integra\Dynace.Z


TARGET = main


C_SRC = main.c LoadDynClasses.c


DYNACE_PATH = ..\..\..

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -D_WINDOWS $(CFEXTRA)
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS $(CFEXTRA)
LFLAGS = /nologo /subsystem:windows
.END


LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib $(OTHERLIBS)\lib\{ssleay32 libeay32 adscm}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 comctl32 advapi32 shell32 odbc32 winmm ole32 wsock32}.lib

OBJS = {$(C_SRC:b)}.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


RESOURCES = $(TARGET).res

.rc.res:
	rc $<

all: Test.class $(TARGET).exe

$(TARGET).exe : $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS) \n\
	        $(OBJS2)\n$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


$(OBJS) : resource.h

LoadDynClasses.c : $(INCDIR)\generics.h excludeLoadDynClass.sed excludeDynaceClassRef.sed
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -j Dynace.generics
	sed -f excludeLoadDynClass.sed $@ >tmp.sed
	mv tmp.sed $@
	sed -f excludeDynaceClassRef.sed Dynace.java >tmp.sed
	mv tmp.sed Dynace.java


Test.class : Test.java
	javac $<



clean:
	$(BINDIR)\rm -zq *.obj *~ *.bak *.class
	$(BINDIR)\rm -zq LoadDynClasses.c Dynace.java DynaceObject.java
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
	$(BINDIR)\rm -zq *.dsw *.opt *.plg
	$(BINDIR)\rm -zq *.dsp

