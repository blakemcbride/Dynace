#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C/C++ 32 bit



CLASS_SRC = Java.d JavaClass.d JavaObject.d JavaCallback.d

#  You must use the -E dmake option to make use of environment variables
JAVAHOME = $(JAVA_HOME)   #  \j2sdk1.4.1_01


DYNACE_PATH = ..\..
JAVAINC = $(JAVAHOME)\include
JAVAINC2 = $(JAVAHOME)\include\win32

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -Zi -Od -D_WINDOWS -W3 -I$(JAVAINC) -I$(JAVAINC2) -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS -I$(JAVAINC) -I$(JAVAINC2) $(CFEXTRA)
.END



OBJ = $(CLASS_SRC:s/.d/.obj/)

#OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -p $<


allok.cm .LIBRARY :  $(INCDIR)\generics.h curlib.cm
	echo Done >$@

curlib.cm .LIBRARY : $(OBJ)
.IF	$(NEW)
	lib /nologo /out:$(LIBDIR)\dynlcm.lib @$(mktmp $(?:t"\n")\n)
.ELSE
	lib /nologo /out:$(LIBDIR)\dynlcm.lib $(LIBDIR)\dynlcm.lib @$(mktmp $(?:t"\n")\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

Java.obj JavaClass.obj : package.h

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -p *.d -h $(INCDIR)\generics.h

makegens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -s *.d -h $(INCDIR)\generics.h


clean realclean:
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c
	$(BINDIR)\rm -zq *.cm
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
	$(BINDIR)\rm -zq *.dsw *.opt *.plg
	$(BINDIR)\rm -zq *.dsp

