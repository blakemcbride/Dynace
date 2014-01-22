
# DMAKE makefile for Microsoft Visual C++ 32 bit


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

.IF $(BOEHM_GC)
CFEXTRA += -DBOEHM_GC
.END

CC = cl
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -WX -nologo -W3 -Od -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
.ELSE
#  The following optimization combinations failed the GC
#  the problem seems to be the combination of -Og and -Ob1 (or -Ob2)
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 -O2 $(CFEXTRA)
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy -Ob1 -Gs -Gy
# worked CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy -Ob1 -Gs
# worked CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy -Gy
# worked CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy -Gs -Gy
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Og -Oi -Ot -Oy -Gs -Gy -Ob2
# worked CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2 -Og -Oi -Ot
# no CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2 -Og
# yes CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2
# yes CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2 -Oi -Ot
CFLAGS = -I$(INCDIR) -WX -nologo -W3 $(CFEXTRA) -Ob2 -Oi -Ot -Oy -Gs -Gy
.END


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<


CLASSES = Object.d Behavior.d MetaClass.d Class.d Method.d GenericFunction.d Dynace.d


OBJ = $(CLASSES:s/.d/.obj/) kernel.obj win32gm.obj malloc.obj getpagesize.obj getinitialpagesize.obj

curlib.cm .LIBRARY : $(OBJ) jumpto.obj
.IF	$(NEW)
	lib /nologo /out:$(LIBDIR)\dynlcm.lib @$(mktmp $(?:t"\n")\n)
.ELSE
	lib /nologo /out:$(LIBDIR)\dynlcm.lib $(LIBDIR)\dynlcm.lib @$(mktmp $(?:t"\n")\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

jumpto.obj : jumpto\jumpto.nt3
	$(CC) $(CFLAGS) -c -Tc$<

kernel.obj : kernels.h Object.iv Behavior.iv Method.iv GenericFunction.iv

Object.obj : Object.iv

Behavior.obj : kernels.h Behavior.iv

GenericFunction.obj : kernels.h GenericFunction.iv

Method.obj : Method.iv

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -ni -p *.d -h $(INCDIR)\generics.h

reallynewgens:
	$(BINDIR)\dpp $(STRAT) -C -ni -p *.d -h $(INCDIR)\generics.h

clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
