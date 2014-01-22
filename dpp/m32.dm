
#  The makefile is designed for the DMAKE utility and Microsoft C 32 bit



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END


CC = cl
.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -WX -nologo -W3 -Od -Zi $(CFEXTRA)
LFLAGS = /subsystem:console /debug
.ELSE
CFLAGS = -I$(INCDIR) -WX -nologo -W3 -O2 $(CFEXTRA)
LFLAGS = /subsystem:console
.END

GFLAGS = -I$(INCDIR) -nologo -Oityb1 -Gs -Gy $(CFEXTRA)


TARGET = dpp.exe

CLASSES = Prototype.d InputStream.d OutputStream.d Token.d ArgumentList.d OutputStream2.d

CFILES = dpp.c 

OBJS = {$(CFILES:b)}.obj {$(CLASSES:b)}.obj

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -p $<

LIBS = $(LIBDIR)\dynlcm.lib 

# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)

$(TARGET) : generics.1 generics.h generics.obj $(OBJS) 
	link /nologo @$(mktmp /out:$@ $(LFLAGS)\n$(OBJS2)\ngenerics.obj setargv.obj\n$(LIBS2)\nwinmm.lib\n)


install : $(TARGET)
	$(BINDIR)\cp $(TARGET) $(BINDIR)

generics.h : $(CLASSES)
	$(BINDIR)\dpp $(STRAT) -C -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c 

generics.obj : generics.c 
	$(CC) $(GFLAGS) -c generics.c

generics.1 newgens:
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASSES:t"\n")\n)
	@echo Done >generics.1


clean realclean:
	$(BINDIR)\rm -zq *.obj *.o *.pdb *.err *.exe *.vcp *.ilk *.mdp *.ncb *.opt *.dsp *.dsw
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*



