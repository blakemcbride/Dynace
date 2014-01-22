#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  WATCOM C/C++ 32 bit


TARGET = main


C_SRC = main.c


CLASS_SRC = class1.d


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include


.IF	$(DEBUG)
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -d2 -j
LFLAGS = -4s -zp2 -zq -l=dos4g /"option caseexact" -d2
.ELSE
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j
LFLAGS = -4s -zp2 -zq -l=dos4g /"option caseexact" 
.END
GFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j -s -w0

LIBS = $(LIBDIR)\dynldw.lib

OBJS = {$(C_SRC:b)}.obj {$(CLASS_SRC:b)}.obj


CC = wcl386


.d.c .PRECIOUS :
	$(BINDIR)\dpp -g -p $<



# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)



$(TARGET).exe : generics.1 generics.h generics.obj $(OBJS) $(LIBS)
	wlink @$(mktmp,tmp.lnk \
		system dos4g\n\
		file $(OBJS2:t"\nfile ")\n\
		file generics.obj\n\
		option caseexact,quiet\n\
		library $(LIBS2)\n\
		name $@\n)


generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp -g -t -h -p @$(mktmp $(^:t"\n")\n)


generics.c : generics.h
	$(BINDIR)\dpp -g -c

generics.obj : generics.c
	$(CC) -c $(GFLAGS) generics.c

generics.1 : 
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1

newgens:
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASS_SRC:t"\n")\n)
	@echo Done >generics.1




clean:
	rm -zq {$(CLASS_SRC:b)}.c
	rm -zq generics.*
	rm -zq *.obj
	rm -zq *.exe
	rm -zq *.o
	rm -zq *.err


