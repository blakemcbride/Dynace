#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  WATCOM C/C++ 32 bit


TARGET = main


C_SRC = main.c


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


OBJS = {$(C_SRC:b)}.obj


CC = wcl386

LIBS = $(LIBDIR)\dynldw.lib


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)



$(TARGET).exe : $(OBJS) $(LIBS)
	wlink @$(mktmp,tmp.lnk \
		system dos4g\n\
		file $(OBJS2:t"\nfile ")\n\
		option caseexact,quiet\n\
		library $(LIBS2)\n\
		name $@\n)



clean:
	rm -zq *.obj
	rm -zq *.exe
	rm -zq *.o
	rm -zq *.err


