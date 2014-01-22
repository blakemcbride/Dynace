#  Makefile designed for use by the DMAKE.EXE utility and		-*-Makefile-*-
#  Microsoft Visual C/C++ 16 bit


TARGET = main


C_SRC = main.c


DYNACE_PATH = ..\..

BINDIR = $(DYNACE_PATH)\bin
LIBDIR = $(DYNACE_PATH)\lib
INCDIR = $(DYNACE_PATH)\include

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -AL -Gf -WX -Zi -Od -Or
LFLAGS = /NOI /NOE /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -Gf -WX -Oit
LFLAGS = /NOI /NOE /PACKC:65500
.END

LIBS = $(LIBDIR)\dynldm.lib

OBJS = {$(C_SRC:b)}.obj

CC = cl



# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : $(OBJS) $(LIBS)
	link /BATCH @$(mktmp $(OBJS2),$*.exe,nul $(LFLAGS)\n$(LIBS2);\n)


clean:
	rm -zq *.obj
	rm -zq *.exe
	rm -zq *.ex1
	rm -zq *.res
	rm -zq *.aps
	rm -zq *.pdb
	rm -zq *.wsp
	rm -zq *.ilk
	rm -zq *.bsc
	rm -zq *.vcp
	rm -zq *.pch
	rm -zq *.sbr
	rm -zq *.vcw
	rm -zq *.map
	rm -zq generics.*
	rm -zqr WinDebug
	rm -zqr WinRel

