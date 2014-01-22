
#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C 16 bit


TARGET = cld


C_SRC = main.c


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Zi -Od -Or
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500 /co
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Oit
LFLAGS = /NOD /NOI /NOE /SE:256 /PACKC:65500
.END

LIBS = $(LIBDIR)\{dwdswm dynldm}.lib

MSLIBS = oldnames llibcew libw oldnames commdlg ddeml ole2


OBJS = {$(C_SRC:b)}.obj


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


$(TARGET).exe : $(TARGET).ex1 $(TARGET).res
	cp $*.ex1 $*.exe
	rc -nologo -31 -k $(TARGET).res $*.exe

$(TARGET).ex1 : $(OBJS) $(TARGET).def $(LIBS)
	link /BATCH @$(mktmp $(OBJS2),$*.ex1,nul $(LFLAGS)\n\
	     $(LIBS2) $(MSLIBS)\n$(TARGET).def\n)


$(OBJS) : resource.h

$(TARGET).res : $(TARGET).rc
	rc -nologo -r $<


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
	$(BINDIR)\rm -zq *.map
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel

