
#  The makefile is designed for the DMAKE utility and WATCOM C 32 bit

#  This executable uses a source file called wildargv.c
#  This file comes with the WATCOM compiler and is used to allow the
#  processing of command line arguments.  It is normally located in the
#  following path:   \watcom\src\startup\wildargv.c
#  Just copy it to the current directory.



BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include



CC = wcl386
.IF $(DEBUG)
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -d2 -j
.ELSE
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j -w4
.END
GFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=dos4g -oilrt -j -s -w0

.IF $(CPP)
CFLAGS += -cc++
GFLAGS += -cc++
DPPCPP = -S3
.END

LNKFLG = -4s -zp2 -zq -l=dos4g /"option caseexact" # -d2


TARGET = dpp.exe

CLASSES = proto.d istream.d ostream.d token.d arglist.d ostream2.d

CFILES = dpp.c wildargv.c

OBJS = {$(CFILES:b)}.obj {$(CLASSES:b)}.obj

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(DPPCPP) -C -g -p $<

$(TARGET) : generics.1 generics.h generics.obj $(OBJS) 
	wlink @$(mktmp,tmp.lnk \
		system dos4g\n\
		file $(OBJS:t"\nfile ")\n\
		file generics.obj\n\
		option caseexact,quiet\n\
		library $(LIBDIR)\\dynldw.lib\n\
		name $@\n)


install : $(TARGET)
	$(BINDIR)\cp $(TARGET) $(BINDIR)

generics.h : $(CLASSES)
	$(BINDIR)\dpp $(DPPCPP) -C -g -t -h -p @$(mktmp $(^:t"\n")\n)

generics.c : generics.h
	$(BINDIR)\dpp $(DPPCPP) -C -g -c 

generics.obj : generics.c 
	$(CC) -c $(GFLAGS) generics.c

generics.1 newgens:
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\dpp $(DPPCPP) -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASSES:t"\n")\n)
	@echo Done >generics.1


clean realclean:
	$(BINDIR)\rm -zq *.obj *.o *.pdb *.err *.exe
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*




