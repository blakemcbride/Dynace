#  Makefile designed for use by the GNUMAKE utility and		-*-Makefile-*-
#  the Wine libraries

CC = winegcc -m32


DYNACE_PATH = ../..

WINEINCPATH = /usr/include/wine


BINDIR = $(DYNACE_PATH)/bin
LIBDIR = $(DYNACE_PATH)/lib
INCDIR = $(DYNACE_PATH)/include

WINEINC = -I$(WINEINCPATH)/windows -I$(WINEINCPATH)/msvcrt


WINELIBS = -lkernel32 -luser32 -lgdi32 -lcomdlg32 -lcomctl32 -ladvapi32 -lshell32 -lmpr -lwine -lmsvcrt -lmsimg32 -lodbc32 -lwsock32 -lrpcrt4

DEBUG = -g
#DEBUG = -O

CFLAGS = -I$(INCDIR) $(WINEINC) -D_WINDOWS
GFLAGS = -I$(INCDIR) -O -w

TARGET = main

CFILES = main.c

OBJS = $(CFILES:.c=.o) 

%.res : %.rc
	echo '#include <winuser.h>' >temp.rc
	echo '#include <commctrl.h>' >>temp.rc
	cat $< >>temp.rc
	wrc temp.rc
	rm -f temp.rc
	mv temp.res $@

%.o : %.c
	$(CC) $(CFLAGS) $(DEBUG) -c $<


$(TARGET) : $(OBJS) algocorp.ico main.res
	$(CC) $(DEBUG) -o $(TARGET) $(OBJS) main.res $(LIBDIR)/wds.a $(LIBDIR)/dynace.a $(WINELIBS) -lm 
	mv $(TARGET).exe $(TARGET)

algocorp.ico : ../setup/algocorp.ico
	cp $^ $@

clean:
	-rm -f *.obj *.exe generics.* *.o *~ main.res temp.rc

realclean: clean
	-rm -f $(TARGET) $(TARGET).exe.so
