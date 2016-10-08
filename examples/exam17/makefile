#  Makefile designed for use by the GNU MAKE utility and		-*-Makefile-*-
#  GCC

ifdef WINE
WINEINCPATH = /usr/include/wine
CC = winegcc -m32
GCC = gcc -m32
else
CC = gcc
GCC = $(CC)
endif

ifdef TCC # target compiler
CC = $(TCC)
GCC = $(TCC)
endif

.SUFFIXES: .d


DYNACE_PATH = ../..

BINDIR = $(DYNACE_PATH)/bin
LIBDIR = $(DYNACE_PATH)/lib
INCDIR = $(DYNACE_PATH)/include


#DEBUG = -g
#DEBUG = -O

ifdef WINE
CFLAGS = -I$(INCDIR) -I$(WINEINCPATH)/windows -D_WINDOWS -D_WIN32 -D__WINE__
GFLAGS = -I$(INCDIR) -I$(WINEINCPATH)/windows -D_WINDOWS -D_WIN32 -D__WINE__ -O -w
else
CFLAGS = -I$(INCDIR)
GFLAGS = -I$(INCDIR) -O -w
endif

TARGET = main

CLASSES = class1.d class2.d

CFILES = main.c

OBJS = $(CFILES:.c=.o) $(CLASSES:.d=.o)

.d.c:
	$(BINDIR)/dpp -g -p $<

.c.o:
	$(CC) $(CFLAGS) $(DEBUG) -c $<


$(TARGET) : generics.1 generics.h generics.o $(OBJS)
	$(CC) $(DEBUG) -o $(TARGET) $(OBJS) generics.o $(LIBDIR)/dynace.a -lm
ifdef WINE
	mv $(TARGET).exe $(TARGET)
endif

generics.h : $(CLASSES)
	$(BINDIR)/dpp -g -t -h -p $?

generics.c : generics.h
	$(BINDIR)/dpp -g -c

# generics.c must be compiled with gcc and not winegcc
generics.o : generics.c 
	$(GCC) $(GFLAGS) -c $<

generics.1 newgens:
	$(BINDIR)/dpp -g $(INCDIR)/generics.h -p $(CLASSES) -h
	touch generics.1


clean:
	-rm -f *.obj *.exe generics.* $(TARGET) $(TARGET).exe.so *.o $(CLASSES:.d=.c) *~

