
include ../makefile.inc

DPPOPTS += $(STRAT)

ifdef	GC
CFLAGS += -DUSE_GC
endif

TARGET = dpp$(EXE)

CLASSES = Prototype.d InputStream.d OutputStream.d Token.d ArgumentList.d OutputStream2.d

CFILES = dpp.c

OBJS = $(CFILES:.c=.o) $(CLASSES:.d=.o)

$(TARGET) : generics.1 generics.h generics.o $(OBJS)
	$(CC) $(OPT) -o $(TARGET) $(OBJS) generics.o $(LIBDIR)/dynace.a -lm

install : $(TARGET)
	cp $(TARGET) $(BINDIR)

generics.h : $(CLASSES)
	$(DPP) $(DPPOPTS) -g -t -h -p $?

generics.c : generics.h
	$(DPP) $(DPPOPTS) -g -c

generics.o : generics.c 
	$(CC) $(GFLAGS) -c $<

generics.1 newgens:
	-rm -f generics.*
	$(DPP) $(DPPOPTS) -g $(INCDIR)/generics.h -p $(CLASSES) -h
	touch generics.1

clean realclean:
	-rm -f *.obj *.exe $(TARGET) *.o *~

