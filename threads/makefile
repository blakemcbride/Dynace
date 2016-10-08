
include ../makefile.inc

DPPOPTS += -g $(INCDIR)/generics.h

CLASSES = Thread.d Pipe.d Semaphore.d

ifdef NO_CYGWIN
CSOURCE = nttimer.c
else
CSOURCE = unixtime.c
endif

OBJ = $(CLASSES:.d=.o) $(CSOURCE:.c=.o) 

$(LIBDIR)/dynace.a :  $(OBJ)
	$(AR) $(ARFLAGS) $@ $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $@;\
		ranlib $@;\
	fi

# Turn off optimization to compensate for some sort of bug in GCC 5.4.0
Thread.o : Thread.c
	$(CC) -c -I$(INCDIR) $<

newgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h

clean:
	-rm -f *.obj *.exe generics.* main *.o core *.err *~ *.pdb

