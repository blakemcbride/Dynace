
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


newgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h

clean:
	-rm -f *.obj *.exe generics.* main *.o core *.err *~ *.pdb

