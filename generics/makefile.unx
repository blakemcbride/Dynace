
include ../makefile.inc

#  When building with WINE you must compile generics.c with gcc and not winegcc!

curlib : $(INCDIR)/generics.h
	-rm -f generics.h
	$(DPP) $(DPPOPTS) -g $(INCDIR)/generics.h -c -h
	$(CC) -c $(GFLAGS) generics.c
	$(AR) $(ARFLAGS) $(LIBDIR)/dynace.a generics.o
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $(LIBDIR)/dynace.a;\
		ranlib $(LIBDIR)/dynace.a;\
	fi
	touch $@

clean:
	-rm -f generics.* *~ *.pdb

realclean: clean
	-rm -f curlib

