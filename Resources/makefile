# Makefile for WDS on Wine and Linux   	-*-Makefile-*-

WINE = 1

include ../makefile.inc

CC = winegcc

CFLAGS += -I$(WINEINCPATH)/msvcrt -D_WINDOWS -fno-builtin


C_SRC = readres.c


OBJ = $(C_SRC:.c=.o)

allok : curlib
	touch $@

curlib :  $(OBJ)
	$(AR) $(ARFLAGS) $(LIBDIR)/wds.a $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $(LIBDIR)/wds.a;\
		ranlib $(LIBDIR)/wds.a;\
	fi
	touch $@


clean realclean:
	-rm -f *.obj *.exe generics.* *.o core *~ *.pdb curlib allok
