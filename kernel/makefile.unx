
include ../makefile.inc

#DPPOPTS += -g $(INCDIR)/generics.h -ni

CLASSES = Object.d Behavior.d MetaClass.d Class.d Method.d GenericFunction.d Dynace.d


OBJ = $(CLASSES:.d=.o) kernel.o malloc.o getpagesize.o getinitialpagesize.o

#  Uncomment the following line if you wish to include the assembler jumpto
#JUMPTO = jumpto.o


$(LIBDIR)/dynace.a :  $(OBJ) $(JUMPTO)
	$(AR) $(ARFLAGS) $@ $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $@;\
		ranlib $@;\
	fi

jumpto.o : jumpto/jumpto.s
	$(CC) -c $<


kernel.o : kernels.h Object.iv Behavior.iv Method.iv GenericFunction.iv

Object.o : Object.iv

Behavior.o : kernels.h Behavior.iv

GenericFunction.o : kernels.h GenericFunction.iv

Method.o : Method.iv


newgens:
	$(DPP) $(DPPOPTS) -g $(INCDIR)/generics.h -ni -p $(CLASSES) -h $(INCDIR)/generics.h

reallynewgens:
	$(DPP) $(DPPOPTS) -ni -p $(CLASSES) -h $(INCDIR)/generics.h


clean:
	-rm -f *.obj *.exe generics.* *.o core *~ *.pdb

