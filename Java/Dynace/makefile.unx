# Makefile for WDS on Wine and Linux   	-*-Makefile-*-

include ../../makefile.inc

INCDIR = ../../include
BINDIR = ../../bin

DPPOPTS += -g $(INCDIR)/generics.h

CLASSES = JavaCallback.d  JavaClass.d  Java.d  JavaObject.d

OBJ = $(CLASSES:.d=.o)



newgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h

reallynewgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h


clean:
	-rm -f *.obj *.exe generics.* *.o core *~ *.pdb

