
# DMAKE makefile for Microsoft Visual C 16 bit


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


curlib.dm : $(INCDIR)\generics.h
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -c -h -M600
	$(CC) -c -AL -I$(INCDIR) -Ozax -nologo generics.c
	$(CC) -c -AL -I$(INCDIR) -Ozax -nologo gens?.c
	lib /batch /noi /nol /page:32 $(LIBDIR)\dynldm-+generics.obj-+gens1.obj-+gens2.obj;
	$(BINDIR)\rm -zq *.obj generics.* gens?.* $(LIBDIR)/dynldm.bak
	echo Done >$@


clean:
	$(BINDIR)\rm -zq *.obj generics.* gens?.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*
