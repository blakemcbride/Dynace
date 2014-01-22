
#  The makefile is designed for the DMAKE utility and Microsoft C 16 bit


#  Can't build dpp.exe with 16 bit compilers because dpp takes more then
#  640K to run.  dpp.exe for DOS is built with the WATCOM 32 bit compiler
#  which includes a DOS extender.


BINDIR = ..\bin
INCDIR = ..\include

CLASSES = proto.d istream.d ostream.d token.d arglist.d

all:

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c 

generics.1 newgens:
	$(BINDIR)\rm -zq generics.*
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h -p @$(mktmp $(CLASSES:t"\n")\n)
	@echo Done >generics.1

clean realclean:
	$(BINDIR)\rm -zq *.obj *.o *.pdb *.err *.exe *.vcp *.ilk *.mdp *.ncb
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*



