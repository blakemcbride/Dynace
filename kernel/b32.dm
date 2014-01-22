
# DMAKE makefile for Borland C 32 bit


.IMPORT .IGNORE : BORLAND_HOME


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include


CC = bcc32
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT = -Od -v
.ELSE
#OPT  = -O -Z -OtglbWeimpv -k-        -Oe kills the threader
OPT  = -O -Z  -OtglbWimpv  -k- 
.END
GOPT  = -O -Z -OtglbWeimpv -k-
COPT = -a4 -d -N-
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
CFLAGS = $(INC) $(COPT) $(OPT) $(WARNINGS) $(CFEXTRA)
GFLAGS = $(INC) $(COPT) $(GOPT) $(WARNINGS) -w-rvl

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -ni -p $<

.c.obj:
	@echo $(CFLAGS) >bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg


CLASSES = object.d behavior.d metaclas.d class.d method.d genfun.d dynace.d


OBJ = $(CLASSES:s/.d/.obj/) kernel.obj malloc.obj


curlib.cb .LIBRARY : $(OBJ) jumpto.obj
	+tlib $(LIBDIR)\dynlcb.lib /E /C /P32 @$(mktmp,tmp.lnk -+$(?:t" &\n-+")\n)
	$(BINDIR)\rm -zq *.obj $(LIBDIR)\dynlcb.bak
	echo Done >$@

jumpto.obj : jumpto\jumpto.b32
	tasm32 /ml $<

kernel.obj : kernels.h object.iv behavior.iv method.iv genfun.iv

object.obj : object.iv

behavior.obj : kernels.h behavior.iv

genfun.obj : kernels.h genfun.iv

method.obj : method.iv

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)/generics.h -ni -p *.d -h $(INCDIR)\generics.h

reallynewgens:
	$(BINDIR)\dpp $(STRAT) -C -ni -p *.d -h $(INCDIR)\generics.h


clean :
	$(BINDIR)\rm -zq *.obj generics.* *.o *.pdb *.err
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean : clean
	$(BINDIR)\rm -zq curlib.*

