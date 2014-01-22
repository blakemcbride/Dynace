
# DMAKE makefile for NT using Borland C++ 32


BORLAND_HOME = f:\bc45


BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CC = bcc32
WARNINGS = -w-par -w-pia -w-pro -w-stu
.IF $(DEBUG)
OPT = -Od -v
.ELSE
OPT  = -O -Z -OtglbWimpv -k- # -Oe doesn't work
.END
GOPT  = -O -Z -Ot -k-
COPT = -a4 -d -N- -D_WINDOWS
INC  = -I$(INCDIR) -I$(BORLAND_HOME)\include
CFLAGS = $(COPT) $(OPT) $(WARNINGS) $(CFEXTRA)
GFLAGS = $(COPT) $(GOPT) $(WARNINGS) -w-rvl



CLASS_SRC = app.d window.d menu.d popmenu.d dialog.d textctl.d statictl.d \
	numbctl.d datectl.d control.d hdlcache.d font.d textvect.d msgdsp.d \
	hdlnode.d mainwind.d popwind.d chldwind.d pushbtn.d checkbox.d \
	radiobtn.d listbox.d combobox.d scrolbar.d icon.d cursor.d brush.d \
	sysbrush.d stkbrush.d sldbrush.d hchbrush.d sysfont.d extfont.d \
	exticon.d sysicon.d sycursor.d excursor.d printer.d btnwind.d \
	pen.d stkpen.d cstmpen.d commdlg.d colordlg.d fontdlg.d filedlg.d \
	printdlg.d helpsys.d msghand.d library.d custctl.d extmenu.d \
	intmenu.d mdialog.d mldialog.d dirlist.d statbar.d toolbar.d \
	clntarea.d wdsopts.d filedspl.d main.d spinctl.d statwind.d splash.d \
	ctlvect.d stattext.d bitmap.d windtask.d tvtsklst.d

C_SRC = printdib.c

OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -t -p $<

.c.obj:
	@echo $(CFLAGS) >bcc32.cfg
	@echo $(INC) >>bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg

allok.nb .LIBRARY : generics.1 generics.h curlib.nb
	echo Done >$@

curlib.nb .LIBRARY : generics.obj $(OBJS)
	+tlib $(LIBDIR)\dwdsnb /E /C /P32 @$(mktmp,tmp.lnk -+$(?:t" &\n-+")\n)
	echo Done >$@
	$(BINDIR)\rm -zq *.obj $(LIBDIR)/dwdsnb.bak

checkbox.obj combobox.obj datectl.obj listbox.obj numbctl.obj pushbtn.obj radiobtn.obj \
stattext.obj textctl.obj window.obj : ctlsave.h

printdib.c : printdib.d
	$(BINDIR)\cp $< $@

generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g -t -h -p @$(mktmp $(^:t"\n"))
	$(BINDIR)\cp generics.h ../include/wingens.h


generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c

generics.obj : generics.c
	@echo $(GFLAGS) >bcc32.cfg
	@echo $(INC) >>bcc32.cfg
	$(CC) -c $<
	@$(BINDIR)\rm -zq bcc32.cfg

newgens generics.1 :
	$(BINDIR)\rm -zq generics.h 
	$(BINDIR)\dpp $(STRAT) -C -g ../include/generics.h -p *.d -h
	$(BINDIR)\cp generics.h ../include/wingens.h
	echo Done >generics.1

makegens:
	$(BINDIR)\rm -zq generics.h
	$(BINDIR)\dpp $(STRAT) -C -g ../include/generics.h -s *.d -h
	$(BINDIR)\cp generics.h ../include/wingens.h

clean:
	$(BINDIR)\rm -zq *.obj *.o *.exe *.err *.pdb
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean: clean
	$(BINDIR)\rm -zq curlib.* allok.* *.c generics.h generics.1


