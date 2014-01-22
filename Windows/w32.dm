
#  This port has never been completed.

# DMAKE makefile for NT using WATCOM C 32

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -t -p $<

CC = wcl386


.IF $(DEBUG)
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=win386 -bt=windows -d2 -j -D_WINDOWS $(CFEXTRA)
.ELSE
CFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=win386 -bt=windows -oilrt -j -D_WINDOWS $(CFEXTRA)
.END
GFLAGS = -I=$(INCDIR) -4s -zp2 -zq -l=win386 -bt=windows -oilrt -j -s -w0 -D_WINDOWS


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

allok.ww .LIBRARY : generics.1 generics.h curlib.ww
	echo Done >$@

curlib.ww .LIBRARY : generics.obj $(OBJS)
	wlib -b -c -q $(LIBDIR)\dwdsww.lib @$(mktmp,tmp.lnk -+$(?:t"\n-+")\n)
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

checkbox.obj combobox.obj datectl.obj listbox.obj numbctl.obj pushbtn.obj radiobtn.obj \
stattext.obj textctl.obj window.obj : ctlsave.h

printdib.c : printdib.d
	$(BINDIR)\cp $< $@

generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g -t -h -p @$(mktmp $(^:t"\n"))
	cp generics.h ../include/wingens.h


generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c

generics.obj : generics.c
	$(CC) -c $(GFLAGS) $<

newgens generics.1 :
	$(BINDIR)\rm -zq generics.h 
	$(BINDIR)\dpp $(STRAT) -C -g ../include/generics.h -p *.d -h
	cp generics.h ../include/wingens.h
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



