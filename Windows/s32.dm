
# DMAKE makefile for NT using Symantec C++ 32

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CC = sc

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g -t -p $<


.IF $(DEBUG)
CFLAGS = -I$(INCDIR) -D_WINDOWS -DWIN32 -mn -a4 -J -w2 -w7 -g -o-all $(CFEXTRA)
GFLAGS = -I$(INCDIR) -D_WINDOWS -DWIN32 -mn -a4 -J -w2 -w7 -g -o-all
.ELSE
CFLAGS = -I$(INCDIR) -D_WINDOWS -DWIN32 -mn -a4 -J -w2 -w7 $(CFEXTRA)
GFLAGS = -I$(INCDIR) -D_WINDOWS -DWIN32 -mn -a4 -J -w2 -w7 
.END


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

allok.ns .LIBRARY : generics.1 generics.h curlib.ns
	echo Done >$@

curlib.ns .LIBRARY : generics.obj $(OBJS)
.IF	$(NEW)
	lib $(LIBDIR)\dwdsns.lib /c /n /noi @$(mktmp +$(?:t" &\n+");\n)
.ELSE
	lib $(LIBDIR)\dwdsns.lib /n /noi @$(mktmp -+$(?:t" &\n-+");\n)
.END
	$(BINDIR)\rm -zq *.obj $(LIBDIR)/dwdsns.bak
	echo Done >$@

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
	$(CC) -c $(GFLAGS) $<

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

