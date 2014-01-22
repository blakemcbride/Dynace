
# DMAKE makefile for Windows using Microsoft Visual C++ 16

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t -p $<


.IF $(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Zi -Fd$(LIBDIR)\dynm16.pdb -Od -Or $(CFEXTRA)
.ELSE
CFLAGS = -nologo -I$(INCDIR) -AL -DWINVER=0x0310 -G2As -GEf -Gf -WX -Oit $(CFEXTRA)
.END

.IF $(DEMO)
CFLAGS += -DDEMO
.ENDIF


CLASS_SRC = app.d window.d menu.d popmenu.d dialog.d textctl.d statictl.d \
	numbctl.d datectl.d control.d hdlcache.d font.d textvect.d msgdsp.d \
	hdlnode.d mainwind.d popwind.d chldwind.d pushbtn.d checkbox.d \
	radiobtn.d listbox.d combobox.d scrolbar.d icon.d cursor.d brush.d \
	sysbrush.d stkbrush.d sldbrush.d hchbrush.d sysfont.d extfont.d \
	exticon.d sysicon.d sycursor.d excursor.d printer.d btnwind.d \
	pen.d stkpen.d cstmpen.d commdlg.d colordlg.d fontdlg.d filedlg.d \
	printdlg.d helpsys.d msghand.d library.d custctl.d extmenu.d \
	intmenu.d mdialog.d mldialog.d dirlist.d statbar.d toolbar.d \
	clntarea.d progman.d wdsopts.d filedspl.d tasklist.d task.d dlgtask.d \
	main.d spinctl.d statwind.d splash.d ctlvect.d stattext.d bitmap.d \
	windtask.d trackrect.d dragwindow.d rectctl.d linectl.d swndtask.d \
	sdlgtask.d funclist.d imagectl.d

C_SRC = printdib.c window2.c

OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj


#.IF $(DEMO)
#..\lib\demo\dwdswm.lib : window.c app.c dialog.c
#	$(CC) $(CFLAGS) -c window.c
#	$(CC) $(CFLAGS) -c app.c
#	$(CC) $(CFLAGS) -c dialog.c
#	cp ../lib/dwdswm.lib ../lib/demo
#	lib /batch /noi /nol $@ -+window.obj-+app.obj-+dialog.obj;
#	rm window.obj app.obj dialog.obj $*.bak
#.END


allok.wm .LIBRARY : $(INCDIR)\generics.h curlib.wm
	echo Done >$@

curlib.wm .LIBRARY : $(OBJS)
	lib /batch /noi /nol /page:64 @$(mktmp $(LIBDIR)\\dwdswm\n-+$(^:db:t" &\n-+")\nNUL\n\n)
	echo Done >$@
	$(BINDIR)\rm -zq *.obj $(LIBDIR)/dwdswm.bak

$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -s @$(mktmp $(^:t"\n"))


checkbox.obj combobox.obj datectl.obj listbox.obj numbctl.obj pushbtn.obj radiobtn.obj \
stattext.obj textctl.obj window.obj : ctlsave.h

window.c window2.c : window.d
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t -64 window2.c -p $<

printdib.c : printdib.d
	$(BINDIR)\cp $< $@

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c

generics.obj : generics.c
	$(CC) -c -AL -I$(INCDIR) -Ozax -nologo -DWINVER=0x0310 -D_WINDOWS generics.c

newgens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -s *.d -h $(INCDIR)\generics.h

makegens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -s *.d -h $(INCDIR)\generics.h
	

clean:
	$(BINDIR)\rm -zq *.obj *.exe *.err *.pdb
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean: clean
	$(BINDIR)\rm -zq curlib.* allok.* *.c generics.h generics.1


