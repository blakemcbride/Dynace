
# DMAKE makefile for NT using Microsoft Visual C++ 32

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

#  JavaScript (Spidermonkey) stuff
#JS_IPATH = C:\JavaScript\src
JS_IPATH = ..\..\JavaScript\js\src
#JS_IPATH = C:\Blake\Systems\JavaScript\js\src
JS_LPATH = $(JS_IPATH)\Debug


.d.c .PRECIOUS :
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t -p $<

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

#if 0
#define WINVER	 -DWINVER=0x0500
#endif

.IF $(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -Od -D_WINDOWS -DWIN32 -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA) $(WINVER)
#GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS -DWIN32 -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Od -D_WINDOWS -DWIN32 -Zi -Fd$(LIBDIR)\dynm32.pdb $(WINVER)
.ELSE
CFLAGS = -nologo -I$(INCDIR) -O2 -D_WINDOWS -DWIN32 $(CFEXTRA) $(WINVER)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS -DWIN32 $(WINVER)
.END

.IF $(DEMO)
CFLAGS += -DDEMO
.ENDIF

.IF $(LASTDATE)
CFLAGS += -DLASTDATE=$(LASTDATE)
.ENDIF


CLASS_SRC = Application.d Window.d Menu.d PopupMenu.d Dialog.d TextControl.d StaticControl.d \
	NumericControl.d DateControl.d Control.d HandleCache.d Font.d TextVector.d MessageDispatcher.d \
	HandleCacheNode.d MainWindow.d PopupWindow.d ChildWindow.d PushButton.d CheckBox.d \
	RadioButton.d ListBox.d ComboBox.d ScrollBar.d Icon.d Cursor.d Brush.d \
	SystemBrush.d StockBrush.d SolidBrush.d HatchBrush.d SystemFont.d ExternalFont.d ExternalFontWithColor.d \
	ExternalIcon.d SystemIcon.d SystemCursor.d ExternalCursor.d Printer.d ButtonWindow.d \
	Pen.d StockPen.d CustomPen.d CommonDialog.d ColorDialog.d FontDialog.d FileDialog.d \
	PrintDialog.d HelpSystem.d MessageHandler.d DynamicLibrary.d CustomControl.d ExternalMenu.d \
	InternalMenu.d ModalDialog.d ModelessDialog.d DirListBox.d StatusBar.d ToolBar.d \
	ClientArea.d wdsopts.d DisplayWindow.d TaskList.d Task.d DialogTask.d main.d \
	SpinControl.d StatusWindow.d SplashWindow.d Scheme.d ControlVector.d StaticTextControl.d TreeView.d \
	TreeViewItem.d ProgramManager.d Bitmap.d WindowTask.d TreeViewTaskList.d TrackRect.d DragWindow.d \
	RectControl.d LineControl.d ImageControl.d CLD.d SchemeWindowTask.d SchemeDialogTask.d Ftp.d Mapi.d \
	FunctionList.d ImageList.d StackTracer.d SchemeThread.d HttpRequest.d DatePicker.d ShortcutMenu.d \
	TimeControl.d JavaScriptString.d WebService.d GenericControl.d
#	JavaScript.d

#supplimental-wind.d

C_SRC = printdib.c 

OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj

allok.nm .LIBRARY : $(INCDIR)\generics.h curlib.nm
	echo Done >$@

curlib.nm .LIBRARY : $(OBJS)
.IF  $(NEW)
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.ELSE
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib $(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -s @$(mktmp $(^:t"\n"))

CheckBox.obj ComboBox.obj DateControl.obj ListBox.obj NumericControl.obj PushButton.obj RadioButton.obj \
StaticTextControl.obj TextControl.obj Window.obj : ctlsave.h

printdib.c : printdib.d
	$(BINDIR)\cp $< $@

JavaScript.obj : JavaScript.c
	$(CC) $(CFLAGS) -c -I$(JS_IPATH) $<

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c

generics.obj : generics.c
	$(CC) -c $(GFLAGS) $<

newgens : 
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -s *.d -h $(INCDIR)\generics.h

makegens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -s *.d -h $(INCDIR)\generics.h


clean:
	$(BINDIR)\rm -zq *.obj *.o *.exe *.err *.pdb
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*

realclean: clean
	$(BINDIR)\rm -zq curlib.* allok.* *.c generics.h generics.1

