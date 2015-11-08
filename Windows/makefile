# Makefile for WDS on Wine and Linux   	-*-Makefile-*-

WINE = 1

include ../makefile.inc

CC = winegcc

CFLAGS += -I$(WINEINCPATH)/msvcrt -D_WINDOWS -fno-builtin


DPPOPTS += -g $(INCDIR)/generics.h

CLASSES = Application.d Window.d Menu.d PopupMenu.d Dialog.d TextControl.d StaticControl.d \
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
	FunctionList.d ImageList.d HttpRequest.d DatePicker.d ShortcutMenu.d \
	TimeControl.d WebService.d GenericControl.d JavaScriptString.d

C_SRC = printdib.c


OBJ = $(CLASSES:.d=.o) $(C_SRC:.c=.o)

allok : generics.1 curlib
	touch $@

curlib :  $(OBJ)
	$(AR) $(ARFLAGS) $(LIBDIR)/wds.a $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $(LIBDIR)/wds.a;\
		ranlib $(LIBDIR)/wds.a;\
	fi
	touch $@


checkbox.obj combobox.obj datectl.obj listbox.obj numbctl.obj pushbtn.obj radiobtn.obj \
stattext.obj textctl.obj window.obj : ctlsave.h

printdib.c : printdib.d
	cp $< $@

generics.1 newgens reallynewgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h
	touch generics.1


clean realclean:
	-rm -f *.obj *.exe generics.* *.o core *~ *.pdb curlib allok *.c
