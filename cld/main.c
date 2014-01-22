
/*  Copyright 1998 Blake McBride (blake@mcbride.name)  */

#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif
#define _POSIX_

#include <windows.h>
#include <wininet.h>
#include <initguid.h>
#include <shlobj.h>
#include <shlguid.h>
#include <objbase.h>
#include <wtypes.h>
#include <string.h>

#include "generics.h"
#include "resource.h"
#include "../Windows/ctlsave.h"

// Yanghui:
#include "TALBarCd.h"
#include "../Windows/color.h"
#define  CHANDLESIZE             7  
#define  DEFAULT_ICON_INDEX      1 
#include <time.h>


// Yanghui

#include <string.h>
#include <tchar.h>

extern	void	saveFlashFile(object wind, char *fname, char *name);
extern	void	saveJSFLFile(object wind, char *fname, char *asname);


#define fileExists(s)	!access(s, 0)

#define LOAD_TYPE_BINARY	0
#define LOAD_TYPE_XML		1
#define BUFFER_SIZE			2048

static	long	file_exit(object wind, unsigned id);

static	void	init_controls(object dlg);
static	long	static_tool(object wind, unsigned bm);
static	long	text_tool(object wind, unsigned bm);
static	long	numeric_tool(object wind, unsigned bm);
static	long	date_tool(object wind, unsigned bm);
static	long	time_tool(object wind, unsigned bm);
static	long	push_tool(object wind, unsigned bm);
static	long	radio_tool(object wind, unsigned bm);
static	long	check_tool(object wind, unsigned bm);
static	long	listbox_tool(object wind, unsigned bm);
static	long	combobox_tool(object wind, unsigned bm);
static	long	quit_tool(object wind, unsigned bm);
static	long	save_tool(object wind, unsigned bm);
static	long	load_tool(object wind, unsigned bm);
static	long	new_tool(object wind, unsigned bm);
static	long	test_tool(object wind, unsigned bm);
static	long	scale_tool(object wind, unsigned bm);
static	long	lang_tool(object wind, unsigned bm);
static	long	generic_tool(object wind, unsigned bm);

static	void	touch(char *file);

// Yanghui:
void			print_barcode(object pntr, double yInches, double xInches, double heightInches, int barWidth, char *msg);

static	long	updateMenuAndToolBar(object objMainWindow);

static	long	about(object wind);
static  long    cut_tool(object wind, unsigned bm);
static  long    copy_tool(object wind, unsigned bm);
static  long    paste_tool(object wind, unsigned bm);
static  long    undo_tool(object wind, unsigned bm);

static  long    print_tool(object wind, unsigned bm);
static  long    run_test_tool(object wind, unsigned bm);
static  long    run_test_tool2(object wind, unsigned bm);
static	long	window_tool(object wind);

// static  long    print_preview_tool(object wind, unsigned bm);

static  long    rect_tool(object wind, unsigned bm);
static  long    line_horz_tool(object wind, unsigned bm);
static  long    line_vert_tool(object wind, unsigned bm);
static  long    image_tool(object wind, unsigned bm);

static  long    align_tool(object wind, unsigned bm);
static  long    sameSize_tool(object wind, unsigned bm);
static  long    toggleTheRulerAndGrid(object wind, unsigned bm);

static  void    addColorOptions(object ctl);
static  void    setColorStringValue(object ctl, char color);
static  char    getColorIndex(const char strTmp[]);
static  void    setColorStringValueFromRGB(object ctl, COLORREF color);
static  int     rect_rbutton(object wctl, unsigned button);
static  int     line_rbutton(object wctl, unsigned button);
static  int     image_rbutton(object wctl, unsigned button);

static  BOOL    setRegKey(char *lpszKey, char *lpszValue, char *lpszValueName);
static  BOOL    deleteRegKey(char *lpszKey);
static  BOOL	modifyStyle(HWND hWnd, DWORD dwRemove, DWORD dwAdd, UINT nFlags);

static	long	fileOpen(object wind, char *file);
static	long	filePrint(object wind, char *strIn);
static	void	registerShellFileTypes();
// Yanghui

static	int	static_rbutton(object wctl, unsigned button);
static	int	text_rbutton(object wctl, unsigned button);
static	int	numeric_rbutton(object wctl, unsigned button);
static	int	date_rbutton(object wctl, unsigned button);
static	int	time_rbutton(object wctl, unsigned button);
static	int	push_rbutton(object wctl, unsigned button);
static	int	radio_rbutton(object wctl, unsigned button);
static	int	check_rbutton(object wctl, unsigned button);
static	int	listbox_rbutton(object wctl, unsigned button);
static	int	combobox_rbutton(object wctl, unsigned button);
static	int	generic_rbutton(object wctl, unsigned button);

static	void	set_mouse_functions(int flg);
static	int	dispatch_mouse_click(object ctl, unsigned button);

static	int	changeSingleControlFont(object ctl, object dlg);
static	int	changeSingleControlFontAndTextColor(object ctl, object dlg);
static	int	loadBitmap(object ctl, object dlg);
static	int	clearBitmap(object ctl, object dlg);
static	int	saveProperty(object ctl, object dlg);
static	int	deleteProperty(object ctl, object dlg);


static	long	changeDefaultControlFonts(object wind, unsigned bm);
static	long	export_xml(object wind, unsigned bm);
static	long	export_flash(object wind, unsigned bm);
static	void	updateControlDictionary(object dlg);
static	void	updateAllControlFonts(object wind);
static	int	changeFont(object nctl, object dlg);
static	int	changeLanguage(object nctl, object dlg);
static	int	changeProperty(object nctl, object dlg);
static	object	getCurrentFont(char *ctltype);
static	int	loadFontComboBoxes(object dlg);

static	void setLanguageValues(object ctl, object dlg);
static	void addLanguageChoices(object ctl, object dlg, int size);
static	long	saveLanguageChoice(object	ctl, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam);

static int getClipboardFileName(char * szClipboardFileName);
static	int	saveFontDefaultSelections(object dlg);


#define	EXT	"cld"

static	char	CLFile[256];
static	object	ControlFonts;
static	object	windBorderRect=NULL;
static	object	wsdlUrlList=NULL;
static	double	scalingFactor=1.0;
static	object	wsdlDOM=NULL;
static	int		loadWSDLList();
static	char	lastURL[BUFFER_SIZE];
static	char	lastOp[BUFFER_SIZE];


static	char	*lcase(char *v)
{
	char	*b = v;
	for ( ; *v ; ++v)
		if (isupper(*v))
			*v = tolower(*v);
	return b;
}

int	start()
{
	object	win, menu=NULL;
	FILE	*fp;
	char    clipboard[_MAX_PATH];
	HMENU	hmenu;
	int     numArgs;
	char    strArg[_MAX_PATH];

	gSetFont(Application, vNew(ExternalFont, "Arial", 10));
 	// gSetFont(Application, vNew(ExternalFont, "Small Fonts Bold Italic Underline Strikeout", 6));
	win = vNew(MainWindow, "Dialog Designer");

	ControlFonts = gNew(StringDictionary);

	menu = mLoadMenu(win, IDR_MAIN_MENU);
	mAssociate(win, ID_HELP_ABOUT, about);

	mAssociate(win, IDB_ALIGN_LEFT,   align_tool);
	mAssociate(win, IDB_ALIGN_RIGHT,  align_tool);
	mAssociate(win, IDB_ALIGN_TOP,    align_tool);
	mAssociate(win, IDB_ALIGN_BOTTOM, align_tool);
	mAssociate(win, IDB_CENTER_VERT,  align_tool);
	mAssociate(win, IDB_CENTER_HORZ,  align_tool);

	mAssociate(win, IDB_SAME_WIDTH,  sameSize_tool);
	mAssociate(win, IDB_SAME_HEIGHT, sameSize_tool);
	mAssociate(win, IDB_SAME_SIZE,   sameSize_tool);

	mAssociate(win, ID_WINDOW_PROPERTIES, window_tool);

	gSetUpdateMenuAndToolBarFunction(DragWindow, updateMenuAndToolBar);

	if (menu) {
		gEnableMenuItem(menu, IDB_ALIGN_LEFT,   MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_ALIGN_RIGHT,  MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_ALIGN_TOP,    MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_ALIGN_BOTTOM, MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_CENTER_VERT,  MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_CENTER_HORZ,  MF_BYCOMMAND|MF_GRAYED);

		gEnableMenuItem(menu, IDB_SAME_WIDTH,  MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_SAME_HEIGHT, MF_BYCOMMAND|MF_GRAYED);
		gEnableMenuItem(menu, IDB_SAME_SIZE,   MF_BYCOMMAND|MF_GRAYED);
	}

	mAddToolBitmap(win, IDB_NEW, 0, 0, new_tool, "New");
	mAddToolBitmap(win, IDB_OPEN, 0, 0, load_tool, "Open");
	mAddToolBitmap(win, IDB_SAVE, 0, 0, save_tool, "Save in binary CLD format");

	mAddToolBitmap(win, IDB_CUT, 0, 0, cut_tool, "Cut");
	mAddToolBitmap(win, IDB_COPY, 0, 0, copy_tool, "Copy");
	mAddToolBitmap(win, IDB_PASTE, 0, 0, paste_tool, "Paste");
	mAddToolBitmap(win, IDB_UNDO, 0, 0, undo_tool, "Undelete");

	mAddToolBitmap(win, IDB_PRINT, 0, 0, print_tool, "Print");
	// mAddToolBitmap(win, IDB_PRINT_PREVIEW, 0, 0, print_preview_tool, "Print Preview");

	mAddToolBitmap(win, IDB_TEST, 0, 0, test_tool, "Toggle Test/Design Mode");
	mAddToolBitmap(win, IDB_LANGSWITCH, 0, 0, lang_tool, "Toggle English/Spanish");
	mAddToolBitmap(win, IDB_SCALE, 0, 0, scale_tool, "Scale/Delete Metafile");
	mAddToolBitmap(win, IDB_CONTROL_FONTS, 0, 0, changeDefaultControlFonts, "Control Fonts");
	mAddToolBitmap(win, IDB_SAVEXML, 0, 0, export_xml, "Save in XML CLD Format");
	mAddToolBitmap(win, IDB_FLASH, 0, 0, export_flash, "Export Flash Action Script File");
	mAddToolBitmap(win, IDB_EXIT, 0, 0, quit_tool, "Exit");
	// mAddToolBitmap(win, IDB_RUN_TEST, 0, 0, run_test_tool, "Run Test");
	
	mAddToolBitmap(win, IDB_STATIC, 0, 3, static_tool, "Static Text");

	mAddToolBitmap(win, IDB_TEXT, 0, 0, text_tool, "Text Entry");
	mAddToolBitmap(win, IDB_NUMBER, 0, 0, numeric_tool, "Numeric Entry");
	mAddToolBitmap(win, IDB_DATE, 0, 0, date_tool, "Date Entry");
	mAddToolBitmap(win, IDB_TIME, 0, 0, time_tool, "Time Entry");
	mAddToolBitmap(win, IDB_PUSHBUTTON, 0, 0, push_tool, "Push Button");
	mAddToolBitmap(win, IDB_RADIOBUTTON, 0, 0, radio_tool, "Radio Button");
	mAddToolBitmap(win, IDB_CHECKBOX, 0, 0, check_tool, "Check Box");
	mAddToolBitmap(win, IDB_LISTBOX, 0, 0, listbox_tool, "List Box");
	mAddToolBitmap(win, IDB_COMBOBOX, 0, 0, combobox_tool, "Combo Box");
	mAddToolBitmap(win, IDB_RECT, 0, 0, rect_tool, "Rectangle");
	mAddToolBitmap(win, IDB_LINE_HORZ, 0, 0, line_horz_tool, "Line");
	mAddToolBitmap(win, IDB_LINE_VERT, 0, 0, line_vert_tool, "Line");
	mAddToolBitmap(win, IDB_BITMAP, 0, 0, image_tool, "Bitmap");
	mAddToolBitmap(win, IDB_GENERICCONTROL, 0, 0, generic_tool, "Generic Control");

	// Yanghui
/***********
	mAddToolBitmap(win, IDB_ALIGN_LEFT,   0, 2, align_tool, "Align Left");
	mAddToolBitmap(win, IDB_ALIGN_RIGHT,  0, 0, align_tool, "Align Right");
	mAddToolBitmap(win, IDB_ALIGN_TOP,    0, 0, align_tool, "Align Top");
	mAddToolBitmap(win, IDB_ALIGN_BOTTOM, 0, 0, align_tool, "Align Bottom");
	mAddToolBitmap(win, IDB_CENTER_VERT,  0, 0, align_tool, "Center Vertical");
	mAddToolBitmap(win, IDB_CENTER_HORZ,  0, 0, align_tool, "Center Horizontal");

	mAddToolBitmap(win, IDB_SAME_WIDTH,   0, 0, sameSize_tool, "Make Same Width");
	mAddToolBitmap(win, IDB_SAME_HEIGHT,  0, 0, sameSize_tool, "Make Same Height");
	mAddToolBitmap(win, IDB_SAME_SIZE,    0, 0, sameSize_tool, "Make Same Size");
***********/
	mAddToolBitmap(win, IDB_RULER_GRID,   0, 3, toggleTheRulerAndGrid, "Toggle Ruler and Grid");

	gEnableToolBitmap(win, IDB_ALIGN_LEFT,   FALSE);
	gEnableToolBitmap(win, IDB_ALIGN_RIGHT,  FALSE);
	gEnableToolBitmap(win, IDB_ALIGN_TOP,    FALSE);
	gEnableToolBitmap(win, IDB_ALIGN_BOTTOM, FALSE);
	gEnableToolBitmap(win, IDB_CENTER_VERT,  FALSE);
	gEnableToolBitmap(win, IDB_CENTER_HORZ,  FALSE);

	gEnableToolBitmap(win, IDB_SAME_WIDTH,   FALSE);
	gEnableToolBitmap(win, IDB_SAME_HEIGHT,  FALSE);
	gEnableToolBitmap(win, IDB_SAME_SIZE,    FALSE);

	gEnableToolBitmap(win, IDB_COPY, FALSE);
	gEnableToolBitmap(win, IDB_CUT,  FALSE);

	// gEnableToolBitmap(win, IDB_UNDO,  FALSE);

	// to see if Clipboard.bin already exists 
	if (getClipboardFileName(clipboard)==-1 || !(*clipboard))
		gEnableToolBitmap(win, IDB_PASTE, FALSE);
	else {
		fp = fopen(clipboard, "rb");
		if (fp) {
			fclose(fp);
			gEnableToolBitmap(win, IDB_PASTE, TRUE);
		} else
			gEnableToolBitmap(win, IDB_PASTE, FALSE);
	}

	gSetMainWindowObj(DragWindow, win);
	// Yanghui

	gBackBrush(win, vNew(SystemBrush, COLOR_BTNFACE));

	mLoadIcon(win, ALGOCORP_ICON);

	// Yanghui: the screen resolution scaling is turned on
	gSetScaleFlg(win,1);    
#ifdef INTEGRA
	gSetScaleFlg(win, 0);    // the screen resolution scaling is turned off
#endif
	// Yanghui

	set_mouse_functions(1);

	// gInitFunction(win, setup_controls_dialog);
	gSetModifyChildren(win, 1);

	gShow(win);
	
	numArgs = gNumArgs(Application);
	if (numArgs==1)
		fileOpen(win, gGetArg(Application, 0));
	else if (numArgs==2) {
		strcpy(strArg, gGetArg(Application, 0));
		if (stricmp("/p", strArg)==0)
			filePrint(win, gGetArg(Application, 1));
	}


	loadWSDLList();
	

	return gProcessMessages(win);
}

#define CLDWSDL "c:\\cldWSDLs.xml"
static	int saveWSDLList()
{
	object opt,seq;
	FILE * fp=fopen(CLDWSDL,"wt");
	fprintf(fp,"\t<wsdls>\n");
	if (wsdlUrlList)
		for (seq = gSequence(wsdlUrlList) ; opt = gNext(seq) ; ) 
			fprintf(fp,"<wsdl>%s</wsdl>",gStringValue(gValue(opt)));
	fprintf(fp,"\t</wsdls>\n");

	fclose(fp);
	return 0;
}

static	int loadWSDLList()
{
	object mainnode=gParseFile(XMLNode,CLDWSDL,NULL,NULL,NULL);
	object curnode, seq, list;

	if (wsdlUrlList)
			gDispose(wsdlUrlList);

	wsdlUrlList=gNew(LinkList);

	if (mainnode)
	{
		if (list=gXPath(mainnode,"/wsdls/wsdl/text()"))
			for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
				gAddLast(wsdlUrlList,gNewWithObj(LinkValue,gNewWithStr(String,gStringValue(curnode))));
			

		gDispose(mainnode);
	}
	return 0;
}

static	long	about(object wind)
{
	object	dlg, ctl;
	char	buf[256];

	/*****
    time_t long_time;
	struct tm *tmTime;
	*****/

	dlg = mNewDialog(ModalDialog, IDD_ABOUT, wind);

	ctl = mAddControl(dlg, StaticControl, IDC_PRODUCT);
	gSetStringValue(ctl, "Cld");

	ctl = mAddControl(dlg, StaticControl, IDC_VERSION);
	strcpy(buf, "Version 2.00 (Beta)");
	gSetStringValue(ctl, buf);

	/*****
	time( &long_time );                // Get time as long integer. 
	tmTime = localtime( &long_time );  // Convert to local time. 
	strcpy(buf, asctime(tmTime));
	buf[24]=' ';
	buf[25] = '\0';
	*****/

	ctl = mAddControl(dlg, StaticControl, IDC_DATE);
	gSetStringValue(ctl, __DATE__);

	ctl = mAddControl(dlg, StaticControl, IDC_COPYRIGHT);
	strcpy(buf, "");
	gSetStringValue(ctl, buf);

	gPerform(dlg);

	gDispose(dlg);

	return 0L;
}


static	long	window_tool(object wind)
{
	object	dlg, ctl;
	int	   end;
	CTLTYPE_RECT_t	v;

	if (!windBorderRect){
		//look to see if I have this control from a load
		object	ctllist = gGetControls(wind);
		object	ctl, seq;	
		if (ctllist)
			for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) 
				if (!strcmp(gName(ctl),"WINDBORDERRECT"))
					windBorderRect=ctl;
		if (!windBorderRect) {  //window rect doesn't exist, so make it
			windBorderRect = gAddRectControl2(wind, 0, 0, &end, "WINDBORDERRECT", "");
			gGetControlParameters(windBorderRect, &v);
			v.frameThickness=2;
			v.frameColor=0;
			v.height=480;
			v.width=640;
		}
		else
			gGetControlParameters(windBorderRect, &v);

		gSetDesignMode(windBorderRect,0);
		gShow(windBorderRect);
	}
	else
		gGetControlParameters(windBorderRect, &v);

	dlg = mNewDialog(ModalDialog, IDD_WINDOW_PROPERTIES, wind);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT_EDIT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH_EDIT);
	gSetShortValue(ctl, v.width);

	if (gPerform(dlg)){
		RECT rect;
		v.height = gCtlShortValue(dlg, IDC_HEIGHT_EDIT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH_EDIT);

		gSetControlParameters(windBorderRect, &v);

		gGetWindowRect(windBorderRect, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(windBorderRect)), &rect, FALSE);
	}

	gDispose(dlg);

	return 0L;
}



static	void	set_mouse_functions(int flg)
{
	gSetMouseFunction(Control, MK_RBUTTON, flg ? dispatch_mouse_click : NULL);
}



// Yanghui:

static	long	toggleTheRulerAndGrid(object wind, unsigned bm)
{
	gToggleTheRulerAndGrid(wind);
	return 0L;
}



static	long	rect_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	ctl = gAddRectControl2(wind, 0, 0, &end, "RECT", "RECT");
	
	font = getCurrentFont("RectControl");
	
	if (font)
		gSetFont(ctl, font);
	// gSetStringValue(ctl, "Rect Control");
	gSetStringValue(ctl, "");
	gShow(ctl);
	gSetFocus(ctl);

	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);
	
	rect.right = rect.left + 120;
	rect.bottom = rect.top + 40;

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOZORDER); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 

	return 0;
}


static	long	line_horz_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl;

	RECT   rect;
	int    dx, dy;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	ctl = gAddLineControl2(wind, 0, 0, &end, "LINE", "LINE");
	
	gShow(ctl);
	gSetFocus(ctl);

	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOZORDER); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 

	return 0;
}


static	long	line_vert_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl;

	RECT   rect;
	int    dx, dy;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	ctl = gAddLineControl2(wind, 0, 0, &end, "LINE", "LINE");
	
	gShow(ctl);
	gSetFocus(ctl);

	GetWindowRect(gHandle(ctl), &rect);
	dy = rect.right - rect.left;
	rect.right = rect.left + 1;
	rect.bottom = rect.top + dy;

	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOZORDER); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 

	return 0;
}


static	long	image_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl;

	RECT   rect;
	int    dx, dy;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	ctl = gAddImageControl2(wind, 0, 0, &end, "BITMAP", "BITMAP");
	
	gShow(ctl);
	gSetFocus(ctl);

	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOZORDER); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 

	return 0;
}


static	long	generic_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl;

	RECT   rect;
	int    dx, dy;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	ctl = gAddGenericControl2(wind, 0, 0, &end, "Generic", "Generic");
	
	gShow(ctl);
	gSetFocus(ctl);

	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOZORDER); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 

	return 0;
}


// Yanghui
static	long	static_tool(object wind, unsigned bm)
{
	RECT    rectMFMargins;    // used foe test metafile
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	/**********
	// the following is an example to fit the metafile on the full screen

	rectMFMargins.left = -44;
	rectMFMargins.top = 0;

	rectMFMargins.right = -40;
	rectMFMargins.bottom = 0;
	
	gSetMFMargins(wind, &rectMFMargins);
	gSetMFCxCyScale(wind, 1.06, 1.12);    

	gLoadGUI(wind, "c:/yqiu/form/9fsdr71.pmf", "c:/yqiu/form/9fsdr71.cld");

	//gLoadGUI(wind, "c:/yqiu/form/9fsdr71.pmf", NULL);

	return 0L;
	**********/
	
	// 	ctl = gAddStaticTextControl(wind, 0, 0, &end, "STATIC", "STATIC");
	ctl = gAddStaticTextControl2(wind, 0, 0, &end, "STATIC", "STATIC");
	// Yanghui
	
	font = getCurrentFont("StaticText");
	
	if (font)
		gSetFont(ctl, font);
	gSetStringValue(ctl, "Static");
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	text_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	//*****************
	// ctl = gAddTextControl(wind, 0, 0, 20, &end, "Text");
	ctl = gAddTextControl2(wind, 0, 0, 20, &end, "Text");
	// Yanghui
	
	font = getCurrentFont("Text");
	if (font)
		gSetFont(ctl, font);
	gSetStringValue(ctl, "Text");
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	numeric_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// 	ctl = gAddNumericControl(wind, 0, 0, 20, &end, "Numeric");
	ctl = gAddNumericControl2(wind, 0, 0, 20, &end, "Numeric");
	// Yanghui

	font = getCurrentFont("Numeric");

	if (font)
		gSetFont(ctl, font);
	gSetShortValue(ctl, 1);
	
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	date_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// ctl = gAddDateControl(wind, 0, 0, 20, &end, "Date");
	ctl = gAddDateControl2(wind, 0, 0, 20, &end, "Date");
	// Yanghui

	font = getCurrentFont("Date");
	if (font)
		gSetFont(ctl, font);
	gSetLongValue(ctl, 19990101L);
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	time_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// ctl = gAddDateControl(wind, 0, 0, 20, &end, "Date");
	ctl = gAddTimeControl2(wind, 0, 0, 20, &end, "Time");
	// Yanghui

	font = getCurrentFont("Time");
	if (font)
		gSetFont(ctl, font);
	gSetLongValue(ctl, 130000000L);
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	push_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// ctl = gAddPushButton(wind, 0, 0, 10, &end, NULL, "Push", "Push");
	ctl = gAddPushButton2(wind, 0, 0, 10, &end, NULL, "Push", "Push");
	// Yanghui

	font = getCurrentFont("PushButton");

	if (font)
		gSetFont(ctl, font);
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}


static	long	radio_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	//  ctl = gAddRadioButton(wind, 0, 0, 10, &end, "Radio", "Radio", NULL);
	ctl = gAddRadioButton2(wind, 0, 0, 10, &end, "Radio", "Radio", NULL);
	// Yanghui

	font = getCurrentFont("RadioButton");

	if (font)
		gSetFont(ctl, font);
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	check_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	//  ctl = gAddCheckBox(wind, 0, 0, 10, &end, "Check", "Check");
	ctl = gAddCheckBox2(wind, 0, 0, 10, &end, "Check", "Check");
	// Yanghui

	font = getCurrentFont("CheckBox");
	if (font)
		gSetFont(ctl, font);
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	listbox_tool(object wind, unsigned bm)
{
	int	   end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// ctl = gAddListBox(wind, 0, 0, 4, 10, &end, "List");
	ctl = gAddListBox2(wind, 0, 0, 4, 10, &end, "List");
	// Yanghui

	font = getCurrentFont("ListBox");

	if (font)
		gSetFont(ctl, font);
	gAddOption(ctl, "Listbox");
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	GetWindowRect(gHandle(ctl), &rect);
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	combobox_tool(object wind, unsigned bm)
{
	int	end;
	object ctl, font;

	RECT   rect;
	int    dx, dy;

	// Yanghui:
	if (gModifyChildren(wind)==0) 
		return 0L; 
	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window

	// 	ctl = gAddComboBox(wind, 0, 0, 4, 10, &end, "Combo");
	ctl = gAddComboBox2(wind, 0, 0, 4, 10, &end, "Combo");
	// Yanghui

	font = getCurrentFont("ComboBox");

	if (font)
		gSetFont(ctl, font);
 	gAddOption(ctl, "Combobox");
	gShow(ctl);
	gSetFocus(ctl);

	// Yanghui:
	SendMessage(gHandle(ctl), CB_GETDROPPEDCONTROLRECT, 0, (LPARAM)((LPRECT)&rect));
	dx = CHANDLESIZE - rect.left;
	dy = CHANDLESIZE - rect.top;
	OffsetRect(&rect, dx, dy);

	SetWindowPos(gHandle(ctl), (HWND)0,  rect.left, rect.top, 0, 0, SWP_NOSIZE); 
	// put the control in the left corner

	gSetWindowRect(ctl, &rect);  // update the record  
	gUpdateControlVectors(gDialog(ctl), ctl);

	gNewDragWindow(DragWindow, ctl, TRUE); 
	// Yanghui

	return 0;
}

static	long	save_tool(object wind, unsigned bm)
{
	int     n;
	char    cldFile[256], metaFile[256];

	//before I do this check to see if there is new
	//xml format data that would be lost and warn if there is
	//Currently the thing to check is the xpath

	int xmlFormatUsed=0;

	if (gLastLoadType(wind)!=LOAD_TYPE_XML)
	{
		object	ctllist = gGetControls(wind);
		object ctl, seq;	
		if (ctllist)
			for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) 
				if (*gXPathBinding(ctl)) {
					xmlFormatUsed=1;
					gDispose(seq);
					break;
				}
	}


	if (xmlFormatUsed  ||  gLastLoadType(wind)==LOAD_TYPE_XML) {
		int ret=gQuery(wind,"WARNING","You may lose data with the normal save.\nSave as XML?",MB_YESNOCANCEL);

		if (ret==IDCANCEL)
			return 0L;

		xmlFormatUsed=(ret==IDYES);
	}

	gSetModifyChildren(wind, 1);

	if (xmlFormatUsed)
		return export_xml(wind,bm);
	else {
		object  fd = vNew(FileDialog, wind);
		
		//gSetModifyChildren(wind, 1);
		gAppendFilter(fd, "Control Layout Definitions", "*." EXT);
		gDefExt(fd, EXT);
		gSetFile(fd, CLFile);

		if (gGetSaveFile(fd)) {
			strcpy(CLFile, gGetFile(fd));
			if (fileExists(CLFile)) {
				char	tfile[256], *p;
				
				strcpy(tfile, CLFile);
				if (p = strrchr(tfile, '.'))
					*p = '\0';
				strcat(tfile, ".bak");
				if (fileExists(tfile))
					_unlink(tfile);        /* delete the .bak file */
				rename(CLFile, tfile);     /* rename .cld to .bak */
			}
			gSaveControls(wind, CLFile);

			n = strlen(CLFile)-4;
			strncpy(cldFile,  CLFile, n);
			strcpy(cldFile+n, "_.cld");
		
			strncpy(metaFile, CLFile, n);
			strcpy(metaFile+n, "_.pmf");

			gSaveControlsToCldAndMetaFile(wind, cldFile, metaFile);
		}
		gDispose(fd);
	}
	return 0;
}

// Yanghui:
static	long	cut_tool(object wind, unsigned bm)
{
	object  objDWs = gGetDWs(DragWindow);
	if (!wind || !objDWs)  // if no DWs, the appearance of the toolbar Bitmap should not be changed
		return -1L;

	gCLDCopyControls(wind);
	gDeleteDragWindows(DragWindow);
	gEnableToolBitmap(wind, IDB_PASTE, TRUE);
	return 0L;
}


static	long	copy_tool(object wind, unsigned bm)
{
	if (!wind)
		return -1L;

	gCLDCopyControls(wind);
	gEnableToolBitmap(wind, IDB_PASTE, TRUE);
	return 0L;
}


static	long	paste_tool(object wind, unsigned bm)
{
	if (!wind) 
		return -1L;

	gCLDPasteControls(wind);
	return 0L;
}


static	long	undo_tool(object wind, unsigned bm)
{
		gUndoLastDelete(wind);

		return 0L;
}

#undef ctl1
#undef ctl2

static	long	run_test_tool(object wind, unsigned bm)
{
	object   ctl1, ctl2;
	object	 printerObj;
	object   cldObj;
	char     *str1, *str2;
	object   objBitMap;
	object   win;

	object	dlg;
	object	ctl;
	int		r;
	char	title[81];
	HDC		hdc;

	object	font;
	LOGFONT	lf;
	HFONT   hfont, hfontOld;

	DWORD   dwVersion;
	DWORD   dwWindowsMajorVersion;

	HWND    hwnd;

	LONG    lResult;
	HKEY    hKeyResult = 0;
	DWORD   dwDisposition = 0;
	HICON   hicon1 = 0;
	HICON   hicon2 = 0;
	
	DWORD   dwContext=0;
	char    errTmp[128], urlTmp[128];
	DWORD   dwTmp, dwTmp2, dwUrl=128, dwCount=128;
	char    *lpMsgBuf;



	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window
	return 0L; 


}


static	long	run_test_tool2(object wind, unsigned bm)
{
	object   ctl;
	object	 printerObj;
	object   cldObj;
	TCHAR    szLongPathName[_MAX_PATH];
	char     strTmp1[80];
	char     strTmp2[80];

	int      nNumArgs;

	if (gModifyChildren(wind)==0) 
		return 0L; 

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window
	
	GetModuleFileName(gInstance(Application), szLongPathName, _MAX_PATH);

	nNumArgs = gNumArgs(Application);

	strcpy(strTmp1, gGetArg(Application, 0));
	strcpy(strTmp2, gGetArg(Application, 1));

	// deleteRegKey("QYH\\qyh1\\qyh2");
	return 0L;
}


static	long	print_tool(object wind, unsigned bm)
{
	object printerObj=NULL;

	gRmAllOfDWs(DragWindow);  // if the dragging window exists, cancel the dragging window


	printerObj = gQueryPrinter(Printer, wind, "cld"); 
	gPrintScreen(wind, printerObj);              // print almost everything on screen

	if (printerObj)
		gDispose(printerObj);

	return 0L;
}

// Yanghui


static	long	load_tool(object wind, unsigned bm)
{
	object	fd;
	char	file[128];
	int	    len;
	// Yanghui: 
	char *  pChar;  
	object  objMFName;
	// Yanghui
	
	// if the dragging window exists, cancel the dragging window
	gRmAllOfDWs(DragWindow); // Yanghui

	fd = vNew(FileDialog, wind);
	gSetModifyChildren(wind, 1);

	gAppendFilter(fd, "Control Layout Definitions", "*." EXT);
	gAppendFilter(fd, "Windows Meta File", "*.wmf;*.pmf");
	gDefExt(fd, EXT);

	gSetFile(fd, CLFile);
	if (gGetOpenFile(fd)) {
		strcpy(file, gGetFile(fd));
		len = strlen(file);
		if (len > 4)
			if (!stricmp(file+len-4, ".wmf") || !stricmp(file+len-4, ".pmf")) 
				gPlayMetaFile(wind, file);
			else if (!stricmp(file+len-4, "." EXT)) {

				strcpy(CLFile, file);

				new_tool(wind,0);

				gLoadGUI(wind, NULL, CLFile);  // the metafile name object iMFName is loaded
				//gLoadControls(wind, CLFile);  /* the metafile name object iMFName is loaded */

				/* Yanghui: */
				/* get the metafile name from the iClientWind */

				objMFName = gGetMFName(wind);

				if (objMFName) {
					pChar = gStringValue(objMFName);  /* the associated meta file */
				
					if (pChar && (*pChar)) {   /* load the metafile */
						strcpy(file, pChar);
						pChar=NULL;
						gPlayMetaFile(wind, file);
						
						/* get the scaling factor from the iClientWind */
						scalingFactor = gGetMFScale(wind);

						if (scalingFactor>0)
							gSetMFScale(wind, scalingFactor);  
					} else
						scalingFactor = 1.0;
				}
			}
			/* Yanghui */
		
	}
	gDispose(fd);
	return 0;
}

static	long	lang_tool(object wind, unsigned bm)
{
	//switch the application language
	gSetCurrentLanguage(wind,!gLanguage(Application));
	gSetLanguage(Application,!gLanguage(Application));
	return 0;
}

static	long	test_tool(object wind, unsigned bm)
{
	// if the dragging window exists, cancel the dragging window
	gRmAllOfDWs(DragWindow); 

	gSetModifyChildren(wind, !gModifyChildren(wind));
	InvalidateRect(gHandle(wind), NULL, FALSE);

	return 0;
}


static	long	scale_tool(object wind, unsigned bm)
{
	object  dlg = mNewDialog(ModalDialog, IDD_SCALE, wind);
	object  ctl;
	short   nDeleteMetafile;
	int     r;

	ctl = mAddControl(dlg, NumericControl, IDC_FACTOR);
	gNumericRange(ctl, .1, 10.0, 8);
	gSetDoubleValue(ctl, scalingFactor); 

	ctl = mAddControl(dlg, CheckBox, IDC_DELETE_METAFILE);
	gSetShortValue(ctl, 0);
	
	if (!gGetMFName(wind))
		gDisable(ctl);

	if (r = gPerform(dlg)) {
		scalingFactor = gCtlDoubleValue(dlg, IDC_FACTOR);

		nDeleteMetafile = gCtlShortValue(dlg, IDC_DELETE_METAFILE);
		if (nDeleteMetafile) {
			gPlayMetaFile(wind, NULL);
			scalingFactor = 1.0;
		}
		
		gSetMFScale(wind, scalingFactor);  
	}

	gDispose(dlg);

	return 0;
}


static	long	new_tool(object wind, unsigned bm)
{
	RECT rect;
	gEraseAll(wind);

	gSetMFScale(wind, 1.0);
	gSetMFCxCyScale(wind, 1.0, 1.0);
	gSetPrinterScale(wind, GetSystemMetrics(SM_CXSCREEN)/800.0);

	SetRectEmpty(&rect);
	gSetMFMargins(wind, &rect);

	scalingFactor = 1.0;
	return 0;
}

static	long	quit_tool(object wind, unsigned bm)
{
	gQuitApplication(Application, 0);
	return 0;
}


// Yanghui:
static	int	line_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_LINE_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_LINE_t	v;
	int	r;

	// gPutToMostFront(wctl, TRUE); 

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));
	gDisable(ctl);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	// fill:
	ctl = mAddControl(dlg, ComboBox, IDC_LINE_STYLE);
	gSetStyle(ctl, CBS_DROPDOWNLIST);
	gAddOption(ctl, "Solid  ____________");    // Solid
	gAddOption(ctl, "Dot1  . . . . . . . . . . . . .");    // Dot1
	gAddOption(ctl, "Dot2  .  .  .  .  .  .  .  .  .");    // Dot2
	gAddOption(ctl, "Dot3  .   .   .   .   .   .   .");    // Dot3
	gAddOption(ctl, "Dot4  .    .    .    .    .    .");   // Dot4
	gAddOption(ctl, "Dot5  .     .     .     .     .");    // Dot5
	gAddOption(ctl, "Dot6  .      .      .      .     ."); // Dot6
	gAddOption(ctl, "Dash1 ____ ____ ____");        // Dash1
	gAddOption(ctl, "Dash2 ___ ___ ___ ___");       // Dash2
	gAddOption(ctl, "Dash3 ____  ____  ____");      // Dash3
	gAddOption(ctl, "Dash4 ____   ____   ____");    // Dash4
	gAddOption(ctl, "Dash5 ____    ____    ____");  // Dash5

	switch (v.lineStyle) {
	case LS_Dot1:
		gSetStringValue(ctl, "Dot1  . . . . . . . . . . . . .");
		break;
	case LS_Dot2:
		gSetStringValue(ctl, "Dot2  .  .  .  .  .  .  .  .  .");
		break;
	case LS_Dot3:
		gSetStringValue(ctl, "Dot3  .   .   .   .   .   .   .");
		break;
	case LS_Dot4:
		gSetStringValue(ctl, "Dot4  .    .    .    .    .    .");
		break;
	case LS_Dot5:
		gSetStringValue(ctl, "Dot5  .     .     .     .     .");
		break;
	case LS_Dot6:
		gSetStringValue(ctl, "Dot6  .      .      .      .     .");
		break;
	case LS_Dash1:
		gSetStringValue(ctl, "Dash1 ____ ____ ____");
		break;
	case LS_Dash2:
		gSetStringValue(ctl, "Dash2 ___ ___ ___ ___");
		break;
	case LS_Dash3:
		gSetStringValue(ctl, "Dash3 ____  ____  ____");
		break;
	case LS_Dash4:
		gSetStringValue(ctl, "Dash4 ____   ____   ____");
		break;
	case LS_Dash5:
		gSetStringValue(ctl, "Dash5 ____    ____    ____");
		break;
	default:
		gSetStringValue(ctl, "Solid  ____________");
	}

	ctl = mAddControl(dlg, ComboBox, IDC_LINE_COLOR);
	addColorOptions(ctl); 
	setColorStringValue(ctl, v.lineColor);

	r = gPerform(dlg);

	if (r) {
		RECT  rect;
		char  strTmp[80];

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));

		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		if (v.height<1)
			v.height = 1;
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		if (v.width<1)
			v.width = 1;
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);

		// gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		// v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.disabled = 'Y';

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		strcpy(strTmp, gCtlStringValue(dlg, IDC_LINE_STYLE));
		if (stricmp(strTmp, "Dot1  . . . . . . . . . . . . .")==0)
			v.lineStyle = LS_Dot1;
		else if (stricmp(strTmp, "Dot2  .  .  .  .  .  .  .  .  .")==0)
			v.lineStyle = LS_Dot2;
		else if (stricmp(strTmp, "Dot3  .   .   .   .   .   .   .")==0)
			v.lineStyle = LS_Dot3;
		else if (stricmp(strTmp, "Dot4  .    .    .    .    .    .")==0)
			v.lineStyle = LS_Dot4;
		else if (stricmp(strTmp, "Dot5  .     .     .     .     .")==0)
			v.lineStyle = LS_Dot5;
		else if (stricmp(strTmp, "Dot6  .      .      .      .     .")==0)
			v.lineStyle = LS_Dot6;
		else if (stricmp(strTmp, "Dash1 ____ ____ ____")==0)
			v.lineStyle = LS_Dash1;
		else if (stricmp(strTmp, "Dash2 ___ ___ ___ ___")==0)
			v.lineStyle = LS_Dash2;
		else if (stricmp(strTmp, "Dash3 ____  ____  ____")==0)
			v.lineStyle = LS_Dash3;
		else if (stricmp(strTmp, "Dash4 ____   ____   ____")==0)
			v.lineStyle = LS_Dash4;
		else if (stricmp(strTmp, "Dash5 ____    ____    ____")==0)
			v.lineStyle = LS_Dash5;
		else 
			v.lineStyle = LS_Solid;
		
		strcpy(strTmp, gCtlStringValue(dlg, IDC_LINE_COLOR));
		v.lineColor = getColorIndex(strTmp);

		gSetControlParameters(wctl, &v);

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}

	gDispose(dlg);
	gSetFocus(wctl);

	set_mouse_functions(1);

	return 0;
}



static	int	rect_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_RECT_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl1, ctl2, ctl3, ctl4;
	CTLTYPE_RECT_t	v;
	int	r;

	// gPutToMostFront(wctl, TRUE); 

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_3D);
	gSetShortValue(ctl, v.f3D);

	addLanguageChoices(wctl, dlg, 8192);

	ctl1 = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl1, 1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);


	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, TextControl, IDC_FONT_COLOR);
	setColorStringValue(ctl, v.textColor);
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_FONT);
	gSetFunction(ctl, changeSingleControlFontAndTextColor);

	ctl = mAddControl(dlg, RadioButton, IDC_DT_MULTI_LINES);
	ctl1 = mAddControl(dlg, RadioButton, IDC_DT_TOP);
	ctl2 = mAddControl(dlg, RadioButton, IDC_DT_VCENTER);
	ctl3 = mAddControl(dlg, RadioButton, IDC_DT_BOTTOM);

	// Note: default must be DT_TOP
	if ( !(v.DT_Format & DT_SINGLELINE) ) {
		gSetShortValue(ctl, 1);
		gSetShortValue(ctl1, 0);
		gSetShortValue(ctl2, 0);
		gSetShortValue(ctl3, 0);
	} else if (v.DT_Format & DT_VCENTER)  {
		gSetShortValue(ctl, 0);
		gSetShortValue(ctl1, 0);
		gSetShortValue(ctl2, 1);
		gSetShortValue(ctl3, 0);
	} else if (v.DT_Format & DT_BOTTOM) {
		gSetShortValue(ctl, 0);
		gSetShortValue(ctl1, 0);
		gSetShortValue(ctl2, 0);
		gSetShortValue(ctl3, 1);
	} else {
		gSetShortValue(ctl, 0);
		gSetShortValue(ctl1, 1);
		gSetShortValue(ctl2, 0);
		gSetShortValue(ctl3, 0);
	}

	ctl1 = mAddControl(dlg, RadioButton, IDC_DT_LEFT);
	ctl2 = mAddControl(dlg, RadioButton, IDC_DT_CENTER);
	ctl3 = mAddControl(dlg, RadioButton, IDC_DT_RIGHT);

	// Note: default must be DT_LEFT
	if ( (v.DT_Format & DT_CENTER) ) {
		gSetShortValue(ctl1, 0);
		gSetShortValue(ctl2, 1);
		gSetShortValue(ctl3, 0);
	} else if ( (v.DT_Format & DT_RIGHT) ) {
		gSetShortValue(ctl1, 0);
		gSetShortValue(ctl2, 0);
		gSetShortValue(ctl3, 1);
	} else {
		gSetShortValue(ctl1, 1);
		gSetShortValue(ctl2, 0);
		gSetShortValue(ctl3, 0);
	}

	// fill:
	ctl1 = mAddControl(dlg, ComboBox, IDC_FOREGROUND_COLOR);
	addColorOptions(ctl1); 
	setColorStringValue(ctl1, v.patternForeColor);

	ctl2 = mAddControl(dlg, ComboBox, IDC_BACKGROUND_COLOR);
	addColorOptions(ctl2); 
	setColorStringValue(ctl2, v.patternBackColor);

	ctl = mAddControl(dlg, CheckBox, IDC_FILL);
	if (v.fill) 
		gSetShortValue(ctl, 1);
	else
		gSetShortValue(ctl, 0);
	// fill

	// frame:
	ctl = mAddControl(dlg, NumericControl, IDC_FRAME_THICKNESS);
	gSetShortValue(ctl, v.frameThickness);

	ctl = mAddControl(dlg, ComboBox, IDC_FRAME_COLOR);
	addColorOptions(ctl); 
	setColorStringValue(ctl, v.frameColor);

	ctl = mAddControl(dlg, CheckBox, IDC_FRAME_LEFT);
	gSetShortValue(ctl, (v.frameStyle&FRAME_Left));

	ctl = mAddControl(dlg, CheckBox, IDC_FRAME_RIGHT);
	gSetShortValue(ctl, (v.frameStyle&FRAME_Right));

	ctl = mAddControl(dlg, CheckBox, IDC_FRAME_TOP);
	gSetShortValue(ctl, (v.frameStyle&FRAME_Top));

	ctl = mAddControl(dlg, CheckBox, IDC_FRAME_BOTTOM);
	gSetShortValue(ctl, (v.frameStyle&FRAME_Bottom));
	// frame


	r = gPerform(dlg);

	if (r) {
		RECT rect;
		int  nTmp;
		char *pStr;
		char  strTmp[80];

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));

		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);

		// gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		// v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.disabled = 'Y';

		v.f3D = gCtlShortValue(dlg, IDC_3D) ? 1 : 0;

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		// fill:
		v.fill = gCtlShortValue(dlg, IDC_FILL) ? 1 : 0;
		v.dotSize = 0;
		v.stepSize = 0;
		
		strcpy(strTmp, gCtlStringValue(dlg, IDC_FOREGROUND_COLOR));
		v.patternForeColor = getColorIndex(strTmp);

		strcpy(strTmp, gCtlStringValue(dlg, IDC_BACKGROUND_COLOR));
		v.patternBackColor = getColorIndex(strTmp);
		// fill 

		// frame:
	    // v.frameColor;
		strcpy(strTmp, gCtlStringValue(dlg, IDC_FRAME_COLOR));
		v.frameColor = getColorIndex(strTmp);

		v.frameStyle = FRAME_Null;
		if ( gCtlShortValue(dlg, IDC_FRAME_LEFT) )
			v.frameStyle = v.frameStyle | FRAME_Left;
		if ( gCtlShortValue(dlg, IDC_FRAME_RIGHT) )
			v.frameStyle = v.frameStyle | FRAME_Right;
		if ( gCtlShortValue(dlg, IDC_FRAME_TOP) )
			v.frameStyle = v.frameStyle | FRAME_Top;
		if ( gCtlShortValue(dlg, IDC_FRAME_BOTTOM) )
			v.frameStyle = v.frameStyle | FRAME_Bottom;

		nTmp = gCtlShortValue(dlg, IDC_FRAME_THICKNESS);
		if (nTmp<0)
			nTmp = 0;

		// FRAME_flag
		if ( (v.frameStyle&FRAME_Left) || (v.frameStyle&FRAME_Right) )
			if (nTmp > v.width)
				nTmp = v.width;

		if ( (v.frameStyle&FRAME_Left) && (v.frameStyle&FRAME_Right) )
			if (nTmp > v.width/2)
				nTmp = v.width/2.0 + 0.5;

		if ( (v.frameStyle&FRAME_Top) || (v.frameStyle&FRAME_Bottom) )
			if (nTmp > v.height)
				nTmp = v.height;

		if ( (v.frameStyle&FRAME_Top) && (v.frameStyle&FRAME_Bottom) )
			if (nTmp > v.height/2)
			nTmp = v.height/2.0 + 0.5;

		v.frameThickness = nTmp;
		// frame

		// Note: default must be DT_TOP
		v.DT_Format = DT_SINGLELINE;
		if (gCtlShortValue(dlg, IDC_DT_MULTI_LINES))
			v.DT_Format = 0;
		else if (gCtlShortValue(dlg, IDC_DT_VCENTER))
			v.DT_Format = v.DT_Format | DT_VCENTER;
		else if (gCtlShortValue(dlg, IDC_DT_BOTTOM))
			v.DT_Format = v.DT_Format | DT_BOTTOM;
		else 
			v.DT_Format = v.DT_Format | DT_TOP;

		// Note: the default must be DT_LEFT
		if (gCtlShortValue(dlg, IDC_DT_CENTER))
			v.DT_Format = v.DT_Format | DT_CENTER;
		else if (gCtlShortValue(dlg, IDC_DT_RIGHT))
			v.DT_Format = v.DT_Format | DT_RIGHT;
		else 
			v.DT_Format = v.DT_Format | DT_LEFT;

		setLanguageValues(wctl, dlg);

		gSetStringValue(wctl, pStr=gLanguageText(wctl,ENGLISH));
		v.textLength = strlen(pStr) + 1;

		strcpy(strTmp, gCtlStringValue(dlg, IDC_FONT_COLOR));
		v.textColor = getColorIndex(strTmp);

		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));

		gSetControlParameters(wctl, &v);

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}

	gDispose(dlg);
	gSetFocus(wctl);

	set_mouse_functions(1);

	return 0;
}


static	int	image_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_IMAGE_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl1, ctl2, ctl3;
	CTLTYPE_IMAGE_t	v;
	int	r;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_SAVE_TO_META_FILE);
	if (v.notSaveToMetaFile)     /*  not save to meta file  */
		gSetShortValue(ctl, 0);    
	else                         /*  save to meta file  */
		gSetShortValue(ctl, 1);   

	ctl1 = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl1, 1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, TextControl, IDC_BitmapFileName);
	if (!gGetBitmapFileName(wctl))
		gSetStringValue(ctl, "");
	else
		gSetStringValue(ctl, gGetBitmapFileName(wctl));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_LOAD_BITMAP);
	gSetFunction(ctl, loadBitmap);

	ctl = mAddControl(dlg, PushButton, IDC_CLEAR_BITMAP);
	gSetFunction(ctl, clearBitmap);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));

		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);

		// gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		// v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.disabled = 'Y';
		v.notSaveToMetaFile = gCtlShortValue(dlg, IDC_SAVE_TO_META_FILE) ? 0 : 1;  // Yanghui

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetStringValue(wctl, gCtlStringValue(dlg, IDC_BitmapFileName));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}

	gDispose(dlg);
	gSetFocus(wctl);

	set_mouse_functions(1);

	return 0;
}


static	int	changeProperty(object nctl, object dlg)
{
	short newIdx=gShortValue(nctl);
	gSetStringValue(gGetControl(dlg,IDC_PROP_VALUE),gStringValue(gAssocAt(nctl,newIdx)));
	return 0;
}


static	int	generic_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_GENERIC_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl1, ctl2, ctl3, propCtl;
	CTLTYPE_IMAGE_t	v;
	int	r;
	object seq, sit;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, TextControl, IDC_PROP_VALUE);

	ctl = mAddControl(dlg, ComboBox, IDC_CONTROL_TYPE_NAME);
	gSetStringValue(ctl,gStringValue(wctl));
	gAddOption(ctl,"TreeControl");

	propCtl = mAddControl(dlg, ComboBox, IDC_PROPERTY_COMBO);
	gSetChgFunction(propCtl,changeProperty);
	for (seq=gSequence(wctl) ; IsObj(seq) && ( sit = gNext(seq)) ; ) 
		gAddOptionWithObj(propCtl,gStringValue(gKey(sit)),gValue(sit));

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, PushButton, IDC_SAVE_PROP);
	gSetFunction(ctl, saveProperty);

	ctl = mAddControl(dlg, PushButton, IDC_DELETE);
	gSetFunction(ctl, deleteProperty);

	/*
	ctl = mAddControl(dlg, CheckBox, IDC_SAVE_TO_META_FILE);
	if (v.notSaveToMetaFile)     //  not save to meta file 
		gSetShortValue(ctl, 0);    
	else                         //  save to meta file 
		gSetShortValue(ctl, 1);   
*/
	ctl1 = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl1, 1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	r = gPerform(dlg);

	if (r) {
		RECT rect;
		int loop;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));

		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);

		// gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';

	//	v.notSaveToMetaFile = gCtlShortValue(dlg, IDC_SAVE_TO_META_FILE) ? 0 : 1;  // Yanghui
		v.notSaveToMetaFile=0;

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 


		gSetStringValue(wctl,gCtlStringValue(dlg,IDC_CONTROL_TYPE_NAME));

		for (loop=0;loop<gSize(propCtl);loop++) {
			char * prop=gStringValue(gValueAt(propCtl,loop));
			gRemoveStr(wctl,prop);
			gAddStr(wctl,prop,gAssocAt(propCtl,loop));
		}

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}

	gDispose(dlg);
	gSetFocus(wctl);

	set_mouse_functions(1);

	return 0;
}


static void addColorOptions(object ctl) 
{
	gAddOption(ctl, "Aqua");
	gAddOption(ctl, "Black");
	gAddOption(ctl, "Blue");
	gAddOption(ctl, "Fuchsia");
	gAddOption(ctl, "Gray");
	gAddOption(ctl, "Green");
	gAddOption(ctl, "Lime");
	gAddOption(ctl, "Magenta");
	gAddOption(ctl, "Maroon");
	gAddOption(ctl, "Navy");
	gAddOption(ctl, "None");
	gAddOption(ctl, "Olive");
	gAddOption(ctl, "Orange");
	gAddOption(ctl, "Purple");
	gAddOption(ctl, "Red");
	gAddOption(ctl, "Silver");
	gAddOption(ctl, "Teal");
	gAddOption(ctl, "Violet");
	gAddOption(ctl, "White");
	gAddOption(ctl, "Yellow");
}


static void  setColorStringValue(object ctl, char color)
{
	switch (color) {
	case CLR_Aqua:
		gSetStringValue(ctl, "Aqua");
		break;
	case CLR_Black:
		gSetStringValue(ctl, "Black");
		break;
	case CLR_Blue:
		gSetStringValue(ctl, "Blue");
		break;
	case CLR_Fuchsia:
		gSetStringValue(ctl, "Fuchsia");
		break;
	case CLR_Gray:
		gSetStringValue(ctl, "Gray");
		break;
	case CLR_Green:
		gSetStringValue(ctl, "Green");
		break;
	case CLR_Lime:
		gSetStringValue(ctl, "Lime");
		break;
	case CLR_Magenta:
		gSetStringValue(ctl, "Magenta");
		break;
	case CLR_Maroon:
		gSetStringValue(ctl, "Maroon");
		break;
	case CLR_Navy:
		gSetStringValue(ctl, "Navy");
		break;
	case CLR_None:
		gSetStringValue(ctl, "None");
		break;
	case CLR_Olive:
		gSetStringValue(ctl, "Olive");
		break;
	case CLR_Orange:
		gSetStringValue(ctl, "Orange");
		break;
	case CLR_Purple:
		gSetStringValue(ctl, "Purple");
		break;
	case CLR_Red:
		gSetStringValue(ctl, "Red");
		break;
	case CLR_Silver:
		gSetStringValue(ctl, "Silver");
		break;
	case CLR_Teal:
		gSetStringValue(ctl, "Teal");
		break;
	case CLR_Violet:
		gSetStringValue(ctl, "Violet");
		break;
	case CLR_White:
		gSetStringValue(ctl, "White");
		break;
	default:
		gSetStringValue(ctl, "None");
	}
}


static char getColorIndex(const char strTmp[])
{
		if (stricmp(strTmp, "Black")==0)
			return CLR_Black;
		else if (stricmp(strTmp, "Blue")==0)
			return CLR_Blue;
		else if (stricmp(strTmp, "Fuchsia")==0)
			return CLR_Fuchsia;
		else if (stricmp(strTmp, "Gray")==0)
			return CLR_Gray;
		else if (stricmp(strTmp, "Green")==0)
			return CLR_Green;
		else if (stricmp(strTmp, "Lime")==0)
			return CLR_Lime;
		else if (stricmp(strTmp, "Magenta")==0)
			return CLR_Magenta;
		else if (stricmp(strTmp, "Maroon")==0)
			return CLR_Maroon;
		else if (stricmp(strTmp, "Navy")==0)
			return CLR_Navy;
		else if (stricmp(strTmp, "None")==0)
			return CLR_None;
		else if (stricmp(strTmp, "Olive")==0)
			return CLR_Olive;
		else if (stricmp(strTmp, "Orange")==0)
			return CLR_Orange;
		else if (stricmp(strTmp, "Purple")==0)
			return CLR_Purple;
		else if (stricmp(strTmp, "Red")==0)
			return CLR_Red;
		else if (stricmp(strTmp, "Silver")==0)
			return CLR_Silver;
		else if (stricmp(strTmp, "Teal")==0)
			return CLR_Teal;
		else if (stricmp(strTmp, "Violet")==0)
			return CLR_Violet;
		else if (stricmp(strTmp, "White")==0)
			return CLR_White;
		else if (stricmp(strTmp, "Yellow")==0)
			return CLR_Yellow;
		else
			return CLR_None;
}


static void  setColorStringValueFromRGB(object ctl, COLORREF color)
{
	if (color == RGB(0, 255, 255) )
		gSetStringValue(ctl, "Aqua");
	else if (color == RGB(0, 0, 0) )
		gSetStringValue(ctl, "Black");
	else if (color == RGB(0, 0, 255) )
		gSetStringValue(ctl, "Blue");
	else if (color == RGB(255, 0, 255) )
		gSetStringValue(ctl, "Fuchsia");
	else if (color == RGB(128, 128, 128) )
		gSetStringValue(ctl, "Gray");
	else if (color == RGB(0, 128, 0) )
		gSetStringValue(ctl, "Green");
	else if (color == RGB(0, 255, 0) )
		gSetStringValue(ctl, "Lime");
	else if (color == RGB(255, 0, 255) )
		gSetStringValue(ctl, "Magenta");
	else if (color == RGB(128, 0, 0) )
		gSetStringValue(ctl, "Maroon");
	else if (color == RGB(0, 0, 128) )
		gSetStringValue(ctl, "Navy");
	else if (color == RGB(128, 128, 0) )
		gSetStringValue(ctl, "Olive");
	else if (color == RGB(255, 128, 0) )
		gSetStringValue(ctl, "Orange");
	else if (color == RGB(128, 0, 128) )
		gSetStringValue(ctl, "Purple");
	else if (color == RGB(255, 0, 0) )
		gSetStringValue(ctl, "Red");
	else if (color == RGB(192, 192, 192) )
		gSetStringValue(ctl, "Silver");
	else if (color == RGB(0, 128, 128) )
		gSetStringValue(ctl, "Teal");
	else if (color == RGB(128, 0, 128) )
		gSetStringValue(ctl, "Violet");
	else if (color == RGB(255, 255, 255) )
		gSetStringValue(ctl, "White");
	else if (color == RGB(255, 255, 0) )
		gSetStringValue(ctl, "Yellow");
	else
		gSetStringValue(ctl, "None");
}




int getBindingTree(char *type, object parent, object tv)
{
	char buf[BUFFER_SIZE];
	object list;
	object curnode, seq;
	
	if (strstr(type,":"))
		type=strstr(type,":")+1;

	strcpy(buf,"/wsdl:definitions/wsdl:types/xs:schema/xs:complexType[@name=\"");
	strcat(buf,type); 
	strcat(buf,"\"]/xs:sequence/xs:element");

	//get the elements
	if (list=gXPath(wsdlDOM,buf))
		for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
		{
			object child;
			if (child=gFindChildNode(curnode,"xs:complexType"))
			{//TODO: this should be more robust, not assume an array
				//I'll leave it as an array until I find an example where it isn't, though
				//if the element had a complex type, find out it's type
				char *type=gGetAttributeValue(gFindChildNode(gFindChildNode(child,"xs:sequence"),"xs:element"),"type");
				
				object item=gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
				
				//recurse
				getBindingTree(type,item,tv);

			}
			else
			{
				//otherwise, add it to the tree
				gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
			}
		}

	if (!list) //this may be axis, so look in an axis style format, instead of John's
	{
		strcpy(buf,"/wsdl:definitions/wsdl:types/schema/complexType[@name=\"");
		strcat(buf,type); 
		strcat(buf,"\"]//sequence/element");
		if (list=gXPath(wsdlDOM,buf))
			for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
			{
				object child;
				if (child=gFindChildNode(curnode,"complexType"))
				{//TODO: this should be more robust, not assume an array
					//I'll leave it as an array until I find an example where it isn't, though
					//if the element had a complex type, find out it's type
					//ALSO - I have no examples of this in AXIS yet
					char *type=gGetAttributeValue(gFindChildNode(gFindChildNode(child,"sequence"),"element"),"type");
					
					object item=gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
					
					//recurse
					getBindingTree(type,item,tv);

				}
				else
				{
					//otherwise, add it to the tree
					gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
				}
			}
		if (!list) //sheesh, still haven't found anything, may be an array
		{
			object n;
			char *arrayType;
			char *idx;
			object item;
			strcpy(buf,"/wsdl:definitions/wsdl:types/schema/complexType[@name=\"");
			strcat(buf,type); 
			strcat(buf,"\"]/complexContent/restriction/attribute");

			list=gXPath(wsdlDOM,buf);

			if (list)
			{
				n=gFirst(list);

				arrayType=gGetAttributeValue(n,"wsdl:arrayType");

				//strip the namespace off
				idx=arrayType;

				while (*idx)
				{
					if (*idx==':')
						arrayType=idx+1;
					if (*idx=='[')
						*idx=0;
					idx++;
				}
				item=gAddTVItem(tv,parent,arrayType);
				getBindingTree(arrayType,item,tv);
			}
			
		}

		if (!list)
		{
			//maybe I have a document style wsdl
			strcpy(buf,"/wsdl:definitions/wsdl:types/schema/element[@name=\"");
			strcat(buf,type); 
			strcat(buf,"\"]");

			list=gXPath(wsdlDOM,buf);

			if (list)
			{
				for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
				{
					object child;
					if (child=gFindChildNode(curnode,"complexType"))
					{//TODO: this should be more robust, not assume an array
						//I'll leave it as an array until I find an example where it isn't, though
						//if the element had a complex type, find out it's type
						//ALSO - I have no examples of this in AXIS yet
						char *type=gGetAttributeValue(gFindChildNode(gFindChildNode(child,"sequence"),"element"),"type");
						
						object item=gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));

						item=gAddTVItem(tv,item,gGetAttributeValue(gFindChildNode(gFindChildNode(child,"sequence"),"element"),"name"));
						
						//recurse
						getBindingTree(type,item,tv);

					}
					else
					{
						//otherwise, add it to the tree
						gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
					}
				}

			}
		}

		if (!list)  //Maybe it's .NET - this is getting complex because we don't have a schema aware xpath in Dynace
		{
			strcpy(buf,"/wsdl:definitions/wsdl:types/s:schema/s:element[@name=\"");
			strcat(buf,type); 
			strcat(buf,"\"]");
			if (list=gXPath(wsdlDOM,buf))
				for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
				{
					object child;
					if (child=gFindChildNode(curnode,"s:complexType"))
					{//TODO: this should be more robust, not assume an array
						//I'll leave it as an array until I find an example where it isn't, though
						//if the element had a complex type, find out it's type
						//ALSO - I have no examples of this in AXIS yet
						char *type=gGetAttributeValue(gFindChildNode(gFindChildNode(child,"s:sequence"),"s:element"),"type");
						
						object item=gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));

						item=gAddTVItem(tv,item,gGetAttributeValue(gFindChildNode(gFindChildNode(child,"s:sequence"),"s:element"),"name"));
						
						//recurse
						getBindingTree(type,item,tv);

					}
					else
					{
						//otherwise, add it to the tree
						gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
					}
				}

		}
		if (!list)  //Maybe it's .NET - this is getting complex because we don't have a schema aware xpath in Dynace
		{
			strcpy(buf,"/wsdl:definitions/wsdl:types/s:schema/s:complexType[@name=\"");
			strcat(buf,type); 
			strcat(buf,"\"]//s:element");
			if (list=gXPath(wsdlDOM,buf))
				for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
				{
					object child;
					if (child=gFindChildNode(curnode,"s:complexType"))
					{//TODO: this should be more robust, not assume an array
						//I'll leave it as an array until I find an example where it isn't, though
						//if the element had a complex type, find out it's type
						//ALSO - I have no examples of this in AXIS yet
						char *type=gGetAttributeValue(gFindChildNode(gFindChildNode(child,"s:sequence"),"s:element"),"type");
						
						object item=gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));

						item=gAddTVItem(tv,item,gGetAttributeValue(gFindChildNode(gFindChildNode(child,"s:sequence"),"s:element"),"name"));
						
						//recurse
						getBindingTree(type,item,tv);

					}
					else
					{
						//otherwise, add it to the tree
						gAddTVItem(tv,parent,gGetAttributeValue(curnode,"name"));
					}
				}

		}
		if (!list)
		{
			//maybe it was a primative type
			//in that case, the choice is *
			gAddTVItem(tv,parent,"*");
		}
	}

	if (list)
		gDispose(list);

	return 1;
}

static char * withoutNamespace(char *s)
{
	if (s)
	{
		char *p=strstr(s,":");
		if (p)
			return p+1;
	}
	return s;

}

static int getXMLStructure(object ictl, object dlg)
{
	object ctl=gGetControl(dlg,IDC_WSDLOPERATION);
	object tv=gGetControl(dlg,IDC_WSDL_TREE);

	char * op=gStringValue(ctl);
	char buf[BUFFER_SIZE];
	object list;
	object curnode, seq;
	object partCurnode, partSeq;

	gRemoveAll(tv);

	if (wsdlDOM==NULL)
		return 0;

	strcpy(lastOp,op);

	strcpy(buf,"/wsdl:definitions/wsdl:portType/wsdl:operation[@name=\"");
	strcat(buf,op);
	strcat(buf,"\"]/wsdl:output");

	//get the message name
	if (list=gXPath(wsdlDOM,buf))
	{
		for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
		{
			object partList;
			char *name=gGetAttributeValue(curnode,"name");

			if (!name)
				name=withoutNamespace(gGetAttributeValue(curnode,"message"));

			//for every message, get all the message parts
			strcpy(buf,"/wsdl:definitions/wsdl:message[@name=\"");
			strcat(buf,name);
			strcat(buf,"\"]/wsdl:part");
			
			if (partList=gXPath(wsdlDOM,buf))
			{

				for (partSeq = gSequence(partList) ; partCurnode = gNext(partSeq) ; ) 
				{
					//for every part, get the type
					char *type=gGetAttributeValue(partCurnode,"type");

					//for every type get the elements
					if (type)
						getBindingTree(type,NULL,tv);
					else  //this must be document type
					{
						type=withoutNamespace(gGetAttributeValue(partCurnode,"element"));
						if (type)
							getBindingTree(type,NULL,tv);
					}
				}

				gDispose(partList);
			}
		}
		gDispose(list);
	}
	return 1;
}


static int getWSDL(object ictl, object dlg)
{
	object ws;
	int ret=0;

//	char *x="http://localhost:8454/destiny/soap/com.integra.destinyweb.dataHandler.URLA1Data?wsdl";
	object urlCtl=gGetControl(dlg,IDC_URL_COMBO);
	char *urlCtlValue=gStringValue(urlCtl);
	char x[BUFFER_SIZE];

	strcpy(x,urlCtlValue);

	if (strstr(x,"|"))
		strcpy(x,strstr(urlCtlValue,"|")+1);

	ws=gNewHttpRequest(HttpRequest,urlCtlValue,"GET");

	ret = gSendRequest(ws, 0);

	/*
	ws = gNewServiceRequest(WebService, x);
	gSetNamespaceURL(ws, "http://integra-online.com/soap/");
	ret = gSendRequest(ws, 0);
	*/
//	vPrintf(wind, "ret = %d\n%s\n", ret, gGetErrorMessage(ws));

	if (!ret)
	{
		//object dom=gGetXMLResponse(ws);
		object dom=gParseString(XMLNode, gGetResponse(ws), NULL, NULL, NULL);

		object ctl=gGetControl(dlg,IDC_WSDLOPERATION);
		object list=gXPath(dom,"/wsdl:definitions/wsdl:portType/wsdl:operation");

		object curnode, seq;
		int loop;
		short found=0;

		if(wsdlDOM)
		{
			gDispose(wsdlDOM);
			wsdlDOM=NULL;
		}
		for (loop=0;loop<gSize(urlCtl);loop++)
		{	
			if (!strcmp(gStringValue(gValueAt(urlCtl,loop)),urlCtlValue))
				found=1;
		}
		
		if (!found)
		{
			gAddOption(urlCtl,gStringValue(urlCtl));
			gAddFirst(wsdlUrlList,gNewWithObj(LinkValue,gNewWithStr(String,urlCtlValue)));
			saveWSDLList();
		}

		strcpy(lastURL,gStringValue(urlCtl));

		wsdlDOM=dom;

		gRemoveAll(ctl);

		if (!list)
			return 0;

		for (seq = gSequence(list) ; curnode = gNext(seq) ; ) 
			gAddOption(ctl,gGetAttributeValue(curnode,"name"));

		gDispose(list);

	}

	gDispose(ws);

	return ret;
}

char xpath[BUFFER_SIZE];

static	int	selectXPath(object ctl, object ftvi, object ttvi)
{
	char temp[BUFFER_SIZE];
	char temp2[BUFFER_SIZE];
	object current=ttvi;
	xpath[0]=0;
	//strcpy(xpath,"/*");
	temp2[0]=0;
	temp2[1]=0;

	while(current)
	{
		strcpy(temp,"/");
		strcat(temp,gStringValue(current));
		
		strcat(temp,temp2);
		
		strcpy(temp2,temp);

		current=gGetParent(current);
	}

	strcat(xpath,temp2+1);
//	strcat(xpath,"/text()");

	gSelect(ttvi);
	return 0;
}


static object getXPathDialog(object ictl)
{
	object tv,item,seq,opt;
	object urlCtl, buttonCtl;

	object	xpdlg = mNewDialog(ModalDialog, IDD_XPATH_BINDING, gGetParent(ictl));

	object ctl = mAddControl(xpdlg, ComboBox, IDC_WSDLOPERATION);
	gSetChgFunction(ctl,getXMLStructure);
	gSetStringValue(ctl,lastOp);

	buttonCtl = mAddControl(xpdlg, PushButton, IDC_LOOKUP);
	gSetFunction(buttonCtl,getWSDL);

	urlCtl = mAddControl(xpdlg, ComboBox, IDC_URL_COMBO);
	gSetChgFunction(urlCtl,getWSDL);


	if (wsdlUrlList)
		for (seq = gSequence(wsdlUrlList) ; opt = gNext(seq) ; ) 
			gAddOption(urlCtl,gStringValue(gValue(opt)));

	tv = mAddControl(xpdlg, TreeView, IDC_WSDL_TREE);
	gAlphabetize(tv);
	gSetSelFun(tv,selectXPath);


	gSetStringValue(urlCtl,lastURL);

	getWSDL(urlCtl,xpdlg);
	getXMLStructure(ctl,xpdlg);

	return xpdlg;

}

static int getXPathViaLookup(object ictl, object dlg)
{
	int r;
	
	object xpdlg=getXPathDialog(ictl);

	r = gPerform(xpdlg);

	if (r)
	{
		object  xpctl = mGetControl(dlg, IDC_XPATH);
		object urlCtl = gGetControl(xpdlg,IDC_URL_COMBO);
		object opCtl = gGetControl(xpdlg,IDC_WSDLOPERATION);

		char t[BUFFER_SIZE];
		char *url=gStringValue(urlCtl);

		t[0]=0;

		if (strstr(url,"|")) 
		{
			//you could have all calls to the same url have the same
			//data name, but I recommend the other option below...
			strcpy(t,url);
			*strstr(t,"|")=0;
			strcat(t,":");
		}
		else
		{
			strcat(t,gStringValue(opCtl));
			strcat(t,":/");
		}

		strcat(t,xpath);
		

		gSetStringValue(xpctl, t);
	}
	
	gDispose(xpdlg);
	

	return 0;
}


static int getTextXPathViaLookup(object ictl, object dlg)
{
	object xpdlg=getXPathDialog(ictl);


	if (gPerform(xpdlg))
		gSetStringValue(mGetControl(dlg, IDC_TEXT_XPATH), xpath);

	
	gDispose(xpdlg);
	

	return 0;
}
static int getDataXPathViaLookup(object ictl, object dlg)
{
	object xpdlg=getXPathDialog(ictl);


	if (gPerform(xpdlg))
		gSetStringValue(mGetControl(dlg, IDC_DATA_XPATH), xpath);
	
	
	gDispose(xpdlg);
	

	return 0;
}
static int getChoiceXPathViaLookup(object ictl, object dlg)
{
	int r;

	object xpdlg=getXPathDialog(ictl);

	r = gPerform(xpdlg);

	if (r)
	{
		object  xpctl = mGetControl(dlg, IDC_CHOICE_XPATH);
		object urlCtl = gGetControl(xpdlg,IDC_URL_COMBO);
		object	opCtl = gGetControl(xpdlg,IDC_WSDLOPERATION);

		char t[BUFFER_SIZE];
		char *url=gStringValue(urlCtl);

		t[0]=0;

		if (strstr(url,"|"))
		{
			strcpy(t,url);
			*strstr(t,"|")=0;
			strcat(t,":/");
		}
		else
		{
			strcat(t,gStringValue(opCtl));
			strcat(t,":/");
		}
		strcat(t,xpath);
		

		gSetStringValue(xpctl, t);
	}
	
	gDispose(xpdlg);
	

	return 0;
}
static int getListXPathViaLookup(object ictl, object dlg)
{
	object xpdlg=getXPathDialog(ictl);

	if (gPerform(xpdlg))
		gSetStringValue(mGetControl(dlg, IDC_LIST_XPATH), xpath);

	gDispose(xpdlg);
	

	return 0;
}

static	int	static_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_STATIC_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_STATIC_t	v;
	int	r;
	char *p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);


	ctl = mAddControl(dlg, TextControl, IDC_URL);
	gTextRange(ctl, 0, 256);
	gSetStringValue(ctl, gURL(wctl));


	addLanguageChoices(wctl, dlg, 8192);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_RIGHT);
	gSetShortValue(ctl, v.right == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_SAVE_TO_META_FILE);
	if (v.filler)  /* Yanghui: not save to meta file  */
		gSetShortValue(ctl, 0);    
	else 
		gSetShortValue(ctl, 1);   

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.right = gCtlShortValue(dlg, IDC_RIGHT) ? 'Y' : 'N';

		v.filler = gCtlShortValue(dlg, IDC_SAVE_TO_META_FILE) ? 0 : 1;  // Yanghui

		gSetControlParameters(wctl, &v);
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));

		setLanguageValues(wctl, dlg);
		gSetStringValue(wctl, gLanguageText(wctl,ENGLISH));
		gSetURL(wctl, gCtlStringValue(dlg,IDC_URL));

		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));
		
		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	text_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_TEXT_PROPERTIES, gGetParent(wctl));
	object	ctl, obj, ctl2, ctl3;
	CTLTYPE_TEXT_t	v;
	char	*p;
	int	r;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);
	
	ctl = mAddControl(dlg, NumericControl, IDC_MINLEN);
	gSetShortValue(ctl, v.minLen);

	ctl = mAddControl(dlg, NumericControl, IDC_MAXLEN);
	gSetShortValue(ctl, v.maxLen);

	ctl = mAddControl(dlg, CheckBox, IDC_CAPITALIZE);
	gSetShortValue(ctl, v.capitalize);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_PASSWORD);
	gSetShortValue(ctl,gPasswordFlag(wctl));

	ctl = mAddControl(dlg, CheckBox, IDC_MULTILINE);                      // Yanghui
	gSetShortValue(ctl, (v.textCtlStyle & ES_MULTILINE)==ES_MULTILINE);   // Yanghui

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, TextControl, IDC_DEFAULT);
	obj = gGetDefault(wctl);
	if (obj)
		gSetValue(ctl, obj);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);
	

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.minLen = gCtlShortValue(dlg, IDC_MINLEN);
		v.maxLen = gCtlShortValue(dlg, IDC_MAXLEN);
		v.capitalize = gCtlShortValue(dlg, IDC_CAPITALIZE);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		gSetPasswordFlag(wctl,gCtlShortValue(dlg, IDC_PASSWORD));

		v.textCtlStyle = gCtlShortValue(dlg, IDC_MULTILINE) ? ES_MULTILINE|ES_AUTOVSCROLL : ES_AUTOHSCROLL;  // Yanghui

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetDefaultStr(wctl, gCtlStringValue(dlg, IDC_DEFAULT));
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}


// Change a window's style
static  BOOL modifyStyle(HWND hWnd, DWORD dwRemove, DWORD dwAdd, UINT nFlags)
{
	DWORD dwStyle, dwNewStyle;
	if (hWnd==NULL)
		return FALSE;

	dwStyle = GetWindowLong(hWnd, GWL_STYLE);
	dwNewStyle = (dwStyle & ~dwRemove) | dwAdd;
	if (dwStyle == dwNewStyle)
		return FALSE;

	SetWindowLong(hWnd, GWL_STYLE, dwNewStyle);
	if (nFlags != 0)
	{
		SetWindowPos(hWnd, NULL, 0, 0, 0, 0,
			SWP_NOSIZE | SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE | nFlags);
	}
	return TRUE;
}

static	int	numeric_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_NUMERIC_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_NUMERIC_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_RIGHT);
	gSetShortValue(ctl, v.right == 'Y');

	ctl = mAddControl(dlg, NumericControl, IDC_MINIMUM);
	gNumericRange(ctl, -9999999999999.9, 9999999999999.9, 1);
	gSetDoubleValue(ctl, v.minimum);

	ctl = mAddControl(dlg, NumericControl, IDC_MAXIMUM);
	gNumericRange(ctl, -9999999999999.9, 9999999999999.9, 1);
	gSetDoubleValue(ctl, v.maximum);

	ctl = mAddControl(dlg, NumericControl, IDC_DP);
	gSetShortValue(ctl, v.dp);

	ctl = mAddControl(dlg, TextControl, IDC_FORMAT);
	gTextRange(ctl, 0, sizeof(v.format)-1);
	gSetStringValue(ctl, v.format);
	gCapitalize(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FIELDWIDTH);
	gSetShortValue(ctl, v.fldWidth);

	ctl = mAddControl(dlg, NumericControl, IDC_DEFAULT);
	gSetDoubleValue(ctl, v.defaultVal);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.right = gCtlShortValue(dlg, IDC_RIGHT) ? 'Y' : 'N';
		v.minimum = gCtlDoubleValue(dlg, IDC_MINIMUM);
		v.maximum = gCtlDoubleValue(dlg, IDC_MAXIMUM);
		v.dp = gCtlShortValue(dlg, IDC_DP);
		strcpy(v.format, gCtlStringValue(dlg, IDC_FORMAT));
		v.fldWidth = gCtlShortValue(dlg, IDC_FIELDWIDTH);
		v.defaultVal = gCtlDoubleValue(dlg, IDC_DEFAULT);

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	date_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_DATE_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_DATE_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, DateControl, IDC_MINIMUM);
	gSetLongValue(ctl, v.minimum);

	ctl = mAddControl(dlg, DateControl, IDC_MAXIMUM);
	gSetLongValue(ctl, v.maximum);

	ctl = mAddControl(dlg, TextControl, IDC_FORMAT);
	gTextRange(ctl, 0, sizeof(v.format)-1);
	gSetStringValue(ctl, v.format);

	ctl = mAddControl(dlg, DateControl, IDC_DEFAULT);
	gSetLongValue(ctl, v.defaultVal);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.minimum = gCtlLongValue(dlg, IDC_MINIMUM);
		v.maximum = gCtlLongValue(dlg, IDC_MAXIMUM);
		strcpy(v.format, gCtlStringValue(dlg, IDC_FORMAT));
		v.defaultVal = gCtlLongValue(dlg, IDC_DEFAULT);

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	time_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_TIME_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_TIME_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, TimeControl, IDC_MINIMUM);
	gSetLongValue(ctl, v.minimum);

	ctl = mAddControl(dlg, TimeControl, IDC_MAXIMUM);
	gSetLongValue(ctl, v.maximum);

	ctl = mAddControl(dlg, TextControl, IDC_FORMAT);
	gTextRange(ctl, 0, sizeof(v.format)-1);
	gSetStringValue(ctl, v.format);

	ctl = mAddControl(dlg, TimeControl, IDC_DEFAULT);
	gSetLongValue(ctl, v.defaultVal);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.minimum = gCtlLongValue(dlg, IDC_MINIMUM);
		v.maximum = gCtlLongValue(dlg, IDC_MAXIMUM);
		strcpy(v.format, gCtlStringValue(dlg, IDC_FORMAT));
		v.defaultVal = gCtlLongValue(dlg, IDC_DEFAULT);

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	push_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_PUSHBUTTON_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_PUSHBUTTON_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	addLanguageChoices(wctl, dlg, 8192);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	// gSetFunction(ctl, changeSingleControlFont);
	gSetFunction(ctl, changeSingleControlFontAndTextColor);  // Yanghui

	r = gPerform(dlg);

	if (r) {
		RECT rect;
		char  strTmp[80];

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gBackBrush(wctl, vNew(SolidBrush, 255, 0, 0));
		gTextBrush(wctl, vNew(SolidBrush, 0, 255, 0));

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));
		setLanguageValues(wctl, dlg);

		gSetStringValue(wctl,gStringValue(gAssocAt(gGetControl(dlg,IDC_LANGUAGESELECT),ENGLISH)));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	radio_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_RADIOBUTTON_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_RADIOBUTTON_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, TextControl, IDC_NEXTID);
	gTextRange(ctl, 1, sizeof(v.next)-1);
	gSetStringValue(ctl, v.next);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DEFAULT);
	gSetShortValue(ctl, v.defaultVal);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	addLanguageChoices(wctl, dlg, 256);

	r = gPerform(dlg);

	if (r) {
		RECT rect;
		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		strcpy(v.next, gCtlStringValue(dlg, IDC_NEXTID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.defaultVal = gCtlShortValue(dlg, IDC_DEFAULT);
		setLanguageValues(wctl, dlg);

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetTitle(wctl, gLanguageText(wctl,ENGLISH));
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	void setLanguageValues(object ctl, object dlg)
{
	int loop;
	for (loop=0;loop<MAX_LANGUAGES;loop++) 
			gSetLanguageText(ctl, loop,
				gStringValue(gAssocAt(gGetControl(dlg,IDC_LANGUAGESELECT),loop)));
}

static void addLanguageChoices(object wctl, object dlg, int size)
{
	int loop;
	char	**languages=gLanguages(Application);
	object ctl = mAddControl(dlg, TextControl, IDC_LANGUAGETEXT);
	gAddHandlerAfter(ctl, (unsigned) WM_KILLFOCUS, saveLanguageChoice);
	gTextRange(ctl, 0, size);
	
	ctl = mAddControl(dlg, ComboBox, IDC_LANGUAGESELECT);
	gSetChgFunction(ctl, changeLanguage);

	for (loop=0;loop<MAX_LANGUAGES;loop++)  
		gInsertOptionWithObj(ctl, loop, languages[loop],gNewWithStr(String,gLanguageText(wctl,loop)));

	gSetDefaultInt(ctl,0);
	gUseDefault(ctl);
	changeLanguage(ctl,dlg);
}
static	int	check_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_CHECKBOX_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_CHECKBOX_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');


	ctl = mAddControl(dlg, CheckBox, IDC_DEFAULT);
	gSetShortValue(ctl, v.defaultVal);

	ctl = mAddControl(dlg, TextControl, IDC_ONSTRING);
	gTextRange(ctl, 1, 256);
	gSetShortValue(wctl, 1);
	gSetStringValue(ctl, gStringValue(wctl));

	ctl = mAddControl(dlg, TextControl, IDC_OFFSTRING);
	gTextRange(ctl, 1, 256);
	gSetShortValue(wctl, 0);
	gSetStringValue(ctl, gStringValue(wctl));

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	addLanguageChoices(wctl, dlg, 256);
	
	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		v.defaultVal = gCtlShortValue(dlg, IDC_DEFAULT);

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetStringValues(wctl, gCtlStringValue(dlg, IDC_ONSTRING), gCtlStringValue(dlg, IDC_OFFSTRING));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));

		setLanguageValues(wctl, dlg);
		gSetTitle(wctl, gLanguageText(wctl,ENGLISH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	listbox_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_LISTBOX_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_LISTBOX_t	v;
	int	r;
	char	*p;
	int     cyChar;  /* Yanghui */

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_USE_TABSTOPS);
	gSetShortValue(ctl, v.type == (short)LBS_USETABSTOPS);

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_CHOICE_XPATH);
	if (p = gChoiceXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_LIST_XPATH);
	if (p = gListKeyXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_DATA_XPATH);
	if (p = gDataXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_TEXT_XPATH);
	if (p = gTextXPathBinding(wctl))
		gSetStringValue(ctl, p);

	
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_CHOICE);
	gSetFunction(ctl, getChoiceXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_DATA);
	gSetFunction(ctl, getDataXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_LIST);
	gSetFunction(ctl, getListXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_TEXT);
	gSetFunction(ctl, getTextXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);

		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';

		v.type = gCtlShortValue(dlg, IDC_USE_TABSTOPS) ? (short)LBS_USETABSTOPS : 0;

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 

		/* gSetControlParameters(wctl, &v); */ /* Yanghui */
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
		
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));
		gSetChoiceXPathBinding(wctl,gCtlStringValue(dlg,IDC_CHOICE_XPATH));
		gSetListKeyXPathBinding(wctl,gCtlStringValue(dlg,IDC_LIST_XPATH));
		gSetDataXPathBinding(wctl,gCtlStringValue(dlg,IDC_DATA_XPATH));
		gSetTextXPathBinding(wctl,gCtlStringValue(dlg,IDC_TEXT_XPATH));

		/* Yanghui: */
		/* it is better to set the font first, when the font needs bigger height, 
		   then make the adjustment */

		cyChar = gLineHeight(gGetFont(wctl)); 
		if (v.height < cyChar) v.height=cyChar;
		gSetControlParameters(wctl, &v);

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
		/* Yanghui */
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	combobox_rbutton(object wctl, unsigned button)
{
	object	dlg = mNewDialog(ModalDialog, IDD_COMBOBOX_PROPERTIES, gGetParent(wctl));
	object	ctl, ctl2, ctl3;
	CTLTYPE_COMBOBOX_t	v;
	int	r;
	char	*p;

	set_mouse_functions(0);
	gGetControlParameters(wctl, &v);

	ctl = mAddControl(dlg, TextControl, IDC_CTLID);
	gTextRange(ctl, 1, sizeof(v.name)-1);
	gSetStringValue(ctl, v.name);

	ctl = mAddControl(dlg, NumericControl, IDC_TABORDER);
	gSetShortValue(ctl, gGetTabOrder(wctl));

	ctl = mAddControl(dlg, NumericControl, IDC_HEIGHT);
	gSetShortValue(ctl, v.height);

	ctl = mAddControl(dlg, NumericControl, IDC_WIDTH);
	gSetShortValue(ctl, v.width);

	ctl = mAddControl(dlg, NumericControl, IDC_YPOS);
	gSetShortValue(ctl, v.yPos);

	ctl = mAddControl(dlg, NumericControl, IDC_XPOS);
	gSetShortValue(ctl, v.xPos);

	ctl = mAddControl(dlg, CheckBox, IDC_HIDE);
	gSetShortValue(ctl, v.hidden == 'Y');

	ctl = mAddControl(dlg, CheckBox, IDC_DISABLE);
	gSetShortValue(ctl, v.disabled == 'Y');

	ctl  = mAddControl(dlg, RadioButton, IDC_POSITION_NOCHANGE);
	ctl2 = mAddControl(dlg, RadioButton, IDC_POSITION_FRONT);
	ctl3 = mAddControl(dlg, RadioButton, IDC_POSITION_BACK);
	gSetShortValue(ctl,  1);
	gSetShortValue(ctl2, 0);
	gSetShortValue(ctl3, 0);

	ctl = mAddControl(dlg, TextControl, IDC_HELPTOPIC);
	if (p = gGetTopic(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_XPATH);
	if (p = gXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_CHOICE_XPATH);
	if (p = gChoiceXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_LIST_XPATH);
	if (p = gListKeyXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_DATA_XPATH);
	if (p = gDataXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_TEXT_XPATH);
	if (p = gTextXPathBinding(wctl))
		gSetStringValue(ctl, p);

	ctl = mAddControl(dlg, TextControl, IDC_FontName);
	gSetStringValue(ctl, gName(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, NumericControl, IDC_FontSize);
	gSetShortValue(ctl, gPointSize(gGetFont(wctl)));
	gDisable(ctl);

	ctl = mAddControl(dlg, PushButton, IDC_ChangeFont);
	gSetFunction(ctl, changeSingleControlFont);

	// Yanghui:
	ctl = mAddControl(dlg, RadioButton, IDC_CBS_SIMPLE);
	if (v.type == CBS_SIMPLE)
		gSetShortValue(ctl, 1);
	else
		gSetShortValue(ctl, 0);

	ctl = mAddControl(dlg, RadioButton, IDC_CBS_DROPDOWN);
	if (v.type == CBS_DROPDOWN)
		gSetShortValue(ctl, 1);
	else
		gSetShortValue(ctl, 0);

	ctl = mAddControl(dlg, RadioButton, IDC_CBS_DROPDOWNLIST);
	if ((v.type & CBS_DROPDOWNLIST) == CBS_DROPDOWNLIST)
		gSetShortValue(ctl, 1);
	else
		gSetShortValue(ctl, 0);

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON);
	gSetFunction(ctl, getXPathViaLookup);
	

	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_CHOICE);
	gSetFunction(ctl, getChoiceXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_DATA);
	gSetFunction(ctl, getDataXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_LIST);
	gSetFunction(ctl, getListXPathViaLookup);
	ctl = mAddControl(dlg, PushButton, IDC_XPATH_BUTTON_TEXT);
	gSetFunction(ctl, getTextXPathViaLookup);

	r = gPerform(dlg);

	if (r) {
		RECT rect;

		strcpy(v.name, gCtlStringValue(dlg, IDC_CTLID));
		v.height = gCtlShortValue(dlg, IDC_HEIGHT);
		v.width = gCtlShortValue(dlg, IDC_WIDTH);
		v.yPos = gCtlShortValue(dlg, IDC_YPOS);
		v.xPos = gCtlShortValue(dlg, IDC_XPOS);
		v.hidden = gCtlShortValue(dlg, IDC_HIDE) ? 'Y' : 'N';
		v.disabled = gCtlShortValue(dlg, IDC_DISABLE) ? 'Y' : 'N';
		
		// Yanghui:
		if (gCtlShortValue(dlg, IDC_CBS_SIMPLE))
			v.type = CBS_SIMPLE;
		else if (gCtlShortValue(dlg, IDC_CBS_DROPDOWN))
			v.type = CBS_DROPDOWN;
		else
			v.type = CBS_DROPDOWNLIST;    // default to CBS_DROPDOWNLIST

		if ( gCtlShortValue(dlg, IDC_POSITION_FRONT) )
			gPutToMostFront(wctl, TRUE); 
		else if (gCtlShortValue(dlg, IDC_POSITION_BACK))
			gPutToMostFront(wctl, FALSE); 
			
		// Yanghui

		gSetControlParameters(wctl, &v);
		gSetTopic(wctl, gCtlStringValue(dlg, IDC_HELPTOPIC));
		gSetTabOrder(wctl, gCtlShortValue(dlg, IDC_TABORDER));
		gSetFont(wctl, vNew(ExternalFont, gCtlStringValue(dlg, IDC_FontName),
				    gCtlShortValue(dlg, IDC_FontSize)));
	
		gSetXPathBinding(wctl,gCtlStringValue(dlg,IDC_XPATH));
		gSetChoiceXPathBinding(wctl,gCtlStringValue(dlg,IDC_CHOICE_XPATH));
		gSetListKeyXPathBinding(wctl,gCtlStringValue(dlg,IDC_LIST_XPATH));
		gSetDataXPathBinding(wctl,gCtlStringValue(dlg,IDC_DATA_XPATH));
		gSetTextXPathBinding(wctl,gCtlStringValue(dlg,IDC_TEXT_XPATH));

		gGetWindowRect(wctl, &rect);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(gHandle(wctl)), &rect, FALSE);
	}
	gDispose(dlg);
	gSetFocus(wctl);
	set_mouse_functions(1);
	return 0;
}

static	int	dispatch_mouse_click(object ctl, unsigned button)
{
	if (ClassOf(ctl) == GenericControl)
		generic_rbutton(ctl, button);
	else if (ClassOf(ctl) == StaticTextControl)
		static_rbutton(ctl, button);
	else if (ClassOf(ctl) == TextControl)
		text_rbutton(ctl, button);
	else if (ClassOf(ctl) == NumericControl)
		numeric_rbutton(ctl, button);
	else if (ClassOf(ctl) == DateControl)
		date_rbutton(ctl, button);
	else if (ClassOf(ctl) == TimeControl)
		time_rbutton(ctl, button);
	else if (ClassOf(ctl) == PushButton)
		push_rbutton(ctl, button);
	else if (ClassOf(ctl) == RadioButton)
		radio_rbutton(ctl, button);
	else if (ClassOf(ctl) == CheckBox)
		check_rbutton(ctl, button);
	else if (ClassOf(ctl) == ListBox)
		listbox_rbutton(ctl, button);
	else if (ClassOf(ctl) == ComboBox)
		combobox_rbutton(ctl, button);
	else if (ClassOf(ctl) == RectControl) {
		if (strcmp(gName(ctl),"WINDBORDERRECT"))
			rect_rbutton(ctl, button);
	}
	else if (ClassOf(ctl) == LineControl)
		line_rbutton(ctl, button);
	else if (ClassOf(ctl) == ImageControl)
		image_rbutton(ctl, button);
	return 0;
}

static	int	changeSingleControlFont(object ctl, object dlg)
{
	// object  fd = vNew(FontDialog, dlg);
	object  nctl = mGetControl(dlg, IDC_FontName);
	object  pctl = mGetControl(dlg, IDC_FontSize);

	// Yanghui:
	object fd;  
	char * fontName;  // including font FaceName [,Bold, Italic, Underline, Strikeout]
	short  fontPointSize;
	if (nctl)
		fontName = gStringValue(nctl);  // get the font name
	if (pctl)
		fontPointSize = gShortValue(pctl);  // get the font point size
	fd = gNewWithInitToLogFontStruct(FontDialog, dlg, fontName, fontPointSize);
	// Yanghui

	if (gPerform(fd)) {
		object	fo = gFont(fd);
		gSetStringValue(nctl, gName(fo));
		gSetShortValue(pctl, gPointSize(fo));
		gDispose(fo);
	}
	
	gDispose(fd);
	
	return 0;
}


// Yanghui:

static	int	loadBitmap(object ctl, object dlg)
{
	object  fd;
	int	    len;
	static  char	bitmapFile[MAX_PATH];
	
	fd = vNew(FileDialog, dlg);

	gAppendFilter(fd, "Bitmap Files", "*.bmp");
	gAppendFilter(fd, "Flash Files", "*.swf");
	gDefExt(fd, "bmp");

	gSetFile(fd, bitmapFile);
	if (gGetOpenFile(fd)) {
		strcpy(bitmapFile, gGetFile(fd));
		len = strlen(bitmapFile);
		if (len > 4)
			if (!stricmp(bitmapFile+len-4, ".bmp") ) {
				object  ctlBitmapFileName = mGetControl(dlg, IDC_BitmapFileName);
				gSetStringValue(ctlBitmapFileName, bitmapFile);
			}
			else
				if (!stricmp(bitmapFile+len-4, ".swf") ) {
					object  ctlBitmapFileName = mGetControl(dlg, IDC_BitmapFileName);
					gSetStringValue(ctlBitmapFileName, bitmapFile);
				}
			
	}
	gDispose(fd);
	return 0;
}


static	int	clearBitmap(object ctl, object dlg)
{
	object  ctlBitmapFileName = mGetControl(dlg, IDC_BitmapFileName);
	gSetStringValue(ctlBitmapFileName, "");
	return 0;
}

static	int	saveProperty(object ctl, object dlg)
{
	object  propValue = mGetControl(dlg, IDC_PROP_VALUE);
	object  propCombo = mGetControl(dlg, IDC_PROPERTY_COMBO);
	char *	propName=gStringValue(propCombo);
	gRemoveStr(propCombo,propName);
	gAddOptionWithObj(propCombo,propName,gNewWithStr(String,gStringValue(propValue)));
	return 0;
}

static	int	deleteProperty(object ctl, object dlg)
{
	object  propCombo = mGetControl(dlg, IDC_PROPERTY_COMBO);
	gRemoveStr(propCombo,gStringValue(propCombo));
	return 0;
}



static	int	changeSingleControlFontAndTextColor(object ctl, object dlg)
{
	object  fontNameCtl = mGetControl(dlg, IDC_FontName);
	object  fontSizeCtl = mGetControl(dlg, IDC_FontSize);
	object  textColorCtl = mGetControl(dlg, IDC_FONT_COLOR);

	object fd;  
	char * fontName;  // including font FaceName [,Bold, Italic, Underline, Strikeout]
	char * textColor; 

	short  fontPointSize;
	if (fontNameCtl)
		fontName = gStringValue(fontNameCtl);      // get the font name
	if (fontSizeCtl)
		fontPointSize = gShortValue(fontSizeCtl);  // get the font point size
	if (textColorCtl)
		textColor = gStringValue(textColorCtl);    // get the font color

	// fd = gNewWithInitToLogFontStruct(FontDialog, dlg, fontName, fontPointSize);
	fd = gNewWithInitToLogFontStructAndTextColor(FontDialog, dlg, fontName, fontPointSize, textColor);

	if (gPerform(fd)) {
		object	fo = gFontWithColor(fd);
		gSetStringValue(fontNameCtl, gName(fo));
		gSetShortValue(fontSizeCtl, gPointSize(fo));
	
		if (textColorCtl)
			setColorStringValueFromRGB(textColorCtl, gGetFontColor(fo));

		gDispose(fo);
	}
	
	gDispose(fd);
	
	return 0;
}
// Yanghui


static	long	changeDefaultControlFonts(object wind, unsigned bm)
{
	object	dlg = mNewDialog(ModalDialog, IDD_CONTROL_FONTS, wind);
	object	nctl, pctl;

	gInitFunction(dlg, loadFontComboBoxes);
	
	nctl = mAddControl(dlg, ComboBox, IDC_StaticTextFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_StaticTextSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_TextFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_TextSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_NumericFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_NumericSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_DateFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_DateSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_TimeFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_TimeSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_PushButtonFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_PushButtonSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_RadioButtonFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_RadioButtonSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_CheckBoxFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_CheckBoxSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_ListBoxFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_ListBoxSize);
	gSetTag(nctl, pctl);
	
	nctl = mAddControl(dlg, ComboBox, IDC_ComboBoxFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_ComboBoxSize);
	gSetTag(nctl, pctl);
	
	// Yanghui:
	nctl = mAddControl(dlg, ComboBox, IDC_RectControlFont);
	gSetChgFunction(nctl, changeFont);
	pctl = mAddControl(dlg, ComboBox, IDC_RectControlSize);
	gSetTag(nctl, pctl);
	// Yanghui

	if (gPerform(dlg)) {
		saveFontDefaultSelections(dlg);
		updateControlDictionary(dlg);
		updateAllControlFonts(wind);
	}

	gDispose(dlg);
	
	return 0L;
}

static	void	updateControlDictionary(object dlg)
{
	object	fo, obj;

	if (fo = gFindStr(ControlFonts, "StaticText")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_StaticTextFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_StaticTextSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "StaticText",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_StaticTextFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_StaticTextSize)))));
	
	if (fo = gFindStr(ControlFonts, "Text")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_TextFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_TextSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "Text",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_TextFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_TextSize)))));
	
	if (fo = gFindStr(ControlFonts, "Numeric")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_NumericFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_NumericSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "Numeric",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_NumericFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_NumericSize)))));
	
	if (fo = gFindStr(ControlFonts, "Date")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_DateFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_DateSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "Date",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_DateFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_DateSize)))));
	
	if (fo = gFindStr(ControlFonts, "Time")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_TimeFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_TimeSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "Time",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_TimeFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_TimeSize)))));
	
	if (fo = gFindStr(ControlFonts, "PushButton")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_PushButtonFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_PushButtonSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "PushButton",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_PushButtonFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_PushButtonSize)))));
	
	if (fo = gFindStr(ControlFonts, "RadioButton")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_RadioButtonFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_RadioButtonSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "RadioButton",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_RadioButtonFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_RadioButtonSize)))));
	
	if (fo = gFindStr(ControlFonts, "CheckBox")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_CheckBoxFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_CheckBoxSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "CheckBox",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_CheckBoxFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_CheckBoxSize)))));
	
	if (fo = gFindStr(ControlFonts, "ListBox")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_ListBoxFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_ListBoxSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "ListBox",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_ListBoxFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_ListBoxSize)))));
	
	if (fo = gFindStr(ControlFonts, "ComboBox")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_ComboBoxFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_ComboBoxSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "ComboBox",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_ComboBoxFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_ComboBoxSize)))));

	// Yanghui:
	if (fo = gFindStr(ControlFonts, "RectControl")) {
		gChangeStringKey(gValue(fo), gCtlStringValue(dlg, IDC_RectControlFont));
		obj = gValue(gValue(fo));
		gChangeValue(gValue(fo), gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_RectControlSize))));
		gDispose(obj);
	} else
		gAddStr(ControlFonts, "RectControl",
			gNewWithStrObj(StringAssociation,
				       gCtlStringValue(dlg, IDC_RectControlFont),
				       gNewWithInt(ShortInteger, atoi(gCtlStringValue(dlg, IDC_RectControlSize)))));
	// Yanghui
}

static	void	updateAllControlFonts(object wind)
{
	object	controls = gGetControls(wind);
	object	seq, ctl;
	object	font;

	if (!controls) return;  /* Yanghui */

	for (seq=gSequence(controls) ; ctl = gNext(seq) ; ) {
		if (ClassOf(ctl) == StaticTextControl)
			font = getCurrentFont("StaticText");
		else if (ClassOf(ctl) == TextControl)
			font = getCurrentFont("Text");
		else if (ClassOf(ctl) == NumericControl)
			font = getCurrentFont("Numeric");
		else if (ClassOf(ctl) == DateControl)
			font = getCurrentFont("Date");
		else if (ClassOf(ctl) == TimeControl)
			font = getCurrentFont("Time");
		else if (ClassOf(ctl) == PushButton)
			font = getCurrentFont("PushButton");
		else if (ClassOf(ctl) == RadioButton)
			font = getCurrentFont("RadioButton");
		else if (ClassOf(ctl) == CheckBox)
			font = getCurrentFont("CheckBox");
		else if (ClassOf(ctl) == ListBox)
			font = getCurrentFont("ListBox");
		else if (ClassOf(ctl) == ComboBox)
			font = getCurrentFont("ComboBox");
		else if (ClassOf(ctl) == RectControl)      // Yanghui
			font = getCurrentFont("RectControl");  // Yanghui
		else
			font = NULL;

		if (font) 
			gSetFont(ctl, font);
	}
	
	gRedrawWindow(wind);
}

static	object	getCurrentFont(char *ctltype)
{
	object	fo = gFindStr(ControlFonts, ctltype);

	if (fo)
		return vNew(ExternalFont, gStringKey(gValue(fo)), gShortValue(gValue(gValue(fo))));

	return NULL;
}

static	int	changeFont(object nctl, object dlg)
{
	object	pctl = gGetTag(nctl);

	gFillPointSizeCombo(Font, gStringValue(nctl), 0, pctl);
	
	return 0;
}
static	long	saveLanguageChoice(object	ctl, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	
	object dlg=gGetParent(ctl);
	object nctl=gGetControl(dlg, IDC_LANGUAGESELECT);
	short newIdx=gShortValue(nctl);
	char * newValue=gCtlStringValue(dlg, IDC_LANGUAGETEXT);
	gChangeStrValue(gAssocAt(nctl,newIdx),newValue);
	return 0;
}
static	int	changeLanguage(object nctl, object dlg)
{
	short newIdx=gShortValue(nctl);
	gSetStringValue(gGetControl(dlg,IDC_LANGUAGETEXT),gStringValue(gAssocAt(nctl,newIdx)));
	return 0;
}



static	int loadFontComboBox(object dlg, long fontControlID, long fontSizeID,  char * controlName, object curnode)
{
	object	fo;
	object	nctl, pctl;
	char	fname[256];
	int	psize;

	nctl = mGetControl(dlg, fontControlID);
	pctl = mGetControl(dlg, fontSizeID);
	if (fo = gFindStr(ControlFonts, controlName)) {
		strcpy(fname, gStringKey(gValue(fo)));
		psize = gShortValue(gValue(gValue(fo)));
	} else {
		if (curnode)
		{
			char fontNodeName[100];
			char fontSizeNodeName[100];
			
			strcpy(fontNodeName,controlName);
			strcat(fontNodeName,"Font");

			strcpy(fontSizeNodeName,controlName);
			strcat(fontSizeNodeName,"Size");

			//load last set from disk
			gPopulateStringFromNode(curnode,fname,fontNodeName);
			psize=gGetIntFromNode(curnode,fontSizeNodeName);
		}
		else
		{
			strcpy(fname, "Arial");
			psize = 10;
		}
	}
	gFillFontNameCombo(Font, fname, psize, nctl, pctl, dlg);

	return 0;
}

static	int	loadFontComboBoxes(object dlg)
{
	object curnode=gParseFile(XMLNode,"CLDFontDefaults.XML",NULL,NULL,NULL);

	if (curnode)
		curnode=gChild(curnode);

	loadFontComboBox(dlg, IDC_StaticTextFont, IDC_StaticTextSize, "StaticText", curnode);
	loadFontComboBox(dlg, IDC_TextFont, IDC_TextSize, "Text", curnode);
	loadFontComboBox(dlg, IDC_NumericFont, IDC_NumericSize, "Numeric", curnode);
	loadFontComboBox(dlg, IDC_DateFont, IDC_DateSize, "Date", curnode);
	loadFontComboBox(dlg, IDC_TimeFont, IDC_TimeSize, "Time", curnode);
	loadFontComboBox(dlg, IDC_PushButtonFont, IDC_PushButtonSize, "PushButton", curnode);
	loadFontComboBox(dlg, IDC_RadioButtonFont, IDC_RadioButtonSize, "RadioButton", curnode);
	loadFontComboBox(dlg, IDC_CheckBoxFont, IDC_CheckBoxSize, "CheckBox", curnode);
	loadFontComboBox(dlg, IDC_ListBoxFont, IDC_ListBoxSize, "ListBox", curnode);
	loadFontComboBox(dlg, IDC_ComboBoxFont, IDC_ComboBoxSize, "ComboBox", curnode);
	loadFontComboBox(dlg, IDC_RectControlFont, IDC_RectControlSize, "RectControl", curnode);

	return 0;
}	

static	int saveFontDefaultSelection(object dlg, long fontControlID, long fontSizeID, char * controlName, FILE *fp)
{
	object	nctl, pctl;
	char fontNodeName[100];
	char fontSizeNodeName[100];

	nctl = mGetControl(dlg, fontControlID);
	pctl = mGetControl(dlg, fontSizeID);

	strcpy(fontNodeName,controlName);
	strcat(fontNodeName,"Font");

	strcpy(fontSizeNodeName,controlName);
	strcat(fontSizeNodeName,"Size");

	fprintf(fp,"\t\t<%s>%s</%s>\n",fontNodeName,gStringValue(nctl),fontNodeName);
	fprintf(fp,"\t\t<%s>%s</%s>\n",fontSizeNodeName,gStringValue(pctl),fontSizeNodeName);

	return 0;
}


static	int	saveFontDefaultSelections(object dlg)
{
	FILE * fp=fopen("CLDFontDefaults.XML","wt");
	fprintf(fp,"\t<FontDefaults>\n");
	saveFontDefaultSelection(dlg, IDC_StaticTextFont, IDC_StaticTextSize, "StaticText", fp);
	saveFontDefaultSelection(dlg, IDC_TextFont, IDC_TextSize, "Text", fp);
	saveFontDefaultSelection(dlg, IDC_NumericFont, IDC_NumericSize, "Numeric", fp);
	saveFontDefaultSelection(dlg, IDC_DateFont, IDC_DateSize, "Date", fp);
	saveFontDefaultSelection(dlg, IDC_TimeFont, IDC_TimeSize, "Time", fp);
	saveFontDefaultSelection(dlg, IDC_PushButtonFont, IDC_PushButtonSize, "PushButton", fp);
	saveFontDefaultSelection(dlg, IDC_RadioButtonFont, IDC_RadioButtonSize, "RadioButton", fp);
	saveFontDefaultSelection(dlg, IDC_CheckBoxFont, IDC_CheckBoxSize, "CheckBox", fp);
	saveFontDefaultSelection(dlg, IDC_ListBoxFont, IDC_ListBoxSize, "ListBox", fp);
	saveFontDefaultSelection(dlg, IDC_ComboBoxFont, IDC_ComboBoxSize, "ComboBox", fp);
	saveFontDefaultSelection(dlg, IDC_RectControlFont, IDC_RectControlSize, "RectControl", fp);
	fprintf(fp,"\t</FontDefaults>\n");

	fclose(fp);
	return 0;
}

/************************************************************
*  szClipboardFileName must point to a buffer of _MAX_PATH
*
*************************************************************/
static int getClipboardFileName(char * szClipboardFileName)
{
	int     nTmp;
	char    szLongPathName[_MAX_PATH];
	char    *strTmp1, *strTmp2;

	if (!szClipboardFileName)
		return -1;

	GetModuleFileName(gInstance(Application), szLongPathName, _MAX_PATH);

	if (!szLongPathName || !(*szLongPathName)) {
		*szClipboardFileName = '\0';
		return -1;
	}

	/* get the path(directory) of this exectable */
	nTmp = 0;
	strTmp1 = szLongPathName;
	while (strTmp1 && (strTmp2=strstr(strTmp1,"\\")) ) {
		nTmp = nTmp + strTmp2 - strTmp1 + 1;
		strTmp1 = strTmp2 + 1;
	}
	if (nTmp>0)
		strncpy(szClipboardFileName, szLongPathName, nTmp);

	szClipboardFileName[nTmp] = '\0';
	strcat(szClipboardFileName, "clipboard.bin");

	return 0;
}


static BOOL setRegKey(char *lpszKey, char *lpszValue, char *lpszValueName)
{
	if (lpszValueName == NULL)
	{
		if (RegSetValue(HKEY_CLASSES_ROOT, lpszKey, REG_SZ,
			  lpszValue, lstrlen(lpszValue) ) != ERROR_SUCCESS) {
		//	TRACE1("Warning: registration database update failed for key '%s'.\n",
		//		lpszKey);
			return FALSE;
		}
		return TRUE;
	} else {
		HKEY hKey;
		if (RegCreateKey(HKEY_CLASSES_ROOT, lpszKey, &hKey) == ERROR_SUCCESS) {
			LONG lResult = RegSetValueEx(hKey, lpszValueName, 0, REG_SZ,
				(CONST BYTE*)lpszValue, (lstrlen(lpszValue) + 1) );

			if (RegCloseKey(hKey) == ERROR_SUCCESS && lResult == ERROR_SUCCESS)
				return TRUE;
		}
		// TRACE1("Warning: registration database update failed for key '%s'.\n", lpszKey);
		return FALSE;
	}
}


static BOOL deleteRegKey(char *lpszKey)
{
	// copy the string
	char *lpszKeyCopy = strdup(lpszKey);
	char *lpszLast = lpszKeyCopy + lstrlen(lpszKeyCopy);

	// work until the end of the string
	while (lpszLast != NULL)
	{
		HKEY hKey;

		char szScrap[_MAX_PATH+1];
		DWORD dwLen = sizeof(szScrap);
		BOOL bItExists = FALSE;

		*lpszLast = '\0';
		lpszLast = _strdec(lpszKeyCopy, lpszLast);

		// try to open that key
		if (RegOpenKey(HKEY_CLASSES_ROOT, lpszKeyCopy, &hKey) != ERROR_SUCCESS)
			break;

		// enumerate the keys underneath
		if (RegEnumKey(hKey, 0, szScrap, dwLen) == ERROR_SUCCESS)
			bItExists = TRUE;
		RegCloseKey(hKey);

		// found one?  quit looping
		if (bItExists)
			break;

		// otherwise, delete and find the previous backwhack
		RegDeleteKey(HKEY_CLASSES_ROOT, lpszKeyCopy);
		lpszLast = strrchr(lpszKeyCopy, '\\');
	}

	// release the string and return
	free(lpszKeyCopy);
	return TRUE;
}


static long updateMenuAndToolBar(object objMainWindow)
{
	int    numOfDWs=0;
	object objMenu=NULL, objDWs=NULL, objCurrentDW=NULL;

	if (!objMainWindow)
		return -1L;

	objMenu      = gMenu(objMainWindow);
	objDWs       = gGetDWs(DragWindow);
	objCurrentDW = gGetCurrentDW(DragWindow);
	numOfDWs     = gGetNumOfDWs(DragWindow);

	if ( !objDWs || !objCurrentDW || numOfDWs<1 ) {
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_LEFT,   FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_RIGHT,  FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_TOP,    FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_BOTTOM, FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_CENTER_VERT,  FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_CENTER_HORZ,  FALSE);

		// gEnableToolBitmap(objMainWindow, IDB_SAME_WIDTH,   FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_SAME_HEIGHT,  FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_SAME_SIZE,    FALSE);
	
		if (objMenu) {
			gEnableMenuItem(objMenu, IDB_ALIGN_LEFT,   MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_RIGHT,  MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_TOP,    MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_BOTTOM, MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_CENTER_VERT,  MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_CENTER_HORZ,  MF_BYCOMMAND|MF_GRAYED);

			gEnableMenuItem(objMenu, IDB_SAME_WIDTH,   MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_SAME_HEIGHT,  MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_SAME_SIZE,    MF_BYCOMMAND|MF_GRAYED);
		}

		gEnableToolBitmap(objMainWindow, IDB_COPY,    FALSE);
		gEnableToolBitmap(objMainWindow, IDB_CUT,     FALSE);
	} else if (numOfDWs==1) {  // update toolbar appearance
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_LEFT,   FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_RIGHT,  FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_TOP,    FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_BOTTOM, FALSE);

		// gEnableToolBitmap(objMainWindow, IDB_CENTER_VERT,  TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_CENTER_HORZ,  TRUE);

		// gEnableToolBitmap(objMainWindow, IDB_SAME_WIDTH,   FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_SAME_HEIGHT,  FALSE);
		// gEnableToolBitmap(objMainWindow, IDB_SAME_SIZE,    FALSE);
	
		if (objMenu) {
			gEnableMenuItem(objMenu, IDB_ALIGN_LEFT,   MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_RIGHT,  MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_TOP,    MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_ALIGN_BOTTOM, MF_BYCOMMAND|MF_GRAYED);

			gEnableMenuItem(objMenu, IDB_CENTER_VERT,  MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_CENTER_HORZ,  MF_BYCOMMAND|MF_ENABLED);

			gEnableMenuItem(objMenu, IDB_SAME_WIDTH,   MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_SAME_HEIGHT,  MF_BYCOMMAND|MF_GRAYED);
			gEnableMenuItem(objMenu, IDB_SAME_SIZE,    MF_BYCOMMAND|MF_GRAYED);
		}

		gEnableToolBitmap(objMainWindow, IDB_COPY,    TRUE);
		gEnableToolBitmap(objMainWindow, IDB_CUT,     TRUE);
	} else if (numOfDWs>1) {		
		object seq, objTmp;
		int    numOfNoLineDWCtls=0;

		for(seq=gSequence(objDWs); objTmp=gNext(seq);) {
			if (ClassOf(gGetSelectedCtl(objTmp))!=LineControl) 
				numOfNoLineDWCtls++;
		}

		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_LEFT,   TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_RIGHT,  TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_TOP,    TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_ALIGN_BOTTOM, TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_CENTER_VERT,  TRUE);
		// gEnableToolBitmap(objMainWindow, IDB_CENTER_HORZ,  TRUE);

		if (objMenu) {
			gEnableMenuItem(objMenu, IDB_ALIGN_LEFT,   MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_ALIGN_RIGHT,  MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_ALIGN_TOP,    MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_ALIGN_BOTTOM, MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_CENTER_VERT,  MF_BYCOMMAND|MF_ENABLED);
			gEnableMenuItem(objMenu, IDB_CENTER_HORZ,  MF_BYCOMMAND|MF_ENABLED);
		}

		gEnableToolBitmap(objMainWindow, IDB_COPY,    TRUE);
		gEnableToolBitmap(objMainWindow, IDB_CUT,     TRUE);
				
		if (numOfNoLineDWCtls>1 && objCurrentDW && ClassOf(objCurrentDW)!=LineControl) {
			// gEnableToolBitmap(objMainWindow, IDW_SAME_WIDTH,   TRUE);
			// gEnableToolBitmap(objMainWindow, IDW_SAME_HEIGHT,  TRUE);
			// gEnableToolBitmap(objMainWindow, IDW_SAME_SIZE,    TRUE);
			
			if (objMenu) {
				gEnableMenuItem(objMenu, IDB_SAME_WIDTH,  MF_BYCOMMAND|MF_ENABLED);
				gEnableMenuItem(objMenu, IDB_SAME_HEIGHT, MF_BYCOMMAND|MF_ENABLED);
				gEnableMenuItem(objMenu, IDB_SAME_SIZE,   MF_BYCOMMAND|MF_ENABLED);
			}
		} else {
			// gEnableToolBitmap(objMainWindow, IDW_SAME_WIDTH,   FALSE);
			// gEnableToolBitmap(objMainWindow, IDW_SAME_HEIGHT,  FALSE);
			// gEnableToolBitmap(objMainWindow, IDW_SAME_SIZE,    FALSE);

			if (objMenu) {
				gEnableMenuItem(objMenu, IDB_SAME_WIDTH,  MF_BYCOMMAND|MF_GRAYED);
				gEnableMenuItem(objMenu, IDB_SAME_HEIGHT, MF_BYCOMMAND|MF_GRAYED);
				gEnableMenuItem(objMenu, IDB_SAME_SIZE,   MF_BYCOMMAND|MF_GRAYED);
			}
		}
	}

	return 0L;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////
// align_tool
//
// align all of the DragWindows in the set to the current DrawWindow according to the flag nAligment
//
//////////////////////////////////////////////////////////////////////////////////////////////////////
static	long	align_tool(object wind, unsigned nAlignment)
{
	HWND        hwndP;
	RECT        rectCurrentDW, rectTmp, rcDrag;
	int         numOfDWs, dx, dy, dxCurrentDW, dyCurrentDW;
	object      objDWs, objCurrentDW, selectedCtl, seq, objTmp;

	objDWs       = gGetDWs(DragWindow);
	objCurrentDW = gGetCurrentDW(DragWindow);
	numOfDWs     = gGetNumOfDWs(DragWindow);

	if ( !objDWs || !objCurrentDW ) 
		return -1L;

	hwndP = GetParent(gHandle(objCurrentDW));
	if (!hwndP)
		return -1L;

	// update current DragWindow, dxCurrentDW and dyCurrentDW may also be used for other controls in the next block
	if ((nAlignment==IDB_CENTER_VERT) || (nAlignment==IDB_CENTER_HORZ)) {
		GetClientRect(hwndP, &rectTmp);            // the client rectangle of the parent window of this DragWindow
		gGetUpdatedDragRect(objCurrentDW, &rcDrag);           // update the rectangle of the the DrawWindow
		gGetUpdatedGroupMoveDragRect(objCurrentDW, &rcDrag);  // update and get the group move rectangle of the the DrawWindow

		if (nAlignment==IDB_CENTER_VERT) {
			dxCurrentDW = 0;
			dyCurrentDW = (rectTmp.top + rectTmp.bottom + 1)/2 - (rcDrag.top + rcDrag.bottom + 1)/2;
		} else {
			dxCurrentDW = (rectTmp.left + rectTmp.right + 1)/2 - (rcDrag.left + rcDrag.right + 1)/2;
			dyCurrentDW = 0;
		}

		gMoveDragRectWithBoundaryCheck(objCurrentDW, dxCurrentDW, dyCurrentDW);           // update iDragRect of the current DrawWindow
		gMoveGroupMoveDragRectWithBoundaryCheck(objCurrentDW, dxCurrentDW, dyCurrentDW);  // update iGroupMoveDragRect of the current DrawWindow
			
		gGetDragRect(objCurrentDW, &rcDrag);
		gSetWindowPositions(objCurrentDW, &rcDrag);
	}

	if (numOfDWs<=1) {
		gRecalculateCRect(DragWindow);  // recalculate the unionized rectangle in the set
		return 0L;
	}

	// update the position of other DragWindows except the current DragWindow

	gGetUpdatedDragRect(objCurrentDW, &rectCurrentDW);  // update iDragRect of the current DrawWindow, rectCurrentDW not used
	gGetUpdatedGroupMoveDragRect(objCurrentDW, &rectCurrentDW);  // update and get iGroupMoveDragRect of the current DrawWindow

	selectedCtl = gGetSelectedCtl(objCurrentDW);
	if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox || 
	   ClassOf(selectedCtl)==RectControl || ClassOf(selectedCtl)==LineControl)
		InflateRect(&rectCurrentDW, -2, -2);    // take care of the 3D look
	
	for(seq=gSequence(objDWs); objTmp=gNext(seq);) {
		if (objTmp!=objCurrentDW) {
			selectedCtl = gGetSelectedCtl(objTmp);
			gGetUpdatedDragRect(objTmp, &rectTmp);           // update iDragRect, rectTmp not used
			gGetUpdatedGroupMoveDragRect(objTmp, &rectTmp);  // update and get iGroupMoveDragRect
		
			switch (nAlignment) {
				case IDB_ALIGN_LEFT:
					dx = rectCurrentDW.left - rectTmp.left;
					dy = 0;
					if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox ||
					   ClassOf(selectedCtl)==RectControl || ClassOf(selectedCtl)==LineControl)
						dx -= 2;    // take care of the 3D effect
					break;
				case IDB_ALIGN_TOP:
					dx = 0;
					dy = rectCurrentDW.top - rectTmp.top;
					if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox ||
					   ClassOf(selectedCtl)==RectControl || ClassOf(selectedCtl)==LineControl)
						dy -= 2;    // take care of the 3D effect
					break;
				case IDB_ALIGN_RIGHT:
					dx = rectCurrentDW.right - rectTmp.right;
					dy = 0;
					if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox ||
					   ClassOf(selectedCtl)==RectControl || ClassOf(selectedCtl)==LineControl)
						dx += 2;    // take care of the 3D effect
					break;
				case IDB_ALIGN_BOTTOM:
					dx = 0;
					dy = rectCurrentDW.bottom - rectTmp.bottom;
					if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox ||
					   ClassOf(selectedCtl)==RectControl || ClassOf(selectedCtl)==LineControl)
						dy += 2;    // take care of the 3D effect
					break;
				case IDB_CENTER_VERT:    // the same as the current DrawWindow
				case IDB_CENTER_HORZ:
					dx = dxCurrentDW;
					dy = dyCurrentDW;
					break;
				default:
					dx = 0;
					dy = 0;
			}
			
			gMoveDragRectWithBoundaryCheck(objTmp, dx, dy);           // update iDragRect
			gMoveGroupMoveDragRectWithBoundaryCheck(objTmp, dx, dy);  // update iGroupMoveDragRect

			gGetDragRect(objTmp, &rcDrag);
			gSetWindowPositions(objTmp, &rcDrag);
		}
	}

	// recalculate the unionized rectangle in the set
	gRecalculateCRect(DragWindow);
    return 0L;
}



void	print_barcode(object pntr, double yInches, double xInches, double heightInches, int barWidth, char *msg)
{
	int	          XOffset, YOffset, HRes, VRes;
	TALBarCode    in;
	METAFILEPICT  out;

	TALpFnct      fun;
	object        bc_dll;
	HDC	          hdc = (HDC) gHandle(pntr);

	
#ifdef	WIN32
	XOffset = GetDeviceCaps(hdc, PHYSICALOFFSETX);
	YOffset = GetDeviceCaps(hdc, PHYSICALOFFSETY);

	bc_dll = gLoadLibrary(DynamicLibrary, "talc3932.dll");
	if (!bc_dll)
		gError(Application, "Can't open Bar Code DLL (TALC3932.DLL)");
#else
	bc_dll = gLoadLibrary(DynamicLibrary, "talc39.dll");
	if (!bc_dll)
		gError(Application, "Can't open Bar Code DLL (TALC39.DLL)");
	{
		POINT	point;

		Escape(hdc, GETPRINTINGOFFSET, 0, NULL, &point);
		XOffset = point.x;
		YOffset = point.y;
	}
#endif
	VRes  = GetDeviceCaps(hdc, LOGPIXELSY);
	HRes  = GetDeviceCaps(hdc, LOGPIXELSX);
	fun = (TALpFnct) gGetProcAddress(bc_dll, "TALCODE39");
	if (!fun)
		gError(Application, "Can't find Bar Code proc address.");

	memset(&in, 0, sizeof in);
	memset(&out, 0, sizeof out);

	strcpy(in.messageBuffer, msg);
	in.messageLength  = strlen(in.messageBuffer);
	in.narrowBarWidth = barWidth;
	in.barCodeHeight  = (int) (heightInches * 2540.0);
	in.outputOption   = OutputTohDC;
	in.outputhDC      = (HDC) gHandle(pntr);
	in.preferences    = 1L;

	in.YPosInInches = yInches - (double) YOffset / (double) VRes;
	in.XPosInInches = xInches - (double) XOffset / (double) HRes;

	fun(&in, &out);

	gDispose(bc_dll);
}


/////////////////////////////////////////////////////////////////////////////////////////////
// sameSize_tool
// 
// make all of the DragWindows in the set the same width, same height, or the same size
// as the current DragWindow. if the control under the DragWindow is a listbox or a combobox, 
// only the same width as the current DragWindow can be made; 
// If the control under the current DragWindow is a LineControl, nothing will happen;
//
/////////////////////////////////////////////////////////////////////////////////////////////
static	long	sameSize_tool(object wind, unsigned nSize)
{
	object  objDWs, objCurrentDW, selectedCtl, seq, objTmp;
	RECT    rectCurrentDW, rcDrag, rcGroupMoveDrag;
	int     numOfDWs, widthCurrentDW, heightCurrentDW;

	objDWs       = gGetDWs(DragWindow);
	objCurrentDW = gGetCurrentDW(DragWindow);
	numOfDWs     = gGetNumOfDWs(DragWindow);

	if ( (!objDWs) || (!objCurrentDW) || (numOfDWs<=1)) 
		return 0L;

	selectedCtl = gGetSelectedCtl(objCurrentDW);
	if (ClassOf(selectedCtl)==LineControl) 
		return 0L;

	gGetUpdatedDragRect(objCurrentDW, &rectCurrentDW);  // update iDragRect, rectCurrentDW not used
	gGetUpdatedGroupMoveDragRect(objCurrentDW, &rectCurrentDW);  // update and get iGroupMoveDragRect

	widthCurrentDW  = rectCurrentDW.right  - rectCurrentDW.left;
	heightCurrentDW = rectCurrentDW.bottom - rectCurrentDW.top;
	
	if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==ComboBox || 
	   ClassOf(selectedCtl)==RectControl ) {
		widthCurrentDW -= 4;    // take care of the 3D effect
		heightCurrentDW -= 4;
	}
		
	for(seq=gSequence(objDWs); objTmp=gNext(seq);) {
		selectedCtl = gGetSelectedCtl(objTmp);
		if (objTmp!=objCurrentDW && ClassOf(selectedCtl)!=LineControl) {
			gGetUpdatedDragRect(objTmp, &rcDrag);
			gGetUpdatedGroupMoveDragRect(objTmp, &rcGroupMoveDrag);
			
			switch (nSize) {
				case IDB_SAME_WIDTH:
					rcDrag.right = rcDrag.left + widthCurrentDW;
					rcGroupMoveDrag.right = rcGroupMoveDrag.left + widthCurrentDW;

					if (ClassOf(selectedCtl)==PushButton || ClassOf(selectedCtl)==ComboBox ||
					   ClassOf(selectedCtl)==RectControl) {
						rcDrag.right += 4;    // take care of the 3D effect
						rcGroupMoveDrag.right += 4;
					}
					break;
				case IDB_SAME_HEIGHT:    //  ignore the cases of ListBox and ComboBox
					if ( (ClassOf(selectedCtl)!=ListBox) && (ClassOf(selectedCtl)!=ComboBox) ) {
						rcDrag.bottom = rcDrag.top + heightCurrentDW;
						rcGroupMoveDrag.bottom = rcGroupMoveDrag.top + heightCurrentDW;
						
						if (ClassOf(selectedCtl)==PushButton  || ClassOf(selectedCtl)==RectControl) {
							rcDrag.bottom += 4;    // take care of the 3D effect
							rcGroupMoveDrag.bottom += 4;
						}
					}
					break;
				case IDB_SAME_SIZE:
					rcDrag.right = rcDrag.left + widthCurrentDW;
					rcGroupMoveDrag.right = rcGroupMoveDrag.left + widthCurrentDW;

					if ( (ClassOf(selectedCtl)!=ListBox) && (ClassOf(selectedCtl)!=ComboBox) ) {
						rcDrag.bottom = rcDrag.top + heightCurrentDW;
						rcGroupMoveDrag.bottom = rcGroupMoveDrag.top + heightCurrentDW;

						if (ClassOf(selectedCtl)==PushButton || ClassOf(selectedCtl)==RectControl) {    // take care of the 3D effect
							rcDrag.right += 4;
							rcGroupMoveDrag.right += 4;
							rcDrag.bottom += 4;
							rcGroupMoveDrag.bottom += 4;
						}
					} else if (ClassOf(selectedCtl)==ComboBox) {
						rcDrag.right += 4;
						rcGroupMoveDrag.right += 4;
					}
					break;
				default:
					break;
			}

			gSetDragRect(objTmp, &rcDrag, NULL);
			gSetGroupMoveDragRect(objTmp, &rcGroupMoveDrag, NULL);
			gSetWindowSizes(objTmp, &rcDrag);
			gInvalidateRect(objTmp, NULL, FALSE);
		}
	}
	
	// recalculate the unionized rectangle in the set
	gRecalculateCRect(DragWindow);
    return 0L;
}


static	long	fileOpen(object wind, char *strIn)
{
	char *  pChar;  
	object  objMFName;
	int	    len;
	char    file[_MAX_PATH];

	if (!wind || !strIn || !(*strIn))
		return -1L;

	strcpy(file, strIn);
	len = strlen(file);

	if (len <= 4)
		return -1L;

	if (stricmp(file+len-4, ".wmf")==0 || stricmp(file+len-4, ".pmf")==0) 
		gPlayMetaFile(wind, file);
	else if (stricmp(file+len-4, ".cld")==0) {

		new_tool(wind,0);

		strcpy(CLFile, file);
		gLoadGUI(wind, NULL, CLFile);  // the metafile name object iMFName is loaded
		
		// get the metafile name from the iClientWind 
		objMFName = gGetMFName(wind);

		if (objMFName) {
			pChar = gStringValue(objMFName);  // the associated meta file
			
			scalingFactor = 1.0;
			if (pChar && (*pChar)) {   // load the metafile 
				gPlayMetaFile(wind, pChar);
						
				// get the scaling factor from the iClientWind 
				scalingFactor = gGetMFScale(wind);
				if (scalingFactor>0)
					gSetMFScale(wind, scalingFactor);  
			}				
		}
	}
	return 0;
}


static	long	filePrint(object wind, char *strIn)
{
	int	    len;
	char    file[_MAX_PATH];

	object  printerObj=NULL, cldObj=NULL;

	if (!wind || !strIn || !(*strIn))
		return -1L;

	strcpy(file, strIn);
	len = strlen(file);

	if (len <= 4)
		return -1L;

	printerObj = gNewPrinter(Printer, wind, 0);     // letter paper
	// printerObj = gNewPrinter(Printer, self, 1);  // legal paper

	if (stricmp(file+len-4, ".wmf")==0 || stricmp(file+len-4, ".pmf")==0) 
		cldObj = gNewCLD(CLD, NULL, file);
	else if (stricmp(file+len-4, ".cld")==0)
		cldObj = gNewCLD(CLD, file, NULL);

	gPrintCLD(cldObj, printerObj);

	if (cldObj)
		cldObj = gDispose(cldObj);

	if (printerObj)
		printerObj = gDispose(printerObj);

	return 0L;
}


static void registerShellFileTypes()
{
	char szFileTypeId[_MAX_PATH];
	char szFileTypeName[_MAX_PATH];
	char szFilterExt[_MAX_PATH];
	char szCommand[_MAX_PATH];

	strcpy(szFileTypeId, "CLD");
	strcpy(szFileTypeName, "Control Layer Designer");
	strcpy(szFilterExt, ".cld");
	strcpy(szCommand, "command");

	// first register the type ID of our server
	if (setRegKey(szFileTypeId, szFileTypeName, NULL))
	{
		char szLongPathName[_MAX_PATH];
		char szOpenCommandLine[_MAX_PATH];
		char szPrintCommandLine[_MAX_PATH];
		char szPrintToCommandLine[_MAX_PATH];
		char szDefaultIconCommandLine[_MAX_PATH];
		
		char szTmp[_MAX_PATH];
	
		LONG lResult;
		LONG lSize = _MAX_PATH * 2;

		GetModuleFileName(gInstance(Application), szLongPathName, _MAX_PATH);

		strcpy(szOpenCommandLine, szLongPathName);
		strcpy(szPrintCommandLine, szLongPathName);
		strcpy(szPrintToCommandLine, szLongPathName);
		strcpy(szDefaultIconCommandLine, szLongPathName);

		strcat(szOpenCommandLine, " \"%1\"");
		strcat(szPrintCommandLine, " /p \"%1\"");
		// strcat(szPrintToCommandLine, " /pt \"%1\" \"%2\" \"%3\" \"%4\"");

		sprintf(szTmp, ",%d", DEFAULT_ICON_INDEX);
		strcat(szDefaultIconCommandLine, szTmp);

		// path\DefaultIcon = path,0
		sprintf(szTmp, "%s\\DefaultIcon", szFileTypeId);
		// setRegKey(szTmp, szDefaultIconCommandLine, NULL);
		setRegKey(szTmp, "ALGOCORP.ICO", NULL);

		// path\shell\open\command = path filename
		sprintf(szTmp, "%s\\shell\\open\\%s", szFileTypeId, szCommand);
		setRegKey(szTmp, szOpenCommandLine, NULL);

		// path\shell\print\command = path /p filename
		sprintf(szTmp, "%s\\shell\\print\\%s", szFileTypeId, szCommand);
		setRegKey(szTmp, szPrintCommandLine, NULL);

		// path\shell\printto\command = path /pt filename printer driver port
		// sprintf(szTmp, "%s\\shell\\printto\\%s", szFileTypeId, szCommand);
		// setRegKey(szTmp, szPrintToCommandLine, NULL);

		lResult = RegQueryValue(HKEY_CLASSES_ROOT, szFilterExt, szTmp, &lSize);
		if (lResult != ERROR_SUCCESS || !szTmp || !(*szTmp) || szTmp == szFileTypeId)
		{
			setRegKey(szFilterExt, szFileTypeId, NULL);
			sprintf(szTmp, "%s\\ShellNew", szFilterExt);
			setRegKey(szTmp, "", "NullFile");
		}
	}
}





#define	BUFLEN	128



static	void	saveXMLFile(object wind, char *fname)
{
	FILE	*fp;

	strcpy(CLFile, fname);
	if (fileExists(CLFile)) {
		char	tfile[256], *p;
		
		strcpy(tfile, CLFile);
		if (p = strrchr(tfile, '.'))
			*p = '\0';
		strcat(tfile, ".bak");
		if (fileExists(tfile))
			_unlink(tfile);        /* delete the .bak file */
		rename(CLFile, tfile);     /* rename .cld to .bak */
	}



	fp = fopen(fname, "wt");

	if (fp) {
		object	ctllist = gGetControls(wind);
		object	seq, ctl, obj, grp;
		object	cls;
		char	name[512];
		object	rgroups = NULL, rgorder;
		object	rgminx, rgminy;
	//	char	vname[256], *p;

				
		fprintf(fp, "<screen>\n");
		
	
		gWriteXML(wind,fp);

		fprintf(fp, "\t<controls>\n");
		
		for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) 
		{
			if (gGetTabOrder(ctl)!=-1)
				gWriteXML(ctl,fp);
		}
		fprintf(fp, "\t</controls>\n");
		fprintf(fp, "</screen>\n");
		
		
		fclose(fp);
	}
}
static	long	export_xml(object wind, unsigned bm)
{
	object  fd = vNew(FileDialog, wind);
	char	fname[256], *p;

	if (gLastLoadType(wind)==LOAD_TYPE_BINARY) {
		int ret=gQuery(wind,"WARNING","You are saving as XML, but loaded as binary.\nContinue?",MB_OKCANCEL);

		if (ret==IDCANCEL)
			return 0L;
	}
	
	strcpy(fname, CLFile);
	p = strrchr(fname, '.');
	if (p) {
		*p = '\0';
		strcat(fname, ".cld");
	}
	
	gSetModifyChildren(wind, 1);
	gAppendFilter(fd, "CLD Files", "*.cld");
	gDefExt(fd, "cld");
	gSetFile(fd, fname);
	gSetFlags(fd, OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST | OFN_NOCHANGEDIR);

	if (gGetSaveFile(fd))
		saveXMLFile(wind, gGetFile(fd));
	
	gDispose(fd);
	
	return 0L;
}

static	long	export_flash(object wind, unsigned bm)
{
	object  fd = vNew(FileDialog, wind);
	char	fname[256], *p;
	char    saveFname[256];

	strcpy(fname, CLFile);
	p = strrchr(fname, '.');
	if (p) {
		*p = '\0';
		strcat(fname, ".as");
	}

	strcpy(saveFname, fname);
	
	gSetModifyChildren(wind, 1);
	gAppendFilter(fd, "Action Script Files", "*.as");
	gDefExt(fd, "as");
	gSetFile(fd, fname);
	gSetFlags(fd, /*OFN_OVERWRITEPROMPT |*/ OFN_PATHMUSTEXIST | OFN_NOCHANGEDIR);

	if (gGetSaveFile(fd)) {
		char	file[200], *p, *asfile = gGetFile(fd), *a, name[200];
		
		a=strrchr(asfile, '\\');
		if (a)
			a++;
		else
			a = asfile;
		strcpy(name, a);
		if (p = strrchr(name, '.'))
			*p = '\0';

		strcpy(file, asfile);
		if (p = strrchr(file, '.'))
			*p = '\0';
		strcat(file, "_i.as");
		saveFlashFile(wind, file, name);
		
		strcpy(file, asfile);
		if (p = strrchr(file, '.'))
			*p = '\0';
		strcat(file, ".jsfl");
		saveJSFLFile(wind, file, name);

		strcpy(file, asfile);
		if (p = strrchr(file, '.'))
			*p = '\0';
		strcat(file, "_d.as");
		saveFlashDataFile(wind,name);
		touch(file);

		touch(asfile);
	}
	gDispose(fd);
	
	return 0L;
}

static	void	touch(char *file)
{
	FILE	*fp = fopen(file, "a");
	if (fp)
		fclose(fp);
}

