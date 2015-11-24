/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif



#include <windows.h>
#include <string.h>
//#include "logfile.h"
#include "hdlcache.h"
#include "ctlsave.h"

#ifdef	_WIN32
#include <pshpack2.h>
#endif

typedef struct  {
	DWORD key;
	short hmf;
	struct {
		short	left, top, right, bottom;
	}	bbox;
	WORD inch;
	DWORD reserved;
	WORD checksum;
}	APM;

#ifdef	_WIN32
#include <poppack.h>
#endif


#define LOAD_TYPE_BINARY	0
#define LOAD_TYPE_XML		1




defclass  Window : Stream, PropertyList  {

	/*  Window Class Info  */

	object		iClassName;

	int		iType;		/*  window type:  0=normal, 1=built in class,
					    2=window control, 3=dialog control  */

	WNDPROC		iWndProc;
	object		iCursor;
	object		iIcon;
	LPCSTR		iMenuName;
	object		iBackBrush;
	UINT		iStyle;
	int		iClsExtra;
	int		iWndExtra;

	/*  Window Info  */

	DWORD		iExStyle;
	object		iWndTitle;
	DWORD		iWndStyle;

	int		iMHPos;		/*  mouse horz position		*/
	int		iMVPos;		/*  mouse vert position		*/

	int		iHPos;		/*  position of upper left	*/
	int		iVPos;		/*  outside corner of window	*/
	int		iVHPos;		/*  virtual positions 		*/
	int		iVVPos;		/*  virtual positions 		*/

	int		iHCSize;	/*  client area size		*/
	int		iVCSize;

	int		iHSize;		/*  whole window size		*/
	int		iVSize;

	object		iParent;	/*  parent window		*/

	object		iChildren;	/*  linked list of child windows  */
	

	HWND		iHwnd;		/*  handle to window created  	*/

	HWND		iZOrder;	/*  window Z order		*/

	object		iMenu;		/*  menu associated with window	*/
	object		iShortcutMenuStr;
	unsigned	iShortcutMenuID;
	int		iShortcutSubmenu;
	object		iShortcutFunctions;

	object		iWndCursor;	/*  specific cursor associated with the window  */

	int		iAutoDispose;	/*  1=dispose when window closed  */

	object		iTag;		/*  arbitrary tag object associated
					    with the window */
	int	iAutoDisposeTag;


	/*  Message handling   */


	object		iMessageHandlers;/*  IntDict of message handlers  */
	iInitFunctions;		/*  Function executed when window created */
	iFunctions;		/*  Function called when window closed    */

	WNDPROC	iOrgWndProc;	/*  original window proc for control  */

	/*  For client area  */

	object		iFont;		/*  default font  		*/
	object		iFonts;		/*  linked list of used fonts	*/

	object		iTextBrush;	/*  text color brush		*/

	object		iClientArea;	/*  Linked list of client area output
					    items  */

	object		iControlDict;	/*  String Dictionary of controls - used
					    when window is used as a dialog  */
	object		iControls;	/*  Linked List of controls */
	int		iXAppendOffset;	/*  Control offsets  */
	int		iYAppendOffset;
		

	object		iPauseArea;	/*  Linked list of client area output
					    items saved when paused  */

	int		iVOffset;	/*  scrolling offsets  */
	int		iHOffset;

	int		iVMax;		/*  maximum client area needed  */
	int		iHMax;

	int		iModifyChildren;  /*  ability to edit child windows  */
	int		iInModifyMode;	  /*  window in process of being modified  */
	int		iTabOrder;	/*  last tab order used		*/

	int		iWait;		/*  number of times set to wait  */

	HANDLE		iHmf;		/*  metafile handle		*/
	int		iEnhancedMetaFile;  /*  1=Enh, 0=old, 2=Placeable */
	APM		iMfHdr;		/*  placeable metafile header   */
	double		iMFScale;	/*  metafile scaling factor	*/

	object          iMFName;        //  metafile name object // Yanghui

	/*  items used for auto scrolling text  */

	int		iMaxLines;	/*  max lines to retain		*/
	int		iCurLine;	/*  current output line		*/
	int		iBegCol;	/*  beginning column for each line */
	int		iCurCol;	/*  current column position	*/
	int		iAutoShow;	/*  1=auto show when window used*/
	int		iEcho;		/*  1=echo characters types  */


	/*  items used for character input  */

	char		*iBuf;		/*  character input buffer	*/
	int		iBsize;		/*  buffer size			*/
	int		iNchars;	/*  number of characters in buffer  */
	int		iRaw;		/*  1=no \b processing		*/
	int		iBlock;		/*  1=wait for input		*/
	

	/*  Toolbar stuff  */

	iToolBar;
	iStatusBar;
	iClientWind;		/*  client area window used when toolbar or
				    statusbar are used.  	*/

	/*  help info associated with a window   */

	iTopic;			/*  help topic associated with window */
	char	*iPrevTopic;	/*  previous topic	*/

	/*  splash window stuff  */
	int	iSplashID;
	int	iSplashTime;

	COLORREF	iHoldColor;	/* Holds the original color while the
					   child control is being moved.	*/

	iDockedChildren;	//  Windows docked to this window (children)
	iDockedParents;		//  Windows this window is docked to (parents)

	iBmp;			/* Bitmap placed on a window permanently.	*/

	iDisableObject;
	iActivateFunctions;	/*  window activation function		     */
	
	iCtlSection;		/* Linked List of Control Sections (String Associations)	*/
	int	iBeenPerformed;

	int	iMenuID;	/* Currently selected menu id */

	int	iIsToolBar;
	iTask;			/*  Task object if this is part of a tasklist  */
	
	int iLastLoadType; /* Last type of file loaded - Binary or XML */

// Yanghui:
	iTrackRectObj;                // the instance TrackRect for grouping selection
	RECT       iRectTrack;        // the rectangle of the TrackRect for grouping selection

	BOOL       iScaleFlg;         // TRUE   ---    scaling when the cld file is loaded
	                              // FALSE  --- no scaling when the cld file is loaded

	double     iControlScale;     // Control scaling factor, Note: this scaling factor is not the same
	                              // as iMFScale because the change of the font size in the setting affects
	                              // metafiles but not controls

	double     iFontScale;        // font scaling factor (the ratio between current logPixelsx to
	                              // the logPixelsx when the form was generated). Note: for small font,
								  // logPixelsx=96; for large font, logPixelsx=120.

	double     iPrinterScale;     // the extra printer scaling factor (used only in the case of iScaleFlg=TRUE)
	                              // it affects gDrawRulerAndGrid and printing only
	
	double     iMFCxScale;        // iMFCxScale and iMFCyScale are used to scale both the metafile and 
	double     iMFCyScale;        // the controls anisotropically

	RECT       iMFMargins;        // iMFMargins is used to add or cut the metafile margins and to move the 
	                              // controls accordingly

	int        iPage;             // the current page the control is located during printing 

	short      iRulerAndGrid;     // the ruler flag

	int        iPageTopMargin;    // the top margin of the page printed
	int        iPageBottomMargin; // the bottom margin of the page printed
	
	iDeletedCtls;                 // the link object which hold the deleted controls during the design phase

// Yanghui
	
	int	iModal;
	int	iEndWindow;
	int	iModalResult;

	int	iCenterWindow;

	int	iTimerCounter;
	object	iTimerFunctions;     // Integer dictionary of timer functions.
 class:
	ifun	cAccessMode;
	cInitFunctions;
init:	class_init;
};


#include "demo.h"


private	imeth	pLoadControls(char *file, int checkMF);
private imeth int pCheckAndLoadXML(self,char *fname);
private	imeth	int	modalWindowParam(object self);

static	void	dispose_children(ivType *iv);
static	void	removeChild(object self, object child);
static	void	create_clientWind(object self, ivType *iv);
static	HMETAFILE	GetPlcMetaFile(object wind, ivType *iv, char *file, APM *mfHdr);
static	void	PlayPlcMetaFile(object wind, ivType *iv, HDC hdc, HMETAFILE hmf, APM *mfHdr);
static	void	update_horz(ivType *iv);
static	void	update_vert(ivType *iv);
static	void	updateDockedWindows(ivType *iv, int hp, int vp);
static	LRESULT	CALLBACK  WindowProc(HWND hwnd, UINT mMsg, WPARAM wParam, LPARAM lParam);
static	void    ScaleMetafile(object wind, ivType *iv, HDC hdc, APM *mfHdr);


// Yanghui:
struct CLDObjectNode {
	object obj;
	struct CLDObjectNode * left;
	struct CLDObjectNode * right;
};

typedef struct CLDObjectNode * CLDObjectLink;
static  int    addCLDObjectLink(CLDObjectLink rootLink, CLDObjectLink nodeLink);
static  int    fillLinkObject(object linkObject, CLDObjectLink rootLink);
static  int    destroyCLDObjectLink(CLDObjectLink rootLink);
static  object createOrderedControlsForPrinting(ivType *iv);
static  int    getClipboardFileName(char * szClipboardFileName);
// Yanghui


#define BUFSIZE		256

#define MAX(a, b)	((a) > (b) ? (a) : (b))
#define MIN(a, b)	((a) < (b) ? (a) : (b))

#define MAX_VRANGE	(iVMax > iVCSize ? iVMax - iVCSize : 0)
#define MAX_HRANGE	(iHMax > iHCSize ? iHMax - iHCSize : 0)

#define IS_OVERLAPPED(s)	!(s & (WS_POPUP | WS_CHILD))

#define	ActiveArea	(iPauseArea ? iPauseArea : iClientArea)
#define	Paused		iPauseArea


#ifdef	WIN32
#define DFLT  	return self  &&  iOrgWndProc   &&  iOrgWndProc != WindowProc  ? \
                           CallWindowProc((WNDPROC) iOrgWndProc, hwnd, mMsg, wParam, lParam) : \
			   DefWindowProc(hwnd, mMsg, wParam, lParam)
#else
#define DFLT  	return self  &&  iOrgWndProc   &&  iOrgWndProc != WindowProc  ? \
                           CallWindowProc((FARPROC) iOrgWndProc, hwnd, mMsg, wParam, lParam) : \
			   DefWindowProc(hwnd, mMsg, wParam, lParam)
#endif

typedef	LRESULT	(*lfun)();


// Yanghui:
// Get the scaling flag for the cld file which is to be loaded
imeth    BOOL gGetScaleFlg()
{
	if (iClientWind)
		return gGetScaleFlg(iClientWind);
	return iScaleFlg;
}
	
// Set the scaling flag for the cld file which is to be loaded
imeth    gSetScaleFlg(BOOL aBool)
{
	if (iClientWind)
		return gSetScaleFlg(iClientWind, aBool);
	iScaleFlg = aBool;
	return self;
}
	

// get the meta file name object
imeth    gGetMFName()
{
	if (iClientWind)
		return gGetMFName(iClientWind);
	return iMFName;
}

static	int	centerWindow(object dlg)
{
	object	wind = gGetParent(dlg);
	int	height, width, xpos = 0, ypos = 0, wh, ww, x, y;
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	gGetSize(dlg, &wh, &ww);

	gGetSize(wind ? wind : Application, &height, &width);
	if (wind)
		gGetPosition(wind, &ypos, &xpos);
	
	y = ypos + (height - wh) / 2;
	x = xpos + (width  - ww) / 2;
	gSetPosition(dlg, y, x);

	gSetScalingMode(Application, pm);
	
	return 0;
}

imeth	gCenter()
{
	iCenterWindow = 1;

	if (iHwnd)
		centerWindow(self);
	return self;
}

private	imeth	LRESULT	process_wm_timer(object	self,
					 HWND	hwnd,
					 UINT	mMsg,
					 WPARAM	wParam,
					 LPARAM	lParam)
{
	object	mobj = iTimerFunctions ? gFindValueInt(iTimerFunctions, wParam) : NULL;
	
	if (mobj) {
		object	obj = gValue(mobj);
		
		if (IsObj((object)obj)  &&  ClassOf((object)obj) == JavaCallbackClassSurrogate)
			gPerformJavaObjCallback((object)obj, self);
		else if (SchemeClassSurrogate  &&  IsObj(obj)  &&  ClassOf(obj) == String) {
			char	cmd[100], ns[80];
			
			sprintf(cmd, "(%s (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object) obj),
				PTOLL(self));
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					      gNamespaceName(SchemeClassSurrogate, (object) obj, ns), 
					      cmd);
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"))", gStringValue((object)obj), PTOLL(self));
			gExecuteStringNR(JavaScriptClassSurrogate, cmd);
		} else {
			ifun	fun = (ifun) gPointerValue(obj);
				
			fun(self);
		}
	}
	return 0L;
}

private	imeth	LRESULT	process_wm_create(object	self, 
					  HWND		hwnd, 
					  UINT		mMsg, 
					  WPARAM	wParam, 
					  LPARAM	lParam)
{
	object	splash, seq, obj;
	
	splash = iSplashID ? gNewSplashWindow(SplashWindow, iSplashID) : NULL;

	if (splash)
		gSetTitle(splash, gStringValue(iWndTitle));
	
	if (iZOrder != HWND_TOP)
		SetWindowPos(iHwnd, iZOrder, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE));

	if (iSplashTime)
		Sleep((long) iSplashTime * 1000L);
	
	if (iInitFunctions)
		gExecuteFunctionsObj(iInitFunctions, self);

	if (iCenterWindow)
		centerWindow(self);
	if (splash)
		gDispose(splash);

	if(iTrackRectObj)
		iTrackRectObj = gCancelTracking(iTrackRectObj);

	if (iTimerFunctions)
		for (seq = gSequence(iTimerFunctions); obj = gNext(seq); )
			SetTimer(hwnd, gIntKey(obj), gIntKey(gValue(obj)), NULL);
	
	return 0L;
}


// Yanghui:

/////////////////////////////////////////////////////////////////////////////////////////////////
// gDrawRulerAndGrid:    draw the ruler and grid
// HDC hdc:              handle to DC
// short nRulerAndGrid:  <=0 and >4 --- no ruler and grid
//                                1 --- ruler only
//                                2 --- ruler and grid at long tick positions
//                                3 --- ruler and grid at long and middle tick positions
//                                4 --- ruler and grid at long, middle, and short tick positions
//
/////////////////////////////////////////////////////////////////////////////////////////////////
private imeth int gDrawRulerAndGrid(HDC hdc, short nRulerAndGrid)
{
	int     nOldMapMode, nOldBkMode, nLogPixelsX, nLogPixelsY;
	int     nShort=3, nMiddle=6, nLong, nCx, nCy, nTmp, nTmp2, i, j;
	UINT    nOldTextAlign;
	HFONT   hfont, hfontOld;
	HPEN    hpen, oldHpen;
	char    strLabel[4];
	POINT   ptViewportOrg, ptWindowOrg;
	SIZE    szWindowExt, szViewportExt;
	LOGFONT logfont;
	RECT    rect;

	if(!hdc)
		return -1;

	if(nRulerAndGrid<=0 || nRulerAndGrid>4)
		return 0;

	nLogPixelsX = GetDeviceCaps(hdc, LOGPIXELSX);
	nLogPixelsY = GetDeviceCaps(hdc, LOGPIXELSY);

	memset(&logfont, 0, sizeof(logfont));
	logfont.lfHeight = -MulDiv(8, nLogPixelsY, 72);
	strcpy(logfont.lfFaceName, "Arial");

	hfont = CreateFontIndirect(&logfont);
	if(!hfont) 
		return -1;

	nLong = abs(logfont.lfHeight);

	hpen = CreatePen(PS_SOLID, 1, RGB(0,0,0));
	if(!hpen) {
		DeleteObject((HGDIOBJ) hfont);
		return -1;
	}

	gGetWindowRect(self, &rect);
	nCx = (rect.right - rect.left)/(iMFScale*iMFCxScale);
	nCy = (rect.bottom - rect.top)/(iMFScale*iMFCyScale);

	nOldMapMode = SetMapMode(hdc, MM_ANISOTROPIC);
	SetWindowOrgEx(hdc, 0, 0, &ptWindowOrg);

	if(iScaleFlg) {
		SetViewportOrgEx(hdc, (int)(-iHOffset+iMFMargins.left*iControlScale*iMFCxScale*iPrinterScale+0.5), 
		                      (int)(-iVOffset+iMFMargins.top*iControlScale*iMFCyScale*iPrinterScale+0.5), &ptViewportOrg); 

		SetWindowExtEx(hdc, (int)((double)nCx/(iMFScale*iMFCxScale*iPrinterScale)+0.5), 
		                    (int)((double)nCy/(iMFScale*iMFCyScale*iPrinterScale)+0.5), &szWindowExt);
	}
	else {
		SetViewportOrgEx(hdc, (int)(-iHOffset+iMFMargins.left*iControlScale*iMFCxScale+0.5), 
		                      (int)(-iVOffset+iMFMargins.top*iControlScale*iMFCyScale+0.5), &ptViewportOrg); 

		SetWindowExtEx(hdc, (int)((double)nCx/(iMFScale*iMFCxScale)+0.5), 
		                    (int)((double)nCy/(iMFScale*iMFCyScale)+0.5), &szWindowExt);
	}

	SetViewportExtEx(hdc, nCx, nCy, &szViewportExt);

	nOldBkMode = SetBkMode(hdc, TRANSPARENT);
	nOldTextAlign = SetTextAlign(hdc, TA_LEFT | TA_TOP);

	// draw the ruler
	hfontOld = SelectObject(hdc, hfont);
	oldHpen = SelectObject(hdc, hpen);

	// x axis ruler
	i=0;
	nTmp2 = -2;
	do {
		nTmp = i*nLogPixelsX;
		if(i>=10)
			nTmp2 = -6;

		if(nTmp>0) {
			sprintf(strLabel, "%d", i);
			TextOut(hdc, nTmp+nTmp2, -2, strLabel, strlen(strLabel));
		}
		
		for(j=1; j<4; j++) {
			MoveToEx(hdc, nTmp+j*nLogPixelsX/8, 0, NULL);
			LineTo(hdc, nTmp+j*nLogPixelsX/8, nShort);
		}
		for(j=5; j<8; j++) {
			MoveToEx(hdc, nTmp+j*nLogPixelsX/8, 0, NULL);
			LineTo(hdc, nTmp+j*nLogPixelsX/8, nShort);
		}

		MoveToEx(hdc, nTmp+nLogPixelsX/2, 0, NULL);
		LineTo(hdc, nTmp+nLogPixelsX/2, nMiddle);

		i++;
	} while(nTmp < nCx+iHOffset);


	// y axis ruler
	i=0;
	do {
		nTmp = i*nLogPixelsY;
		if(nTmp>0) {
			sprintf(strLabel, "%d", i);
			TextOut(hdc, 2, nTmp-abs(logfont.lfHeight)*2/3, strLabel, strlen(strLabel));
		}
		
		for(j=1; j<4; j++) {
			MoveToEx(hdc, 0, nTmp+j*nLogPixelsY/8, NULL);
			LineTo(hdc, nShort, nTmp+j*nLogPixelsY/8);
		}
		for(j=5; j<8; j++) {
			MoveToEx(hdc, 0, nTmp+j*nLogPixelsY/8, NULL);
			LineTo(hdc, nShort, nTmp+j*nLogPixelsY/8);
		}

		MoveToEx(hdc, 0, nTmp+nLogPixelsY/2, NULL);
		LineTo(hdc, nMiddle, nTmp+nLogPixelsY/2);

		i++;
	} while(nTmp < nCy+iVOffset);


	SelectObject(hdc, oldHpen);
	DeleteObject((HGDIOBJ) hpen);

	SelectObject(hdc, hfontOld);
	DeleteObject((HGDIOBJ) hfont);
	// ruler drawing finished

	if(nRulerAndGrid<2) {// no grid
		SetMapMode(hdc, nOldMapMode);
		SetWindowOrgEx(hdc, ptWindowOrg.x, ptWindowOrg.y, NULL);
		SetViewportOrgEx(hdc, ptViewportOrg.x, ptViewportOrg.y, NULL);  
		SetWindowExtEx(hdc, szWindowExt.cx, szWindowExt.cy, NULL);
		SetViewportExtEx(hdc, szViewportExt.cx, szViewportExt.cy, NULL);
		SetBkMode(hdc, nOldBkMode);
		SetTextAlign(hdc, nOldTextAlign);
		return 0;
	}
	

	// draw grid at the long ruler tick position
	hpen = CreatePen(PS_SOLID, 1, RGB(0, 0, 128)); // Navy color
	if(!hpen)
		return -1;
	oldHpen = SelectObject(hdc, hpen);

	// x axis grid at long ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsX;
		if(nTmp>0) {
			MoveToEx(hdc, nTmp, nLong, NULL);
			LineTo(hdc, nTmp, (int)((rect.bottom+iVOffset)/(iMFScale*iMFCyScale)));
		}
		i++;
	} while(nTmp < nCx+iHOffset);

	// y axis grid at long ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsY;
		if(i>=10)
			nLong = (int) (abs(logfont.lfHeight)*1.5);
		if(nTmp>0) {
			MoveToEx(hdc, nLong, nTmp, NULL);
			LineTo(hdc, (int)((rect.right+iHOffset)/(iMFScale*iMFCxScale)), nTmp);
		}
		i++;
	} while(nTmp < nCy+iVOffset);
	SelectObject(hdc, oldHpen);
	DeleteObject((HGDIOBJ) hpen);

	if(nRulerAndGrid<3) { // no grid at middle and short ruler tick position
		SetMapMode(hdc, nOldMapMode);
		SetWindowOrgEx(hdc, ptWindowOrg.x, ptWindowOrg.y, NULL);
		SetViewportOrgEx(hdc, ptViewportOrg.x, ptViewportOrg.y, NULL);  
		SetWindowExtEx(hdc, szWindowExt.cx, szWindowExt.cy, NULL);
		SetViewportExtEx(hdc, szViewportExt.cx, szViewportExt.cy, NULL);
		SetBkMode(hdc, nOldBkMode);
		SetTextAlign(hdc, nOldTextAlign);
		return 0;
	}
		

	// draw grid at the middle ruler tick position
	hpen = CreatePen(PS_SOLID, 1, RGB(0, 128, 0)); // Green color
	if(!hpen)
		return -1;
	oldHpen = SelectObject(hdc, hpen);

	// x axis grid at the middle ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsX;
		MoveToEx(hdc, nTmp+nLogPixelsX/2, nMiddle, NULL);
		LineTo(hdc, nTmp+nLogPixelsX/2, (int)((rect.bottom+iVOffset)/(iMFScale*iMFCyScale)));
		i++;
	} while(nTmp < nCx+iHOffset);

	// y axis grid at the middle ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsY;
		MoveToEx(hdc, nMiddle, nTmp+nLogPixelsY/2, NULL);
		LineTo(hdc, (int)((rect.right+iHOffset)/(iMFScale*iMFCxScale)), nTmp+nLogPixelsY/2);
		i++;
	} while(nTmp < nCy+iVOffset);
	SelectObject(hdc, oldHpen);
	DeleteObject((HGDIOBJ) hpen);

	if(nRulerAndGrid<4) { // no grid at short ruler tick position
		SetMapMode(hdc, nOldMapMode);
		SetWindowOrgEx(hdc, ptWindowOrg.x, ptWindowOrg.y, NULL);
		SetViewportOrgEx(hdc, ptViewportOrg.x, ptViewportOrg.y, NULL);  
		SetWindowExtEx(hdc, szWindowExt.cx, szWindowExt.cy, NULL);
		SetViewportExtEx(hdc, szViewportExt.cx, szViewportExt.cy, NULL);
		SetBkMode(hdc, nOldBkMode);
		SetTextAlign(hdc, nOldTextAlign);
		return 0;
	}


	// draw grid at the short ruler tick position
	hpen = CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
	if(!hpen)
		return -1;
	oldHpen = SelectObject(hdc, hpen);

	// x axis grid at the middle ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsX;
		for(j=1; j<4; j++) {
			MoveToEx(hdc, nTmp+j*nLogPixelsX/8, nShort, NULL);
			LineTo(hdc, nTmp+j*nLogPixelsX/8, (int)((rect.bottom+iVOffset)/(iMFScale*iMFCyScale)));
		}
		for(j=5; j<8; j++) {
			MoveToEx(hdc, nTmp+j*nLogPixelsX/8, nShort, NULL);
			LineTo(hdc, nTmp+j*nLogPixelsX/8, (int)((rect.bottom+iVOffset)/(iMFScale*iMFCyScale)));
		}
		i++;
	} while(nTmp < nCx+iHOffset);

	// y axis grid at the middle ruler tick position
	i=0;
	do {
		nTmp = i*nLogPixelsY;
		for(j=1; j<4; j++) {
			MoveToEx(hdc, nShort, nTmp+j*nLogPixelsY/8, NULL);
			LineTo(hdc, (int)((rect.right+iHOffset)/(iMFScale*iMFCxScale)), nTmp+j*nLogPixelsY/8);
		}
		for(j=5; j<8; j++) {
			MoveToEx(hdc, nShort, nTmp+j*nLogPixelsY/8, NULL);
			LineTo(hdc, (int)((rect.right+iHOffset)/(iMFScale*iMFCxScale)), nTmp+j*nLogPixelsY/8);
		}
		i++;
	} while(nTmp < nCy+iVOffset);
	SelectObject(hdc, oldHpen);
	DeleteObject((HGDIOBJ) hpen);

	SetMapMode(hdc, nOldMapMode);
	SetWindowOrgEx(hdc, ptWindowOrg.x, ptWindowOrg.y, NULL);
	SetViewportOrgEx(hdc, ptViewportOrg.x, ptViewportOrg.y, NULL);  
	SetWindowExtEx(hdc, szWindowExt.cx, szWindowExt.cy, NULL);
	SetViewportExtEx(hdc, szViewportExt.cx, szViewportExt.cy, NULL);
	SetBkMode(hdc, nOldBkMode);
	SetTextAlign(hdc, nOldTextAlign);

	return 0;
}

// Yanghui


private	imeth	LRESULT	process_wm_paint(object	self, 
					 HWND	hwnd, 

					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	PAINTSTRUCT	ps;
	object		seq, obj;

	BeginPaint(iHwnd, &ps);

	gDrawRulerAndGrid(self, ps.hdc, iRulerAndGrid);  // Yanghui

//	SetBkColor(ps.hdc, gColor(iBackBrush)); 
	SetBkMode(ps.hdc, TRANSPARENT);

	// First output hidden control vectors and all text vectors
	for (seq=gSequence(iClientArea) ; obj = gNext(seq) ; )
//		if (!iInModifyMode || !gIsKindOf(gValue(obj), ControlVector))
		if (!gIsKindOf(gValue(obj), ControlVector) || gHiddenStatus(gControl(gValue(obj))) == 1)
			gOutputVector(gValue(obj), &ps, iVOffset, iHOffset);

	// Now output visible control vectors
	for (seq=gSequence(iClientArea) ; obj = gNext(seq) ; )
//		if (!iInModifyMode || !gIsKindOf(gValue(obj), ControlVector))
		if (gIsKindOf(gValue(obj), ControlVector) && gHiddenStatus(gControl(gValue(obj))) != 1)
			gOutputVector(gValue(obj), &ps, iVOffset, iHOffset);
		
#ifndef unix
#ifdef	WIN32
	if (iHmf)
		if (iEnhancedMetaFile == 1) {
			RECT	rect;
			GetClientRect(iHwnd, &rect);
			rect.left = rect.right / 4;
			rect.right = 3 * rect.right / 4;
			rect.top = rect.bottom / 4;
			rect.bottom = 3 * rect.bottom / 4;
			PlayEnhMetaFile(ps.hdc, iHmf, &rect);
		} else if (iEnhancedMetaFile == 2)
			PlayPlcMetaFile(self, iv, ps.hdc, iHmf, &iMfHdr);
		else 
			PlayMetaFile(ps.hdc, iHmf);
#else
	if (iHmf)
		if (iEnhancedMetaFile == 2)
			PlayPlcMetaFile(self, iv, ps.hdc, iHmf, &iMfHdr);
		else 
			PlayMetaFile(ps.hdc, iHmf);
#endif
#endif
	if (iBmp)
		gDrawBitmap(iBmp);

	EndPaint(iHwnd, &ps);

	return 0L;
}

private	imeth	LRESULT	process_wm_activate(object	self, 
					    HWND	hdlg, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
#ifdef	WIN32
	unsigned	arg = LOWORD(wParam);
#else
	unsigned	arg = wParam;
#endif
	if (iActivateFunctions)
		gExecuteFunctionsObjInt(iActivateFunctions, self, arg);
	switch (arg)  {
	case WA_ACTIVE:
	case WA_CLICKACTIVE:
		iPrevTopic = gSetTopic(HelpSystem, iTopic ? gStringValue(iTopic) : NULL);
		break;
	case WA_INACTIVE:
/*		gSetTopic(HelpSystem, iPrevTopic);  */
		break;
	}
	return 0;
}

//#outboth

static	void	update_vert(ivType *iv)
{
	if (iIsToolBar) {
		ShowScrollBar(iHwnd, SB_VERT, FALSE);
		return;
	}
	if (iVOffset > MAX_VRANGE)
		iVOffset = MAX_VRANGE;
	if (iVCSize < iVMax)  {
		SetScrollRange(iHwnd, SB_VERT, 0, MAX_VRANGE, FALSE);
		SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
		ShowScrollBar(iHwnd, SB_VERT, TRUE);
	} else {
		if (iVOffset)  {
			ScrollWindow(iHwnd, 0, iVOffset, NULL, NULL);
			iVOffset = 0;
			UpdateWindow(iHwnd);
		}
		ShowScrollBar(iHwnd, SB_VERT, FALSE);
	}
}

static	void	update_horz(ivType *iv)
{
	if (iIsToolBar) {
		ShowScrollBar(iHwnd, SB_HORZ, FALSE);
		return;
	}
	if (iHOffset > MAX_HRANGE)
		iHOffset = MAX_HRANGE;
	if (iHCSize < iHMax)  {
		SetScrollRange(iHwnd, SB_HORZ, 0, MAX_HRANGE, FALSE);
		SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
		ShowScrollBar(iHwnd, SB_HORZ, TRUE);
	} else {
		if (iHOffset)  {
			ScrollWindow(iHwnd, iHOffset, 0, NULL, NULL);
			iHOffset = 0;
//			UpdateWindow(iHwnd);
		}
		ShowScrollBar(iHwnd, SB_HORZ, FALSE);
	}
}

private imeth	void	pUpdateMax(object self)
{
	object	seq, obj;
	int	pm;

	if (!iClientArea  || gIsKindOf(self, Control))
		return;
	
	pm = gSetScalingMode(Application, SM_PIXELS);
	
	iVMax = iHMax = 0;
	for (seq=gSequence(iClientArea) ; obj = gNext(seq) ; )
		gUpdateMax(gValue(obj), &iVMax, &iHMax);

	gSetScalingMode(Application, pm);
}

//#out1

imeth	gVertShift(int rows)
{
	object	seq, obj;
	int	vMax=0, hMax=0, h, v;

	gScaleToPixels(Application, &rows, NULL, iFont);
	rows = -rows;
	for (seq=gSequence(ActiveArea) ; obj = gNext(seq) ; )
		if (gVertShiftTV(gValue(obj), rows, &v, &h))
			gDeepDispose(obj);
		else  {
			hMax = MAX(hMax, h);
			vMax = MAX(vMax, v);
		}
	if (!Paused) {
		iHMax = hMax;
		iVMax = vMax;
		if (iHwnd)  {
			update_vert(iv);
			update_horz(iv);
//  Use the following two lines OR the InvalidateRect
//  The ScrollWindow is more effecient but the if turns out to be rather
//  complex (haven't figured it out yet...)
//		if (???)
//			ScrollWindow(iHwnd, 0, rows, NULL, NULL);
			InvalidateRect(iHwnd, NULL, TRUE);
		}
	}
	return self;
}

static	void	update_parts(ivType *iv)
{
	if (iStatusBar)
		gUpdateState(iStatusBar);
	if (iToolBar)
		gUpdateState(iToolBar);
	if (iClientWind)
		gUpdateState(iClientWind);
}


private	imeth	LRESULT	process_wm_vscroll(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	object seq, obj;
	int	lineHeight;
	int	inc;
#ifdef	WIN32
	SCROLLINFO scrollInfo;         // Yanghui
#endif
	if (!iFont  ||  iIsToolBar)
		return 1L;
	lineHeight = gLineHeight(iFont);
	switch (LOWORD(wParam))  {
	case SB_TOP:
		inc = -iVOffset;
		break;
	case SB_BOTTOM:
		break;
	case SB_LINEUP:
		inc = -lineHeight;
		break;
	case SB_LINEDOWN:
		inc = lineHeight;
		break;
	case SB_PAGEUP:
		inc = -(iVCSize / lineHeight) * lineHeight;
		break;
	case SB_PAGEDOWN:
		inc = (iVCSize / lineHeight) * lineHeight;
		break;
	case SB_THUMBTRACK:
 	
// Yanghui:
// For WIN32: if you have data in the client area longer than (2^16 - 1) pixel
// the HIWORD(wParam) will give the wrong number, use GetScrollInfo instead
// For WIN16: In order to fix it you have to scale the Range of the scroll bar

#ifdef WIN32
		// inc = HIWORD(wParam) - iVOffset;  

		memset(&scrollInfo, 0, sizeof(SCROLLINFO));
		scrollInfo.cbSize = sizeof(SCROLLINFO); 
		scrollInfo.fMask = SIF_TRACKPOS;

		// Call GetScrollInfo to get current tracking position in scrollInfo.nTrackPos 
		if (!GetScrollInfo(hwnd, SB_VERT, &scrollInfo) )
			return 1L; // GetScrollInfo failed   

		inc = scrollInfo.nTrackPos - iVOffset;
// Yanghui
#else
		inc = LOWORD(lParam) - iVOffset;
#endif
		break;
	default:
		inc = 0;
		break;
	}
	if (iVOffset + inc < 0)
		inc = -iVOffset;
	if (iVOffset + inc > MAX_VRANGE)
		inc = MAX_VRANGE - iVOffset;
	if (inc)  {
		iVOffset += inc;
		ScrollWindow(iHwnd, 0, -inc, NULL, NULL);
		SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
		// UpdateWindow(iHwnd); 
		update_parts(iv);
	}
	
	return 0L;
}



private	imeth	LRESULT	process_wm_hscroll(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	int	aveCharWidth;
	int	inc;
#ifdef	WIN32
	SCROLLINFO scrollInfo;         // Yanghui
#endif
	if (ClassOf(self) == ListBox)
		return gCallDefaultProc(self, mMsg, wParam, lParam);
	if (!iFont  ||  iIsToolBar)
		return 1L;
	aveCharWidth = gAveCharWidth(iFont);
	switch (LOWORD(wParam))  {
	case SB_TOP:
		inc = -iHOffset;
		break;
	case SB_BOTTOM:
		break;
	case SB_LINEUP:
		inc = -aveCharWidth;
		break;
	case SB_LINEDOWN:
		inc = aveCharWidth;
		break;
	case SB_PAGEUP:
		inc = -(iHCSize / aveCharWidth) * aveCharWidth;
		break;
	case SB_PAGEDOWN:
		inc = (iHCSize / aveCharWidth) * aveCharWidth;
		break;
	case SB_THUMBTRACK:
// Yanghui:
// For WIN32: if you have data in the client area wider than (2^16 - 1) pixel
// the HIWORD(wParam) will give the wrong number, use GetScrollInfo instead
// For WIN16: In order to fix it you have to scale the Range of the scroll bar

#ifdef WIN32
		// inc = HIWORD(wParam) - iHOffset;

		memset(&scrollInfo, 0, sizeof(SCROLLINFO));
		scrollInfo.cbSize = sizeof(SCROLLINFO); 
		scrollInfo.fMask = SIF_TRACKPOS;

		// Call GetScrollInfo to get current tracking position in scrollInfo.nTrackPos 
		if (!GetScrollInfo(hwnd, SB_HORZ, &scrollInfo) )
			return 1L; // GetScrollInfo failed   

		inc = scrollInfo.nTrackPos - iHOffset;
// Yanghui

#else
		inc = LOWORD(lParam) - iHOffset;
#endif
		break;
	default:
		inc = 0;
		break;
	}
	if (iHOffset + inc < 0)
		inc = -iHOffset;
	if (iHOffset + inc > MAX_HRANGE)
		inc = MAX_HRANGE - iHOffset;
	if (inc)  {
		iHOffset += inc;
		ScrollWindow(iHwnd, -inc, 0, NULL, NULL);
		SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
		// UpdateWindow(iHwnd);
		update_parts(iv);
	}
	return 0L;
}


private	imeth	LRESULT	process_wm_size(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	RECT	r;

	if (wParam != SIZE_MINIMIZED ) {
		iHCSize = LOWORD(lParam);
		iVCSize = HIWORD(lParam);
		pUpdateMax(self);
		if (ClassOf(self) != ListBox  &&  ClassOf(self) != ComboBox) {
			update_vert(iv);
			update_horz(iv);
		}
		InvalidateRect(iHwnd, NULL, TRUE);  // Yanghui
		
		if (ClassOf(self)!=ComboBox) 
			GetWindowRect(iHwnd, &r);
		else
			SendMessage(iHwnd, CB_GETDROPPEDCONTROLRECT, 0, (LPARAM)((LPRECT)&r));

		iVSize = r.bottom - r.top;
		iHSize = r.right - r.left;
		
		update_parts(iv);

	}
	return 0L;
}

static	void	updateDockedWindows(ivType *iv, int hp, int vp)
{
	if (iDockedChildren) {
		int	pm = gSetScalingMode(Application, SM_PIXELS);
		object	seq, obj;
		int	x, y, dx, dy;
		RECT	rect;

		GetWindowRect(iHwnd, &rect);
		dx = rect.left - hp;
		dy = rect.top - vp;
		for (seq=gSequence(iDockedChildren) ; obj = gNext(seq) ; ) {
			gGetPosition(obj, &y, &x);
			gSetPosition(obj, y + dy, x + dx);
		}
		gSetScalingMode(Application, pm);
	}
}

private	imeth	LRESULT	process_wm_move(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (!IsIconic(hwnd) ) {
		RECT	rect;
		WINDOWPLACEMENT	wp;

		GetWindowPlacement(iHwnd, &wp);

		GetWindowRect(iHwnd, &rect);
		if (GetWindowLong(iHwnd, GWL_STYLE) & WS_CHILD) {
			POINT	p;
			HWND	h = GetParent(iHwnd);
			p.x = rect.left;
			p.y = rect.top;
			ScreenToClient(h, &p);
			rect.left = p.x;
			rect.top = p.y;
		}
		updateDockedWindows(iv, iHPos, iVPos);
		
		iHPos = rect.left;
		iVPos = rect.top;
	}
	return 0L;
}

imeth	gDockParent(object wind)
{
	if (!iDockedParents)
		iDockedParents = gNew(Set);

	gAdd(iDockedParents, wind);
	
	return self;
}

imeth	gDock(object wind)
{
	if (!iDockedChildren)
		iDockedChildren = gNew(Set);

	gAdd(iDockedChildren, wind);
	gDockParent(wind, self);
	
	return self;
}

imeth	gUndockParent(object wind)
{
	if (iDockedParents)
		gRemoveObj(iDockedParents, wind);
	
	return self;
}

imeth	gUndock(object wind)
{
	if (iDockedChildren)
		gRemoveObj(iDockedChildren, wind);
	gUndockParent(wind, self);
	
	return self;
}

imeth	int	gModifyMode()
{
	return iInModifyMode;
}

imeth	gSetModifyMode(int i)
{
	iInModifyMode = i;
	return self;
}

imeth	int	gModifyChildren()
{
	if (iClientWind)
		return gModifyChildren(iClientWind);
	
	return iModifyChildren;
}

imeth	int	gSetModifyChildren(int mode)
{
	int	p;
	
	if (iClientWind)
		return gSetModifyChildren(iClientWind, mode);

	p = iModifyChildren;
	iModifyChildren = mode;

	// Yanghui:
	if(iControls) {
		object seq, ctl;
		if(iModifyChildren)
			for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
				if( ClassOf(ctl) == LineControl || ClassOf(ctl) == RectControl || 
					ClassOf(ctl) == StaticTextControl )
					EnableWindow(gHandle(ctl), TRUE);  // No matter what the state of the control is,
					                                   // let it alive (I did not change the stored value).
			}
		else
			for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
				if( ClassOf(ctl) == LineControl || ClassOf(ctl) == RectControl || 
					ClassOf(ctl) == StaticTextControl )
					gDisable(ctl);  // set the control to the disabled state
			}
	}
	// Yanghui

	return p;
}


#define	MOVE_CTL	(wParam == MK_LBUTTON)
#define	SIZE_CTL	(wParam == (MK_CONTROL | MK_LBUTTON))
#define	DEL_CTL		(wParam == (MK_CONTROL | MK_RBUTTON))


private	imeth	LRESULT	process_wm_buttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	object	parent;
	RECT    rect;

/*  Windows 2000 vs. other OS's gets different messages when clicking on menu items  */
	if (iMenuID && iMenu) {	// Clicking a menu item
		int	orval = 0;
		ifun	fun;

		if (mMsg == WM_RBUTTONDOWN)
			orval = MK_RBUTTON;
		else if (mMsg == WM_LBUTTONDOWN)
			orval = MK_LBUTTON;
		
		fun = gGetMouseFunction(iMenu, iMenuID, wParam | orval);

		if (fun)
			fun(iMenu, iMenuID, (unsigned) wParam | orval);
	}

	iInModifyMode = GetWindowLong(iHwnd, GWL_STYLE) & WS_CHILD  &&
		(parent=gGetParent(self))  && gModifyChildren(parent);
	if (iInModifyMode) {
		object	ctl;
		int	move_or_size = MOVE_CTL || SIZE_CTL;
				
		if (move_or_size ) {
			POINT	p;

			GetCursorPos(&p);
			iMHPos = p.x;
			iMVPos = p.y;
			SetCapture(hwnd);
		}

		// Yanghui:  for group selection
		if ((ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  
		    !gIsKindOf(ctl, Control) && !gIsKindOf(ctl, ToolBar)) {
			POINT	p;
			gRmAllOfDWs(DragWindow);
			GetCursorPos(&p);
			ScreenToClient(gHandle(self), &p);

			iRectTrack.left = p.x;
			iRectTrack.top = p.y;
			iRectTrack.right = p.x;
			iRectTrack.bottom = p.y;
			
			if(iTrackRectObj)
				iTrackRectObj = gCancelTracking(iTrackRectObj);

			iTrackRectObj = gInitTracking(TrackRect, gHandle(self));
			gDrawTrackRect(iTrackRectObj, &iRectTrack, TRUE);
		}
		// Yanghui


		if ((ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  gIsKindOf(ctl, Control) ) {
			if (DEL_CTL) {
				//  really hide!
				gSetModifyChildren(parent, 0);
				gHide(ctl);
				gSetModifyChildren(parent, 1);
				gSetTabOrder(ctl, -1);		//  mark as deleted
				gAddToDeletedCtls(parent, ctl);  // keep track of the deleted controls. Yanghui
			} else if (move_or_size) {

				COLORREF	wincolor = gColor(gGetBackBrush(gDialog(ctl)));
			
				iHoldColor = gColor(gGetBackBrush(ctl));

				if(ClassOf(ctl) != RectControl && ClassOf(ctl) != LineControl)  // Yanghui
					if (wincolor == iHoldColor)
						if (wincolor == RGB(255, 255, 255))
							gBackBrush(ctl, vNew(SolidBrush, 192, 192, 192));
						else
							gBackBrush(ctl, vNew(SolidBrush, 255, 255, 255));
				
				gInvalidate3dCtl(ctl);
				
				// the above code changes the background color of the control if it is the 
				// same as the dialog. so when the control moves, it is visible. 
				// Note: the following two statwments are needed.
				InvalidateRect(hwnd, NULL, TRUE);
				UpdateWindow(hwnd);
				// Yanghui
			}
		}
	}
	return 0L;
}

imeth	gUpdateControlVectors(object ctl)
{
	object	seq, obj, vec;
	
	for (seq=gSequence(iClientArea) ; obj = gNext(seq) ; ) {
		vec = gValue(obj);
		if (gIsKindOf(vec, ControlVector)  &&  ctl == gControl(vec))
			gUpdateWithOffset(vec, &iVMax, &iHMax, iVOffset, iHOffset);
	}
	return self;
}

/*
private	imeth	pUpdateControlVectors(object self, object ctl)
{
	object	seq, obj, vec;
	
	for (seq=gSequence(iClientArea) ; obj = gNext(seq) ; ) {
		vec = gValue(obj);
		if (gIsKindOf(vec, ControlVector)  &&  ctl == gControl(vec))
			gUpdateWithOffset(vec, &iVMax, &iHMax, iVOffset, iHOffset);
	}
	return self;
}
*/

private	imeth	LRESULT	process_wm_buttonup(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{

/*  Windows 2000 vs. other OS's gets different messages when clicking on menu items  */
	if (iMenuID && iMenu) {	// Clicking a menu item
		int	orval = 0;
		ifun	fun;

		if (mMsg == WM_RBUTTONUP)
			orval = MK_RBUTTON;
		else if (mMsg == WM_LBUTTONUP)
			orval = MK_LBUTTON;
		
		fun = gGetMouseFunction(iMenu, iMenuID, wParam | orval);

		if (fun)
			fun(iMenu, iMenuID, (unsigned) wParam | orval);
	} else if (mMsg == WM_RBUTTONUP && (iShortcutMenuID || iShortcutMenuStr)) {
		object	menu, seq, obj;
		if (iShortcutMenuStr)
			menu = gLoadShortcutMenuStr(ShortcutMenu, gStringValue(iShortcutMenuStr), iShortcutSubmenu);
		else
			menu = gLoadShortcutMenu(ShortcutMenu, iShortcutMenuID, iShortcutSubmenu);
		gSetParent(menu, self);

		for (seq = gSequence(iShortcutFunctions); obj = gNext(seq); )
			gAssociate(menu, gIntKey(obj), (vfun) gPointerValue(gValue(obj)));
		
		gPerform(menu);

		gDispose(menu);
	}

	if (iInModifyMode) {
		object	ctl;

		// Yanghui: for group selection
		if ( (ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  
			!gIsKindOf(ctl, Control) && !gIsKindOf(ctl, ToolBar)) {
				object  seq;
				RECT    rectCtl;

				if(iTrackRectObj) 
					iTrackRectObj=gCancelTracking(iTrackRectObj);
				
				if(iControls) {
					for (seq=gSequence(iControls) ; ctl = gNext(seq); ) {
						GetWindowRect(gHandle(ctl), &rectCtl);
						gScreenToClientRect(TrackRect, gHandle(self), &rectCtl);
						
						if( gRectInRect(TrackRect, &iRectTrack, &rectCtl) && gHiddenStatus(ctl)!=1) 
							gNewDragWindow(DragWindow, ctl, FALSE); 
					}
				}
				SetRectEmpty(&iRectTrack);
		}
		// Yanghui

		ReleaseCapture();

		InvalidateRect(hwnd, NULL, TRUE);

		iInModifyMode = 0;
		if ((ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  gIsKindOf(ctl, Control) ) {
			object		dlg = gDialog(self);
			COLORREF	ctlcolor = gColor(gGetBackBrush(ctl));
			ivType		*iv2 = ivPtr(dlg);
			RECT        rect;  // Yanghui

			if(ClassOf(ctl) != RectControl && ClassOf(ctl) != LineControl)  // Yanghui
				if (ctlcolor != iHoldColor) {
					gBackBrush(ctl, vNew(SolidBrush,
						     (int) GetRValue(iHoldColor),
						     (int) GetGValue(iHoldColor),
						     (int) GetBValue(iHoldColor)));
				}
			gInvalidate3dCtl(ctl);
			gUpdateControlVectors(dlg, ctl);

			update_vert(iv2);
			update_horz(iv2);

			// Yanghui:
			if(ClassOf(ctl) == RectControl || ClassOf(ctl) == LineControl)  {
				RECT rect;
				gGetWindowRect(ctl, &rect);
				InvalidateRect(GetParent(hwnd), &rect, TRUE);
				UpdateWindow(GetParent(hwnd));   
			}
			
			InvalidateRect(hwnd, NULL, TRUE); 
			UpdateWindow(hwnd);                
			// Yanghui
		}

		// Yanghui: the following code update the scroll bar of the parent window
		//          if this object is a control
		if(gIsKindOf(self, Control)) {
			object parentObj = gGetParent(self);
			if( parentObj && gIsKindOf(parentObj, Window) )
				gUpdateScrollBar(parentObj);
		}
		// Yanghui

	}
	return 0L;
}

private	imeth	LRESULT	process_wm_mousemove(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	object	ctl;

	// Yanghui: for group selection
	if (iInModifyMode  && (ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  
		!gIsKindOf(ctl, Control) && !gIsKindOf(ctl, ToolBar)) {
			POINT	p;
			if(iTrackRectObj)
				iTrackRectObj = gCancelTracking(iTrackRectObj);

			GetCursorPos(&p);
			ScreenToClient(gHandle(self), &p);
			iRectTrack.right = p.x;
			iRectTrack.bottom = p.y;

			iTrackRectObj = gInitTracking(TrackRect, gHandle(self));
			gDrawTrackRect(iTrackRectObj, &iRectTrack, TRUE);
	}
	// Yanghui

	if (iInModifyMode  &&  (ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))  &&  gIsKindOf(ctl, Control)) {
/******************************************************
 this procedure only handles move changed by Yanghui
		if (SIZE_CTL) {
			POINT	p;
			
			GetCursorPos(&p);
			iHSize += p.x - iMHPos;
			iVSize += p.y - iMVPos;
			iMHPos = p.x;
			iMVPos = p.y;
			SetWindowPos(iHwnd, (HWND)0, iHPos, iVPos, iHSize, iVSize, (SWP_NOMOVE | SWP_NOZORDER));
		} else if (MOVE_CTL) {
*****************************************************/
		if (MOVE_CTL) {
			POINT	p;

			GetCursorPos(&p);

			iHPos += p.x - iMHPos;
			iVPos += p.y - iMVPos;
			iVHPos += p.x - iMHPos;
			iVVPos += p.y - iMVPos;

			if (iHPos<0) {
				iVHPos -= iHPos;
				iHPos -= iHPos;
			}

			if (iVPos<0) {
				iVVPos -= iVPos;
				iVPos -= iVPos;
			}

			iMHPos = p.x;
			iMVPos = p.y;
			SetWindowPos(iHwnd, (HWND)0, iHPos, iVPos, iHSize, iVSize, (SWP_NOSIZE | SWP_NOZORDER));
		}
	}
	return 0L;
}

private	imeth	LRESULT	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	iOrgWndProc;
	switch (wParam)  {
	case VK_HOME:
		SendMessage(hwnd, WM_VSCROLL, SB_TOP, 0L);
		break;
	case VK_END:
		SendMessage(hwnd, WM_VSCROLL, SB_BOTTOM, 0L);
		break;
	case VK_PRIOR:
		SendMessage(hwnd, WM_VSCROLL, SB_PAGEUP, 0L);
		break;
	case VK_NEXT:
		SendMessage(hwnd, WM_VSCROLL, SB_PAGEDOWN, 0L);
		break;
	case VK_UP:
		SendMessage(hwnd, WM_VSCROLL, SB_LINEUP, 0L);
		break;
	case VK_DOWN:
		SendMessage(hwnd, WM_VSCROLL, SB_LINEDOWN, 0L);
		break;
	case VK_LEFT:
		SendMessage(hwnd, WM_HSCROLL, SB_PAGEUP, 0L);
		break;
	case VK_RIGHT:
		SendMessage(hwnd, WM_HSCROLL, SB_PAGEDOWN, 0L);
		break;
	case VK_DELETE:
		/* Add this to allow masking on dynamic TextControls to work.
		   If this option is removed, dynamic TextControls will no longer
		   handle the delete key appropriately. */
		break;
	default:
		DFLT;
		break;
	}
	return 0L;
}

private	imeth	LRESULT	process_wm_char(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (!iRaw  &&  wParam == '\b')  {
		if (iNchars)
			iNchars--;
	} else if (iNchars < iBsize) {
		int	c = wParam == '\r' ? (WPARAM) '\n' : wParam;
		iBuf[iNchars++] = c;
		if (iEcho) {
			gPutc(self, c);
			gRedrawWindow(self);
		}
	}
	return 0L;
}

private	imeth	LRESULT	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	object	dlg;
	
	iPrevTopic = gSetTopic(HelpSystem, iTopic ? gStringValue(iTopic) : NULL);

	if (gIsKindOf(self, Control)  &&  gIsKindOf((dlg = gDialog(self)), Window))
		gUpdateScrollData(dlg, self);
	
	return 0L;
}

private	imeth	LRESULT	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	/*  this doesn't work because dialogs receive an ACTIVATE
		    message before this KILLFOCUS
	gSetTopic(HelpSystem, iPrevTopic);    */
	return 0L;
}

private	imeth	LRESULT	process_wm_menuselect(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	int	mItem;	// menu item or submenu index
	int	mFlags;	// menu flags
	HMENU	hMenu;	// handle to menu clicked

#ifdef WIN32
	mItem = LOWORD(wParam);
	mFlags = HIWORD(wParam);
	hMenu = (HMENU) lParam;
#else
	mItem = wParam;
	mFlags = LOWORD(lParam);
	hMenu = (HMENU) HIWORD(lParam);
#endif
	iMenuID = 0;
	if ((mFlags != 0xffff || hMenu) && !(mFlags & (MF_POPUP | MF_SEPARATOR)))
		iMenuID = mItem;
	
	return 0L;
}

static
LRESULT	CALLBACK  WindowProc(HWND	hwnd, 
			     UINT	mMsg, 
			     WPARAM	wParam, 
			     LPARAM	lParam)
{
	object	h;
	HC_VARS;

	if (mMsg == WM_NCCREATE) {
		object	obj = (object) ((CREATESTRUCT *) lParam)->lpCreateParams;
		if (IsObj(obj)) {
			if (!gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hwnd))
				HC_NEW(WINDOW_HANDLE_CACHE, hwnd, obj);
			HC_UPDATE(WINDOW_HANDLE_CACHE, hwnd);
			iHwnd = hwnd;
		} else {
			HC_UPDATE(WINDOW_HANDLE_CACHE, hwnd);
			if (iv)
				iHwnd = hwnd;
		}
	} else
		HC_UPDATE(WINDOW_HANDLE_CACHE, hwnd);

	if (self)
		h = gFindValueInt(iMessageHandlers, mMsg);
	if (self  &&  h)
		return gProcessWindowMsg(h, self, hwnd, mMsg, wParam, lParam, iOrgWndProc != WindowProc ? iOrgWndProc : NULL);
	else 
		DFLT;
}

private	imeth	LRESULT	process_wm_command(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	lfun		fp;
	unsigned	cmd, id;
	HWND		chldwind;
	object		ctl;

	id = LOWORD(wParam);
	chldwind = (HWND) lParam;

#ifdef WIN32
	cmd = HIWORD(wParam);
#else
	cmd = HIWORD(lParam);
#endif
	switch (cmd) {

	case BN_CLICKED:  /*  button clicked or menu clicked  */

		if (chldwind)  {   	/*  for child control  */
			ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, chldwind);
			if (ctl) {
				if (ClassOf(ctl) == PushButton) {
					gPerform(ctl);
					return 0L;
				} else if (ClassOf(ctl) == RadioButton) {
					gSetShortValue(ctl, 1);
					gPerform(ctl);
					return 0L;
				} else if (ClassOf(ctl) == CheckBox) {
					gSetShortValue(ctl, !gShortValue(ctl));
					gUpdate(ctl);
					gPerform(ctl);
					return 0L;
				} else if (ClassOf(ctl) == ButtonWindow)
					gProcessCmd(ctl, id, cmd);
				}
		} else {	/*  for menu  */
			int	shift, control;
			int	orval = 0;
			ifun	fun;
			
			if (!self  ||  !iMenu)
				break;

			shift = GetKeyState(VK_SHIFT) < 0 ? MK_SHIFT : 0;
			control = GetKeyState(VK_CONTROL) < 0 ? MK_CONTROL : 0;
			orval = shift | control | MK_LBUTTON;
			fun = gGetMouseFunction(iMenu, id, orval);
			if (fun)
				fun(iMenu, iMenuID, (unsigned) wParam | orval);
			else if (!shift && !control) {
				fp = (lfun) gMenuFunction(iMenu, id);
				if (!fp)
					break;
				if (IsObj((object)fp)  &&  ClassOf(fp) == JavaCallbackClassSurrogate) {
					object	callback = (object)fp;
					BOOL	bErr;
					return gPerformJavaMenuCallback(callback, self, id);
				} else if (SchemeClassSurrogate  &&  IsObj((object)fp)  &&  ClassOf(fp) == String) {
					char	cmd[100], ns[80];
					object	ret;
					long	res = 0;
					sprintf(cmd, "(%s (int->object %lld) %u)",
						gFunctionName(SchemeClassSurrogate, (object)fp),
						PTOLL(self), id);
					ret = gExecuteInNamespace(SchemeClassSurrogate,
								  gNamespaceName(SchemeClassSurrogate, (object)fp, ns), 
								  cmd);
					if (IsObj(ret)) {
						res = gLongValue(ret);
						gDispose(ret);
					}
					return res;
				} else if (JavaScriptClassSurrogate  &&  IsObj((object)fp)  &&  ClassOf(fp) == JavaScriptString) {
					long	res = 0;
					object	ret;
					char	cmd[128];
					
					sprintf(cmd, "%s(StringToObject(\"%lld\"), %ld)", gStringValue((object)fp), PTOLL(self), (long) id);
					ret = gExecuteString(JavaScriptClassSurrogate, cmd);
					if (IsObj(ret)) {
						if (ClassOf(ret) == LongInteger)
							res = gLongValue(ret);
						gDispose(ret);
					}
					return res;
				} else
					return fp(self, id);
				break;
			}
		}
		break;
	case LBN_DBLCLK:
//	case CBN_DBLCLK:    same value as LBN_DBLCLK 
		ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, chldwind);
		if (ctl  &&  (gIsKindOf(ctl, ListBox)   ||
			      gIsKindOf(ctl, ComboBox)))  {
			gPerform(ctl);
		}
		break;
	case LBN_SELCHANGE:
/*	case CBN_SELCHANGE:    same value as LBN_SELCHANGE  */
		ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, chldwind);
		if (ctl  &&  (gIsKindOf(ctl, ListBox)  ||
			      gIsKindOf(ctl, ComboBox))) {
			gPerformChg(ctl);
		}
		break;
	}
	DFLT;
}

static	LRESULT	process_wm_destroy(object	self, 
				   HWND		hwnd, 
				   UINT		mMsg, 
				   WPARAM	wParam, 
				   LPARAM	lParam)
{
	if (ClassOf(self) == MainWindow)
		PostQuitMessage(0);
	return 0L;
}

static	LRESULT	process_wm_syscolorchange(object	self, 
					  HWND		hwnd, 
					  UINT		mMsg, 
					  WPARAM	wParam, 
					  LPARAM	lParam)
{
	extern	BOOL  (WINAPI *Ctl3dColorChange)(void);
	if (Ctl3dColorChange)
		Ctl3dColorChange();
	return 0L;
}

private	imeth	LRESULT	process_wm_setcursor(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iWndCursor)
		SetCursor(gHandle(iWndCursor));
	return 0L;
}

private	imeth	LRESULT	process_wm_close(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	int	r = 1;
	if (gIsKindOf(self, MainWindow)  &&  gInModal(MessageDispatcher))
		return 0L;
	if (iFunctions) {
		object	fun;
		fun = iFunctions;
		iFunctions = NULL;
		r = gExecuteFunctionsObj(fun, self);
		if (IsObj(self))
			iFunctions = fun;
		else {
			r = 0;
			gDeepDispose(fun);
		}
	}
	if (r) {
		if (gIsKindOf(self, MainWindow))
			gDisposeExitObjects(Application);
		if (iAutoDispose)
			gDispose(self);
		else  {
			SetMenu(hwnd, (HMENU) 0);
			if (iControlDict)
				iControlDict = gDeepDispose(iControlDict);
			if (iControls)
				iControls = gDispose(iControls);
			dispose_children(iv);
			gFreeHelp(HelpSystem, self);
			DestroyWindow(hwnd);
			if (iHwnd)  {
				while (HC_DELETE(WINDOW_HANDLE_CACHE, iHwnd));
				iHwnd = 0;
			}
		}
	}
	if (iModal && !iEndWindow) {
		iModalResult = 0;
		iEndWindow = 1;
	}
	return 0L;
}

private	imeth	LRESULT	process_wm_color(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	HANDLE	hctl;
	object	ctl, bb, tb;
#ifdef	WIN32
	hctl = (HWND) lParam;
#else
	hctl = (HWND) LOWORD(lParam);
#endif
	ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hctl);

/*  This causes a problem with fast tab switching in TabControl - I need to figure out why
	if (gUsingAlternateColor(Application))
	{	
		object cbb=gGetBackBrush(Application);

		if (ctl  && ( gIsKindOf(ctl, ListBox)||gIsKindOf(ctl, PushButton)|| gIsKindOf(ctl, StaticControl)||gIsKindOf(ctl, StaticTextControl)||gIsKindOf(ctl, RadioButton)
			||gIsKindOf(ctl, LineControl)
			||gIsKindOf(ctl, CheckBox)||gIsKindOf(ctl, TextVector)||gIsKindOf(ctl, RectControl))){
			if (!cbb)
				return (BOOL)NULL;			
			SetBkColor((HDC) wParam, gColor(cbb));
			return (BOOL) gHandle(cbb);   //   ???   
		}
		
		if (ctl && RespondsTo(ctl,gDisableStatus)  &&  gDisableStatus(ctl)==1)
		{
			if (!cbb)
				return (BOOL)NULL;
			SetBkColor((HDC) wParam, gColor(cbb));
			return (BOOL) gHandle(cbb);   //   ???   
		}

		if (!ctl)
		{
			char windowClassName[256];
			GetClassName(hctl,windowClassName,256);
			
			if (strcmp(windowClassName,"SysTreeView32")&&strcmp(windowClassName,"ListBox")) //leave tree view alone
			{
				if (!cbb)
 					return (BOOL)NULL;
				SetBkColor((HDC) wParam, gColor(cbb));
				return (BOOL) gHandle(cbb);  
			}
			else
			{
				object br=gNewSolidBrush(SolidBrush,255,255,255);
				SetBkColor((HDC) wParam, gColor(br));
				gDispose(br);
				return (BOOL) gHandle(br); 	

			}
		}
	}
*/
	if (ctl  &&  gIsKindOf(ctl, Control)) {
		bb = gGetBackBrush(ctl);
		tb = gGetTextBrush(ctl);
		SetBkColor((HDC) wParam, gColor(bb));
		SetTextColor((HDC) wParam, gColor(tb));
		return (INT_PTR) gHandle(bb);
	} else if ((bb = gGetBackBrush(self))  &&  (tb = gGetTextBrush(self)) /* &&  !Ctl3dSubclassDlgEx */)  {
		SetBkColor((HDC) wParam, gColor(bb));
		SetTextColor((HDC) wParam, gColor(tb));
		return (INT_PTR) gHandle(bb);
	} else
		return 0L;  //  code needed for Ctl3d stuff
}

private	imeth	pHideDockedWindows(object self)
{ 
	object	seq, obj;

	for (seq=gSequence(iDockedChildren) ; obj = gNext(seq) ; )
		gSetState(obj, SW_HIDE);

	return self;
}

private	imeth	pShowDockedWindows(object self)
{
	object	seq, obj;

	for (seq=gSequence(iDockedChildren) ; obj = gNext(seq) ; )
		gSetState(obj, SW_SHOW);

	return self;
}

private	imeth	LRESULT	process_wm_syscommand(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	if (iDockedChildren) {
		switch (wParam) {
		case SC_MINIMIZE:
			pHideDockedWindows(self);
			break;
		case SC_RESTORE:
			pShowDockedWindows(self);
			break;
		case SC_MAXIMIZE:
			pShowDockedWindows(self);
			break;
		}
	}
	return 0L;
}

static	void	setup_handlers(object obj, ivType *iv)
{

	/*  Set up message handlers   */

	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	gAddHandlerAfter(obj, (unsigned) WM_SYSCOMMAND, process_wm_syscommand);

	gAddHandlerAfter(obj, (unsigned) WM_CREATE, process_wm_create);
	gDefaultProcessingMode(obj, (unsigned) WM_CREATE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_COMMAND, process_wm_command);
	gDefaultProcessingMode(obj, (unsigned) WM_COMMAND, 0);  /*  no auto default processing  */

	if (ClassOf(obj) != StatusBar  &&  ClassOf(obj) != ToolBar) {
		gAddHandlerAfter(obj, (unsigned) WM_PAINT, process_wm_paint);
		gDefaultProcessingMode(obj, (unsigned) WM_PAINT, 0);  /*  no auto default processing  */
	}

	gAddHandlerAfter(obj, (unsigned) WM_SIZE, process_wm_size);
//	gDefaultProcessingMode(obj, (unsigned) WM_SIZE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_MOVE, process_wm_move);

	gAddHandlerAfter(obj, (unsigned) WM_ACTIVATE, process_wm_activate);

	gAddHandlerAfter(obj, (unsigned) WM_DESTROY, process_wm_destroy);
	gDefaultProcessingMode(obj, (unsigned) WM_DESTROY, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CLOSE, process_wm_close);
	gDefaultProcessingMode(obj, (unsigned) WM_CLOSE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SYSCOLORCHANGE, process_wm_syscolorchange);
	gDefaultProcessingMode(obj, (unsigned) WM_SYSCOLORCHANGE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_VSCROLL, process_wm_vscroll); 
	gDefaultProcessingMode(obj, (unsigned) WM_VSCROLL, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_HSCROLL, process_wm_hscroll);
	gDefaultProcessingMode(obj, (unsigned) WM_HSCROLL, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	gDefaultProcessingMode(obj, (unsigned) WM_KEYDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETCURSOR, process_wm_setcursor);

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_MENUSELECT, process_wm_menuselect);

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDOWN, process_wm_buttondown);

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONUP, process_wm_buttonup);

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_buttondown);

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONUP, process_wm_buttonup);

	gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);

#ifdef	WIN32
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORBTN, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORBTN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORDLG, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORDLG, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOREDIT, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOREDIT, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORLISTBOX, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORLISTBOX, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORMSGBOX, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORMSGBOX, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSCROLLBAR, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSCROLLBAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSTATIC, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSTATIC, 0);  /*  no auto default processing  */
#else
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOR, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOR, 0);  /*  no auto default processing  */
#endif

	gAddHandlerAfter(obj, (unsigned) WM_TIMER, process_wm_timer);
}

static	void	setup_builtin_handlers(object obj, ivType *iv)
{

	/*  Set up message handlers   */
	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	gAddHandlerAfter(obj, (unsigned) WM_SYSCOMMAND, process_wm_syscommand);

	gAddHandlerAfter(obj, (unsigned) WM_CREATE, process_wm_create);
	gDefaultProcessingMode(obj, (unsigned) WM_CREATE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_COMMAND, process_wm_command);
	gDefaultProcessingMode(obj, (unsigned) WM_COMMAND, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SIZE, process_wm_size);
//	gDefaultProcessingMode(obj, (unsigned) WM_SIZE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_MOVE, process_wm_move);

	gAddHandlerAfter(obj, (unsigned) WM_DESTROY, process_wm_destroy);
	gDefaultProcessingMode(obj, (unsigned) WM_DESTROY, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CLOSE, process_wm_close);
	gDefaultProcessingMode(obj, (unsigned) WM_CLOSE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SYSCOLORCHANGE, process_wm_syscolorchange);
	gDefaultProcessingMode(obj, (unsigned) WM_SYSCOLORCHANGE, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_VSCROLL, process_wm_vscroll); 
	if (ClassOf(obj) != ListBox  &&  ClassOf(obj) != ComboBox)
		gDefaultProcessingMode(obj, (unsigned) WM_VSCROLL, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_HSCROLL, process_wm_hscroll);
	gDefaultProcessingMode(obj, (unsigned) WM_HSCROLL, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	gDefaultProcessingMode(obj, (unsigned) WM_KEYDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETCURSOR, process_wm_setcursor);

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_MENUSELECT, process_wm_menuselect);

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONUP, process_wm_buttonup);

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDOWN, process_wm_buttondown);

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_buttondown);

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONUP, process_wm_buttonup);

	gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);

	gAddHandlerAfter(obj, (unsigned) WM_TIMER, process_wm_timer);
}

cmeth	gNewWithStr : newWithClass (char *class_name)		/*  should only be called by Window subclasses  */
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	if (class_name)
		iClassName = gNewWithStr(String, class_name);
	else {
		static	long	cn = 1L;
		char	className[40];
		sprintf(className, "Dynace-%lld-%lu", PTOLL(gInstance(Application)), cn++);
		iClassName = gNewWithStr(String, className);
	}
	
	iWndProc = WindowProc;

	DEMO_INC;

/*	iWndExtra = sizeof(object);  room to save window object  */
/*	iStyle = CS_HREDRAW | CS_VREDRAW;   */

	iFonts = gNew(LinkObject);

	iClientArea = gNew(LinkList);

	iMaxLines = 40;

	iBuf = malloc(BUFSIZE);
	iBsize = BUFSIZE;
	iBlock = 1;

	if (cInitFunctions  &&  !gIsKindOf(self, Control))
		iInitFunctions = gDeepCopy(cInitFunctions);

	iCursor = gCopy(gGetCursor(Application));
	iIcon   = gGetIcon(Application);
	gUse(obj, gGetFont(Application));
	iTextBrush = gGetTextBrush(Application);
	iBackBrush = gGetBackBrush(Application);

	iMFScale = 1.0;

	// Yanghui:
	iControlScale = 1.0;
	iFontScale    = 1.0;

	iPrinterScale = GetSystemMetrics(SM_CXSCREEN)/800.0;

	iMFCxScale = 1.0;
	iMFCyScale = 1.0;

	iRulerAndGrid = 0;

	iPageTopMargin = 2;
	iPageBottomMargin = 2;

	SetRectEmpty(&iMFMargins);
	// Yanghui

	setup_handlers(obj, iv);

	return obj;
}

cvmeth	vNew()		/*  should only be called by Window subclasses  */
{
	return newWithClass(self, NULL);
}

/*  used to create a child window which is used as a control (using a built
    in control class)  */

cmeth	gNewBuiltIn(char *class, parent)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iClassName = gNewWithStr(String, class);

	iWndProc = WindowProc;

	iType = 1;

	if (parent)
		gSetParent(obj, parent);

	if (cInitFunctions  &&  !gIsKindOf(self, Control))
		iInitFunctions = gDeepCopy(cInitFunctions);

	iCursor = gCopy(gGetCursor(Application));
	iIcon   = gGetIcon(Application);
//	gUse(obj, gGetFont(Application));
	iTextBrush = gGetTextBrush(Application);
	iBackBrush = gGetBackBrush(Application);

	setup_builtin_handlers(obj, iv);

	return obj;
}

// Yanghui:
cmeth	gCLDNewBuiltIn(char *class)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iClassName = gNewWithStr(String, class);

	if (cInitFunctions  &&  !gIsKindOf(self, Control))
		iInitFunctions = gDeepCopy(cInitFunctions);

	iWndProc = 0;

	iType = 1;

	iCursor = 0;
	iIcon   = 0;
	iTextBrush = 0;
	iBackBrush = 0;

	return obj;
}
// Yanghui

cmeth	gNewDialogControl()
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	if (cInitFunctions  &&  !gIsKindOf(self, Control))
		iInitFunctions = gDeepCopy(cInitFunctions);

	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	iType = 2;

	return obj;
}

/*  This method only used internally by every subclass  */

imeth	gSubclassWindow(HWND hctl)
{
	iOrgWndProc = SubclassWindow(hctl, WindowProc);
	return self;
}

imeth	LRESULT	gCallDefaultProc(UINT	mMsg, 
				 WPARAM	wParam, 
				 LPARAM	lParam)
{
#ifdef	WIN32
	return CallWindowProc((WNDPROC) iOrgWndProc, gHandle(self), mMsg, wParam, lParam);
#else
	return CallWindowProc((FARPROC) iOrgWndProc, gHandle(self), mMsg, wParam, lParam);
#endif
}

imeth	gSetTitle(char *title)	/*  title may also be an object  */
{
	if (iWndTitle)
	{
		//Don't flicker if title didn't change
		char * t;

		if ((t=gStringValue(iWndTitle))!=NULL  &&  title!=NULL  &&  !strcmp(t,title))
			return self;

		gDispose(iWndTitle);
	}
	if (title)
		if (!IsObj((object)title))
			iWndTitle = gNewWithStr(String, title);
		else
			iWndTitle = (object) title;
	else
		iWndTitle = NULL;
	if (iHwnd)
		SetWindowText(iHwnd, (LPCSTR) (iWndTitle ? gStringValue(iWndTitle) : ""));
	return self;
}

imeth	gSetStyle(DWORD style)
{
	iWndStyle = style;
	return self;
}

imeth	DWORD gGetStyle()
{
	return iWndStyle;
}

imeth	gSetExStyle(DWORD style)
{
	iExStyle = style;
	return self;
}

imeth	gUpdateControlPosition(int y, int x)
{
	object dragWindow;  // Yanghui
	gScaleToPixels(Application, &y, &x, iFont);
	iVPos = y;
	iHPos = x;
	if (iHwnd)
		SetWindowPos(iHwnd, HWND_TOP, iHPos, iVPos, 0, 0, (SWP_NOSIZE | SWP_NOZORDER));

	// Yanghui:
	dragWindow = gGetDragWindow(self);
	if(dragWindow) {
		gGetUpdatedGroupMoveDragRect(dragWindow, NULL);  //  update the GroupMoveDragRect
		gGetUpdatedDragRect(dragWindow, NULL);           // update the DragRect
		gUpdateDragRectWindow(dragWindow);
	}
	// Yanghui

	return self;
}

imeth	gSetVirtualPosition(int y, int x)
{
	int	y2, x2;
	
	y2 = y;
	x2 = x;
	gScaleToPixels(Application, &y2, &x2, iFont);
	iVVPos = y2;
	iVHPos = x2;

	if (gIsKindOf(self, Control)) {
		object parent = gGetParent(self);          // Yanghui
		if(parent)                                 // Yanghui
			gGetScrollPosition(parent, &y2, &x2);  // Yanghui
		else                                       // Yanghui
			y2 = x2 = 0;                           // Yanghui
	}
	else
		y2 = x2 = 0;
	gSetPosition(self, y-y2, x-x2); 
	return self;
}


// Yanghui:
//////////////////////////////////////////////////////////////////////////
// gSetVirtualPosition2
// 
// This is the clone of subprogram "gSetVirtualPosition", what it does is
// to increase the position of the new control by "hOffset x vOffset"
// so that after "gOutputVector" in "process_wm_paint" shifts the position
// "hOffset x vOffset", the control is still visible in the client area of 
// the main window. 
// 
//////////////////////////////////////////////////////////////////////////
imeth	gSetVirtualPosition2(int y, int x)
{
	int	x1, y1, x2, y2;
	
	x1 = x;
	y1 = y;

	gScaleToPixels(Application, &y1, &x1, iFont);

	if (gIsKindOf(self, Control)) {
		object parent = gGetParent(self);           // Yanghui
		if(parent)                                  // Yanghui
			gGetScrollPosition(parent, &y2, &x2);   // Yanghui
		else                                        // Yanghui
			y2 = x2 = 0;                            // Yanghui
	}
	else
		y2 = x2 = 0;

	iVPos = y1 + y2;
	iHPos = x1 + x2;

	iVHPos = iHPos + x2;
	iVVPos = iVPos + y2;

	return self;
}
// Yanghui

imeth	gSetPosition(int y, int x)
{
	int	hx = iHPos, hy = iVPos;
	RECT	r;
	int	isControl = gIsKindOf(self, Control);
	
	if (isControl && (hx != x || hy != y)) {
		int	pm = gSetScalingMode(Application, SM_PIXELS);

		gGetRect(self, &r);
	
		r.bottom += 2;
		r.right += 2;

		r.top -= 2;
		r.left -= 2;

		gSetScalingMode(Application, pm);
	}
	gScaleToPixels(Application, &y, &x, iFont);
	iVPos = y;
	iHPos = x;
	if (gIsKindOf(self, Control)) {
		int	y2=0, x2=0;                            // yanghui
		object parent = gGetParent(self);          // yanghui
		if(parent)                                 // yanghui
			gGetScrollPosition(parent, &y2, &x2);  // yanghui
		iVVPos = iVPos + y2;
		iVHPos = iHPos + x2;
	} else {
		iVVPos = iVPos;
		iVHPos = iHPos;
	}
	if (isControl) {
		object	dlg = gDialog(self);
		// pUpdateControlVectors(dlg, self);
		if(dlg) {  // Yanghui:
			gUpdateControlVectors(dlg, self);
			if (hx != x || hy != y) {
				HANDLE	hdlg = gHandle(dlg);

				if (hdlg) {
					InvalidateRect(gHandle(dlg), &r, TRUE);
					gInvalidate3dCtl(self);
				}
			}
		}  // Yanghui
	}
	if (iHwnd)
		SetWindowPos(iHwnd, HWND_TOP, iHPos, iVPos, 0, 0, (SWP_NOSIZE | SWP_NOZORDER));

	updateDockedWindows(iv, hx, hy);
	
	return self;
}

imeth	gSetSize(int height, int width)
{
	object  fontObj;          // Yanghui	
	int     cxChar, cyChar;   // Yanghui

	int	hh = iHSize, hw = iVSize;
	RECT	r;
	int	isControl = gIsKindOf(self, Control);


	if (isControl && (hh != height || hw != width)) {
		int	pm = gSetScalingMode(Application, SM_PIXELS);

		gGetRect(self, &r);
	
		r.bottom += 2;
		r.right += 2;

		r.top -= 2;
		r.left -= 2;

		gSetScalingMode(Application, pm);
	}
	gScaleToPixels(Application, &height, &width, iFont);

	// Yanghui:
	if (ClassOf(self)==ListBox) {
		fontObj = gGetFont(self);
		cxChar = gAveCharWidth(fontObj);
		cyChar = gTextHeight(fontObj);
		if ( height<cyChar )
			height = cyChar;
	}
	// Yanghui

	iVSize = height;
	iHSize = width;

	if (isControl) { 
		object	dlg = gDialog(self);
		// pUpdateControlVectors(dlg, self);
		if(dlg) {  // Yanghui:
			gUpdateControlVectors(dlg, self);
			if (hh != height || hw != width) {
				HANDLE	hdlg = gHandle(dlg);
				if (hdlg)  {
					InvalidateRect(hdlg, &r, TRUE);
					gInvalidate3dCtl(self);
				}
			}
		}  // Yanghui
	}
	if (iHwnd) 
		SetWindowPos(iHwnd, HWND_TOP, iHPos, iVPos, iHSize, iVSize, SWP_NOZORDER);
	return self;
}

imeth	gGetScreenPosition(int *y, int *x)
{
	if (iHwnd) {
		RECT	r;

		GetWindowRect(iHwnd, &r);
		if (y)
			*y = r.top;
		if (x)
			*x = r.left;
	} else {
		if (y)
			*y = 0;
		if (x)
			*x = 0;
	}
	return self;
}

imeth	gGetPosition(int *y, int *x)
{
	*y = iVPos;
	*x = iHPos;
	gScaleToCurrentMode(Application, y, x, iFont);
	return self;
}

imeth	gGetScrollPosition(int *y, int *x)
{
	*y = iVOffset;
	*x = iHOffset;
	gScaleToCurrentMode(Application, y, x, iFont);
	return self;
}

imeth	gGetVirtualPosition(int *y, int *x)
{
#if 0
	int	y2, x2;
	
	if (gIsKindOf(self, Control))
		gGetScrollPosition(gGetParent(self), &y2, &x2);
	else
		y2 = x2 = 0;
	gGetPosition(self, y, x);
	*y += y2;
	*x += x2;
#else
	*y = iVVPos;
	*x = iVHPos;
	gScaleToCurrentMode(Application, y, x, iFont);
#endif
	return self;
}

imeth	gGetSize(int *height, int *width)
{
	*height = iVSize;
	*width  = iHSize;
	gScaleToCurrentMode(Application, height, width, iFont);
	return self;
}

imeth	gGetClientSize(int *height, int *width)
{
	*height = iVCSize;
	*width  = iHCSize;
	gScaleToCurrentMode(Application, height, width, iFont);
	return self;
}

private	imeth	pUndockAll()
{
	object	seq, obj;
	if (iDockedChildren)
		for (seq=gSequence(iDockedChildren) ; obj = gNext(seq) ; )
			gUndock(self, obj);
	if (iDockedParents)
		for (seq=gSequence(iDockedParents) ; obj = gNext(seq) ; )
			gUndock(obj, self);
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iFunctions) {
		object	fun;
		int	r;
		fun = iFunctions;
		iFunctions = NULL;
		r = gExecuteFunctionsObj(fun, self);
		if (!IsObj(self)) {
			gDeepDispose(fun);
			return NULL;
		}
		iFunctions = fun;
		if (!r)
			return NULL;
	}
	if (iControlDict)
		iControlDict = gDeepDispose(iControlDict);
	if (iControls)
		iControls = gDispose(iControls);
	dispose_children(iv);
	if (iParent  &&  gIsKindOf(iParent, Window))
		removeChild(iParent, self);
	if (iHwnd)  {
		SetMenu(iHwnd, (HMENU) 0);
		gFreeHelp(HelpSystem, self);
		DestroyWindow(iHwnd);
		while (HC_DELETE(WINDOW_HANDLE_CACHE, iHwnd));
	}
	pUndockAll(self);
	if (iActivateFunctions)
		gDeepDispose(iActivateFunctions);
	if (iWndTitle)
		gDispose(iWndTitle);
	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);
	if (iFonts)
		gDeepDispose(iFonts);
	if (iIcon)
		gDispose(iIcon);
	if (iCursor)
		gDispose(iCursor);
	if (iMenu)
		gDeepDispose(iMenu);
	if (iShortcutMenuStr)
		gDispose(iShortcutMenuStr);
	if (iShortcutFunctions)
		gDeepDispose(iShortcutFunctions);
	if (iWndCursor)
		gDispose(iWndCursor);
	if (iBackBrush)
		gDispose(iBackBrush);
	if (iTextBrush)
		gDispose(iTextBrush);
	if (iBuf)
		free(iBuf);
	if (iClientArea)
		gDeepDispose(iClientArea);
	if (iPauseArea)
		gDeepDispose(iPauseArea);
	if (iMessageHandlers)
		gDeepDispose(iMessageHandlers);
	if (!iType)
		UnregisterClass(gStringValue(iClassName), gInstance(Application));
	if (iClassName)
		gDispose(iClassName);
	if (iDockedChildren)
		gDispose(iDockedChildren);
	if (iDockedParents)
		gDispose(iDockedParents);
	if (iBmp)
		gDispose(iBmp);
	if (iInitFunctions)
		gDeepDispose(iInitFunctions);
		
#ifndef unix
#ifdef	WIN32
	if (iHmf)
		if (iEnhancedMetaFile == 1)
			DeleteEnhMetaFile(iHmf);
		else
			DeleteMetaFile(iHmf);
#else
	if (iHmf)
		DeleteMetaFile(iHmf);
#endif
#endif
	if (iFunctions)
		gDeepDispose(iFunctions);
	if (iTopic)
		gDispose(iTopic);
//	if (IsObj(iParent))
//		gUndock(iParent, self);

	if (iCtlSection) {
		gDisposeAllNodes(iCtlSection);
		gDispose(iCtlSection);
	}
	
	// Yanghui:
	if (iMFName)    
		gDispose(iMFName);

	if(iTrackRectObj) 
		iTrackRectObj=gCancelTracking(iTrackRectObj);

	if(iDeletedCtls)
		iDeletedCtls=gDispose(iDeletedCtls);
	// Yanghui

	gDisposePropertyList(self);
	
	if (iTimerFunctions)
		iTimerFunctions = gDeepDispose(iTimerFunctions);
	
	return gDispose(super);
}

imeth	gAddHandlerAfter(unsigned msg, LRESULT (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 1);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerAfter(h, fun);
	return self;
}

imeth	gAddHandlerBefore(unsigned msg, LRESULT (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 1);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerBefore(h, fun);
	return self;
}

imeth	gDefaultProcessingMode(unsigned msg, int mode)
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 1);
		gAddInt(iMessageHandlers, msg, h);
	}
	gSetDefaultProcessingMode(h, mode);
	return self;
}

imeth	gOverrideDefaultProcessing(unsigned msg, int mode)
{
	object	h = gFindValueInt(iMessageHandlers, msg);
	if (h)
		gOverrideDefaultProcessingMode(h, mode);
	return self;
}

imeth	int	gShow()
{
	WNDCLASS	wndclass;
	HINSTANCE	inst;
	static int	once = 1;



	if (gUsingAlternateColor(Application) && (gIsKindOf(self, ClientArea) || gIsKindOf(self, ListBox)))
		gBackBrush(self, gNewSolidBrush(SolidBrush, 255, 255, 255));

// I need to change the color here
	if (gUsingAlternateColor(Application) && !iType && !gIsKindOf(self, ClientArea))
		iBackBrush = gGetBackBrush(Application);

	if (gUsingAlternateColor(Application) && (ClassOf(self) == Window  ||  ClassOf(self) == MainWindow)) {
		int	r, g, b;
		gGetColorTheme(Application, &r, &g, &b);
		gSetColorTheme(self, r, g, b);
	}
	
	inst = gInstance(Application);
	if (!iType  &&  !iHwnd  /*  &&  !gPrevInstance(Application) */)  {
		wndclass.lpszClassName = (LPCSTR) gStringValue(iClassName);
		wndclass.hInstance     = inst;
		wndclass.lpfnWndProc   = iWndProc;
		wndclass.hCursor       = gHandle(iCursor);
		wndclass.hIcon         = gHandle(iIcon);
		wndclass.lpszMenuName  = iMenuName;
		wndclass.hbrBackground = gHandle(iBackBrush);
		wndclass.style         = iStyle;
		wndclass.cbClsExtra    = iClsExtra;
		wndclass.cbWndExtra    = iWndExtra;

		RegisterClass(&wndclass);
	}
	
	if (!iHwnd) {
		iHwnd = CreateWindowEx(iExStyle, 
					  (LPCSTR) gStringValue(iClassName), 
					  (LPCSTR) (iWndTitle ? gStringValue(iWndTitle) : NULL), 
					  iWndStyle, 
					  iHPos, 
					  iVPos, 
					  iHSize, 
					  iVSize, 
					  iParent ? gHandle(iParent) : (HWND) 0, 
					  iMenu ? (HMENU) gHandle(iMenu) : (HMENU) 0, 
					  inst, 
					  self);
		if (iType)
			HC_NEW(WINDOW_HANDLE_CACHE, iHwnd, self);
	}
	
//	ShowScrollBar(iHwnd, SB_BOTH, FALSE);
	if (iWndStyle & WS_MAXIMIZE)
		ShowWindow(iHwnd, SW_SHOWMAXIMIZED);
	else if (iWndStyle & WS_MINIMIZE)
		ShowWindow(iHwnd, SW_SHOWMINIMIZED);
	else if (!(iWndStyle & WS_VISIBLE))
		ShowWindow(iHwnd, SW_HIDE);
	else if (once) {
		once = 0;
		ShowWindow(iHwnd, gShow(Application));
	} else
		ShowWindow(iHwnd, SW_SHOW);
	UpdateWindow(iHwnd);
	if (iStatusBar) {
		gShow(iStatusBar);
		gUpdateState(iStatusBar);
	}
	if (iToolBar) {
		gShow(iToolBar);
		gPerform(iToolBar);
		gUpdateState(iToolBar);
	}
	if (iStatusBar  ||  iToolBar) {
		if (!iClientWind)
			create_clientWind(self, iv);
		gShow(iClientWind);
		gUpdateState(iClientWind);
	}

	if (iBmp)
		InvalidateRect(iHwnd, NULL, TRUE);
		
#ifdef	DEMO
	if (MainWindow == ClassOf(self)  ||  0 == demo_number_of_windows%DEMO_MSG_FREQ)  {
		demo_message(iHwnd);
		demo_number_of_windows = 0;
	}
#endif

	if (ClassOf(self) == ToolBar)
		iIsToolBar = 1;

	return 0;
}

imeth	gUpdate()
{
	if (iHwnd)
		UpdateWindow(iHwnd);
	return self;
}

imeth	gTextOut(int row, int col, char *txt)
{
	object	tv;

	if (iClientWind)
		return gTextOut(iClientWind, row, col, txt);
	gScaleToPixels(Application, &row, &col, iFont);
	if (iAutoShow)  {
		gShow(self);
		iAutoShow = 0;
	}
	tv = vNew(TextVector, row, col, iFont, iHwnd, txt,
		  &iVMax, &iHMax, gColor(iTextBrush));
	gAddLast(ActiveArea, gNewWithObj(LinkValue, tv));
	if (!iIsToolBar  &&  iHwnd  &&  !Paused)  {
		if (iVCSize < iVMax)  {
			SetScrollRange(iHwnd, SB_VERT, 0, MAX_VRANGE, FALSE);
			SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
		}
		if (iHCSize < iHMax)  {
			SetScrollRange(iHwnd, SB_HORZ, 0, MAX_HRANGE, FALSE);
			SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
		}
	}
	return self;
}


// Yanghui:
imeth int gSetBkMode(int iBkMode)
{
	HDC  hDC;
	int  nBkMode;

	if (iClientWind)
		return gSetBkMode(iClientWind, iBkMode);

	hDC = GetDC(iHwnd);
	nBkMode = SetBkMode(hDC, iBkMode);
	ReleaseDC(iHwnd, hDC);

	return nBkMode;  
}

imeth	gDrawTextWithScale(char *lpString, RECT *lpRect, int nFormat, double scale)
{
	RECT rect;
	if(!lpString)
		return self;

	if(scale <= 0) 
		scale = 1.0;

	CopyRect(&rect, lpRect);

	rect.left *= scale;
	rect.top *= scale;
	rect.right *= scale;
	rect.bottom *= scale;

	gDrawText(self, lpString, &rect, nFormat);
	return self;
}

imeth	gDrawText(char *lpString, RECT *lpRect, int nFormat)
{
	object	tv;

	if (iClientWind)
		return gDrawText(iClientWind, lpString, lpRect, nFormat);

	if (iAutoShow)  {
		gShow(self);
		iAutoShow = 0;
	}

	if(nFormat < 0) 
		nFormat = 0x00000000;

	tv = gNewWithRect(TextVector, lpRect, nFormat, iFont, iHwnd, lpString,
		  &iVMax, &iHMax, gColor(iTextBrush));

	gAddLast(ActiveArea, gNewWithObj(LinkValue, tv));

	if (!iIsToolBar  &&  iHwnd  &&  !Paused)  {
		if (iVCSize < iVMax)  {
			SetScrollRange(iHwnd, SB_VERT, 0, MAX_VRANGE, FALSE);
			SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
		}
		if (iHCSize < iHMax)  {
			SetScrollRange(iHwnd, SB_HORZ, 0, MAX_HRANGE, FALSE);
			SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
		}
	}
	return self;
}
// Yanghui


imeth	gLoadIcon(unsigned icon)
{
	object	obj;

	if (!(obj = gLoad(ExternalIcon, icon)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadSystemIcon(LPCSTR icon)
{
	object	obj;

	if (!(obj=gLoadSys(SystemIcon, icon)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadFont(char *name,	/*  may be object too  */
		  int size)
{
	object	obj;

	if (iClientWind)
		return gLoadFont(iClientWind, name, size);
	
	if (!(obj = vNew(ExternalFont, name, size)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadSystemFont(unsigned font)
{
	object	obj;

	if (iClientWind)
		return gLoadSystemFont(iClientWind, font);
	
	if (!(obj=gLoad(SystemFont, font)))
		return NULL;
	return gUse(self, obj);
}

static	void	appendFont(object list, object font)
{
	object	seq, next;
	int	add = 1;

	for (seq=gSequence(list) ; next = gNext(seq) ; )
		if (next == font)
			add = 0;
	if (add)
		gAddFirst(list, font);
}

imeth	gUse(obj)
{
	ChkArg(obj, 2);
	if (gIsKindOf(obj, Font))  {
		appendFont(iFonts, obj);
		iFont = obj;
	} else if (gIsKindOf(obj, Icon))  {
		if (iIcon)
			gDispose(iIcon);
		iIcon = obj;
	} else if (gIsKindOf(obj, Cursor))  {
		if (iCursor)
			gDispose(iCursor);
		iCursor = obj;
	} else if (gIsKindOf(obj, Menu))  {
		iMenu = gPush(obj, iMenu);
		if (iHwnd)
			SetMenu(iHwnd, obj ? gHandle(obj) : 0);
	} else if (gIsKindOf(obj, Brush))  {
		if (iBackBrush)
			gDispose(iBackBrush);
		iBackBrush = obj;
	} else
		gError(self, "Incorrect 2nd argument to Use::Window");
	DEMO_CHK_MAX;
	return obj;
}

imeth	gLoadCursor(unsigned cursor)
{
	object	obj;

	if (!(obj=gLoad(ExternalCursor, cursor)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadSystemCursor(LPCSTR cursor)
{
	object	obj;

	if (!(obj=gLoadSys(SystemCursor, cursor)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadMenu(unsigned menu)
{
	object	obj;

	if (!(obj=gLoad(ExternalMenu, menu)))
		return NULL;
	gSetParent(obj, self);
	return gUse(self, obj);
}

imeth	gLoadMenuStr(char *menu)
{
	object	obj;

	if (!(obj=gLoadStr(ExternalMenu, menu)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadShortcutMenu(unsigned menu, int submenu)
{
	iShortcutMenuID = menu;
	iShortcutSubmenu = submenu;
	
	return self;
}	

imeth	gLoadShortcutMenuStr(char *menu, int submenu)
{
	iShortcutMenuStr = gNewWithStr(String, menu);
	iShortcutSubmenu = submenu;
	
	return self;
}

imeth	gPopMenu()
{
	if (ClassOf(self) == ClientArea)
		return gPopMenu(gGetParent(self));
	iMenu = gPop(iMenu);
	if (iHwnd)
		SetMenu(iHwnd, iMenu ? gHandle(iMenu) : 0);
	return iMenu;
}

imeth	gMenu()
{
	return iMenu;
}

imeth	gChangeMenuText(unsigned id, char *str)
{
	HANDLE	mh;
	if (iMenu) {
		mh = gHandle(iMenu);
		ModifyMenu(mh, id, MF_BYCOMMAND|MF_STRING, id, str);
		if (iHwnd)
			DrawMenuBar(iHwnd);
		return self;
	} else
		return NULL;
}	

imeth	gAssociate(int id, void (*fun)())
{
	if (iMenu)
		gAssociate(iMenu, id, fun);
	return iMenu;
}

imeth	gShortcutFunction(int id, void (*fun)())
{
	if (!iShortcutFunctions)
		iShortcutFunctions = gNew(IntegerDictionary);
	
	gAddInt(iShortcutFunctions, id, gNewWithPtr(Pointer, (void *) fun));

	return self;
}

imeth	gMenuItemMode(unsigned id, unsigned mode)
{
	if (!iMenu)
		return NULL;
	return gSetMode(iMenu, id, mode);
}

imeth	int	gSetCheckMark(unsigned id, int mode)
{
	if (!iMenu)
		return -1;
	
	return gSetCheckMark(iMenu, id, mode);
}

imeth	gErase : Erase (int bRow, int eRow, int bCol, int eCol)
{
	RECT	r;
	object	seq, obj;
	int	h, v, hMax=0, vMax=0;

	if (iClientWind)
		return gErase(iClientWind, bRow, eRow, bCol, eCol);
	gScaleToPixels(Application, &bRow, &bCol, iFont);
	gScaleToPixels(Application, &eRow, &eCol, iFont);
	r.top    = bRow;
	r.bottom = eRow;
	r.left   = bCol;
	r.right  = eCol;
	for (seq=gSequence(ActiveArea) ; obj = gNext(seq) ; )
		if (gWithinRange(gValue(obj), &r, &h, &v))
			gDeepDispose(obj);
		else  {
			hMax = MAX(hMax, h);
			vMax = MAX(vMax, v);
		}
	if (!Paused) {
		iHMax = hMax;
		iVMax = vMax;
		if (iVOffset > MAX_VRANGE)
			iVOffset = MAX_VRANGE;
		if (iHwnd)  {
			update_vert(iv);
			update_horz(iv);
			InvalidateRect(iHwnd, &r, TRUE);
		}
	}
	return self;
}

imeth	gEraseLines(int beg, int end)
{	
	int	h = gLineHeight(iFont);
	int	sm = gSetScalingMode(Application, SM_PIXELS);

	beg = h * beg + 1;   /*  I don't know why the +1 is needed, but it is...*/
	end =  (end+1) * h - 1;
	Erase(self, beg, end, 0, 2500);
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gEraseAll()
{
	// if the dragging window exists, cancel the dragging window
	gRmAllOfDWs(DragWindow);  // Yanghui

	if (iClientWind)
		return gEraseAll(iClientWind);
	gPlayMetaFile(self, NULL);
	gDisposeAllControls(self);

	if(iBmp) {  // Yanghui:
		gDispose(iBmp);
		iBmp = NULL;
	}           // Yanghui

	if (!Paused) {
		iHOffset = iVOffset = iVMax = iHMax = 0;
		iCurLine = iCurCol = 0;
		if (iHwnd)  {
			InvalidateRect(iHwnd, NULL, TRUE);
			ShowScrollBar(iHwnd, SB_BOTH, FALSE);
		}
	}
	return self;
}

imeth	gPause()
{
	if (iClientWind)
		return gPause(iClientWind);
	if (Paused)
		return self;
	iPauseArea = gDeepCopy(iClientArea);
	return self;
}

imeth	gResume()
{
	int	sm;

	if (iClientWind)
		return gResume(iClientWind);
	if (!Paused)
		return self;
	gDeepDispose(iClientArea);
	iClientArea = iPauseArea;
	iPauseArea = NULL;

	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);
	gUpdate(self);
	sm = gSetScalingMode(Application, SM_PIXELS);
	gScrollVert(self, MAX_VRANGE);  /* scroll to end of list  */
	gSetScalingMode(Application, sm);

	return self;
}

imeth	gScrollVert(int inc)
{
	if (iIsToolBar)
		return NULL;
	if (iClientWind)
		return gScrollVert(iClientWind, inc);
	
	gScaleToPixels(Application, &inc, NULL, iFont);
	if (iVOffset + inc < 0)
		inc = -iVOffset;
	if (iVOffset + inc > MAX_VRANGE)
		inc = MAX_VRANGE - iVOffset;
	if (inc)  {
		iVOffset += inc;
		ScrollWindow(iHwnd, 0, -inc, NULL, NULL);
		SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
//		UpdateWindow(iHwnd);
	}
	return self;
}

imeth	gScrollHorz(int inc)
{
	if (iIsToolBar)
		return NULL;
	if (iClientWind)
		return gScrollHorz(iClientWind, inc);

	gScaleToPixels(Application, NULL, &inc, iFont);
	if (iHOffset + inc < 0)
		inc = -iHOffset;
	if (iHCSize  &&  iHCSize != CW_USEDEFAULT  &&  iHOffset + inc > MAX_HRANGE)
		inc = MAX_HRANGE - iHOffset;
	if (inc)  {
		iHOffset += inc;
		ScrollWindow(iHwnd, -inc, 0, NULL, NULL);
		SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
//		UpdateWindow(iHwnd);
	}
	return self;
}

imeth	int	gQuery : query(char *title, char *msg, UINT options)
{
	return MessageBox(iHwnd, msg, title, options);  
}	

imeth	gMessage(char *msg)
{
	MessageBox(iHwnd, msg, "Message Window", MB_OK);  
	return self;
}	

#ifdef	DEMO

void	demo_message(HWND hwnd)
{
#ifndef	LASTDATE
	MessageBox(hwnd, 

"This is a demo version of the Dynace Windows Development System (WDS) "
"by Blake McBride (blake@mcbride.name)."
		   , "Demo Information Window", MB_OK);
#endif
}

#endif

imeth	gMessageWithTopic(char *msg, char *topic)
{
	char	*p = gSetTopic(HelpSystem, topic);
	MessageBox(iHwnd, msg, "Message Window", MB_OK);  
	gSetTopic(HelpSystem, p);
	return self;
}	

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHwnd;
}

imeth	int	gAutoDispose(int ad)
{
	int	pad = iAutoDispose;
	iAutoDispose = ad;
	return pad;
}

imeth	int	gAutoShow(int a)
{
	int	pa = iAutoShow;
	iAutoShow = a;
	return pa;
}

imeth	int	gWrite(char *str, unsigned len)
{
	char	*t, c;
	int	sm, n=len, lh;
	object	so;

	if (!str  ||  !len)
		return 0;
	if (iClientWind)
		return gWrite(iClientWind, str, len);

	t = gStringValue(so = gNewWithInt(String, len+1));
	if (IsObj((object) str))
		str = gStringValue((object) str);
	memcpy(t, str, len);
	t[len] = '\0';
	str = t;
	sm = gSetScalingMode(Application, SM_PIXELS);
	lh = gLineHeight(iFont);
	for (t=str ; str  &&  n  &&  *str ; str=t)  {
		if (iCurLine == iMaxLines)  {
			gVertShift(self, lh);
			iCurLine--;
		}
		for (t=str ; *t  &&  *t != '\n' &&  n-- ; ++t);
		if (c=*t)
			*t = '\0';
		gTextOut(self, iCurLine*lh, iCurCol, str);
		if (c == '\n')  {
			iCurLine++;
			iCurCol = iBegCol;
			*t++ = c;
			if (n)
				n--;
		} else
			iCurCol += gStrPixelWidth(iFont, str);
	}
	gDispose(so);
	if (!iIsToolBar  &&  !Paused) {
		gUpdate(self);
		gSetScalingMode(Application, SM_PIXELS);
		gScrollVert(self, MAX_VRANGE);  /* scroll to end of list  */
	}
	gSetScalingMode(Application, sm);
	return len;
}

imeth	gSetTag(tag)
{
	object	ptag = iTag;
	iTag = tag;
	if (ptag  &&  iAutoDisposeTag)
		return gDeepDispose(ptag);
	else
		return ptag;
}

imeth	gGetTag()
{
	return iTag;
}


imeth	gAutoDisposeTag()
{
	iAutoDisposeTag = 1;
	return self;
}

imeth	gTextBrush(brush)
{
	if (iClientWind)
		return gTextBrush(iClientWind, brush);

	ChkArgTyp(brush, 2, Brush);
	if (iTextBrush)
		gDispose(iTextBrush);
	iTextBrush = brush;
	return self;
}

imeth	gGetTextBrush()
{
	if (iClientWind)
		return gGetTextBrush(iClientWind);
	return iTextBrush;
}

imeth	gGetBackBrush()
{
	if (iClientWind)
		return gGetBackBrush(iClientWind);
	return iBackBrush;
}

imeth void gSetColorTheme(int r, int g, int b)
{
	if (iMenu)
		gSetColorTheme(iMenu, r, g, b);
	if (iStatusBar)
		gBackBrush(iStatusBar, gGetBackBrush(Application));
	if (iToolBar) {
		gUseAlternateColor(iToolBar);
		gBackBrush(iToolBar, gGetBackBrush(Application));
	}
}

imeth	gBackBrush(brush)
{
	if (iClientWind)
		return gBackBrush(iClientWind, brush);

	ChkArgTyp(brush, 2, Brush);
	if (iBackBrush)
		gDispose(iBackBrush);
	iBackBrush = brush;
	return self;
}

imeth	gGetFont()
{
	if (iClientWind)
		return gGetFont(iClientWind);
	return iFont;
}

imeth	int	gKbhit()
{
	MSG	msg;
		
	while (!iNchars  &&  PeekMessage(&msg, (HWND) 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return iNchars;
}

imeth	int	gGetch()
{
	int	ch;
	
	while (!iNchars  &&  iBlock  &&  gProcessOneMessage(MessageDispatcher));
	if (iNchars)  {
		ch = iBuf[0];
		if (--iNchars)
			memmove(iBuf, iBuf+1, iNchars);
		return ch;
	}
	return 0;
}

imeth	int	gRead(char *buf, unsigned n)
{
	while (iNchars < n  &&  iBlock  &&  gProcessOneMessage(MessageDispatcher));
	n = MIN(n, iNchars);
	if (n)  {
		memcpy(buf, iBuf, n);
		if (iNchars -= n)
			memmove(iBuf, iBuf+n, iNchars);
	}
	return n;
}

imeth	char	*gGets(char *buf, int n)
{
	int	i, nl;

	n--;
	if (iBlock)
		while (1)  {
			for (i=nl=0 ; i < iNchars ; )
				if (iBuf[i++] == '\n')  {
					nl = 1;
					break;
				}
			if (nl  ||  i == n  ||  !gProcessOneMessage(MessageDispatcher))
				break;
		}


	n = MIN(n, iNchars);
	for (i=0 ; i != n ; ++i)
		if ((buf[i] = iBuf[i]) == '\n')  {
			i++;
			break;
		}
	buf[i] = '\0';
	if (i)  {
		if (iNchars -= i)
			memmove(iBuf, iBuf+i, iNchars);
	}
	return i ? buf : NULL;
}

imeth	long	gAdvance(long n)
{
	while (iNchars < n  &&  iBlock  &&  gProcessOneMessage(MessageDispatcher));
	n = MIN(n, iNchars);
	if (n)  {
		if (iNchars -= n)
			memmove(iBuf, iBuf+n, iNchars);
	}
	return n;
}

imeth	long	gLength()
{
	return iNchars;
}

imeth	long	gPosition()
{
	return 0L;
}

imeth	int	gEndOfStream()
{
	return !iNchars;
}

static	void	class_init()
{
	gDontCollect(CLASS);
}

imeth	int	gSetBlock(int blk)
{
	int	p = iBlock;
	iBlock = blk;
	return p;
}

imeth	int	gSetRaw(int raw)
{
	int	p = iRaw;
	iRaw = raw;
	return p;
}

imeth	int	gSetEcho(int echo)
{
	int	p = iEcho;
	iEcho = echo;
	return p;
}

private	imeth	void	addChild(object self, object child)
{
	if (child)  {
		if (!iChildren)
			iChildren = gNew(LinkList);
		gAddLast(iChildren, gNewWithObj(LinkValue, child));
	}
}

static	void	dispose_children(ivType *iv)
{
	object	obj;

	if (iChildren)  {
		//  since iStatusBar was a child window, and therfore disposed
		//  of by this routine, make sure there is
		//  no future attempt to use the object
		iClientWind = iToolBar = iStatusBar = NULL;  
		
		// Yanghui: the above statements must be in front of the gDispose statements
		//          in the following. 

		while (obj = gFirst(iChildren))
			gDispose(gValue(obj));  /*  just child not link  */
		iChildren = gDeepDispose(iChildren);  
	}
}

private	imeth	void	removeChild(object self, object child)
{
	object	seq, obj;

	if (iChildren  &&  child)
		for (seq=gSequence(iChildren) ; obj = gNext(seq) ; )
			if (gValue(obj) == child)
				gDispose(obj);  /*  just link not child */
}

imeth	gSetParent(pwnd)
{
	ChkArgNul(pwnd, 2);
	if (iParent  &&  gIsKindOf(iParent, Window))
		removeChild(iParent, self);
	iParent = pwnd;
	DEMO_CHK_MAX;
	if (pwnd  &&  gIsKindOf(pwnd, Window))
		addChild(pwnd, self);
	return self;
}

imeth	gGetParent()
{
	return iParent;
}

imeth	char	*gSetTopic(char *topic)
{
	if (iTopic)
		if (topic  &&  *topic)
			gChangeStrValue(iTopic, topic);
		else
			iTopic = gDispose(iTopic);
	else if (topic  &&  *topic)
		iTopic = gNewWithStr(String, topic);
	return topic;
}

imeth	char	*gGetTopic()
{
	return iTopic ? gStringValue(iTopic) : NULL;
}

imeth	int	gSetMaxLines(int ml)
{
	int	r;

	if (iClientWind)
		return gSetMaxLines(iClientWind, ml);
	r = iMaxLines;
	if (ml > 0)
		iMaxLines = ml;
	return r;
}

cmeth	gInitFunction(int (*fun)())
{
	if (cInitFunctions)
		gDeepDispose(cInitFunctions);
	
	cInitFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(cInitFunctions, fun);
	return self;
}

cmeth	gAddInitFunctionBefore(int (*fun)())
{
	if (!cInitFunctions)
		cInitFunctions = gNew(FunctionList);

	gAddFunctionBefore(cInitFunctions, fun);
	
	return self;
}

cmeth	gAddInitFunctionAfter(int (*fun)())
{
	if (!cInitFunctions)
		cInitFunctions = gNew(FunctionList);

	gAddFunctionAfter(cInitFunctions, fun);
	
	return self;
}



imeth	gInitFunction(int (*fun)())
{
	if (iInitFunctions)
		gDeepDispose(iInitFunctions);
	
	if (cInitFunctions  &&  !gIsKindOf(self, Control))
		iInitFunctions = gDeepCopy(cInitFunctions);
	else
		iInitFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(iInitFunctions, fun);
	return self;
}

imeth	gAddInitFunctionBefore(int (*fun)())
{
	if (!iInitFunctions)
		iInitFunctions = gNew(FunctionList);

	gAddFunctionBefore(iInitFunctions, fun);
	
	return self;
}

imeth	gAddInitFunctionAfter(int (*fun)())
{
	if (!iInitFunctions)
		iInitFunctions = gNew(FunctionList);

	gAddFunctionAfter(iInitFunctions, fun);
	
	return self;
}

imeth	gCompletionFunction(int (*fun)())
{
	if (iFunctions)
		gDeepDispose(iFunctions);
	
	iFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(iFunctions, fun);
	return self;
}

imeth	gAddCompletionFunctionBefore(int (*fun)())
{
	if (!iFunctions)
		iFunctions = gNew(FunctionList);

	gAddFunctionBefore(iFunctions, fun);
	
	return self;
}

imeth	gAddCompletionFunctionAfter(int (*fun)())
{
	if (!iFunctions)
		iFunctions = gNew(FunctionList);

	gAddFunctionAfter(iFunctions, fun);
	
	return self;
}

imeth	gMoveToTop()
{
	if (iHwnd)
		BringWindowToTop(iHwnd);
	return self;
}

imeth	gSetState(int state)
{
	if (iHwnd)
		ShowWindow(iHwnd, state);
	return self;
}


imeth	gSetFocus()
{
	if (iHwnd)
		SetFocus(iHwnd);
	return self;
}

imeth	int	gInDialog()
{
	return !!iHwnd;
}

imeth	int	gIsIconic()
{
	return iHwnd && IsIconic(iHwnd) == TRUE ? 1 : 0;
}

imeth	int	gIsMaximized()
{
	return iHwnd && IsZoomed(iHwnd) == TRUE ? 1 : 0;
}

imeth	gMaximize()
{
	if (iHwnd)
		ShowWindow(iHwnd, SW_MAXIMIZE);
	return self;
}

imeth	int	gIsNormal()
{
	return iHwnd && IsZoomed(iHwnd) == FALSE && IsIconic(iHwnd) == FALSE ? 1 : 0;
}

imeth	gSetStatusBar(sb)
{
	iStatusBar = sb;
	return self;
}

imeth	gGetStatusBar()
{
	return iStatusBar;
}

imeth	gSetToolBar(sb)
{
	iToolBar = sb;
	return self;
}

imeth	gGetToolBar()
{
	return iToolBar;
}

imeth	gGetClientWind()
{
	if (!iClientWind  &&  (iToolBar  ||  iStatusBar))
		create_clientWind(self, iv);
	return iClientWind ? iClientWind : self;
}
 
static	void	create_clientWind(object self, ivType *iv)
{
	iClientWind = vNew(ClientArea, self);
	gSetMaxLines(iClientWind, iMaxLines);
	if (iBackBrush)
		gBackBrush(iClientWind, gCopy(iBackBrush));
	if (iTextBrush)
		gTextBrush(iClientWind, gCopy(iTextBrush));
	if (iFont)
		gUse(self, gCopy(iFont));
}

imeth	gAddSection(int id, int width, int mode)
{
	if (!iStatusBar)
		vNew(StatusBar, self);
	if (!iClientWind)
		create_clientWind(self, iv);
	return iStatusBar ? gAddSection(iStatusBar, id, width, mode) : NULL;
}

imeth	gAddToolBitmap(unsigned id1, unsigned id2, int space, LRESULT (*fun)(), char *tip)
{
	if (!iToolBar)
		vNew(ToolBar, self);
	if (!iClientWind)
		create_clientWind(self, iv);
	return iToolBar ? gAddToolBitmap(iToolBar, id1, id2, space, fun, tip) : NULL;
}

imeth	gAddToolComboBox(int height, int width, int space, int (*fun)(), char *tip, char *name)
{
	if (!iToolBar)
		vNew(ToolBar, self);
	if (!iClientWind)
		create_clientWind(self, iv);
	return iToolBar ? gAddToolComboBox(iToolBar, height, width, space, fun, tip, name) : NULL;
}

imeth	gAddToolBitmapFromFile(char *file, unsigned id1, unsigned id2, int space, LRESULT (*fun)(), char *tip)
{
	if (!iToolBar)
		vNew(ToolBar, self);
	if (!iClientWind)
		create_clientWind(self, iv);
	return iToolBar ? gAddToolBitmapFromFile(iToolBar, file, id1, id2, space, fun, tip) : NULL;
}

imeth	int	gEnableToolBitmap(unsigned id, int val)
{
	return iToolBar ? gEnableToolBitmap(iToolBar, id, val) : 0;
}

imeth	int	gShowToolBitmap(unsigned id, int val)
{
	return iToolBar ? gShowToolBitmap(iToolBar, id, val) : 0;
}

imeth	ifun	gSetToolFunction(unsigned id, LRESULT (*fun)())
{
	return iToolBar ? gSetToolFunction(iToolBar, id, fun) : (ifun) 0;
}

imeth	gChangeTip(unsigned id, char *tip)
{
	if (iToolBar)
		return gChangeTip(iToolBar, id, tip);
	return NULL;
}

imeth	gSetSectionText(int id, char *text)
{
	return iStatusBar ? gSetSectionText(iStatusBar, id, text) : NULL;
}

imeth	gSetZOrder(HWND m)
{
	iZOrder = m;
	if (iHwnd)
		SetWindowPos(iHwnd, iZOrder, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE));
	return self;
}

imeth	gSetCursor(cursor)
{
	HWND	hwnd = gHandle(self);

	if (iWndCursor)
		gDispose(iWndCursor);
	iWndCursor = cursor;
	gDefaultProcessingMode(self, (unsigned) WM_SETCURSOR, !cursor);
	if (hwnd)
		if (iWndCursor)
			SetCursor(gHandle(iWndCursor));
		else 
			SetCursor(gHandle(gGetCursor(Application)));
	return self;
}

imeth	gWaitCursor(int wait)
{
	if (wait) {
		if (!iWait++) {
			gSetCursor(self, gLoadSys(SystemCursor, IDC_WAIT));
			gDisable(self);
		}
	} else
		if (--iWait <= 0) {
			gEnable(self);
			gSetCursor(self, NULL);
			iWait = 0;
		}
	return self;
}

imeth	gEnable()
{
	HWND	wnd = gHandle(self);
	if (wnd)
		EnableWindow(wnd, TRUE);
	return self;
}

imeth	gDisable()
{
	HWND	wnd = gHandle(self);
	if (wnd)
		EnableWindow(wnd, FALSE);
	return self;
}

imeth	gSetRedraw(int flg)
{
	HWND	wnd = gHandle(self);
	if (wnd)
		if (flg) {
			SendMessage(wnd, WM_SETREDRAW, TRUE, 0L);
			InvalidateRect(wnd, NULL, TRUE);
		} else
			SendMessage(wnd, WM_SETREDRAW, FALSE, 0L);
	return self;
}

//#out2

imeth	gAddControl(ctl, UINT ctlID)
{
	char	buf[256];

	if (iClientWind)
		return gAddControl(iClientWind, ctl, ctlID);
	if (!iControlDict)
		iControlDict = gNew(StringDictionary);
	if (!iControls)
		iControls = gNew(LinkObject);
	gAddLast(iControls, ctl);
	if ( (ClassOf(ctl)!=StaticTextControl) && (ClassOf(ctl)!=RectControl) && (ClassOf(ctl)!=LineControl) ) {  // Yanghui
		iTabOrder += 10;
		gSetTabOrder(ctl, iTabOrder);
	}
	strcpy(buf, gName(ctl));
	if (!strlen(buf))
		sprintf(buf, "%d", gSize(iControls));
	gAddStr(iControlDict, buf, ctl);

	gAddLast(ActiveArea, gNewWithObj(LinkValue, vNew(ControlVector, ctl, iHwnd, &iVMax, &iHMax)));
	if (!iIsToolBar  &&  iHwnd  &&  !Paused)  {
		if (iVCSize < iVMax)  {
			SetScrollRange(iHwnd, SB_VERT, 0, MAX_VRANGE, FALSE);
			SetScrollPos(iHwnd, SB_VERT, iVOffset, TRUE);
		}
		if (iHCSize < iHMax)  {
			SetScrollRange(iHwnd, SB_HORZ, 0, MAX_HRANGE, FALSE);
			SetScrollPos(iHwnd, SB_HORZ, iHOffset, TRUE);
		}
	}
//	gSetFont(ctl, gCopy(iFont));
	return ctl;
}
			
imeth	gChangeControlName(object ctl, char *old, char *newName)
{
	if (iClientWind)
		return gChangeControlName(iClientWind, ctl, old, newName);
	gRemoveStr(iControlDict, old);
	gAddStr(iControlDict, newName, ctl);
	return self;
}

imeth	gCheckControlSecurity(int show)
{
	object	seq, ctl;
	
	if (iClientWind)
		return gCheckControlSecurity(iClientWind, show);
	for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
		if (ClassOf(ctl) != StaticControl) {
			if (cAccessMode) {
				int	mode = cAccessMode(ctl);
				if (mode) {
					gDisable(ctl);
					if (mode == 2  &&  gIsKindOf(ctl, SpinControl)) {
						gHideData(ctl, 1);
						gUpdate(ctl);
					}
				}
			}
		}
		if (show)
			gShow(ctl);
	}
	return self;
}
							      
private imeth	pPerformControls()
{
	object	ctl;

	gCheckControlSecurity(self, 1);
	
	if (ctl = gNextControl(self, NULL))
		SetFocus(gHandle(ctl));

	gUpdateScrollBar(self);

	return self;
}	

imeth	int	gPerform()
{
	if (iClientWind)
		return gPerform(iClientWind);

	iBeenPerformed = 1;
	iModal = 0;

	if (!iControls)
		return FALSE;

	gShow(self);
	pPerformControls(self);

	InvalidateRect(gHandle(self), NULL, TRUE);  // Yanghui: a clumsy fixing
	return TRUE;
}

imeth	int	gPerformModal()
{
	if (iClientWind)
		return gPerformModal(iClientWind);

	iBeenPerformed = 1;
	iModal = 1;

	if (!iControls)
		return FALSE;

	gShow(self);

	return modalWindowParam(self);
}

imeth	gGetControlStr(char *name)
{
	if (iClientWind)
		return gGetControlStr(iClientWind, name);
	
	return iControlDict ? gFindValueStr(iControlDict, name) : NULL;
}

imeth	gControls()
{
	if (iClientWind)
		return gControls(iClientWind);
	return iControlDict;
}

imeth	gGetControls()
{
	if (iClientWind)
		return gGetControls(iClientWind);
	return iControls;
}

imeth	gNextControl(object fctl)  //  will return first if fctl is NULL
{
	object	ctl, seq;
	int	found = 0;

	if (iClientWind)
		return gNextControl(iClientWind, fctl);
	if (!iControls)
		return NULL;
	do {
		for (seq=gSequence(iControls) ; ctl = gNext(seq) ; )
			if (!fctl  ||  found == 1) {
				if ((ClassOf(ctl) == TextControl  ||
				     ClassOf(ctl) == NumericControl  ||
				     ClassOf(ctl) == DateControl  ||
				     ClassOf(ctl) == TimeControl  ||
				     ClassOf(ctl) == CheckBox  ||
				     (ClassOf(ctl) == RadioButton  &&  gShortValue(ctl))  ||
				     ClassOf(ctl) == PushButton  ||
				     ClassOf(ctl) == ComboBox  ||
				     ClassOf(ctl) == ListBox)  &&
				    gHiddenStatus(ctl) != 1  &&
				    gDisableStatus(ctl) != 1) {
					gDispose(seq);
					return ctl;
				}
			} else
				if (ctl == fctl)
					found++;
	} while (found == 1);
	return NULL;
}

imeth	gPreviousControl(object fctl)
{
	object	ctl, seq, prev=NULL;

	if (iClientWind)
		return gPreviousControl(iClientWind, fctl);
	if (!iControls)
		return NULL;
	for (seq=gSequence(iControls) ; ctl = gNext(seq) ; )
		if (ctl == fctl) {
			gDispose(seq);
			if (prev)
				return prev;
			break;  // find last
		} else if ((ClassOf(ctl) == TextControl  ||
			    ClassOf(ctl) == NumericControl  ||
			    ClassOf(ctl) == DateControl  ||
			    ClassOf(ctl) == TimeControl  ||
			    ClassOf(ctl) == CheckBox  ||
			    (ClassOf(ctl) == RadioButton  &&  gShortValue(ctl))  ||
			    ClassOf(ctl) == PushButton  ||
			    ClassOf(ctl) == ComboBox  ||
			    ClassOf(ctl) == ListBox)  &&
			   gHiddenStatus(ctl) != 1  &&
			   gDisableStatus(ctl) != 1)
			prev = ctl;
	// find last
	for (seq=gSequence(iControls) ; ctl = gNext(seq) ; )
		if ((ClassOf(ctl) == TextControl  ||
		     ClassOf(ctl) == NumericControl  ||
		     ClassOf(ctl) == DateControl  ||
		     ClassOf(ctl) == TimeControl  ||
		     ClassOf(ctl) == CheckBox  ||
		     ClassOf(ctl) == RadioButton  ||
		     ClassOf(ctl) == PushButton  ||
		     ClassOf(ctl) == ComboBox  ||
		     ClassOf(ctl) == ListBox)  &&
		    gHiddenStatus(ctl) != 1  &&
		    gDisableStatus(ctl) != 1)
			prev = ctl;
	return prev;
}

imeth	gDisposeAllControls()
{
	if (iClientWind)
		return gDisposeAllControls(iClientWind);

	if (iCtlSection) {
		gDisposeAllNodes(iCtlSection);
		iCtlSection = gDispose(iCtlSection);
	}
	
	if (iControls)
		iControls = gDeepDispose(iControls);
	if (iControlDict)
		iControlDict = gDispose(iControlDict);
	gDeepDisposeAllNodes(ActiveArea);
//	gEraseAll(self);
	iTabOrder = 0;
	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);
	return NULL;
}


imeth	int	gCheckValue()
{
	object	ss, ctl;
	int	r = 0;

	if (iClientWind)
		return gCheckValue(iClientWind);
	
	if (iDisableObject)
		gDisable(iDisableObject);
	if (!iControls)
		return 0;
	for (ss = gSequence(iControls) ; ctl = gNext(ss) ; )
		if (gCheckValue(ctl)) {
			r = 1;	/*   error  */
			gDispose(ss);
			break;
		}
	if (iDisableObject)
		gEnable(iDisableObject);
	if (r)
		gSetFocus(ctl);  // needed by tasklist
	return r;
}


imeth	gAddStaticControl(int y, int x, int *end, char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	long	wth;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddStaticControl(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(StaticControl, 2, name, self);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(gGetBackBrush(self)));
	
	gScaleToPixels(Application, &y, &x, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, (int) wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStringValue(ctl, txt);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}

imeth	gAddRectControl(int y, int x, int height, int width, char *name, char *txt)
{
	object  ctl;
	int     sm;
	object  font;

	if (iClientWind)
		return gAddRectControl(iClientWind, y, x, height, width, name, txt);

	ctl = gNewWindowControl(RectControl, 1, name, self);
	// gBackBrush(ctl, gCopy(iBackBrush));

	sm = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, width);
	gSetVirtualPosition(ctl, y, x);

	if(txt)
		gSetStringValue(ctl, txt);
	else if(name)
		gSetStringValue(ctl, name);
	else
		gSetStringValue(ctl, "Rect Control");

	gSetScalingMode(Application, sm);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}


imeth	gAddImageControl(int y, int x, int height, int width, char *name, char *txt)
{
	object  ctl;
	int     sm;

	if (iClientWind)
		return gAddImageControl(iClientWind, y, x, height, width, name, txt);

	ctl = gNewWindowControl(ImageControl, 1, name, self);

	sm = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, width);
	gSetVirtualPosition(ctl, y, x);

	if(txt && (*txt))
		gSetStringValue(ctl, txt);

	gSetScalingMode(Application, sm);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}

imeth	gAddGenericControl(int y, int x, int height, int width, char *name, char *txt)
{
	object  ctl;
	int     sm;

	if (iClientWind)
		return gAddGenericControl(iClientWind, y, x, height, width, name, txt);

	ctl = gNewWindowControl(GenericControl, 1, name, self);

	sm = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, width);
	gSetVirtualPosition(ctl, y, x);

	if(txt && (*txt))
		gSetStringValue(ctl, txt);

	gSetScalingMode(Application, sm);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}


imeth	gAddLineControl(int y, int x, int height, int width, char *name, char *txt)
{
	object  ctl;
	int     sm;

	if (iClientWind)
		return gAddLineControl(iClientWind, y, x, height, width, name, txt);

	ctl = gNewWindowControl(LineControl, 1, name, self);
	gBackBrush(ctl, vNew(StockBrush, NULL_BRUSH) );

	sm = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, width);
	gSetVirtualPosition(ctl, y, x);

	gSetScalingMode(Application, sm);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}


imeth	gAddImageControl2(int y, int x, int *end, char *name, char *txt)
{
	object  font;
	int     height;
	object  ctl;
	long    wth;
	int     space;
	int     mode;

	if (iClientWind)
		return gAddImageControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);
	ctl = gNewWindowControl(ImageControl, 1, name, self);

	gBackBrush(ctl, vNew(StockBrush, NULL_BRUSH) );

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height*3, iModifyChildren ? (int) wth * 3 : (int) wth);

	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);

	if (end)
		*end = wth + x + space;

	gSetStyle(ctl, WS_VISIBLE);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);

	return ctl;
}

imeth	gAddGenericControl2(int y, int x, int *end, char *name, char *txt)
{
	object  font;
	int     height;
	object  ctl;
	long    wth;
	int     space;
	int     mode;

	if (iClientWind)
		return gAddGenericControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);
	ctl = gNewWindowControl(GenericControl, 1, name, self);

	gBackBrush(ctl, vNew(StockBrush, NULL_BRUSH) );

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height*3, iModifyChildren ? (int) wth * 3 : (int) wth);

	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);

	if (end)
		*end = wth + x + space;

	gSetStyle(ctl, WS_VISIBLE);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);

	return ctl;
}


imeth	gAddStaticTextControl(int y, int x, int *end, char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	long	wth;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddStaticTextControl(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(StaticTextControl, 1, name, self);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	gScaleToPixels(Application, &y, &x, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, iModifyChildren ? (int) wth * 5 : (int) wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
//	gSetStyle(ctl, WS_VISIBLE | WS_BORDER | ES_AUTOHSCROLL);
//	gSetStyle(ctl, WS_VISIBLE | ES_AUTOHSCROLL);
	gSetStyle(ctl, WS_VISIBLE | ES_MULTILINE);
	gSetStringValue(ctl, txt);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);
	return ctl;
}

imeth	gAddTextControl(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddTextControl(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(TextControl, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, (WS_VISIBLE | ES_AUTOHSCROLL) & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddNumericControl(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddNumericControl(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(NumericControl, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddDateControl(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddDateControl(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(DateControl, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddTimeControl(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddTimeControl(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(TimeControl, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddButton, gAddPushButton (int y, int x, int wth, int *end, int (*fun)(object ctl, object wnd),
			      char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddPushButton(iClientWind, y, x, wth, end, fun, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(PushButton, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetFunction(ctl, fun);
	gSetSize(ctl, (int)(1.7 * height), wth);
	gSetVirtualPosition(ctl, y, x);

	gSetStringValue(ctl, txt);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	return ctl;
}

imeth	gAddCheckBox(int y, int x, int wth, int *end, char *name, char *title)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddCheckBox(iClientWind, y, x, wth, end, name, title);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(CheckBox, 1, name, self);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, (int)(height), wth);
	gSetVirtualPosition(ctl, y, x);
	gSetTitle(ctl, title);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddListBox(int y, int x, int ht, int wth, int *end, char *name)
{
	object	font;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddListBox(iClientWind, y, x, ht, wth, end, name);

	font = gGetFont(Application);
	ctl = gNewWindowControl(ListBox, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, &ht, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);

	gSetSize(ctl, ht, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, (WS_VISIBLE | WS_VSCROLL) & ~WS_BORDER | LBS_NOTIFY);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);

	return ctl;
}

imeth	gAddComboBox(int y, int x, int ht, int wth, int *end, char *name)
{
	object	font;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddComboBox(iClientWind, y, x, ht, wth, end, name);

	font = gGetFont(Application);
	ctl = gNewWindowControl(ComboBox, 1, name, self);
	space = gAveCharWidth(font);

	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, &ht, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, ht, wth);
	gSetVirtualPosition(ctl, y, x);
	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL);
	gSetComboboxStyle(ctl, (int)CBS_DROPDOWNLIST);  // Yanghui

	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddRadioButton(int y, int x, int wth, int *end, char *name, char *title, char *next)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddRadioButton(iClientWind, y, x, wth, end, name, title, next);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowRadioButton(RadioButton, 1, name, self, next);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	gScaleToPixels(Application, &y, &x, font);
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, (int)(height), wth);
	gSetVirtualPosition(ctl, y, x);
	gSetTitle(ctl, title);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}


// YANGHUI:
imeth	gAddStaticControl2(int y, int x, int *end, char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	long	wth;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddStaticControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(StaticControl, 2, name, self);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(gGetBackBrush(self)));
	
	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, (int) wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStringValue(ctl, txt);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);

	return ctl;
}


imeth	gAddRectControl2(int y, int x, int *end, char *name, char *txt)
{
	object  font;
	int     height;
	object  ctl;
	long    wth;
	int     space;
	int     mode;

	if (iClientWind)
		return gAddRectControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(RectControl, 1, name, self);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, iModifyChildren ? (int) wth * 5 : (int) wth);

	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);

	if (end)
		*end = wth + x + space;
//	gSetStyle(ctl, WS_VISIBLE | WS_BORDER | ES_AUTOHSCROLL);
//	gSetStyle(ctl, WS_VISIBLE | ES_AUTOHSCROLL);
	gSetStyle(ctl, WS_VISIBLE | ES_MULTILINE);
	gSetStringValue(ctl, txt);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);

	return ctl;
}


imeth	gAddLineControl2(int y, int x, int *end, char *name, char *txt)
{
	object  ctl;
	long    width;
	int     mode;
	object  font;

	if (iClientWind)
		return gAddLineControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	width = gAveCharWidth(font);
	ctl = gNewWindowControl(LineControl, 1, name, self);

	gBackBrush(ctl, vNew(StockBrush, NULL_BRUSH) );
	
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, 1, (int) width * 30);

	gSetVirtualPosition2(ctl, y, x);

	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);

	return ctl;
}


imeth	gAddStaticTextControl2(int y, int x, int *end, char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	long	wth;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddStaticTextControl2(iClientWind, y, x, end, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(StaticTextControl, 1, name, self);
	wth = gStrPixelWidth(font, txt);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height, iModifyChildren ? (int) wth * 5 : (int) wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
//	gSetStyle(ctl, WS_VISIBLE | WS_BORDER | ES_AUTOHSCROLL);
//	gSetStyle(ctl, WS_VISIBLE | ES_AUTOHSCROLL);
	gSetStyle(ctl, WS_VISIBLE | ES_MULTILINE);
	gSetStringValue(ctl, txt);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	if (!iModifyChildren)
		gDisable(ctl);
	return ctl;
}

imeth	gAddTextControl2(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddTextControl2(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(TextControl, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font); 
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);

	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, (WS_VISIBLE | ES_AUTOHSCROLL) & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddNumericControl2(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddNumericControl2(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(NumericControl, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddDateControl2(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddDateControl2(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(DateControl, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddTimeControl2(int y, int x, int wth, int *end, char *name)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddTimeControl2(iClientWind, y, x, wth, end, name);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(TimeControl, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, height+2, wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_VISIBLE & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddButton2, gAddPushButton2 (int y, int x, int wth, int *end, int (*fun)(object ctl, object wnd),
			      char *name, char *txt)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddPushButton2(iClientWind, y, x, wth, end, fun, name, txt);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(PushButton, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);    // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetFunction(ctl, fun);
	gSetSize(ctl, (int)(1.7 * height), wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	gSetStringValue(ctl, txt);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	gAutoAttach(ctl, 0);
	return ctl;
}

imeth	gAddCheckBox2(int y, int x, int wth, int *end, char *name, char *title)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddCheckBox2(iClientWind, y, x, wth, end, name, title);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowControl(CheckBox, 1, name, self);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, (int)(height), wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	gSetTitle(ctl, title);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddListBox2(int y, int x, int ht, int wth, int *end, char *name)
{
	object	font;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddListBox2(iClientWind, y, x, ht, wth, end, name);

	font = gGetFont(Application);
	ctl = gNewWindowControl(ListBox, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, &ht, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);

	gSetSize(ctl, ht, wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, (WS_VISIBLE | WS_VSCROLL) & ~WS_BORDER);
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);

	return ctl;
}

imeth	gAddComboBox2(int y, int x, int ht, int wth, int *end, char *name)
{
	object	font;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddComboBox2(iClientWind, y, x, ht, wth, end, name);

	font = gGetFont(Application);
	ctl = gNewWindowControl(ComboBox, 1, name, self);
	space = gAveCharWidth(font);

	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, &ht, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, ht, wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	if (end)
		*end = wth + x + space;
	gSetStyle(ctl, WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL);
	gSetComboboxStyle(ctl, (int)CBS_DROPDOWNLIST);  // Yanghui

	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}

imeth	gAddRadioButton2(int y, int x, int wth, int *end, char *name, char *title, char *next)
{
	object	font;
	int	height;
	object	ctl;
	int	space;
	int	mode;

	if (iClientWind)
		return gAddRadioButton2(iClientWind, y, x, wth, end, name, title, next);

	font = gGetFont(Application);
	height = gLineHeight(font);
	ctl = gNewWindowRadioButton(RadioButton, 1, name, self, next);
	space = gAveCharWidth(font);

	gBackBrush(ctl, gCopy(iBackBrush));
	
	// gScaleToPixels(Application, &y, &x, font);  // Yanghui
	gScaleToPixels(Application, NULL, &wth, font);
	mode = gSetScalingMode(Application, SM_PIXELS);
	gSetSize(ctl, (int)(height), wth);

	// Yanghui:
	// gSetVirtualPosition(ctl, y, x);   
	gSetVirtualPosition2(ctl, y, x);
	// Yanghui

	gSetTitle(ctl, title);
	if (end)
		*end = wth + x + space;
	gDispose(font);
	gSetScalingMode(Application, mode);
	gAddControl(self, ctl, 1);
	return ctl;
}
// YANGHUI

imeth	gPlayMetaFile(char *file)
{
	if (iClientWind)
		return gPlayMetaFile(iClientWind, file);

	// Yanghui: set the meta file name object
	if (iMFName)
		iMFName = gDispose(iMFName); 

	if (file) 
		iMFName = gNewWithStr(String, file);
	// Yanghui

#ifndef unix
#ifdef	WIN32
	if (iHmf)
		if (iEnhancedMetaFile == 1)
			DeleteEnhMetaFile(iHmf);
		else
			DeleteMetaFile(iHmf);
	if (file) {
		if (iHmf = GetEnhMetaFile(file))
			iEnhancedMetaFile = 1;
#define	TEMPP 0
#if TEMPP  //  Windows 95 issues an error message if GetMetaFile fails (sometimes)
		else if  (iHmf = GetMetaFile(file))
			iEnhancedMetaFile = 0;
#endif
		else if  (iHmf = GetPlcMetaFile(self, iv, file, &iMfHdr))
			iEnhancedMetaFile = 2;
	} else
		iHmf = (HANDLE) 0;
#else
	if (iHmf)
		DeleteMetaFile(iHmf);
	if (file) {
#define	TEMPP 0
#if TEMPP  //  Windows 95 issues an error message if GetMetaFile fails (sometimes)
		if  (iHmf = GetMetaFile(file))
			iEnhancedMetaFile = 0;
		else
#endif
			if  (iHmf = GetPlcMetaFile(self, iv, file, &iMfHdr))
				iEnhancedMetaFile = 2;
	} else
		iHmf = (HANDLE) 0;
#endif
	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);
#endif
	return iHmf ? self : NULL;
}

#include <fcntl.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>


static	HMETAFILE	GetPlcMetaFile(object wind, ivType *iv, char *file, APM *mfHdr)
{
#ifndef unix
	HMETAFILE hmf;
	BOOL	isPlaceable = FALSE;
	int	hf;
	unsigned	toRead;
#ifdef	_WIN32
	char	*buf;
#else
	HGLOBAL hMem;
	char huge *buf;
#endif
	long filesize, offset, bytesRead;

	hf = open(file, _O_RDONLY | _O_BINARY);
	if (hf == -1) {
		gMoreHandles(LowFile);
		hf = open(file, _O_RDONLY | _O_BINARY);
	}
	if (hf == -1)
		return 0;

	// test for a "Placeable" Metafile format. if it is, then we need to get
	// the placeable header for scaling, but we want to skip it for playing.
	if (sizeof(APM) != read(hf, mfHdr, sizeof(APM))) {           // read in enough for the placement mf header
		close(hf);
		return 0;
	}
	filesize = lseek(hf, 0L, SEEK_END);    // get the metafile's size while we're here

	if (mfHdr->key == 0x9AC6CDD7L) {
		lseek(hf, (long) sizeof(APM), SEEK_SET);           // position beyond header for placement meta file
		isPlaceable = TRUE;            // flag as placment metafile
		filesize -= sizeof(APM);
	}
	else
		lseek(hf, 0L, SEEK_SET);           // otherwise position to start of file for normal mf

	// allocate a buffer big enough to hold the metafile
#ifdef	_WIN32
	buf = (char *) malloc((unsigned) filesize);
	if (!buf) {
		close(hf);
		return 0;
	}
#else
	hMem = GlobalAlloc(GPTR, filesize);

	if (!hMem) {
		close(hf);
		return 0;
	}

	// lock the buffer in memory while we read the metafile
	buf = (char huge *)GlobalLock(hMem);
#endif
	offset = 0;
	toRead = min((long) 0xFFFE, filesize);
	while (toRead) {
		bytesRead = read(hf, buf + offset, toRead);
		if (bytesRead < 0)
			break;
		offset += bytesRead;
		toRead = min((long) 0xFFFE, filesize-offset);
	}
	close(hf);

#ifdef	_WIN32
	hmf = SetMetaFileBitsEx((UINT) filesize, buf);
	if (!hmf) {
		free(buf);
		return 0;
	}
#else
	// let windows move the memory block now
	GlobalUnlock(hMem);

	// get handle to the metafile ... windows does the conversion for us

	hmf = SetMetaFileBits(hMem);
	if (!hmf) {
		GlobalFree(hMem);
		return 0;
	}
#endif
	{
		/*
		  kludgey way of enabling scrolling on metafiles
		  What I'm doing is copying the ScaleMetafile code to figure out how big the metafile is.
		  I then output some text (spaces via gTextOut) to those ending coordinates.
		*/
		
		int pm = gSetScalingMode(Application, SM_PIXELS);
		int xExt = mfHdr->bbox.right - mfHdr->bbox.left;
		int yExt = mfHdr->bbox.bottom - mfHdr->bbox.top;
		int	height, width, lx, ly;
		float	inches;

		float dViewportOrgX, dViewportOrgY;    // Yanghui
		
		HDC	hdc = GetWindowDC(iHwnd);

		SaveDC(hdc);  // save the device environment so we can put it back later
		SetMapMode(hdc, MM_ANISOTROPIC);

		// Yanghui:
		SetWindowOrgEx(hdc, 0, 0, NULL);

		if(iMFCxScale<=0)
			iMFCxScale = 1.0;

		if(iMFCyScale<=0)
			iMFCyScale = 1.0;

		if(iMFScale<=0)
			iMFScale = 1.0;

		if(iControlScale<=0)
			iControlScale = 1.0;

		if(iFontScale<=0)
			iFontScale = 1.0;

		// SetViewportOrgEx(hdc, -iHOffset, -iVOffset, NULL);  //  scroll parameters H, V
		SetViewportOrgEx(hdc, (int)( (dViewportOrgX=-iHOffset+iMFMargins.left*iControlScale*iMFCxScale)+0.5), 
		                      (int)( (dViewportOrgY=-iVOffset+iMFMargins.top*iControlScale*iMFCyScale)+0.5), NULL);  //  scroll parameters H, V
		
		// SetWindowExtEx(hdc, (int)((double)xExt/iMFScale), (int)((double)yExt/iMFScale), NULL);
		SetWindowExtEx(hdc, (int)( (double)xExt/(iMFScale*iMFCxScale)+0.5 ), 
		                    (int)( (double)yExt/(iMFScale*iMFCyScale)+0.5 ), NULL); 
		// Yanghui

		inches = (float) xExt / (float) mfHdr->inch;
		width = (inches * GetDeviceCaps(hdc, LOGPIXELSX)) + .5;
		lx = GetDeviceCaps(hdc, LOGPIXELSX);
		ly = GetDeviceCaps(hdc, LOGPIXELSY);

		inches = (float) yExt / (float) mfHdr->inch;
		height = (inches * GetDeviceCaps(hdc, LOGPIXELSY)) + .5;
		SetViewportExtEx(hdc, width, height, NULL);

		// Yanghui:
		{  // the correct way to enable scrolling on metafiles
			int nX=0, nY=0;

			if(iFont) {
				nX = gAveCharWidth(iFont); 
				nY = gLineHeight(iFont); 
			}
			else {
				HDC hdcTmp;
				TEXTMETRIC tmTmp;

				hdcTmp = GetDC(iHwnd);
				GetTextMetrics(hdcTmp, &tmTmp);
				ReleaseDC(iHwnd, hdcTmp);
				nX = tmTmp.tmAveCharWidth;
				nY = tmTmp.tmHeight + tmTmp.tmExternalLeading;
			}

			gTextOut(wind, (int)( (height+iMFMargins.bottom)*iMFScale*iMFCyScale+dViewportOrgY-nY), 
				           (int)( (width+iMFMargins.right)*iMFScale*iMFCxScale+dViewportOrgX-nX), " "); 
		}

		// gTextOut(wind, (int)(height*iMFScale), (int)(width*iMFScale), "      ");
		// Yanghui

		gSetScalingMode(Application, pm);
		RestoreDC(hdc, -1);        // restore the device environment
		ReleaseDC(iHwnd, hdc);
	}

	return hmf;
#endif
}

//#out1

typedef	struct {
	int	minx;
	int	miny;
}	ScreenMins;

static	int	CALLBACK	metaFunc(HDC		hdc,
					 HANDLETABLE	*tbl,
					 METARECORD	*rec,
					 int		nobj,
					 LPARAM		lParam)
{
	ScreenMins	*mins = (ScreenMins *) lParam;
	int	r = 1;
	if (rec->rdFunction == META_RECTANGLE) {
		int	savex, savey;

		savey = rec->rdParm[0];
		savex = rec->rdParm[1];
		if (rec->rdParm[0] - rec->rdParm[2] < mins->miny)
			rec->rdParm[0] = mins->miny + rec->rdParm[2];
		if (rec->rdParm[1] - rec->rdParm[3] < mins->minx)
			rec->rdParm[1] = mins->minx + rec->rdParm[3];
		PlayMetaFileRecord(hdc, tbl, rec, nobj);
		rec->rdParm[0] = savey;
		rec->rdParm[1] = savex;
	}
	else if (rec->rdFunction == META_EXTTEXTOUT) {  // Yanghui:
		int     nLen;
		LOGFONT logfont;
		HFONT   hfontOld, hfont;
		char    charFaceName[80];
		DWORD   dwVersion = GetVersion();
		DWORD   dwWindowsMajorVersion =  (DWORD)(LOBYTE(LOWORD(dwVersion)));
		// DWORD   dwWindowsMinorVersion =  (DWORD)(HIBYTE(LOWORD(dwVersion)));

		if(dwWindowsMajorVersion >= 5) {
			hfontOld = SelectObject(hdc, GetStockObject(SYSTEM_FONT));
			GetObject(hfontOld, sizeof(LOGFONT), &logfont);

			strcpy(charFaceName, logfont.lfFaceName);
			nLen = strlen(charFaceName);
			if(nLen > 0) {
				charFaceName[nLen] = '\0';
				if (!strcmpi(charFaceName, "TxFntB8")  || !strcmpi(charFaceName, "TxFntB10") ||
					!strcmpi(charFaceName, "TxFntB12") || !strcmpi(charFaceName, "TxFntB14") ||
					!strcmpi(charFaceName, "TxFntN6")  || !strcmpi(charFaceName, "TxFntN8") ||
					!strcmpi(charFaceName, "OCR-A"))
						logfont.lfCharSet = ANSI_CHARSET;  // reset the charset
			}
			hfont = CreateFontIndirect(&logfont);
			SelectObject(hdc, hfont);
		
			PlayMetaFileRecord(hdc, tbl, rec, nobj);
			DeleteObject(SelectObject(hdc, hfontOld));
		}
		else 
			PlayMetaFileRecord(hdc, tbl, rec, nobj);
	}
	else if (rec->rdFunction == META_PATBLT) {
		int	nWidth, nHeight;

		nHeight = rec->rdParm[2];
		nWidth  = rec->rdParm[3];

		if (nHeight < mins->miny-1)
			rec->rdParm[2] = mins->miny-1;

		if (nWidth < mins->minx-1)
			rec->rdParm[3] = mins->minx-1;

		PlayMetaFileRecord(hdc, tbl, rec, nobj);

		rec->rdParm[2] = nHeight;
		rec->rdParm[3] = nWidth;
	}  // Yanghui
	else
		PlayMetaFileRecord(hdc, tbl, rec, nobj);
	return r;
}

static	BOOL	playMetaFile(HDC hdc, HMETAFILE hmf)
{
	SIZE	vp, we;
	ScreenMins	mins;

	GetViewportExtEx(hdc, &vp);
	GetWindowExtEx(hdc, &we);
	mins.minx = 1 + MulDiv(1, we.cx, vp.cx);
	mins.miny = 1 + MulDiv(1, we.cy, vp.cy);
	return EnumMetaFile(hdc, hmf, metaFunc, (LPARAM) &mins);
}

static	void	PlayPlcMetaFile(object wind, ivType *iv, HDC hdc, HMETAFILE hmf, APM *mfHdr)
{
	BOOL	r;

	SaveDC(hdc);  // save the device environment so we can put it back later

	SetMapMode(hdc, MM_ANISOTROPIC);

//	if (isPlaceable)   // if a placement meta file, scale it
		ScaleMetafile(wind, iv, hdc, mfHdr);

	r = playMetaFile(hdc, hmf);  // play the metafile

	RestoreDC(hdc, -1);        // restore the device environment
}

#if 0
	DeleteMetaFile(hmf);     // remove metafile from memory
#ifdef	_WIN32
	free(buf);
#else
	GlobalFree(hMem);
#endif
	return hmf;
#endif


#define	SCALE(a)	(int)(a*scale)

imeth	double gSetMFScale(object self, double val)
{
	double oldMFScale;
	if (iClientWind)
		return gSetMFScale(iClientWind, val);

	oldMFScale = iMFScale;
	iMFScale = val;

	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);
	return oldMFScale;
}	

// Yanghui:

imeth	double gGetMFScale(object self)
{
	if (iClientWind)
		return gGetMFScale(iClientWind);

	if(iMFScale<=0)
		iMFScale = 1.0; 

	return iMFScale;
}

imeth	double gGetControlScale(object self)
{
	if (iClientWind)
		return gGetControlScale(iClientWind);

	if(iControlScale<=0)
		iControlScale = 1.0; 

	return iControlScale;
}

imeth	double gGetFontScale(object self)
{
	if (iClientWind)
		return gGetFontScale(iClientWind);

	if(iFontScale<=0)
		iFontScale = 1.0; 

	return iFontScale;
}

imeth gSetMFCxCyScale(double dCxScale, double dCyScale)
{
	if (iClientWind)
		return gSetMFCxCyScale(iClientWind, dCxScale, dCyScale);

	iMFCxScale = dCxScale;
	iMFCyScale = dCyScale;

	if(iMFCxScale<=0)
		iMFCxScale = 1.0;

	if(iMFCyScale<=0)
		iMFCyScale = 1.0;

	return self;
}

imeth gGetMFCxCyScale(double *dCxScale, double *dCyScale)
{
	if (iClientWind)
		return gGetMFCxCyScale(iClientWind, dCxScale, dCyScale);

	*dCxScale = iMFCxScale;
	*dCyScale = iMFCyScale;
	return self;
}

// Get the printer scale for the cld file 
imeth    double gGetPrinterScale()
{
	if (iClientWind)
		return gGetPrinterScale(iClientWind);
	return iPrinterScale;
}


// Set the printer scale for the cld file 
imeth  double gSetPrinterScale(double printerScale)
{
	double oldPrinterScale;
	if (iClientWind)
		return gSetPrinterScale(iClientWind, printerScale);

	oldPrinterScale = iPrinterScale;
	iPrinterScale = printerScale;
	if(iPrinterScale<=0)
		iPrinterScale = 1.0;

	return oldPrinterScale;
}


imeth gSetMFMargins(RECT *pRect)
{
	if (iClientWind)
		return gSetMFMargins(iClientWind, pRect);

	CopyRect(&iMFMargins, pRect);

	return self;
}


imeth gGetMFMargins(RECT *pRect)
{
	if (iClientWind)
		return gGetMFMargins(iClientWind, pRect);

	CopyRect(pRect, &iMFMargins);

	return self;
}

imeth gToggleTheRulerAndGrid()
{
	if (iClientWind)
		return gToggleTheRulerAndGrid(iClientWind);

	if(++iRulerAndGrid > 4)
		iRulerAndGrid = 0;

	InvalidateRect(iHwnd, NULL, TRUE);
	return self;
}


imeth gSetPageMargins(int nTopMargin, int nBottomMargin)
{
	if (iClientWind)
		return gSetPageMargins(iClientWind, nTopMargin, nBottomMargin);

	iPageTopMargin = nTopMargin;
	iPageBottomMargin = nBottomMargin;
	return self;
}


imeth gGetPageMargins(int *p_nTopMargin, int *p_nBottomMargin)
{
	if (iClientWind)
		return gGetPageMargins(iClientWind, p_nTopMargin, p_nBottomMargin);

	*p_nTopMargin = iPageTopMargin;
	*p_nBottomMargin = iPageBottomMargin;
	return self;
}

// Yanghui	

static	void  ScaleMetafile(object wind, ivType *iv, HDC hdc, APM *mfHdr)
{
	int xExt, yExt;
	int	height;
	int	width;
	float	inches;

	SetMapMode(hdc, MM_ANISOTROPIC);

	// Yanghui:
	SetWindowOrgEx(hdc, 0, 0, NULL);

	if(iMFCxScale<=0)
		iMFCxScale = 1.0;

	if(iMFCyScale<=0)
		iMFCyScale = 1.0;

	if(iMFScale<=0)
		iMFScale = 1.0;

	if(iControlScale<=0)
		iControlScale = 1.0;

	if(iFontScale<=0)
		iFontScale = 1.0;

	// SetViewportOrgEx(hdc, -iHOffset, -iVOffset, NULL);  //  scroll parameters H, V
	SetViewportOrgEx(hdc, -iHOffset+(int)(iMFMargins.left*iControlScale*iMFCxScale+0.5), 
	                      -iVOffset+(int)(iMFMargins.top*iControlScale*iMFCyScale+0.5), NULL);  //  scroll parameters H, V
	// Yanghui

	xExt = mfHdr->bbox.right - mfHdr->bbox.left;
	yExt = mfHdr->bbox.bottom - mfHdr->bbox.top;

	// Yanghui:
	SetWindowExtEx(hdc, (int)((double)xExt/(iMFScale*iMFCxScale)), (int)((double)yExt/(iMFScale*iMFCyScale)), NULL);
	// SetWindowExtEx(hdc, (int)((double)xExt/iMFScale+0.5), (int)((double)yExt/iMFScale+0.5), NULL);
	// Yanghui

	inches = (float) xExt / (float) mfHdr->inch;
	width = (inches * GetDeviceCaps(hdc, LOGPIXELSX)) + .5;

	inches = (float) yExt / (float) mfHdr->inch;
	height = (inches * GetDeviceCaps(hdc, LOGPIXELSY)) + .5;

	SetViewportExtEx(hdc, width, height, NULL);

}

//#out2

imeth	gAddSplashWindow(int id, int (*fun)())
{
	gInitFunction(self, fun);
	iSplashID = id;
	
	return self;
}

imeth	gAddSplashWindowWithInt(int id, int tm)
{
	iSplashID = id;
	iSplashTime = tm;
	return self;
}

imeth	gGetRect(RECT *r)
{
	int	row, col, height, width;

	gGetPosition(self, &row, &col);
	gGetSize(self, &height, &width);
	r->top = row;
	r->left = col;
	r->bottom = row + height;
	r->right = col + width;
	return self;
}


// Yanghui:
// get the original color when the child control is being moved
imeth   COLORREF    gGetHoldColor()
{
	return iHoldColor;
}


imeth   gSetHoldColor(COLORREF colorRef)
{
	iHoldColor = colorRef;
	return self;
}


imeth	gGetClientRect(RECT *r)
{
	if (iHwnd)
		GetClientRect(iHwnd, r);
	else {
		gGetRect(self, r);
		r->bottom -= r->top;
		r->top = 0;
		r->right -= r->left;
		r->left = 0;
	}
	return self;
}

imeth	gGetWindowRect(RECT *r)
{
	int	pm = gSetScalingMode(Application, SM_PIXELS);
	gGetRect(self, r);
	gSetScalingMode(Application, pm);

	return self;
}

// update the window rect and the virtual window position
imeth	gSetWindowRect(RECT *r)
{
	int	pm = gSetScalingMode(Application, SM_PIXELS);
	gSetPosition(self, r->top, r->left);
	gSetSize(self, r->bottom - r->top, r->right - r->left);
	gSetScalingMode(Application, pm);
	
	return self;
}
// Yanghui

imeth	gUpdateScrollData(ctl)
{
	int	pm;
	RECT	cr, wr, wcr;
	int	crow, ccol, wrow, wcol;

	if (iIsToolBar)
		return NULL;
	
	pm = gSetScalingMode(Application, SM_PIXELS);
	GetClientRect(iHwnd, &wcr);
	GetWindowRect(iHwnd, &wr);
	gGetPosition(self, &wrow, &wcol);

	GetWindowRect(gHandle(ctl), &cr);
	gGetPosition(ctl, &crow, &ccol);

	if (cr.bottom > (wr.top + wcr.bottom))
		gScrollVert(self, (cr.bottom - (wr.top + wcr.bottom)));

	if (cr.right > (wr.left + wcr.right))
		if (cr.right - cr.left <= wr.right - wr.left) // If not as wide as the parent window.
			gScrollHorz(self, (cr.right - (wr.left + wcr.right)));
		else                                                            // Wider than it's parent window.
			gScrollHorz(self, (cr.left - (GetSystemMetrics(SM_CYDLGFRAME) + 2)) - wr.left);

	GetWindowRect(gHandle(ctl), &cr);
	gGetPosition(ctl, &crow, &ccol);

// I'm not sure about this, so I'm testing.
//	if (crow < wrow)
//		gScrollVert(self, crow - wrow);
//	if (ccol < wcol)
//		gScrollHorz(self, ccol - wcol);

	if (cr.top < wr.top)
		gScrollVert(self, cr.top - wr.top);
	if (cr.left < wr.left)
		gScrollVert(self, cr.left - wr.left);
	gSetScalingMode(Application, pm);
	return self;
}


//  Give a unique tab order to each control

static	void	check_taborders(object ctls)
{
	object	seq, ctl;
	int	highest=0, current, i, n, m;

	for (seq=gSequence(ctls) ; ctl = gNext(seq) ; ) {
		current = gGetTabOrder(ctl);
		if (current > highest)
			highest = current;
	}
	m = gSize(ctls) - 1;
	for (n=0 ; n < m ; n++)
		for (i=0, seq=gSequence(ctls) ; ctl = gNext(seq) ; ++i)
			if (n == i)
				current = gGetTabOrder(ctl);
			else if (i > n)
				if (gGetTabOrder(ctl) == current  &&  current >= 0)
					gSetTabOrder(ctl, ++highest);
}

#define	CLD_VERSION	2
static	char	CLD_CODE[] = "CLD";
static	char	CLIPBOARD_CODE[] = "CLIPBOARD";  // Yanghui

imeth	gSaveControls(char *file)
{
	FILE	*fp;
	object	seq, ctl, lctl;
	int	sm, m, n, low=0, done = -1, current;
	CLD_HEADER_t	head;
	short	size = sizeof head;
	HDC  hDc;   // Yanghui
	
	if (iClientWind)
		return gSaveControls(iClientWind, file);
	if (!iControls)
		return NULL;

	fp = fopen(file, "wb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "wb");
	}
	if (!fp)
		return NULL;
	if (1 != fwrite(CLD_CODE, sizeof CLD_CODE, 1, fp)  ||
	    1 != fwrite(&size, sizeof size, 1, fp)) {
		fclose(fp);
		return NULL;
	}

	// Yanghui:
	// save the current resolution and horizontal and vertical 
	// dots per inch in the CLD file so that 
	// the next time when the saved file is loaded in computers with
	// different resolution, it will look the same by proper scaling
	// the meta file scaling factor and the meta file name length
	// are also saved
	
	memset(&head, 0, sizeof(head));

	head.version   = CLD_VERSION;
	head.cxInPixel = GetSystemMetrics(SM_CXSCREEN);
	head.cyInPixel = GetSystemMetrics(SM_CYSCREEN);

	hDc = GetDC(iHwnd);
	    // If iHwnd is NULL, GetDC retrieves the device context for the entire screen. 

	head.logPixelsx = GetDeviceCaps(hDc, LOGPIXELSX);
	head.logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
	ReleaseDC(iHwnd, hDc);

	head.metaFileNameLen = iMFName ? gSize(iMFName)+1 : 0;
	head.metaFileScale = head.metaFileNameLen>0 ? iMFScale : 1.0;

	if (1 != fwrite(&head, sizeof head, 1, fp)) {
		fclose(fp);
		return NULL;
	}

	// get the meta file name and save it in the file
	if (head.metaFileNameLen)
		if ( 1 != fwrite(gStringValue(iMFName), head.metaFileNameLen, 1, fp) ) {
			fclose(fp);
			return NULL;
		}
	// Yanghui
		
	sm = gSetScalingMode(Application, SM_PIXELS);
	
	// Yanghui:
	// first save LineControl, RectControl, StaticTextControl
	if(iControls)
		for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
				if( ClassOf(ctl) == LineControl || ClassOf(ctl) == RectControl || 
					ClassOf(ctl) == StaticTextControl )
					if( gGetTabOrder(ctl) >= 0 )
						gSaveControl(ctl, fp); 
		}
	// Yanghui

	check_taborders(iControls);
	m = gSize(iControls);
	for (n=0 ; n++ < m ; ) {
		lctl = NULL;
		for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
			current = gGetTabOrder(ctl);
			if (current > done  &&  (!lctl  ||  current < low)) { 
				lctl = ctl;
				low = current;
			}
		}
		if (lctl) {
			if (low >= 0)
				if( ClassOf(lctl)!=LineControl && ClassOf(lctl)!=RectControl &&  // Yanghui
					ClassOf(lctl)!=StaticTextControl )                           // Yanghui
						gSaveControl(lctl, fp);
			done = low;
		}
	}
	gSetScalingMode(Application, sm);
	fclose(fp);
	return self;
}

static	void	removeSection(ivType *iv, char *file)
{
	object	seq, lnk, obj;

	for (seq=gSequenceLinks(iCtlSection) ; lnk=gNext(seq) ; )
		if (!strcmp(gStringKey(obj = gValue(lnk)), file)) {
			gDispose(obj);
			gDispose(lnk);
			gDispose(seq);
			break;
		}
}

static	void	removeControl(ivType *iv, object ctl)
{
	object	seq, lnk, vec;

	gRemoveStr(iControlDict, gName(ctl));
	for (seq=gSequenceLinks(iControls) ; lnk=gNext(seq) ; )
		if (gValue(lnk) == ctl) {
			gDeepDispose(lnk);
			gDispose(seq);
			break;
		}
	for (seq=gSequence(iClientArea) ; lnk=gNext(seq) ; ) {
		vec = gValue(lnk);
		if (gIsKindOf(vec, ControlVector)  &&  ctl == gControl(vec)) {
			gDispose(lnk);
			gDispose(seq);
			break;
		}
	}
}

private	imeth	pRemoveFrom(ctl)
{
	object	seq, tctl;

	if (!ctl)
		return NULL;
	
	if (iClientWind)
		return pRemoveFrom(iClientWind, ctl);
	for (seq=gSequence(iControls) ; seq  &&  (tctl = gNext(seq)) ; )
		if (ctl == tctl) {
			do {
				removeControl(iv, tctl);
			} while (seq  &&  (tctl = gNext(seq)));
			seq = NULL;
		}
	pUpdateMax(self);
	update_vert(iv);
	update_horz(iv);
	InvalidateRect(iHwnd, NULL, TRUE);
	return self;
}

imeth	gRemoveControl(object ctl)
{accessIVs;

	removeControl(iv, ctl);
	return self;
}

imeth	gRemoveFromControl(object ctl)
{
	return pRemoveFrom(self, ctl);
}
		  
private	imeth	pFindSection(char *file)
{
	object	seq, obj;
	object	robj = NULL;
	
	for (seq=gSequence(iCtlSection) ; obj = gNext(seq) ; )
		if (!strcmp(gStringKey(obj), file))
			robj = gValue(obj);

	return robj;
}

imeth	gRemoveFrom(char *file)
{
	object	seq, obj, ctl = NULL;

	if (iClientWind)
		return gRemoveFrom(iClientWind, file);
	
	if (!iCtlSection)
		return NULL;

	for (seq=gSequence(iCtlSection) ; seq  &&  (obj = gNext(seq)) ; )
		if (!strcmp(gStringKey(obj), file)) {
			ctl = gValue(obj);
			do {
				removeSection(iv, gStringKey(obj));
			} while (seq  &&  (obj = gNext(seq)));
			seq = NULL;
		}
	if (!ctl)
		return NULL;

	if (ctl)
		pRemoveFrom(self, ctl);
	
	return self;
}

imeth	gAppendControls(char *file)
{
	object	r, seq, ctl, fctl;
	int	sm, x, y, h, w, x2, y2;
	
	if (pCheckAndLoadXML(self,file))
		return self;
	
	if (iClientWind)
		return gAppendControls(iClientWind, file);

	if (iCtlSection &&  pFindSection(self, file))
		return NULL;

	sm = gSetScalingMode(Application, SM_PIXELS);
	iXAppendOffset = iYAppendOffset = 0;
	if (iControls)
		for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
			gGetVirtualPosition(ctl, &y, &x);
			gGetPosition(ctl, &y2, &x2);
			gGetSize(ctl, &h, &w);
			if (x + w > iXAppendOffset)
				iXAppendOffset = x + w;
			if (y + h > iYAppendOffset)
				iYAppendOffset = y + h;
		}
	iXAppendOffset = 0;
	r = pLoadControls(self, file, 1);
	iXAppendOffset = iYAppendOffset = 0;
	gSetScalingMode(Application, sm);
	if (!r)
		return NULL;

	if (!iCtlSection)
		iCtlSection = gNew(LinkObject);

	gAddLast(iCtlSection, gNewWithStrObj(StringAssociation, file, r));

	return r;
}

static	int	laterTime(char *ff, char *tf)
{
	int	rval = 0;
	struct	_stat	fsb, tsb;
	long	ft, tt;

	if (!_stat(tf, &tsb)  &&  !_stat(ff, &fsb))
		rval = (fsb.st_mtime <= tsb.st_mtime);

	return rval;
}
		  
private	imeth	pLoadGUI(object self, char *fname)
{
	char    fbuf[_MAX_PATH];
	char    cbuf[_MAX_PATH];
	char	*p;
	object	rval = NULL;

	strcpy(fbuf, fname);
	p = strrchr(fbuf, '.');
	
	if (p && !stricmp(p, ".cld")) {
		strcpy(p, "_.cld");
		strcpy(cbuf, fbuf);
		strcpy(p, "_.pmf");

		if (!access(cbuf, 0) && !access(fbuf, 0) && laterTime(fname, cbuf) && laterTime(fname, fbuf))
			rval = gLoadGUI(self, fbuf, cbuf);
	}
	return rval;	
}


private imeth int pCheckAndLoadXML(self,char *fname)
{

	FILE * fyle=fopen(fname,"rb");
	
	if (!fyle)
		return 0;
	else
	{
		char c=fgetc(fyle);
		int loop;
		iLastLoadType=LOAD_TYPE_BINARY;

		//take a quick peek inside the file and see if it's xml
			
		
		for (loop=0;(loop<100) && (c!=EOF);loop++) {
			if (c=='<') {
				iLastLoadType=LOAD_TYPE_XML;
				break;
			}
			if (c!=' '  &&  c!='\f'  && c!='\n') {
				iLastLoadType=LOAD_TYPE_BINARY;
				break;
			}
			c=fgetc(fyle);
		}
		
		fclose(fyle);

		//it is xml, so skip over to the xml load

		if (iLastLoadType==LOAD_TYPE_XML) {
			iMFName=NULL;
			gLoadXMLFile(self,fname);
			return 1;
		}
		
		return 0;
	}
}


private	imeth	pLoadControls(char *file, int checkMF)
{
	FILE	*fp;
	short	type, size;
	CLD_HEADER_t	head;
	int	sm;
	object	ctl;
	char	code[sizeof CLD_CODE];
	object	firstCtl = NULL;

	// Yanghui:
	char* tmpMFName; 
	HDC   hDc;
	int   logPixelsx;
	int	fileok = 1;
	// Yanghui
	
	if (iClientWind)
		return pLoadControls(iClientWind, file, checkMF);
	if (!iYAppendOffset)
		gDisposeAllControls(self);

	if ((!iCtlSection || !gSize(iCtlSection)) && (checkMF && pLoadGUI(self, file)))
		return self;
	
	fp = fopen(file, "rb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "rb");
	}
	if (!fp)
		return NULL;
	if (1 != fread(code, sizeof code, 1, fp)  ||
	    1 != fread(&size, sizeof size, 1, fp)  ||
	    strcmp(code, CLD_CODE)) {
		fclose(fp);
		return NULL;
	}

	memset(&head, 0, sizeof head);
	if (size == 1) {  // old format - size was version number and there was no header
		head.version = 1;
		head.cxInPixel = 800;
		head.cyInPixel = 600;
		head.logPixelsx = 96;    // default to Small Fonts
		head.logPixelsy = 96;    // default to Small Fonts
		head.metaFileScale = 1.0;
		head.metaFileNameLen = 0;

	} else {
		// if (1 != fread(&head, (int) (size > sizeof(head) ? sizeof(head) : size), 1, fp)) 
		if (1 != fread(&head, (int)min(size, sizeof(head)), 1, fp)) {  // Yanghui
			fclose(fp);
			return NULL; 
		}
		if (size > sizeof(head)) 
			fseek(fp, (long)(size-sizeof(head)), SEEK_CUR); 
		else if(size < sizeof(head)) {
			head.logPixelsx = 96;    // default to Small Fonts 
			head.logPixelsy = 96;    // default to Small Fonts
		}
	}

	iMFScale = head.metaFileNameLen>0 ? head.metaFileScale : 1.0;
	iControlScale = 1.0;
	iFontScale = 1.0;

	iPrinterScale = head.cxInPixel/800.0;

	// for different screen width and height and font size: do the scaling if the scaling flag is set
	if (gGetScaleFlg(self)) {
		if(head.cxInPixel>0) {
			iMFScale = iMFScale*GetSystemMetrics(SM_CXSCREEN) / (double) head.cxInPixel;
			iControlScale = iControlScale*GetSystemMetrics(SM_CXSCREEN) / (double) head.cxInPixel;
		}

		hDc = GetDC(iHwnd);
		// If this iHwnd is NULL, GetDC retrieves the device context for the entire screen. 
		logPixelsx = GetDeviceCaps(hDc, LOGPIXELSX);
		ReleaseDC(iHwnd, hDc);
		
		if(logPixelsx>0) {
				iFontScale = (double)logPixelsx/head.logPixelsx;
				iMFScale = iMFScale/iFontScale;
		}
	}

	if (iMFName)
		iMFName = gDispose(iMFName);

#ifndef unix
#ifdef	WIN32
	if (iHmf)
		if (iEnhancedMetaFile == 1) 
			DeleteEnhMetaFile(iHmf);
		else
			DeleteMetaFile(iHmf);
#else
	if (iHmf)
		DeleteMetaFile(iHmf);
#endif
	iHmf = (HANDLE) 0;
#endif

	if (head.metaFileNameLen) {
		tmpMFName = (char *)malloc(head.metaFileNameLen);
		if (tmpMFName) {
			fread(tmpMFName,  (int) (head.metaFileNameLen), 1, fp);
			iMFName = gNewWithStr(String, tmpMFName);
			free(tmpMFName);
		} else {
			fclose(fp);
			return NULL;
		}
	}
	
	sm = gSetScalingMode(Application, SM_PIXELS);
	while (fileok && 1 == fread(&type, sizeof type, 1, fp)) {
		switch (type) {
		case CTLTYPE_STATIC:
			ctl = gLoadControl(StaticTextControl, fp, self);
			break;
		case CTLTYPE_TEXT:
			ctl = gLoadControl(TextControl, fp, self);
			break;
		case CTLTYPE_NUMERIC:
			ctl = gLoadControl(NumericControl, fp, self);
			break;
		case CTLTYPE_DATE:
			ctl = gLoadControl(DateControl, fp, self);
			break;
		case CTLTYPE_TIME:
			ctl = gLoadControl(TimeControl, fp, self);
			break;
		case CTLTYPE_PUSHBUTTON:
			ctl = gLoadControl(PushButton, fp, self);
			break;
		case CTLTYPE_RADIOBUTTON:
			ctl = gLoadControl(RadioButton, fp, self);
			break;
		case CTLTYPE_CHECKBOX:
			ctl = gLoadControl(CheckBox, fp, self);
			break;
		case CTLTYPE_LISTBOX:
			ctl = gLoadControl(ListBox, fp, self);
			break;
		case CTLTYPE_COMBOBOX:
			ctl = gLoadControl(ComboBox, fp, self);
			break;
		case CTLTYPE_RECT:
			ctl = gLoadControl(RectControl, fp, self);
			break;
		case CTLTYPE_LINE:
			ctl = gLoadControl(LineControl, fp, self);
			break;
		case CTLTYPE_IMAGE:
			ctl = gLoadControl(ImageControl, fp, self);
			break;
		default:
			fileok = 0;
			break;
		}
		if (fileok) {
			char	fbuf[256], *fnp;

			strcpy(fbuf, file);
			if (fnp = strstr(fbuf, "_.cld"))
				strcpy(fnp, ".cld");
			gSetFileResource(ctl, fbuf);
		}
//		gSetFont(ctl, gCopy(iFont));
		if (!firstCtl)
			firstCtl = ctl;
//		if (iBeenPerformed)
//			gShow(ctl);
	}
	gSetScalingMode(Application, sm);
	fclose(fp);
	pUpdateMax(self);
	update_vert(iv);
	update_horz(iv);
	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);

	if (!fileok  &&  firstCtl) {
		pRemoveFrom(self, firstCtl);
		firstCtl = NULL;
	}
	return firstCtl;
}

imeth	gLoadControls(char *file)
{
	if (pCheckAndLoadXML(self,file))
		return self;
	else {
		object	r = pLoadControls(self, file, 1);

		if (!iCtlSection)
			iCtlSection = gNew(LinkObject);

		gAddLast(iCtlSection, gNewWithStrObj(StringAssociation, file, r));

		return r;
	}
}

// Yanghui:
/**************
imeth	gLoadGUI(char *wmf, char *cld)
{
	iBeenPerformed = 0;
	gPlayMetaFile(self, wmf);
	gLoadControls(self, cld);
	return self;
}
**************/

imeth	gLoadGUI(char *wmf, char *cld)
{
	char	file[128];
	char	*pChar;  
	object  aMFName;
	double	scalingFactor;

	iBeenPerformed = 0;
	
	
	if (pCheckAndLoadXML(self,cld))
		return self;
		
	if (!pLoadControls(self, cld, 0))
		return NULL;

	if (wmf) {
		gPlayMetaFile(self, wmf);
		return self;
	}

	aMFName = gGetMFName(self);
	if (aMFName) {  // if metafile name is not specified explicitly, 
                        // then check to see if it is specified in the cld head
		pChar = gStringValue(aMFName);  // the associated meta file 
		if (pChar) {   // load the metafile 
			strcpy(file, pChar);
			gPlayMetaFile(self, file);
		}
	}

	return self;
}
// Yanghui
	  
imeth	gAddBitmap(UINT bmpID, int mode, int y, int x)
{
	if (iClientWind)
		return gAddBitmap(iClientWind, bmpID, mode, y, x);

	if (iBmp)
		gDispose(iBmp);

	iBmp = gNewBitmap(Bitmap, bmpID, mode, y, x, self);
	
	return self;
}

// Yanghui:
imeth	gGetBitmap()
{
	if (iClientWind)
		return gGetBitmap(iClientWind);

	return iBmp;
}
// Yanghui

imeth	gRemoveBitmap(UINT bmpID)
{
	if (iBmp)
		iBmp = gDispose(iBmp);

	return self;
}

imeth	gDisableObject(obj)	//  needed by tasklist code
{
	iDisableObject = obj;
	return self;
}

imeth	gSetActivateFunction(ifun fun)
{
	if (iActivateFunctions)
		gDeepDispose(iActivateFunctions);
	
	iActivateFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(iActivateFunctions, fun);
	return self;
}

imeth	gAddActivateFunctionBefore(int (*fun)())
{
	if (!iActivateFunctions)
		iActivateFunctions = gNew(FunctionList);

	gAddFunctionBefore(iActivateFunctions, fun);
	
	return self;
}

imeth	gAddActivateFunctionAfter(int (*fun)())
{
	if (!iActivateFunctions)
		iActivateFunctions = gNew(FunctionList);

	gAddFunctionAfter(iActivateFunctions, fun);
	
	return self;
}

imeth	gAddAppendOffsets(short *y, short *x)
{
	*x += iXAppendOffset;
	*y += iYAppendOffset;
	return self;
}

imeth	ofun	gSetToolBarMouseFunction(unsigned id, unsigned button, ifun fun)
{
	return gSetToolBarMouseFunction(iToolBar, id, button, fun);
}

imeth	ofun	gSetMenuMouseFunction(unsigned id, unsigned button, ifun fun)
{
	return gSetMenuMouseFunction(iMenu, id, button, fun);
}

imeth	gGetToolControl(char *name)
{
	if (iToolBar)
		return gGetControlStr(iToolBar, name);
	return NULL;
}

imeth	gRedrawWindow()
{
	InvalidateRect(iHwnd, NULL, TRUE);
	return self;
}

imeth	gUpdateAccessMode()
{
	if (iMenu)
		gUpdateAccessMode(iMenu);

	if (iToolBar)
		gUpdateAccessMode(iToolBar);
	
	return self;
}

imeth	gSetTask(task)
{
	return iTask = task;
}

imeth	gGetTask()
{
	return iTask;
}

cmeth	ifun	gSetAccessModeFunction(ifun f)
{
	ifun	org = cAccessMode;
	cAccessMode = f;
	return org;
}

private	imeth	int	modalWindowParam(object self)
{
	HWND	pw = iParent ? gHandle(iParent) : (HWND) 0;
	
	if (IsWindow(pw)) {
		while (GetWindowStyle(pw) & WS_CHILD)
			pw = GetParent(pw);
		if (pw)
			EnableWindow(pw, FALSE);
	}
	iEndWindow = 0;

	pPerformControls(self);

	gProcessModalWindowMessages(MessageDispatcher, &iEndWindow, iHwnd);
	if (pw)
		EnableWindow(pw, TRUE);

	if (iParent)
		gSetZOrder(iParent, HWND_TOP);
	
	return iModalResult;
}

imeth	gEndPerform(int res)
{
	iModalResult = res;
	iEndWindow = 1;

	return self;
}


// Yanghui:
imeth	gSaveControlsToCldAndMetaFile(char *cldFile, char *plcMetaFile)
{
	CLD_HEADER_t  head;
	FILE          *fp;
	object        seq, ctl, lctl;
	int           sm, m, n, low=0, done = -1, current;
	short         size = sizeof head;
	HDC           hdcView, hdcMeta, hdcEMF;
	char          metaFile[128];
	APM           mfHdr;
	int           nMetaFile, nFileSize, nMapModeOld;
	RECT          rect;
	char          *buf;
	POINT         ptWindowOrgOld, ptViewportOrgOld;
	SIZE          szWindowExtOld, szViewportExtOld;

	if (iClientWind)
		return gSaveControlsToCldAndMetaFile(iClientWind, cldFile, plcMetaFile);

	if (!cldFile || !plcMetaFile || !iControls)
		return NULL;

	n = strlen(plcMetaFile)-4;
	if(n<=0)
		return NULL;
	strncpy(metaFile, plcMetaFile, n);
	strcpy(metaFile+n, ".wmf");

	fp = fopen(cldFile, "wb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(cldFile, "wb");
	}
	if (!fp)
		return NULL;

	if (1 != fwrite(CLD_CODE, sizeof CLD_CODE, 1, fp)  ||
	    1 != fwrite(&size, sizeof size, 1, fp)) {
		fclose(fp);
		return NULL;
	}

	// save the current resolution and horizontal and vertical 
	// dots per inch in the CLD file so that 
	// the next time when the saved file is loaded in computers with
	// different resolution, it will look the same by proper scaling
	// the meta file scaling factor and the meta file name length
	// are also saved
	
	memset(&head, 0, sizeof(head));

	head.version   = CLD_VERSION;
	head.cxInPixel = GetSystemMetrics(SM_CXSCREEN);
	head.cyInPixel = GetSystemMetrics(SM_CYSCREEN);

	hdcView = GetDC(iHwnd);  // If iHwnd is NULL, GetDC retrieves the device context for the entire screen. 
	head.logPixelsx = GetDeviceCaps(hdcView, LOGPIXELSX);
	head.logPixelsy = GetDeviceCaps(hdcView, LOGPIXELSY);
	head.metaFileNameLen = iMFName ? gSize(iMFName)+1 : 0;
	head.metaFileScale = head.metaFileNameLen>0 ? iMFScale : 1.0;

	if (1 != fwrite(&head, sizeof head, 1, fp)) {
		ReleaseDC(iHwnd, hdcView);
		fclose(fp);
		return NULL;
	}

	// get the meta file name and save it in the file
	if (head.metaFileNameLen)
		if ( 1 != fwrite(gStringValue(iMFName), head.metaFileNameLen, 1, fp) ) {
			ReleaseDC(iHwnd, hdcView);
			fclose(fp);
			return NULL;
		}

	// nSaveDC = SaveDC(hdcView);
	nMapModeOld = SetMapMode(hdcView, MM_ANISOTROPIC);
	SetWindowOrgEx(hdcView, 0, 0, &ptWindowOrgOld);
	SetWindowExtEx(hdcView, head.logPixelsx, head.logPixelsx, &szWindowExtOld);
	
	SetViewportOrgEx(hdcView, 0, 0, &ptViewportOrgOld);
	SetViewportExtEx(hdcView, 300, 300, &szViewportExtOld);
	// SetViewportExtEx(hdcView, head.logPixelsx, head.logPixelsx, &szViewportExtOld);

	memset(&mfHdr, 0, sizeof(mfHdr) );
	hdcMeta = CreateMetaFile(metaFile);
	SetTextAlign(hdcMeta, TA_LEFT | TA_BOTTOM);

	check_taborders(iControls);
	sm = gSetScalingMode(Application, SM_PIXELS);
	m = gSize(iControls);
	for (n=0 ; n++ < m ; ) {
		lctl = NULL;
		for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
			current = gGetTabOrder(ctl);
			if (current > done  &&  (!lctl  ||  current < low)) {
				lctl = ctl;
				low = current;
			}
		}
		if (lctl) {
			if (low >= 0) {
				if( ClassOf(lctl) == LineControl || ClassOf(lctl) == RectControl 
				        || (ClassOf(lctl) == StaticTextControl && !gGetSavingStatus(lctl)) 
						|| (ClassOf(lctl) == ImageControl && !gGetSavingStatus(lctl)) ) {
					gGetWindowRect(lctl, &rect);
					mfHdr.bbox.right = max(mfHdr.bbox.right, rect.right);
					mfHdr.bbox.bottom = max(mfHdr.bbox.bottom, rect.bottom);
					gSaveControlToMetaFile(lctl, hdcMeta, hdcView);
				}
				else
					gSaveControl(lctl, fp);
			}
			done = low;
		}

	}
	gSetScalingMode(Application, sm);
	fclose(fp);
	DeleteMetaFile(CloseMetaFile(hdcMeta));

	SetMapMode(hdcView, nMapModeOld);
	SetWindowOrgEx(hdcView, (int)ptWindowOrgOld.x, (int)ptWindowOrgOld.y, NULL);
	SetWindowExtEx(hdcView, (int)szWindowExtOld.cx, (int)szWindowExtOld.cy, NULL);
	SetViewportOrgEx(hdcView, (int)ptViewportOrgOld.x, (int)ptViewportOrgOld.y, NULL);
	SetViewportExtEx(hdcView, (int)szViewportExtOld.cx, (int)szViewportExtOld.cy, NULL);
	// RestoreDC(hdcView, nSaveDC);

	ReleaseDC(iHwnd, hdcView);

	mfHdr.key         = 0x9AC6CDD7L;
	mfHdr.hmf         = 0;
	mfHdr.bbox.left   = 0;
	mfHdr.bbox.top    = 0;
	mfHdr.bbox.right  = mfHdr.bbox.right * 300.0/head.logPixelsx + 0.5;
	mfHdr.bbox.bottom = mfHdr.bbox.bottom * 300.0/head.logPixelsy + 0.5;
	mfHdr.inch        = 300;
	// mfHdr.inch     = head.logPixelsx;
	mfHdr.reserved    = 0;
	mfHdr.checksum    = 0;

	fp = fopen(plcMetaFile, "wb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(plcMetaFile, "wb");
	}
	if (!fp)
		return NULL;

	if (1 != fwrite(&mfHdr, sizeof(mfHdr), 1, fp)) {
		fclose(fp);
		return NULL;
	}

	nMetaFile = open(metaFile, _O_RDONLY | _O_BINARY);
	if (nMetaFile == -1) {
		gMoreHandles(LowFile);
		nMetaFile = open(metaFile, _O_RDONLY | _O_BINARY);
	}
	if (nMetaFile == -1) {
		fclose(fp);
		return 0;
	}

	lseek(nMetaFile, 0L, SEEK_SET);
	nFileSize = lseek(nMetaFile, 0L, SEEK_END);    // get the metafile's size while we're here
	buf = (char *) malloc((unsigned) nFileSize);
	
	if (!buf) {
		close(nMetaFile);
		fclose(fp);
		return 0;
	}

	lseek(nMetaFile, 0L, SEEK_SET);
	read(nMetaFile, buf, nFileSize);
	fwrite(buf, nFileSize, 1, fp);
	free(buf);

	close(nMetaFile);
	fclose(fp);
	remove(metaFile);

	return self;
}


//////////////////////////////////////////////////////////////////////////////////////
// gPrintScreen: This function tries to support WYSIWYG (what you see is what you get)
//               it will print what is on screen including meta file and text of the
//               controls except PushButton RadioButton, CheckBox, ListBox, and ComboBox
// printerObj:   the printer object, if printerObj is NULL, the printerObj is self 
//               generated and destroyed in this subroutine
// 
//////////////////////////////////////////////////////////////////////////////////////
imeth	gPrintScreen(object printerObj)
{
	object  linkObject=NULL;
	HDC     hdcPrinter;
	char    *MFName=NULL;
	BOOL    bSelfBuiltPrinterObj = FALSE;

	if (iClientWind)
		return gPrintScreen(iClientWind, printerObj);

	if(iMFName)
		MFName = gStringValue(iMFName);

	linkObject = createOrderedControlsForPrinting(iv);
	if( !linkObject && (!MFName || !(*MFName)) ) 
		return NULL;

	if(!printerObj) {
		printerObj = gNewPrinter(Printer, self, 0);     // letter paper
		// printerObj = gNewPrinter(Printer, self, 1);  // legal paper
		bSelfBuiltPrinterObj = TRUE;
	}

	if(!printerObj)
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) ) {
		if(bSelfBuiltPrinterObj)
			printerObj = gDispose(printerObj);
		return NULL;
	}
	
	iPage = 0;
	if( gCLDStartPage(printerObj) ) {  
		object  seq, ctl;
		HDC     hdcView;
		int     printerLogPixelsX, printerLogPixelsY;
		int     viewLogPixelsX, viewLogPixelsY;
		double  dScaleX, dScaleY;

		UINT    oldTextAlign;
		int     oldMapMode;

		printerLogPixelsX  = GetDeviceCaps(hdcPrinter, LOGPIXELSX);
		printerLogPixelsY  = GetDeviceCaps(hdcPrinter, LOGPIXELSY);

		hdcView = GetDC(iHwnd);  // if hwndP=NULL, GetDC retrieves the device context for the entire screen
		viewLogPixelsX = GetDeviceCaps(hdcView, LOGPIXELSX);
		viewLogPixelsY = GetDeviceCaps(hdcView, LOGPIXELSY);
		ReleaseDC(iHwnd, hdcView);

		if( (viewLogPixelsX<=0) || (viewLogPixelsY<=0) ) {  // early exit of printing
			gCLDEndPage(printerObj);

			if(bSelfBuiltPrinterObj)
				printerObj = gDispose(printerObj);
			return NULL;
		}

		gCLDPrintMetaFile(printerObj, MFName);

		if(iScaleFlg) {
			dScaleX = printerLogPixelsX*iFontScale/(viewLogPixelsX*iPrinterScale);
			dScaleY = printerLogPixelsY*iFontScale/(viewLogPixelsY*iPrinterScale);
		}
		else {
			dScaleX = printerLogPixelsX/(double)viewLogPixelsX;
			dScaleY = printerLogPixelsY/(double)viewLogPixelsY;
		}

		// use correct text alignment and mapping mode
		oldTextAlign = SetTextAlign(hdcPrinter, TA_LEFT | TA_TOP);
		oldMapMode = SetMapMode(hdcPrinter, MM_TEXT);

		// print the text of the controls
		if(linkObject)
			for (seq=gSequence(linkObject) ; ctl = gNext(seq) ; ) {
				if( ClassOf(ctl) == LineControl       || ClassOf(ctl) == RectControl || 
				    ClassOf(ctl) == StaticTextControl || ClassOf(ctl) == TextControl || 
				    ClassOf(ctl) == NumericControl    || ClassOf(ctl) == DateControl || 
				    ClassOf(ctl) == TimeControl	      || ClassOf(ctl) == ImageControl) 
					// skip PushButton, RadioButton, CheckBox, ListBox, and ComboBox
					gPrintCtlScreen(ctl, printerObj, dScaleX, dScaleY, iHOffset, iVOffset);
			}

		// get the old text alignment and mapping mode back
		SetTextAlign(hdcPrinter, oldTextAlign);
		SetMapMode(hdcPrinter, oldMapMode);

		gCLDEndPage(printerObj);
	}

	if(bSelfBuiltPrinterObj)
		printerObj = gDispose(printerObj);  // dispose self built printer object

	if(linkObject)
		linkObject = gDispose(linkObject);
	return self;
}


imeth	int gGetPage()
{
	return iPage;
}


imeth	int gTurnPage()
{
	return (++iPage);
}


/***************************
/////////////////////////////////////////////////////////////////////////////////////////////
// createOrderedControlsForPrinting: return a newly created linkObject, which includes the
//                                   same set of controls as iControls, but is ordered 
//                                   by rect.bottom. The linkObject must be disposed by the 
//                                   calling function. This is a bubble sorting routine.
/////////////////////////////////////////////////////////////////////////////////////////////
static object createOrderedControlsForPrinting(ivType *iv)
{ 
	object linkObject=NULL, linkSeq=NULL, link=NULL, ctl=NULL;
	object *controlArray=NULL;
	RECT   rect1, rect2;
	int    i, j, nControls;
	
	if (!iControls)
		return NULL;

	// get the number of controls
	nControls = 0;
	for (linkSeq=gSequenceLinks(iControls) ; link = gNext(linkSeq) ; ) 
		nControls++;

	controlArray = (object *)malloc(sizeof(object)*nControls);
	nControls = 0;
	for (linkSeq=gSequenceLinks(iControls) ; link = gNext(linkSeq) ; ) {
		controlArray[nControls++] = gValue(link);
	}

	// sort the array of objects according to the rect.bototm of the object
	for(i=0; i<nControls; i++)
	{
		gGetWindowRect(controlArray[i], &rect1);
		for(j=i+1; j<nControls; j++) {
			gGetWindowRect(controlArray[j], &rect2);
			if(rect2.bottom < rect1.bottom) {
				ctl = controlArray[i];
				controlArray[i] = controlArray[j];
				controlArray[j] = ctl;
			}
		}
	}

	// create a sorted line object
	linkObject = gNew(LinkObject);
	for(i=0; i<nControls; i++) {
		gGetWindowRect(controlArray[i], &rect1);
		gAddLast(linkObject, controlArray[i]);
	}

	free(controlArray);

	return linkObject;
}
***************************/



static int addCLDObjectLink(CLDObjectLink rootLink, CLDObjectLink nodeLink)
{
	RECT rectRoot, rectNode;
	if(!rootLink || !nodeLink)
		return -1;

	gGetWindowRect(rootLink->obj, &rectRoot);
	gGetWindowRect(nodeLink->obj, &rectNode);

	if(rectNode.bottom <= rectRoot.bottom) {
		if(rootLink->left)
			addCLDObjectLink(rootLink->left, nodeLink);
		else 
			rootLink->left = nodeLink;
	}
	else {
		if(rootLink->right)
			addCLDObjectLink(rootLink->right, nodeLink);
		else 
			rootLink->right = nodeLink;
	}
	return 0;
}


static int fillLinkObject(object linkObject, CLDObjectLink rootLink)
{
	if(!linkObject || !rootLink)
		return -1;

	fillLinkObject(linkObject, rootLink->left);
	gAddLast(linkObject, rootLink->obj);
	fillLinkObject(linkObject, rootLink->right);
	return 0;
}


static int destroyCLDObjectLink(CLDObjectLink rootLink)
{
	if(!rootLink)
		return -1;

	destroyCLDObjectLink(rootLink->left);
	destroyCLDObjectLink(rootLink->right);
	
	free(rootLink);
	return 0;
}


/////////////////////////////////////////////////////////////////////////////////////////////
// createOrderedControlsForPrinting: return a newly created linkObject, which includes the
//                                   same set of controls as iControls, but is ordered 
//                                   by rect.bottom. The returned linkObject must be disposed 
//                                   by the calling function.
/////////////////////////////////////////////////////////////////////////////////////////////
static object createOrderedControlsForPrinting(ivType *iv)
{ 
	object linkObject=NULL, linkSeq=NULL, link=NULL;
	CLDObjectLink rootLink, nodeLink;

	if (!iControls)
		return NULL;

	// construct rootNode
	linkSeq = gSequenceLinks(iControls); 
	link = gNext(linkSeq);
	if(!link)
		return NULL;

	rootLink = (CLDObjectLink)malloc(sizeof(struct CLDObjectNode));
	rootLink->obj   = gValue(link);
	rootLink->left  = NULL;
	rootLink->right = NULL;

	// construct CLDObjectLink
	for ( ; link = gNext(linkSeq) ; ) {
		nodeLink = (CLDObjectLink)malloc(sizeof(struct CLDObjectNode));
		if(!nodeLink) {
			vError(Application, "memory alocation failure in 'malloc(sizeof(CLDObjectNode))' of 'window.d'");
		}

		nodeLink->obj   = gValue(link);
		nodeLink->left  = NULL;
		nodeLink->right = NULL;

		addCLDObjectLink(rootLink, nodeLink);
	}

	// create a sorted link object
	linkObject = gNew(LinkObject);
	fillLinkObject(linkObject, rootLink);

	// destroy CLDObjectLink
	destroyCLDObjectLink(rootLink);

	return linkObject;
}


/////////////////////////////////////////////////////////////
//  szClipboardFileName must point to a buffer of _MAX_PATH
//
static int getClipboardFileName(char * szClipboardFileName)
{
	int     nTmp;
	char    szLongPathName[_MAX_PATH];
	char    *strTmp1, *strTmp2;

	if(!szClipboardFileName)
		return -1;

	GetModuleFileName(gInstance(Application), szLongPathName, _MAX_PATH);

	if(!szLongPathName || !(*szLongPathName)) {
		*szClipboardFileName = '\0';
		return -1;
	}

	nTmp = 0;
	strTmp1 = szLongPathName;
	while (strTmp1 && (strTmp2=strstr(strTmp1,"\\")) ) {
		nTmp = nTmp + strTmp2 - strTmp1 + 1;
		strTmp1 = strTmp2 + 1;
	}

	if(nTmp>0)
		strncpy(szClipboardFileName, szLongPathName, nTmp);

	szClipboardFileName[nTmp] = '\0';
	strcat(szClipboardFileName, "clipboard.bin");

	return 0;
}

imeth	gCLDCopyControls()
{
	short   version = CLD_VERSION;
	object  seq, objDragWindow, ctl;
	object  objDWs;  // the set of the instances of the DragWindow
	int     sm;
	FILE    *fp;
	char    clipboard[_MAX_PATH];

	if (iClientWind)
		return gCLDCopyControls(iClientWind);

	objDWs = gGetDWs(DragWindow);

	if (!objDWs)
		return NULL;

	if(getClipboardFileName(clipboard)==-1 || !(*clipboard))
		return NULL;

	fp = fopen(clipboard, "wb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(clipboard, "wb");
	}
	if (!fp)
		return NULL;

	if (1 != fwrite(CLIPBOARD_CODE, sizeof CLIPBOARD_CODE, 1, fp)  ||
		1 != fwrite(&version, sizeof version, 1, fp)) {
		fclose(fp);
		return NULL;
	}

	sm = gSetScalingMode(Application, SM_PIXELS);
	for(seq=gSequence(objDWs); objDragWindow=gNext(seq);) { 
		if( ctl=gGetSelectedCtl(objDragWindow) )
			gSaveControl(ctl, fp);
	}
	gSetScalingMode(Application, sm);

	fclose(fp);
	return self;
}


imeth	gCLDPasteControls()
{
	char    code[sizeof CLIPBOARD_CODE];
	FILE    *fp;
	short   version, type;
	int	    sm;
	object  ctl;

	char    clipboard[_MAX_PATH];

	short   nXshift, nYshift;
	RECT    rectTmp1, rectTmp2, rectTmp;

	if (iClientWind)
		return gCLDPasteControls(iClientWind);

	gGetCRect(DragWindow,  &rectTmp);
	gRmAllOfDWs(DragWindow);  // if the dragging windows exist, cancel the dragging window

	nXshift = X_SHIFT_FOR_PASTE;  // defined in ctlsave.h
	nYshift = Y_SHIFT_FOR_PASTE;  // defined in ctlsave.h

	if(getClipboardFileName(clipboard)==-1 || !(*clipboard))
		return NULL;

	fp = fopen(clipboard, "rb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(clipboard, "rb");
	}
	if (!fp)
		return NULL;

	if (1 != fread(code, sizeof code, 1, fp)  ||
	    1 != fread(&version, sizeof version, 1, fp) ||
	    strcmp(code, CLIPBOARD_CODE) || version!=CLD_VERSION) {
			fclose(fp);
			return NULL;
	}

	sm = gSetScalingMode(Application, SM_PIXELS);
	while (1 == fread(&type, sizeof type, 1, fp)) {
		switch (type) {
		case CTLTYPE_STATIC:
			ctl = gCLDPasteControl(StaticTextControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_TEXT:
			ctl = gCLDPasteControl(TextControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_NUMERIC:
			ctl = gCLDPasteControl(NumericControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_DATE:
			ctl = gCLDPasteControl(DateControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_TIME:
			ctl = gCLDPasteControl(TimeControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_PUSHBUTTON:
			ctl = gCLDPasteControl(PushButton, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_RADIOBUTTON:
			ctl = gCLDPasteControl(RadioButton, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_CHECKBOX:
			ctl = gCLDPasteControl(CheckBox, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_LISTBOX:
			ctl = gCLDPasteControl(ListBox, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_COMBOBOX:
			ctl = gCLDPasteControl(ComboBox, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_RECT:
			ctl = gCLDPasteControl(RectControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_LINE:
			ctl = gCLDPasteControl(LineControl, fp, self, nXshift, nYshift);
			break;
		case CTLTYPE_IMAGE:
			ctl = gCLDPasteControl(ImageControl, fp, self, nXshift, nYshift);
			break;
		default:
			fclose(fp);
			vError(self, "Bad resource file %s", clipboard);
			break;
		}
		gNewDragWindow(DragWindow, ctl, FALSE);  // put the newly pasted controls to the DragWindow list

		CopyRect(&rectTmp1, &rectTmp);
		gGetWindowRect(ctl, &rectTmp2);
		UnionRect(&rectTmp, &rectTmp1, &rectTmp2);
	}
	gSetScalingMode(Application, sm);
	fclose(fp);
	pUpdateMax(self);
	update_vert(iv);
	update_horz(iv);

	InflateRect(&rectTmp, CHANDLESIZE, CHANDLESIZE);
	if (iHwnd)
		InvalidateRect(iHwnd, &rectTmp, TRUE);

	gCLDCopyControls(self);

	return self;
}


imeth gAddToDeletedCtls(object ctl)
{
	if (iClientWind)
		return gAddToDeletedCtls(iClientWind, ctl);

	if(!ctl)
		return NULL;

	if(!iDeletedCtls)
		iDeletedCtls = gNew(LinkObject);

	if(!iDeletedCtls)
		return NULL;

	gAddLast(iDeletedCtls, ctl);
	return self;
}


imeth gUndoLastDelete()
{
	object linkObj, ctl;

	if (iClientWind)
		return gUndoLastDelete(iClientWind);

	if(!iDeletedCtls)
		return NULL;

	linkObj = gRemoveLast(iDeletedCtls);

	if(!linkObj)
		return NULL;
	
	ctl = gValue(linkObj);
	gDispose(linkObj);

	if(ctl) {
		object parentObj;
		parentObj = gGetParent(ctl);
		gSetModifyChildren(parentObj, 0);
		gDisplay(ctl);
		gSetModifyChildren(parentObj, 1);
	
		if ( (ClassOf(ctl)!=StaticTextControl) && (ClassOf(ctl)!=RectControl) && (ClassOf(ctl)!=LineControl) ) { 
			iTabOrder += 10;
			gSetTabOrder(ctl, iTabOrder);
		}
		else
			gSetTabOrder(ctl, 0);
	}

	// if there is no deleted controls in the link object 'iDeletedCtls',  dispose the link object
	if(!gLast(iDeletedCtls))
		iDeletedCtls = gDispose(iDeletedCtls);

	return self;
}


imeth gGetDeletedCtls()
{
	return iDeletedCtls;
}

imeth	gShowScrollBar(int option, int show)
{
	ShowScrollBar(iHwnd, option, show);

	return self;
}

imeth	gUpdateScrollBar()
{
	if (iClientWind)
		return gUpdateScrollBar(iClientWind);

	pUpdateMax(self);
	update_vert(iv);
	update_horz(iv);
	return self;
}

imeth	int	gAddTimer(int interval, int (*fun)())
{
	object	lobj, obj;
	
	if (!iTimerFunctions)
		iTimerFunctions = gNew(IntegerDictionary);

	iTimerCounter++;
	
	if (IsObj((object) fun)  &&  
	    (ClassOf(fun) == String || ClassOf(fun) == JavaCallbackClassSurrogate))
		obj = (object) fun;
	else
		obj = gNewWithPtr(Pointer, (void *) fun);

	lobj = gNewWithIntObj(IntegerAssociation, interval, obj);
	gAddInt(iTimerFunctions, iTimerCounter, lobj);

	if (iHwnd)
		SetTimer(iHwnd, iTimerCounter, interval, NULL);
	
	return iTimerCounter;
}

imeth	int	gKillTimer(int timerID)
{
	if (!iTimerFunctions  ||  !gDeepDisposeInt(iTimerFunctions, timerID))
		return 0;
	return KillTimer(iHwnd, timerID);
}

imeth	char	*gFirstCLDSection()
{
	return (iCtlSection && gSize(iCtlSection)) ? gStringKey(gFirst(iCtlSection)) : "";
}

imeth	object	gDisableAll()
{
	object	seq, ctl;

	for (seq = gSequence(iControls) ; ctl = gNext(seq) ; )
		gDisable(ctl);
	return self;
}

imeth	object	gEnableAll()
{
	object	seq, ctl;

	for (seq = gSequence(iControls) ; ctl = gNext(seq) ; )
		gEnable(ctl);
	return self;
}

imeth	gRedrawMenu()
{
	if (iHwnd && iMenu)
		DrawMenuBar(iHwnd);
	return self;
}	



#define	BUFLEN	128

imeth gReadXML(node)
{
	CLD_HEADER_t	head;
	int	sm;

	object	firstCtl = NULL;

	HDC   hDc;
	int   logPixelsx;
	int	fileok = 1;
	
	object list;
	object seq;
	object curnode;

	if (!node)
		return self;
	
	curnode=gFindChildNode(gChild(node),"window");

	
	memset(&head, 0, sizeof head);
	
	head.version = gIntValue(gChild(gFindChildNode(curnode,"version")));
	head.cxInPixel = gIntValue(gChild(gFindChildNode(curnode,"cxInPixel")));
	head.cyInPixel = gIntValue(gChild(gFindChildNode(curnode,"cyInPixel")));
	head.logPixelsx = gIntValue(gChild(gFindChildNode(curnode,"logPixelsx")));
	head.logPixelsy = gIntValue(gChild(gFindChildNode(curnode,"logPixelsy")));
	head.metaFileScale = gDoubleValue(gChild(gFindChildNode(curnode,"metaFileScale")));
	head.metaFileNameLen = 0;


	iMFScale = head.metaFileScale;
	iControlScale = 1.0;
	iFontScale = 1.0;

	iPrinterScale = head.cxInPixel/800.0;

	// for different screen width and height and font size: do the scaling if the scaling flag is set
	if (gGetScaleFlg(self)) {
		if(head.cxInPixel>0) {
			iMFScale = iMFScale*GetSystemMetrics(SM_CXSCREEN) / (double) head.cxInPixel;
			iControlScale = iControlScale*GetSystemMetrics(SM_CXSCREEN) / (double) head.cxInPixel;
		}

		hDc = GetDC(iHwnd);
		// If this iHwnd is NULL, GetDC retrieves the device context for the entire screen. 
		logPixelsx = GetDeviceCaps(hDc, LOGPIXELSX);
		ReleaseDC(iHwnd, hDc);
		
		if(logPixelsx>0) {
				iFontScale = (double)logPixelsx/head.logPixelsx;
				iMFScale = iMFScale/iFontScale;
		}
	}

	if (iMFName)
		iMFName = gDispose(iMFName);


	iHmf = (HANDLE) 0;

	sm = gSetScalingMode(Application, SM_PIXELS);
	
	list=gXPath(node,"//controls/*");
	for (seq = gSequence(list) ; curnode = gNext(seq) ; ) {

		char *name=gGetName(curnode);
		object ctl;
		if (!strcmp("static", name))
			ctl=gLoadControlFromXML(StaticTextControl, curnode, self);
		else if (!strcmp("text", name))
			ctl=gLoadControlFromXML(TextControl, curnode, self);
		else if (!strcmp("numeric", name))
			ctl=gLoadControlFromXML(NumericControl, curnode, self);
		else if (!strcmp("date", name))
			ctl=gLoadControlFromXML(DateControl, curnode, self);
		else if (!strcmp("time", name))
			ctl=gLoadControlFromXML(TimeControl, curnode, self);
		else if (!strcmp("checkbox", name))
			ctl=gLoadControlFromXML(CheckBox, curnode, self);
		else if (!strcmp("combobox", name))
			ctl=gLoadControlFromXML(ComboBox, curnode, self);
		else if (!strcmp("rectangle", name))
			ctl=gLoadControlFromXML(RectControl, curnode, self);
		else if (!strcmp("line", name))
			ctl=gLoadControlFromXML(LineControl, curnode, self);
		else if (!strcmp("listbox", name))
			ctl=gLoadControlFromXML(ListBox, curnode, self);
		else if (!strcmp("pushbutton", name))
			ctl=gLoadControlFromXML(PushButton, curnode, self);
		else if (!strcmp("radiobutton", name))
			ctl=gLoadControlFromXML(RadioButton, curnode, self);
		else if (!strcmp("image", name))
			ctl=gLoadControlFromXML(ImageControl, curnode, self);
		else 
			ctl=gLoadControlFromXML(GenericControl, curnode, self);
				
		if (!firstCtl)
			firstCtl=ctl;

	}
	
	
	gSetScalingMode(Application, sm);

	pUpdateMax(self);
	update_vert(iv);
	update_horz(iv);
	if (iHwnd)
		InvalidateRect(iHwnd, NULL, TRUE);

	return firstCtl;

}

imeth gLoadXMLFile(char *fname)
{
	object node=gParseFile(XMLNode,fname,NULL,NULL,NULL);

	gReadXML(self,node);

	return self;
}

imeth gWriteXML(FILE *fp)
{
	HDC	hdcView = GetDC(iHwnd);  // If iHwnd is NULL, GetDC retrieves the device context for the entire screen. 

	fprintf(fp,"\t<window>\n");
	fprintf(fp,"\t\t<version>%d</version>\n",CLD_VERSION);
	fprintf(fp,"\t\t<cxInPixel>%d</cxInPixel>\n",GetSystemMetrics(SM_CXSCREEN));
	fprintf(fp,"\t\t<cyInPixel>%d</cyInPixel>\n",GetSystemMetrics(SM_CYSCREEN));
	fprintf(fp,"\t\t<logPixelsx>%d</logPixelsx>\n",GetDeviceCaps(hdcView, LOGPIXELSX));
	fprintf(fp,"\t\t<logPixelsy>%d</logPixelsy>\n",GetDeviceCaps(hdcView, LOGPIXELSY));
	fprintf(fp,"\t\t<metaFileScale>%f</metaFileScale>\n",iMFScale);
	fprintf(fp,"\t</window>\n");

	return self;
}

imeth int gLastLoadType()
{
	return iLastLoadType;
}

// Yanghui


imeth	gSetCurrentLanguage(int language)
{
	object	ss, ctl;
	if (iControls)
		for (ss = gSequence(iControls) ; ctl = gNext(ss) ; )  
			gSetCurrentLanguage(ctl,language);
		
	if (iClientWind)
		gSetCurrentLanguage(iClientWind,language);
	return self;
}

imeth	ifun gControlSecurityFunction()
{
	return cAccessMode;
}

