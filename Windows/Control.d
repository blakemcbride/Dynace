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




//#include "logfile.h"
#include "hdlcache.h"
#include "ctlsave.h"


defclass  Control : Window {
	int	iDisabled;	/*  1=input disabled, 2=enabled, 0=default */
	int	iHidden;	/*  1=hidden, 2=show, 0=default            */
	iName;			/*  control name                           */
	int	iAutoAttach;	/*  for database field associations	   */
                         	/*  1=ok to auto attach,  0=don't auto attach  */
	iVector;		/*  ControlVector for positioning on a
				    window.				   */
	int	iTabOrder;
	ifun	iMouseFunction[6];

	iFont;
	
	iXPathBinding;
	
	int	iCaretHeight;

	iDragWindow;    // the instance of DragWindow

	iSetFunctions;	// FunctionList object

	iDataChangeFunctions;	// FunctionList object

	object	iFileResource;
	
	int iCurrentLanguage;  //	current language 0=English, 1=Spanish
	iLanguageText[MAX_LANGUAGES];		// text in languages

class:
	ifun	cMouseFunction[6];
};

#include <string.h>


static int matchLogfontForTheLineOfText(HDC hdcView, double dScaleX, HDC hdcPrinter, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign, int nAveCharWidth);
static int matchLogfontForOneToken(HDC hdcPrinter, LOGFONT *pLogfont, const RECT *pRect, const char *token, double dScaleX, int nHalfWhiteSpace);

static int matchLogfontForTheLineOfTextOfMetaFile(HDC hdcView, double dScaleX, HDC hdcMetafile, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign);

private	imeth	long	process_wm_lbuttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam);

private	imeth	long	process_wm_rbuttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam);

private	imeth	long	process_wm_mousemove(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam);  // Yanghui

private	imeth	long	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam);

private	imeth	long	process_wm_getdlgcode(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam);

/*  This method is always overwritten by subclasses of Control and have the
    following format:  vNew(UINT ctlID)
*/

cvmeth	vNew(char *name)
{
	object	obj = gNewDialogControl(self);
	ivType	*iv = ivPtr(obj);

	if (name)
		iName = gNewWithStr(String, name);
	iAutoAttach = 1;

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDOWN, process_wm_lbuttondown);
	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);
	gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);  // Yanghui
	memcpy(iMouseFunction, cMouseFunction, sizeof cMouseFunction);

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);

	gAddHandlerBefore(obj, (unsigned) WM_GETDLGCODE, process_wm_getdlgcode);
	gDefaultProcessingMode(obj, (unsigned) WM_GETDLGCODE, 0);
	
	return obj;
}

cmeth	gNewCont(char *name, char *class, parent)
{
	object	obj = gNewBuiltIn(self, class, parent);
	ivType	*iv = ivPtr(obj);

	if (name)
		iName = gNewWithStr(String, name);
	iAutoAttach = 1;
	
	iFont = gGetFont(Application);
	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDOWN, process_wm_lbuttondown);
	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);
	gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);  // Yanghui

	memcpy(iMouseFunction, cMouseFunction, sizeof cMouseFunction);

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);

	gAddHandlerBefore(obj, (unsigned) WM_GETDLGCODE, process_wm_getdlgcode);
	gDefaultProcessingMode(obj, (unsigned) WM_GETDLGCODE, 0);

	return obj;
}


private	imeth	long	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	long rval = 0;
	object	parent;

	if (parent=gGetParent(self))
		if (GetKeyState(VK_CONTROL) & 0x80)
			if (wParam == VK_TAB)
				rval = SendMessage(gHandle(parent), mMsg, wParam, lParam);

	return rval;
}

private	imeth	long	process_wm_getdlgcode(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	long rval = DLGC_WANTARROWS | DLGC_WANTCHARS | DLGC_RADIOBUTTON;

	MSG* pMsg = NULL;
	if (lParam) {
		pMsg = (MSG *)lParam;
		if (pMsg->wParam == VK_TAB &&
			GetKeyState(VK_CONTROL) & 0x80)
			rval |= DLGC_WANTTAB;
	}

	if (!strcmp(gName(ClassOf(self)), "SpreadsheetControl"))
	//if (gIsKindOf(self, SpreadsheetControl))
		rval |= DLGC_WANTTAB;

	return rval;
}

// Yanghui:
imeth gGetDragWindow()
{
	return iDragWindow;
}


/////////////////////////////////////////////////////
// The old DragWindow object is returned instead of
// being disposed. The reason is that this 
// function is called inside the gDispose(iDragWindow).
// If the old DragWindow object is disposed here, an
// infinite cycle may occur.
/////////////////////////////////////////////////////
imeth gSetDragWindow(object dragWindow)
{
	object oldDragWindow = iDragWindow;
	iDragWindow = dragWindow;
	return oldDragWindow;
}


cmeth	gCLDNewCont(char *name, char *class)
{
	object	obj = gCLDNewBuiltIn(self, class);
	ivType	*iv = ivPtr(obj);

	if (name)
		iName = gNewWithStr(String, name);

	iAutoAttach = 0;
	iFont = gGetFont(Application);

	return obj;
}
// Yanghui


imeth	object	gDispose, gDeepDispose ()
{
	int loop;
	
	for (loop=0;loop<MAX_LANGUAGES;loop++)
		if (iLanguageText[loop])
			gDispose(iLanguageText[loop]);
			
	if(iDragWindow)
		iDragWindow = gDispose(iDragWindow);

	if (iName)
		gDispose(iName);
	if (iFont)
		gDispose(iFont);

	if (iSetFunctions)
		gDeepDispose(iSetFunctions);
	if (iDataChangeFunctions)
		gDeepDispose(iDataChangeFunctions);

	if (iFileResource)
		gDispose(iFileResource);
		
	if (iXPathBinding)
		gDispose(iXPathBinding);
	
	return gDispose(super);
}

imeth	int	gCheckValue()
{
	return 0;	/*  OK  */
}

imeth	gForceEnable : forceEnable ()
{
	if (ClassOf(self) != StaticTextControl  &&
	    ClassOf(self) != RadioButton  &&
	    ClassOf(self) != CheckBox &&
	    ClassOf(self) != RectControl  &&  // Yanghui
	    ClassOf(self) != LineControl)     // Yanghui
		gBackBrush(self, vNew(SystemBrush, COLOR_WINDOW));

	if (!gDesignMode(self))
		gEnable(super);
	iDisabled = 2;
	return self;
}

imeth	gEnable()
{
	ifun	fun = gControlSecurityFunction(gDialog(self));
	
	if (fun && fun(self))
		iDisabled = 1;
	else
		forceEnable(self);
	return self;
}

imeth	gDisable()
{
	if (ClassOf(self) != StaticTextControl  &&
	    ClassOf(self) != RadioButton  &&
	    ClassOf(self) != CheckBox &&
	    ClassOf(self) != RectControl &&    // Yanghui
	    ClassOf(self) != LineControl)      // Yanghui
		gBackBrush(self, vNew(SystemBrush, COLOR_BTNFACE));
	if (!gDesignMode(self))
		gDisable(super);
	iDisabled = 1;
	return self;
}

imeth	gDisplay()
{
	HWND	hctl = gHandle(self);
	if (hctl  &&  !gDesignMode(self)) {
		ShowWindow(hctl, SW_SHOW);
		gInvalidate3dCtl(self);
	}
	iHidden = 2;
	return self;
}

imeth	gHide()
{
	HWND	hctl = gHandle(self);
	if (hctl  &&  !gDesignMode(self)) {
		ShowWindow(hctl, SW_HIDE);
		gInvalidate3dCtl(self);
	}
	iHidden = 1;
	return self;
}

imeth	int	gDesignMode()
{
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	HWND	hctl = gHandle(self);
	if (gDesignMode(self))
		return self;
	if (iHidden == 1)
		ShowWindow(hctl, SW_HIDE);
	else if (iHidden == 2)
		ShowWindow(hctl, SW_SHOW);
	if (iDisabled == 1)
		EnableWindow(hctl, FALSE);
	else if (iDisabled == 2)
		EnableWindow(hctl, TRUE);
	return self;
}

imeth	gSetFocus()
{
	HWND	hctl = gHandle(self);
	object	dlg = gDialog(self);

	if (hctl)
		if (dlg  &&  gIsKindOf(dlg, Window))
			SetFocus(hctl);
		else
			PostMessage(gHandle(gDialog(self)), WM_NEXTDLGCTL, (WPARAM) hctl, (LPARAM) TRUE);
	return self;
}

imeth	char	*gName()
{
	return iName ? gStringValue(iName) : "";
}

imeth	gSetName(char *name)	/*  name may also be an object  */
{
	object	dlg = gDialog(self);
	if (dlg  &&  gIsKindOf(dlg, Window))
		gChangeControlName(dlg, self, gStringValue(iName), name);
	if (iName)
		gDispose(iName);
	if (name)
		if (!IsObj((object)name))
			iName = gNewWithStr(String, name);
		else
			iName = (object) name;
	else
		iName = NULL;
	return self;
}

imeth	gCreateCaret()
{
	HANDLE	hctl = gHandle(self);
	if (!iCaretHeight) {
		TEXTMETRIC	tm;
		if (!iFont)
			iFont = gGetFont(Application);
		gGetTM(iFont, &tm);
		iCaretHeight = tm.tmAscent;
	}
	CreateCaret(hctl, (HBITMAP) NULL, 0, iCaretHeight);
	ShowCaret(hctl);
	return self;
}

imeth	gSetSI(si)
{
	return self;
}

imeth	gGetSI()
{
	return NULL;
}

imeth	gUpdate()
{
	return self;
}

imeth	gUseDefault()
{
	return self;
}

imeth	unsigned     gGetResourceID()
{
	return gGetCtlID(self);
}

imeth	int	gAutoAttach(int mode)
{
	int	prev = iAutoAttach;
	if (mode == 0  ||  mode == 1)
		iAutoAttach = mode;
	return prev;
}

imeth	int	gHiddenStatus()
{
	return iHidden;
}

imeth	int	gDisableStatus()
{
	return iDisabled;
}

imeth	gSetVector(v)
{
	iVector = v;

	return self;
}

imeth	gVector()
{
	return iVector;
}

imeth	gInvalidate3dCtl()
{
	if (gIsKindOf(gDialog(self), Window)  &&
	    (gIsKindOf(self, TextControl) ||
	     gIsKindOf(self, NumericControl) ||
	     gIsKindOf(self, DateControl) ||
	     gIsKindOf(self, TimeControl) ||
	     gIsKindOf(self, ListBox))) {
		RECT	r;
		int	pm = gSetScalingMode(Application, SM_PIXELS);

		gGetRect(self, &r);
	
		r.bottom += 2;
		r.right += 2;

		r.top -= 2;
		r.left -= 2;

		InvalidateRect(gHandle(gDialog(self)), &r, TRUE);
		
		gSetScalingMode(Application, pm);
	}

	return self;
}

imeth	int	gSetTabOrder(int t)
{
	int	old = iTabOrder;
	iTabOrder = t;
	return old;
}

imeth	int	gGetTabOrder()
{
	return iTabOrder;
}

static	int	get_index(WPARAM p)
{
	if (p & MK_RBUTTON)
		if (p & MK_SHIFT)
			return 2;
		else if (p & MK_CONTROL)
			return 4;
		else
			return 0;
	else if (p & MK_LBUTTON)
		if (p & MK_SHIFT)
			return 3;
		else if (p & MK_CONTROL)
			return 5;
		else
			return 1;
	else
		return -1;
}

cmeth	ofun	gSetMouseFunction(unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;

	if (i >= 0) {
		org = cMouseFunction[i];
		cMouseFunction[i] = fun;
	}
	return (ofun) org;
}

imeth	ofun	gSetMouseFunction(unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;

	if (i >= 0) {
		org = iMouseFunction[i];
		iMouseFunction[i] = fun;
		if (IsObj((object)org))
			gDispose((object)org);
	}
	return (ofun) org;
}


private	imeth	long	process_wm_lbuttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	int    i;

	// Yanghui:
	// if the parent window of the control is not a Dialog class
	// and it is in modify mode
	if (gDesignMode(self)) {
		if( (wParam & MK_CONTROL) || (wParam & MK_SHIFT) ) {	
			gNewDragWindow(DragWindow, self, FALSE);    // manipulate all of the controls including the current one
			return 0L;
		}
		else if( (GetAsyncKeyState(VK_MENU) & 0x8000 ? TRUE : FALSE) ) {
			gNewDragWindow(DragWindow, self, TRUE);    // manipulate the current control only
			return 0L;
		}
		else {
			gRmAllOfDWs(DragWindow);  // if any dragging window exists, cancel the dragging window
#ifdef WIN32
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
			return 0L;
		}
	}
	// Yanghui

	i = get_index(wParam);
	if (i >= 0  &&  iMouseFunction[i])
		iMouseFunction[i](self, (unsigned) wParam);

	return 0L;
}


// Yanghui:
private	imeth	long	process_wm_mousemove(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (gDesignMode(self) ) {
#ifdef WIN32
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
	}
	return 0L;
}
// Yanghui


private	imeth	long	process_wm_rbuttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	int  i;
	i = get_index(wParam);

	if (i >= 0  &&  iMouseFunction[i])
		iMouseFunction[i](self, (unsigned) wParam);

	return 0L;
}


imeth	gSetFont(object font)
{
	HANDLE	hctl = gHandle(self);

	if (iFont)
		gDispose(iFont);
	iCaretHeight = 0;

	iFont = font;

	if (hctl) {
		SendMessage(hctl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
#ifdef	WIN32
		if(ClassOf(self) == StaticTextControl) { // Yanghui:
			int margins = gAveCharWidth(iFont);  
			SendMessage(hctl, EM_SETMARGINS, (WPARAM)EC_LEFTMARGIN,  margins);  
			SendMessage(hctl, EM_SETMARGINS, (WPARAM)EC_RIGHTMARGIN, margins);  
		}  // Yanghui
#endif
	}
	
	return self;
}


imeth	gGetFont()
{
	return iFont;
}



// Yanghui:

///////////////////////////////////////////////////////////////////////////////////////////////////////
// gPutToMostFront:
//
// if bool is TRUE, the position of the control in the link object "iControls" becomes the last node 
// if bool is FALSE, the position of the control in the link object "iControls" becomes the first node 
//
///////////////////////////////////////////////////////////////////////////////////////////////////////
imeth object gPutToMostFront(BOOL bool)
{ 
	object linkObject=NULL, linkSeq=NULL, link=NULL, obj=NULL;
	
	linkObject = gGetControls(gGetParent(self));
	if(!linkObject)
		return NULL;

	for (linkSeq=gSequenceLinks(linkObject) ; link = gNext(linkSeq) ; ) {
		obj = gValue(link);
		if( obj == self) {
			gRemove(link);
			if(bool) {
				gAddLast(linkObject, self);
				SetWindowPos(gHandle(self), HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
			}
			else {
				gAddFirst(linkObject, self);
				SetWindowPos(gHandle(self), HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
			}
			gDispose(linkSeq);
			break;
		}
	}
	return linkObject;
}



//////////////////////////////////////////////////////////////////////////////////////////////////////////
// drawOneLineOfText:     draw one line of text
// 
// HDC hdcView:           a handle to the view DC
// double dScaleX:        the scaling factor from view DC to printer DC
//
// HDC hdcPrinter:        a handle to the printer DC
// LOGFONT *pLogfont:     a pointer to the LOGFONT structure in printer DC unit
// const RECT *pRectLine: a pointer to the rectangle which is in printer DC unit to put the line of text
// char lineOfText[]:     the line of text
// nAlign:                DT_LEFT | DT_RIGHT | DT_CENTER
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
cmeth int gDrawOneLineOfText(HDC hdcView, double dScaleX, HDC hdcPrinter, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign, int nAveCharWidth)
{
	char     *lineOfTextCopy, *token, *strPoint, *pLineOfTextCopy;
	int      nPreToken, nUpToToken, nToken, nLineOfText;
	SIZE     sizePreToken, sizeUpToToken, sizeToken;
	RECT     rectToken, rectLine;
	LOGFONT  logfont;

	HFONT    hfont, hfontOld;
	int      nWhiteSpaceSize, nWhiteSpacePreToken, nWhiteSpaceToken;

	if(!hdcView || !hdcPrinter || !pLogfont || !pRectLine || !lineOfText || !(*lineOfText) )
		return -1;
	
	if(dScaleX<0)
		dScaleX = -dScaleX;

#ifdef WIN32
	GetTextExtentPoint32(hdcView, " ", 1, &sizeToken);
#else
	GetTextExtentPoint(hdcView, " ", 1, &sizeToken);
#endif
	nWhiteSpaceSize = sizeToken.cx*dScaleX + 0.5;
	if(nWhiteSpaceSize<=0)
		return -1;
	
	lineOfTextCopy = (char *)malloc(strlen(lineOfText)+1);
	if(!lineOfTextCopy)
		return -1;
	strcpy(lineOfTextCopy, lineOfText);
	nLineOfText = strlen(lineOfText);

	CopyRect(&rectLine, pRectLine);
	logfont = *pLogfont;

	// get the logfont for the line of text to support WYSIWYG
	matchLogfontForTheLineOfText(hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, nAlign, nAveCharWidth);

	hfont = CreateFontIndirect(&logfont);
	if(!hfont) {
		free(lineOfTextCopy);
		return -1;
	}

	// replace the print dc font with the control font
	hfontOld = SelectObject(hdcPrinter, hfont);

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfTextCopy, strlen(lineOfTextCopy), &sizeUpToToken);
#else
	GetTextExtentPoint(hdcView, lineOfTextCopy, strlen(lineOfTextCopy), &sizeUpToToken);
#endif

	if(nAlign & DT_RIGHT)       // align right
		rectLine.left =  rectLine.right - sizeUpToToken.cx*dScaleX + 0.5;
	else if(nAlign & DT_CENTER) // align center
		rectLine.left = (rectLine.left + rectLine.right - sizeUpToToken.cx*dScaleX)*0.5 + 0.5;

	CopyRect(&rectToken, &rectLine);
	pLineOfTextCopy = lineOfTextCopy;

	token = strtok(pLineOfTextCopy, " \t\0");
	if(!token) {
		DeleteObject( SelectObject(hdcPrinter, hfontOld) );
		free(lineOfTextCopy);
		return -1;
	}

	nToken = strlen(token);
	nPreToken = strstr(lineOfText, token) - lineOfText;
	nUpToToken = nPreToken + nToken;

	if(nUpToToken < nLineOfText)
		pLineOfTextCopy = lineOfTextCopy + nUpToToken + 1 ;
	else
		pLineOfTextCopy = lineOfTextCopy + nUpToToken;

	if(nPreToken>0) {
#ifdef WIN32
		GetTextExtentPoint32(hdcView, lineOfText, nPreToken, &sizePreToken);
#else
		GetTextExtentPoint(hdcView, lineOfText, nPreToken, &sizePreToken);
#endif
	}
	else {
		sizePreToken.cx = 0;
		sizePreToken.cy = 0;
	}

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#else
	GetTextExtentPoint(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#endif

	rectToken.left  = rectLine.left + sizePreToken.cx*dScaleX + 0.5;
	rectToken.right = rectLine.left + sizeUpToToken.cx*dScaleX + 0.5;

#ifdef WIN32
	GetTextExtentPoint32(hdcPrinter, token, nToken, &sizeToken);
#else
	GetTextExtentPoint(hdcPrinter, token, nToken, &sizeToken);
#endif

	if(nToken>0)
		TextOut(hdcPrinter, rectToken.left, (rectToken.bottom+rectToken.top-sizeToken.cy)/2, token, nToken);

	nWhiteSpacePreToken = (rectToken.right-rectToken.left - sizeToken.cx)/nWhiteSpaceSize;

	// I have to use pLineOfTextCopy, instead of NULL, to avoid data corruption
	while( (token=strtok(pLineOfTextCopy,"  \t\0")) != NULL ) {
		nToken = strlen(token);
		strPoint =  nUpToToken + lineOfText;
		nPreToken = strstr(strPoint, token) - strPoint + nUpToToken;
		nUpToToken = nPreToken + nToken;

		if(nUpToToken < nLineOfText)
			pLineOfTextCopy = lineOfTextCopy + nUpToToken + 1 ;
		else
			pLineOfTextCopy = lineOfTextCopy + nUpToToken;

		if(nPreToken>0) {
#ifdef WIN32
			GetTextExtentPoint32(hdcView, lineOfText, nPreToken, &sizePreToken);
#else
			GetTextExtentPoint(hdcView, lineOfText, nPreToken, &sizePreToken);
#endif
		}
		else {
			sizePreToken.cx = 0;
			sizePreToken.cy = 0;
		}

#ifdef WIN32
		GetTextExtentPoint32(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#else
		GetTextExtentPoint(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#endif

		rectToken.left  = rectLine.left + sizePreToken.cx*dScaleX + 0.5;
		rectToken.right = rectLine.left + sizeUpToToken.cx*dScaleX + 0.5;

#ifdef WIN32
		GetTextExtentPoint32(hdcPrinter, token, nToken, &sizeToken);
#else
		GetTextExtentPoint(hdcPrinter, token, nToken, &sizeToken);
#endif

		nWhiteSpaceToken = (rectToken.right-rectToken.left-sizeToken.cx)/nWhiteSpaceSize;

		if(nWhiteSpacePreToken >= nWhiteSpaceToken+1) {
			nWhiteSpacePreToken = (rectToken.right-rectToken.left-sizeToken.cx+nWhiteSpaceSize/2)/nWhiteSpaceSize;
			rectToken.left  -= nWhiteSpaceSize/2;
			rectToken.right -= nWhiteSpaceSize/2;
		}
		else if(nWhiteSpaceToken >= nWhiteSpacePreToken + 1) {
			nWhiteSpacePreToken = (rectToken.right-rectToken.left-sizeToken.cx-nWhiteSpaceSize/2)/nWhiteSpaceSize;
			rectToken.left  += nWhiteSpaceSize/2;
			rectToken.right += nWhiteSpaceSize/2;
		}
		else 
			nWhiteSpacePreToken = nWhiteSpaceToken;

		if( (nToken>0) && (rectToken.right<=rectLine.right+2*nWhiteSpaceSize) )  // two extra white space
			TextOut(hdcPrinter, rectToken.left, (rectToken.bottom+rectToken.top-sizeToken.cy)/2, token, nToken);
	}

	DeleteObject( SelectObject(hdcPrinter, hfontOld) );
	free(lineOfTextCopy);

	return 0;
}



//////////////////////////////////////////////////////////////////////////////////////////////////////////
// gDrawOneLineOfTextToMetaFile:  draw one line of text to metafile
// 
// HDC        hdcView:     a handle to the view DC
// double     dScaleX:     the scaling factor from view DC to metafile DC
//
// HDC        hdcMetafile: a handle to the metafile DC
// LOGFONT    *pLogfont:   a pointer to the LOGFONT structure in metafile DC unit
// char       *lineOfText: the line of text
// int        nAlign:      DT_LEFT | DT_RIGHT | DT_CENTER
// const RECT *pRectLine:  a pointer to the rectangle which is in metafile DC unit to put the line of text
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
cmeth int gDrawOneLineOfTextToMetaFile(HDC hdcView, double dScaleX, HDC hdcMetafile, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign)
{
	int     nLineOfText;
	SIZE    sizeLineOfText;
	RECT    rectLine;
	HFONT   hfont, hfontOld;
	LOGFONT logfont;

	if(!hdcView || !hdcMetafile || !pLogfont || !pRectLine || !lineOfText || !(*lineOfText) )
		return -1;
	
	if(dScaleX<0)
		dScaleX = -dScaleX;
	
	nLineOfText = strlen(lineOfText);
	CopyRect(&rectLine, pRectLine);
	logfont = *pLogfont;

	// get the logfont for the line of text to support WYSIWYG
	matchLogfontForTheLineOfTextOfMetaFile(hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, nAlign);

	hfont = CreateFontIndirect(&logfont);
	if(!hfont) 
		return -1;

	// replace the metafile dc font with the control font
	hfontOld = SelectObject(hdcMetafile, hfont);

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfText, nLineOfText, &sizeLineOfText);
#else
	GetTextExtentPoint(hdcView, lineOfText, nLineOfText, &sizeLineOfText);
#endif

	if(nAlign & DT_RIGHT)        // align right
		rectLine.left =  rectLine.right - sizeLineOfText.cx*dScaleX + 0.5;
	else if(nAlign & DT_CENTER)  // align center
		rectLine.left = (rectLine.left + rectLine.right - sizeLineOfText.cx*dScaleX)*0.5 + 0.5;

	TextOut(hdcMetafile, rectLine.left, (int)(rectLine.bottom+rectLine.top+sizeLineOfText.cy*dScaleX+1)/2, 
		lineOfText, nLineOfText);

	DeleteObject( SelectObject(hdcMetafile, hfontOld) );
	return 0;
}



////////////////////////////////////////////////////////////////////////////////////////////
// matchLogfontForTheLineOfText: In order to support WYSIWYG for the entire line of text, 
//                        individual token must be analyzed. A minimal font size fit for 
//                        all tokens if the line of text is managed.
// 
// HDC hdcView:           a handle to the view DC
// double dScaleX:        the scaling factor from view DC to printer DC
//
// HDC hdcPrinter:        a handle to the printer DC
// LOGFONT *pLogfont:     a pointer to the LOGFONT structure in printer DC unit
// const RECT *pRectLine: the rectangle in printer DC unit to put the line of text
// char *lineOfText:      the line of text
// nAlign:                DT_LEFT | DT_RIGHT | DT_CENTER
//
////////////////////////////////////////////////////////////////////////////////////////////
static int matchLogfontForTheLineOfText(HDC hdcView, double dScaleX, HDC hdcPrinter, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign, int nAveCharWidth)
{
	char     *lineOfTextCopy, *token, *strPoint, *pLineOfTextCopy;
	int      nPreToken, nUpToToken, nLineOfText;
	SIZE     sizePreToken, sizeUpToToken;
	RECT     rectLine, rectToken;
	int      nHalfWhiteSpace;

	if(!hdcView || !hdcPrinter || !pLogfont || !pRectLine || !lineOfText || !(*lineOfText) )
		return -1;

	if(dScaleX<0)
		dScaleX = -dScaleX;

	lineOfTextCopy = (char *)malloc(strlen(lineOfText)+1);
	if(!lineOfTextCopy)
		return -1;
	strcpy(lineOfTextCopy, lineOfText);
	nLineOfText = strlen(lineOfText);
	
#ifdef WIN32
	GetTextExtentPoint32(hdcView, " ", 1, &sizePreToken);
#else
	GetTextExtentPoint(hdcView, " ", 1, &sizePreToken);
#endif
	nHalfWhiteSpace = sizePreToken.cx*dScaleX/2.0 + 0.5;

	CopyRect(&rectLine, pRectLine);

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfTextCopy, strlen(lineOfTextCopy), &sizeUpToToken); // sizeUpToToken:temporary
#else
	GetTextExtentPoint(hdcView, lineOfTextCopy, strlen(lineOfTextCopy), &sizeUpToToken);   // sizeUpToToken:temporary
#endif

	if(nAlign & DT_RIGHT)       // align right
		rectLine.left =  rectLine.right - sizeUpToToken.cx*dScaleX + 0.5;
	else if(nAlign & DT_CENTER) // align center
		rectLine.left = (rectLine.left + rectLine.right - sizeUpToToken.cx*dScaleX)*0.5 + 0.5;

	CopyRect(&rectToken, &rectLine);
	pLineOfTextCopy = lineOfTextCopy;
	token = strtok(pLineOfTextCopy, " \t\0");
	if(!token) {
		free(lineOfTextCopy);
		return -1;
	}

	nPreToken = strstr(lineOfText, token) - lineOfText;
	nUpToToken = nPreToken + strlen(token);
	
	if(nUpToToken < nLineOfText)
		pLineOfTextCopy = lineOfTextCopy + nUpToToken + 1 ;
	else
		pLineOfTextCopy = lineOfTextCopy + nUpToToken;

	if(nPreToken>0) {
#ifdef WIN32
		GetTextExtentPoint32(hdcView, lineOfText, nPreToken, &sizePreToken);
#else
		GetTextExtentPoint(hdcView, lineOfText, nPreToken, &sizePreToken);
#endif
	}
	else {
		sizePreToken.cx = 0;
		sizePreToken.cy = 0;
	}

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#else
	GetTextExtentPoint(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#endif

	rectToken.left  = rectLine.left + sizePreToken.cx*dScaleX + 0.5;
	rectToken.right = rectLine.left + sizeUpToToken.cx*dScaleX + 0.5;

	matchLogfontForOneToken(hdcPrinter, pLogfont, &rectToken, token, dScaleX, nHalfWhiteSpace);

	// I have to use pLineOfTextCopy, instead of NULL, to avoid data corruption
	while( (token=strtok(pLineOfTextCopy," \t\0")) !=NULL ) {
		strPoint = nUpToToken + lineOfText;
		nPreToken = strstr(strPoint, token) - strPoint + nUpToToken;
		nUpToToken = nPreToken + strlen(token);

		if(nUpToToken < nLineOfText)
			pLineOfTextCopy = lineOfTextCopy + nUpToToken + 1 ;
		else
			pLineOfTextCopy = lineOfTextCopy + nUpToToken;

		if(nPreToken>0) {
#ifdef WIN32
			GetTextExtentPoint32(hdcView, lineOfText, nPreToken, &sizePreToken);
#else
			GetTextExtentPoint(hdcView, lineOfText, nPreToken, &sizePreToken);
#endif
		}
		else {
			sizePreToken.cx = 0;
			sizePreToken.cy = 0;
		}

#ifdef WIN32
		GetTextExtentPoint32(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#else
		GetTextExtentPoint(hdcView, lineOfText, nUpToToken, &sizeUpToToken);
#endif

		rectToken.left  = rectLine.left + sizePreToken.cx*dScaleX + 0.5;
		rectToken.right = rectLine.left + sizeUpToToken.cx*dScaleX + 0.5;

		if(rectToken.right <= rectLine.right+nHalfWhiteSpace)  // half half white space 
			matchLogfontForOneToken(hdcPrinter, pLogfont, &rectToken, token, dScaleX, nHalfWhiteSpace);
	}

	free(lineOfTextCopy);
	return 0;
}



/////////////////////////////////////////////////////////////////////////////////////////////
// matchLogfontForOneToken: In order to support WYSIWYG for the entire line of text, 
//                     individual token must be analyzed. A minimal font size suited for 
//                     all tokens in the entire line is managed.
// 
// HDC hdcPrinter:     a handle to the printer DC
// LOGFONT *pLogfont:  a pointer to the LOGFONT structure in printer DC unit
// const RECT *pRect:  the rectangle in printer DC unit to put the text
// const char *token:  the text
//
////////////////////////////////////////////////////////////////////////////////////////////
static int matchLogfontForOneToken(HDC hdcPrinter, LOGFONT *pLogfont, const RECT *pRect, 
									const char *token, double dScaleX, int nHalfWhiteSpace)
{
	HFONT hfont, hfontOld;
	int   nToken, cxRect, nDelta, nLoop;
	SIZE  sizeToken;

	if(!hdcPrinter || !pLogfont || !pRect || !token || !(*token) )
		return -1;

	if(dScaleX<0)
		dScaleX = -dScaleX;

	hfont = CreateFontIndirect(pLogfont);
	if(!hfont)
		return -1;

	cxRect = pRect->right - pRect->left;
	
	// replace the print dc font with the control font
	hfontOld = SelectObject(hdcPrinter, hfont);

	nToken = strlen(token);

#ifdef WIN32
	GetTextExtentPoint32(hdcPrinter, token, nToken, &sizeToken);
#else
	GetTextExtentPoint(hdcPrinter, token, nToken, &sizeToken);
#endif

	nDelta = sizeToken.cx - cxRect;
	nLoop = 0;
	while( nDelta > nHalfWhiteSpace) {  // Half WhiteSpace
		DeleteObject( SelectObject(hdcPrinter, hfontOld) );
		pLogfont->lfWidth  = pLogfont->lfWidth*0.9875;
		pLogfont->lfHeight = pLogfont->lfHeight*0.9875;

		hfont = CreateFontIndirect(pLogfont);
		if(!hfont)
			return -1;

		// replace the print dc font with the control font
		hfontOld = SelectObject(hdcPrinter, hfont);

#ifdef WIN32 
		GetTextExtentPoint32(hdcPrinter, token, nToken, &sizeToken);
#else 
		GetTextExtentPoint(hdcPrinter, token, nToken, &sizeToken);
#endif 

		nDelta = sizeToken.cx - cxRect;
		if(++nLoop > 30)
			break;
	}
	// no printing
	DeleteObject( SelectObject(hdcPrinter, hfontOld) );

	return 0;
}



////////////////////////////////////////////////////////////////////////////////////////////
// matchLogfontForTheLineOfTextOfMetaFile: In order to support WYSIWYG for the entire line of text, 
//                         individual token must be analyzed. A minimal font size fit for 
//                         all tokens if the line of text is managed.
// 
// HDC        hdcView:     a handle to the view DC
// double     dScaleX:     the scaling factor from view DC to printer DC
//
// HDC        hdcMetafile: a handle to the printer DC
// LOGFONT    *pLogfont:   a pointer to the LOGFONT structure in printer DC unit
// const RECT *pRectLine:  the rectangle in printer DC unit to put the line of text
// char       *lineOfText: the line of text
// int        nAlign:      DT_LEFT | DT_RIGHT | DT_CENTER
//
////////////////////////////////////////////////////////////////////////////////////////////
static int matchLogfontForTheLineOfTextOfMetaFile(HDC hdcView, double dScaleX, HDC hdcMetafile, LOGFONT *pLogfont, 
                             const RECT *pRectLine, char *lineOfText, int nAlign)
{
	HFONT  hfont, hfontOld;
	RECT   rectLine;
	SIZE   sizeLineOfText;
	char   *lineOfTextCopy;
	int    nLineOfText, cxRect, nDelta, nLoop, nHalfWhiteSpace;

	if(!hdcView || !hdcMetafile || !pLogfont || !pRectLine || !lineOfText || !(*lineOfText) )
		return -1;

	if(dScaleX<0)
		dScaleX = -dScaleX;

	lineOfTextCopy = (char *)malloc(strlen(lineOfText)+1);
	if(!lineOfTextCopy)
		return -1;
	strcpy(lineOfTextCopy, lineOfText);
	nLineOfText = strlen(lineOfText);
	
#ifdef WIN32
	GetTextExtentPoint32(hdcView, " ", 1, &sizeLineOfText);
#else
	GetTextExtentPoint(hdcView, " ", 1, &sizeLineOfText);
#endif
	nHalfWhiteSpace = sizeLineOfText.cx*dScaleX/2.0 + 0.5;
	CopyRect(&rectLine, pRectLine);

#ifdef WIN32
	GetTextExtentPoint32(hdcView, lineOfTextCopy, strlen(lineOfTextCopy), &sizeLineOfText);  // sizeLineOfText:temporary
#else
	GetTextExtentPoint(hdcView,   lineOfTextCopy, strlen(lineOfTextCopy), &sizeLineOfText);  // sizeLineOfText:temporary
#endif
	cxRect = sizeLineOfText.cx*dScaleX;

	if(nAlign & DT_RIGHT)       // align right
		rectLine.left =  rectLine.right - cxRect + 0.5;
	else if(nAlign & DT_CENTER) // align center
		rectLine.left = (rectLine.left + rectLine.right - cxRect)*0.5 + 0.5;

	hfont = CreateFontIndirect(pLogfont);
	if(!hfont) {
		free(lineOfTextCopy);
		return -1;
	}

	// replace the metafile dc font with the control font
	hfontOld = SelectObject(hdcMetafile, hfont);

#ifdef WIN32
	GetTextExtentPoint32(hdcMetafile, lineOfTextCopy, nLineOfText, &sizeLineOfText);
#else
	GetTextExtentPoint(hdcMetafile, lineOfTextCopy, nLineOfText, &sizeLineOfText);
#endif
	sizeLineOfText.cx = sizeLineOfText.cx*dScaleX + 0.5;
	sizeLineOfText.cy = sizeLineOfText.cy*dScaleX + 0.5;

	nDelta = sizeLineOfText.cx - cxRect;
	nLoop = 0;
	while( nDelta > nHalfWhiteSpace) {  // Half WhiteSpace
		DeleteObject( SelectObject(hdcMetafile, hfontOld) );
		pLogfont->lfWidth  = pLogfont->lfWidth*0.9875;
		pLogfont->lfHeight = pLogfont->lfHeight*0.9875;

		hfont = CreateFontIndirect(pLogfont);
		if(!hfont) {
			free(lineOfTextCopy);
			return -1;
		}

		// replace the print dc font with the control font
		hfontOld = SelectObject(hdcMetafile, hfont);

#ifdef WIN32 
		GetTextExtentPoint32(hdcMetafile, lineOfTextCopy, nLineOfText, &sizeLineOfText);
#else 
		GetTextExtentPoint(hdcMetafile, lineOfTextCopy, nLineOfText, &sizeLineOfText);
#endif 
		sizeLineOfText.cx = sizeLineOfText.cx*dScaleX + 0.5;
		sizeLineOfText.cy = sizeLineOfText.cy*dScaleX + 0.5;


		nDelta = sizeLineOfText.cx - cxRect;
		if(++nLoop > 30)
			break;
	}
	// no printing
	DeleteObject( SelectObject(hdcMetafile, hfontOld) );
	
	free(lineOfTextCopy);
	return 0;
}



// Yanghui

imeth	gAddSetFunctionBefore(int (*fun)())
{
	if (!iSetFunctions)
		iSetFunctions = gNew(FunctionList);

	gAddFunctionBefore(iSetFunctions, fun);
	
	return self;
}

imeth	gAddSetFunctionAfter(int (*fun)())
{
	if (!iSetFunctions)
		iSetFunctions = gNew(FunctionList);

	gAddFunctionAfter(iSetFunctions, fun);
	
	return self;
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	org = NULL;
	
	if (iSetFunctions) {
		object	obj = gFirst(iSetFunctions);
		
		if (ClassOf(obj) != String &&  ClassOf(obj) != JavaCallbackClassSurrogate)
			org = (ofun) gPointerValue(obj);
		
		iSetFunctions = gDeepDispose(iSetFunctions);
	}

	if (fun)
		gAddSetFunctionBefore(self, fun);
	
	return org;
}

imeth	int	gExecuteSetFunctions(object dlg)
{
	return iSetFunctions ? gExecuteFunctionsObjObj(iSetFunctions, self, dlg) : 0;
}

imeth	gAddDataChangeFunctionBefore(int (*fun)())
{
	if (!iDataChangeFunctions)
		iDataChangeFunctions = gNew(FunctionList);

	gAddFunctionBefore(iDataChangeFunctions, fun);
	
	return self;
}

imeth	gAddDataChangeFunctionAfter(int (*fun)())
{
	if (!iDataChangeFunctions)
		iDataChangeFunctions = gNew(FunctionList);

	gAddFunctionAfter(iDataChangeFunctions, fun);
	
	return self;
}

imeth	ofun	gDataChangeFunction(int (*fun)())
{
	ofun	org = NULL;
	
	if (iDataChangeFunctions) {
		object	obj = gFirst(iDataChangeFunctions);
		
		if (ClassOf(obj) != String &&  ClassOf(obj) != JavaCallbackClassSurrogate)
			org = (ofun) gPointerValue(obj);
		
		iDataChangeFunctions = gDeepDispose(iDataChangeFunctions);
	}

	if (fun)
		gAddDataChangeFunctionBefore(self, fun);
	
	return org;
}

imeth	int	gExecuteDataChangeFunctions(object dlg)
{
	return iDataChangeFunctions ? gExecuteFunctionsObjObj(iDataChangeFunctions, self, dlg) : 0;
}

imeth	gSetFileResource(char *fname)
{
	if (iFileResource)
		gChangeStrValue(iFileResource, fname);
	else
		iFileResource = gNewWithStr(String, fname);

	return self;
}

imeth	char	*gFileResource()
{
	return iFileResource ? gStringValue(iFileResource) : "";
}
	
imeth gSetXPathBinding(char *binding)
{
	if (iXPathBinding)
		gChangeStrValue(iXPathBinding, binding);
	else
		iXPathBinding=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gXPathBinding()
{
	return iXPathBinding ? gStringValue(iXPathBinding) : "";
}

imeth	gResetText()
{
	return self;
}

imeth	int gCurrentLanguage()
{
	return iCurrentLanguage;
}

imeth	gSetCurrentLanguage(int currLang)
{
	if (currLang == iCurrentLanguage)
		return self;

	iCurrentLanguage=currLang;
	gResetText(self);
	
	return self;	
}

imeth	gSetLanguageText(int lang,char *val)
{	
	if (!iLanguageText[lang])
		iLanguageText[lang] = gNewWithStr(String, val);
	else
		gChangeStrValue(iLanguageText[lang], val);

	return self;
}

imeth	char * gLanguageText(int lang)
{
	//return the text for the langauge, if it isn't there, return the English
/*
	if (iLanguageText[lang])
		return gStringValue(iLanguageText[lang]);
	else
		if (iLanguageText[ENGLISH])
			return gStringValue(iLanguageText[ENGLISH]);
		else
			return "";
*/	
	return iLanguageText[lang] ? gStringValue(iLanguageText[lang]) : 
		(iLanguageText[ENGLISH] ? gStringValue(iLanguageText[ENGLISH]) : "");
}

imeth	gLanguageObject()
{
	return iLanguageText[iCurrentLanguage];
}


