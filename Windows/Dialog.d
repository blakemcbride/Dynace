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

#include <ctype.h>

defclass  Dialog : PropertyList  {
	int	iType;		/*  MODAL / MODELESS			*/
	iFile;			/*  resource file name			*/
	char	*iResource;	/*  resource char ID			*/
	WORD	iID;		/*  resource WORD ID			*/
	HWND	iHDlg;		/*  handle to dialog			*/
	iParent;		/*  parent window			*/
	int	iInDlg;		/*  1=in dialog, 0=not in dialog  	*/
	iControls;	     	/*  IntegerDictionary of controls  	*/
	iControlNames;		/*  StringDictionary of controls	*/
	int	iAutoDispose;	/*  Auto dispose on close flag		*/
	int	iHPos;		/*  dialog horz. position		*/
	int	iVPos;		/*  dialog vert. position		*/
	int	iHSize;
	int	iVSize;
	HWND	iZOrder;	/*  dialog Z order			*/
	iCursor;
	int	iWait;		/*  number of times set to wait  	*/
	iDisableObject;

	iInitFunctions;		/*  Functions executed after dialog created  */
	iFunctions;		/*  Function called when dialog completes    */
	iTabFunctions;		/*  Function called when dialog completes    */
	iActivateFunctions;	/*  window activation function		     */
	iTab;			/*  holds the tab control class for this dlg */
	iTag;			/*  arbitrary tag object associated with dlg */
	int	iAutoDisposeTag;
	iResult;		/*  object to get result of dialog	*/

	iTitle;			/*  dialog title			*/

	iMessageHandlers;	/*  IntDict of message handlers  	*/

	iBackBrush;		/*  background color			*/
	iTextBrush;		/*  text color				*/

	iTopic;			/*  help topic associated with entire dialog */
	char	*iPrevTopic;	/*  previous topic	*/

	iClassName;		/*  name of the custom class if used	*/
	int	iRegistered;	/*  1=class registered			*/

	iIcon;			/*  icon				*/
	iTask;			/*  Task object if this is part of a tasklist  */
	
	/*  Modal dialog simulation variables  */

	int	iEndDialog;
	int	iModalResult;

	iDockedChildren;	//  Windows docked to this window (children)
	iDockedParents;		//  Windows this window is docked to (parents)

	int	iDlgState ;	/*  Used to determine show state	*/

	int	iCenterDialog;	/*  Initialize dialog to be centered.   */

 class:
	cRegisteredClasses;	/*  StringDictionary of registered classes  */
	ifun	cAccessMode;
	cInitFunctions;
	
init:	init_class;
};

#ifdef	WIN32
#include <commctrl.h>
#endif

#include "demo.h"

extern	BOOL   (WINAPI *Ctl3dSubclassDlgEx)(HWND, DWORD);


static	void	update_position(ivType *iv);
static	void	updateDockedWindows(ivType *iv, int hp, int vp);


#define MODAL		0
#define MODELESS	1

static	FARPROC		pDialogProc = NULL;

imeth	gCheckControlSecurity(int init)
{
	object	seq, obj, ctl;
	for (seq = gSequence(iControls) ; obj = gNext(seq) ; )  {
		ctl = gValue(obj);
		if (ClassOf(ctl) != StaticControl) {
			if (cAccessMode) {
				int	mode = cAccessMode(ctl);
				if (mode) {
					gDisable(ctl);
					if (mode == 2  &&  gIsKindOf(ctl, SpinControl))
						gHideData(ctl, 1);
				}
			}
			if (init)
				gInitialize(ctl, iHDlg, self);
		}
	}
	
	return self;
}

static	void	initControls(object set, HWND hDlg, object dlg)
{
	object	ss, i, ctl;

	/* static controls must be initialized first to support the
	   DirListBox controls  */
	for (ss = gSequence(set) ; i = gNext(ss) ; )  {
		ctl = gValue(i);
		if (ClassOf(ctl) == StaticControl)
			gInitialize(ctl, hDlg, dlg);
	}
	
	gSetCurrentLanguage(dlg, gLanguage(Application));	
	gCheckControlSecurity(dlg, 1);
	DEMO_CHK_MAX;
}

imeth	gSetCurrentLanguage(int language)
{
	object	ss, i, ctl;

	if (iControls)
		for (ss = gSequence(iControls) ; i = gNext(ss) ; )  {
			ctl = gValue(i);
			gSetCurrentLanguage(ctl,language);
		}
	
	return self;
}

imeth	int	gCheckValue()
{
	object	ss, i, ctl;
	int	r = 0;

	if (iDisableObject)
		gDisable(iDisableObject);
	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		if (gCheckValue(ctl=gValue(i))) {
			r = 1;	/*   error  */
			gDispose(ss);
			break;
		}
	if (iDisableObject)
		gEnable(iDisableObject);
	if (r)
		gSetFocus(ctl);  // needed by tasklist
	DEMO_CHK_MAX;
	return r;
}

static	int	centerDialog(object dlg)
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

static	BOOL	CALLBACK
DialogProc(HWND		hDlg, 
	   UINT		mMsg, 
	   WPARAM	wParam, 
	   LPARAM	lParam)
{
	object	h;
	BOOL	r = FALSE;
	HC_VARS;

	if (mMsg == WM_INITDIALOG)  {
		HC_NEW(DIALOG_HANDLE_CACHE, hDlg, (object) lParam);
		HC_UPDATE(DIALOG_HANDLE_CACHE, hDlg);
		iInDlg = 1;
		iHDlg = hDlg;
		if (iClassName  &&  Ctl3dSubclassDlgEx)
			Ctl3dSubclassDlgEx(hDlg, 0xffff);
		initControls(iControls, hDlg, self);
		if (iTitle)
			SetWindowText(hDlg, gStringValue(iTitle));
		if (iHPos != -1  &&  iVPos != -1)
			SetWindowPos(hDlg, iZOrder, iHPos, iVPos, 0, 0, SWP_NOSIZE);
		else {
			update_position(iv);
			if (iZOrder != HWND_TOP)
				SetWindowPos(hDlg, iZOrder, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE));
		}


		if (iInitFunctions)
			gExecuteFunctionsObj(iInitFunctions, self);
		
		if (iCenterDialog)
			centerDialog(self);
		if (iTab)
			gTabInit(iTab, self);
		r = TRUE;
		DEMO_MSG(hDlg);
	} else
		HC_UPDATE(DIALOG_HANDLE_CACHE, hDlg);
	if (self)  
		h = gFindValueInt(iMessageHandlers, mMsg);
	if (self  &&  h)
		return gProcessDialogMsg(h, self, hDlg, mMsg, wParam, lParam);
	else
		return r;
}

/*
  We need to not perform kill focus functions when the dialog
  is being canceled and not twice when it's being disposed.
*/

static	void	eliminate_killfocus_functions(object ctls)
{
	object	seq, ctl;

	for (seq=gSequence(ctls) ; ctl = gNext(seq) ; ) {
		ctl = gValue(ctl);
		if (ClassOf(ctl) == TextControl  ||
		    ClassOf(ctl) == DateControl  ||
		    ClassOf(ctl) == TimeControl  ||
		    ClassOf(ctl) == NumericControl  ||
		    ClassOf(ctl) == ComboBox)
			gSetFunction(ctl, NULL);
		if (!strcmp(gName(ClassOf(ctl)), "SpreadsheetControl"))
			gUpdate(ctl);
	}
}

private	imeth	BOOL	process_wm_command(object	self, 
					   HWND		hdlg, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	unsigned	cmd;
	object		ctl;
	int		res, exit;
	unsigned	id;
	BOOL		ret = FALSE;

#ifdef WIN32
	cmd = HIWORD(wParam);
#else
	cmd = HIWORD(lParam);
#endif
	id = LOWORD(wParam);
	switch (cmd)  {
	case BN_CLICKED:
		ctl = gFindValueInt(iControls, id);
		if (ctl  &&  ClassOf(ctl) == PushButton) {
			object	seq, obj, ctl;
			
			for (seq=gSequence(iControls) ; obj=gNext(seq) ; ) {
				ctl = gValue(obj);
				if (!strcmp(gName(ClassOf(ctl)), "SpreadsheetControl"))
					gUpdate(ctl);
			}
		}
		if (ctl  &&  (ClassOf(ctl) == PushButton  ||  ClassOf(ctl) == RadioButton  ||  ClassOf(ctl) == CheckBox))
			gPerform(ctl);
		if (ctl  &&  ClassOf(ctl) == PushButton)
			exit = gGetExitType(ctl);
		else if (id == IDOK)
			exit = IDOK;
		else if (id == IDCANCEL)
			exit = IDCANCEL;
		else
			exit = 0;
		
		if (exit  &&  iParent  &&  !strcmp(gName(ClassOf(iParent)), "TabControl")) {
			PostMessage(gHandle(gDialog(iParent)), mMsg, wParam, lParam);
			break;
		}
		if (exit == IDOK)
			if (gCheckValue(self))  {
				ret = FALSE;
				break;
			}
		if (exit) {
			eliminate_killfocus_functions(iControls);
			res = id == IDCANCEL ? 0 : id;
			if (iTabFunctions) {
				object fun;
				fun = iTabFunctions;
				iTabFunctions = NULL;
				res = gExecuteFunctionsObjInt(fun, self, res);
				gDeepDispose(fun);
				if (!IsObj(self))
					return TRUE;
			}
			if (iFunctions) {
				object	fun;
				fun = iFunctions;
				iFunctions = NULL;
				res = gExecuteFunctionsObjInt(fun, self, res);
				gDeepDispose(fun);
				if (!IsObj(self))
					return TRUE;
			}
			if (iResult)
				gChangeShortValue(iResult, res);
			update_position(iv);
			iInDlg = 0;
			if (iType == MODAL) {
//				EndDialog(hdlg, res);
				iEndDialog = 1;
				iModalResult = res;
			} else  {
				DestroyWindow(hdlg);
				gRemoveModeless(MessageDispatcher, hdlg);
				gReleaseHandle(self);
			}					
			if (iAutoDispose)
				gDispose(self);
			ret = TRUE;
		}
		break;
	case LBN_DBLCLK:
/*	case CBN_DBLCLK:   same value as LBN_DBLCLK  */
		ctl = gFindValueInt(iControls, id);
		if (ctl  &&  (gIsKindOf(ctl, ListBox)   ||
			      gIsKindOf(ctl, ComboBox)))  {
			gPerform(ctl);
			ret = TRUE;
		}
		break;
	case LBN_SELCHANGE:
/*	case CBN_SELCHANGE:    same value as LBN_SELCHANGE  */
		ctl = gFindValueInt(iControls, id);
		if (ctl  &&  (gIsKindOf(ctl, ListBox)  ||
			      gIsKindOf(ctl, ComboBox))) {
			gPerformChg(ctl);
			ret = TRUE;
		}
		break;
	}
	return ret;
}

private	imeth	BOOL	process_wm_scroll(object	self, 
					  HWND		hdlg, 
					  UINT		mMsg, 
					  WPARAM	wParam, 
					  LPARAM	lParam)
{
	int		ctlID;
	HWND		hCtl;
	unsigned	nCode;	/*  converted to int later  */
	unsigned	pos;	/*  converted to int later  */
	object		ctl;


	nCode = LOWORD(wParam);
#ifdef	WIN32
	hCtl = (HWND) lParam;
	pos  = HIWORD(wParam);
#else
	hCtl = HIWORD(lParam);
	pos  = LOWORD(lParam);
#endif
	ctlID = GetDlgCtrlID(hCtl);
	if (!ctlID)
		return FALSE;
	ctl = gFindValueInt(iControls, ctlID);
	if (ctl)  {
		gProcessCmd(ctl, nCode, pos);
		return TRUE;
	}
	return FALSE;
}

private	imeth	BOOL	process_wm_color(object	self, 
					 HWND	hdlg, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	HANDLE	hctl;
	object	ctl, cbb, ctb;
	int	force = 0;
#ifdef	WIN32
	hctl = (HWND) lParam;
#else
	hctl = (HWND) LOWORD(lParam);
#endif
	ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, hctl);


/*  On rare occations Windows 95 passes a menu handle to a window procedure.??  */
/*  The check below is designed to handle that condition.  */
	if (ctl  &&  !gIsKindOf(ctl, Control))
		return FALSE;
	

	

	if (ctl  &&  ctl != self) {
		cbb = gGetBackBrush(ctl);
		ctb = gGetTextBrush(ctl);
		force = 1;
	} else
		cbb = NULL;
	if (!cbb  ||  !ctb) {
		force = 0;
		cbb = iBackBrush;
		ctb = iTextBrush;
	}


	if (gUsingAlternateColor(Application))
	{	
		if (hctl==iHDlg)
			return (BOOL) gHandle(cbb);  

		if (ctl  && ( /*gIsKindOf(ctl, ListBox)||*/gIsKindOf(ctl, PushButton)|| gIsKindOf(ctl, StaticControl)||gIsKindOf(ctl, StaticTextControl)||gIsKindOf(ctl, RadioButton)
			||gIsKindOf(ctl, LineControl)
			||gIsKindOf(ctl, CheckBox)||gIsKindOf(ctl, TextVector)||gIsKindOf(ctl, RectControl))){
			SetBkColor((HDC) wParam, gColor(cbb));
			SetTextColor((HDC) wParam, gColor(ctb));
			return (BOOL) gHandle(cbb);   //   ???   
		}
		
		if (ctl && gDisableStatus(ctl)==1)
		{
			SetBkColor((HDC) wParam, gColor(cbb));
			SetTextColor((HDC) wParam, gColor(ctb));
			return (BOOL) gHandle(cbb);   //   ???   
		}

		if (!ctl)
		{
			char windowClassName[256];
			GetClassName(hctl,windowClassName,256);
			
			if (strcmp(windowClassName,"SysTreeView32")&&strcmp(windowClassName,"ListBox")) //leave tree view alone
			{
 				SetBkColor((HDC) wParam, gColor(cbb));
				return (BOOL) gHandle(cbb);  
			}
			else
			{
				object br=gNewSolidBrush(SolidBrush,255,255,255);
				SetBkColor((HDC) wParam, gColor(br));
				return (BOOL) gHandle(br); 	

			}
		}
	}

	if (cbb  &&  ctb  &&  (force || !Ctl3dSubclassDlgEx))  {
		SetBkColor((HDC) wParam, gColor(cbb));
		SetTextColor((HDC) wParam, gColor(ctb));
		return (BOOL) gHandle(cbb);   //   ???   
	} else
		return FALSE;  //  code needed for Ctl3d stuff
		
}

private	imeth	BOOL	process_wm_activate(object	self, 
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


	return FALSE;
}

private	imeth	BOOL	process_wm_move(object	self, 
					HWND	hdlg, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (!IsIconic(iHDlg))
		updateDockedWindows(iv, iHPos, iVPos);
	update_position(iv);
	if (iTab)
		gUpdatePosition(iTab);
	return FALSE;
}

private	imeth	BOOL	process_wm_setcursor(object	self, 
					     HWND	hdlg, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iCursor)
		SetCursor(gHandle(iCursor));
	return iCursor ? TRUE : FALSE;
}

#ifdef	WIN32

private	imeth	BOOL	process_wm_notify(object	self, 
					  HWND		hdlg, 
					  UINT		mMsg, 
					  WPARAM	wParam, 
					  LPARAM	lParam)
{
	NMHDR	*h = (NMHDR *) lParam;

	if (h->code >= TVN_LAST  &&  h->code <= TVN_FIRST) {
		object	ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, h->hwndFrom);
		if (ctl  &&  IsObj(ctl)  &&  ClassOf(ctl) == TreeView)
			gProcessNotify(ctl, lParam);
	}
	return FALSE;
}

#endif

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

private	imeth	BOOL	process_wm_syscommand(object	self, 
					      HWND	hdlg, 
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
	return FALSE;
}

static	void	setup_handlers(object obj, ivType *iv)
{

	/*  Set up message handlers   */

	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	gAddDlgHandlerAfter(obj, (unsigned) WM_SYSCOMMAND, process_wm_syscommand);

	gAddDlgHandlerAfter(obj, (unsigned) WM_COMMAND, process_wm_command);

	gAddDlgHandlerAfter(obj, (unsigned) WM_VSCROLL, process_wm_scroll);
	gAddDlgHandlerAfter(obj, (unsigned) WM_HSCROLL, process_wm_scroll);

	gAddDlgHandlerAfter(obj, (unsigned) WM_MOVE, process_wm_move);

	gAddDlgHandlerAfter(obj, (unsigned) WM_ACTIVATE, process_wm_activate);

	gAddDlgHandlerAfter(obj, (unsigned) WM_SETCURSOR, process_wm_setcursor);

#ifdef	WIN32
	gAddDlgHandlerAfter(obj, (unsigned) WM_NOTIFY, process_wm_notify);
#endif

#ifndef unix
#ifdef	WIN32
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORBTN, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORDLG, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLOREDIT, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORLISTBOX, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORMSGBOX, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORSCROLLBAR, process_wm_color);
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLORSTATIC, process_wm_color);
#else
	gAddDlgHandlerAfter(obj, (unsigned) WM_CTLCOLOR, process_wm_color);
#endif
#endif
	DEMO_CHK_MAX;
}

/*  New only used internally  */

cvmeth	vNew(int type, char *file, char *resource, wind)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iDlgState = SW_SHOW;
	iType = type;
	if (file)
		iFile = gNewWithStr(String, file);
	if (cInitFunctions)
		iInitFunctions = gDeepCopy(cInitFunctions);
	iResource = resource;
	iParent = wind;
	DEMO_INC;
	iControls = gNewWithInt(IntegerDictionary, 41);
	iControlNames = gNewWithInt(StringDictionary, 41);
	iTextBrush = gGetTextBrush(Application);
	iBackBrush = gGetBackBrush(Application);
	setup_handlers(obj, iv);
	DEMO_CHK_MAX;
	if (iType == MODELESS)
		iAutoDispose = 1;
	iVPos = iHPos = -1;
	iZOrder = HWND_TOP;
	return obj;
}

cmeth	gNewDialog(WORD resource, wind)
{
	object	obj = gNewDialogStr(self, (char *) MAKEINTRESOURCE(resource), wind);
	ivType	*iv;
	if (obj) {
		iv = ivPtr(obj);
		iID = resource;
		iDlgState = SW_SHOW;
	}
	
	return obj;
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
	eliminate_killfocus_functions(iControls);
	if (iFunctions) {
		object	fun;
		fun = iFunctions;
		iFunctions = NULL;
		gExecuteFunctionsObjInt(fun, self, -1);
		gDeepDispose(fun);
		if (!IsObj(self))
			return NULL;
	}
	if (iTabFunctions) {
		object	fun;
		fun = iTabFunctions;
		iTabFunctions = NULL;
		gExecuteFunctionsObjInt(fun, self, -1);
		gDeepDispose(fun);
		if (!IsObj(self))
			return NULL;
	}
	if (iType == MODELESS  &&  iHDlg)  {
		DestroyWindow(iHDlg);
		HC_DELETE(DIALOG_HANDLE_CACHE, iHDlg);
		gRemoveModeless(MessageDispatcher, iHDlg);
	}
	pUndockAll(self);
	gDeepDispose(iControls);
	gDispose(iControlNames);
	if (iActivateFunctions)
		gDeepDispose(iActivateFunctions);
	if (iTitle)
		gDispose(iTitle);
	if (iBackBrush)
		gDispose(iBackBrush);
	if (iTextBrush)
		gDispose(iTextBrush);
	gDeepDispose(iMessageHandlers);
	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);
	if (iIcon)
		gDispose(iIcon);
	if (iCursor)
		gDispose(iCursor);
	if (iRegistered)  {
		char	*cname = gStringValue(iClassName);
		object	sa = gFindStr(cRegisteredClasses, cname);
		object	valobj = gValue(sa);
		int	val = gShortValue(valobj);
		if (val == 1)  {
			UnregisterClass(cname, gInstance(Application));
			gDeepDisposeStr(cRegisteredClasses, cname);
		} else
			gChangeShortValue(valobj, val-1);
	}
	if (iClassName)
		gDispose(iClassName);
	if (iFile)
		gDispose(iFile);
	if (iTopic)
		gDispose(iTopic);
	if (iDockedChildren)
		gDispose(iDockedChildren);
	if (iDockedParents)
		gDispose(iDockedParents);
//	if (IsObj(iParent))
//		gUndock(iParent, self);
	if (iInitFunctions)
		gDeepDispose(iInitFunctions);
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gAddControl(ctlClass, UINT ctlID)
{
	object	ctl;

	ChkArgTyp(ctlClass, 2, Control);
	ctl = vNew(ctlClass, ctlID, NULL, self);
	if (!gAddInt(iControls, ctlID, ctl))
		vError(self, "Attempt to add a duplicate control id (%ld) to dialog id %ld", (long) ctlID, (long) iID);
	return ctl;
}

imeth	gAddControlStr(ctlClass, UINT ctlID, char *name)
{
	object	ctl;

	ChkArgTyp(ctlClass, 2, Control);
	ctl = vNew(ctlClass, ctlID, name, self);
	gAddInt(iControls, ctlID, ctl);
	gAddStr(iControlNames, name, ctl);
	return ctl;
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

private	imeth	int	dialogBoxParam(HINSTANCE	ins,
				       char		*name,
				       HWND		owner,
				       DLGPROC		dp,
				       LPARAM		lParam)
{
	HWND	pw = owner;
	MSG	msg;
	
	if (IsWindow(pw)) {
		while (GetWindowStyle(pw) & WS_CHILD)
			pw = GetParent(pw);
		if (pw)
			EnableWindow(pw, FALSE);
	}
#ifdef	WIN32
	if (iFile)
		iHDlg = ResourceDialogParam(gStringValue(iFile), name, owner, dp, lParam);
	else
#endif
		iHDlg = CreateDialogParam(ins, name, owner, dp, lParam);
	if (!iHDlg)
		gError(Application, "Dialog not found.");
	if (!iHDlg) {
		if (pw)
			EnableWindow(pw, TRUE);
		return -1;
	}
	ShowWindow(iHDlg, iDlgState);
	iEndDialog = 0;
	gProcessModalMessages(MessageDispatcher, &iEndDialog, iHDlg);
	if (pw)
		EnableWindow(pw, TRUE);
	DestroyWindow(iHDlg);
	gReleaseHandle(self);
	return iModalResult;
}

imeth	int	gPerform()
{
	if (iClassName)  {
		WNDCLASS  wc;
		char	*cname = gStringValue(iClassName);
		object	sa = gFindStr(cRegisteredClasses, cname);

		if (!sa)  {
			GetClassInfo((HINSTANCE)0, WC_DIALOG, &wc);
			wc.lpszClassName = cname;
			wc.lpszMenuName  = NULL;
			wc.hInstance     = gInstance(Application);
			if (iIcon)
				wc.hIcon = gHandle(iIcon);
			RegisterClass(&wc);
			gAddStr(cRegisteredClasses, cname, gNewWithInt(ShortInteger, 1));
		} else {
			object	valobj = gValue(sa);
			int	val = gShortValue(valobj);
			gChangeShortValue(valobj, val+1);
		}
		iRegistered = 1;
	}
	if (!pDialogProc)
		pDialogProc = MakeProcInstance((FARPROC) DialogProc, gInstance(Application));
	if (iType == MODAL)
		return dialogBoxParam(self, gInstance(Application), iResource, iParent ? gHandle(iParent) : (HWND) 0, (DLGPROC) pDialogProc, (LPARAM) self);
	if (!iHDlg)  {
#ifdef	WIN32
		if (iFile)
			iHDlg = ResourceDialogParam(gStringValue(iFile), iResource, iParent ? gHandle(iParent) : (HWND) 0, (DLGPROC) pDialogProc, (LPARAM) self);
		else
#endif
			iHDlg = CreateDialogParam(gInstance(Application), iResource, iParent ? gHandle(iParent) : (HWND) 0, (DLGPROC) pDialogProc, (LPARAM) self);
		if (!iHDlg)
			gError(Application, "Dialog not found.");
		gAddModeless(MessageDispatcher, iHDlg, self);
	}
	ShowWindow(iHDlg, iDlgState);
	return 0;
}

imeth	gReleaseHandle()
{
	object	seq, obj;

	for (seq=gSequence(iControls) ; obj=gNext(seq) ; )
		gReleaseHandle(gValue(obj));
	if (iHDlg) {
		HC_DELETE(DIALOG_HANDLE_CACHE, iHDlg);
		iHDlg = 0;
	}
	return self;
}	

imeth	gGetControl(UINT ctlID)
{
	return gFindValueInt(iControls, ctlID);
}

imeth	gGetControlStr(char *name)
{
	return gFindValueStr(iControlNames, name);
}

imeth	gControls()
{
	return iControlNames;
}

private	imeth	getCtl(object self, UINT ctlID)
{
	object	ctl;

	ctl = gFindValueInt(iControls, ctlID);
	if (!ctl)
		gError(self, (char *) vSprintf(String, "Control ID %d doesn't exist.", ctlID));
	return ctl;
}

imeth	gCtlValue(UINT ctlID)
{
	object	ctl;

	ctl = getCtl(self, ctlID);
	DEMO_CHK_MAX;
	return gValue(ctl);
}

imeth	gIndexValue(UINT ctlID)
{
	object	ctl;

	ctl = getCtl(self, ctlID);
	return gListIndex(ctl);
}

imeth	char	*gCtlStringValue(UINT ctlID)
{
	DEMO_CHK_MAX;
	return gStringValue(getCtl(self, ctlID));
}

imeth	short	gCtlShortValue(UINT ctlID)
{
	DEMO_CHK_MAX;
	return gShortValue(getCtl(self, ctlID));
}

imeth	unsigned short	gCtlUnsignedShortValue(UINT ctlID)
{
	DEMO_CHK_MAX;
	return gUnsignedShortValue(getCtl(self, ctlID));
}

imeth	long	gCtlLongValue(UINT ctlID)
{
	DEMO_CHK_MAX;
	return gLongValue(getCtl(self, ctlID));
}

imeth	double	gCtlDoubleValue(UINT ctlID)
{
	DEMO_CHK_MAX;
	return gDoubleValue(getCtl(self, ctlID));
}

imeth	int	gInDialog()
{
	return iInDlg;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHDlg;
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

imeth	int	gQuery : query(char *title, char *msg, UINT options)
{
	HANDLE	h;
	int	r = 0;

	if (iHDlg)
		h = iHDlg;
	else if (iParent)
		h = gHandle(iParent);
	else
		h = 0;
	r = MessageBox(h, msg, title, options);  
	return r;
}	

imeth	gMessage(char *msg)
{
	query(self, "Message Window", msg, MB_OK);
	return self;
}	

imeth	gMessageWithTopic(char *msg, char *topic)
{
	char	*p = gSetTopic(HelpSystem, topic);
	gMessage(self, msg);
	gSetTopic(HelpSystem, p);
	return self;
}	

imeth	int	gAutoDispose(int ad)
{
	int	pad = iAutoDispose;
	iAutoDispose = ad;
	return pad;
}

imeth	gAutoDisposeTag()
{
	iAutoDisposeTag = 1;
	return self;
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

	if (cInitFunctions)
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

imeth	gTabCompletionFunction(object tab, int (*fun)())
{
	iTab = tab;
	if (iTabFunctions)
		gDeepDispose(iTabFunctions);
	
	iTabFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(iTabFunctions, fun);
	return self;
}

imeth	gAddTabCompletionFunctionBefore(object tab, int (*fun)())
{
	iTab = tab;
	if (!iTabFunctions)
		iTabFunctions = gNew(FunctionList);

	gAddFunctionBefore(iTabFunctions, fun);
	
	return self;
}

imeth	gAddTabCompletionFunctionAfter(object tab, int (*fun)())
{
	iTab = tab;
	if (!iTabFunctions)
		iTabFunctions = gNew(FunctionList);

	gAddFunctionAfter(iTabFunctions, fun);
	
	return self;
}

imeth	gTabControl()
{
	return iTab;
}

imeth	gSetResult(res)
{
	ChkArgNul(res, 2);
	iResult = res;
	return self;
}

imeth	gGetParent()
{
	return iParent;
}

imeth	gTextBrush(brush)
{
	ChkArgTyp(brush, 2, Brush);
	if (iTextBrush)
		gDispose(iTextBrush);
	iTextBrush = brush;
	return self;
}

imeth	gBackBrush(brush)
{
	ChkArgTyp(brush, 2, Brush);
	if (iBackBrush)
		gDispose(iBackBrush);
	iBackBrush = brush;
	return self;
}

imeth	gGetTextBrush()
{
	return iTextBrush;
}

imeth	gGetBackBrush()
{
	return iBackBrush;
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

typedef	long (*lfun)();

imeth	gAddDlgHandlerAfter(unsigned msg, BOOL (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 2);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerAfter(h, (lfun) fun);
	return self;
}

imeth	gAddDlgHandlerBefore(unsigned msg, BOOL (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 2);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerBefore(h, (lfun) fun);
	return self;
}

imeth	gSetTitle(char *title)
{
	if (iTitle)
		gDispose(iTitle);
	iTitle = gNewWithStr(String, title);
	if (iInDlg  &&  iHDlg)
		SetWindowText(iHDlg, gStringValue(iTitle));
	return self;
}

static	void	init_class()
{
	gDontCollect(CLASS);
	cRegisteredClasses = gNew(StringDictionary);
}

imeth	gUse(obj)
{
	ChkArg(obj, 2);
	if (gIsKindOf(obj, Icon))  {
		if (iIcon)
			gDispose(iIcon);
		iIcon = obj;
	}  else
		gError(self, "Incorrect 2nd argument to Use::Dialog");
	return obj;
}

imeth	gSetClass(char *class)
{
	iClassName = gNewWithStr(String, class);
	return self;
}

imeth	gMoveToTop()
{
	if (iHDlg)
		BringWindowToTop(iHDlg);
	return self;
}

imeth	gSetState(int state)
{
	iDlgState = state;
	if (iHDlg)
		ShowWindow(iHDlg, state);
	return self;
}

imeth	gSetFocus()
{
	if (iHDlg)
		SetFocus(iHDlg);
	return self;
}

imeth	gSetPosition(int y, int x)
{
	int	hp = iHPos, vp = iVPos;
	gScaleToPixels(Application, &y, &x, NULL);
	iVPos = y;
	iHPos = x;
	if (iHDlg)
		SetWindowPos(iHDlg, HWND_TOP, iHPos, iVPos, 0, 0, (SWP_NOSIZE | SWP_NOZORDER));
	updateDockedWindows(iv, hp, vp);
	return self;
}

static	void	updateDockedWindows(ivType *iv, int hp, int vp)
{
	if (iDockedChildren) {
		int	pm = gSetScalingMode(Application, SM_PIXELS);
		object	seq, obj;
		int	x, y, dx, dy;
		RECT	rect;

		GetWindowRect(iHDlg, &rect);
		dx = rect.left - hp;
		dy = rect.top - vp;
		for (seq=gSequence(iDockedChildren) ; obj = gNext(seq) ; ) {
			gGetPosition(obj, &y, &x);
			gSetPosition(obj, y + dy, x + dx);
		}
		gSetScalingMode(Application, pm);
	}
}

static	void	update_position(ivType *iv)
{
	if (iHDlg  &&  !IsIconic(iHDlg)) {
		RECT	rect;
		GetWindowRect(iHDlg, &rect);
		iHPos = rect.left;
		iVPos = rect.top;
		iHSize = rect.right - rect.left;
		iVSize = rect.bottom - rect.top;
	}
}

imeth	gGetPosition(int *y, int *x)
{
	update_position(iv);
	*y = iVPos;
	*x = iHPos;
	gScaleToCurrentMode(Application, y, x, NULL);
	return self;
}

imeth	gGetSize(int *y, int *x)
{
	update_position(iv);
	*y = iVSize;
	*x = iHSize;
	gScaleToCurrentMode(Application, y, x, NULL);
	return self;
}

imeth	int	gIsIconic()
{
	return iHDlg && IsIconic(iHDlg) == TRUE ? 1 : 0;
}

imeth	int	gIsMaximized()
{
	return iHDlg && IsZoomed(iHDlg) == TRUE ? 1 : 0;
}

imeth	int	gIsNormal()
{
	return iHDlg && IsZoomed(iHDlg) == FALSE && IsIconic(iHDlg) == FALSE ? 1 : 0;
}

imeth	gSetZOrder(HWND m)
{
	iZOrder = m;
	if (iInDlg  &&  iHDlg)
		SetWindowPos(iHDlg, iZOrder, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE));
	return self;
}

imeth	object	gUseDefault()
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		gUseDefault(gValue(i));
	return self;
}

imeth	object	gDisableAll()
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		gDisable(gValue(i));
	return self;
}

imeth	object	gEnableAll()
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		gEnable(gValue(i));
	return self;
}

imeth	object	gUpdate()
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		gUpdate(gValue(i));
	return self;
}

imeth	gPressButton(unsigned id)
{
	BOOL	r;
	object	btn = gGetControl(self, id);
	if (!btn  ||  !iInDlg  ||  !iHDlg)
		return NULL;
#ifdef	WIN32
	r = PostMessage(iHDlg, WM_COMMAND, MAKEWPARAM((unsigned short)id, BN_CLICKED), (LPARAM) gHandle(btn));
#else
	r = PostMessage(iHDlg, WM_COMMAND, (WPARAM) id, MAKELPARAM((HWND)gHandle(btn), BN_CLICKED));
#endif
	return r ? self : NULL;
}

imeth	gSetCursor(cursor)
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		gSetCursor(gValue(i), cursor ? gCopy(cursor) : cursor);
	if (iCursor)
		gDispose(iCursor);
	iCursor = cursor;
	if (iHDlg)
		if (iCursor)
			SetCursor(gHandle(iCursor));
		else 
			SetCursor(gHandle(gGetCursor(Application)));
	return self;
}

imeth	gWaitCursor(int wait)
{
	if (wait) {
		if (!iWait++)
			gSetCursor(self, gLoadSys(SystemCursor, IDC_WAIT));
	} else
		if (--iWait <= 0) {
			gSetCursor(self, NULL);
			iWait = 0;
		}
	return self;
}

imeth	gEnable()
{
	object	ss, i;

	if (iHDlg)
		EnableWindow(iHDlg, TRUE);

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		if (!strcmp(gName(ClassOf(gValue(i))), "TabControl"))
			gEnable(gValue(i));
	
	return self;
}

imeth	gDisable()
{
	object	ss, i;

	for (ss = gSequence(iControls) ; i = gNext(ss) ; )
		if (!strcmp(gName(ClassOf(gValue(i))), "TabControl"))
			gDisable(gValue(i));
	
	if (iHDlg)
		EnableWindow(iHDlg, FALSE);
	return self;
}

imeth	unsigned     gGetResourceID()
{
	return iID;
}

imeth	unsigned     gResourceIDFromFileResource()
{
	return (unsigned) (iResource ? (WORD) ((DWORD) iResource) : 0);
}

imeth	gDisableObject(obj)	//  needed by tasklist code
{
	iDisableObject = obj;
	return self;
}

cmeth	ifun	gSetAccessModeFunction(ifun f)
{
	ifun	org = cAccessMode;
	cAccessMode = f;
	return org;
}

imeth	gSetTask(task)
{
	return iTask = task;
}

imeth	gGetTask()
{
	return iTask;
}

imeth	gCenter()
{
	iCenterDialog = 1;

	if (iInDlg)
		centerDialog(self);
	return self;
}

imeth	int	gTabInit(object dlg)      //  this is needed.
{
	gDoesNotImplement(CLASS, Generic(gTabInit));
	return 0;
}

imeth	char	*gDialogFileName()
{
	return iFile ? gStringValue(iFile) : "";
}
	
imeth	ifun gControlSecurityFunction()
{
	return cAccessMode;
}






