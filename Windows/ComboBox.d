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

defclass  ComboBox : Control  {
	HWND	iHCtl;		/*  handle to control	  	*/
	HWND	iEdit;		/*  handle to edit control	*/
	WNDPROC	iOrgEdtProc;	/*  original window proc for edit control  */
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	iWindowControl; /*  1=window control		*/
	iValue;			/*  field value (offset)	*/
	iTextValue;		/*  field value (text)		*/
	iDefault;		/*  default value		*/
	short   iComboboxStyle; //  Yanghui
	int	iDisposeValue;	/*  1=auto dispose of iTextValue*/

	iSI;			/*  ODBC StatementInfo		*/

	int	(*iAcf)();	/*  aux checking function	*/
	iOptions;		/*  linked list of items	*/
	iAssoc;			/*  linked list of associated objects  */
	int	iRequired;	/*  1=selection required	*/
	int	iAlphabetize;	/*  1=alphabetize items		*/
	int	(*iCmpFun)();	/*  alpha compare function	*/
	int	(*iDCFun)();	/*  function  - double clicks	*/
	int	(*iChgFun)();	/*  selection change function	*/
	int	iFindMode;	/*  1=exact, 2=prefix, 3=best   */
	iEdtCursor;		/*  cursor associated with edit part  */
	
	iChoiceXPath;
	iListKeyXPath;
	iDataXPath;
	iTextXPath;
	
	iMessageHandlers;	/*  IntDict of message handlers for edit ctl */

	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/
	
	int	iAutoDispose;	/*  Auto dispose of associated objects on close flag	*/
};


#include <ctype.h>
#include <string.h>


static	int	find_string(ivType *iv, char *choice);
static	int	makeValue(ivType *iv);
static	int	cmpfun(char *s1, object o1, char *s2, object o2);

#ifdef	WIN32
#define DFLT	(iOrgEdtProc != EdtWindowProc) ? CallWindowProc((WNDPROC) iOrgEdtProc, hwnd, mMsg, wParam, lParam) : 0L
#else
#define DFLT	(iOrgEdtProc != EdtWindowProc) ? CallWindowProc((FARPROC) iOrgEdtProc, hwnd, mMsg, wParam, lParam) : 0L
#endif

#define	ACTIVE	(iHCtl  &&  iDlg  &&  gInDialog(iDlg))

/*  Window procedure for edit control portion of combobox only  */

static
LRESULT	CALLBACK  EdtWindowProc(HWND	hwnd, 
				UINT	mMsg, 
				WPARAM	wParam, 
				LPARAM	lParam)
{
	object	h;
	HC_VARS;

	HC_UPDATE(WINDOW_HANDLE_CACHE, hwnd);

	if (self)  
		h = gFindValueInt(iMessageHandlers, mMsg);
	if (self  &&  h)
		return gProcessWindowMsg(h, self, hwnd, mMsg, wParam, lParam, iOrgEdtProc);
	else if (self)
		return DFLT;
	return 0L;
}

private	imeth	long	process_wm_char(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;

	if (wParam == '\t') {
		object	next = GetKeyState(VK_SHIFT) & 0x80 ? gPreviousControl(iDlg, self) : gNextControl(iDlg, self);
		if (next)
			SetFocus(gHandle(next));
			
		return 0L;
	}
	

	
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	if (iTopic  &&  iHCtl == iEdit)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	return 0L;
}

private	imeth	long	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{

	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
	{
		if (wParam==VK_F4) //keeps the list from opening and then instantly closing
			return 0L;
		return gCallDefaultProc(self, mMsg, wParam, lParam);
	}
}

private	imeth	long	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iTopic  &&  iHCtl == iEdit)
		gSetTopic(HelpSystem, iPrevTopic);
	makeValue(iv);
/*
  We don't want the loose focus function to execute if the user clicks on CANCEL.  We test
  this by checking who is getting the focus next.  This test doesn't work as desired if
  the CANCEL button immediatly follows the control in question.  In that case if the user
  tabs out of the control the killfocus function will not be executed as desired.  The fix
  for this is to never make the CANCEL follow, in tab order terms, a control in which you
  have a killfocus function attached.
*/
	if (!wParam  ||  GetDlgCtrlID((HANDLE) wParam) != IDCANCEL)
		if ((HWND) wParam != iEdit  &&  gExecuteSetFunctions(self, iDlg))
			PostMessage(gHandle(iDlg), WM_NEXTDLGCTL, (WPARAM) iHCtl, (LPARAM) TRUE);
	return 0L;
}

private	imeth	long	process_wm_color(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	object	bb, tb;

#if 0
/*  On rare occations Windows 95 passes a menu handle to a window procedure.??  */
/*  The check below is designed to handle that condition.  */
	if (!gIsKindOf(self, CLASS))
		return 0L;
#endif
	bb = gGetBackBrush(self);
	tb = gGetTextBrush(self);
	if (!bb  ||  !tb) {
		bb = gNewSolidBrush(SolidBrush,255,255,255); //gGetBackBrush(iDlg);
		tb = gGetTextBrush(iDlg);
	}
	SetBkColor((HDC) wParam, gColor(bb));
	SetTextColor((HDC) wParam, gColor(tb));
	return (long) gHandle(bb);    /*  ???  */
}

private	imeth	long	process_edt_char(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{

	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	if (wParam == '\t') {
		object	next = GetKeyState(VK_SHIFT) & 0x80 ? gPreviousControl(iDlg, self) : gNextControl(iDlg, self);
		if (next)
			SetFocus(gHandle(next));
		return 0L;
	}
	
	return DFLT;
}

private	imeth	long	process_edt_setfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	return 0L;
}

private	imeth	long	process_edt_killfocus(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	if (iTopic)
		gSetTopic(HelpSystem, iPrevTopic);
	makeValue(iv);
/*
  We don't want the loose focus function to execute if the user clicks on CANCEL.  We test
  this by checking who is getting the focus next.  This test doesn't work as desired if
  the CANCEL button immediatly follows the control in question.  In that case if the user
  tabs out of the control the killfocus function will not be executed as desired.  The fix
  for this is to never make the CANCEL follow, in tab order terms, a control in which you
  have a killfocus function attached.
*/
	if (!wParam  ||  GetDlgCtrlID((HANDLE) wParam) != IDCANCEL)
		if ((HWND) wParam != iHCtl  &&  gExecuteSetFunctions(self, iDlg))
			PostMessage(gHandle(iDlg), WM_NEXTDLGCTL, (WPARAM) iEdit, (LPARAM) TRUE);
	return 0L;
}

private	imeth	long	process_edt_setcursor(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	if (iEdtCursor)
		SetCursor(gHandle(iEdtCursor));
	return 0L;
}

private	imeth	long	process_wm_rbuttondown(object	self, 
					       HWND	hwnd, 
					       UINT	mMsg, 
					       WPARAM	wParam, 
					       LPARAM	lParam)
{
	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
		return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_rbuttonup(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
		return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_lbuttondblclk(object	self, 
						 HWND	hwnd, 
						 UINT	mMsg, 
						 WPARAM	wParam, 
						 LPARAM	lParam)
{
	iDlg;
	makeValue(iv);

	gPerform(self);

	return 0L;
}

private	imeth	long	process_wm_mousemove(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	return 0L;
}

cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iOptions = gNew(LinkList);
	iAssoc = gNew(LinkList);
	iAutoDispose = 1;
	iFindMode = 3;
	iDisposeValue = 1;
	iCmpFun = cmpfun;
	iDlg = dlg;

	/*  Init message handlers  */

#ifdef	WIN32
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORBTN, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORDLG, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOREDIT, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORLISTBOX, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORMSGBOX, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSCROLLBAR, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSTATIC, process_wm_color);
#ifndef unix
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORBTN, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORDLG, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOREDIT, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORLISTBOX, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORMSGBOX, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSCROLLBAR, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSTATIC, 0);
#endif
#else
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOR, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOR, 0);  /*  no auto default processing  */
#endif
	
	//DCM
	if(gIsKindOf(iDlg,Window))
	{
		gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	}

	
	

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	if (ClassOf(dlg) == ToolBar)
		gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);


	/*  Init edit control handlers  */

	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	gAddEdtHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_edt_setfocus);

	gAddEdtHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_edt_killfocus);

	gAddEdtHandlerAfter(obj, (unsigned) WM_SETCURSOR, process_edt_setcursor);

	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "combobox", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iWindowControl = 1;
	iOptions = gNew(LinkList);
	iAssoc = gNew(LinkList);
	iAutoDispose = 1;
	iFindMode = 3;
	iDisposeValue = 1;
	iCmpFun = cmpfun;

	/*  Init message handlers  */

#ifdef	WIN32
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORBTN, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORDLG, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOREDIT, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORLISTBOX, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORMSGBOX, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSCROLLBAR, process_wm_color);
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLORSTATIC, process_wm_color);

	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORBTN, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORDLG, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOREDIT, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORLISTBOX, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORMSGBOX, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSCROLLBAR, 0);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLORSTATIC, 0);
#else
	gAddHandlerAfter(obj, (unsigned) WM_CTLCOLOR, process_wm_color);
	gDefaultProcessingMode(obj, (unsigned) WM_CTLCOLOR, 0);  /*  no auto default processing  */
#endif

	//DCM
	if(gIsKindOf(iDlg,Window))
	{
		gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	}
	
	


	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	if (ClassOf(parent) == ToolBar)
		gAddHandlerAfter(obj, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	
	 
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

//	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	
	/*  Init edit control handlers  */

	iMessageHandlers = gNewWithInt(IntegerDictionary, 27);

	gAddEdtHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_edt_setfocus);

	gAddEdtHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_edt_killfocus);

	gAddEdtHandlerAfter(obj, (unsigned) WM_SETCURSOR, process_edt_setcursor);

	//DCM uncommented and changed method to prevent program exit
	gAddEdtHandlerAfter(obj, (unsigned) WM_CHAR, process_edt_char);
	gDefaultEdtProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */
 
	gAddEdtHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);
	gDefaultEdtProcessingMode(obj, (unsigned) WM_RBUTTONDOWN, 0);  /*  no auto default processing  */

	gAddEdtHandlerAfter(obj, (unsigned) WM_RBUTTONUP, process_wm_rbuttonup);
	gDefaultEdtProcessingMode(obj, (unsigned) WM_RBUTTONUP, 0);  /*  no auto default processing  */

	gAddEdtHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);

	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

static	void	initList(ivType *iv)
{
	object	seq, obj;
	
	SendMessage(iHCtl, WM_SETREDRAW, FALSE, 0L);
	for (seq=gSequence(iOptions) ; obj=gNext(seq) ; )
		SendMessage(iHCtl, CB_INSERTSTRING, -1, (LPARAM) (LPSTR) gStringValue(gValue(obj)));
	SendMessage(iHCtl, WM_SETREDRAW, TRUE, 0L);
}

typedef	struct {
	int	n;
	HWND	v[5];
}	WindVect;

static	BOOL CALLBACK EnumChildProc(HWND hwnd, LPARAM lParam)
{
	char	name[80];
	WindVect *v = (WindVect *) lParam;
	if (v->n < (sizeof(v->v)/sizeof(*v->v))) {
		GetClassName(hwnd, name, sizeof(name)-1);
		if (!stricmp(name, "edit")) {
			v->v[v->n++] = hwnd;
			return FALSE;
		}
		return TRUE;
	} else
		return FALSE;
}

imeth	int	gShow()
{
	int	idx = -1;
	char	*str;
	POINT	pt;
	WindVect v;
	object	font = gGetFont(self);

	if (iHCtl)
		return 0;
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

#if 0   //  doesn't work in Windows 95  ???
	/*  get handle to edit control  */
	pt.x = pt.y = 1;
	iEdit = ChildWindowFromPoint(iHCtl, pt);
#else
	v.n = 0;
	EnumChildWindows(iHCtl, EnumChildProc, (LPARAM) &v);
	if (v.n)
		iEdit = v.v[0];
#endif

	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	
	if (iEdit  &&  iEdit != iHCtl)  {
		HC_NEW(WINDOW_HANDLE_CACHE, iEdit, self);
		iOrgEdtProc = SubclassWindow(iEdit, EdtWindowProc);

		if (gIsKindOf(iDlg, Window)) {
			RECT	r;
			int	pm = gSetScalingMode(Application, SM_PIXELS);

			gGetPosition(self, &r.top, &r.left);
			gGetSize(self, &r.bottom, &r.right);
			r.bottom += r.top;
			r.right += r.left;

			InvalidateRect(gHandle(iDlg), &r, FALSE);
		
			gSetScalingMode(Application, pm);
		}
	}
	
	initList(iv);
	if (iValue)  {
		idx = gShortValue(iValue);
		if (idx >= 0)
			SendMessage(iHCtl, CB_SETCURSEL, idx, 0L);
	}
	if (idx < 0  &&  iTextValue)  {
		str = gStringValue(iTextValue);
		idx = find_string(iv, str);
		if (idx >= 0)  {
			SendMessage(iHCtl, CB_SETCURSEL, idx, 0L);
			if (iValue)
				gChangeShortValue(iValue, idx);
			else
				iValue = gNewWithInt(ShortInteger, idx);
		}  else  {
			SendMessage(iHCtl, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
			if (iValue)
				iValue = gDispose(iValue);
		}
	}

	if (iSI)
		gUpdate(iSI);
	gInitialize(super, (HWND)0, NULL);
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	int	idx = -1;
	char	*str;
	POINT	pt;
	WindVect v;
	object	fobj = gGetFont(self);

	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "ComboBox control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}

#if 0   //  doesn't work in Windows 95  ???
	/*  get handle to edit control  */
	pt.x = pt.y = 1;
	iEdit = ChildWindowFromPoint(iHCtl, pt);
#else
#if 1
	v.n = 0;
	EnumChildWindows(iHCtl, EnumChildProc, (LPARAM) &v);
	if (v.n)
		iEdit = v.v[0];
#else
	iEdit = GetWindow(iHCtl, GW_CHILD);
#endif
#endif

	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);

	if (iEdit  &&  iEdit != iHCtl)  {
		HC_NEW(WINDOW_HANDLE_CACHE, iEdit, self);
		iOrgEdtProc = SubclassWindow(iEdit, EdtWindowProc);
	}

	if (fobj)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(fobj), (LPARAM) 0);

	initList(iv);
	if (iValue)  {
		idx = gShortValue(iValue);
		if (idx >= 0)
			SendMessage(iHCtl, CB_SETCURSEL, idx, 0L);
	}
	if (idx < 0  &&  iTextValue)  {
		str = gStringValue(iTextValue);
		idx = find_string(iv, str);
		if (idx >= 0)  {
			SendMessage(iHCtl, CB_SETCURSEL, idx, 0L);
			if (iValue)
				gChangeShortValue(iValue, idx);
			else
				iValue = gNewWithInt(ShortInteger, idx);
		}  else  {
			SendMessage(iHCtl, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
			if (iValue)
				iValue = gDispose(iValue);
		}
	}
	if (iSI)
		gUpdate(iSI);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	if (iValue)
		gDispose(iValue);
	if (iTextValue  &&  iDisposeValue)
		gDispose(iTextValue);
	if (iDefault)
		gDispose(iDefault);
	if (iEdtCursor)
		gDispose(iEdtCursor);
	gReleaseHandle(self);
	gDeepDispose(iOptions);
	if (iAutoDispose)
		gDeepDispose(iAssoc);
	else
		gDispose(iAssoc);
	gDeepDispose(iMessageHandlers);
	if (IsObj((object) iChgFun))
		gDispose((object) iChgFun);
	if (IsObj((object) iDCFun))
		gDispose((object) iDCFun);
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	if (iTopic)
		gDispose(iTopic);
	return gDispose(super);
}

imeth	gReleaseHandle()
{
	if (iHCtl)
		HC_DELETE(WINDOW_HANDLE_CACHE, iHCtl);
	if (iEdit  &&  iHCtl != iEdit)
		HC_DELETE(WINDOW_HANDLE_CACHE, iEdit);
	iHCtl = iEdit = 0;
	return self;
}	

static	int	makeValue(ivType *iv)
{
	char	*buf = gGetBuf(Application);
	int	val, r;

	val = SendMessage(iHCtl, CB_GETCURSEL, 0, 0L);
	if (iValue)
		gChangeShortValue(iValue, val);
	else
		iValue = gNewWithInt(ShortInteger, val);

	if (val >= 0)
		SendMessage(iHCtl, CB_GETLBTEXT, val, (LPARAM) (LPSTR) buf);
	else  if (CBS_DROPDOWNLIST == (CBS_DROPDOWNLIST & GetWindowLong(iHCtl, GWL_STYLE)))
		*buf = '\0';
	else {
		r = SendMessage(iHCtl, WM_GETTEXT, gBufSize(Application), (LPARAM) (LPSTR) buf);
		if (r < 0)
			*buf = '\0';
	}
	if (iTextValue)
		gChangeStrValue(iTextValue, buf);
	else
		iTextValue = gNewWithStr(String, buf);
	if (iSI)
		gUpdate(iSI);
	return val;
}

imeth	int	gCheckValue()
{
	char	*buf = gGetBuf(Application);
	int	val;

	val = gShortValue(self);
	if (iRequired  &&  val == -1)  {
//		MessageBox(gHandle(iDlg), "No Item Selected.", "Error Message Window", MB_OK);  
		gErrorMessage(Application, "No Item Selected.");
		SetFocus(iHCtl);
		return 1;	/*  error  */
	}
	if (iAcf) {
		int	r = 0;

		if (SchemeClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == String) {
			char	cmd[100], ns[80];
			object	ret;
			int	res;
			sprintf(cmd, "(%s (int->object %ld) (int->object %ld))",
				gFunctionName(SchemeClassSurrogate, (object)iAcf),
				(long) self, (long) iValue);
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object)iAcf, ns), 
						  cmd);
			if (IsObj(ret)) {
				if (r = ClassOf(ret) == String)
					strcpy(buf, gStringValue(ret));
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == JavaScriptString) {
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"))", gStringValue((object)iAcf), (long) self, (long) iValue);
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (r = ClassOf(ret) == String)
					strcpy(buf, gStringValue(ret));
				gDispose(ret);
			}
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == JavaCallbackClassSurrogate) {
			object msg = gPerformJavaCheckValueCallback((object)iAcf, self, iValue);
			if (msg) {
				r = 1;
				strcpy(buf, gStringValue(msg));
				gDispose(msg);
			}
		} else
			r = iAcf(self, iValue, buf);
		if (r) {
			if (*buf)
//				MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
				gErrorMessage(Application, buf);
			SetFocus(iHCtl);
			return 1;	/*  error  */
		}
	}
	return 0;
}

imeth	gValue : Value ()
{
	if (ACTIVE)
		makeValue(iv);
	if (!iValue)
		iValue = gNewWithInt(ShortInteger, -1);
	if (!iTextValue)
		iTextValue = gNew(String);
	return iTextValue;
}

imeth	gListIndex : ListIndex ()
{
	if (ACTIVE)
		makeValue(iv);
	if (!iValue)
		iValue = gNewWithInt(ShortInteger, -1);
	if (!iTextValue)
		iTextValue = gNew(String);
	return iValue;
}

imeth	short	gShortValue()
{
	return gShortValue(ListIndex(self));
}

imeth	char	*gStringValue()
{
	return gStringValue(Value(self));
}

imeth	gCheckFunction(int (*fun)())
{
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	iAcf = fun;
	return self;
}

imeth	gSetValue : SetValue (val)
{
	char	*buf = gGetBuf(Application), *str;
	int	r, i;

	ChkArg(val, 2);
	if (ClassOf(val) == String)  {
		if (iTextValue)
			gChangeValue(iTextValue, val);
		else
			iTextValue = gCopy(val);
		if (iSI)
			gUpdate(iSI);
		if (ACTIVE)  {
			str = gStringValue(val);
			r = find_string(iv, str);
			SendMessage(iHCtl, CB_SETCURSEL, r, 0L);
			if (r >= 0)  {
				if (iValue)
					gChangeShortValue(iValue, r);
				else
					iValue = gNewWithInt(ShortInteger, r);
			}  else  {
				SendMessage(iHCtl, WM_SETTEXT, 0, (LPARAM) (LPSTR) str);
				if (iValue)
					iValue = gDispose(iValue);
			}
		} else if (iValue)
			iValue = gDispose(iValue);
	} else {
		if (iValue)
			gChangeValue(iValue, val);
		else
			iValue = gCopy(val);
		if (ACTIVE)  {
			i = gShortValue(val);
			r = SendMessage(iHCtl, CB_SETCURSEL, i, 0L);
			if (i >= 0  &&  r >= 0)  {
				SendMessage(iHCtl, CB_GETLBTEXT, i, (LPARAM) (LPSTR) buf);
				if (iTextValue)
					gChangeStrValue(iTextValue, buf);
				else
					iTextValue = gNewWithStr(String, buf);
 			} else if (iTextValue)
				gChangeStrValue(iTextValue, "");
		} else if (iTextValue)
			gChangeStrValue(iTextValue, "");
		if (iSI)
			gUpdate(iSI);
	}
	return self;
}

imeth	gUpdate : update ()
{
	if (iTextValue)
		SetValue(self, iTextValue);
	else if (iValue)
		SetValue(self, iValue);
	return self;
}

imeth	gSetStringValue(char *val)
{
	object	t;
	SetValue(self, t=gNewWithStr(String, val));
	gDispose(t);
	return self;
}

imeth	gSetShortValue(int val)
{
	object	t;
	SetValue(self, t=gNewWithInt(ShortInteger, val));
	gDispose(t);
	return self;
}

static	void	add_alpha(ivType *iv, object opt, object obj)
{
	object	lnk = gLast(iOptions);
	object	newlv = gNewWithObj(LinkValue, opt);
	char	*ops = gStringValue(opt);
	object	ass;
	
	if (!lnk  ||  iCmpFun(gStringValue(gValue(lnk)), gValue(gLast(iAssoc)), ops, obj) <= 0)  {
		gAddLast(iOptions, newlv);
		gAddLast(iAssoc, gNewWithObj(LinkValue, obj));
		return;
	}
	for (lnk=gFirst(iOptions), ass=gFirst(iAssoc) ; lnk ; lnk = gNext(lnk),ass=gNext(ass))
		if (iCmpFun(gStringValue(gValue(lnk)), gValue(ass), ops, obj) >= 0)  {
			gAddBefore(lnk, newlv);
			gAddBefore(ass, gNewWithObj(LinkValue, obj));
			return;
		}
	gAddLast(iOptions, newlv);
	gAddLast(iAssoc, gNewWithObj(LinkValue, obj));
}

static	int	cmpfun(char *s1, object o1, char *s2, object o2)
{
	return stricmp(s1, s2);
}

/*  do a binary search for insertion point  */

static	int	find_alpha_pos(ivType *iv, char *ops, object obj)
{
	int	num = SendMessage(iHCtl, CB_GETCOUNT, 0, (LPARAM) 0);
	int	low, high, mid, cond;
	char	*str = gGetBuf(Application);
	object	lnk;
	
	if (num <= 0)
		return -1;

	//  check for simple append first
	lnk = gLast(iOptions);
	if (!lnk  ||  iCmpFun(gStringValue(gValue(lnk)), gValue(gLast(iAssoc)), ops, obj) <= 0)
		return -1;
	
	low = 0;
	high = num - 1;
	while (low <= high)  {
		mid = (low + high) / 2;
		SendMessage(iHCtl, CB_GETLBTEXT, (WPARAM) mid, (LPARAM) str);
		cond = iCmpFun(ops, obj, str, gValue(gNth(iAssoc, mid+1)));
		if (cond < 0)
			high = mid - 1;
		else if (cond > 0)
			low = mid + 1;
		else if (low == high)
			return mid;
		else
			high = mid;
	}
	return low;
}

imeth	gInsertOptionWithObj : iowo (int pos, char *ops, obj)
{
	object	opt;

	if (IsObj((object) ops))  {
		opt = (object) ops;
		ops = gStringValue(opt);
	} else
		opt = gNewWithStr(String, ops);
	gInsertObjAt(iOptions, gNewWithObj(LinkValue, opt), pos);
	gInsertObjAt(iAssoc, gNewWithObj(LinkValue, obj), pos);
	if (ACTIVE)
		SendMessage(iHCtl, CB_INSERTSTRING, (WPARAM) pos, (LPARAM) (LPSTR) ops);
	return self;
}

imeth	gAddOptionAt(int pos, char *ops)
{
	return iowo(self, pos, ops, NULL);
}

imeth	gAddOptionWithObj : aowo (char *ops, obj)
{
	object	opt;
	WPARAM	pos;
	if (ACTIVE)  {
		if (IsObj((object) ops))  {
			opt = (object) ops;
			ops = gStringValue(opt);
		} else
			opt = gNewWithStr(String, ops);
		pos = iAlphabetize ? find_alpha_pos(iv, ops, obj) : -1;
		gInsertObjAt(iOptions, gNewWithObj(LinkValue, opt), pos);
		gInsertObjAt(iAssoc, gNewWithObj(LinkValue, obj), pos);
		SendMessage(iHCtl, CB_INSERTSTRING, pos, (LPARAM) (LPSTR) ops);
	}  else  {
		if (!IsObj((object) ops))
			opt = gNewWithStr(String, ops);
		else
			opt = (object) ops;
		if (iAlphabetize)
			add_alpha(iv, opt, obj);
		else {
			gAddLast(iOptions, gNewWithObj(LinkValue, opt));
			gAddLast(iAssoc, gNewWithObj(LinkValue, obj));
		}
	}
	return self;
}

imeth	gAddOption(char *ops)
{
	return aowo(self, ops, NULL);
}

imeth	gRequired(int req)
{
	iRequired = req;
	return self;
}

imeth	gAttach(result)
{
	ChkArg(result, 2);
	if (iTextValue  &&  iDisposeValue)
		gDispose(iTextValue);
	iTextValue = result;
	iDisposeValue = 0;
	return update(self);
}

imeth	gUnattach()
{
	if (iDisposeValue  ||  !iTextValue)
		return self;
	iTextValue = gCopy(iTextValue);
	iDisposeValue = 1;
	return self;
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

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHCtl;
}

imeth	HANDLE	gEditHandle()
{
	return (HANDLE) iEdit;
}

imeth	gAddEdtHandlerAfter(unsigned msg, long (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 3);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerAfter(h, fun);
	return self;
}

imeth	gAddEdtHandlerBefore(unsigned msg, long (*fun)())
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 3);
		gAddInt(iMessageHandlers, msg, h);
	}
	gAddWindowHandlerBefore(h, fun);
	return self;
}

imeth	gDefaultEdtProcessingMode(unsigned msg, int mode)
{
	object	h;
	h = gFindValueInt(iMessageHandlers, msg);
	if (!h)  {
		h = vNew(MessageHandler, 3);
		gAddInt(iMessageHandlers, msg, h);
	}
	gSetDefaultProcessingMode(h, mode);
	return self;
}

imeth	gRemoveInt : remove_idx (int idx)
{
	LRESULT	r;
	object	lnk;
	
	if (lnk = gNth(iOptions, idx+1))
		gDeepDispose(lnk);
	if (lnk = gNth(iAssoc, idx+1))
		if (iAutoDispose)
			gDeepDispose(lnk);
		else
			gDispose(lnk);
	if (ACTIVE)
		r = SendMessage(iHCtl, CB_DELETESTRING, (WPARAM) idx, (LPARAM) 0);
	else
		r = lnk ? 1 : -1;
	return r >= 0 ? self : NULL;
}

imeth	gRemoveStr(char *choice)
{accessIVs;
	int	idx;
	if (ACTIVE)
		idx = find_string(iv, choice);
	else
		idx = -1;
	return idx >= 0 ? remove_idx(self, idx) : NULL;
}

imeth	gReplaceInt(int idx, char *opt)
{
	int	n;
	if (!ACTIVE)
		return NULL;
	n = SendMessage(iHCtl, CB_GETCOUNT, 0, (LPARAM) 0);
	if (idx < 0  ||  idx >= n  ||  n <= 0)
		return NULL;
	remove_idx(self, idx);
	return gAddOptionAt(self, idx, opt);
}

imeth	gReplaceStr(char *from, char *to)
{accessIVs;
	if (!ACTIVE)
		return NULL;
	return gReplaceInt(self, find_string(iv, from), to);
}

imeth	int	gFindIndex(char *choice)
{accessIVs;
	if (ACTIVE)
		return find_string(iv, choice);
	else
		return -1;
}

imeth	int	gIsSelectedStr(char *choice)
{
	int	i, j;
	if (!ACTIVE)
		return 0;
	i = find_string(iv, choice);
	if (i < 0)
		return 0;
	j = SendMessage(iHCtl, CB_GETCURSEL, 0, (LPARAM) 0);
	return i == j;
}

imeth	gAlphabetize()
{
	iAlphabetize = 1;
	return self;
}


imeth	ofun	gSetChgFunction(int (*fun)())
{
	ofun	org = (ofun) iChgFun;
	if (IsObj((object) iChgFun)) {
		gDispose((object) iChgFun);
		org = NULL;
	}
	iChgFun = fun;
	return org;
}

imeth	int	gPerform()
{
	return iDCFun ? (*iDCFun)(self, iDlg) : 0;
}

imeth	int	gPerformChg()
{
	if (iChgFun)
		if (SchemeClassSurrogate  &&  IsObj((object)iChgFun)  &&  ClassOf(iChgFun) == String) {
			char	cmd[100], ns[80];
			object	ret;
			int	res=0;
			sprintf(cmd, "(%s (int->object %ld) (int->object %ld))",
				gFunctionName(SchemeClassSurrogate, (object)iChgFun),
				(long) self, (long) iDlg);
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object)iChgFun, ns), 
						  cmd);
			if (IsObj(ret)) {
				res = gLongValue(ret);
				gDispose(ret);
			}
			return res;
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)iChgFun)  &&  ClassOf(iChgFun) == JavaScriptString) {
			int	res = 0;
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"))", gStringValue((object)iChgFun), (long) self, (long) iDlg);
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (ClassOf(ret) == LongInteger)
					res = gLongValue(ret);
				gDispose(ret);
			}
			return res;
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iChgFun)  &&  ClassOf(iChgFun) == JavaCallbackClassSurrogate)
			return gPerformJavaObjCallback((object)iChgFun, iDlg);
		else
			return (*iChgFun)(self, iDlg);
	return 0;
}

imeth	int	gNumbSelected()
{
	int	n;

	if (!ACTIVE)
		return iValue && gShortValue(iValue) >= 0 ? 1 : 0;
	n = SendMessage(iHCtl, CB_GETCURSEL, 0, (LPARAM) 0);
	return n < 0 ? 0 : 1;
}

imeth	gValueAt(int idx)
{
	int	r;
	char	*buf;

	if (!ACTIVE) {
		object	v = gNth(iOptions, idx+1);
		return v ? gCopy(gValue(v)) : NULL;
	}
	buf = gGetBuf(Application);
	r = SendMessage(iHCtl, CB_GETLBTEXT, (WPARAM) idx, (LPARAM) buf);
	return r < 0 ? NULL : gNewWithStr(String, buf);
}

imeth	gAssocAt(int idx)
{
	object	r = gNth(iAssoc, idx+1);
	return r ? gValue(r) : NULL;
}

imeth	gListAssoc()
{
	int	i = gShortValue(self);
	return i >= 0 ? gAssocAt(self, i) : NULL;
}

imeth	gRemoveAll()
{
	if (ACTIVE)
		SendMessage(iHCtl, CB_RESETCONTENT, 0, (LPARAM) 0);
	gDeepDispose(iOptions);
	if (iAutoDispose)
		gDeepDispose(iAssoc);
	else
		gDispose(iAssoc);
	iOptions = gNew(LinkList);
	iAssoc = gNew(LinkList);
	return self;
}

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
}

static	int	find_string(ivType *iv, char *choice)
{
	int	i = -1;
	if (iFindMode != 2)
		i = SendMessage(iHCtl, CB_FINDSTRINGEXACT, -1, (LPARAM) (LPSTR) choice);
	if (i < 0  &&  iFindMode != 1)
		i = SendMessage(iHCtl, CB_FINDSTRING, -1, (LPARAM) (LPSTR) choice);
	return i;
}

imeth	int	gFindMode(int mode)
{
	int	prev = iFindMode;
	if (mode >= 1  &&  mode <= 3)
		iFindMode = mode;
	return prev;
}

imeth	gDialog, gGetParent ()
{
	return iDlg;
}

imeth	gSetDefaultStr(char *str)
{
	if (iDefault)
		gDispose(iDefault);
	iDefault = gNewWithStr(String, str);
	return self;
}

imeth	gSetDefaultInt(int val)
{
	if (iDefault)
		gDispose(iDefault);
	iDefault = gNewWithInt(ShortInteger, val);
	return self;
}

imeth	gUseDefault()
{
	if (!iDefault)
		iDefault = gNew(ShortInteger);
	return SetValue(self, iDefault);
}

imeth	gGetDefault()
{
	return iDefault;
}

imeth	gSetSI(si)
{
	iSI = si;
	return self;
}

imeth	gGetSI()
{
	return iSI;
}

imeth	int	gSize()
{
	if (ACTIVE)
		return SendMessage(iHCtl, CB_GETCOUNT, 0, (LPARAM) 0);
	else
		return gSize(iOptions);
}

imeth	ofun	gSetDCFunction(int (*fun)())
{
	ofun	org = (ofun) iDCFun;
	if (IsObj((object) iDCFun)) {
		gDispose((object) iDCFun);
		org = NULL;
	}
	iDCFun = fun;
	return org;
}

imeth	gFixedFont()
{
	return gSetFont(self, mLoad(SystemFont, ANSI_FIXED_FONT));
}

imeth	gSetCursor(cursor)
{
	if (iEdtCursor)
		gDispose(iEdtCursor);
	iEdtCursor = cursor;
	gDefaultEdtProcessingMode(self, (unsigned) WM_SETCURSOR, !cursor);
	return gSetCursor(super, cursor ? gCopy(cursor) : NULL);
}

imeth	gWaitCursor(int wait)
{
	return gSetCursor(self, wait ? gLoadSys(SystemCursor, IDC_WAIT) : NULL);
}

imeth	gCompareFunction(ifun fun)
{
	iCmpFun = fun;
	return self;
}

imeth	int	gAutoDispose(int ad)
{
	int	pad = iAutoDispose;
	iAutoDispose = ad;
	return pad;
}

imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg);
}


// Yanghui:
imeth	DWORD gSetComboboxStyle(DWORD aComboboxStyle)
{
	DWORD nComboboxStyle = (DWORD) iComboboxStyle;
	iComboboxStyle = (short) aComboboxStyle; 
	return nComboboxStyle;
}
// Yanghui


imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_COMBOBOX_t	*v = vp;
	int	height, width, xPos, yPos, len;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	object	fobj = gGetFont(self);

	gGetSize(self, &height, &width);
	gGetVirtualPosition(self, &yPos, &xPos);
	v->hidden = gHiddenStatus(self) == 1 ? 'Y' : 'N';
	v->disabled = gDisableStatus(self) == 1 ? 'Y' : 'N';
	strncpy(v->name, gName(self), (sizeof(v->name)-1));
	v->height = height;
	v->width  = width;
	v->xPos   = xPos;
	v->yPos   = yPos;
	v->type = iComboboxStyle;  // Yanghui
	v->items = 0;
	v->defaultVal = 0;
	if (iTopic  &&  (len=gSize(iTopic)))
		v->helpTopicLen = len + 1;
	else
		v->helpTopicLen = 0;
	v->fontNameLen = fobj ? strlen(gName(fobj)) + 1 : 0;
	v->fontSize = fobj ? gPointSize(fobj) : 0;
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSetControlParameters(void *vp)
{
	CTLTYPE_COMBOBOX_t	*v = vp;
	// HWND hwnd; // Yanghui
	int	sm = gSetScalingMode(Application, SM_PIXELS);

	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gSetName(self, v->name);
 
	 // Yanghui:
	iComboboxStyle = v->type; 

  	gSetStyle(self, WS_VISIBLE | WS_VSCROLL | (long)v->type); 
	//hwnd = gHandle(self);
	//SetWindowLong(hwnd, GWL_STYLE, WS_CHILD | WS_VISIBLE | WS_VSCROLL | (long)v->type & ~(WS_OVERLAPPED | WS_POPUP));
	//SetWindowPos(hwnd, NULL, 0,0,0,0, (SWP_NOSIZE | SWP_NOMOVE | SWP_NOZORDER | SWP_DRAWFRAME ) ); 
	// Yanghui
 
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_COMBOBOX_t	v;
	short	type = CTLTYPE_COMBOBOX, size = sizeof(v);
	object	fobj = gGetFont(self);

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.helpTopicLen  &&  1 != fwrite(gStringValue(iTopic), (int) v.helpTopicLen, 1, fp))
		return NULL;

	if (fobj  &&  v.fontNameLen  &&  1 != fwrite(gName(fobj), (int) v.fontNameLen, 1, fp))
		return NULL;

	return self;
}

#define	BUFLEN	128

cmeth	gLoadControl(FILE *fp, parent)
{
	CTLTYPE_COMBOBOX_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];

	double controlScaleFactor;  // Yanghui
	
	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);
	
	// Yanghui:
	// get the screen resolution in the CLD file which is loaded in window.d
	// and the current screen resolution and scale the controls in the cld file
	// so that the cld file will be displayed with the same look
	
	if(gGetScaleFlg(parent)) {    // do the scaling if the scaling flag is set
		controlScaleFactor = gGetControlScale(parent);
		if(controlScaleFactor>0) {
			double dCxScale, dCyScale, dXpos, dYpos;
			RECT   MFMarginRect;
			int    logPixelsy;
			HWND   hWnd;
			HDC    hDc;
			int    textHeight;

			gGetMFCxCyScale(parent, &dCxScale, &dCyScale);
			gGetMFMargins(parent, &MFMarginRect);

			// to reduce the accumulated error, a special trick is used 
			dXpos  = (v.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale;
			v.xPos = dXpos + 0.5;
			dYpos  = (v.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale;
			v.yPos = dYpos + 0.5;

			v.width  = (dXpos + v.width*controlScaleFactor*dCxScale  + 0.5) - v.xPos;
			v.height = (dYpos + v.height*controlScaleFactor*dCyScale + 0.5) - v.yPos;

			v.fontSize = (int)(v.fontSize*controlScaleFactor*dCyScale)/gGetFontScale(parent);

			// if the font information was not saved, adjust the fontsize. 
			hWnd = gHandle(parent);
			hDc = GetDC(hWnd);  
				// if hWnd=NULL, GetDC retrieves the device context for the entire screen
			logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
			ReleaseDC(hWnd, hDc);
		
			textHeight = v.height/max(2, (1+v.items));

			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(MulDiv(textHeight, 72, logPixelsy), v.fontSize);
		}
	}
	// Yanghui

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddComboBox(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, &end, v.name);

	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.helpTopicLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetTopic(ctl, p);
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

	// Yanghui:
	// when scaling is processed, the v.fontSize may be too small and not available for the chosen font,
	// although system would get the closest match, but the font height would not match the control height.
	// In this case, Arial font will be chosen. In addition, if the old cld does not have a font chosen, 
	// Arial font is the default
	if (gGetScaleFlg(parent)) {     // do the scaling if the scaling flag is set
		if (v.fontNameLen) {
			p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			if (1 != fread(p, (int) v.fontNameLen, 1, fp))
				vError(Application, "Error reading control layout definition file");

			// if(v.fontSize<8)
			// 	gSetFont(ctl, vNew(ExternalFont, "Arial", v.fontSize));
			// else
				gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));

			if (v.fontNameLen > BUFLEN)
				free(p);
		}
		else
			gSetFont(ctl, vNew(ExternalFont, "Arial", v.fontSize));
	}
	else {  // the original code
		if (v.fontNameLen) {
			p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			if (1 != fread(p, (int) v.fontNameLen, 1, fp))
				vError(Application, "Error reading control layout definition file");
			gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));
			if (v.fontNameLen > BUFLEN)
				free(p);
		}
	}
	// Yanghui

	/*****************************************************************/
	// This should be set up in CLD and set in gSetControlParameters 
	// gSetStyle(ctl, WS_VISIBLE | CBS_DROPDOWNLIST);
	/*****************************************************************/
	// Yanghui:
	if(v.type<(short)CBS_SIMPLE || v.type > (short)CBS_DROPDOWNLIST)
		v.type = CBS_DROPDOWNLIST;
		
	if ((v.type & CBS_DROPDOWNLIST)==CBS_DROPDOWNLIST)
		v.type |= CBS_HASSTRINGS;	

	// gSetStyle(ctl, WS_VISIBLE | WS_VSCROLL | (long)v.type); 
	// Yanghui

	gSetControlParameters(ctl, &v);

	if (gModifyChildren(parent)) {
		gAddOption(ctl, "Combobox");
		gShow(ctl);

		gSetSize(ctl, v.height, v.width);          // Yanghui
		gSetVirtualPosition(ctl, v.yPos, v.xPos);  // Yanghui
	}
	
		
	return ctl;
}

imeth	gGetSize(int *height, int *width)
{
	object	font = gGetFont(super);
	RECT	r;

	gGetSize(super, height, width);

	// Yanghui:
	if(iHCtl) {
		SendMessage(iHCtl, CB_GETDROPPEDCONTROLRECT, 0, (LPARAM)((LPRECT)&r));
		*height = r.bottom - r.top;
	}
	// Yanghui

	/*******************************************
	if (iEdit) {
		GetWindowRect(iEdit, &r);
		*height = r.bottom - r.top;
	} else {
		*height = 22;
    }
	********************************************/

	gScaleToCurrentMode(Application, height, width, font);
	
	return self;
}

imeth	int	gGetDropDownHeight()
{
	object	font = gGetFont(super);
	int	h, w;

	gGetSize(super, &h, &w);
	gScaleToCurrentMode(Application, &h, NULL, font);

	return h;
}


// Yanghui:
cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_COMBOBOX_t  v;
	int                 end;
	object              ctl;
	short               size;
	char                *p, buf[BUFLEN];

	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);
	
	v.xPos += nXshift;
	v.yPos += nYshift;

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddComboBox(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, &end, v.name);

	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.helpTopicLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetTopic(ctl, p);
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

	if (v.fontNameLen) {
		p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.fontNameLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));
		if (v.fontNameLen > BUFLEN)
			free(p);
	}

	/*****************************************************************/
	// This should be set up in CLD and set in gSetControlParameters 
	// gSetStyle(ctl, WS_VISIBLE | CBS_DROPDOWNLIST);
	/*****************************************************************/
	if(v.type<(short)CBS_SIMPLE || v.type > (short)CBS_DROPDOWNLIST)
		v.type = CBS_DROPDOWNLIST;

	if ((v.type & CBS_DROPDOWNLIST)==CBS_DROPDOWNLIST)
		v.type |= CBS_HASSTRINGS;

	gSetStyle(ctl, WS_VISIBLE | WS_VSCROLL | (long)v.type ); 

	gSetControlParameters(ctl, &v);

	if (gModifyChildren(parent)) {
		gAddOption(ctl, "Combobox");
		gShow(ctl);

		gSetSize(ctl, v.height, v.width);
		gSetVirtualPosition(ctl, v.yPos, v.xPos);
	}
	


		
	return ctl;
}


/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the combobox control of the cld file,
//                  This is a dummy reading, no object is generated.
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_COMBOBOX_t v;
	short              size;
	char               *p, buf[BUFLEN];

	if(!fp)
		return NULL;

	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);

	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.helpTopicLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

	if (v.fontNameLen) {
		p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.fontNameLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (v.fontNameLen > BUFLEN)
			free(p);
	}

	return NULL;
}

// Yanghui

imeth gWriteXML(FILE *fp)
{

	CTLTYPE_COMBOBOX_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);

	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<combobox>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<defaultVal>%d</defaultVal>\n",v.defaultVal);
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<fontname>%s</fontname>\n",gStringToXML(XMLNode,buf,gName(fnt)));
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<fontNameLen>%d</fontNameLen>\n",v.fontNameLen);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<type>%d</type>\n",v.type);
	fprintf(fp,"\t\t\t<items>%d</items>\n",v.items);
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	fprintf(fp,"\t\t\t<textXPath>%s</textXPath>\n",gTextXPathBinding(self));
	fprintf(fp,"\t\t\t<dataXPath>%s</dataXPath>\n",gDataXPathBinding(self));
	fprintf(fp,"\t\t\t<listKeyXPath>%s</listKeyXPath>\n",gListKeyXPathBinding(self));
	fprintf(fp,"\t\t\t<choiceXPath>%s</choiceXPath>\n",gChoiceXPathBinding(self));
	fprintf(fp,"\t\t</combobox>\n");


	return self;
}




cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_COMBOBOX_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];

	double controlScaleFactor;  // Yanghui
	
	memset(&v, 0, sizeof v);
	
	gPopulateStringFromNode(curnode,v.name,"name");
	v.xPos=gGetIntFromNode(curnode,"x");
	v.yPos=gGetIntFromNode(curnode,"y");
	v.width=gGetIntFromNode(curnode,"width");
	v.height=gGetIntFromNode(curnode,"height");
	v.hidden=gGetCharFromNode(curnode,"hidden");
	v.disabled=gGetCharFromNode(curnode,"disabled");
	v.fontSize=gGetIntFromNode(curnode,"fontsize");
	v.defaultVal=gGetIntFromNode(curnode,"defaultVal");
	v.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	v.type=gGetIntFromNode(curnode,"type");
	v.items=gGetIntFromNode(curnode,"items");
	
	// Yanghui:
	// get the screen resolution in the CLD file which is loaded in window.d
	// and the current screen resolution and scale the controls in the cld file
	// so that the cld file will be displayed with the same look
	
	if(gGetScaleFlg(parent)) {    // do the scaling if the scaling flag is set
		controlScaleFactor = gGetControlScale(parent);
		if(controlScaleFactor>0) {
			double dCxScale, dCyScale, dXpos, dYpos;
			RECT   MFMarginRect;
			int    logPixelsy;
			HWND   hWnd;
			HDC    hDc;
			int    textHeight;

			gGetMFCxCyScale(parent, &dCxScale, &dCyScale);
			gGetMFMargins(parent, &MFMarginRect);

			// to reduce the accumulated error, a special trick is used 
			dXpos  = (v.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale;
			v.xPos = dXpos + 0.5;
			dYpos  = (v.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale;
			v.yPos = dYpos + 0.5;

			v.width  = (dXpos + v.width*controlScaleFactor*dCxScale  + 0.5) - v.xPos;
			v.height = (dYpos + v.height*controlScaleFactor*dCyScale + 0.5) - v.yPos;

			v.fontSize = (int)(v.fontSize*controlScaleFactor*dCyScale)/gGetFontScale(parent);

			// if the font information was not saved, adjust the fontsize. 
			hWnd = gHandle(parent);
			hDc = GetDC(hWnd);  
				// if hWnd=NULL, GetDC retrieves the device context for the entire screen
			logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
			ReleaseDC(hWnd, hDc);
		
			textHeight = v.height/max(2, (1+v.items));

			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(MulDiv(textHeight, 72, logPixelsy), v.fontSize);
		}
	}
	// Yanghui

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddComboBox(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, &end, v.name);

	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	gPopulateStringFromNode(curnode,buf,"textXPath");
	gSetTextXPathBinding(ctl,buf);
	
	gPopulateStringFromNode(curnode,buf,"dataXPath");
	gSetDataXPathBinding(ctl,buf);
	
	gPopulateStringFromNode(curnode,buf,"listKeyXPath");
	gSetListKeyXPathBinding(ctl,buf);
	
	gPopulateStringFromNode(curnode,buf,"choiceXPath");
	gSetChoiceXPathBinding(ctl,buf);
	
	
	
	
	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
			
		gPopulateStringFromNode(curnode,p,"helpTopic");
		
		gSetTopic(ctl, p);
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

	// Yanghui:
	// when scaling is processed, the v.fontSize may be too small and not available for the chosen font,
	// although system would get the closest match, but the font height would not match the control height.
	// In this case, Arial font will be chosen. In addition, if the old cld does not have a font chosen, 
	// Arial font is the default
	if (gGetScaleFlg(parent)) {     // do the scaling if the scaling flag is set
		if (v.fontNameLen) {
			p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			
			gPopulateStringFromNode(curnode,p,"fontname");

			gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));

			if (v.fontNameLen > BUFLEN)
				free(p);
		}
		else
			gSetFont(ctl, vNew(ExternalFont, "Arial", v.fontSize));
	}
	else {  // the original code
		if (v.fontNameLen) {
			p = v.fontNameLen > BUFLEN ? malloc((unsigned)v.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			gPopulateStringFromNode(curnode,p,"fontname");
			gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));
			if (v.fontNameLen > BUFLEN)
				free(p);
		}
	}
	// Yanghui

	/*****************************************************************/
	// This should be set up in CLD and set in gSetControlParameters 
	// gSetStyle(ctl, WS_VISIBLE | CBS_DROPDOWNLIST);
	/*****************************************************************/
	// Yanghui:
	if(v.type<(short)CBS_SIMPLE || v.type > (short)CBS_DROPDOWNLIST)
		v.type = CBS_DROPDOWNLIST;
		
	if ((v.type & CBS_DROPDOWNLIST)==CBS_DROPDOWNLIST)
		v.type |= CBS_HASSTRINGS;	

	// gSetStyle(ctl, WS_VISIBLE | WS_VSCROLL | (long)v.type); 
	// Yanghui

	gSetControlParameters(ctl, &v);

	if (gModifyChildren(parent)) {
		gAddOption(ctl, "Combobox");
		gShow(ctl);

		gSetSize(ctl, v.height, v.width);          // Yanghui
		gSetVirtualPosition(ctl, v.yPos, v.xPos);  // Yanghui
	}
	
		
	return ctl;
}

imeth gSetTextXPathBinding(char *binding)
{
	if (iTextXPath)
		gChangeStrValue(iTextXPath, binding);
	else
		iTextXPath=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gTextXPathBinding()
{
	return iTextXPath ? gStringValue(iTextXPath) : "";
}	
	
imeth gSetDataXPathBinding(char *binding)
{
	if (iDataXPath)
		gChangeStrValue(iDataXPath, binding);
	else
		iDataXPath=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gDataXPathBinding()
{
	return iDataXPath ? gStringValue(iDataXPath) : "";
}	
	
imeth gSetListKeyXPathBinding(char *binding)
{
	if (iListKeyXPath)
		gChangeStrValue(iListKeyXPath, binding);
	else
		iListKeyXPath=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gListKeyXPathBinding()
{
	return iListKeyXPath ? gStringValue(iListKeyXPath) : "";
}

	
imeth gSetChoiceXPathBinding(char *binding)
{
	if (iChoiceXPath)
		gChangeStrValue(iChoiceXPath, binding);
	else
		iChoiceXPath=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gChoiceXPathBinding()
{
	return iChoiceXPath ? gStringValue(iChoiceXPath) : "";
}

imeth char * gMainKeyXPathBinding()
{
	return gXPathBinding(self);
}

imeth gSetMainKeyXPathBinding(char *binding)
{
	return gSetXPathBinding(self, binding);
}





