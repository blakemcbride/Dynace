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

defclass  DateControl : SpinControl  {
	HWND	iHCtl;		/*  handle to edit control  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	iWindowControl; /*  1=window control		*/
	iValue;			/*  field value			*/
	iDefault;		/*  default value		*/
	int	iDisposeValue;	/*  1=auto dispose of iValue	*/
	int	(*iAcf)();	/*  aux checking function	*/
	int	(*iDCFun)();	/*  function - double click	*/
	int	iCutoff;
	int	iCentury;	/*  Contains earliest possible century for confusion.	*/

	iSI;			/*  ODBC StatementInfo		*/

	iBuf;			/*  field entry buffer		*/
	int	iState;		/*  state of field		*/

	long	iMinimum;	/*  minimum value		*/
	long	iMaximum;	/*  maximum value		*/
	int	iAllowNone;	/*  allow no date flag		*/
	int	iHideData;	/*  don't show data - show ***  */

	iDateFmt;		/*  format of date in control	*/
	
	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/
class:
	int	cCutoff;
	int	cCentury;	/*  Contains earliest possible century for confusion.	*/
	int	cYear;		/*  Contains the current year.	*/

init:	class_init;
};


#include <ctype.h>


#define MAX_LENGTH	10	/*  max field width	*/

#define NEW_STATE	0	/*  nothing is being displayed in the control window	*/
#define DISPLAY_STATE	1	/*  control window contains a value ment for display only  */
#define ENTRY_STATE	2	/*  control window contains an editable usable value  */

#define	issep(x)	((x) == '/'  ||  (x) == '-'  ||  (x) == '.')

static	void	saveValue(ivType *iv);
static	int	validDate(ivType *iv);
static	int	makeYear(ivType *iv, int y);

private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect);


static	void	class_init()
{
	object	td = gToday(Date);
	int	c = gLongValue(td) / 10000;
	int	y = (c % 100) + 31;		// 30 years into the future, 70 years into the past

	cCentury = (c - 69) / 100;
	
	cCutoff = y > 99 ? y - 100 : y;
	cYear = c;
	
	gDispose(td);
}

private	imeth	long	process_wm_char(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	DWORD	r = SendMessage(iHCtl, EM_GETSEL, (WPARAM)0, (LPARAM) 0);
	WORD	start = LOWORD(r);
	WORD	end   = HIWORD(r);

	if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	if (wParam == '\r')
		return gCallDefaultProc(self, mMsg, wParam, lParam);
	if (wParam == '\t')
		if (iWindowControl) {
			object	next = GetKeyState(VK_SHIFT) & 0x80 ? gPreviousControl(iDlg, self) : gNextControl(iDlg, self);
			if (next)
				SetFocus(gHandle(next));
			return 0L;
		} else
			return gCallDefaultProc(self, mMsg, wParam, lParam);
	if (wParam == 'd' || wParam == 'D' || wParam == 'c' || wParam == 'C') {
		long	dt = 0;
		iState = DISPLAY_STATE;
		dt = gPickDate(DatePicker, self, NULL);

		if (dt)
			gSetLongValue(self, dt);
		gSetFocus(self);
		return 0L;
	}
	if (!issep(wParam)  &&  !isdigit(wParam)  &&  wParam != '\b')  {
		MessageBeep(-1);
		return 0L;
	}
	if (start == end  &&  wParam != '\b'  &&  MAX_LENGTH <= GetWindowTextLength(iHCtl))  {
		MessageBeep(-1);
		return 0L;
	}
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	if (iBuf)
		SetWindowText(iHCtl, (LPCSTR) gStringValue(iBuf));
	else
		SetWindowText(iHCtl, (LPCSTR) "");
	iState = ENTRY_STATE;
#ifdef	WIN32
	SendMessage(iHCtl, EM_SETSEL, (WPARAM)0, (LPARAM) -1);
#else
	SendMessage(iHCtl, EM_SETSEL, (WPARAM)0, MAKELPARAM(0, -1));
#endif
	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	gCreateCaret(self);
	return 0L;
}

private	imeth	long	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	int	vd;
	
	/*  Windows sometimes sends kill focus messages to controls that don't have the focus!!  */
	if (iState != ENTRY_STATE)
		return 0L;

	vd = validDate(iv);

	saveValue(iv);
	if (iTopic)
		gSetTopic(HelpSystem, iPrevTopic);
/*
  We don't want the loose focus function to execute if the user clicks on CANCEL.  We test
  this by checking who is getting the focus next.  This test doesn't work as desired if
  the CANCEL button immediatly follows the control in question.  In that case if the user
  tabs out of the control the killfocus function will not be executed as desired.  The fix
  for this is to never make the CANCEL follow, in tab order terms, a control in which you
  have a killfocus function attached.
*/
	if (!wParam  ||  GetDlgCtrlID((HANDLE) wParam) != IDCANCEL)
		if (!vd) {
//			MessageBox(gHandle(iDlg), "Invalid date format.  Use M/D/Y, M/D, MDDY, MDD, M-D-Y, M.D.Y", "Error Message Window", MB_OK);  
			gErrorMessage(Application, "Invalid date format.  Use M/D/Y, M/D, MDDY, MDD, M-D-Y, M.D.Y");
			PostMessage(gHandle(iDlg), WM_NEXTDLGCTL, (WPARAM) iHCtl, (LPARAM) TRUE);
		} else if (gExecuteSetFunctions(self, iDlg))
			PostMessage(gHandle(iDlg), WM_NEXTDLGCTL, (WPARAM) iHCtl, (LPARAM) TRUE);
	DestroyCaret();
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
	if (iDCFun) {
		if (SchemeClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == String) {
			char	cmd[100], ns[80];
			int	res;
			sprintf(cmd, "(%s (int->object %ld) (int->object %ld))",
				gFunctionName(SchemeClassSurrogate, (object)iDCFun),
				(long) self, (long) iDlg);
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					      gNamespaceName(SchemeClassSurrogate, (object)iDCFun, ns), 
					      cmd);
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaCallbackClassSurrogate)
			return gPerformJavaObjCallback((object)iDCFun, iDlg);
		else if (JavaScriptClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"))", gStringValue((object)iDCFun), (long) self, (long) iDlg);
			gExecuteStringNR(JavaScriptClassSurrogate, cmd);
		} else
			iDCFun(self, iDlg);
		return 0L;
	} else if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
		return gCallDefaultProc(self, mMsg, wParam, lParam);
}

static	int	pickDate(object dctl, unsigned mbtn)
{
	long	dt = gPickDate(DatePicker, dctl, NULL);
	int	x, y;

	if (dt)
		gSetLongValue(dctl, dt);
	return 0;
}

cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	object	td = gToday(Date);

	iCtlID = ctlID;
	iState = NEW_STATE;
	iMinimum = gLongValue(gAddYears(td, -100));
	iMaximum = gLongValue(gAddYears(td, 200));
	gDispose(td);
	iAllowNone = 1;
	iDisposeValue = 1;
	iDlg = dlg;
	iDateFmt = gNewWithStr(String, "%m %d, %Y");
	iCutoff = cCutoff;
	iCentury = cCentury;

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

//	gSetMouseFunction(obj, (MK_SHIFT | MK_RBUTTON), pickDate);

	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "edit", parent);
	ivType	*iv = ivPtr(obj);
	object	td = gToday(Date);

	iDlg = parent;
	iWindowControl = 1;
	iCtlID = ctlID;
	iState = NEW_STATE;
	iMinimum = gLongValue(gAddYears(td, -100));
	iMaximum = gLongValue(gAddYears(td, 200));
	gDispose(td);
	iAllowNone = 1;
	iDisposeValue = 1;
	iDateFmt = gNewWithStr(String, "%m %d, %Y");
	iCutoff = cCutoff;
	iCentury = cCentury;

	gSetStyle(obj, WS_VISIBLE);

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);
	gDefaultProcessingMode(obj, (unsigned) WM_RBUTTONDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONUP, process_wm_rbuttonup);
	gDefaultProcessingMode(obj, (unsigned) WM_RBUTTONUP, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONDBLCLK, 0);  /*  no auto default processing  */

//	gSetMouseFunction(obj, (MK_SHIFT | MK_RBUTTON), pickDate);
	
	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

static	object	fmtDate(object val, char *fmt)
{
	object	rval;
	
	if (gIsKindOf(val, Date))
		rval = gFormatDate(val, fmt);
	else {
		object	t = gNewWithLong(Date, gLongValue(val));

		rval = gFormatDate(t, fmt);
		gDispose(t);
	}
	return rval;
}

imeth	int	gShow()
{
	object	font = gGetFont(self);
	if (iHCtl)
		return 0;
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	
	if (iValue  &&  gLongValue(iValue))  {
		object	t;
		if (iBuf)
			gDispose(iBuf);
		iBuf = fmtDate(iValue, "%n/%d/%Y");
		t = fmtDate(iValue, gStringValue(iDateFmt));

		if (iHideData)
			SetWindowText(iHCtl, "****************************");
		else
			SetWindowText(iHCtl, (LPCSTR) gStringValue(t));
		iState = DISPLAY_STATE;
		gDispose(t);
	} else  {
		if (iBuf)
			gChangeStrValue(iBuf, "");
		else
			iBuf = gNew(String);
		SetWindowText(iHCtl, (LPCSTR) "");
		iState = ENTRY_STATE;
	}

	gInitialize(super, (HWND)0, NULL);
	if (iSI)
		gUpdate(iSI);
	if (gIsKindOf(iDlg, Window)) {
		RECT	r;
		int	pm = gSetScalingMode(Application, SM_PIXELS);

		gGetPosition(self, &r.top, &r.left);
		gGetSize(self, &r.bottom, &r.right);
		r.bottom += r.top + 2;
		r.right += r.left + 2;

		r.top -= 2;
		r.left -= 2;
		
		InvalidateRect(gHandle(iDlg), &r, TRUE);
		
		gSetScalingMode(Application, pm);
	}
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	object	font = gGetFont(self);
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "DateControl control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	if (iValue  &&  gLongValue(iValue))  {
		object	t;
		if (iBuf)
			gDispose(iBuf);
		iBuf = fmtDate(iValue, "%n/%d/%Y");
		t = fmtDate(iValue, gStringValue(iDateFmt));

		if (iHideData)
			SetWindowText(iHCtl, "****************************");
		else
			SetWindowText(iHCtl, (LPCSTR) gStringValue(t));
		iState = DISPLAY_STATE;
		gDispose(t);
	} else  {
		if (iBuf)
			gChangeStrValue(iBuf, "");
		else
			iBuf = gNew(String);
		SetWindowText(iHCtl, (LPCSTR) "");
		iState = ENTRY_STATE;
	}
	if (iSI)
		gUpdate(iSI);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (iValue  &&  iDisposeValue)
		gDispose(iValue);
	if (iDefault)
		gDispose(iDefault);
	if (iBuf)
		gDispose(iBuf);
	if (iDateFmt)
		gDispose(iDateFmt);
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
	if (iHCtl) {
		HC_DELETE(WINDOW_HANDLE_CACHE, iHCtl);
		iHCtl = 0;
	}
	return self;
}	

static	long	dt_to_long_noDelim(ivType *iv, char *v)
{
	int	y, m, d = 0;
	char	*cp = v;

	while (*cp)
		if (!isdigit(*cp++))
			return 0L;

	switch (cp - v) {
	case 3:		//	m/dd - Automatically add the current year
		m = v[0] - '0';
		d = (v[1] -'0') * 10 + (v[2] - '0');
		y = cYear;
		break;
	case 4:		//	mm/dd - Automatically add the current year
		m = (v[0] -'0') * 10 + (v[1] - '0');
		d = (v[2] -'0') * 10 + (v[3] - '0');
		y = cYear;
		break;
	case 5:		//	m/dd/yy
		m = v[0] - '0';
		d = (v[1] -'0') * 10 + (v[2] - '0');
		y = makeYear(iv, (v[3] -'0') * 10 + (v[4] - '0'));
		break;
	case 6:		//	mm/dd/yy
		m = (v[0] -'0') * 10 + (v[1] - '0');
		d = (v[2] -'0') * 10 + (v[3] - '0');
		y = makeYear(iv, (v[4] -'0') * 10 + (v[5] - '0'));
		break;
	case 7:		//	m/dd/yyyy
		m = v[0] - '0';
		d = (v[1] -'0') * 10 + (v[2] - '0');
		y = (v[3] -'0') * 1000 + (v[4] -'0') * 100 + (v[5] -'0') * 10 + (v[6] - '0');
		break;
	case 8:		//	mm/dd/yyyy
		m = (v[0] -'0') * 10 + (v[1] - '0');
		d = (v[2] -'0') * 10 + (v[3] - '0');
		y = (v[4] -'0') * 1000 + (v[5] -'0') * 100 + (v[6] -'0') * 10 + (v[7] - '0');
		break;
	}

	if (d)
		return (long) y * 10000L + (long) m * 100L + (long) d;

	return 0L;
}

static	long	dt_to_long(ivType *iv, char *v)
{
	int	y, m, d, i;
	char	buf[MAX_LENGTH+1];
	long	temp;

	if (temp = dt_to_long_noDelim(iv, v))
		return temp;
	
	while (isspace(*v))
		v++;
	if (!v[0])
		return(0L);
	for (i=0 ; *v  &&  !issep(*v) ; v++)
		if (*v != ' ')
			buf[i++] = *v;
	if (!i  ||  !*v++)
		return 0L;
	buf[i] = '\0';
	m = atoi(buf);

	for (i=0 ; *v  &&  !issep(*v) ; )
		buf[i++] = *v++;
	if (!i)
		return 0L;
	buf[i] = '\0';
	d = atoi(buf);

	if (*v)
		v++;
	for (i=0 ; *v  &&  !issep(*v) ; )
		buf[i++] = *v++;
	buf[i] = '\0';
	if (!i  ||  *v)
		y = cYear;
	else
		y = makeYear(iv, atoi(buf));

	return (long) y * 10000L + (long) m * 100L + (long) d;
}

static	int	Vi(ivType *iv, char *v)	/*  validate character string is number	 
					    returns 1 for valid	input, 0 otherwise */
{
	int	y, m, d, i;
	char	buf[MAX_LENGTH+1];
	long	val;
	object	dt;

	if (!(val = dt_to_long_noDelim(iv, v))) {
		while (isspace(*v))
			v++;
		if (!v[0])
			return(1);
		for (i=0 ; *v  &&  !issep(*v) ; v++)
			if (*v != ' ')
				buf[i++] = *v;
		buf[i] = '\0';
		m = atoi(buf);
		if (!*v  ||  m > 12  ||  m < 1)
			return 0;

		for (v++, i=0 ; *v  &&  !issep(*v) ; )
			buf[i++] = *v++;
		buf[i] = '\0';
		d = atoi(buf);
		if (d > 31  ||  d < 1)
			return 0;

		if (*v)
			v++;
		for (i=0 ; *v  &&  !issep(*v) ; )
			buf[i++] = *v++;
		buf[i] = '\0';
		if (!i  ||  *v)
			y = cYear;
		else
			y = makeYear(iv, atoi(buf));
		if (*v  ||  y > 4000)
			return 0;

		val = (long) y * 10000L + (long) m * 100L + (long) d;
	}
	dt = gNewWithLong(Date, val);
	i = gValidDate(dt);
	gDispose(dt);
	return i;
}

imeth	int	gCheckValue()
{
	char	*buf = gGetBuf(Application);
	long	val;
	object	n, s;
	
	/*  KILLFOCUS may not have been received yet if user pressed Enter  */

	if (iState != NEW_STATE)  {
		char	*pbv, fld[MAX_LENGTH+1];

		if (iState == ENTRY_STATE)  {
			if (!GetWindowText(iHCtl, (LPSTR) (pbv=fld), MAX_LENGTH+1))
				*pbv = '\0';
		} else if (iState == DISPLAY_STATE)
			if (iBuf)
				pbv = gStringValue(iBuf);
			else
				pbv = "";
		if (!(!*pbv && iAllowNone  ||  Vi(iv, pbv)))  {
			sprintf(buf, "%s must contain a valid date.", strlen(gName(self)) ? gName(self) : "Field");
//			MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
			gErrorMessage(Application, buf);
			SetFocus(iHCtl);
			return 1; /*  error  */
		}
		val = dt_to_long(iv, pbv);
	} else if (iValue)
		val = gLongValue(iValue);
	else
		val = 0L;
	if (!(val >= iMinimum  ||  !val  &&  iAllowNone))  {
		n = gNewWithLong(Date, iMinimum);
		s = fmtDate(n, gStringValue(iDateFmt));
		sprintf(buf, "%s must be greater than or equal to %s", strlen(gName(self)) ? gName(self) : "Field", gStringValue(s));
		gDispose(n);
		gDispose(s);
//		MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
		gErrorMessage(Application, buf);
		SetFocus(iHCtl);
		return 1;	/*  error  */
	}
	if (val > iMaximum)  {
		n = gNewWithLong(Date, iMaximum);
		s = fmtDate(n, gStringValue(iDateFmt));
		sprintf(buf, "%s must be less than or equal to %s", strlen(gName(self)) ? gName(self) : "Field", gStringValue(s));
		gDispose(n);
		gDispose(s);
//		MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
		gErrorMessage(Application, buf);
		SetFocus(iHCtl);
		return 1;	/*  error  */
	}
	if (iValue)
		gChangeLongValue(iValue, val);
	else
		iValue = gNewWithLong(Date, val);
	if (iAcf) {
		int	r = 0;

		if (SchemeClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == String) {
			char	cmd[100], ns[80];
			object	ret;

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
	if (iSI)
		gUpdate(iSI);
	return 0;
}

static	int	validDate(ivType *iv)
{
	char	buf[MAX_LENGTH+1];
	object	t;
	char	*cp;

	if (!GetWindowText(iHCtl, (LPSTR) buf, MAX_LENGTH+1))
		*buf = '\0';
#if 0
	for (cp = buf; *cp; cp++)
		if (*cp == '-')
			*cp = '/';
#endif
	return Vi(iv, buf);
}

static	void	saveValue(ivType *iv)
{
	char	buf[MAX_LENGTH+1];
	object	t;
	char	*cp;

	if (!GetWindowText(iHCtl, (LPSTR) buf, MAX_LENGTH+1))
		*buf = '\0';
#if 0
	for (cp = buf; *cp; cp++)
		if (*cp == '-')
			*cp = '/';
#endif
	if (iBuf)
		gChangeStrValue(iBuf, buf);
	else
		iBuf = gNewWithStr(String, buf);
	if (!Vi(iv, buf))
		*buf = '\0';
	if (iValue)
		gChangeLongValue(iValue, dt_to_long(iv, buf));
	else
		iValue = gNewWithLong(Date, dt_to_long(iv, buf));
	if (iSI)
		gUpdate(iSI);

	t = fmtDate(iValue, gStringValue(iDateFmt));
	if (iHideData)
		SetWindowText(iHCtl, "****************************");
	else
		SetWindowText(iHCtl, (LPCSTR) gStringValue(t));
	iState = DISPLAY_STATE;
	gDispose(t);
}

imeth	gValue : Value ()
{
	/*  update buf from control if you can  */

	if (iDlg  &&  gInDialog(iDlg)  &&  iHCtl  &&  iState == ENTRY_STATE  &&  !iHideData)  {
		char	buf[MAX_LENGTH+1];

		if (!GetWindowText(iHCtl, (LPSTR) buf, MAX_LENGTH+1))
			*buf = '\0';
		if (iBuf)
			gChangeStrValue(iBuf, buf);
		else
			iBuf = gNewWithStr(String, buf);
	}

	/*  update value from buf if you can  */

	if (iBuf)  {
		char	*bp = gStringValue(iBuf);
		if (Vi(iv, bp)) {
			if (iValue)
				gChangeLongValue(iValue, dt_to_long(iv, bp));
			else
				iValue = gNewWithLong(Date, dt_to_long(iv, bp));
			if (iSI)
				gUpdate(iSI);
		}
	}

		
	/*  create a default value if necessary  */

	if (!iValue)
		iValue = gNew(Date);

	return iValue;
}

imeth	long	gLongValue, gDateValue ()
{
	return gLongValue(Value(self));
}

imeth	gDateRange(long minimum, long maximum, int an)
{
	iMinimum = minimum;
	iMaximum = maximum;
	iAllowNone = an;
	return self;
}

imeth	gCheckFunction(int (*fun)())
{
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	iAcf = fun;
	return self;
}

imeth	gUpdate : update ()
{
	long	dt;

	if (!iValue)
		iValue = gNew(Date);
	if (iBuf)
		gDispose(iBuf);
	dt = gLongValue(iValue);
	if (dt)  {
		object	t;
		
		iBuf = fmtDate(iValue, "%n/%d/%Y");

		if (iState == ENTRY_STATE) {
			if (iHCtl)
				if (iHideData)
					SetWindowText(iHCtl, "****************************");
				else
					SetWindowText(iHCtl, (LPCSTR) gStringValue(iBuf));
		} else if (iDlg  &&  gInDialog(iDlg))  {
			if (iHCtl) {
				if (iHideData)
					SetWindowText(iHCtl, "****************************");
				else {
					t = fmtDate(iValue, gStringValue(iDateFmt));
					SetWindowText(iHCtl, (LPCSTR) gStringValue(t));
					gDispose(t);
				}
			}
			iState = DISPLAY_STATE;
		}
	}  else  {
		iBuf = gNew(String);
		if (iDlg  &&  gInDialog(iDlg))  {
			if (iHCtl)
				SetWindowText(iHCtl, (LPCSTR) "");
			iState = DISPLAY_STATE;
		}
	}
	
	return self;
}

imeth	gSetValue(val)
{
	ChkArgTyp(val, 2, Date);
	if (iValue)
		gChangeValue(iValue, val);
	else
		iValue = gNewWithLong(Date, gLongValue(val));
	if (iSI)
		gUpdate(iSI);

	return update(self);
}

imeth	gSetLongValue, gSetDateValue (long val)
{
	if (iValue)
		gChangeLongValue(iValue, val);
	else
		iValue = gNewWithLong(Date, val);
	if (iSI)
		gUpdate(iSI);

	return update(self);
}

imeth	gAttach(result)
{
	ChkArg(result, 2);
	if (iValue  &&  iDisposeValue)
		gDispose(iValue);
	iValue = result;
	iDisposeValue = 0;
	return update(self);
}

imeth	gUnattach()
{
	if (iDisposeValue  ||  !iValue)
		return self;
	iValue = gCopy(iValue);
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

imeth	gSelect()
{
	if (iHCtl)
#ifdef	WIN32
		SendMessage(iHCtl, EM_SETSEL, (WPARAM)0, (LPARAM) -1);
#else
		SendMessage(iHCtl, EM_SETSEL, (WPARAM)0, MAKELPARAM(0, -1));
#endif
	return self;
}

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
}

imeth	gDialog, gGetParent ()
{
	return iDlg;
}

imeth	gUseDefault()
{
	if (!iValue)
		iValue = gNew(Date);
	if (iDefault)
		gChangeValue(iValue, iDefault);
	else
		gChangeLongValue(iValue, 0);
	if (iSI)
		gUpdate(iSI);
	return update(self);
}

imeth	gSetDefaultLong(long val)
{
	if (iDefault)
		gDispose(iDefault);
	iDefault = gNewWithLong(LongInteger, val);
	return self;
}

imeth	gGetDefault()
{
	return iDefault;
}

imeth	int	gHideData(int flg)
{
	int	r = iHideData;
	if (r == 0  ||  r == 1)
		iHideData = flg;
	return r;
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

imeth	gFormatDate(char *fmt)
{
	gChangeStrValue(iDateFmt, fmt);
	return self;
}

imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg);
}

cmeth	int	gSetCutoffYear(int y)
{
	int	p = cCutoff;
	object	td = gToday(Date);
	int	c = gLongValue(td) / 10000;
	int	ty = c % 100;
	int	x = y - ty - 1;

	if (x < 0)
		x += 100;

	cCutoff = y;

	cCentury = (c - (100 - x)) / 100;

	gDispose(td);
	return p;
}

imeth	int	gSetCutoffYear(int y)
{
	int	p = iCutoff;
	object	td = gToday(Date);
	int	c = gLongValue(td) / 10000;
	int	ty = c % 100;
	int	x = y - ty - 1;

	if (x < 0)
		x += 100;

	iCutoff = y;
	iCentury = (c - (100 - x)) / 100;

	gDispose(td);
	return p;
}

static	int	makeYear(ivType *iv, int y)
{
	if (iCutoff  &&  y < iCutoff)
		return y + ((iCentury + 1) * 100);
	else if (y < 200)
		return y + (iCentury * 100);
	else
		return y;
}

imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_DATE_t	*v = vp;
	int	height, width, xPos, yPos, len;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	object	fobj = gGetFont(self);

	gGetSize(self, &height, &width);
	gGetVirtualPosition(self, &yPos, &xPos);
	strncpy(v->name, gName(self), (sizeof(v->name)-1));
	v->height = height;
	v->width  = width;
	v->xPos   = xPos;
	v->yPos   = yPos;
	v->hidden = gHiddenStatus(self) == 1 ? 'Y' : 'N';
	v->disabled = gDisableStatus(self) == 1 ? 'Y' : 'N';
	v->minimum = iMinimum;
	v->maximum = iMaximum;
	v->defaultVal = iDefault ? gLongValue(iDefault) : 0L;
	strcpy(v->format, gStringValue(iDateFmt));
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
	CTLTYPE_DATE_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	object	td = gToday(Date);

	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gSetName(self, v->name);
	iMinimum = v->minimum ? v->minimum : gLongValue(gAddYears(td, -100));
	iMaximum = v->maximum ? v->maximum : gLongValue(gAddYears(td, 200));
	gDispose(td);
	if (iDefault)
		iDefault = gDispose(iDefault);
	if (v->defaultVal)
		iDefault = gNewWithLong(LongInteger, v->defaultVal);
	gChangeStrValue(iDateFmt, v->format);
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_DATE_t	v;
	short	type = CTLTYPE_DATE, size = sizeof(v);
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
	CTLTYPE_DATE_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];

	double controlScaleFactor;
	
	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);

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
		
			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(abs(MulDiv(v.height-4, 72, logPixelsy)), v.fontSize);
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddDateControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

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
	
	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent)) {
		object	td = gToday(Date);
		gSetValue(ctl, td);
		gDispose(td);
		gShow(ctl);
	}
	return ctl;
}


cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_DATE_t  v;
	int             end;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];

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
	ctl = gAddDateControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

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
	
	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent)) {
		object	td = gToday(Date);
		gSetValue(ctl, td);
		gDispose(td);
		gShow(ctl);
	}
	return ctl;
}


////////////////////////////////////////////////////////////////////////////
// gPrintCtlScreen: print the text of the control at the correct location
// printerObj:      the printer object
// dScaleX:         the scaling factor for the x coordinate of the control, 
//	                dScaleX = printerLogPixelsX/(double)viewLogPixelsX;
// dScaleY:         the scaling factor for the y coordinate of the control, 
//                  dScaleY = printerLogPixelsY/(double)viewLogPixelsY
//
////////////////////////////////////////////////////////////////////////////
imeth	gPrintCtlScreen(object printerObj, double dScaleX, double dScaleY, int nViewOffsetX, int nViewOffsetY)
{
	RECT    rect;
	HDC     hdcPrinter;

	object  fontObj, parentObj;
	double  dScaleXX, dScaleYY;
	int     nPhysicalWidth, nPhysicalHeight, nPrinterOffsetX, nPrinterOffsetY, nTmp;
	DWORD   dwMargin;

	// check input parameters:
	if(!printerObj)    // validate the printerObj
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) )  // get and validate the print DC
		return NULL;

	if(dScaleX<=0 || dScaleY<=0)  // validate the view to printer scaling factors
		return NULL;

	if(gHiddenStatus(self)==1)  // skipping printing
		return self;

	if(!iValue)
		return self;

	parentObj=gGetParent(self);
	if(!parentObj)
		return NULL;

	fontObj = gGetFont(self);
	if(!fontObj)
		return NULL;

	gCLDGetPhysicalParameters(printerObj, &nPhysicalWidth, &nPhysicalHeight, &nPrinterOffsetX, &nPrinterOffsetY);

	// get the rect of the control 
	SetRectEmpty(&rect);
	gGetWindowRect(self, &rect);

	dScaleXX = dScaleX;
	dScaleYY = dScaleY;
	if( !gGetScaleFlg(parentObj) ) {  // the scaling flag is not set (for Integra)
		// scale the rect from view DC to the printer DC
		rect.left   = (rect.left   + nViewOffsetX) * dScaleX - nPrinterOffsetX + 0.5;
		rect.right  = (rect.right  + nViewOffsetX) * dScaleX - nPrinterOffsetX + 0.5;
		rect.top    = (rect.top    + nViewOffsetY) * dScaleY - nPrinterOffsetY + 0.5;
		rect.bottom = (rect.bottom + nViewOffsetY) * dScaleY - nPrinterOffsetY + 0.5;
	}
	else {  // do the MFMargin shift and extra scaling if the scaling flag is set (for Fsi)
		double MFCxScale=1.0, MFCyScale=1.0, controlScale=1.0, dblTmp;
		RECT   MFMarginRect;
	
		gGetMFCxCyScale(parentObj, &MFCxScale, &MFCyScale);
		SetRectEmpty(&MFMarginRect);
		gGetMFMargins(parentObj, &MFMarginRect);
		controlScale = gGetControlScale(parentObj);

		// scale the rect to the printer DC
		if( (dblTmp=controlScale*MFCxScale) > 0 ) {
			rect.left   = ( (rect.left  + nViewOffsetX - 0.5)/dblTmp - MFMarginRect.left )*dScaleX - nPrinterOffsetX + 0.5;
			rect.right  = ( (rect.right + nViewOffsetX - 0.5)/dblTmp - MFMarginRect.left )*dScaleX - nPrinterOffsetX + 0.5;
			dScaleXX = dScaleX/dblTmp;
		}
		else
			return NULL;

		if( (dblTmp=controlScale*MFCyScale) > 0 ) {
			rect.top    = ( (rect.top    + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY + 0.5;
			rect.bottom = ( (rect.bottom + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY + 0.5;
			dScaleYY = dScaleY/dblTmp;
		}
		else
			return NULL;
	}
  	
#ifdef WIN32  	
	// exclude the margins at the beginning and in the end of the rectangle
	dwMargin = SendMessage(gHandle(self), EM_GETMARGINS, (WPARAM)NULL, (LPARAM)NULL);
	rect.left  += LOWORD(dwMargin) * dScaleXX + 0.5;
	rect.right -= HIWORD(dwMargin) * dScaleXX + 0.5;
#endif

	{ // turn the page if necessary
		int nPage, nPage2, nTopMargin, nBottomMargin;

		gGetPageMargins(parentObj, &nTopMargin, &nBottomMargin);
		nTopMargin    *= dScaleY;
		nBottomMargin *= dScaleY;

		nPage = gGetPage(parentObj);
		nTmp = nPhysicalHeight - nTopMargin - nBottomMargin - nPrinterOffsetY;
		if(nTmp > 0)
			nPage2 = rect.bottom / nTmp;
		else
			nPage2 = nPage;

		if(nPage2 != nPage ) {
			gCLDEndPage(printerObj);
			gCLDStartPage(printerObj);
			nPage = gTurnPage(parentObj);
		}

		if(nPage>0) {
			rect.top    = rect.top    - nPage*nTmp + nTopMargin + nPrinterOffsetY;
			rect.bottom = rect.bottom - nPage*nTmp + nTopMargin + nPrinterOffsetY;
		}
	}

	// draw the text
	pFormatAndPrintText(self, hdcPrinter, dScaleXX, dScaleYY, &rect);

	return self;
}



/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the date control of the cld file for printing
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_DATE_t    v;
	short             size;
	char              *p, buf[BUFLEN];
	object	          ctl = NULL;
	ivType            *iv = NULL;
	object            today;

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

	ctl = gCLDNewWindowControl(DateControl, v.name, parentObj);
	iv = ivPtr(ctl);

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

	gSetControlParameters(ctl, &v);
	today = gToday(Date);
	gSetValue(ctl, today);
	today = gDispose(today);

	return ctl;
}


cmeth	gCLDNewWindowControl(char *name, object parentObj)
{
	object	obj = gCLDNewCont(super, name, "edit");
	ivType	*iv = ivPtr(obj);
	object	td = gToday(Date);

	iDlg = parentObj;
	iWindowControl = 0;
	iCtlID = 0;
	iState = NEW_STATE;
	iMinimum = gLongValue(gAddYears(td, -100));
	iMaximum = gLongValue(gAddYears(td, 200));
	gDispose(td);
	iAllowNone = 1;
	iDisposeValue = 1;
	iDateFmt = gNewWithStr(String, "%m %d, %Y");
	iCutoff = cCutoff;
	iCentury = cCentury;

	return obj;
}



/////////////////////////////////////////////////////////////////////////////////
// pFormatAndPrintText: format and draw text of the control to support WYSIWYG.
//
// HDC    hdcPrinter: a handle to the printer DC
// double dScaleX:    the horizontal scaling factor from view to printer
// double dScaleY:    the vertical scaling factor from view to printer
// RECT   *pRect:     the rectangle of the control in printer unit
//             
/////////////////////////////////////////////////////////////////////////////////
private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect)
{
	object   strObj, fontObj;
	HFONT    hfontOld, hfontView;
	HWND     hwnd;
	HDC      hdcView;
	LOGFONT  logfont;
	char     *strValue;
	int      nAveCharWidth;

	if(!hdcPrinter || pRect)
		return -1;

	// get the string value
	if(!iValue)
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	// get the font of the control 
	hfontView = gHandle(fontObj);
	if(!hfontView)
		return -1;

	if(iDateFmt)  // construct a formatted string object
		strObj = fmtDate(iValue, gStringValue(iDateFmt));

	// get a point to the string
	strValue = strObj ? gStringValue(strObj) : gStringValue(iValue);
	if(!strValue || !(*strValue)) {
		if(strObj)  // destroy the string object
			strObj = gDispose(strObj);
		return -1;
	}

	hwnd = gHandle(self);
	hdcView = GetDC(hwnd);  
	hfontOld = SelectObject(hdcView, hfontView);

	GetObject(hfontView, sizeof(LOGFONT), &logfont);
	logfont.lfWidth  = logfont.lfWidth*dScaleX + 0.5;
	logfont.lfHeight = -(abs(logfont.lfHeight)*dScaleY + 0.5);
	nAveCharWidth = gAveCharWidth(fontObj)*dScaleX + 0.5;

	gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, pRect, strValue, DT_LEFT, nAveCharWidth);  // align left

	SelectObject(hdcView, hfontOld);
	ReleaseDC(hwnd, hdcView);

	if(strObj)  // destroy the string object
		strObj = gDispose(strObj);

	return 0;
}


imeth gWriteXML(FILE *fp)
{

	CTLTYPE_DATE_t	v;
	char		buf[1024];
	object		fnt = gGetFont(self);

	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<date>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<defaultVal>%ld</defaultVal>\n",v.defaultVal);
	fprintf(fp,"\t\t\t<format>%s</format>\n",gStringToXML(XMLNode,buf,v.format));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<fontname>%s</fontname>\n",gStringToXML(XMLNode,buf,gName(fnt)));
	fprintf(fp,"\t\t\t<minimum>%ld</minimum>\n",v.minimum);
	fprintf(fp,"\t\t\t<maximum>%ld</maximum>\n",v.maximum);
	fprintf(fp,"\t\t\t<fontNameLen>%d</fontNameLen>\n",v.fontNameLen);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	fprintf(fp,"\t\t</date>\n");

	return self;
}


cmeth	gLoadControlFromXML(curnode, parent)
{
	CTLTYPE_DATE_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];
	double controlScaleFactor;
	
	char temp[BUFLEN];

	int loop;

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
	v.minimum=gGetFloatFromNode(curnode,"minimum");
	v.maximum=gGetFloatFromNode(curnode,"maximum");
	gPopulateStringFromNode(curnode,temp,"format");
	v.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	
	for (loop=0;loop<20;loop++)
		v.format[loop]=temp[loop];


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
		
			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(abs(MulDiv(v.height-4, 72, logPixelsy)), v.fontSize);
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddDateControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	// helpTopic
	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"helpTopic");
		gSetTopic(ctl, p);
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

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
	
	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent)) {
		object	td = gToday(Date);
		gSetValue(ctl, td);
		gDispose(td);
		gShow(ctl);
	}
	
	
	return ctl;
}




