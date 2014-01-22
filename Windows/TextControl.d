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

#include "hdlcache.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "ctlsave.h"

/* The following defines are also defined in string.d.
   If they are changed here, they will need to be changed
   there as well. */
#define MASK_MAX	128
#define	MASK_BASE	(unsigned char) 128
#define MASK_LEFT	'>'
#define MASK_RIGHT	'<'
#define MASK_FILLER	'_'	/*  This is specific to the text control	*/



defclass  TextControl : SpinControl  {
	HWND	iHCtl;		/*  handle to edit control  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	iWindowControl; /*  1=window control		*/
	iValue;			/*  field value			*/
	iDefault;		/*  default value		*/

	DWORD	iTextCtlStyle;    // Yanghui

	int	iDisposeValue;	/*  1=auto dispose of iValue	*/
	int	(*iAcf)();	/*  aux checking function	*/
	int	(*iDCFun)();	/*  function - double click	*/

	iSI;			/*  ODBC StatementInfo		*/

	int	iCap;		/*  capitilize string		*/
	int	iMaxLen;	/*  maximum string length	*/
	int	iAbsoluteMaxLen;/*  absolute maximum length     */
	int	iMinLen;	/*  minimum string length	*/

	iBuf;			/*  Field entry buffer		*/
	iMask;			/*  Data Mask			*/
	int	iRightMask;	/*  1 = Read from right to left */
	int	iHideData;	/*  don't show data - show ***  */
	int	iPasswordFlag;  /* flag to mark password field */
	int	iHtmlFlag;      /* flag to mark html formatted field */

	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/

	int	iReturnMask;
	int	(*iMFun)();

class:
	char		*cBuf;
	unsigned	cBufLen;
	char		cMaskBuf[MASK_MAX];
};

static	char	*applyMask(ivType *iv, char *inmask, char *intext);
static	char	*removeMask(ivType *iv);

static	char   *rs(char *s);
static	char   *cs(char *s);
static	void	expand_buf(int len);

private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect);
				   
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
	if (iCap)
		wParam = toupper(wParam);
	if (iMask) {
		int	len = GetWindowTextLength(iHCtl) + 1;
		int	i = start, j, hold;
		char	*mp = gStringValue(iMask);
		int	right = *mp++ == MASK_RIGHT;
		int	inc = wParam == '\b' ? -1 : 1;

		if (wParam == ' ') { /* space should simulate the delete key here */
#ifdef	WIN32
			SendMessage(iHCtl, WM_KEYDOWN, (WPARAM) VK_DELETE, (LPARAM) 0);
#else
			SendMessage(iHCtl, WM_KEYDOWN, (WPARAM) VK_DELETE, MAKELPARAM(0, 0));
#endif
			return 0L;
		}
		
		if (start == len - 1 && wParam != '\b' || !start && !end && wParam == '\b' ) {
			MessageBeep(-1);
			return 0L;
		}

		expand_buf(len);
		if (!GetWindowText(iHCtl, (LPSTR) cBuf, len))
			*cBuf = '\0';

		if (start == end && wParam == '\b') {
			hold = i;
			for (i--; i >= 0 && (unsigned char) mp[i] < MASK_BASE; i--);
			if (i >= 0)
				cBuf[i] = MASK_FILLER;
			else
				i = hold;
		} else if (wParam == '\b') {
			while (i < end) {
				if ((unsigned char) mp[i] >= MASK_BASE)
					cBuf[i] = MASK_FILLER;
				i++;
			}
			i = start;
			for ( ; mp[i]  &&  (unsigned char) mp[i] < MASK_BASE; i++);
		} else {
			int	ok = 1;
			
			for ( ; mp[i]  &&  (unsigned char) mp[i] < MASK_BASE; i++);
			if (mp[i] && (unsigned char) mp[i] >= MASK_BASE) {
				ifun	fun = gMaskFunction(String, mp[i]);
				
				if (fun(wParam))
					cBuf[i++] = wParam;
				else
					ok = 0;
			} else
				ok = 0;

			if (!ok) {
				MessageBeep(-1);
				return 0L;
			}

			if (start != end) {
				hold = i;
				
				while (i < end) {
					if ((unsigned char) mp[i] >= MASK_BASE)
						cBuf[i] = MASK_FILLER;
					i++;
				}
				i = hold;
			}
		}

		hold = i;
		for (j = inc < 0 ? wParam != '\b' ? i - 1 : i : i; (inc < 0 && i || inc > 0 && i < len - 1) &&
			     (unsigned char) mp[j] < MASK_BASE; i += inc, j += inc);
		
		if ((unsigned char) mp[j] < MASK_BASE)
			i = hold;

		if (iBuf)
			gChangeStrValue(iBuf, cs(cBuf));
		else
			iBuf = gNewWithStr(String, cs(cBuf));
		if (iValue)
			gChangeStrValue(iValue, removeMask(iv));
		else
			iValue = gNewWithStr(String, removeMask(iv));
		SetWindowText(iHCtl, gStringValue(iBuf));

#ifdef	WIN32
		SendMessage(iHCtl, EM_SETSEL, (WPARAM)i, (LPARAM) i);
#else
		SendMessage(iHCtl, EM_SETSEL, (WPARAM)1, MAKELPARAM(i, i));
#endif
		return 0L;
	}
	if (start == end  &&  wParam != '\b'  &&  iMaxLen  &&  iMaxLen <= GetWindowTextLength(iHCtl))  {
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
	if (iMask) {
		int	shift = GetKeyState(VK_SHIFT) < 0;
		DWORD	r = SendMessage(iHCtl, EM_GETSEL, (WPARAM)0, (LPARAM) 0);
		WORD	start = LOWORD(r);
		WORD	end   = HIWORD(r);
		int	len = GetWindowTextLength(iHCtl) + 1;
		int	i = start;
		char	*mp = gStringValue(iMask) + 1;
		int	dochange = 0;

		expand_buf(len);
		if (!GetWindowText(iHCtl, (LPSTR) cBuf, len))
			*cBuf = '\0';
		
		switch(wParam) {
		case VK_DELETE:
			if (shift) {
#ifdef	WIN32
				SendMessage(iHCtl, WM_COPY, (WPARAM) 0, (LPARAM) 0);
#else
				SendMessage(iHCtl, WM_COPY, (WPARAM) 0, MAKELPARAM(0, 0));
#endif
			}
			
			if (i == end && (unsigned char) mp[i] >= MASK_BASE)
				cBuf[i] = MASK_FILLER;
			else {
				while (i < end) {
					if ((unsigned char) mp[i] >= MASK_BASE)
						cBuf[i] = MASK_FILLER;
					i++;
				}
				i = start;
			}
 
			if (start == end && i < len - 1)
				i++;
			for ( ; i < len - 1 && (unsigned char) mp[i] < MASK_BASE; i++);

			dochange = 1;
			break;
		case VK_INSERT:
			if (shift) {
				HANDLE	hCB;
				char	clipbuf[MASK_MAX];
				char	result[MASK_MAX];
				int	j = 0;

				*clipbuf = '\0';
				
				OpenClipboard(iHCtl);
				if (hCB = GetClipboardData(CF_TEXT)) {
					char	*cbData = (char *) GlobalLock(hCB);

					while (j < MASK_MAX && (clipbuf[j] = *cbData++))
						j++;

					GlobalUnlock(hCB);

					if (j >= MASK_MAX)
						clipbuf[MASK_MAX - 1] = '\0';

				}
				CloseClipboard();
				
				strcpy(result, applyMask(iv, mp + start, clipbuf));

				if (start == end)
					for (j = 0; result[j] && result[j] != MASK_FILLER; j++)
						cBuf[i++] = result[j];
				else
					for (j = 0; result[j] && start + j < end; j++)
						cBuf[i++] = result[j];

				dochange = 1;
				shift = 0;
			}
			break;
		case VK_DOWN:
		case VK_RIGHT:
			if (!shift) {
				if (end <= len - 1)
					for (i = end + 1; i <= len - 1 && (unsigned char) mp[i] < MASK_BASE; i++);
				if (i > len - 1)
					i = len - 1;
			}
			break;
		case VK_UP:
		case VK_LEFT:
			if (!shift) {
				if (start)
					for (i = start - 1; i && (unsigned char) mp[i] < MASK_BASE; i--);
				for ( ; i < len - 1 && (unsigned char) mp[i] < MASK_BASE; i++);
			}
			break;
		case VK_HOME:
			if (!shift)
				for (i = 0; i < len - 1 && (unsigned char) mp[i] < MASK_BASE; i++);
			break;
		case VK_END:
			if (!shift)
				i = len - 1;
			break;
		default:
			shift = 1;  /* Set shift equal to one so the setsel call below won't happen */
			break;
		}

		if (dochange) {
			if (iBuf)
				gChangeStrValue(iBuf, cs(cBuf));
			else
				iBuf = gNewWithStr(String, cs(cBuf));
			if (iValue)
				gChangeStrValue(iValue, removeMask(iv));
			else
				iValue = gNewWithStr(String, removeMask(iv));
			SetWindowText(iHCtl, gStringValue(iBuf));
		}

		if (!shift) {
#ifdef	WIN32
			SendMessage(iHCtl, EM_SETSEL, (WPARAM) i, (LPARAM) i);
#else
			SendMessage(iHCtl, EM_SETSEL, (WPARAM) 1, MAKELPARAM(i, i));
#endif
			return 0L;
		}
	}
	
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	if (gIsKindOf(iDlg, Window))
		gSelect(self);
	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	if (iBuf)
		SetWindowText(iHCtl, gStringValue(gStripCenter(iBuf)));

#ifdef	WIN32
	SendMessage(iHCtl, EM_SETSEL, (WPARAM) 0, (LPARAM) -1);
#else
	SendMessage(iHCtl, EM_SETSEL, (WPARAM) 1, MAKELPARAM(0, -1));
#endif
	/*  without the following line, if you attached a function to a field which got executed from the killfocus
	    and the function created a new dialog then when you returned there was no caret in the control.  */
	gCreateCaret(self);
	return 0L;
}

private	imeth	long	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	int	len = GetWindowTextLength(iHCtl) + 1;

	expand_buf(len);
	if (!GetWindowText(iHCtl, (LPSTR) cBuf, len))
		*cBuf = '\0';
	if (iBuf)
		gChangeStrValue(iBuf, cs(cBuf));
	else
		iBuf = gNewWithStr(String, cs(cBuf));
	if (iValue)
		gChangeStrValue(iValue, removeMask(iv));
	else
		iValue = gNewWithStr(String, removeMask(iv));
	SetWindowText(iHCtl, gStringValue(iBuf));
	if (iSI)
		gUpdate(iSI);
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
		if (gExecuteSetFunctions(self, iDlg))
			PostMessage(gHandle(iDlg), WM_NEXTDLGCTL, (WPARAM) iHCtl, (LPARAM) TRUE);
	if (iMask) {
		object	sobj = gNewWithObj(String, iBuf);
		char	c = gSetMaskFiller(sobj, MASK_FILLER);

		gRemoveMask(sobj, gStringValue(iMask), NULL);
		gSetMaskFiller(sobj, c);
		gApplyMask(sobj, gStringValue(iMask), NULL);

		if (iMFun)
			iMFun(gStringValue(iMask), gStringValue(sobj), cMaskBuf);
		else
			strcpy(cMaskBuf, gStringValue(sobj));

		gChangeStrValue(sobj,cMaskBuf);

		gRemoveMask(sobj, gStringValue(iMask), NULL);
		c = gSetMaskFiller(sobj, MASK_FILLER);
		gApplyMask(sobj, gStringValue(iMask), NULL);
		gSetMaskFiller(sobj, c);

		gChangeStrValue(iBuf, gStringValue(sobj));
		if (iValue)
			gChangeStrValue(iValue, removeMask(iv));
		else
			iValue = gNewWithStr(String, removeMask(iv));
		gDispose(sobj);
	}
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
			sprintf(cmd, "(%s (int->object %ld) (int->object %ld))",
				gFunctionName(SchemeClassSurrogate, (object)iDCFun),
				(long) self, (long) iDlg);
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					  	    gNamespaceName(SchemeClassSurrogate, (object)iDCFun, ns), 
						    cmd);
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaCallbackClassSurrogate)
			gPerformJavaObjCallback((object)iDCFun, iDlg);
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

private	imeth	long	process_wm_paste(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	int len = 0;
	HGLOBAL hGlb = NULL;
	char *newVal = NULL, *clipData = NULL;

	if (iMaxLen != 0) {
		newVal = (char*)malloc(iMaxLen + 1);
		if (IsClipboardFormatAvailable(CF_TEXT)) {
			if (OpenClipboard(iHCtl)) {
				hGlb = GetClipboardData(CF_TEXT);
				clipData = GlobalLock(hGlb);
				if (clipData != NULL) {
					strncpy(newVal, clipData, iMaxLen);
					newVal[iMaxLen] = '\0';
				}
				GlobalUnlock(hGlb);
			}
			CloseClipboard();
		}
		if (strlen(newVal) > 0) {
			gSetStringValue(self, "");
			gSetStringValue(self, newVal);
		}
		free(newVal);
	}
	else
		gCallDefaultProc(self, mMsg, wParam, lParam);
	return 0L;
}

cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iDisposeValue = 1;
	iDlg = dlg;
	iReturnMask = 1;

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	gDefaultProcessingMode(obj, (unsigned) WM_KEYDOWN, 0);  /*  no auto default processing  */

	gAddHandlerBefore(obj, (unsigned) WM_PASTE, process_wm_paste);
	gDefaultProcessingMode(obj, (unsigned) WM_PASTE, 0);

	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "edit", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iWindowControl = 1;
	iCtlID = ctlID;
	iDisposeValue = 1;
	iReturnMask = 1;

	gSetStyle(obj, WS_VISIBLE); 
	iTextCtlStyle = ES_AUTOHSCROLL;     // Yanghui

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	gDefaultProcessingMode(obj, (unsigned) WM_KEYDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);
	gDefaultProcessingMode(obj, (unsigned) WM_RBUTTONDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_RBUTTONUP, process_wm_rbuttonup);
	gDefaultProcessingMode(obj, (unsigned) WM_RBUTTONUP, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONDBLCLK, 0);  /*  no auto default processing  */

	return obj;
}

imeth	gSetStyle(DWORD style)
{
	// Yanghui:
	if( (ES_AUTOHSCROLL & style) == ES_AUTOHSCROLL )
		iTextCtlStyle = ES_AUTOHSCROLL;  
	else
		iTextCtlStyle = ES_MULTILINE|ES_AUTOVSCROLL;
	// Yanghui

	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);  
	return gSetStyle(super, style);
}


// Yanghui:
imeth	DWORD gGetStyle()
{
	return (iTextCtlStyle | gGetStyle(super));
}
// Yanghui



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
	if (iBuf)
		if (iHideData)
			SetWindowText(iHCtl, "****************************");
		else
			SetWindowText(iHCtl, (LPCSTR) gStringValue(iBuf));
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
		sprintf(buf, "TextControl control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	if (iValue)
		gChangeStrValue(iValue, removeMask(iv));
	else
		iValue = gNewWithStr(String, removeMask(iv));
	if (iBuf)
		if (iHideData)
			SetWindowText(iHCtl, "****************************");
		else
			SetWindowText(iHCtl, (LPCSTR) gStringValue(iBuf));	
	if (iSI)
		gUpdate(iSI);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (iValue  &&  iDisposeValue)
		gDispose(iValue);
	if (iBuf)
		gDispose(iBuf);
	if (iDefault)
		gDispose(iDefault);
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

static	void	MakeValue(ivType *iv)
{
	int	len;
	
	if (!iDlg  ||  !iHCtl  ||  iHideData)
		return;
	len = GetWindowTextLength(iHCtl) + 1;
	expand_buf(len);
	if (!GetWindowText(iHCtl, (LPSTR) cBuf, len))
		*cBuf = '\0';
	if (iBuf)
		gChangeStrValue(iBuf, cs(cBuf));
	else
		iBuf = gNewWithStr(String, cs(cBuf));
	if (iValue)
		gChangeStrValue(iValue, removeMask(iv));
	else
		iValue = gNewWithStr(String, removeMask(iv));
	if (iSI)
		gUpdate(iSI);
}

imeth	int	gCheckValue()
{
	char	*buf = gGetBuf(Application);
	
	MakeValue(iv);
	if (iMinLen  &&  iMinLen > gSize(iValue))  {
		sprintf(buf, "%s must contain at least %d characters.", strlen(gName(self)) ? gName(self) : "Field", iMinLen);
//		MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
		gErrorMessage(Application, buf);  
		SetFocus(iHCtl);
		return 1;	/*  error  */
	}
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
	return 0;
}

imeth	gValue : Value ()
{
	if (iDlg  &&  gInDialog(iDlg))
		MakeValue(iv);
	if (!iValue)
		iValue = gNew(String);
	if (!iBuf) {
		iBuf = gNewWithObj(String, iValue);
		if (iMask)
			gApplyMask(iBuf, gStringValue(iMask), NULL);
	}
/*
	if (iMask) {
		object	sobj = gNewWithObj(String, iBuf);
		char	c = gSetMaskFiller(sobj, MASK_FILLER);

		gRemoveMask(sobj, gStringValue(iMask), NULL);
		gSetMaskFiller(sobj, c);
		gApplyMask(sobj, gStringValue(iMask), NULL);

		if (iMFun)
			iMFun(gStringValue(iMask), gStringValue(sobj), cMaskBuf);
		else
			strcpy(cMaskBuf, gStringValue(sobj));
		gChangeStrValue(iBuf, cMaskBuf);
		gDispose(sobj);
	}
	*/	
	return gStripCenter(iValue);
}

imeth	char	*gStringValue()
{
	return gStringValue(Value(self));
}

// Yanghui:
imeth	char	*gStringValueWithoutStripping()
{
	if(!iValue)
		return NULL;
	else
		return gStringValue(iValue);
}
// Yanghui

imeth	gCapitalize()
{
	iCap = 1;
	return self;
}

imeth	gMaxLength(int len)
{
	if (len < 0)
		iAbsoluteMaxLen = len = -len;
	if (iAbsoluteMaxLen  &&  len > iAbsoluteMaxLen)
		len = iAbsoluteMaxLen;
	iMaxLen = len;
	return self;
}

imeth	gMinLength(int len)
{
	iMinLen = len;
	return self;
}

imeth	int	gGetMinLength()
{
	return iMinLen;
}

imeth	gTextRange(int b, int e)
{
	if (e < 0)
		iAbsoluteMaxLen = e = -e;
	if (iAbsoluteMaxLen  &&  e > iAbsoluteMaxLen)
		e = iAbsoluteMaxLen;
	iMinLen = b;
	iMaxLen = e;
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
	if (iBuf)
		gChangeStrValue(iBuf, applyMask(iv, NULL, NULL));
	else
		iBuf = gNewWithStr(String, applyMask(iv, NULL, NULL));
	if (iDlg  &&  gInDialog(iDlg)  &&  iHCtl)
		if (iHideData)
			SetWindowText(iHCtl, "****************************");
		else
			SetWindowText(iHCtl, (LPCSTR) gStringValue(iBuf));
	return self;
}

imeth	gSetValue : SetValue (val)
{
	object	str = NULL;

	ChkArg(val, 2);
	if (iMaxLen  &&  gSize(val) > iMaxLen) {
		val = str = gCopy(val);
		gTake(str, iMaxLen);
	}
	if (!iValue)
		iValue = gNewWithObj(String, val);
	else
		gChangeValue(iValue, val);
	if (iSI)
		gUpdate(iSI);
	if (str)
		gDispose(str);
	return update(self);
}

imeth	gSetStringValue(char *val)
{
	object	str = NULL;
	
	if (iMaxLen  &&  strlen(val) > iMaxLen) {
		str = gNewWithStr(String, val);
		val = gStringValue(gTake(str, iMaxLen));
	}

	if (!iBuf)
		iBuf = gNewWithStr(String, val);
	else
		gChangeStrValue(iBuf, val);
	if (!iValue)
		iValue = gNewWithStr(String, val);
	else
		gChangeStrValue(iValue, val);
	if (iSI)
		gUpdate(iSI);

	if (str)
		gDispose(str);
	
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
	iAbsoluteMaxLen = 0;
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

imeth	gSetDefaultStr(char *str)
{
	if (iDefault)
		gChangeStrValue(iDefault, str);
	else
		iDefault = gNewWithStr(String, str);
	return self;
}

imeth	gGetDefault()
{
	return iDefault;
}

imeth	gUseDefault()
{
	if (iValue)
		if (iDefault)
			gChangeValue(iValue, iDefault);
		else
			gChangeStrValue(iValue, "");
	else
		if (iDefault)
			iValue = gNewWithObj(String, iDefault);
		else
			iValue = gNew(String);
	if (iSI)
		gUpdate(iSI);
	return update(self);
}

static	char   *cs(char *s)
{
	int i, j, flg;

	if (!s)
		return (s);
	for (j = i = 0, flg = 1; s[i]; i++)
		if (flg)
			if (s[i] == ' ')
				j++;
			else
				flg = 0;
	i -= j + 1;
	memmove(s, s + j, i + 2);
	for (; i >= 0 && s[i] == ' '; i--);
	s[i + 1] = '\0';
	return s;
}

static	char   *rs(char *s)
{
	char	*p = NULL, *b = s;

	for ( ; *s ; s++)
		if (*s == ' ') {
			if (!p)
				p = s;
		} else
			p = NULL;
	if (p)
		*p = '\0';
	return b;
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

static	char	*applyMask(ivType *iv, char *inmask, char *intext)
{
	char	*mask;
	char	*text;
	object	sobj;
	char	buf[MASK_MAX];

	if (intext)
		text = intext;
	else if (iValue)
		text = gStringValue(iValue);
	else
		text = "";
	
	if (!iMask)
		return text;

	sobj = gNew(String);

	*buf = MASK_LEFT;
	if (inmask && (*inmask == MASK_LEFT || *inmask == MASK_RIGHT))
		strcpy(buf, inmask);
	else if (inmask)
		strcpy(buf + 1, inmask);
	else
		strcpy (buf, gStringValue(iMask));

	mask = buf;

	gSetMaskFiller(sobj, MASK_FILLER);
	
	gApplyMask(sobj, mask, text);
	strcpy(cMaskBuf, gStringValue(sobj));
	gDispose(sobj);

	return cMaskBuf;
}

static	char	*removeMask(ivType *iv)
{
	object	sobj;
	char	*mask;
	char	*text = iBuf ? gStringValue(iBuf) : "";
	char	c;

	if (!iMask)
		return text;

	sobj = gNew(String);
	mask = gStringValue(iMask);

	c = gSetMaskFiller(sobj, MASK_FILLER);
	
	gRemoveMask(sobj, mask, text);

	if (iReturnMask) {
		gSetMaskFiller(sobj, c);
		gApplyMask(sobj, mask, NULL);
		
		if (iMFun)
			iMFun(gStringValue(iMask), gStringValue(sobj), cMaskBuf);
		else
			strcpy(cMaskBuf, gStringValue(sobj));
	}
	else
		strcpy(cMaskBuf, gStringValue(sobj));

	gDispose(sobj);
	
	return cMaskBuf;
}

static	void	expand_buf(int len)
{
	if (cBufLen > len)
		return;
	if (cBufLen)
		cBuf = realloc(cBuf, len);
	else
		cBuf = malloc(len);
	cBufLen = len;
	if (!cBuf)
		gError(Application, "Out of memory.");
}

imeth	int	gHideData(int flg)
{
	int	r = iHideData;
	if (r == 0  ||  r == 1)
		iHideData = flg;
	return r;
}

imeth	int	gReturnMask(int val)
{
	int	pv = iReturnMask;

	iReturnMask = val;
	return pv;
}

imeth	gSetMaskFunction(ifun fun)
{
	iMFun = fun;
	return self;
}

static	maskFormatPhone(char *mask, char *in, char *out)
{
	object	sobj = gNewWithStr(String, in);

	gStripCenter(gRemoveMask(sobj, mask, NULL));

	if (gSize(sobj) == 7 || !gSize(sobj)) {
		strcpy(out, gStringValue(sobj));
		out[8] = '\0';
		out[7] = out[6];
		out[6] = out[5];
		out[5] = out[4];
		out[4] = out[3];
		out[3] = '-';
	} else {
		gApplyMask(sobj, mask, NULL);
		strcpy(out, gStringValue(sobj));
	}

	gDispose(sobj);

	return 0;
}

static	maskFormatZipCode(char *mask, char *in, char *out)
{
	object	sobj = gNewWithStr(String, in);

	gStripCenter(gRemoveMask(sobj, mask, NULL));

	if (gSize(sobj) == 5 || !gSize(sobj))
		strcpy(out, gStringValue(sobj));
	else {
		gApplyMask(sobj, mask, NULL);
		strcpy(out, gStringValue(sobj));
	}

	gDispose(sobj);
	return 0;
}

static	maskFormatSSNumber(char *mask, char *in, char *out)
{
	object	sobj = gNewWithStr(String, in);

	gStripCenter(gRemoveMask(sobj, mask, NULL));

	if (!gSize(sobj))
		*out = '\0';
	else {
		gApplyMask(sobj, mask, NULL);
		strcpy(out, gStringValue(sobj));
	}

	gDispose(sobj);
	return 0;
}

imeth	gSetMask(char *mask)
{
	if (iMask)
		iMask = gDispose(iMask);

	if (mask && *mask)
		iMask = gNewWithStr(String, gLoadMask(String, mask));
	
	return update(self);
}

imeth	gStandardMask(int val)
{
	switch(val) {
	case MASK_FORMAT_NONE:
		iMFun = NULL;
		break;
	case MASK_FORMAT_PHONE:
		gSetMask(self, "<(###) ###-####");
		iMFun = maskFormatPhone;
		break;
	case MASK_FORMAT_ZIPCODE:
		gSetMask(self, "#####-####");
		iMFun = maskFormatZipCode;
		break;
	case MASK_FORMAT_SSNUMBER:
		gSetMask(self, "###-##-####");
		iMFun = maskFormatSSNumber;
		break;
	default:
		iMFun = NULL;
		break;
	}
	return self;
}

imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg);
}

imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_TEXT_t	*v = vp;
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
	v->minLen = iMinLen;
	v->maxLen = iMaxLen;
	v->capitalize = iCap;

	v->textCtlStyle = iTextCtlStyle;  // Yanghui

	if (iDefault  &&  (len=gSize(iDefault)))
		v->defaultLen = len + 1;
	else
		v->defaultLen = 0;
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
	CTLTYPE_TEXT_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gSetName(self, v->name);
	iMinLen = v->minLen;
	iMaxLen = v->maxLen;
	iCap    = v->capitalize;

	iTextCtlStyle = v->textCtlStyle;
	gSetStyle(self, (WS_VISIBLE | iTextCtlStyle) & ~WS_BORDER);  // Yanghui

	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_TEXT_t	v;
	short	type = CTLTYPE_TEXT, size = sizeof(v);
	object	fobj = gGetFont(self);

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.defaultLen  &&  1 != fwrite(gStringValue(iDefault), (int) v.defaultLen, 1, fp))
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
	CTLTYPE_TEXT_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];

	double controlScaleFactor;  // Yanghui

	if (1 != fread(&size, sizeof size, 1, fp)) 
		return NULL; 
	if (size < sizeof(v)) {
		memset(&v, 0, sizeof v); 
		v.textCtlStyle = ES_AUTOHSCROLL;
	}

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
	// Yanghui


	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddTextControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

	if (v.defaultLen) {
		p = v.defaultLen > BUFLEN ? malloc((unsigned)v.defaultLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.defaultLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetDefaultStr(ctl, p);
		if (v.defaultLen > BUFLEN)
			free(p);
	}

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
			//	gSetFont(ctl, vNew(ExternalFont, "Arial", v.fontSize));
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
	
	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent)) {
		gSetStringValue(ctl, "Text");
		gShow(ctl);
	}

	return ctl;
}


// Yanghui:
cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_TEXT_t  v;
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
	ctl = gAddTextControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

	if (v.defaultLen) {
		p = v.defaultLen > BUFLEN ? malloc((unsigned)v.defaultLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.defaultLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetDefaultStr(ctl, p);
		if (v.defaultLen > BUFLEN)
			free(p);
	}

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
		gSetStringValue(ctl, "Text");
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
	char    *strValue=NULL;
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

	// draw text according to the selected font
	pFormatAndPrintText(self, hdcPrinter, dScaleXX, dScaleYY, &rect);

	return self;
}

cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_TEXT_t    v;
	short             size;
	char              *p, buf[BUFLEN];
	object	          ctl = NULL;
	ivType            *iv = NULL;

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

	ctl = gCLDNewWindowControl(TextControl, v.name, parentObj);
	iv = ivPtr(ctl);

	if (v.defaultLen) {
		p = v.defaultLen > BUFLEN ? malloc((unsigned)v.defaultLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.defaultLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetDefaultStr(ctl, p);
		if (v.defaultLen > BUFLEN)
			free(p);
	}

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
	gSetStringValue(ctl, v.name);
	return ctl;
}


///////////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the text control of the cld file for printing
//
///////////////////////////////////////////////////////////////////////////////
cmeth	gCLDNewWindowControl(char *name, object parentObj)
{
	object	obj = gCLDNewCont(super, name, "edit");
	ivType	*iv = ivPtr(obj);

	iDlg = parentObj;
	iWindowControl = 0;
	iCtlID = 0;
	iDisposeValue = 1;
	iReturnMask = 0;

	return obj;
}


////////////////////////////////////////////////////////////////////////////////////////
// pFormatAndPrintText: format and draw text of the control to support WYSIWYG.
//
// HDC    hdcPrinter: a handle to the printer DC
// double dScaleX:    the horizontal scaling factor from view to printer
// double dScaleY:    the vertical scaling factor from view to printer
// RECT   *pRect:     the rectangle of the control in printer unit
//             
////////////////////////////////////////////////////////////////////////////////////////
private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect)
{
	object   fontObj;
	HFONT    hfontOld, hfontView;
	HWND     hwnd;
	HDC      hdcView;
	LOGFONT  logfont;
	char     *strValue;
	int      nAveCharWidth;

	if(!hdcPrinter || !pRect)
		return -1;

	// get the string value
	if(!iValue)
		return -1;

	strValue = gStringValue(iValue);
	if(!strValue || !(*strValue)) 
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	// get the font of the control 
	hfontView = gHandle(fontObj);
	if(!hfontView)
		return -1;

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

	return 0;
}

// Yanghui
 
imeth gWriteXML(FILE *fp)
{
	CTLTYPE_TEXT_t	v;
	char		*cp, buf[1024];
	object		dobj = gGetDefault(self);
	object		fnt = gGetFont(self);

	gGetControlParameters(self, &v);

	if (dobj)
		cp = gStringValue(dobj);
	else
		cp = "";


	fprintf(fp,"\t\t<text>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<text>%s</text>\n",gStringToXML(XMLNode,buf,cp));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<fontname>%s</fontname>\n",gName(fnt));
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<textCtlStyle>%d</textCtlStyle>\n",v.textCtlStyle);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<minLen>%d</minLen>\n",v.minLen);
	fprintf(fp,"\t\t\t<maxLen>%d</maxLen>\n",v.maxLen);
	fprintf(fp,"\t\t\t<capitalize>%d</capitalize>\n",v.capitalize);
	fprintf(fp,"\t\t\t<defaultLen>%d</defaultLen>\n",v.defaultLen);
	fprintf(fp,"\t\t\t<fontNameLen>%d</fontNameLen>\n",v.fontNameLen);
	fprintf(fp,"\t\t\t<passwordFlag>%d</passwordFlag>\n",gPasswordFlag(self));
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	fprintf(fp,"\t\t</text>\n");

	return self;
} 

cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_TEXT_t	v;
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
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	v.minLen=gGetIntFromNode(curnode,"minLen");
	v.maxLen=gGetIntFromNode(curnode,"maxLen"); 
	v.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	v.capitalize=gGetIntFromNode(curnode,"capitalize");
	v.defaultLen=gGetIntFromNode(curnode,"defaultLen");
	v.textCtlStyle=gGetIntFromNode(curnode,"textCtlStyle");
	
	

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
	// Yanghui


	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddTextControl(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name);

	gPopulateStringFromNode(curnode, buf, "xpath");
	gSetXPathBinding(ctl,buf);
	gSetPasswordFlag(ctl, gGetIntFromNode(curnode, "passwordFlag")); 

	if (v.defaultLen) {
		p = v.defaultLen > BUFLEN ? malloc((unsigned)v.defaultLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"text");
		gSetDefaultStr(ctl, p);
		if (v.defaultLen > BUFLEN)
			free(p);
	}

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
	
	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent)) {
		gSetStringValue(ctl, "Text");
		gShow(ctl);
	}
	
	return ctl;
}

imeth	int	gSetPasswordFlag(int pword)
{
	int	pval = iPasswordFlag;
	iPasswordFlag = pword;
	return pval;
}

imeth	int	gPasswordFlag()
{
	return iPasswordFlag;
}

imeth	int	gSetHTMLFlag(int htmlFlg)
{
	int	pval = iHtmlFlag;
	iHtmlFlag = htmlFlg;
	return pval;
}

imeth	int	gHTMLFlag()
{
	return iHtmlFlag;
}




