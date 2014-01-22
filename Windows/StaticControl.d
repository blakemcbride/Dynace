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



#include "logfile.h"
#include "hdlcache.h"

defclass  StaticControl : Control  {
	HWND	iHCtl;		/*  handle to edit control  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	iValue;			/*  field value			*/
};


#include <ctype.h>


cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iDlg = dlg;
	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "static", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iCtlID = ctlID;

	gSetStyle(obj, WS_VISIBLE);

	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

imeth	int	gShow()
{
	object	fobj = gGetFont(self);

	if (iHCtl)
		return 0;
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

	if (fobj)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(fobj), (LPARAM) 0);
	
	if (iValue)
		SetWindowText(iHCtl, (LPCSTR) gStringValue(iValue));

	gInitialize(super, (HWND)0, NULL);
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	object	font = gGetFont(self);
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "StaticControl control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	if (iValue)
/*		SetDlgItemText(hDlg, iCtlID, (LPCSTR) gStringValue(iValue));  */
		SetWindowText(iHCtl, (LPCSTR) gStringValue(iValue));
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (iValue)
		gDispose(iValue);
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

imeth	gValue : Value ()
{
	if (!iValue)
		iValue = gNew(String);
	return iValue;
}

imeth	char	*gStringValue()
{
	return gStringValue(Value(self));
}

imeth	gSetValue : SetValue (val)
{
	ChkArg(val, 2);
	if (!iValue)
		iValue = gNewWithObj(String, val);
	else
		gChangeValue(iValue, val);
	if (iDlg  &&  gInDialog(iDlg))
		SetWindowText(iHCtl, (LPCSTR) gStringValue(val));
	return self;
}

imeth	gSetStringValue, gSetTitle (char *val)
{
	if (!iValue)
		iValue = gNewWithStr(String, val);
	else
		gChangeStrValue(iValue, val);
	if (iDlg  &&  gInDialog(iDlg))
		SetWindowText(iHCtl, (LPCSTR) val);
	return self;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHCtl;
}

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
}

imeth	gDialog, gGetParent ()
{
	return iDlg;
}









