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

#define MAX(a, b)	((a) > (b) ? (a) : (b))
#define MIN(a, b)	((a) < (b) ? (a) : (b))

defclass  ScrollBar : Control  {
	HWND	iHCtl;		/*  handle to control	  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	iValue;			/*  field value			*/
	iDefault;		/*  default value		*/
	int	iDisposeValue;	/*  1=auto dispose of iValue	*/
	int	(*iAcf)();	/*  aux checking function	*/
	int	(*iFun)();	/*  alt control function	*/

	iSI;			/*  ODBC StatementInfo		*/

	int	iMinimum;
	int	iMaximum;
	int	iLineInc;
	int	iPageInc;
	int	iPos;
};


#include <ctype.h>


cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iMinimum = 0;
	iMaximum = 100;
	iPageInc = 10;
	iLineInc = 2;
	iDisposeValue = 1;
	iDlg = dlg;
	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "scrollbar", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iCtlID = ctlID;
	iMinimum = 0;
	iMaximum = 100;
	iPageInc = 10;
	iLineInc = 2;
	iDisposeValue = 1;

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
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

	SetScrollRange(iHCtl, SB_CTL, iMinimum, iMaximum, FALSE);
	SetScrollPos(iHCtl, SB_CTL, iPos, TRUE);

	gInitialize(super, (HWND)0, NULL);
	if (iSI)
		gUpdate(iSI);
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "ScrollBar control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	SetScrollRange(iHCtl, SB_CTL, iMinimum, iMaximum, FALSE);
	SetScrollPos(iHCtl, SB_CTL, iPos, TRUE);
	if (iSI)
		gUpdate(iSI);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (iValue  &&  iDisposeValue)
		gDispose(iValue);
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	if (iDefault)
		gDispose(iDefault);
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

imeth	int	gCheckValue()
{
	char	buf[80];
	
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
	if (!iValue)
		iValue = gNewWithInt(ShortInteger, iMinimum);
	return iValue;
}

imeth	short	gShortValue : ShortValue ()
{
	return gShortValue(Value(self));
}

imeth	gScrollBarRange(int minimum, int maximum, int pageInc, int lineInc)
{
	int	pos;

	iMinimum = minimum;
	iMaximum = maximum;
	iPageInc = pageInc;
	iLineInc = lineInc;
	if (iValue)
		pos = gShortValue(iValue);
	else
		pos = iMinimum;
	pos = MAX(iMinimum, pos);
	pos = MIN(iMaximum, pos);
	if (iValue)
		gChangeShortValue(iValue, pos);
	else
		iValue = gNewWithInt(ShortInteger, pos);
	if (iSI)
		gUpdate(iSI);
	if (iDlg  &&  gInDialog(iDlg))  {
		SetScrollRange(iHCtl, SB_CTL, iMinimum, iMaximum, FALSE);
		SetScrollPos(iHCtl, SB_CTL, pos, TRUE);
	}
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
	if (iDlg  &&  gInDialog(iDlg)  &&  iValue)  {
		SetScrollRange(iHCtl, SB_CTL, iMinimum, iMaximum, FALSE);
		SetScrollPos(iHCtl, SB_CTL, gShortValue(iValue), TRUE);
	}
	return self;
}

imeth	gSetValue : SetValue (val)
{
	int	pos;

	ChkArg(val, 2);
	if (!iValue)
		iValue = gNew(ShortInteger);
	pos = gShortValue(val);
	pos = MAX(iMinimum, pos);
	pos = MIN(iMaximum, pos);
	gChangeShortValue(iValue, pos);
	if (iSI)
		gUpdate(iSI);
	return update(self);
}

imeth	gSetShortValue : SetShortValue (int pos)
{
	object	t = gNewWithInt(ShortInteger, pos);
	SetValue(self, t);
	gDispose(t);
	return self;
}

imeth	gIncrement(int inc)
{
	int	pos = ShortValue(self);
	pos += inc;
	SetShortValue(self, pos);
	return self;
}

imeth	LRESULT	gProcessCmd(unsigned code, unsigned npos)
{
	int	pos;

	if (iFun) {
		iFun(self, code, npos);
		return (LRESULT) 0;
	}
	if (iValue)
		pos = gShortValue(iValue);
	else
		pos = iMinimum;
	switch ((int)code)  {
	case SB_TOP:
		pos = iMinimum;
		break;
	case SB_BOTTOM:
		pos = iMaximum;
		break;
	case SB_LINEUP:
		pos -= iLineInc;
		break;
	case SB_LINEDOWN:
		pos += iLineInc;
		break;
	case SB_PAGEUP:
		pos -= iPageInc;
		break;
	case SB_PAGEDOWN:
		pos += iPageInc;
		break;
	case SB_THUMBTRACK:
		pos = (int) npos;
		break;
	}
	pos = MAX(iMinimum, pos);
	pos = MIN(iMaximum, pos);
	SetScrollPos(iHCtl, SB_CTL, pos, TRUE);
	if (iValue)
		gChangeShortValue(iValue, pos);
	else
		iValue = gNewWithInt(ShortInteger, pos);
	if (iSI)
		gUpdate(iSI);
	return (LRESULT) 0;
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

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHCtl;
}

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
}

imeth	gDialog()
{
	return iDlg;
}

imeth	gSetDefaultInt(int val)
{
	if (iDefault)
		gChangeShortValue(iDefault, val);
	else
		iDefault = gNewWithInt(ShortInteger, val);
	return self;
}

imeth	gUseDefault()
{
	if (!iDefault)
		iDefault = gNew(ShortInteger);
	return SetValue(self, iDefault);
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

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	org = (ofun) iFun;
	iFun = fun;
	return org;
}








