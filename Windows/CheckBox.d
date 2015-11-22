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

defclass  CheckBox : Control  {
	HWND	iHCtl;		/*  handle to edit control  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	iWindowControl; /*  1=window control		*/
	iValue;			/*  field value			*/
	iDefault;		/*  default value		*/
	int	iDisposeValue;	/*  1=auto dispose of iValue	*/
	int	(*iAcf)();	/*  aux checking function	*/
	int	(*iDCFun)();	/*  function - double click	*/

	iOn;			/*  String rep. of on		*/
	iOff;			/*  String rep. of off		*/
	
	iSI;			/*  ODBC StatementInfo		*/

	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/
};


#include <ctype.h>

static	object	makeValue(ivType *iv);


private	imeth	LRESULT	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	return 0L;
}

private	imeth	LRESULT	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iTopic)
		gSetTopic(HelpSystem, iPrevTopic);
	makeValue(iv);
	return 0L;
}

private	imeth	LRESULT	process_wm_char(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (wParam == '\t') {
		object	next = GetKeyState(VK_SHIFT) & 0x80 ? gPreviousControl(iDlg, self) : gNextControl(iDlg, self);
		if (next)
			SetFocus(gHandle(next));
		return 0L;
	}
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	LRESULT	process_wm_lbuttondblclk(object	self, 
						 HWND	hwnd, 
						 UINT	mMsg, 
						 WPARAM	wParam, 
						 LPARAM	lParam)
{
	if (iDCFun) {
		if (SchemeClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == String) {
			char	cmd[100], ns[80];
			int	res;
			sprintf(cmd, "(%s (int->object %lld) (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object)iDCFun),
				(long long) self, (long long) iDlg);
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					      gNamespaceName(SchemeClassSurrogate, (object)iDCFun, ns), 
					      cmd);
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaCallbackClassSurrogate)
			return gPerformJavaObjCallback((object)iDCFun, iDlg);
		else if (JavaScriptClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToObject(\"%lld\"))", gStringValue((object)iDCFun), (long long) self, (long long) iDlg);
			gExecuteStringNR(JavaScriptClassSurrogate, cmd);
		} else
			iDCFun(self, iDlg);
		return 0L;
	} else if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
		return gCallDefaultProc(self, mMsg, wParam, lParam);
}

cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iDisposeValue = 1;
	iDlg = dlg;
	iOn = gNewWithStr(String, "Y");
	iOff = gNewWithStr(String, "N");

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "button", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iWindowControl = 1;
	iCtlID = ctlID;
	iDisposeValue = 1;
	iOn = gNewWithStr(String, "Y");
	iOff = gNewWithStr(String, "N");

	gSetStyle(obj, WS_VISIBLE);

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONDBLCLK, 0);  /*  no auto default processing  */

	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = BS_CHECKBOX | WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
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
	
	if (iValue)
		SendMessage(iHCtl, BM_SETCHECK, (WPARAM) gShortValue(iValue), 0L);
	if (gLanguageObject(self))
		SetWindowText(iHCtl, gLanguageText(self,gCurrentLanguage(self)));
	if (iSI)
		gUpdate(iSI);
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
		sprintf(buf, "CheckBox control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	if (iValue)
		CheckDlgButton(hDlg, iCtlID, (int) gShortValue(iValue));
	if (gLanguageObject(self))
		SetWindowText(iHCtl, gLanguageText(self,gCurrentLanguage(self)));
	if (iSI)
		gUpdate(iSI);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iOn);
	gDispose(iOff);
	if (iValue  &&  iDisposeValue)
		gDispose(iValue);
	if (iDefault)
		gDispose(iDefault);
	gReleaseHandle(self);
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

static	object	makeValue(ivType *iv)
{
	int	val, isWind;
	
	isWind = iDlg ? gIsKindOf(iDlg, Window) : 0;
	if (iDlg  &&  gInDialog(iDlg)  &&  (!isWind  ||  iHCtl))  {
		if (isWind)
			val = SendMessage(iHCtl, BM_GETCHECK, (WPARAM) 0, 0L);
		else
			val = IsDlgButtonChecked(gHandle(iDlg), iCtlID);
		if (iValue)
			gChangeShortValue(iValue, val);
		else
			iValue = gNewWithInt(ShortInteger, val);
	}
	if (!iValue)
		iValue = gNewWithInt(ShortInteger, 0);
	if (iSI)
		gUpdate(iSI);
	return iValue;
}

imeth	int	gCheckValue()
{
	char	*buf = gGetBuf(Application);
	
	makeValue(iv);
	if (iAcf) {
		int	r=0;

		if (SchemeClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == String) {
			char	cmd[100], ns[80];
			object	ret;

			sprintf(cmd, "(%s (int->object %lld) (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object)iAcf),
				(long long) self, (long long) iValue);
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
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToObject(\"%lld\"))", gStringValue((object)iAcf), (long long) self, (long long) iValue);
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
	makeValue(iv);
	return iValue;
}

imeth	short	gShortValue()
{
	return gShortValue(Value(self));
}

imeth	gSetStringValues(char *on, char *off)
{
	gChangeStrValue(iOn, on);
	gChangeStrValue(iOff, off);
	return self;
}

imeth	char	*gStringValue()
{
	return gShortValue(Value(self)) ? gStringValue(iOn) : gStringValue(iOff);
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
	if (iDlg  &&  gInDialog(iDlg)  &&  iValue)
		if (gIsKindOf(iDlg, Window)) {
			int	val;
			if (iHCtl) {
				val = iValue ? gShortValue(iValue) : 0;
				SendMessage(iHCtl, BM_SETCHECK, (WPARAM) val, 0L);
			}
		} else
			CheckDlgButton(gHandle(iDlg), iCtlID, (int) gShortValue(iValue));
	return self;
}

imeth	gSetValue : SetValue (val)
{
	ChkArg(val, 2);
	if (iValue)
		gChangeValue(iValue, val);
	else
		iValue = gCopy(val);
	if (iSI)
		gUpdate(iSI);
	return update(self);
}

imeth	gSetShortValue(int val)
{
	object	t = gNewWithInt(ShortInteger, val);
	SetValue(self, t);
	gDispose(t);
	return self;
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

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
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

imeth	int	gPerform()
{
	return gExecuteSetFunctions(self, iDlg);
}

imeth	gDialog, gGetParent ()
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

imeth	gSetTitle(char *title)
{
	if (iHCtl)
		SetWindowText(iHCtl, title);
		
	gSetLanguageText(self,gCurrentLanguage(self),title);
	
	return self;
}

imeth	char	*gGetTitle()
{
	return gLanguageText(self,gCurrentLanguage(self));
}

imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg);
}

imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_CHECKBOX_t	*v = vp;
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
	v->len = gLanguageObject(self) ? strlen(gLanguageText(self,ENGLISH)) + 1 : 0;
	v->defaultVal = iDefault ? gShortValue(iDefault) : 0;
	v->onStrLen = iOn ? strlen(gStringValue(iOn)) + 1 : 0;
	v->offStrLen = iOff ? strlen(gStringValue(iOff)) + 1 : 0;
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
	CTLTYPE_CHECKBOX_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gSetName(self, v->name);
	gSetDefaultInt(self, v->defaultVal);
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_CHECKBOX_t	v;
	short	type = CTLTYPE_CHECKBOX, size = sizeof(v);
	object	fobj = gGetFont(self);

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.len  &&  1 != fwrite(gLanguageText(self,ENGLISH), (int) v.len, 1, fp))
		return NULL;

	if (v.onStrLen  &&  1 != fwrite(gStringValue(iOn), (int) v.onStrLen, 1, fp))
		return NULL;

	if (v.offStrLen  &&  1 != fwrite(gStringValue(iOff), (int) v.offStrLen, 1, fp))
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
	CTLTYPE_CHECKBOX_t	v;
	int	end;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];
	ivType	*iv;

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
	ctl = gAddCheckBox(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name, NULL);

	iv = ivPtr(ctl);
	
	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetTitle(ctl, p);
		if (v.len > BUFLEN)
			free(p);
	}

	if (v.onStrLen) {
		p = v.onStrLen > BUFLEN ? malloc((unsigned)v.onStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.onStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (iOn)
			gChangeStrValue(iOn, p);
		else
			iOn = gNewWithStr(String, p);
		if (v.onStrLen > BUFLEN)
			free(p);
	}

	if (v.offStrLen) {
		p = v.offStrLen > BUFLEN ? malloc((unsigned)v.offStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.offStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (iOff)
			gChangeStrValue(iOff, p);
		else
			iOff = gNewWithStr(String, p);
		if (v.offStrLen > BUFLEN)
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
			//else
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
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}


cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_CHECKBOX_t v;
	int                end;
	object             ctl;
	short              size;
	char               *p, buf[BUFLEN];
	ivType             *iv;

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
	ctl = gAddCheckBox(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name, NULL);

	iv = ivPtr(ctl);
	
	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetTitle(ctl, p);
		if (v.len > BUFLEN)
			free(p);
	}

	if (v.onStrLen) {
		p = v.onStrLen > BUFLEN ? malloc((unsigned)v.onStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.onStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (iOn)
			gChangeStrValue(iOn, p);
		else
			iOn = gNewWithStr(String, p);
		if (v.onStrLen > BUFLEN)
			free(p);
	}

	if (v.offStrLen) {
		p = v.offStrLen > BUFLEN ? malloc((unsigned)v.offStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.offStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (iOff)
			gChangeStrValue(iOff, p);
		else
			iOff = gNewWithStr(String, p);
		if (v.offStrLen > BUFLEN)
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
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}


/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the checkbox control of the cld file,
//                  This is a dummy reading, no object is generated.
//                  
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_CHECKBOX_t v;
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

	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (v.len > BUFLEN)
			free(p);
	}

	if (v.onStrLen) {
		p = v.onStrLen > BUFLEN ? malloc((unsigned)v.onStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.onStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (v.onStrLen > BUFLEN)
			free(p);
	}

	if (v.offStrLen) {
		p = v.offStrLen > BUFLEN ? malloc((unsigned)v.offStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.offStrLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		if (v.offStrLen > BUFLEN)
			free(p);
	}

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

imeth gWriteXML(FILE *fp)
{

	CTLTYPE_CHECKBOX_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);
	int loop;
	char	**languages=gLanguages(Application);

	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<checkbox>\n");
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
	fprintf(fp,"\t\t\t<len>%d</len>\n",v.len);
	fprintf(fp,"\t\t\t<title>%s</title>\n",gStringToXML(XMLNode,buf,gGetTitle(self)));
	fprintf(fp,"\t\t\t<onStrLen>%d</onStrLen>\n",v.onStrLen);
	fprintf(fp,"\t\t\t<onStr>%s</onStr>\n",gStringToXML(XMLNode,buf,gStringValue(iOn)));
	fprintf(fp,"\t\t\t<offStrLen>%d</offStrLen>\n",v.offStrLen);
	fprintf(fp,"\t\t\t<offStr>%s</offStr>\n",gStringToXML(XMLNode,buf,gStringValue(iOff)));
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	for (loop=0;loop<MAX_LANGUAGES;loop++)
		fprintf(fp,"\t\t\t<%s>%s</%s>\n",languages[loop],gStringToXML(XMLNode,buf,gLanguageText(self,loop)),languages[loop]); 

	fprintf(fp,"\t\t</checkbox>\n");


	return self;
}



cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_CHECKBOX_t	v;
	int	end, loop;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];
	ivType	*iv;
	char	**languages=gLanguages(Application);

	double controlScaleFactor;
	char temp[BUFLEN];

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
	v.len=gGetIntFromNode(curnode,"len");
	v.onStrLen=gGetIntFromNode(curnode,"onStrLen");
	v.offStrLen=gGetIntFromNode(curnode,"offStrLen");

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
	ctl = gAddCheckBox(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, v.name, NULL);

	for (loop=0;loop<MAX_LANGUAGES;loop++)
	{
		gPopulateStringFromNode(curnode,buf,languages[loop]);
		gSetLanguageText(ctl,loop,buf);
	}
	
	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	iv = ivPtr(ctl);
	
	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"title");
		gSetTitle(ctl, p);
		gSetLanguageText(ctl, ENGLISH, p);
		if (v.len > BUFLEN)
			free(p);
	}

	if (v.onStrLen) {
		p = v.onStrLen > BUFLEN ? malloc((unsigned)v.onStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"onStr");
		if (iOn)
			gChangeStrValue(iOn, p);
		else
			iOn = gNewWithStr(String, p);
		if (v.onStrLen > BUFLEN)
			free(p);
	}

	if (v.offStrLen) {
		p = v.offStrLen > BUFLEN ? malloc((unsigned)v.offStrLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"offStr");
		if (iOff)
			gChangeStrValue(iOff, p);
		else
			iOff = gNewWithStr(String, p);
		if (v.offStrLen > BUFLEN)
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
	if (gModifyChildren(parent))
		gShow(ctl);
	
	return ctl;
}


imeth	gResetText()
{
	SetWindowText(gHandle(self), (LPCSTR) gLanguageText(self,gCurrentLanguage(self))); 		
	return self;
}




