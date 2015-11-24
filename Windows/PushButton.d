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
#include "ctlsave.h"

defclass  PushButton : Control  {
	HWND	iHCtl;		/*  handle to control	  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	iWindowControl; /*  1=window control		*/
	int	(*iDCFun)();	/*  function - double click	*/
	int	iExitType;	/*  0=don't exit dialog or IDCANCEL or IDOK  */

	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/
};


#include <ctype.h>

private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect);


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
	} else if (wParam == 13) {
#ifdef	WIN32
		PostMessage(gHandle(iDlg), WM_COMMAND, MAKEWPARAM((unsigned short)iCtlID, BN_CLICKED), (LPARAM) iHCtl);
#else
		PostMessage(gHandle(iDlg), WM_COMMAND, (WPARAM) iCtlID, MAKELPARAM(iHCtl, BN_CLICKED));
#endif
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

			sprintf(cmd, "(%s (int->object %lld) (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object)iDCFun),
				PTOLL(self), PTOLL(iDlg));
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					      gNamespaceName(SchemeClassSurrogate, (object)iDCFun, ns), 
					      cmd);
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaCallbackClassSurrogate)
			gPerformJavaObjCallback((object)iDCFun, iDlg);
		else if (JavaScriptClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToObject(\"%lld\"))", gStringValue((object)iDCFun), PTOLL(self), PTOLL(iDlg));
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

	iDlg = dlg;
	iCtlID = ctlID;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

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
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;
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
	style = BS_PUSHBUTTON | WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

imeth	gInitialize(HWND hDlg, dlg)
{
	object	font = gGetFont(self);
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "PushButton control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
	if (gLanguageObject(self))
		SetWindowText(iHCtl, (LPCSTR) gLanguageText(self,gCurrentLanguage(self)));
	return gInitialize(super, hDlg, dlg);
}

imeth	int	gShow()
{
	if (!iHCtl) {
		object	font = gGetFont(self);
		
		gShow(super);
		iHCtl = gHandle(super);
		gSubclassWindow(self, iHCtl);
		if (font)
			SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0);
		if (gLanguageObject(self))
			SetWindowText(iHCtl, (LPCSTR) gLanguageText(self,gCurrentLanguage(self)));
		gInitialize(super, (HWND)0, NULL);
	}
	return 0;
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (IsObj((object) iDCFun))
		gDispose((object) iDCFun);
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

imeth	gSetValue : SetValue (val)
{
	ChkArg(val, 2);
	if (iDlg  &&  gInDialog(iDlg))
		SetWindowText(iHCtl, (LPCSTR) gStringValue(val));	
	gSetLanguageText(self,gCurrentLanguage(self),gStringValue(val));	
	return self;
}

imeth	gSetStringValue(char *val)
{
	gSetLanguageText(self,gCurrentLanguage(self),val);	
	if (iDlg  &&  gInDialog(iDlg))
		SetWindowText(iHCtl, (LPCSTR) val);	
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

imeth	gDialog, gGetParent ()
{
	return iDlg;
}

imeth	gClick()
{
	PostMessage(gHandle(iDlg),
		    WM_COMMAND,
		    MAKEWPARAM(iCtlID, BN_CLICKED),
		    (LPARAM) iHCtl);
	return self;
}

imeth	char	*gStringValue()
{
	return gLanguageText(self,gCurrentLanguage(self));
}

imeth	gSetExitType(int type)
{
	iExitType = type;
	return self;
}

imeth	int	gGetExitType()
{
	return iExitType;
}

imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg);
}

imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_PUSHBUTTON_t	*v = vp;
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
	v->len = *gLanguageText(self,ENGLISH) ? strlen(gLanguageText(self,ENGLISH)) + 1 : 0;
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
	CTLTYPE_PUSHBUTTON_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gSetName(self, v->name);
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_PUSHBUTTON_t	v;
	short	type = CTLTYPE_PUSHBUTTON, size = sizeof(v);
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

	if (v.helpTopicLen  &&  1 != fwrite(gStringValue(iTopic), (int) v.helpTopicLen, 1, fp))
		return NULL;

	if (fobj  &&  v.fontNameLen  &&  1 != fwrite(gName(fobj), (int) v.fontNameLen, 1, fp))
		return NULL;

	return self;
}

#define	BUFLEN	128

cmeth	gLoadControl(FILE *fp, parent)
{
	CTLTYPE_PUSHBUTTON_t	v;
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
	ctl = gAddPushButton(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, NULL, v.name, NULL);

	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.len > BUFLEN)
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
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}


// Yanghui:
cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_PUSHBUTTON_t  v;
	int                   end;
	object                ctl;
	short                 size;
	char                  *p, buf[BUFLEN];

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
	ctl = gAddPushButton(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, NULL, v.name, NULL);

	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.len > BUFLEN)
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

	// check input parameters:
	if(!printerObj)    // validate the printerObj
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) )  // get and validate the print DC
		return NULL;

	if(dScaleX<=0 || dScaleY<=0)  // validate the view to printer scaling factors
		return NULL;

	if(gHiddenStatus(self)==1)  // skipping printing
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
 	
	// leave some space in the beginning and in the end of the rectangle
	nTmp = gAveCharWidth(fontObj)*dScaleXX + 0.5;
	rect.left  += nTmp;
	rect.right -= nTmp;

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
// gCLDLoadControl: load the push button of the cld file for printing
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_PUSHBUTTON_t  v;
	short                 size;
	char                  *p, buf[BUFLEN];
	object	              ctl = NULL;
	ivType                *iv = NULL;

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

	ctl = gCLDNewWindowControl(PushButton, v.name, parentObj);
	iv = ivPtr(ctl);

	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.len, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.len > BUFLEN)
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
	return ctl;
}


cmeth	gCLDNewWindowControl(char *name, object parentObj)
{
	object	obj = gCLDNewCont(super, name, "button");
	ivType	*iv = ivPtr(obj);

	iDlg = parentObj;
	iWindowControl = 0;
	iCtlID = 0;
	iExitType = 0;

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
	object   fontObj;
	HFONT    hfontOld, hfontView;
	HDC      hdcView;
	HWND     hwnd;
	RECT     rectLine;
	LOGFONT  logfont;
	char     *strValue;
	int      nAveCharWidth;

	if(!hdcPrinter || !pRect)
		return -1;

	// get a point to the string
	strValue = gLanguageText(self,gCurrentLanguage(self));
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

	CopyRect(&rectLine, pRect);
	gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, strValue, DT_CENTER, nAveCharWidth);  // align Center

	SelectObject(hdcView, hfontOld);
	ReleaseDC(hwnd, hdcView);

	return 0;
}

// Yanghui

imeth gWriteXML(FILE *fp)
{
	CTLTYPE_PUSHBUTTON_t	v;
	char		buf[1024];
	object		fnt = gGetFont(self);
	int loop;
	char	**languages=gLanguages(Application);

	gGetControlParameters(self, &v);


	fprintf(fp,"\t\t<pushbutton>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<text>%s</text>\n",gStringToXML(XMLNode,buf,gLanguageText(self,ENGLISH)));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<fontname>%s</fontname>\n",gStringToXML(XMLNode,buf,gName(fnt)));
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<fontNameLen>%d</fontNameLen>\n",v.fontNameLen);
	fprintf(fp,"\t\t\t<len>%d</len>\n",v.len);
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	for (loop=0;loop<MAX_LANGUAGES;loop++)
		fprintf(fp,"\t\t\t<%s>%s</%s>\n",languages[loop],gStringToXML(XMLNode,buf,gLanguageText(self,loop)),languages[loop]); 

	fprintf(fp,"\t\t</pushbutton>\n");

	return self;
} 


cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_PUSHBUTTON_t	v;
	int	end, loop;
	object	ctl;
	short	size;
	char	*p, buf[BUFLEN];
	char	**languages=gLanguages(Application);

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
	v.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	v.len=gGetIntFromNode(curnode,"len");
	
	
	
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
	ctl = gAddPushButton(parent, (int) v.yPos, (int) v.xPos, (int) v.width, &end, NULL, v.name, NULL);

	
	for (loop=0;loop<MAX_LANGUAGES;loop++)
	{
		gPopulateStringFromNode(curnode,buf,languages[loop]);
		gSetLanguageText(ctl,loop,buf);
	}
	
	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	if (v.len) {
		p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"text");
		gSetStringValue(ctl, p);
		gSetLanguageText(ctl, ENGLISH, p);
		if (v.len > BUFLEN)
			free(p);
	}

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
			gPopulateStringFromNode(curnode,p,"fontname");
			gSetFont(ctl, vNew(ExternalFont, p, v.fontSize));
			if (v.fontNameLen > BUFLEN)
				free(p);
		}
	}
	// Yanghui
	
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




