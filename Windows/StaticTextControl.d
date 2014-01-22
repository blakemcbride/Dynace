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



#include "ctlsave.h"


defclass StaticTextControl : TextControl  {
	iURL; //URL to link to
	int   iRight;
	char  iNotSaveToMetaFile;   // 0: save it to meta file _.pmf (default value)
	                            // 1: save it to cld file _.cld (not in _.pmf)
};

#include "color.h"

static char *chopOneLineOfText(HDC hdcView, int cxView, char *strValue, char *lineOfText, int nAveCharWidth);

private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect);
private imeth int pFormatAndDrawTextToMetaFile(HDC hdcMetafile, const RECT *pRect);


imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_STATIC_t	*v = vp;
	int	height, width, xPos, yPos;
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
	v->filler = iNotSaveToMetaFile;  // filler is borrowed here to flag saving to meta file or not -- Yanghui
	v->len    = strlen(gLanguageText(self,ENGLISH)) + 1;
	v->right  = iRight ? 'Y' : 'N';
	v->fontNameLen = fobj ? strlen(gName(fobj)) + 1 : 0;
	v->fontSize = fobj ? gPointSize(fobj) : 0;
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSetControlParameters(void *vp)
{
	CTLTYPE_STATIC_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
//	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gDisable(self);

	// Yanghui:
	// filler is borrowed here to flag saving to meta file or not 
	if(v->filler)
		iNotSaveToMetaFile = 1;
	else
		iNotSaveToMetaFile = 0;  
	// Yanghui

	gSetName(self, v->name);
	if (iRight = v->right == 'Y')
		gSetStyle(self, WS_VISIBLE | ES_MULTILINE  | ES_RIGHT);
	gSetScalingMode(Application, sm);
	return self;
}

imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_STATIC_t	v;
	short	type = CTLTYPE_STATIC, size = sizeof(v);
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

	if (fobj  &&  v.fontNameLen  &&  1 != fwrite(gName(fobj), (int) v.fontNameLen, 1, fp))
		return NULL;

	return self;
}



#define	BUFLEN	128


cmeth	gLoadControl(FILE *fp, parent)
{
	CTLTYPE_STATIC_t	v;
	object	ctl;
	int	end;
	
	// Yanghui:
	double controlScaleFactor;  
	HWND hwnd;
	int margins;
	
	short	size;
	char	*p, buf[BUFLEN];

	
	// Yanghui

	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);

	p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
	if (!p)
		return NULL;
	if (1 != fread(p, (int) v.len, 1, fp))
		return NULL;

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
	ctl = gAddStaticTextControl(parent, (int) v.yPos, (int) v.xPos, &end, v.name, p);

	gSetLanguageText(ctl, ENGLISH, p);
	if (v.len > BUFLEN)
		free(p);

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
	// Yanghui

	gSetControlParameters(ctl, &v);
	
	if (gModifyChildren(parent))
		gShow(ctl);

	// Yanghui:
#ifdef	WIN32
	hwnd = gHandle(ctl);
	margins = gAveCharWidth(gGetFont(ctl));

	SendMessage(hwnd, EM_SETMARGINS, (WPARAM)EC_LEFTMARGIN,  margins);
	SendMessage(hwnd, EM_SETMARGINS, (WPARAM)EC_RIGHTMARGIN, margins);
#endif
	// Yanghui
	return ctl;
}


// Yanghui:
cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_STATIC_t  v;
	int	              end;
	object            ctl;
	short             size;
	char              *p, buf[BUFLEN];

	if (1 != fread(&size, sizeof size, 1, fp))
		return NULL;
	if (size < sizeof(v))
		memset(&v, 0, sizeof v);
	if (1 != fread(&v, (int) (size > sizeof(v) ? sizeof(v) : size), 1, fp))
		return NULL;
	if (size > sizeof(v))
		fseek(fp, (long)(size-sizeof(v)), SEEK_CUR);

	p = v.len > BUFLEN ? malloc((unsigned)v.len) : buf;
	if (!p)
		return NULL;
	if (1 != fread(p, (int) v.len, 1, fp))
		return NULL;

	v.xPos += nXshift;
	v.yPos += nYshift;

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddStaticTextControl(parent, (int) v.yPos, (int) v.xPos, &end, v.name, p);

	if (v.len > BUFLEN)
		free(p);

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
	SIZE    sizeView, sizePrinter;
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

	// get a point to the string
	strValue = gLanguageText(self,ENGLISH);
	if(!strValue || !(*strValue))
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

	pFormatAndPrintText(self, hdcPrinter, dScaleXX, dScaleYY, &rect);

	return self;
}


/////////////////////////////////////////////////////////////////
// hdcMeta: a handle to a metafile 
// hdcView: a handle to a view DC of the parent window
//
imeth gSaveControlToMetaFile(HDC hdcMeta, HDC hdcView)
{
	RECT         rect;
	int          nAveCharWidth;
	DWORD        dwMargin;
	int          xOffset, yOffset;

	gGetWindowRect(self, &rect);
	gGetScrollPosition(gGetParent(self), &yOffset, &xOffset);
	rect.left   += xOffset;
	rect.right  += xOffset;
	rect.top    += yOffset;
	rect.bottom += yOffset;

#ifdef WIN32
	// exclude the margins at the beginning and in the end of the rectangle
	dwMargin = SendMessage(gHandle(self), EM_GETMARGINS, (WPARAM)NULL, (LPARAM)NULL);
	rect.left  += LOWORD(dwMargin);
	rect.right -= HIWORD(dwMargin);
#endif
	LPtoDP(hdcView, (LPPOINT)&rect, 2);

	pFormatAndDrawTextToMetaFile(self, hdcMeta, &rect);
	return self;
}



////////////////////////////////////////////////////////////////////////////////////////
// pFormatAndDrawTextToMetaFile: format and draw text of the control to support WYSIWYG.
//
// HDC   hdcMetafile: a handle to the metafile DC
// RECT  *pRect:      a pointer to the rectangle of the control in metafile unit
//             
////////////////////////////////////////////////////////////////////////////////////////
private imeth int pFormatAndDrawTextToMetaFile(HDC hdcMetafile, const RECT *pRect)
{
	object  fontObj;
	HFONT   hfontOld, hfontView;
	HDC     hdcView;
	char    *strPoint, *strValue, *lineOfText;
	RECT    rectLine;
	int     cxView, nTextHeight, nAveCharWidthView, nAveCharWidth, nOldBkMode;
	LOGFONT logfont;
	double  dScaleX, dScaleY;
	HWND    hwnd;
	COLORREF oldTextColor;

	if(!hdcMetafile || !pRect)
		return -1;

	// get a point to the string
	strValue = gStringValueWithoutStripping(self);
	if(!strValue || !(*strValue))
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	// get the font of the control 
	hfontView = gHandle(fontObj);
	if(!hfontView)
		return -1;

	lineOfText = (char *)malloc(strlen(strValue)+1);
	if(!lineOfText)
		return -1;

	hwnd = gHandle(self);
	hdcView = GetDC(hwnd);
	
	dScaleX = 300.0/GetDeviceCaps(hdcView, LOGPIXELSX);
	dScaleY = 300.0/GetDeviceCaps(hdcView, LOGPIXELSY);
	  
	hfontOld = SelectObject(hdcView, hfontView);

	GetObject(hfontView, sizeof(LOGFONT), &logfont);
	nTextHeight = gLineHeight(fontObj)*dScaleY + 0.5;
	logfont.lfWidth  = logfont.lfWidth*dScaleX + 0.5;
	logfont.lfHeight = -(abs(logfont.lfHeight)*dScaleY + 0.5);
	nAveCharWidthView = gAveCharWidth(fontObj);  
	nAveCharWidth = nAveCharWidthView*dScaleX + 0.5;
	
	CopyRect(&rectLine, pRect);

	nOldBkMode = SetBkMode(hdcMetafile, TRANSPARENT);
	oldTextColor = SetTextColor(hdcMetafile, CLR_Array[CLR_Black]);  // set the text color to black

	if(dScaleX>0 && dScaleY>0) {
		cxView = (rectLine.right - rectLine.left)/dScaleX + 0.5;
		rectLine.bottom = rectLine.top + nTextHeight;
		strPoint = strValue;
		do {
			strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
			if(iRight)
				gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_RIGHT);
			else
				gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_LEFT);

			rectLine.top += nTextHeight;
			rectLine.bottom += nTextHeight;
		} while(strPoint!=NULL && (rectLine.bottom<=pRect->bottom+2*dScaleY) );
	}

	SetTextColor(hdcMetafile, oldTextColor);
	SetBkMode(hdcMetafile, nOldBkMode); 

	SelectObject(hdcView, hfontOld);
	ReleaseDC(hwnd, hdcView);

	free(lineOfText);

	return 0;
}



///////////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the static text control of the cld file for printing
//
///////////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_STATIC_t  v;
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

	ctl = gCLDNewWindowControl(StaticTextControl, v.name, parentObj);
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



////////////////////////////////////////////////////////////////////////////////////////
// pFormatAndPrintText: format and draw text of the control to support WYSIWYG.
//
// HDC    hdcPrinter: a handle to the printer DC
// double dScaleX:    the horizontal scaling factor from view to printer
// double dScaleY:    the vertical scaling factor from view to printer
// RECT   *pRect:     a pointer to the rectangle of the control in printer unit
//             
////////////////////////////////////////////////////////////////////////////////////////
private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect)
{
	object  fontObj;
	HFONT   hfontOld, hfontView;
	HWND    hwnd;
	HDC     hdcView;
	char    *strPoint, *strValue, *lineOfText;
	RECT    rectLine;
	int     cxView, nTextHeight, nAveCharWidthView, nAveCharWidth;
	LOGFONT logfont;

	if(!hdcPrinter || !pRect)
		return -1;

	// get a point to the string
	strValue = gStringValueWithoutStripping(self);
	if(!strValue || !(*strValue))
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	// get the font of the control 
	hfontView = gHandle(fontObj);
	if(!hfontView)
		return -1;

	if(dScaleX<0)
		dScaleX = -dScaleX;

	if(dScaleY<0)
		dScaleY = -dScaleY;

	lineOfText = (char *)malloc(strlen(strValue)+1);
	if(!lineOfText)
		return -1;

	hwnd =  gHandle(self);
	hdcView = GetDC(hwnd);  
	hfontOld = SelectObject(hdcView, hfontView);

	GetObject(hfontView, sizeof(LOGFONT), &logfont);
	nTextHeight = gLineHeight(fontObj)*dScaleY + 0.5;
	logfont.lfWidth  = logfont.lfWidth*dScaleX + 0.5;
	logfont.lfHeight = -(abs(logfont.lfHeight)*dScaleY + 0.5);
	nAveCharWidthView = gAveCharWidth(fontObj);  
	nAveCharWidth = nAveCharWidthView*dScaleX + 0.5;
	
	CopyRect(&rectLine, pRect);
	if(dScaleX>0 && dScaleY>0) {
		cxView = (rectLine.right - rectLine.left)/dScaleX + 0.5;
		rectLine.bottom = rectLine.top + nTextHeight;

		strPoint = strValue;
		do {
			strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
			if(iRight)
				gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_RIGHT, nAveCharWidth);
			else
				gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_LEFT, nAveCharWidth);

			rectLine.top += nTextHeight;
			rectLine.bottom += nTextHeight;
		} while(strPoint!=NULL && (rectLine.bottom<=pRect->bottom+2*dScaleY) );
	}

	SelectObject(hdcView, hfontOld);
	ReleaseDC(hwnd, hdcView);

	free(lineOfText);
	return 0;
}



//////////////////////////////////////////////////////////////////////////////////
// chopOneLineOfText: chop one line of text
//
// HDC hdcView:       a handle to the view DC
// int cxView:        the width of the control in view DC unit
// char *strValue:    a string point to the string in the string object
// char *lineOfText:  the line of text chopped from strValue
// int nAveCharWidth: the average character width in view DC unit
//
// Note: if strValue has more than one line of text, it returns a string point 
//       which points to the next string to be chopped, else it returns NULL
//
//////////////////////////////////////////////////////////////////////////////////
static char * chopOneLineOfText(HDC hdcView, int cxView, char *strValue, char *lineOfText, int nAveCharWidth)
{
	char *strPoint, *strValueCopy, *token, *pStrValueCopy;
	int  nPreToken, nUpToToken, nUpToToken0, nStrValue;
	SIZE sizeLine;

	if(!hdcView || !strValue || !(*strValue) || !lineOfText ) {
		lineOfText[0] = '\0';
		return NULL;
	}

	strValueCopy = (char *)malloc(strlen(strValue)+1);
	if(!strValueCopy) {
		lineOfText[0] = '\0';
		return NULL;
	}
		
	strcpy(strValueCopy, strValue);
	strPoint = strValue;

	nStrValue = strlen(strValue);
	pStrValueCopy = strValueCopy;
	token = strtok(pStrValueCopy, " \t\0");
	if(!token) {
		*lineOfText = '\0';
		free(strValueCopy);
		return NULL;
	}

	nPreToken = strstr(strPoint, token) - strPoint ;
	nUpToToken = nPreToken + strlen(token);
	if(nUpToToken<nStrValue)
		pStrValueCopy = strValueCopy+ nUpToToken + 1 ;
	else
		pStrValueCopy = strValueCopy+ nUpToToken;

#ifdef WIN32
	GetTextExtentPoint32(hdcView, strValue, nUpToToken, &sizeLine);
#else
	GetTextExtentPoint(hdcView, strValue, nUpToToken, &sizeLine);
#endif

	if(sizeLine.cx > cxView + nAveCharWidth/5) {  // nAveCharWidth/5 pixel in view
		int nTotal, i;
		strncpy(lineOfText, strValue, nUpToToken);
		lineOfText[nUpToToken] = '\0';
		free(strValueCopy);

		// skip the space at the beginning of next line for the continuation of chopping
		nTotal = strlen(strValue) - strlen(lineOfText);
		strPoint = strValue + nUpToToken;
		for(i=0; i<nTotal; i++) {
			if(*strPoint==' ')
				strPoint++;
			else
				break;
		}
		return strPoint;
	}

	nUpToToken0 = nUpToToken;

	// I have to use pStrValueCopy, instead of NULL, to avoid data corruption
	while( (token=strtok(pStrValueCopy, " \t\0")) != NULL ) {
		strPoint = strValue + nUpToToken;

		nPreToken = strstr(strPoint, token) - strPoint + nUpToToken;
		nUpToToken = nPreToken + strlen(token); 
		if(nUpToToken<nStrValue)
			pStrValueCopy = strValueCopy+ nUpToToken + 1 ;
		else
			pStrValueCopy = strValueCopy+ nUpToToken;

#ifdef WIN32
		GetTextExtentPoint32(hdcView, strValue, nUpToToken, &sizeLine);
#else
		GetTextExtentPoint(hdcView, strValue, nUpToToken, &sizeLine);
#endif
		if(sizeLine.cx > cxView + nAveCharWidth/5) {  // nAveCharWidth/5 pixel in view
			strncpy(lineOfText, strValue, nUpToToken0); 
			lineOfText[nUpToToken0] = '\0';
			free(strValueCopy);
			return (strValue + nPreToken);
		}
		nUpToToken0 = nUpToToken;
	}
	strcpy(lineOfText, strValue);
	free(strValueCopy);
	return NULL;
}


imeth char gGetSavingStatus()
{
	return iNotSaveToMetaFile;
}


imeth char gSetSavingStatus(char nNewStatus)
{
	char nOldStatus = iNotSaveToMetaFile;
	iNotSaveToMetaFile = nNewStatus;
	return nOldStatus;
}

// Yanghui


imeth gWriteXML(FILE *fp)
{
	CTLTYPE_STATIC_t	v;
	char			*cp, buf[1024];
	object			fnt = gGetFont(self);
	int loop;
	char	**languages=gLanguages(Application);
	
	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<static>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<text>%s</text>\n",gStringToXML(XMLNode,buf,gLanguageText(self,ENGLISH)));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<fontsize>%d</fontsize>\n",gPointSize(fnt));
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<right>%c</right>\n",v.right);
	fprintf(fp,"\t\t\t<len>%d</len>\n",v.len);	
	fprintf(fp,"\t\t\t<fontname>%s</fontname>\n",gStringToXML(XMLNode,buf,gName(fnt)));
	fprintf(fp,"\t\t\t<fontNameLen>%d</fontNameLen>\n",v.fontNameLen);
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	fprintf(fp,"\t\t\t<htmlFlag>%d</htmlFlag>\n",gHTMLFlag(self));
	fprintf(fp,"\t\t\t<url>%s</url>\n",gURL(self));
	for (loop=0;loop<MAX_LANGUAGES;loop++)
		fprintf(fp,"\t\t\t<%s>%s</%s>\n",languages[loop],gStringToXML(XMLNode,buf,gLanguageText(self,loop)),languages[loop]); 

	fprintf(fp,"\t\t</static>\n");
	
	return self;
}

cmeth	gLoadControlFromXML(curnode, parent)
{
	object	ctl;
	int	end, loop;
	char	**languages=gLanguages(Application);
	// Yanghui:
	double controlScaleFactor;  
	HWND hwnd;
	int margins;
	char	*p, buf[BUFLEN];
	
	CTLTYPE_STATIC_t vp;

	char *text=NULL;
	
	ZeroMemory(&vp,sizeof(vp));

	gPopulateStringFromNode(curnode,vp.name,"name");
	vp.xPos=gGetIntFromNode(curnode,"x");
	vp.yPos=gGetIntFromNode(curnode,"y");
	vp.width=gGetIntFromNode(curnode,"width");
	vp.height=gGetIntFromNode(curnode,"height");
	vp.hidden=gGetCharFromNode(curnode,"hidden");
	vp.disabled=gGetCharFromNode(curnode,"disabled");
	vp.right=gGetCharFromNode(curnode,"right");
	vp.len=gGetIntFromNode(curnode,"len");
	vp.fontSize=gGetIntFromNode(curnode,"fontsize");
	vp.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	
	if (vp.len)
	{
		text=malloc(vp.len+1);
		gPopulateStringFromNode(curnode,text,"text");
	}	
	
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
			dXpos  = (vp.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale;
			vp.xPos = dXpos + 0.5;
			dYpos  = (vp.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale;
			vp.yPos = dYpos + 0.5;

			vp.width  = (dXpos + vp.width*controlScaleFactor*dCxScale  + 0.5) - vp.xPos;
			vp.height = (dYpos + vp.height*controlScaleFactor*dCyScale + 0.5) - vp.yPos;

			vp.fontSize = (int)(vp.fontSize*controlScaleFactor*dCyScale)/gGetFontScale(self);

			// if the font information was not saved, adjust the fontsize. 
			hWnd = gHandle(parent);
			hDc = GetDC(hWnd);  
				// if hWnd=NULL, GetDC retrieves the device context for the entire screen
			logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
			ReleaseDC(hWnd, hDc);
		
			if(vp.fontSize <= 0)    // the font was not saved, default it to 10 
				vp.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				vp.fontSize = min(abs(MulDiv(vp.height-4, 72, logPixelsy)), vp.fontSize);
		}
	}
	// Yanghui

	gAddAppendOffsets(parent, &vp.yPos, &vp.xPos);
	

	ctl = gAddStaticTextControl(parent, (int) vp.yPos, (int) vp.xPos, &end, vp.name, text);

	gSetLanguageText(ctl, ENGLISH, text);
	
	for (loop=0;loop<MAX_LANGUAGES;loop++)
	{
		gPopulateStringFromNode(curnode,buf,languages[loop]);
		gSetLanguageText(ctl,loop,buf);
	}
	
	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	gPopulateStringFromNode(curnode,buf,"url");
	gSetURL(ctl,buf);
	gSetHTMLFlag(ctl,gGetIntFromNode(curnode,"htmlFlag")); 


	// Yanghui:
	// when scaling is processed, the v.fontSize may be too small and not available for the chosen font,
	// although system would get the closest match, but the font height would not match the control height.
	// In this case, Arial font will be chosen. In addition, if the old cld does not have a font chosen, 
	// Arial font is the default
	if (gGetScaleFlg(parent)) {     // do the scaling if the scaling flag is set
		if (vp.fontNameLen) {
			p = vp.fontNameLen > BUFLEN ? malloc((unsigned)vp.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			gPopulateStringFromNode(curnode,p,"fontname");

			// if(v.fontSize<8)
			//	gSetFont(ctl, vNew(ExternalFont, "Arial", vp.fontSize));
			// else
				gSetFont(ctl, vNew(ExternalFont, p, vp.fontSize));

			if (vp.fontNameLen > BUFLEN)
				free(p);
		}
		else
			gSetFont(ctl, vNew(ExternalFont, "Arial", vp.fontSize));
	}
	else {  // the original code
		if (vp.fontNameLen) {
			p = vp.fontNameLen > BUFLEN ? malloc((unsigned)vp.fontNameLen) : buf;
			if (!p)
				vError(Application, "out of memory");
			gPopulateStringFromNode(curnode,p,"fontname");
			gSetFont(ctl, vNew(ExternalFont, p, vp.fontSize));
			if (vp.fontNameLen > BUFLEN)
				free(p);
		}
	}
	
	gSetControlParameters(ctl, &vp);
	
	
	if (gModifyChildren(parent))
		gShow(ctl);

	// Yanghui:
#ifdef	WIN32
	hwnd = gHandle(ctl);
	margins = gAveCharWidth(gGetFont(ctl));

	SendMessage(hwnd, EM_SETMARGINS, (WPARAM)EC_LEFTMARGIN,  margins);
	SendMessage(hwnd, EM_SETMARGINS, (WPARAM)EC_RIGHTMARGIN, margins);
#endif

	free(text);

	// Yanghui
	return ctl;


}

imeth	gResetText()
{
	SetWindowText(gHandle(self), (LPCSTR) gLanguageText(self,gCurrentLanguage(self))); 	
	return self;
}

imeth	gSetCurrentLanguage(int currLang)
{
	gSetCurrentLanguage(super,currLang);
	if (gLanguageObject(self))
		gSetStringValue(self,gLanguageText(self,gCurrentLanguage(self)));
	return self;	
}

imeth	gSetStringValue(char *val)
{
	gSetStringValue(super,val);
	gSetLanguageText(self,gCurrentLanguage(self),val);
	return self;
}


imeth	object	gDispose, gDeepDispose ()
{
	if (iURL)
		gDispose(iURL);
	return gDispose(super);
}

imeth gSetURL(char *binding)
{
	if (iURL)
		gChangeStrValue(iURL, binding);
	else
		iURL=gNewWithStr(String,binding);
	
	return self;
}

imeth char * gURL()
{
	return iURL ? gStringValue(iURL) : "";
}




