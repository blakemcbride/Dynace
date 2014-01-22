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

defclass  RectControl : Control  {
	HWND     iHCtl;             // handle to control	  	
	UINT     iCtlID;            // control ID			
	iDlg;                       // dialog object		
	int      iWindowControl;    // 1=window control	

	int      (*iDCFun)();       // function - double click	
	
	int		 iDesignMode;		//flag to turn off design mode

	int      iExitType;         // 0=don't exit dialog or IDCANCEL or IDOK  

	char     iF3D;              // 3D effect flag

	short    iFrameThickness;   // the frame thickness in pixel
	char     iFrameColor;       // the frame color
	char     iFrameStyle;       // FRAME_Left | FRAME_Top | FRAME_Right | FRAME_Bottom                        

	char     iFill;             // 0: the rectangle control is transparent 
	                            // 1: the rectangle control is filled with the dot pattern

	short    iDotSize;          // the size of dot in the pattern in pixel
	short    iStepSize;         // the space between dots in pixel
	char     iPatternForeColor; // CLR_Array[iPatternForeColor] is the foreground RGB color
	char     iPatternBackColor; // CLR_Array[iPatternBackColor] is the background RGB color

	short    iDT_Format;        // specifies the method of formatting the text in the rectangle
	char     iTextColor;        // CLR_Array[nColorText] is the foreground color for the text
	                            // Note: the background is transparent so that the pattern brush 
								// that fills the space of the background will be visible

	//  help info associated with a control   
	iTopic;	                    //  help topic associated with control 
	char	*iPrevTopic;        //  previous topic
	int	iAllowEdge;	    //  Allows text to start and end right on the text edge

	int		iUseBackgroundRGBColor;
	COLORREF	iBackgroundRGBColor;
	int		iUseTextRGBColor;
	COLORREF	iTextRGBColor;
};


#include <ctype.h>


#include "color.h"


static long drawFrameRect(HDC hdc, short nColor, int nSize, const RECT *pRect, UINT nFlag, BOOL bNoPrinting);
static long fillRect(HDC hdc, const RECT *pRect, char patternForeColor, char patternBackColor, int useRGB, COLORREF rgbcolor);

static long adjustDrawText(char *pStr);
static long draw3DFrameRect(HDC hdc, const RECT *pRect, BOOL bNoPrinting);
static long paintOtherControls(object self);

static long adjustDrawTextRectForView(RECT *pRect, int nFrameThickness, char frameStyle, int nAveCharWidth);
static long adjustDrawTextRectForPrinting(RECT *pRect, double dFrameThickness, char frameStyle, double dAveCharWidth);

static char *chopOneLineOfText(HDC hdcView, int cxView, char *strValue, char *lineOfText, int nAveCharWidth);

private imeth int pFormatAndPrintText(HDC hdcPrinter, double dScaleX, double dScaleY, const RECT *pRect);
private imeth int pFormatAndDrawTextToMetaFile(HDC hdcMetafile, const RECT *pRect);
private imeth int pFormatAndViewText(HDC hdc, const RECT *pRect, int nAveCharWidth);

				     
private	imeth	long	process_wm_paint(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	PAINTSTRUCT	 ps;
	HGDIOBJ      hFont, hBrush;
	RECT         rect;
	int          nAveCharWidth, nFontHeight;
	object       fontObj;

	gGetWindowRect(self, &rect);

	OffsetRect(&rect, -rect.left, -rect.top);

	BeginPaint(hwnd, &ps);

	SetBkMode(ps.hdc, TRANSPARENT);

	// erase the background
	//hBrush = GetStockObject(LTGRAY_BRUSH);
	//if(hBrush)
	//	FillRect(ps.hdc, &rect, hBrush);
	
	if(iFill) { // fill rect with pattern brush
		SetBkMode(ps.hdc, OPAQUE);
		fillRect(ps.hdc, &rect, iPatternForeColor, iPatternBackColor, iUseBackgroundRGBColor, iBackgroundRGBColor);
	}

	// draw frame rect
	if(gHiddenStatus(self)!=1)
		drawFrameRect(ps.hdc,  iFrameColor, (int)iFrameThickness, &rect, (UINT)iFrameStyle, TRUE);

	// draw 3D
	if(iF3D==1)
		draw3DFrameRect(ps.hdc, &rect, TRUE);


	fontObj = gGetFont(self);
	
	// leave one character space at the beginning and in the end of the rectangle
	nAveCharWidth = 0;
	if(!iAllowEdge && fontObj) 
		nAveCharWidth = gAveCharWidth(fontObj);

	adjustDrawTextRectForView(&rect, iFrameThickness, iFrameStyle, nAveCharWidth);

	pFormatAndViewText(self, ps.hdc, &rect, nAveCharWidth);
	EndPaint(hwnd, &ps);

	return 0L;
}


/////////////////////////////////////////////////////////////////
// hdcMeta: a handle to a metafile 
// hdcView: a handle to a view DC of the parent window
//
imeth gSaveControlToMetaFile(HDC hdcMeta, HDC hdcView)
{
	HGDIOBJ      hFont;
	RECT         rect;
	int          nAveCharWidth, nFontHeight;
	object       fontObj;
	HBRUSH       hBrush;
	POINT        ptTmp;
	int          xOffset, yOffset;
	double       dFactorX, dFactorY;

	if(!hdcMeta || !hdcView)
		return NULL;

	gGetWindowRect(self, &rect);
	gGetScrollPosition(gGetParent(self), &yOffset, &xOffset);
	rect.left   += xOffset;
	rect.right  += xOffset;
	rect.top    += yOffset;
	rect.bottom += yOffset;
	// LPtoDP(hdcView, (LPPOINT)&rect, 2);

	dFactorX = 300.0/GetDeviceCaps(hdcView, LOGPIXELSX);
	dFactorY = 300.0/GetDeviceCaps(hdcView, LOGPIXELSY);
	
	rect.left  = rect.left*dFactorX + 0.5;
	rect.right = rect.right*dFactorX;

	rect.top    = rect.top*dFactorY + 0.5;
	rect.bottom = rect.bottom*dFactorY;
	
	if(iFill)  // fill rect with pattern brush
		fillRect(hdcMeta, &rect, iPatternForeColor, iPatternBackColor, iUseBackgroundRGBColor, iBackgroundRGBColor);

	// leave one character space at the beginning and in the end of the rectangle
	fontObj = gGetFont(self);
	nAveCharWidth = 0;
	if(!iAllowEdge && fontObj) 
		nAveCharWidth = gAveCharWidth(fontObj);

	ptTmp.x = iFrameThickness;
	ptTmp.y = nAveCharWidth;
	LPtoDP(hdcView, (LPPOINT)&ptTmp, 1);
	
	// draw frame rect
	if(gHiddenStatus(self)!=1)
		drawFrameRect(hdcMeta,  iFrameColor, (int)ptTmp.x, &rect, (UINT)iFrameStyle, FALSE);

	// draw 3D
	if(iF3D==1)
		draw3DFrameRect(hdcMeta, &rect, FALSE);

	adjustDrawTextRectForView(&rect, ptTmp.x, iFrameStyle, ptTmp.y);
	pFormatAndDrawTextToMetaFile(self, hdcMeta, &rect);
	
	return self;
}


///////////////////////////////////////////////////////////////
// make other control objects painted on top of this control 
// 
///////////////////////////////////////////////////////////////
static long paintOtherControls(object self)
{
	object seq, obj, linkObject=NULL;
	RECT   rect, rectCtl, rectIntersect;
	int    nOrderSelf=0, nOrderObj=0;
	HWND   hwndCtl, hwnd, hwndP;

	if(!self)
		return -1L;

	linkObject = gGetControls(gGetParent(self));
	if(!linkObject)
		return -1L;

	hwnd = gHandle(self);
	if(!hwnd)
		return -1L;

	hwndP = GetParent(hwnd);

	for (seq=gSequence(linkObject) ; obj = gNext(seq) ; ) {
		nOrderSelf++;
		if(obj==self) {
			gDispose(seq);
			break;
		}
	}

	gGetWindowRect(self, &rect);
	for (seq=gSequence(linkObject) ; obj = gNext(seq) ; ) {
		nOrderObj++;
		if( ClassOf(obj) != ClassOf(self)) {
			gGetWindowRect(obj, &rectCtl);
			IntersectRect(&rectIntersect, &rectCtl, &rect);
			if( !IsRectEmpty(&rectIntersect) ) {
				if( (ClassOf(obj)!=RectControl) || (nOrderObj>nOrderSelf) ) {
					hwndCtl=gHandle(obj);
					gClientToScreenRect(TrackRect, hwndP, &rectIntersect);
					gScreenToClientRect(TrackRect, hwndCtl, &rectIntersect);
					InvalidateRect(hwndCtl, &rectIntersect, FALSE);
				}
			}
		}
	}
	return 0L;
}


private	imeth	long	process_wm_erase(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	PAINTSTRUCT	 ps;
	HGDIOBJ      hBrush;
	RECT         rect;
	COLORREF	 color;

	gGetWindowRect(self, &rect);

	OffsetRect(&rect, -rect.left, -rect.top);

	BeginPaint(hwnd, &ps);

	// erase the background
	hBrush = gHandle(gGetBackBrush(gGetParent(self)));

	if(hBrush)
		FillRect(ps.hdc, &rect, hBrush);
	
	EndPaint(hwnd, &ps);

	return 0L;
}

////////////////////////////////////////////////////////////////////////////////////////
// pFormatAndDrawTextToMetaFile: format and draw text of the control to support WYSIWYG.
//
// HDC  hdcMetafile: a handle to a metafile 
// RECT *pRect:      a pointer to the rectangle of the control in metafile unit
//             
////////////////////////////////////////////////////////////////////////////////////////
private imeth int pFormatAndDrawTextToMetaFile(HDC hdcMetafile, const RECT *pRect)
{
	object   fontObj;
	HFONT    hfontOld, hfontView;
	HDC      hdcView;
	RECT     rectLine;
	int      nTextHeight, nAveCharWidth, nAveCharWidthView, cxView;
	LOGFONT  logfont;
	int      nOldBkMode, nMapModeOld;
	COLORREF oldTextColor;
	char     *strValue, *strValueCopy, *lineOfText, *strPoint;
	double   dScaleX, dScaleY;

	if(!hdcMetafile || !pRect)
		return -1;

	strValue = gLanguageText(self,ENGLISH);

	if(!strValue || !(*strValue))
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	// get the font of the control 
	hfontView = gHandle(fontObj);
	if(!hfontView)
		return -1;

	// make a copy of the strValue
	strValueCopy = (char *)malloc(strlen(strValue)+1);
	if(!strValueCopy)
		return -1;

	lineOfText = (char *)malloc(strlen(strValue)+1);
	if(!lineOfText) {
		free(strValueCopy);
		return -1;
	}

	strcpy(strValueCopy, strValue);
	adjustDrawText(strValueCopy);

	nOldBkMode = SetBkMode(hdcMetafile, TRANSPARENT);
	oldTextColor = GetTextColor(hdcMetafile);
	if (iUseTextRGBColor)
		SetTextColor(hdcMetafile, iTextRGBColor);
	else if(iTextColor>=0 && iTextColor<=CLR_Max)
		SetTextColor(hdcMetafile, CLR_Array[iTextColor]);
	else
		SetTextColor(hdcMetafile, CLR_Array[CLR_Black]);

	hdcView = GetDC(iHCtl);  
	
	dScaleX = 300.0/GetDeviceCaps(hdcView, LOGPIXELSX);
	dScaleY = 300.0/GetDeviceCaps(hdcView, LOGPIXELSY);

	nMapModeOld = SetMapMode(hdcView, MM_TEXT);
	hfontOld = SelectObject(hdcView, hfontView);

	GetObject(hfontView, sizeof(LOGFONT), &logfont);
	nTextHeight = gLineHeight(fontObj)*dScaleY + 0.5;
	logfont.lfWidth  = logfont.lfWidth*dScaleX + 0.5;
	logfont.lfHeight = -(abs(logfont.lfHeight)*dScaleY + 0.5);
	nAveCharWidthView = gAveCharWidth(fontObj);
	nAveCharWidth = nAveCharWidthView*dScaleX + 0.5;

	CopyRect(&rectLine, pRect);
	if(dScaleX>0 && dScaleY>0){
		cxView = (rectLine.right - rectLine.left)/dScaleX + 0.5;
		
		strPoint = strValueCopy;
		if(iDT_Format & DT_SINGLELINE) {  // single line
			strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
			if(iDT_Format & DT_VCENTER)
				rectLine.bottom = rectLine.bottom;
			else if(iDT_Format & DT_BOTTOM)
				rectLine.top = rectLine.bottom - nTextHeight;
			else  // must be the default
				rectLine.bottom = rectLine.top + nTextHeight;

			if(iDT_Format & DT_CENTER)      // align center
				gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_CENTER);
			else if(iDT_Format & DT_RIGHT)  // align right
				gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_RIGHT);
			else                            // align left (must be the default)
				gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_LEFT);
		}
		else {  // multiple lines
			rectLine.bottom = rectLine.top + nTextHeight;
			do {
				strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
				if(iDT_Format & DT_CENTER)      // align center
					gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_CENTER);
				else if(iDT_Format & DT_RIGHT)  // align right
					gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_RIGHT);
				else                            // align left (must be the default)
					gDrawOneLineOfTextToMetaFile(Control, hdcView, dScaleX, hdcMetafile, &logfont, &rectLine, lineOfText, DT_LEFT);

				rectLine.top += nTextHeight;
				rectLine.bottom += nTextHeight;
			} while(strPoint!=NULL && (rectLine.bottom<=pRect->bottom+2*dScaleY) );
		}
	}

	SetMapMode(hdcView, nMapModeOld);
	SelectObject(hdcView, hfontOld);
	ReleaseDC(iHCtl, hdcView);

	SetTextColor(hdcMetafile, oldTextColor);
	SetBkMode(hdcMetafile, nOldBkMode);  // get the original background

	free(lineOfText);
	free(strValueCopy);

	return 0;
}

private imeth int pFormatAndViewText(HDC hdc, const RECT *pRect, int nAveCharWidth)
{
	COLORREF  oldTextColor;
	HGDIOBJ   hOldFont, hFont;
	char      *strValueCopy, *strValue, *lineOfText, *strPoint;
	RECT      rectLine;
	int       nOldBkMode, nTextHeight, cxView;
	SIZE      sizeLine;
	object    fontObj;

	if(!hdc || !pRect)
		return -1;

	fontObj = gGetFont(self);
	if(!fontObj)
		return -1;

	strValue = gLanguageText(self,gCurrentLanguage(self));
	if(!strValue || !(*strValue))
		return -1;

	nTextHeight = gLineHeight(fontObj);
	hFont = gHandle(fontObj);
	if(!hFont)
		return -1;

	strValueCopy = (char *)malloc(strlen(strValue)+1);
	if(!strValueCopy)
		return -1;

	lineOfText = (char *)malloc(strlen(strValue)+1);
	if(!lineOfText) {
		free(strValueCopy);
		return -1;
	}

	nOldBkMode = SetBkMode(hdc, TRANSPARENT);

	oldTextColor = GetTextColor(hdc);
	if (iUseTextRGBColor)
		SetTextColor(hdc, iTextRGBColor);
	else if(iTextColor>=0 && iTextColor<=CLR_Max)
		SetTextColor(hdc, CLR_Array[iTextColor]);
	else
		SetTextColor(hdc, CLR_Array[CLR_Black]);

	hOldFont = SelectObject(hdc, hFont);

	strcpy(strValueCopy, strValue);
	adjustDrawText(strValueCopy);

	CopyRect(&rectLine, pRect);
	cxView = rectLine.right - rectLine.left;

	strPoint = strValueCopy;
	if(iDT_Format & DT_SINGLELINE) {  // single line
		strPoint = chopOneLineOfText(hdc, cxView, strPoint, lineOfText, nAveCharWidth);
		if(lineOfText && (*lineOfText)) {
			if(iDT_Format & DT_VCENTER)
				rectLine.bottom = rectLine.bottom;
			else if(iDT_Format & DT_BOTTOM)
				rectLine.top = rectLine.bottom - nTextHeight;
			else  // must be the default
				rectLine.bottom = rectLine.top + nTextHeight;
#ifdef WIN32
			GetTextExtentPoint32(hdc, lineOfText, strlen(lineOfText), &sizeLine);
#else
			GetTextExtentPoint(hdc, lineOfText, strlen(lineOfText), &sizeLine);
#endif
			if(iDT_Format & DT_CENTER)      // align center
				ExtTextOut(hdc, (rectLine.left+rectLine.right-sizeLine.cx)/2, 
					            (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, ETO_CLIPPED, &rectLine,
						         lineOfText, strlen(lineOfText), NULL);
			else if(iDT_Format & DT_RIGHT)  // align right
				ExtTextOut(hdc, rectLine.right-sizeLine.cx, (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, 
					       ETO_CLIPPED, &rectLine, lineOfText, strlen(lineOfText), NULL);
			else                            // align left (must be the default)
				ExtTextOut(hdc, rectLine.left, (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, 
					       ETO_CLIPPED, &rectLine, lineOfText, strlen(lineOfText), NULL);
		}
	}
	else {  // multiple lines
		rectLine.bottom = rectLine.top + nTextHeight;
		do {
			strPoint = chopOneLineOfText(hdc, cxView, strPoint, lineOfText, nAveCharWidth);
			if(lineOfText && (*lineOfText)) {
#ifdef WIN32
				GetTextExtentPoint32(hdc, lineOfText, strlen(lineOfText), &sizeLine);
#else
				GetTextExtentPoint(hdc, lineOfText, strlen(lineOfText), &sizeLine);
#endif
				if(iDT_Format & DT_CENTER)      // align center
					ExtTextOut(hdc, (rectLine.left+rectLine.right-sizeLine.cx)/2, 
					                (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, ETO_CLIPPED, &rectLine,
						             lineOfText, strlen(lineOfText), NULL);
				else if(iDT_Format & DT_RIGHT)  // align right
					ExtTextOut(hdc, rectLine.right-sizeLine.cx, (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, 
						       ETO_CLIPPED, &rectLine, lineOfText, strlen(lineOfText), NULL);
				else                            // align left (must be the default)
					ExtTextOut(hdc, rectLine.left, (rectLine.top+rectLine.bottom-sizeLine.cy+1)/2, 
						       ETO_CLIPPED, &rectLine, lineOfText, strlen(lineOfText), NULL);
			}
			rectLine.top += nTextHeight;
			rectLine.bottom += nTextHeight;
		} while(strPoint!=NULL && (rectLine.bottom<=pRect->bottom) );
	}

	SelectObject(hdc, hOldFont);
	SetTextColor(hdc, oldTextColor);
	SetBkMode(hdc, nOldBkMode);  // get the original background

	free(lineOfText);
	free(strValueCopy);

	return 0;
}



static long fillRect(HDC hdc, const RECT *pRect, char patternForeColor, char patternBackColor, int useRGB, COLORREF rgbcolor)
{
	HBRUSH   hBrushPattern;
	RECT     rectTmp;
	int      oldBkMode;	
	HBITMAP  hBitmap;
	COLORREF oldTextColor, oldBkColor;
	BYTE     bPattern[] = {0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF,
							0x7F, 0x7F, 0xFF, 0xFF, 0xF7, 0xF7, 0xFF, 0xFF};

	if(!hdc || !pRect)
		return -1L;

	CopyRect(&rectTmp, pRect);

	if(patternForeColor<0 || patternForeColor>CLR_Max)
		patternForeColor = CLR_Black;

	if(patternBackColor<0 || patternBackColor>CLR_Max)
		patternBackColor = CLR_Silver;

	hBitmap = CreateBitmap(8, 8, 1, 1, (LPVOID)bPattern);
	if(!hBitmap)
		return -1L;

	hBrushPattern = CreatePatternBrush(hBitmap);

	if(!hBrushPattern) {
		DeleteObject(hBitmap);
		return -1L;
	}

	if (useRGB) {
		oldTextColor = SetTextColor(hdc, rgbcolor);
		oldBkColor = SetBkColor(hdc, rgbcolor);
	} else {
		oldTextColor = SetTextColor(hdc, CLR_Array[patternForeColor]);
		oldBkColor = SetBkColor(hdc, CLR_Array[patternBackColor]);
	}
	oldBkMode = SetBkMode(hdc, OPAQUE);

	FillRect(hdc, &rectTmp, hBrushPattern);

	SetBkColor(hdc, oldBkColor);
	SetBkMode(hdc, oldBkMode);
	SetTextColor(hdc, oldTextColor);

	DeleteObject((HGDIOBJ) hBrushPattern);
	DeleteObject(hBitmap);

	return 0L;
}


////////////////////////////////////////////////////////
//  ^    first line\n    second line  ^
//  the first '^' is a place hold for ' ', 
//  so is the last '^'. the "\n" is replaced by
//  a newline
//
////////////////////////////////////////////////////////
static long adjustDrawText(char *pStr)
{
	int   nStrLen, i, j;
	char  *strTmp;

	if(!pStr)
		return -1L;

	nStrLen = strlen(pStr);
	if(nStrLen<1)
		return -1L;

	strTmp = (char *)malloc(nStrLen+1);
	strcpy(strTmp, pStr);

	// replace the place hold by space from the beginning
	i=0;
	while(i<nStrLen && strTmp[i]=='^')
	{
		strTmp[i] = 32;
		i++;
	}

	// replace the place hold by space from the end
	i = nStrLen-1;
	while(i>=0 && strTmp[i]=='^')
	{
		strTmp[i] = 32;
		i--;
	}

	i = 0;
	j = 0;
	while (i<nStrLen-1)
	{
		if( strTmp[i]==0x5C && strTmp[i+1]==0x6E ) {  // '\n'
			pStr[j] = 10;
			i += 2;
			j++;
		}
		else {
			pStr[j] = strTmp[i];
			i++;
			j++;
		}
	} 

	if(i==nStrLen-1) {
		pStr[j++] = strTmp[i];
		pStr[j] = '\0';
	}
	else 
		pStr[j] = '\0';

	free(strTmp);

	return 0L;
}


static long adjustDrawTextRectForView(RECT *pRect, int nFrameThickness, char frameStyle, int nAveCharWidth)
{
	if (!pRect) 
		return -1L;

	pRect->left  += nAveCharWidth;
	pRect->right -= nAveCharWidth;

	if( (frameStyle<FRAME_Null) || (frameStyle>FRAME_Full) ) 
		return -1L;
		
	if(frameStyle & FRAME_Left)
		pRect->left += nFrameThickness;
	
	if(frameStyle & FRAME_Right) 
		pRect->right -= nFrameThickness;

	if(pRect->bottom>=pRect->top) {
		if(frameStyle & FRAME_Top)
			pRect->top += nFrameThickness;

		if(frameStyle & FRAME_Bottom)
			pRect->bottom -= nFrameThickness;
	}
	else {
		if(frameStyle & FRAME_Top)
			pRect->top -= nFrameThickness;

		if(frameStyle & FRAME_Bottom)
			pRect->bottom += nFrameThickness;
	}

	return 0L;
}



static long adjustDrawTextRectForPrinting(RECT *pRect, double dFrameThickness, char frameStyle, double dAveCharWidth)
{
	double dLeft, dRight;
	int nFrameThickness = dFrameThickness + 0.5;

	if(!pRect) 
		return -1L;

	dLeft  = pRect->left  + dAveCharWidth;
	dRight = pRect->right - dAveCharWidth;

	if( (frameStyle<FRAME_Null) || (frameStyle>FRAME_Full) ) {
		pRect->left  = dLeft  + 0.5;
		pRect->right = dRight + 0.5;
		return -1L;
	}
		
	if(frameStyle & FRAME_Left)
		dLeft += dFrameThickness;
	
	if(frameStyle & FRAME_Right) 
		dRight -= dFrameThickness;

	pRect->left  = dLeft  + 0.5;
	pRect->right = dRight + 0.5;

	if(pRect->bottom>=pRect->top) {
		if(frameStyle & FRAME_Top)
			pRect->top += nFrameThickness;

		if(frameStyle & FRAME_Bottom)
			pRect->bottom -= nFrameThickness;
	}
	else {
		if(frameStyle & FRAME_Top)
			pRect->top -= nFrameThickness;

		if(frameStyle & FRAME_Bottom)
			pRect->bottom += nFrameThickness;
	}

	return 0L;
}



///////////////////////////////////////////////////////////////////////////////////////////////
// drawDisabledFrameRect:
//
// pRect:  a point to the rectangle of the frame
//
///////////////////////////////////////////////////////////////////////////////////////////////
static long draw3DFrameRect(HDC hdc, const RECT *pRect, BOOL bNoPrinting)
{
	HBRUSH hBrush, oldHbrush;
	RECT   rect;
	int    x, y;

	if(!hdc || !pRect) 
		return -1L;

	CopyRect(&rect, pRect);
	if(bNoPrinting)
		OffsetRect(&rect, -rect.left, -rect.top);

	x = rect.right - rect.left;
	y = abs(rect.bottom - rect.top);

	if(rect.bottom>=rect.top) {  
		// top and left bar
		hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left, rect.top, x, 1, PATCOPY);
		PatBlt(hdc, rect.left, rect.top, 1, y, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		hBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOWTEXT));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left + 1, rect.top + 1, x - 2, 1, PATCOPY);
		PatBlt(hdc, rect.left + 1, rect.top + 1, 1, y - 2, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		// bottom and right bar
		hBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOW));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left, rect.bottom-1, x, 1, PATCOPY);
		PatBlt(hdc, rect.right-1, rect.top, 1, y, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNFACE));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left + 1, rect.bottom-2, x - 2, 1, PATCOPY);
		PatBlt(hdc, rect.right-2, rect.top + 1, 1, y - 2, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);
	}
	else { 
		// top and left bar
		hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left, rect.top, x, -1, PATCOPY);
		PatBlt(hdc, rect.left, rect.top, 1, -y, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		hBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOWTEXT));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left + 1, rect.top - 1, x - 2, -1, PATCOPY);
		PatBlt(hdc, rect.left + 1, rect.top - 1, 1, -(y - 2), PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		// bottom and right bar
		hBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOW));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left, rect.bottom+1, x, -1, PATCOPY);
		PatBlt(hdc, rect.right-1, rect.top, 1, -y, PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNFACE));	
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);
		PatBlt(hdc, rect.left + 1, rect.bottom+2, x - 2, -1, PATCOPY);
		PatBlt(hdc, rect.right-2, rect.top - 1, 1, -(y - 2), PATCOPY);
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);
	}

	return 0L;
}


///////////////////////////////////////////////////////////////////////////////////////////////
// drawFrameRect:
//
// nColor: CLR_Array[nColor] is the color for the frame
// nSize:  the size of the frame in pixel
// pRect:  a point to the rectangle of the frame
// nFlag:  one of "FRAME_Full | FRAME_Null | FRAME_Left | FRAME_Top | FRAME_Right | FRAME_Bottom"
//
///////////////////////////////////////////////////////////////////////////////////////////////
static long drawFrameRect(HDC hdc, short nColor, int nSize, const RECT *pRect, UINT nFlag, BOOL bNoPrinting)
{
	HBRUSH hBrush, oldHbrush;
	RECT   rect;
	int    x, y;

	if(!pRect) 
		return -1L;

	CopyRect(&rect, pRect);
	if(bNoPrinting)
		OffsetRect(&rect, -rect.left, -rect.top);

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	if(nSize < 0)
		nSize = abs(nSize);

	x = rect.right - rect.left;
	y = abs(rect.bottom - rect.top);

	if( (nFlag<FRAME_Null) || (nFlag>FRAME_Full) )
		nFlag = FRAME_Full;
		
	if( (nFlag&FRAME_Left) || (nFlag&FRAME_Right) )
		if(nSize > x)
			nSize = x;

	if( (nFlag&FRAME_Left) && (nFlag&FRAME_Right) )
		if(nSize > x/2)
			nSize = x/2.0 + 0.5;

	if( (nFlag&FRAME_Top) || (nFlag&FRAME_Bottom) )
		if(nSize > y)
			nSize = y;

	if( (nFlag&FRAME_Top) && (nFlag&FRAME_Bottom) )
		if(nSize > y/2)
			nSize = y/2.0 + 0.5;

	hBrush = CreateSolidBrush(CLR_Array[nColor]);
	if(!hBrush)
		return -1L;

	oldHbrush = SelectObject(hdc, hBrush);

	if(rect.bottom >= rect.top) { 
		// top bar
		if(nFlag & FRAME_Top)
			PatBlt(hdc, rect.left, rect.top, x, nSize, PATCOPY);

		// bottom bar
		if(nFlag & FRAME_Bottom)
			PatBlt(hdc, rect.left, (rect.bottom - nSize), x, nSize, PATCOPY);

		// left bar
		if(nFlag & FRAME_Left)
			PatBlt(hdc, rect.left, rect.top, nSize, y, PATCOPY);

		// right bar
		if(nFlag & FRAME_Right)
			PatBlt(hdc, (rect.right - nSize), rect.top, nSize, y, PATCOPY);
	}
	else {  
		// top bar
		if(nFlag & FRAME_Top)
			PatBlt(hdc, rect.left, rect.top, x, -nSize, PATCOPY);

		// bottom bar
		if(nFlag & FRAME_Bottom)
			PatBlt(hdc, rect.left, (rect.bottom + nSize), x, -nSize, PATCOPY);

		// left bar
		if(nFlag & FRAME_Left)
			PatBlt(hdc, rect.left, rect.top, nSize, -y, PATCOPY);

		// right bar
		if(nFlag & FRAME_Right)
			PatBlt(hdc, (rect.right - nSize), rect.top, nSize, -y, PATCOPY);
	}

	SelectObject(hdc, oldHbrush);
	DeleteObject((HGDIOBJ) hBrush);
	return 0L;
}


private	imeth	long	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	RECT rect;
	if(!self || !hwnd)
		return 0L;

	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));

	gGetWindowRect(self, &rect);
	InvalidateRect(GetParent(hwnd), &rect, FALSE);
	return 0L;
}


private	imeth	long	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	RECT rect;
	if(!self || !hwnd)
		return 0L;

	if (iTopic)
		gSetTopic(HelpSystem, iPrevTopic);

	gGetWindowRect(self, &rect);
	InvalidateRect(GetParent(hwnd), &rect, FALSE);
	return 0L;
}


private	imeth	long	process_wm_char(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	if (wParam == 13) {  // Enter
#ifdef	WIN32
		PostMessage(gHandle(iDlg), WM_COMMAND, MAKEWPARAM((unsigned short)iCtlID, BN_CLICKED), (LPARAM) iHCtl);
#else
		PostMessage(gHandle(iDlg), WM_COMMAND, (WPARAM) iCtlID, MAKELPARAM(iHCtl, BN_CLICKED));
#endif
	}

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
			return gPerformJavaObjCallback((object)iDCFun, iDlg);
		else if (JavaScriptClassSurrogate  &&  IsObj((object)iDCFun)  &&  ClassOf(iDCFun) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"))", gStringValue((object)iDCFun), (long) self, (long) iDlg);
			gExecuteStringNR(JavaScriptClassSurrogate, cmd);
		} else
			iDCFun(self, iDlg);
		return 0L;
	} 
	else if (iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg))
		return 0L;
	else
		return gCallDefaultProc(self, mMsg, wParam, lParam);
}

private	imeth	long	process_wm_lbuttonup(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	gExecuteSetFunctions(self, iDlg);
	return gCallDefaultProc(self, mMsg, wParam, lParam);
}



cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object  font;
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);

	iDlg = dlg;
	iCtlID = ctlID;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

	gSetStyle(obj, WS_VISIBLE);
	iF3D = 0;

	// fill in:
	iFill = 0;
	gBackBrush(obj, vNew(StockBrush, NULL_BRUSH) );
	
	iDotSize = 0;
	iStepSize    = 0;
	iPatternForeColor = CLR_Black;
	iPatternBackColor = CLR_Silver;

	// frame:
	iFrameThickness = 1;
	iFrameColor = CLR_Black;    
	iFrameStyle = (FRAME_Left | FRAME_Right | FRAME_Top | FRAME_Bottom);       

	// text:
	iDT_Format = (DT_VCENTER | DT_CENTER | DT_SINGLELINE);
	iTextColor = CLR_Black;

	gSetLanguageText(self,ENGLISH, "Rect Control"); 
	iDesignMode=1;

	// text:
	// gSetFont must set a new font object.
	// gSetFont(obj, vNew(ExternalFont, "Arial", 10));
	font = gGetFont(Application);
	if(font)
		gSetFont(obj, font);
	else
		gSetFont(obj, vNew(ExternalFont, "Arial", 10));

	//  Init message handlers 

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);
	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);
	gAddHandlerAfter(obj, (unsigned) WM_PAINT, process_wm_paint);
	gAddHandlerAfter(obj, (unsigned) WM_ERASEBKGND, process_wm_erase);

	return obj;
}


cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object  font;
	object	obj = gNewCont(super, name, "button", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iWindowControl = 1;
	iCtlID = ctlID;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

	gSetStyle(obj, WS_VISIBLE);
	iF3D = 0;

	// fill in:
	iFill = 0;
	gBackBrush(obj, vNew(StockBrush, NULL_BRUSH) );

	iDotSize = 0;
	iStepSize    = 0;
	iPatternForeColor = CLR_Black;
	iPatternBackColor = CLR_None;

	// frame:
	iFrameThickness = 1;
	iFrameColor = CLR_Black;    
	iFrameStyle = (FRAME_Left | FRAME_Right | FRAME_Top | FRAME_Bottom);     

	// text:
	iDT_Format = (DT_VCENTER | DT_CENTER | DT_SINGLELINE);
	iTextColor = CLR_Black;
	gSetLanguageText(obj,ENGLISH,"Rect Control"); 
	
	iDesignMode=1;

	// text:
	// gSetFont must set a new font object.
	// gSetFont(obj, vNew(ExternalFont, "Arial", 10));
	font = gGetFont(Application);
	if(font)
		gSetFont(obj, font);
	else
		gSetFont(obj, vNew(ExternalFont, "Arial", 10));

	// Init message handlers  
	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  // no auto default processing

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONDBLCLK, 0);  // no auto default processing

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONUP, process_wm_lbuttonup);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONUP, 0);  // no auto default processing

	gAddHandlerAfter(obj, (unsigned) WM_PAINT, process_wm_paint);
	gAddHandlerAfter(obj, (unsigned) WM_ERASEBKGND, process_wm_erase);
	
	return obj;
}



imeth	gSetStyle(DWORD style)
{
	style = BS_OWNERDRAW | WS_CHILD | WS_VISIBLE | (style & 0x11110000L);

	// Note: you CAN NOT "OR" the style as follows:
	// style = BS_OWNERDRAW | WS_VISIBLE | WS_CHILD | style;
	// The reason is that BS_OWNERDRAW=0x0000000BL.
	// Some other BS_ controls  can use the "OR", but not
	// everyone.

	return gSetStyle(super, style);
}



imeth	gInitialize(HWND hDlg, dlg)
{
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "Rect control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);

	return gInitialize(super, hDlg, dlg);
}



imeth	int	gShow()
{
	object	font;
	if (iHCtl)
		return 0;
	
	font = gGetFont(self);
		
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);
	if (font)
		SendMessage(iHCtl, WM_SETFONT, (WPARAM) gHandle(font), (LPARAM) 0L);

	gInitialize(super, (HWND)0, NULL);
	if (gIsKindOf(iDlg, Window)) {
		RECT	rect;
		gGetWindowRect(self, &rect);
		InvalidateRect(gHandle(iDlg), &rect, TRUE);
	}
	return 0;
}


imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);

	if (IsObj((object) iDCFun))
		gDispose((object) iDCFun);

	if (iTopic)
		iTopic=gDispose(iTopic);

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


imeth	gSetDesignMode(int mode)
{
	iDesignMode=mode;
	return self;
}


imeth	int	gDesignMode()
{
	return iWindowControl  &&  iDlg  &&  gModifyChildren(iDlg) && iDesignMode;
}



imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_RECT_t  *v = vp;
	int	            height, width, xPos, yPos, len;
	int	            sm = gSetScalingMode(Application, SM_PIXELS);
	object          fobj;
	if(!v)
		return NULL;

	strncpy(v->name, gName(self), (sizeof(v->name)-1));

	// size and position of the rectangle
	gGetSize(self, &height, &width);
	gGetVirtualPosition(self, &yPos, &xPos);

	v->height = height;
	v->width  = width;
	v->xPos   = xPos;
	v->yPos   = yPos;

	// the status of the rectangle
	v->hidden = gHiddenStatus(self) == 1 ? 'Y' : 'N';
	v->disabled = gDisableStatus(self) == 1 ? 'Y' : 'N';
	v->f3D = iF3D;

	// font used for the text inside the rectangle
	fobj = gGetFont(self);
	v->fontNameLen = fobj ? strlen(gName(fobj)) + 1 : 0;
	v->fontSize = fobj ? gPointSize(fobj) : 0;

	// the text and its format inside the rectangle
	v->textLength = *gLanguageText(self,ENGLISH) ? strlen(gLanguageText(self,ENGLISH)) + 1 : 0;
	v->DT_Format = iDT_Format; 
	v->textColor = iTextColor;

	// frame of the rectangle
	v->frameThickness = iFrameThickness;
	v->frameColor = iFrameColor;
	v->frameStyle = iFrameStyle;  

	// pattern that fills the rectangle
	
	v->dotSize = iDotSize;
	v->stepSize = iStepSize;
	v->patternForeColor = iPatternForeColor;
	v->patternBackColor = iPatternBackColor;

	v->fill = iFill;

	if ( iTopic  &&  (len=gSize(iTopic)) )
		v->helpTopicLen = len + 1;
	else
		v->helpTopicLen = 0;

	gSetScalingMode(Application, sm);
	return self;
}



imeth	gSetControlParameters(void *vp)
{
	CTLTYPE_RECT_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	if(!v)
		return NULL;

	gSetName(self, v->name);
	
	// size and position of the rectangle
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);

	// the status of the rectangle
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	// v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	gDisable(self);

	iF3D = (v->f3D ? 1 : 0);

	// iValue to be determined by v->textLength;
	gSetDT_Format(self, v->DT_Format);
	gSetTextColor(self, v->textColor);

	// do not change the order of following statement
	gSetFrameColor(self, v->frameColor);
	gSetFrameStyle(self, v->frameStyle);
	gSetFrameThickness(self, v->frameThickness);
	
	gSetFill(self, v->fill);
	gSetPattern(self, v->dotSize, v->stepSize, v->patternForeColor, v->patternBackColor);

	if(iFill==0)
		gBackBrush(self, vNew(StockBrush, NULL_BRUSH) );
	
	gSetScalingMode(Application, sm);
	return self;
}



imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_RECT_t	v;
	short	type = CTLTYPE_RECT, size = sizeof(v);
	object	fobj = gGetFont(self);
	if(!fp)
		return NULL;

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.textLength  &&  1 != fwrite(gLanguageText(self,ENGLISH), (int) v.textLength, 1, fp))
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
	CTLTYPE_RECT_t	v;
	int             end;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];
	double          controlScaleFactor;
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

			if( ( (v.frameStyle & FRAME_Left) || (v.frameStyle & FRAME_Right) ) && 
			    ( (v.frameStyle & FRAME_Top)  || (v.frameStyle & FRAME_Bottom) ) ) 
				v.frameThickness = v.frameThickness*controlScaleFactor*min(dCxScale, dCyScale) + 0.5;

			else if ( (v.frameStyle & FRAME_Left) || (v.frameStyle & FRAME_Right) )
				v.frameThickness = v.frameThickness*controlScaleFactor*dCxScale + 0.5;

			else if ( (v.frameStyle & FRAME_Top) || (v.frameStyle & FRAME_Bottom) )
				v.frameThickness = v.frameThickness*controlScaleFactor*dCyScale + 0.5;
			
			v.dotSize = v.dotSize*controlScaleFactor*dCxScale + 0.5;
			if(v.dotSize<1)
				v.dotSize = 1;

			v.stepSize = v.stepSize*controlScaleFactor*dCxScale + 0.5;
			if(v.stepSize<1)
				v.stepSize = 1;

			// if the font information was not saved, adjust the fontsize. 
			hWnd = gHandle(parent);
			hDc = GetDC(hWnd);  // if hWnd=NULL, GetDC retrieves the device context for the entire screen
			logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
			ReleaseDC(hWnd, hDc);
		
			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(abs(MulDiv(v.height, 72, logPixelsy)), v.fontSize);
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddRectControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

	// the text 
	if (v.textLength) {
		p = v.textLength > BUFLEN ? malloc((unsigned)v.textLength) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.textLength, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		gSetLanguageText(ctl, ENGLISH, p);
		if (v.textLength > BUFLEN)
			free(p);
	}

	// helpTopic
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

	// the font:
	// when scaling is processed, the v.fontSize may be too small and not available for the chosen font,
	// although system would get the closest match, but the font height would not match the control height.
	// In this case, Arial font will be chosen. 
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
	
	gSetControlParameters(ctl, &v);

	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}



cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_RECT_t	v;
	int             end;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];
	double          controlScaleFactor;
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

	v.xPos += nXshift;
	v.yPos += nYshift;

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddRectControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

	// the text 
	if (v.textLength) {
		p = v.textLength > BUFLEN ? malloc((unsigned)v.textLength) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.textLength, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.textLength > BUFLEN)
			free(p);
	}

	// helpTopic
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



imeth object gSetPattern(short dotSize, short stepSize, char patternForeColor, char patternBackColor)
{
	iUseBackgroundRGBColor = 0;
	if(stepSize<1)
		stepSize = 1;
	iStepSize = stepSize;

	if(dotSize<1)
		dotSize = 1;
	iDotSize = dotSize;

	if(patternForeColor<0 || patternForeColor>CLR_Max)
		patternForeColor = CLR_Black;
	iPatternForeColor = patternForeColor;

	if(patternBackColor<0 || patternBackColor>CLR_Max)
		patternBackColor = CLR_Black;
	iPatternBackColor = patternBackColor;

	return self;
}



imeth object gSetFrameColor(char frameColor)
{
	if (frameColor>=0 && frameColor<=CLR_Max)
		iFrameColor = frameColor;
	else
		iFrameColor = CLR_Black;

	return self;
}


imeth object gSetFrameStyle(char frameStyle)
{
	if( frameStyle>=FRAME_Null && frameStyle<=FRAME_Full )
		iFrameStyle = frameStyle;
	else
		iFrameStyle = FRAME_Full;

	return self;
}


imeth object gSetFrameThickness(short frameThickness)
{
	RECT   rect;
	int    x, y;

	if(frameThickness<0)
		frameThickness = 0;

	if(!iHCtl) {
		iFrameThickness = frameThickness;
		return self;
	}

	GetWindowRect(iHCtl, &rect);
	x = rect.right - rect.left;
	y = abs(rect.bottom-rect.top);

	// adjust frameThickness according to iFrameStyle
	if( iFrameStyle<FRAME_Null || iFrameStyle>FRAME_Full )
		iFrameStyle = FRAME_Full;

	if( (iFrameStyle&FRAME_Left) || (iFrameStyle&FRAME_Right) )
		if(frameThickness > x)
			frameThickness = x;

	if( (iFrameStyle&FRAME_Left) && (iFrameStyle&FRAME_Right) )
		if(frameThickness > x/2)
			frameThickness = x/2.0 + 0.5;

	if( (iFrameStyle&FRAME_Top) || (iFrameStyle&FRAME_Bottom) )
		if(frameThickness > y)
			frameThickness = y;

	if( (iFrameStyle&FRAME_Top) && (iFrameStyle&FRAME_Bottom) )
		if(frameThickness > y/2)
			frameThickness = y/2.0 + 0.5;

	iFrameThickness = frameThickness;

	return self;
}


imeth object gSetFill(char fill)
{
	if(fill)
		iFill=1;
	else
		iFill=0;

	return self;
}


imeth object gSetTextColor(char textColor)
{
	iUseTextRGBColor = 0;
	if (textColor>=0 && textColor<=CLR_Max)
		iTextColor = textColor;
	else
		iTextColor = CLR_Black;

	return self;
}


imeth object gSetDT_Format(short DT_Format)
{
	if (DT_Format >= 0)
		iDT_Format = DT_Format;
	else
		iDT_Format = (DT_VCENTER | DT_CENTER | DT_SINGLELINE);
	
	return self;
}


imeth object gSetTextObj(object textObj)
{
	gSetLanguageText(self,gCurrentLanguage(self),textObj?gStringValue(textObj):"");
	return self;
}


imeth object gSetTextObjWithStr(char *pStr)
{
	gSetLanguageText(self,gCurrentLanguage(self),pStr);
	return self;
}


////////////////////////////////////////////////////////////////////////////////
// gPrintCtlScreen: print the text of the control at the correct location
// printerObj:      the printer object
// dScaleX:         the scaling factor for the x coordinate of the control, 
//	                dScaleX = printerLogPixelsX/(double)viewLogPixelsX;
// dScaleY:         the scaling factor for the y coordinate of the control, 
//                  dScaleY = printerLogPixelsY/(double)viewLogPixelsY
//
////////////////////////////////////////////////////////////////////////////////
imeth	gPrintCtlScreen(object printerObj, double dScaleX, double dScaleY, int nViewOffsetX, int nViewOffsetY)
{
	RECT    rect;
	HDC     hdcPrinter;
	object  fontObj, parentObj;
	int     nFrameThickness;
	double  dFrameThickness, dAveCharWidth, dScaleXX, dScaleYY;
	int     nPhysicalWidth, nPhysicalHeight, nPrinterOffsetX, nPrinterOffsetY;
	HBRUSH  hBrush;

	// check input parameters:
	if(!printerObj)    // validate the printerObj
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) )  // get and validate the print DC
		return NULL;

	if(dScaleX<=0 || dScaleY<=0)  // validate the view to printer scaling factors
		return NULL;

	if(gHiddenStatus(self)==1)  // skipping printing
		return self;

	fontObj = gGetFont(self);

	parentObj=gGetParent(self);
	if(!parentObj)
		return NULL;

	gCLDGetPhysicalParameters(printerObj, &nPhysicalWidth, &nPhysicalHeight, &nPrinterOffsetX, &nPrinterOffsetY);

	// get the rect of the control 
	SetRectEmpty(&rect);
	gGetWindowRect(self, &rect);

	if(iDotSize<1)
		iDotSize=1;

	if(iStepSize<1)
		iStepSize=1;

	if( ( (iFrameStyle & FRAME_Left) || (iFrameStyle & FRAME_Right) ) && 
		( (iFrameStyle & FRAME_Top)  || (iFrameStyle & FRAME_Bottom) ) ) 
		dFrameThickness = iFrameThickness*min(dScaleX, dScaleY);
	else if ( (iFrameStyle & FRAME_Left) || (iFrameStyle & FRAME_Right) )
		dFrameThickness = iFrameThickness*dScaleX;
	// else if ( (iFrameStyle & FRAME_Top) || (iFrameStyle & FRAME_Bottom) )
	//	dFrameThickness = iFrameThickness*dScaleY;
	else 
		dFrameThickness = iFrameThickness*dScaleY;

	dScaleXX = dScaleX;
	dScaleYY = dScaleY;
	if( !gGetScaleFlg(parentObj) ) {  // the scaling flag is not set (for Integra)
		// scale the rect from view DC to the printer DC
		rect.left   = (rect.left   + nViewOffsetX) * dScaleX - nPrinterOffsetX;
		rect.right  = (rect.right  + nViewOffsetX) * dScaleX - nPrinterOffsetX;
		rect.top    = (rect.top    + nViewOffsetY) * dScaleY - nPrinterOffsetY;
		rect.bottom = (rect.bottom + nViewOffsetY) * dScaleY - nPrinterOffsetY;
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
			rect.left   = ( (rect.left  + nViewOffsetX - 0.5)/dblTmp - MFMarginRect.left )*dScaleX - nPrinterOffsetX;
			rect.right  = ( (rect.right + nViewOffsetX - 0.5)/dblTmp - MFMarginRect.left )*dScaleX - nPrinterOffsetX;
			dScaleXX = dScaleX/dblTmp;
		}
		else
			return NULL;

		if( (dblTmp=controlScale*MFCyScale) > 0 ) {
			rect.top    = ( (rect.top    + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY;
			rect.bottom = ( (rect.bottom + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY;
			dScaleYY = dScaleY/dblTmp;
		}
		else
			return NULL;

		dblTmp = 0.0;
		if( ( (iFrameStyle & FRAME_Left) || (iFrameStyle & FRAME_Right) ) && 
		    ( (iFrameStyle & FRAME_Top)  || (iFrameStyle & FRAME_Bottom) ) ) 
			dblTmp = controlScale*min(MFCxScale, MFCyScale);
		else if ( (iFrameStyle & FRAME_Left) || (iFrameStyle & FRAME_Right) )
			dblTmp = controlScale*MFCxScale;
		else if ( (iFrameStyle & FRAME_Top) || (iFrameStyle & FRAME_Bottom) )
			dblTmp = controlScale*MFCyScale;

		if(dblTmp>0) 
			dFrameThickness = dFrameThickness/dblTmp;
	}
	nFrameThickness = dFrameThickness + 0.5;

	{ // turn the page if necessary
		int nPage, nPage2, nTopMargin, nBottomMargin, nTmp;

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

	if(iFill)  // fill rect with pattern brush
		fillRect(hdcPrinter, &rect, iPatternForeColor, iPatternBackColor, iUseBackgroundRGBColor, iBackgroundRGBColor);

	// draw frame rect
	if(gHiddenStatus(self)!=1)
		drawFrameRect(hdcPrinter, iFrameColor, nFrameThickness, &rect, (UINT)iFrameStyle, FALSE);
	
	// draw 3D
	if(iF3D==1)
		draw3DFrameRect(hdcPrinter, &rect, FALSE);

	// leave one character space at the beginning and in the end of the rectangle
	dAveCharWidth = 0.0;
	if(fontObj)
		dAveCharWidth = gAveCharWidth(fontObj)*dScaleXX;

	adjustDrawTextRectForPrinting(&rect, dFrameThickness, iFrameStyle, dAveCharWidth);
	pFormatAndPrintText(self, hdcPrinter, dScaleXX, dScaleYY, &rect);

	return self;
}


/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the rect control of the cld file for printing
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_RECT_t    v;
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

	ctl = gCLDNewWindowControl(RectControl, v.name, parentObj);
	iv = ivPtr(ctl);

	// the text 
	if (v.textLength) {
		p = v.textLength > BUFLEN ? malloc((unsigned)v.textLength) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.textLength, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.textLength > BUFLEN)
			free(p);
	}

	// helpTopic
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
	object  font;
	object	obj = gCLDNewCont(super, name, "button");
	ivType	*iv = ivPtr(obj);

	iDlg = parentObj;
	iWindowControl = 0;
	iCtlID = 0;
	iExitType = 0;

	iF3D = 0;

	// fill in:
	iFill = 0;
	gBackBrush(obj, vNew(StockBrush, NULL_BRUSH) );

	iDotSize = 1;
	iStepSize    = 1;
	iPatternForeColor = CLR_Black;
	iPatternBackColor = CLR_None;

	// frame:
	iFrameThickness = 1;
	iFrameColor = CLR_Black;    
	iFrameStyle = (FRAME_Left | FRAME_Right | FRAME_Top | FRAME_Bottom);     

	// text:
	iDT_Format = (DT_VCENTER | DT_CENTER | DT_SINGLELINE);
	iTextColor = CLR_Black;
	gSetLanguageText(self,ENGLISH,"Rect Control"); 

	// text:
	// gSetFont must set a new font object.
	// gSetFont(obj, vNew(ExternalFont, "Arial", 10));
	font = gGetFont(Application);
	if(font)
		gSetFont(obj, font);
	else
		gSetFont(obj, vNew(ExternalFont, "Arial", 10));

	return obj;
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
	object   fontObj;
	HFONT    hfontOld, hfontView;
	HDC      hdcView;
	RECT     rectLine;
	int      nTextHeight, nAveCharWidth, nAveCharWidthView, cxView;
	LOGFONT  logfont;
	int      nOldBkMode, nMapModeOld;
	COLORREF oldTextColor;
	char     *strValue, *strValueCopy, *lineOfText, *strPoint;

	if(!hdcPrinter || !pRect)
		return -1;

	if(dScaleX<0)
		dScaleX = -dScaleX;

	if(dScaleY<0)
		dScaleY = -dScaleY;

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

	// make a copy of the strValue
	strValueCopy = (char *)malloc(strlen(strValue)+1);
	if(!strValueCopy)
		return -1;

	lineOfText = (char *)malloc(strlen(strValue)+1);
	if(!lineOfText) {
		free(strValueCopy);
		return -1;
	}

	strcpy(strValueCopy, strValue);
	adjustDrawText(strValueCopy);

	nOldBkMode = SetBkMode(hdcPrinter, TRANSPARENT);
	oldTextColor = GetTextColor(hdcPrinter);
	if (iUseTextRGBColor)
		SetTextColor(hdcPrinter, iTextRGBColor);
	else if(iTextColor>=0 && iTextColor<=CLR_Max)
		SetTextColor(hdcPrinter, CLR_Array[iTextColor]);
	else
		SetTextColor(hdcPrinter, CLR_Array[CLR_Black]);

	hdcView = GetDC(iHCtl);  
	nMapModeOld = SetMapMode(hdcView, MM_TEXT);
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

		strPoint = strValueCopy;
		if(iDT_Format & DT_SINGLELINE) {  // single line
			strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
			if(iDT_Format & DT_VCENTER)
				rectLine.bottom = rectLine.bottom;
			else if(iDT_Format & DT_BOTTOM)
				rectLine.top = rectLine.bottom - nTextHeight;
			else  // must be the default
				rectLine.bottom = rectLine.top + nTextHeight;

			if(iDT_Format & DT_CENTER)      // align center
				gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_CENTER, nAveCharWidth);
			else if(iDT_Format & DT_RIGHT)  // align right
				gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_RIGHT, nAveCharWidth);
			else                            // align left (must be the default)
				gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_LEFT, nAveCharWidth);
		}
		else {  // multiple lines
			rectLine.bottom = rectLine.top + nTextHeight;
			do {
				strPoint = chopOneLineOfText(hdcView, cxView, strPoint, lineOfText, nAveCharWidthView);
				if(iDT_Format & DT_CENTER)      // align center
					gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_CENTER, nAveCharWidth);
				else if(iDT_Format & DT_RIGHT)  // align right
					gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_RIGHT, nAveCharWidth);
				else                            // align left (must be the default)
					gDrawOneLineOfText(Control, hdcView, dScaleX, hdcPrinter, &logfont, &rectLine, lineOfText, DT_LEFT, nAveCharWidth);

				rectLine.top += nTextHeight;
				rectLine.bottom += nTextHeight;
			} while(strPoint!=NULL && (rectLine.bottom<=pRect->bottom+2*dScaleY) );
		}
	}

	SetMapMode(hdcView, nMapModeOld);
	SelectObject(hdcView, hfontOld);
	ReleaseDC(iHCtl, hdcView);

	SetTextColor(hdcPrinter, oldTextColor);
	SetBkMode(hdcPrinter, nOldBkMode);  // get the original background

	free(lineOfText);
	free(strValueCopy);

	return 0;
}


///////////////////////////////////////////////////////////////////////////////////
// chopOneLineOfText: chop one line of text
//
// HDC hdcView:       a handle to the view DC
// int cxView:        the width of the control in view DC unit
// char *strValue:    a string point to the string in the string object
// char *lineOfText:  the line of text chopped from strValue
//
// Note: if strValue has more than one line of text, it returns a string point 
//       which points to the next string to be chopped, else it returns NULL
//
///////////////////////////////////////////////////////////////////////////////////
static char * chopOneLineOfText(HDC hdcView, int cxView, char *strValue, char *lineOfText, int nAveCharWidth)
{
	char *strPoint, *strValueCopy, *token, *pStrValueCopy;
	int  nPreToken, nUpToToken, nUpToToken0, nStrValue, nTotal, i;
	SIZE sizeLine;

	if(!hdcView || !strValue || !(*strValue) || !lineOfText) {
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
		strncpy(lineOfText, strValue, nUpToToken);
		lineOfText[nUpToToken] = '\0';
		free(strValueCopy);

		nTotal = strlen(lineOfText);
		for(i=0; i<nTotal; i++)
			if(lineOfText[i] == 10) 
				lineOfText[i] = '\0';

		// skip the space at the beginning of next line for 
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
			pStrValueCopy = strValueCopy + nUpToToken + 1 ;
		else
			pStrValueCopy = strValueCopy + nUpToToken;

#ifdef WIN32
		GetTextExtentPoint32(hdcView, strValue, nUpToToken, &sizeLine);
#else
		GetTextExtentPoint(hdcView, strValue, nUpToToken, &sizeLine);
#endif
		if(sizeLine.cx > cxView + nAveCharWidth/5) {  // nAveCharWidth/5 pixel in view
			strncpy(lineOfText, strValue, nUpToToken0); 
			lineOfText[nUpToToken0] = '\0';
			free(strValueCopy);

			// check '\n' in lineOfText, if there is a '\n', it is chopped there 
			nTotal = strlen(lineOfText);
			for(i=0; i<nTotal; i++)
				if(lineOfText[i] == 10) {
					lineOfText[i] = '\0';
					return (strValue + strlen(lineOfText)+1);
				}
			return (strValue + nPreToken);
		}
		nUpToToken0 = nUpToToken;
	}
	strcpy(lineOfText, strValue);
	free(strValueCopy);

	// check '\n' in lineOfText, if there is a '\n', it is chopped there 
	nTotal = strlen(lineOfText);
	for(i=0; i<nTotal; i++)
		if(lineOfText[i] == 10) {
			lineOfText[i] = '\0';
			return (strValue + strlen(lineOfText)+1);
		}
	return NULL;
}

imeth	int	gAllowEdge(int val)
{
	int	pval = iAllowEdge;
	iAllowEdge = val;
	return pval;
}

imeth	int	gDraw3D(int val)
{
	int	pval = iF3D;

	if (val > 0)
		val = 1;
	if (val >= 0)
		iF3D = val;
	return pval;
}

imeth	gSetBackgroundColor(char backcolor)
{
	iFill = 1;
	return gSetPattern(self, 1, 1, backcolor, backcolor);
}

imeth	gSetBackgroundRGBColor(int red, int green, int blue)
{
	iFill = 1;
	iUseBackgroundRGBColor = 1;
	iBackgroundRGBColor = RGB(red, green, blue);
	return self;
}

imeth	gSetTextRGBColor(int red, int green, int blue)
{
	iUseTextRGBColor = 1;
	iTextRGBColor = RGB(red, green, blue);
	return self;
}

imeth	gSetBackgroundColorRef(COLORREF clr)
{
	iFill = 1;
	iUseBackgroundRGBColor = 1;
	iBackgroundRGBColor = clr;
	return self;
}

imeth	gSetTextColorRef(COLORREF clr)
{
	iUseTextRGBColor = 1;
	iTextRGBColor = clr;
	return self;
}


imeth gWriteXML(FILE *fp)
{

	CTLTYPE_RECT_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);
	int loop;
	char	**languages=gLanguages(Application);
	
	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<rectangle>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
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
	fprintf(fp,"\t\t\t<f3D>%d</f3D>\n",v.f3D);
	fprintf(fp,"\t\t\t<textLength>%d</textLength>\n",v.textLength);
	fprintf(fp,"\t\t\t<DT_Format>%d</DT_Format>\n",v.DT_Format);
	fprintf(fp,"\t\t\t<textColor>%d</textColor>\n",v.textColor);
	fprintf(fp,"\t\t\t<frameThickness>%d</frameThickness>\n",v.frameThickness);
	fprintf(fp,"\t\t\t<frameColor>%d</frameColor>\n",v.frameColor);
	fprintf(fp,"\t\t\t<frameStyle>%d</frameStyle>\n",v.frameStyle);
	fprintf(fp,"\t\t\t<patternForeColor>%d</patternForeColor>\n",v.patternForeColor);
	fprintf(fp,"\t\t\t<patternBackColor>%d</patternBackColor>\n",v.patternBackColor);
	fprintf(fp,"\t\t\t<dotSize>%d</dotSize>\n",v.dotSize);
	fprintf(fp,"\t\t\t<stepSize>%d</stepSize>\n",v.stepSize);
	fprintf(fp,"\t\t\t<text>%s</text>\n",gStringToXML(XMLNode,buf,gLanguageText(self,ENGLISH)));
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	for (loop=0;loop<MAX_LANGUAGES;loop++)
		fprintf(fp,"\t\t\t<%s>%s</%s>\n",languages[loop],gStringToXML(XMLNode,buf,gLanguageText(self,loop)),languages[loop]); 

	fprintf(fp,"\t\t</rectangle>\n");


	return self;
}




cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_RECT_t	v;
	int             end,loop;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];
	double          controlScaleFactor;
	char			**languages=gLanguages(Application);
	
	memset(&v, 0, sizeof v);
	
	gPopulateStringFromNode(curnode,v.name,"name");
	v.xPos=gGetIntFromNode(curnode,"x");
	v.yPos=gGetIntFromNode(curnode,"y");
	v.width=gGetIntFromNode(curnode,"width");
	v.height=gGetIntFromNode(curnode,"height");
	v.hidden=gGetCharFromNode(curnode,"hidden");
	v.disabled=gGetCharFromNode(curnode,"disabled");
	v.fontSize=gGetIntFromNode(curnode,"fontsize");
	v.fontNameLen=gGetIntFromNode(curnode,"fontNameLen");
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	v.f3D=gGetIntFromNode(curnode,"f3D");
	v.textLength=gGetIntFromNode(curnode,"textLength");
	v.DT_Format=gGetIntFromNode(curnode,"DT_Format");
	v.textColor=gGetIntFromNode(curnode,"textColor");
	v.frameThickness=gGetIntFromNode(curnode,"frameThickness");
	v.frameColor=gGetIntFromNode(curnode,"frameColor");
	v.frameStyle=gGetIntFromNode(curnode,"frameStyle");
	v.patternForeColor=gGetIntFromNode(curnode,"patternForeColor");
	v.patternBackColor=gGetIntFromNode(curnode,"patternBackColor");
	v.dotSize=gGetIntFromNode(curnode,"dotSize");
	v.stepSize=gGetIntFromNode(curnode,"stepSize");



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

			if( ( (v.frameStyle & FRAME_Left) || (v.frameStyle & FRAME_Right) ) && 
			    ( (v.frameStyle & FRAME_Top)  || (v.frameStyle & FRAME_Bottom) ) ) 
				v.frameThickness = v.frameThickness*controlScaleFactor*min(dCxScale, dCyScale) + 0.5;

			else if ( (v.frameStyle & FRAME_Left) || (v.frameStyle & FRAME_Right) )
				v.frameThickness = v.frameThickness*controlScaleFactor*dCxScale + 0.5;

			else if ( (v.frameStyle & FRAME_Top) || (v.frameStyle & FRAME_Bottom) )
				v.frameThickness = v.frameThickness*controlScaleFactor*dCyScale + 0.5;
			
			v.dotSize = v.dotSize*controlScaleFactor*dCxScale + 0.5;
			if(v.dotSize<1)
				v.dotSize = 1;

			v.stepSize = v.stepSize*controlScaleFactor*dCxScale + 0.5;
			if(v.stepSize<1)
				v.stepSize = 1;

			// if the font information was not saved, adjust the fontsize. 
			hWnd = gHandle(parent);
			hDc = GetDC(hWnd);  // if hWnd=NULL, GetDC retrieves the device context for the entire screen
			logPixelsy = GetDeviceCaps(hDc, LOGPIXELSY);
			ReleaseDC(hWnd, hDc);
		
			if(v.fontSize <= 0)    // the font was not saved, default it to 10 
				v.fontSize = 10;

			if(logPixelsy>0)       // fit the font into the control
				v.fontSize = min(abs(MulDiv(v.height, 72, logPixelsy)), v.fontSize);
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddRectControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);
	
	for (loop=0;loop<MAX_LANGUAGES;loop++)
	{
		gPopulateStringFromNode(curnode,buf,languages[loop]);
		gSetLanguageText(ctl,loop,buf);
	}
	
	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	// the text 
	if (v.textLength) {
		p = v.textLength > BUFLEN ? malloc((unsigned)v.textLength) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"text");
		gSetStringValue(ctl, p);
		gSetLanguageText(ctl, ENGLISH, p);
		if (v.textLength > BUFLEN)
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

	// the font:
	// when scaling is processed, the v.fontSize may be too small and not available for the chosen font,
	// although system would get the closest match, but the font height would not match the control height.
	// In this case, Arial font will be chosen. 
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
	
	gSetControlParameters(ctl, &v);

	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}

imeth	gResetText()
{
	RECT	rect;
	HWND	hwnd;
		
	gSetStringValue(self,gLanguageText(self,gCurrentLanguage(self)));
	
	//this wasn't redrawing the text, so I'll add this here
	//to avoid changing how it works most of the time
	RedrawWindow(iHCtl,NULL,NULL,RDW_INVALIDATE|RDW_ERASE|RDW_ERASENOW);
	RedrawWindow(iHCtl,NULL,NULL,RDW_INVALIDATE|RDW_UPDATENOW);

		
	return self;
}




