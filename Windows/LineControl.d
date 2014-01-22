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

defclass  LineControl : Control  {
	HWND     iHCtl;             // handle to control	  	
	UINT     iCtlID;            // control ID			
	iDlg;                       // dialog object		
	int      iWindowControl;    // 1=window control	

	int      (*iDCFun)();       // function - double click	

	int      iExitType;         // 0=don't exit dialog or IDCANCEL or IDOK  

	char     iLineColor;        // the line color
	char     iLineStyle;        // LS_Solid | LS_Dot1 | LS_Dot2 | LS_Dot3 | LS_Dot4 | LS_Dot5 | LS_Dot6
	
	//  help info associated with a control   
	iTopic;	                    //  help topic associated with control 
	char	*iPrevTopic;        //  previous topic	
};


#include <ctype.h>

#include "color.h"

static long drawSolidLine(HDC hdc, short nColor, const RECT *pRect, BOOL bNoPrinting);

static long drawDotLine(HDC hdc, short nColor, const RECT *pRect, short nLineStyle);
static long drawDotLineToPrinter(HDC hdc, double dScaleX, short nColor, const RECT *pRect, short nLineStyle);
static long drawDotLineToMetafile(HDC hdc, HDC hdcView, short nColor, const RECT *pRect, short nLineStyle);

static long drawDashLine(HDC hdc, short nColor, const RECT *pRect, short dashRatio, short nLineStyle, BOOL bNoPrinting);
static void correctRect(RECT *pRect);
static long paintOtherControls(object self);


private	imeth	long	process_wm_paint(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	PAINTSTRUCT	ps;
	RECT        rect;
	gGetWindowRect(self, &rect);

	BeginPaint(hwnd, &ps);
	switch (iLineStyle) {
		case LS_Solid:
			drawSolidLine(ps.hdc, iLineColor, &rect, TRUE);
			break;
		case LS_Dash1:
		case LS_Dash2:
		case LS_Dash3:
		case LS_Dash4:
		case LS_Dash5:
			drawDashLine(ps.hdc, iLineColor, &rect, 8, iLineStyle, TRUE);
			break;
		default:  // dots
			drawDotLine(ps.hdc, iLineColor, &rect, iLineStyle);
	}
	EndPaint(hwnd, &ps);

	return 0L;
}


imeth gSaveControlToMetaFile(HDC hdcMeta, HDC hdcView)
{
	RECT  rect;
	int   xOffset, yOffset;
	if(!hdcMeta || !hdcView)
		return NULL;

	gGetScrollPosition(gGetParent(self), &yOffset, &xOffset);
	gGetWindowRect(self, &rect);
	rect.left   += xOffset;
	rect.right  += xOffset;
	rect.top    += yOffset;
	rect.bottom += yOffset;
	LPtoDP(hdcView, (LPPOINT)&rect, 2);

	switch (iLineStyle) {
		case LS_Solid:
			drawSolidLine(hdcMeta, iLineColor, &rect, FALSE);
			break;
		case LS_Dash1:
		case LS_Dash2:
		case LS_Dash3:
		case LS_Dash4:
		case LS_Dash5:
			drawDashLine(hdcMeta, iLineColor, &rect, 8, iLineStyle, FALSE);
			break;
		default:  // dots
			drawDotLineToMetafile(hdcMeta, hdcView, iLineColor, &rect, iLineStyle);
	}
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
		if( obj != self) {
			gGetWindowRect(obj, &rectCtl);
			IntersectRect(&rectIntersect, &rectCtl, &rect);
			if( !IsRectEmpty(&rectIntersect) ) {
				if(ClassOf(obj)!=RectControl && ClassOf(obj)!=LineControl || 
				  (ClassOf(obj)==LineControl && nOrderObj>nOrderSelf) ) {
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


///////////////////////////////////////////////////////////////////////////////////////////////
// drawSolidLine:
//
// nColor: CLR_Array[nColor] is the color for the solid line
// pRect:  a point to the rectangle of the line
//
///////////////////////////////////////////////////////////////////////////////////////////////
static long drawSolidLine(HDC hdc, short nColor, const RECT *pRect, BOOL bNoPrinting)
{
	HBRUSH hBrush, oldHbrush;
	RECT   rect;

	if(!hdc || !pRect) 
		return -1L;

	CopyRect(&rect, pRect);
	if(bNoPrinting)
		OffsetRect(&rect, -rect.left, -rect.top);

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	hBrush = CreateSolidBrush(CLR_Array[nColor]);
	if(!hBrush)
		return -1L;

	oldHbrush = SelectObject(hdc, hBrush);

	PatBlt(hdc, rect.left, rect.top, (rect.right-rect.left), (rect.bottom-rect.top), PATCOPY);

	SelectObject(hdc, oldHbrush);
	DeleteObject((HGDIOBJ) hBrush);
	return 0L;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////
// drawDotLineToMetafile:
//
// nColor: CLR_Array[nColor] is the color for the solid line
// pRect:  a point to the rectangle of the line
//
//////////////////////////////////////////////////////////////////////////////////////////////////////
static long drawDotLineToMetafile(HDC hdc, HDC hdcView, short nColor, const RECT *pRect, short nLineStyle)
{
	HPEN    hpen, oldHpen;
	HBRUSH  hBrush, oldHbrush;
	int     left, top, right, bottom;
	int     nDiameter, nDashLength, nStep, x, y;
	RECT    rect;

	if(!hdc || !hdcView || !pRect) 
		return -1L;

	if(nLineStyle<1 || nLineStyle>LS_Separator)
		nLineStyle = LS_Dot4;

	CopyRect(&rect, pRect);

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	nDiameter = min( x=abs(rect.right-rect.left), y=abs(rect.bottom-rect.top) );
	if(nDiameter<1)
		nDiameter=1;

	nDashLength = 3*300/GetDeviceCaps(hdcView, LOGPIXELSX);
	nStep = 6*300/GetDeviceCaps(hdcView, LOGPIXELSX) + 0.5;

	hBrush = CreateSolidBrush(CLR_Array[nColor]);
	if(!hBrush)
		return -1L;
	oldHbrush = SelectObject(hdc, hBrush);

	if(nDiameter<6) {  // less than two pixels in screen unit
		left = rect.left;
		top  = rect.top;

		if(x>=y)  { // horizontal orientation
			if(rect.bottom < rect.top)
				nDiameter = -abs(nDiameter);
			do {
				nDashLength = min(nDashLength, rect.right-left);
				PatBlt(hdc, left, top, nDashLength, nDiameter, PATCOPY);
				left += nStep;
			} while(left<rect.right);
		}
		else  { // vertical orientation
			if(rect.bottom >= rect.top) {  
				do {
					nDashLength = min(nDashLength, rect.bottom-top);
					PatBlt(hdc, left, top, nDiameter, nDashLength, PATCOPY);
					top += nStep;
				} while(top<rect.bottom);
			}
			else {  
				nStep = -abs(nStep);
				do {
					nDashLength = -min(abs(nDashLength), abs(rect.bottom-top));
					PatBlt(hdc, left, top, nDiameter, nDashLength, PATCOPY);
					top += nStep;
				} while(top>rect.bottom);
			}
		}
	}
	else {
		hpen = CreatePen(PS_SOLID, 1, CLR_Array[nColor]);
		if(!hpen) {
			SelectObject(hdc, oldHbrush);
			DeleteObject((HGDIOBJ) hBrush);
			return -1L;
		}

		oldHpen = SelectObject(hdc, hpen);

		switch(nLineStyle) {
			case LS_Dot2:
				nStep = nDiameter * 2;
				break;
			case LS_Dot3:
				nStep = nDiameter * 2.5;
				break;
			case LS_Dot4:
				nStep = nDiameter * 3;
				break;
			case LS_Dot5:
				nStep = nDiameter * 3.5;
				break;
			case LS_Dot6:
				nStep = nDiameter * 4;
				break;
			default:
				nStep = nDiameter * 1.5;
				break;
		}

		left = rect.left;
		top = rect.top;
		right = rect.left+nDiameter;

		if(x>=y)  { // horizontal orientation
			bottom = rect.top+nDiameter;
			while(right<=rect.right) {
				Ellipse(hdc, left, top, right, bottom);
				left += nStep;
				right += nStep;
			} 
		}
		else  { // vertical orientation
			if(rect.bottom>=rect.top) {  
				bottom = rect.top+nDiameter;
				while(bottom<=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top += nStep;
					bottom += nStep;
				}
			}
			else { 
				bottom = rect.top-nDiameter;
				while(bottom>=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top -= nStep;
					bottom -= nStep;
				}
			}
		}

		SelectObject(hdc, oldHpen);
		DeleteObject((HGDIOBJ) hpen);
	}


	SelectObject(hdc, oldHbrush);
	DeleteObject((HGDIOBJ) hBrush);

	return 0L;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////
// drawDotLineToMetafile:
//
// nColor: CLR_Array[nColor] is the color for the solid line
// pRect:  a point to the rectangle of the line
//
//////////////////////////////////////////////////////////////////////////////////////////////////////
static long drawDotLineToPrinter(HDC hdc, double dScaleX, short nColor, const RECT *pRect, short nLineStyle)
{
	HPEN    hpen, oldHpen;
	HBRUSH  hBrush, oldHbrush;
	int     left, top, right, bottom;
	int     nDiameter, nDashLength, nStep, x, y;
	RECT    rect;

	if(!hdc || !pRect) 
		return -1L;

	if(nLineStyle<1 || nLineStyle>LS_Separator)
		nLineStyle = LS_Dot4;

	CopyRect(&rect, pRect);

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	nDiameter = min( x=abs(rect.right-rect.left), y=abs(rect.bottom-rect.top) );
	if(nDiameter<1)
		nDiameter=1;

	nDashLength = 3*dScaleX*600/GetDeviceCaps(hdc, LOGPIXELSX);
	nStep = 6*dScaleX*600/GetDeviceCaps(hdc, LOGPIXELSX) + 0.5;

	hBrush = CreateSolidBrush(CLR_Array[nColor]);
	if(!hBrush)
		return -1L;
	oldHbrush = SelectObject(hdc, hBrush);

	if(nDiameter< (int)(2*dScaleX)) {  // less than two pixels in screen unit
		left = rect.left;
		top  = rect.top;

		if(x>=y)  { // horizontal orientation
			if(rect.bottom < rect.top)
				nDiameter = -abs(nDiameter);
			do {
				nDashLength = min(nDashLength, rect.right-left);
				PatBlt(hdc, left, top, nDashLength, nDiameter, PATCOPY);
				left += nStep;
			} while(left<rect.right);
		}
		else  { // vertical orientation
			if(rect.bottom >= rect.top) {  
				do {
					nDashLength = min(nDashLength, rect.bottom-top);
					PatBlt(hdc, left, top, nDiameter, nDashLength, PATCOPY);
					top += nStep;
				} while(top<rect.bottom);
			}
			else {  
				nStep = -abs(nStep);
				do {
					nDashLength = -min(abs(nDashLength), abs(rect.bottom-top));
					PatBlt(hdc, left, top, nDiameter, nDashLength, PATCOPY);
					top += nStep;
				} while(top>rect.bottom);
			}
		}
	}
	else {
		hpen = CreatePen(PS_SOLID, 1, CLR_Array[nColor]);
		if(!hpen) {
			SelectObject(hdc, oldHbrush);
			DeleteObject((HGDIOBJ) hBrush);
			return -1L;
		}

		oldHpen = SelectObject(hdc, hpen);

		switch(nLineStyle) {
			case LS_Dot2:
				nStep = nDiameter * 2;
				break;
			case LS_Dot3:
				nStep = nDiameter * 2.5;
				break;
			case LS_Dot4:
				nStep = nDiameter * 3;
				break;
			case LS_Dot5:
				nStep = nDiameter * 3.5;
				break;
			case LS_Dot6:
				nStep = nDiameter * 4;
				break;
			default:
				nStep = nDiameter * 1.5;
				break;
		}

		left = rect.left;
		top = rect.top;
		right = rect.left+nDiameter;

		if(x>=y)  { // horizontal orientation
			bottom = rect.top+nDiameter;
			while(right<=rect.right) {
				Ellipse(hdc, left, top, right, bottom);
				left += nStep;
				right += nStep;
			} 
		}
		else  { // vertical orientation
			if(rect.bottom>=rect.top) {  
				bottom = rect.top+nDiameter;
				while(bottom<=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top += nStep;
					bottom += nStep;
				}
			}
			else { 
				bottom = rect.top-nDiameter;
				while(bottom>=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top -= nStep;
					bottom -= nStep;
				}
			}
		}

		SelectObject(hdc, oldHpen);
		DeleteObject((HGDIOBJ) hpen);
	}


	SelectObject(hdc, oldHbrush);
	DeleteObject((HGDIOBJ) hBrush);

	return 0L;
}



//////////////////////////////////////////////////////////////////////////////////////////////////////
// drawDotLine:
//
// nColor: CLR_Array[nColor] is the color for the solid line
// pRect:  a point to the rectangle of the line
//
//////////////////////////////////////////////////////////////////////////////////////////////////////
static long drawDotLine(HDC hdc, short nColor, const RECT *pRect, short nLineStyle)
{
	HPEN   hpen, oldHpen;
	RECT   rect;
	int    nDiameter, x, y;

	if(!hdc || !pRect) 
		return -1L;

	if(nLineStyle<1 || nLineStyle>LS_Separator)
		nLineStyle = LS_Dot4;

	CopyRect(&rect, pRect);
	correctRect(&rect);
	OffsetRect(&rect, -rect.left, -rect.top);

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	nDiameter = min( x=abs(rect.right-rect.left), y=abs(rect.bottom-rect.top) );
	if(nDiameter<1)
		nDiameter=1;

	if(nDiameter<2) {
		int    bkMode;
		bkMode = SetBkMode(hdc, TRANSPARENT);
		hpen = CreatePen(PS_DOT, 1, CLR_Array[nColor]);
		if(!hpen)
			return -1L;
		oldHpen = SelectObject(hdc, hpen);
		MoveToEx(hdc, rect.left, rect.top, NULL);
		if(x>y)
			LineTo(hdc, rect.right, rect.top);
		else
			LineTo(hdc, rect.left, rect.bottom);

		SetBkMode(hdc, bkMode);
		SelectObject(hdc, oldHpen);
		DeleteObject((HGDIOBJ) hpen);
	}
	else {
		HBRUSH hBrush, oldHbrush;
		int    left, top, right, bottom;
		int    nStep;
		hBrush = CreateSolidBrush(CLR_Array[nColor]);
		if(!hBrush)
			return -1L;
		oldHbrush = SelectObject(hdc, hBrush);

		hpen = CreatePen(PS_SOLID, 1, CLR_Array[nColor]);
		if(!hpen) {
			SelectObject(hdc, oldHbrush);
			DeleteObject((HGDIOBJ) hBrush);
			return -1L;
		}

		oldHpen = SelectObject(hdc, hpen);

		switch(nLineStyle) {
			case LS_Dot2:
				nStep = nDiameter * 2;
				break;
			case LS_Dot3:
				nStep = nDiameter * 2.5;
				break;
			case LS_Dot4:
				nStep = nDiameter * 3;
				break;
			case LS_Dot5:
				nStep = nDiameter * 3.5;
				break;
			case LS_Dot6:
				nStep = nDiameter * 4;
				break;
			default:
				nStep = nDiameter * 1.5;
				break;
		}

		left = rect.left;
		top = rect.top;
		right = rect.left+nDiameter;

		if(x>=y)  { // horizontal orientation
			bottom = rect.top+nDiameter;
			while(right<=rect.right) {
				Ellipse(hdc, left, top, right, bottom);
				left += nStep;
				right += nStep;
			} 
		}
		else  { // vertical orientation
			if(rect.bottom>=rect.top) {  
				bottom = rect.top+nDiameter;
				while(bottom<=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top += nStep;
					bottom += nStep;
				}
			}
			else { 
				bottom = rect.top-nDiameter;
				while(bottom>=rect.bottom) {
					Ellipse(hdc, left, top, right, bottom);
					top -= nStep;
					bottom -= nStep;
				}
			}
		}
		SelectObject(hdc, oldHbrush);
		DeleteObject((HGDIOBJ) hBrush);

		SelectObject(hdc, oldHpen);
		DeleteObject((HGDIOBJ) hpen);
	}

	return 0L;
}



///////////////////////////////////////////////////////////////////////////////////////
// drawDashLine:
//
// nColor: CLR_Array[nColor] is the color for the solid line
// pRect:  a point to the rectangle of the line
//
///////////////////////////////////////////////////////////////////////////////////////
static long drawDashLine(HDC hdc, short nColor, const RECT *pRect, short dashRatio, short nLineStyle, BOOL bNoPrinting)
{
	RECT   rect;
	HBRUSH hBrush, oldHbrush;
	int    nSide, nDashLength, nStep, x, y, left, top, right, bottom;

	if(!hdc || !pRect) 
		return -1L;

	if(nLineStyle<=LS_Separator || nLineStyle>LS_Max)
		nLineStyle = LS_Dash2;

	CopyRect(&rect, pRect);
	if(bNoPrinting) {
		correctRect(&rect);
		OffsetRect(&rect, -rect.left, -rect.top);
	}

	if (nColor<0 || nColor>CLR_Max)
		nColor = CLR_Black;

	nSide = min( x=abs(rect.right-rect.left), y=abs(rect.bottom-rect.top) );
	if(nSide<1)
		nSide = 1;

	nDashLength = nSide*dashRatio;

	hBrush = CreateSolidBrush(CLR_Array[nColor]);
	if(!hBrush)
		return -1L;
	oldHbrush = SelectObject(hdc, hBrush);

	switch(nLineStyle) {
		case LS_Dash1:
			nStep = nDashLength * 1.25;
			break;
		case LS_Dash2:
			nStep = nDashLength * 1.333333;
			break;
		case LS_Dash4:
			nStep = nDashLength * 1.75;
			break;
		case LS_Dash5:
			nStep = nDashLength * 2;
			break;
		default:
			nStep = nDashLength * 1.5;
			break;
	}

	left = rect.left;
	top = rect.top;

	if(x>=y)  { // horizontal orientation
		if(rect.bottom < rect.top)
			nSide = -abs(nSide);
		do {
			nDashLength = min(nDashLength, rect.right-left);
			PatBlt(hdc, left, top, nDashLength, nSide, PATCOPY);
			left += nStep;
		} while(left<rect.right);
	}
	else  { // vertical orientation
		if(rect.bottom >= rect.top) {  
			do {
				nDashLength = min(nDashLength, rect.bottom-top);
				PatBlt(hdc, left, top, nSide, nDashLength, PATCOPY);
				top += nStep;
			} while(top<rect.bottom);
		}
		else {  
			nStep = -abs(nStep);
			do {
				nDashLength = -min(abs(nDashLength), abs(rect.bottom-top));
				PatBlt(hdc, left, top, nSide, nDashLength, PATCOPY);
				top += nStep;
			} while(top>rect.bottom);
		}
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
			gPerformJavaObjCallback((object)iDCFun, iDlg);
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



cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);

	iDlg = dlg;
	iCtlID = ctlID;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

	gSetStyle(obj, WS_VISIBLE);

	iLineColor = CLR_Black;    
	iLineStyle = LS_Solid;     

	//  Init message handlers 
	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_PAINT, process_wm_paint);

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
	
	iLineColor = CLR_Black;    
	iLineStyle = LS_Solid;     

	// Init message handlers  
	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	gAddHandlerAfter(obj, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(obj, (unsigned) WM_CHAR, 0);  // no auto default processing

	gAddHandlerAfter(obj, (unsigned) WM_LBUTTONDBLCLK, process_wm_lbuttondblclk);
	gDefaultProcessingMode(obj, (unsigned) WM_LBUTTONDBLCLK, 0);  // no auto default processing

	gAddHandlerAfter(obj, (unsigned) WM_PAINT, process_wm_paint);
	
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
	if (iHCtl) 
		return 0;

	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

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
	CTLTYPE_LINE_t	*v = vp;
	int	height, width, xPos, yPos, len;
	int	sm = gSetScalingMode(Application, SM_PIXELS);

	if(!v)
		return NULL;

	strncpy(v->name, gName(self), (sizeof(v->name)-1));

	// size and position of the line
	gGetSize(self, &height, &width);
	gGetVirtualPosition(self, &yPos, &xPos);

	v->height = height;
	v->width  = width;
	v->xPos   = xPos;
	v->yPos   = yPos;

	// the status of the line
	v->hidden = gHiddenStatus(self) == 1 ? 'Y' : 'N';
	v->disabled = gDisableStatus(self) == 1 ? 'Y' : 'N';

	// the attributes of the line
	v->lineColor = iLineColor;
	v->lineStyle = iLineStyle;  

	if (iTopic  &&  (len=gSize(iTopic)))
		v->helpTopicLen = len + 1;
	else
		v->helpTopicLen = 0;

	gSetScalingMode(Application, sm);
	return self;
}



imeth	gSetControlParameters(void *vp)
{
	CTLTYPE_LINE_t	*v = vp;
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

	// the attributes of the line
	gSetLineColor(self, v->lineColor);
	gSetLineStyle(self, v->lineStyle);

	gBackBrush(self, vNew(StockBrush, NULL_BRUSH) );

	gSetScalingMode(Application, sm);
	return self;
}



imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_LINE_t  v;
	short           type = CTLTYPE_LINE, size = sizeof(v);
	
	if(!fp)
		return NULL;

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.helpTopicLen  &&  1 != fwrite(gStringValue(iTopic), (int) v.helpTopicLen, 1, fp))
		return NULL;

	return self;
}


#define	BUFLEN	128

cmeth	gLoadControl(FILE *fp, parent)
{
	CTLTYPE_LINE_t	v;
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
			double dCxScale, dCyScale;
			RECT   MFMarginRect;

			gGetMFCxCyScale(parent, &dCxScale, &dCyScale);
			gGetMFMargins(parent, &MFMarginRect);

			// to reduce the accumulated error, a special trick is used 
			v.xPos  = (v.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale + 0.5;
			v.yPos  = (v.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale + 0.5;			 

			v.width  = v.width*controlScaleFactor*dCxScale  + 0.5;
			v.height = v.height*controlScaleFactor*dCyScale + 0.5;

			if(v.width<1)
				v.width = 1;
			if(v.height<1)
				v.height = 1;
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddLineControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}



cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_LINE_t	v;
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
	ctl = gAddLineControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}



imeth object gSetLineColor(char lineColor)
{
	if (lineColor>=0 && lineColor<=CLR_Max)
		iLineColor = lineColor;
	else
		iLineColor = CLR_Black;

	return self;
}



imeth object gSetLineStyle(char lineStyle)
{
	if(lineStyle>=0 && lineStyle<=LS_Max)
		iLineStyle = lineStyle;
	else
		iLineStyle = LS_Solid;

	return self;
}



///////////////////////////////////////////////////////////////////////////////
// correctRect
//
// This routine corrects a rectangle so that its right>left and bottom>top.
//
///////////////////////////////////////////////////////////////////////////////
static void correctRect(RECT *pRect)
{
	RECT rect;
	SetRect(&rect, pRect->left, pRect->top, pRect->right, pRect->bottom);

	if( (rect.right > rect.left) && (rect.bottom < rect.top) ) 
		SetRect(pRect, rect.left, rect.bottom, rect.right, rect.top);
	else if( (rect.right < rect.left) && (rect.bottom > rect.top) ) 
		SetRect(pRect, rect.right, rect.top, rect.left, rect.bottom);
	else if( (rect.right < rect.left) && (rect.bottom < rect.top) ) 
		SetRect(pRect, rect.right, rect.bottom, rect.left, rect.top);
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

	object  parentObj;
	int     nPhysicalWidth, nPhysicalHeight, nPrinterOffsetX, nPrinterOffsetY;

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

	gCLDGetPhysicalParameters(printerObj, &nPhysicalWidth, &nPhysicalHeight, &nPrinterOffsetX, &nPrinterOffsetY);

	// get the rect of the control 
	SetRectEmpty(&rect);
	gGetWindowRect(self, &rect);

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
		}
		else
			return NULL;

		if( (dblTmp=controlScale*MFCyScale) > 0 ) {
			rect.top    = ( (rect.top    + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY;
			rect.bottom = ( (rect.bottom + nViewOffsetY - 0.5)/dblTmp - MFMarginRect.top )*dScaleY - nPrinterOffsetY;
		}
		else
			return NULL;
	}
	
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

	switch (iLineStyle) {
		case LS_Solid:
			drawSolidLine(hdcPrinter, iLineColor, &rect, FALSE);
			break;
		case LS_Dash1:
		case LS_Dash2:
		case LS_Dash3:
		case LS_Dash4:
		case LS_Dash5:
			drawDashLine(hdcPrinter, iLineColor, &rect, 8, iLineStyle, FALSE);
			break;
		default:  // dots
			drawDotLineToPrinter(hdcPrinter, dScaleX, iLineColor, &rect, iLineStyle);
	}

	return self;
}


/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the line control of the cld file for printing
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_LINE_t    v;
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

	ctl = gCLDNewWindowControl(LineControl, v.name, parentObj);
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

	iLineColor = CLR_Black;    
	iLineStyle = LS_Solid;     

	return obj;
}

imeth gWriteXML(FILE *fp)
{

	CTLTYPE_LINE_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);

	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<line>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<lineColor>%d</lineColor>\n",v.lineColor);
	fprintf(fp,"\t\t\t<lineStyle>%d</lineStyle>\n",v.lineStyle);
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	
	fprintf(fp,"\t\t</line>\n");


	return self;
}



cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_LINE_t	v;
	int             end;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];
	double          controlScaleFactor;
	
	memset(&v, 0, sizeof v);
	
	gPopulateStringFromNode(curnode,v.name,"name");
	v.xPos=gGetIntFromNode(curnode,"x");
	v.yPos=gGetIntFromNode(curnode,"y");
	v.width=gGetIntFromNode(curnode,"width");
	v.height=gGetIntFromNode(curnode,"height");
	v.hidden=gGetCharFromNode(curnode,"hidden");
	v.disabled=gGetCharFromNode(curnode,"disabled");
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	v.lineColor=gGetIntFromNode(curnode,"lineColor");
	v.lineStyle=gGetIntFromNode(curnode,"lineStyle");

	// get the screen resolution in the CLD file which is loaded in window.d
	// and the current screen resolution and scale the controls in the cld file
	// so that the cld file will be displayed with the same look
	
	if(gGetScaleFlg(parent)) {    // do the scaling if the scaling flag is set
		controlScaleFactor = gGetControlScale(parent);
		if(controlScaleFactor>0) {
			double dCxScale, dCyScale;
			RECT   MFMarginRect;

			gGetMFCxCyScale(parent, &dCxScale, &dCyScale);
			gGetMFMargins(parent, &MFMarginRect);

			// to reduce the accumulated error, a special trick is used 
			v.xPos  = (v.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale + 0.5;
			v.yPos  = (v.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale + 0.5;			 

			v.width  = v.width*controlScaleFactor*dCxScale  + 0.5;
			v.height = v.height*controlScaleFactor*dCyScale + 0.5;

			if(v.width<1)
				v.width = 1;
			if(v.height<1)
				v.height = 1;
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddLineControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}




