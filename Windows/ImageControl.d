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

#include "logfile.h"
#include "hdlcache.h"
#include "ctlsave.h"

#include <ctype.h>

#include <windows.h>
#include <stdlib.h>


#ifndef	INT
#define	INT	int
#endif

#define PALVERSION      0x300
#define MAXPALETTE      256       /* max. # supported palette entries */

/* macro to determine if resource is a DIB */
#define ISDIB(bft) ((bft) == BFT_BITMAP)

/* Macro to determine to round off the given value to the closest byte */
#define WIDTHBYTES(i)   ((i+31)/32*4)
#define MAXREAD  32768                 /* Number of bytes to be read during */
                                       /* each read operation.              */
#define BFT_BITMAP 0x4d42   /* 'BM' */

#define	StartWait()
#define	EndWait()


// the class can be used to load other image files, such as gif and jpeg files with minor changes.
// therefore the class is named ImageControl.

defclass  ImageControl : Control  {
	HWND        iHCtl;              // handle to control	  	
	UINT        iCtlID;             // control ID
	iDlg;                           // dialog object		
	int         iWindowControl;     // 1=window control	

	int         (*iDCFun)();        // function - double click	

	int         iExitType;          // 0=don't exit dialog or IDCANCEL or IDOK  

	iValue;                         // the text object whose string value is the image file name

	//  help info associated with a control   
	iTopic;	                        // help topic associated with control 
	char	   *iPrevTopic;         // previous topic
	
	// BITMAPINFO *iBitmapInfo;	    // a pointer to the BITMAPINFO struct, the bitmap file name is stored 
	                                // as a string in iValue

	int	    iButtonBorder;      // 0 = no border, -1 = button down border, 1 = button up border
	UINT	    iBitmapID;          // Resource id of a bitmap
	BOOL        iBLegitDraw;        // We have a valid bitmap to draw 
	HPALETTE    iHpalCurrent;       // Handle to current palette
	HANDLE      iHdibCurrent;       // Handle to current memory DIB 
	HBITMAP     iHbmCurrent;        // Handle to current memory BITMAP
	HANDLE      iHbiCurrent;        // Handle to current bitmap info struct

	char        iNotSaveToMetaFile; // 0: save it to meta file _.pmf (default value)
	                                // 1: save it to cld file _.cld (not in _.pmf)
};


#include "color.h"


static long         drawFrameRect(HDC hdc, const RECT *pRect);
static long         fillRectWithDefaultPicture(HDC hdc, const RECT *pRect);
static	BOOL DibInfo (HANDLE hbi, LPBITMAPINFOHEADER lpbi);
private	imeth	int	pInitDIBFromResource(object self);
static	BOOL StretchDibBlt (
    HDC hdc,
    INT x,
    INT y,
    INT dx,
    INT dy,
    HANDLE hdib,
    INT x0,
    INT y0,
    INT dx0,
    INT dy0,
    LONG rop);
private imeth 	int	pInitDIB(char *achFileName);

#if 0
static BITMAPINFO * LoadPackedDib (char *szFileName);
static int          GetPackedDibWidth (BITMAPINFO * pPackedDib);
static int          GetPackedDibHeight (BITMAPINFO * pPackedDib);
static int          GetPackedDibBitCount (BITMAPINFO * pPackedDib);
static int          GetPackedDibRowLength (BITMAPINFO * pPackedDib);
static int          GetPackedDibInfoHeaderSize (BITMAPINFO * pPackedDib);
static int          GetPackedDibColorsUsed (BITMAPINFO * pPackedDib);
static int          GetPackedDibNumColors (BITMAPINFO * pPackedDib);
static int          GetPackedDibColorTableSize (BITMAPINFO * pPackedDib);
static RGBQUAD    * GetPackedDibColorTablePtr (BITMAPINFO * pPackedDib);
static RGBQUAD    * GetPackedDibColorTableEntry (BITMAPINFO * pPackedDib, int i);
static BYTE       * GetPackedDibBitsPtr (BITMAPINFO * pPackedDib);
static int          GetPackedDibBitsSize (BITMAPINFO * pPackedDib);
static HPALETTE     CreatePackedDibPalette (BITMAPINFO * pPackedDib);

private imeth long gFillRectWithBitmap(object self, HDC hdc, const RECT *pRect)
{
	if(!hdc || !pRect || !iBitmapInfo)
		return -1L;

	SetStretchBltMode (hdc, COLORONCOLOR) ;

	StretchDIBits (hdc, 
				pRect->left, pRect->top, 
				(pRect->right-pRect->left), 
				(pRect->bottom-pRect->top),
				0, 0, 
				GetPackedDibWidth (iBitmapInfo),
				GetPackedDibHeight (iBitmapInfo),
				GetPackedDibBitsPtr (iBitmapInfo),
				iBitmapInfo,
				DIB_RGB_COLORS, SRCCOPY) ;
	return 0L;
}
#endif

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PrintDIB(HDC hDC, int xPos, int yPos, int width, int height) *
 *                                                                          *
 *  PURPOSE    :  Set the DIB bits to the printer DC.                       *
 *                                                                          *
 ****************************************************************************/

private imeth long gFillRectWithBitmap16(object self, HDC hDC, const RECT *pRect)
{
	BITMAPINFOHEADER bi;
	INT dibX,  dibY;
	INT dibDX, dibDY;

    INT xPos = pRect->left;
    INT yPos = pRect->top;
    INT width = pRect->right - pRect->left;
    INT height = pRect->bottom - pRect->top;

	if (!iBLegitDraw)
		return -1L;

	DibInfo(iHbiCurrent, &bi);

	dibX  = 0;
	dibY  = 0;
	dibDX = (INT)bi.biWidth;
	dibDY = (INT)bi.biHeight;

	if (iHdibCurrent){
		/* Stretch the DIB to printer DC */
		StretchDibBlt ( hDC,
				xPos,
				yPos,
				width,
				height,
				iHdibCurrent,
				dibX,
				dibY,
				dibDX,
				dibDY,
				SRCCOPY);

		if (iButtonBorder) {
			DWORD	o1 = iButtonBorder > 0 ? WHITENESS : BLACKNESS;
			DWORD	o2 = iButtonBorder > 0 ? BLACKNESS : WHITENESS;
		
			//  top line
			PatBlt(hDC, xPos, yPos, width, 1, o1);
			//  left line
			PatBlt(hDC, xPos, yPos, 1, height, o1);
			//  bottom line
			PatBlt(hDC, xPos, pRect->bottom - 1, width, 1, o2);
			//  right line
			PatBlt(hDC, pRect->right - 1, yPos, 1, height, o2);
		}
	}

	return 0L;
}


private	imeth	long	process_wm_paint(object	self, 
					HWND	hwnd, 
					UINT	mMsg, 
					WPARAM	wParam, 
					LPARAM	lParam)
{
	PAINTSTRUCT	 ps;
	RECT         rect;

	gGetWindowRect(self, &rect);
	OffsetRect(&rect, -rect.left, -rect.top);

	BeginPaint(hwnd, &ps);

	SetBkMode(ps.hdc, OPAQUE);
	if( (iValue || iBitmapID) && gHiddenStatus(self)!=1 )
		// gFillRectWithBitmap(self, ps.hdc, &rect);
		gFillRectWithBitmap16(self, ps.hdc, &rect);
	else
		fillRectWithDefaultPicture(ps.hdc, &rect);
	
	if (iValue) {
		int len=gSize(iValue);
		if ( len>4  && !stricmp(gStringValue(iValue)+len-4, ".swf") ) {
			fillRectWithDefaultPicture(ps.hdc, &rect);
		}	
	}

	EndPaint(hwnd, &ps);
	return 0L;
}



///////////////////////////////////////////////////////////
// hdcMeta: a handle to a metafile 
// hdcView: a handle to a view DC of the parent window
//
imeth gSaveControlToMetaFile(HDC hdcMeta, HDC hdcView)
{
	RECT    rect;
	int     xOffset, yOffset;
	double  dFactorX, dFactorY;

	if(!hdcMeta || !hdcView || !iValue)
		return NULL;

	if(gHiddenStatus(self)==1)  
		return self;

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
	
	// gFillRectWithBitmap(self, hdcMeta, &rect);
	gFillRectWithBitmap16(self, hdcMeta, &rect);

	return self;
}


static long fillRectWithDefaultPicture(HDC hdc, const RECT *pRect)
{
	HBRUSH   hHatchBrush;
	int      oldBkMode;	
	COLORREF oldBkColor;
	RECT     rect;

	if(!hdc || !pRect)
		return -1L;

	CopyRect(&rect, pRect);

	hHatchBrush = CreateHatchBrush(HS_VERTICAL, RGB(255, 0, 0));

	if(!hHatchBrush) 
		return -1L;

	oldBkMode    = SetBkMode(hdc, OPAQUE);
	oldBkColor   = SetBkColor(hdc, RGB(0, 0, 255));

	FillRect(hdc, &rect, hHatchBrush);

	SetBkColor(hdc, oldBkColor);
	SetBkMode(hdc, oldBkMode);

	DeleteObject((HGDIOBJ) hHatchBrush);

	return 0L;
}


//////////////////////////////////////////////////////////
// drawFrameRect:
//
// pRect:  a point to the rectangle of the frame
//
//////////////////////////////////////////////////////////
static long drawFrameRect(HDC hdc, const RECT *pRect)
{
	HPEN  hpen, oldHpen;
	RECT  rect;

	if(!hdc || !pRect) 
		return -1L;

	CopyRect(&rect, pRect);

	hpen = CreatePen(PS_SOLID, 1, RGB(0,0,0));
	if(!hpen)
		return -1L;

	oldHpen = SelectObject(hdc, hpen);
	MoveToEx(hdc, rect.left, rect.top, NULL);
	LineTo(hdc, rect.left, rect.bottom-1);
	LineTo(hdc, rect.right-1, rect.bottom-1);

	MoveToEx(hdc, rect.left, rect.top, NULL);
	LineTo(hdc, rect.right-1, rect.top);
	LineTo(hdc, rect.right-1, rect.bottom-1);

	SelectObject(hdc, oldHpen);
	DeleteObject(hpen);

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
	iExitType = 0;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

	gSetStyle(obj, WS_VISIBLE);

	iValue = 0; 
	// iBitmapInfo = NULL;
	iNotSaveToMetaFile = 0;

	//  Init message handlers 
	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS,  process_wm_setfocus);
	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);
	gAddHandlerAfter(obj, (unsigned) WM_PAINT,     process_wm_paint);

	return obj;
}


cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "button", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iWindowControl = 1;
	iCtlID = ctlID;
	iExitType = 0;
	if (ctlID == IDCANCEL  ||  ctlID == IDOK)
		iExitType = ctlID;

	gSetStyle(obj, WS_VISIBLE);

	iValue = 0; 
	// iBitmapInfo = NULL;
	iNotSaveToMetaFile = 0;

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

	// if (iValue)
	// SetWindowText(iHCtl, (LPCSTR) gStringValue(iValue));
	// iValue is used as the name of the bitmap file, instead of the caption of the control

	return gInitialize(super, hDlg, dlg);
}


imeth char * gGetBitmapFileName()
{
	if(iValue)
		return gStringValue(iValue);
	return NULL;
}


imeth	int	gShow()
{
	if (iHCtl)
		return 0;
	
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

	// iValue is used as the name of the bitmap file, 
	// instead of the caption of the control

	gInitialize(super, (HWND)0, NULL);
	if (gIsKindOf(iDlg, Window)) {
		RECT	rect;
		gGetWindowRect(self, &rect);
		InvalidateRect(gHandle(iDlg), &rect, TRUE);
	}
	return 0;
}


/****************************************************************************
 *                                                                          *
 *  FUNCTION   : FreeDib(void)                                              *
 *                                                                          *
 *  PURPOSE    : Frees all currently active bitmap, DIB and palette objects *
 *               and initializes their handles.                             *
 *                                                                          *
 ****************************************************************************/
private imeth	void	gFreeDib()
{
	if (iHpalCurrent)
		DeleteObject(iHpalCurrent);

	if (iHbmCurrent)
		DeleteObject(iHbmCurrent);

	if (iHdibCurrent)
		GlobalFree(iHdibCurrent);

	if (iHbiCurrent && iHbiCurrent != iHdibCurrent)
		GlobalFree(iHbiCurrent);

	iBLegitDraw  = FALSE;
	iHpalCurrent = 0;
	iHdibCurrent = 0;
	iHbmCurrent  = 0;
	iHbiCurrent  = 0;
}


imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);

	if (IsObj((object) iDCFun))
		gDispose((object) iDCFun);

	if (iTopic)
		iTopic=gDispose(iTopic);

	if(iValue) 
		iValue=gDispose(iValue);

	// if(iBitmapInfo) {
	// 	free(iBitmapInfo);
	//	iBitmapInfo = NULL;
	// }

	gFreeDib(self);

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
	return gSetTextObj(self, val);
}


imeth	gSetStringValue(char *val)
{
	int len;

	// string already in iValue
	if(iValue && stricmp(gStringValue(iValue),val)==0)
		return self;

	iBitmapID = 0;
	
	// clear instance variable
	if(iValue) {
		gDispose(iValue);
		iValue = NULL;
	}
	// if(iBitmapInfo) {
	// 	free(iBitmapInfo);
	// 	iBitmapInfo = NULL;
	// }

	// empty string
	if(!val || !(*val)) 
		return NULL;
	
	// a new string
	// check the string val
	len = strlen(val);
	
	if ( len>4  && !stricmp(val+len-4, ".swf") ){
		iValue = gNewWithStr(String, val);
		return self;
	}
	
	if ( len<=4  || stricmp(val+len-4, ".bmp") )
		return NULL;
		

	// iBitmapInfo = LoadPackedDib (val);
	// if(iBitmapInfo) // correct bitmap file
	iValue = gNewWithStr(String, val);

	pInitDIB(self, val);

	return self;
}


imeth	gSetBitmapID(int id)
{
	if (!id)
		return NULL;
	
	if (iBitmapID == id)
		return self;

	iBitmapID = id;
	
	if(iValue)
		iValue = gDispose(iValue);
	
	pInitDIB(self, NULL);

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
	return iValue ? gStringValue(iValue) : NULL;
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


imeth	gGetControlParameters(void *vp)
{
	CTLTYPE_IMAGE_t  *v = vp;
	int	 height, width, xPos, yPos, len;
	int	 sm = gSetScalingMode(Application, SM_PIXELS);
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
	v->notSaveToMetaFile = iNotSaveToMetaFile;
	
	// the text and its format inside the rectangle
	v->imageFileNameLen = iValue ? strlen(gStringValue(iValue)) + 1 : 0;

	if ( iTopic  &&  (len=gSize(iTopic)) )
		v->helpTopicLen = len + 1;
	else
		v->helpTopicLen = 0;

	gSetScalingMode(Application, sm);
	return self;
}


imeth	gSetControlParameters(void *vp)
{
	CTLTYPE_IMAGE_t	*v = vp;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	if(!v)
		return NULL;

	gSetName(self, v->name);
	
	// size and position of the rectangle
	gSetSize(self, v->height, v->width);
	gSetVirtualPosition(self, v->yPos, v->xPos);

	// the status of the rectangle
	v->hidden == 'Y' ? gHide(self) : gDisplay(self);
	v->disabled == 'Y' ? gDisable(self) : gEnable(self);
	iNotSaveToMetaFile = v->notSaveToMetaFile;

	// iValue to be determined by v->imageFileNameLen;

	gSetScalingMode(Application, sm);
	return self;
}


imeth	gSaveControl(FILE *fp)
{
	CTLTYPE_IMAGE_t	v;
	short	type = CTLTYPE_IMAGE, size = sizeof(v);
	if(!fp)
		return NULL;

	gGetControlParameters(self, &v);
	if (1 != fwrite(&type, sizeof type, 1, fp))
		return NULL;
	if (1 != fwrite(&size, sizeof size, 1, fp))
		return NULL;
	if (1 != fwrite(&v, sizeof v, 1, fp))
		return NULL;

	if (v.imageFileNameLen  &&  1 != fwrite(gStringValue(iValue), (int) v.imageFileNameLen, 1, fp))
		return NULL;

	if (v.helpTopicLen  &&  1 != fwrite(gStringValue(iTopic), (int) v.helpTopicLen, 1, fp))
		return NULL;

	return self;
}


#define	BUFLEN	128


cmeth	gLoadControl(FILE *fp, parent)
{
	CTLTYPE_IMAGE_t	v;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];

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
		double controlScaleFactor = gGetControlScale(parent);
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
	ctl = gAddImageControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

	// image file name
	if (v.imageFileNameLen) {
		p = v.imageFileNameLen > BUFLEN ? malloc((unsigned)v.imageFileNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.imageFileNameLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.imageFileNameLen > BUFLEN)
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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}


cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	CTLTYPE_IMAGE_t	v;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];

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
	ctl = gAddImageControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

	// image file name
	if (v.imageFileNameLen) {
		p = v.imageFileNameLen > BUFLEN ? malloc((unsigned)v.imageFileNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		if (1 != fread(p, (int) v.imageFileNameLen, 1, fp))
			vError(Application, "Error reading control layout definition file");
		gSetStringValue(ctl, p);
		if (v.imageFileNameLen > BUFLEN)
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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}



imeth object gSetTextObj(object textObj)
{
	char *pfileName;
	int  len;

	// same object 
	if(iValue==textObj)
		return self;

	iBitmapID = 0;
	
	// clear instance variable
	if(iValue) {
		gDispose(iValue);
		iValue = NULL;
	}
	// if(iBitmapInfo) {
	//	free(iBitmapInfo);
	//	iBitmapInfo = NULL;
	// }

	// empty string object
	if(!textObj) 
		return NULL;

	// new string object
	// check the string in textObj
	pfileName = gStringValue(textObj);
	len = strlen(pfileName);

	if ( len<=4  || stricmp(pfileName+len-4, ".bmp") )
		return NULL;
	
	// iBitmapInfo = LoadPackedDib (pfileName);
	// if(iBitmapInfo) // correct bitmap file
	iValue = textObj;

	pInitDIB(self, pfileName);
	return self;
}


imeth object gSetTextObjWithStr(char *pStr)
{
	int len;

	// string already in iValue
	if(iValue && stricmp(gStringValue(iValue),pStr)==0)
		return self;

	iBitmapID = 0;
	
	// clear instance variable
	if(iValue) {
		gDispose(iValue);
		iValue = NULL;
	}
	// if(iBitmapInfo) {
	//	free(iBitmapInfo);
	// 	iBitmapInfo = NULL;
	//}

	// empty string
	if(!pStr || !(*pStr)) 
		return NULL;

	// a new string
	// check the string pStr
	len = strlen(pStr);
	if ( len<=4  || stricmp(pStr+len-4, ".bmp") )
		return NULL;
	
	// iBitmapInfo = LoadPackedDib (pStr);
	// if(iBitmapInfo) // correct bitmap file
	iValue = gNewWithStr(String, pStr);

	pInitDIB(self, pStr);

	return self;
}


///////////////////////////////////////////////////////////////////////////////
// gPrintCtlScreen: print the bitmap of the control at the correct location
// printerObj:      the printer object
// dScaleX:         the scaling factor for the x coordinate of the control, 
//	                dScaleX = printerLogPixelsX/(double)viewLogPixelsX;
// dScaleY:         the scaling factor for the y coordinate of the control, 
//                  dScaleY = printerLogPixelsY/(double)viewLogPixelsY
//
///////////////////////////////////////////////////////////////////////////////
imeth	gPrintCtlScreen(object printerObj, double dScaleX, double dScaleY, int nViewOffsetX, int nViewOffsetY)
{
	RECT    rect;
	HDC     hdcPrinter;
	int     nPhysicalWidth, nPhysicalHeight, nPrinterOffsetX, nPrinterOffsetY;
	object  parentObj;

	if(!iValue)  // no bitmap to be printed
		return NULL;

	if(gHiddenStatus(self)==1)  // skipping printing
		return self;

	// check input parameters:
	if(!printerObj)    // validate the printerObj
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) )  // get and validate the print DC
		return NULL;

	if(dScaleX<=0 || dScaleY<=0)  // validate the view to printer scaling factors
		return NULL;

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

	// gFillRectWithBitmap(self, hdcPrinter, &rect);
	gFillRectWithBitmap16(self, hdcPrinter, &rect);

	return self;
}



/////////////////////////////////////////////////////////////////////////
// gCLDLoadControl: load the line control of the cld file for printing
//
/////////////////////////////////////////////////////////////////////////
cmeth	gCLDLoadControl(FILE *fp, object parentObj)
{
	CTLTYPE_IMAGE_t  v;
	short            size;
	char             *p, buf[BUFLEN];
	object	         ctl = NULL;
	ivType           *iv = NULL;

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

	ctl = gCLDNewWindowControl(ImageControl, v.name, parentObj);
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

	if(iValue)
		gDispose(iValue);

	iValue = 0; 
	// iBitmapInfo = NULL;
	iNotSaveToMetaFile = 0;

	return obj;
}


#if 0
//////////////////////////////////////////////////////////////
//  LoadPackedDib: Load DIB File as Packed-Dib Memory Block
//////////////////////////////////////////////////////////////

static BITMAPINFO * LoadPackedDib (char *szFileName)
{
     BITMAPFILEHEADER bmfh ;
     BITMAPINFO     * pbmi ;
     BOOL             bSuccess ;
     DWORD            dwPackedDibSize, dwBytesRead ;
     HANDLE           hFile ;

	 if(!szFileName || !(*szFileName))
		return NULL ;

     // Open the file: read access, prohibit write access
     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL, 
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
          return NULL ;

     // Read in the BITMAPFILEHEADER
     bSuccess = ReadFile (hFile, &bmfh, sizeof (BITMAPFILEHEADER), 
                          &dwBytesRead, NULL) ;

     if (!bSuccess || (dwBytesRead != sizeof (BITMAPFILEHEADER))         
                   || (bmfh.bfType != * (WORD *) "BM"))
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

     // Allocate memory for the packed-DIB & read it in
     dwPackedDibSize = bmfh.bfSize - sizeof (BITMAPFILEHEADER) ;

     pbmi = malloc (dwPackedDibSize) ;

     bSuccess = ReadFile (hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;

     if (!bSuccess || (dwBytesRead != dwPackedDibSize))
     {
          free (pbmi) ;
          return NULL ;
     }

     return pbmi ;
}


/////////////////////////////////////////////////
// Functions to get information from Packed Dib
/////////////////////////////////////////////////
static int GetPackedDibWidth (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcWidth ;
     else
          return pPackedDib->bmiHeader.biWidth ;
}

static int GetPackedDibHeight (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcHeight ;
     else
          return abs (pPackedDib->bmiHeader.biHeight) ;
}

static int GetPackedDibBitCount (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcBitCount ;
     else
          return pPackedDib->bmiHeader.biBitCount ;
}

static int GetPackedDibRowLength (BITMAPINFO * pPackedDib)
{
     return ((PackedDibGetWidth (pPackedDib) * 
              PackedDibGetBitCount (pPackedDib) + 31) & ~31) >> 3 ;
}


//////////////////////////////////////////////////////////////
// GetPackedDibInfoHeaderSize includes possible color masks!
//////////////////////////////////////////////////////////////
static int GetPackedDibInfoHeaderSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcSize ;

     else if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPINFOHEADER))
          return pPackedDib->bmiHeader.biSize + 
                    (pPackedDib->bmiHeader.biCompression == 
                                        BI_BITFIELDS ? 12 : 0) ;

     else return pPackedDib->bmiHeader.biSize ;
}


////////////////////////////////////////////////////////////////
// GetPackedDibColorsUsed returns value in information header;
// could be 0 to indicate non-truncated color table!
////////////////////////////////////////////////////////////////
static int GetPackedDibColorsUsed (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return 0 ;
     else
          return pPackedDib->bmiHeader.biClrUsed ;
}


//////////////////////////////////////////////////////////////////////
//  GetPackedDibNumColors is actual number of entries in color table
//////////////////////////////////////////////////////////////////////
static int GetPackedDibNumColors (BITMAPINFO * pPackedDib)
{
     int nNumColors ;

     nNumColors = GetPackedDibColorsUsed (pPackedDib) ;

     if (nNumColors == 0 && GetPackedDibBitCount (pPackedDib) < 16)
          nNumColors = 1 << GetPackedDibBitCount (pPackedDib) ;

     return nNumColors ;
}


static int GetPackedDibColorTableSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return GetPackedDibNumColors (pPackedDib) * sizeof (RGBTRIPLE) ;
     else
          return GetPackedDibNumColors (pPackedDib) * sizeof (RGBQUAD) ;
}


static RGBQUAD * GetPackedDibColorTablePtr (BITMAPINFO * pPackedDib)
{
     if (GetPackedDibNumColors (pPackedDib) == 0)
          return 0 ;

     return (RGBQUAD *) (((BYTE *) pPackedDib) + 
                                   GetPackedDibInfoHeaderSize (pPackedDib)) ;
}


static RGBQUAD * GetPackedDibColorTableEntry (BITMAPINFO * pPackedDib, int i)
{
     if (GetPackedDibNumColors (pPackedDib) == 0)
          return 0 ;

     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return (RGBQUAD *) 
               (((RGBTRIPLE *) GetPackedDibColorTablePtr (pPackedDib)) + i) ;
     else
          return GetPackedDibColorTablePtr (pPackedDib) + i ;
}


/////////////////////////////////
//  PackedDibGetBitsPtr finally!
/////////////////////////////////
static BYTE * GetPackedDibBitsPtr (BITMAPINFO * pPackedDib)
{
     return ((BYTE *) pPackedDib) + GetPackedDibInfoHeaderSize (pPackedDib) +
                                    GetPackedDibColorTableSize (pPackedDib) ;
}

//////////////////////////////////////////////////////////////////////////
//  PackedDibGetBitsSize can be calculated from the height and row length
//      if it's not explicitly in the biSizeImage field
//////////////////////////////////////////////////////////////////////////
static int GetPackedDibBitsSize (BITMAPINFO * pPackedDib)
{
     if ((pPackedDib->bmiHeader.biSize != sizeof (BITMAPCOREHEADER)) &&
         (pPackedDib->bmiHeader.biSizeImage != 0))
         return pPackedDib->bmiHeader.biSizeImage ;

     return GetPackedDibHeight (pPackedDib) * 
            GetPackedDibRowLength (pPackedDib) ;
}


///////////////////////////////////////////////////////////////////
// CreatePackedDibPalette creates logical palette from Packed DIB
///////////////////////////////////////////////////////////////////
static HPALETTE CreatePackedDibPalette (BITMAPINFO * pPackedDib)
{
     HPALETTE     hPalette ;
     int          i, nNumColors ;
     LOGPALETTE * plp ;
     RGBQUAD    * prgb ;

     if (0 == (nNumColors = GetPackedDibNumColors (pPackedDib)))
          return (HPALETTE) NULL;

     plp = malloc (sizeof (LOGPALETTE) * 
                         (nNumColors - 1) * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = nNumColors ;

     for (i = 0 ; i < nNumColors ; i++)
     {
          prgb = GetPackedDibColorTableEntry (pPackedDib, i) ;

          plp->palPalEntry[i].peRed   = prgb->rgbRed ;
          plp->palPalEntry[i].peGreen = prgb->rgbGreen ;
          plp->palPalEntry[i].peBlue  = prgb->rgbBlue ;
          plp->palPalEntry[i].peFlags = 0 ;
     }

     hPalette = CreatePalette (plp) ;
     free (plp) ;

     return hPalette ;
}
#endif


/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibNumColors(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    : Determines the number of colors in the DIB by looking at   *
 *               the BitCount filed in the info block.                      *
 *                                                                          *
 *  RETURNS    : The number of colors in the DIB.                           *
 *                                                                          *
 ****************************************************************************/
static	WORD DibNumColors (VOID FAR * pv)
{
	INT                 bits;
	LPBITMAPINFOHEADER  lpbi;
	LPBITMAPCOREHEADER  lpbc;

	lpbi = ((LPBITMAPINFOHEADER)pv);
	lpbc = ((LPBITMAPCOREHEADER)pv);

	/*  With the BITMAPINFO format headers, the size of the palette
	 *  is in biClrUsed, whereas in the BITMAPCORE - style headers, it
	 *  is dependent on the bits per pixel ( = 2 raised to the power of
	 *  bits/pixel).
	 */
	if (lpbi->biSize != sizeof(BITMAPCOREHEADER)){
		if (lpbi->biClrUsed != 0)
			return (WORD)lpbi->biClrUsed;
		bits = lpbi->biBitCount;
	}
	else
		bits = lpbc->bcBitCount;

	switch (bits){
        case 1:
                return 2;
        case 4:
                return 16;
        case 8:
                return 256;
        default:
                /* A 24 bitcount DIB has no color table */
                return 0;
	}
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : lread(int fh, VOID FAR *pv, DWORD ul)                      *
 *                                                                          *
 *  PURPOSE    : Reads data in steps of 32k till all the data has been read.*
 *                                                                          *
 *  RETURNS    : 0 - If read did not proceed correctly.                     *
 *               number of bytes read otherwise.                            *
 *                                                                          *
 ****************************************************************************/
static	DWORD lread (
    INT       fh,
    VOID FAR      *pv,
    DWORD             ul)
{
	DWORD     ulT = ul;
	BYTE *hp = pv;

	while (ul > (DWORD)MAXREAD) {
		if (_lread(fh, (LPSTR)hp, (UINT)MAXREAD) != MAXREAD)
			return 0;
		ul -= MAXREAD;
		hp += MAXREAD;
	}
	if (_lread(fh, (LPSTR)hp, (UINT)ul) != (UINT)ul)
		return 0;
	return ulT;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf)
 *                                                                          *
 *  PURPOSE    : read file header (which is packed) and convert into unpacked BITMAPFILEHEADER strucutre
 *                                                                          *
 *  RETURNS    : VOID
 *                                                                          *
 ****************************************************************************/

static VOID ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf, LPDWORD lpdwoff)
{
        DWORD off;

        off = _llseek(fh, 0L, (UINT) SEEK_CUR);
        *lpdwoff = off;

/*              BITMAPFILEHEADER STRUCUTURE is as follows 
 *              BITMAPFILEHEADER
 *              WORD    bfType 
 >          ....                  <     add WORD if packed here!
 *              DWORD   bfSize 
 *              WORD    bfReserved1
 *              WORD    bfReserved2
 *              DWORD   bfOffBits 
 *                      This is the packed format, unpacked adds a WORD after bfType
 */

        /* read in bfType*/
        _lread(fh, (LPSTR) &pbf->bfType, sizeof(WORD));
        /* read in last 3 dwords*/
        _lread(fh, (LPSTR) &pbf->bfSize, sizeof(DWORD) * 3);

}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadDibBitmapInfo(int fh)                                  *
 *                                                                          *
 *  PURPOSE    : Will read a file in DIB format and return a global HANDLE  *
 *               to it's BITMAPINFO.  This function will work with both     *
 *               "old" (BITMAPCOREHEADER) and "new" (BITMAPINFOHEADER)      *
 *               bitmap formats, but will always return a "new" BITMAPINFO  *
 *                                                                          *
 *  RETURNS    : A handle to the BITMAPINFO of the DIB in the file.         *
 *                                                                          *
 ****************************************************************************/
static	HANDLE ReadDibBitmapInfo(INT fh)
{
	DWORD     off;
	HANDLE    hbi = 0;
	INT       size;
	INT       i;
	WORD      nNumColors;

	RGBQUAD FAR       *pRgb;
	BITMAPINFOHEADER   bi;
	BITMAPCOREHEADER   bc;
	LPBITMAPINFOHEADER lpbi;
	BITMAPFILEHEADER   bf;
	DWORD              dwWidth = 0;
	DWORD              dwHeight = 0;
	WORD               wPlanes, wBitCount;

	if (fh == -1)
		return 0;
#ifdef FIXDWORDALIGNMENT
	/* Reset file pointer and read file header */
	off = _llseek(fh, 0L, (UINT)SEEK_CUR);
	if ((SIZEOF_BITMAPFILEHEADER_PACKED)  != _lread(fh, (LPSTR)&bf, (UINT)sizeof (SIZEOF_BITMAPFILEHEADER_PACKED)))
		return FALSE;
#else
        ReadBitMapFileHeaderandConvertToDwordAlign(fh, &bf, &off);
        /* at this point we have read the file into bf*/
#endif

	/* Do we have a RC HEADER? */
	if (!ISDIB (bf.bfType)) {    
		bf.bfOffBits = 0L;               
                _llseek(fh, off, (UINT)SEEK_SET); /*seek back to beginning of file*/
	}
	if (sizeof (bi) != _lread(fh, (LPSTR)&bi, (UINT)sizeof(bi)))
		return FALSE;

	nNumColors = DibNumColors (&bi);

	/* Check the nature (BITMAPINFO or BITMAPCORE) of the info. block
	 * and extract the field information accordingly. If a BITMAPCOREHEADER,
	 * transfer it's field information to a BITMAPINFOHEADER-style block
	 */
	switch (size = (INT)bi.biSize){
        case sizeof (BITMAPINFOHEADER):
		break;

        case sizeof (BITMAPCOREHEADER):

		bc = *(BITMAPCOREHEADER*)&bi;

		dwWidth   = (DWORD)bc.bcWidth;
		dwHeight  = (DWORD)bc.bcHeight;
		wPlanes   = bc.bcPlanes;
		wBitCount = bc.bcBitCount;

		bi.biSize           = sizeof(BITMAPINFOHEADER);
		bi.biWidth              = dwWidth;
		bi.biHeight             = dwHeight;
		bi.biPlanes             = wPlanes;
		bi.biBitCount           = wBitCount;

		bi.biCompression        = BI_RGB;
		bi.biSizeImage          = 0;
		bi.biXPelsPerMeter      = 0;
		bi.biYPelsPerMeter      = 0;
		bi.biClrUsed            = nNumColors;
		bi.biClrImportant       = nNumColors;

		_llseek(fh, (LONG)sizeof (BITMAPCOREHEADER) - sizeof (BITMAPINFOHEADER), (UINT)SEEK_CUR);
		break;

        default:
		/* Not a DIB! */
		return 0;
	}

	/*  Fill in some default values if they are zero */
	if (bi.biSizeImage == 0){
		bi.biSizeImage = WIDTHBYTES ((DWORD)bi.biWidth * bi.biBitCount)
			* bi.biHeight;
	}
	if (bi.biClrUsed == 0)
		bi.biClrUsed = DibNumColors(&bi);

	/* Allocate for the BITMAPINFO structure and the color table. */
	hbi = GlobalAlloc (GHND, (LONG)bi.biSize + nNumColors * sizeof(RGBQUAD));
	if (!hbi)
		return 0;
	lpbi = (VOID FAR *)GlobalLock (hbi);
	*lpbi = bi;

	/* Get a pointer to the color table */
	pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + bi.biSize);
	if (nNumColors){
		if (size == sizeof(BITMAPCOREHEADER)){
			/* Convert a old color table (3 byte RGBTRIPLEs) to a new
			 * color table (4 byte RGBQUADs)
			 */
			_lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBTRIPLE));

			for (i = nNumColors - 1; i >= 0; i--){
				RGBQUAD rgb;

				rgb.rgbRed      = ((RGBTRIPLE FAR *)pRgb)[i].rgbtRed;
				rgb.rgbBlue     = ((RGBTRIPLE FAR *)pRgb)[i].rgbtBlue;
				rgb.rgbGreen    = ((RGBTRIPLE FAR *)pRgb)[i].rgbtGreen;
				rgb.rgbReserved = (BYTE)0;

				pRgb[i] = rgb;
			}
		}
		else
			_lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBQUAD));
	}

	if (bf.bfOffBits != 0L){
		_llseek(fh, off + bf.bfOffBits, (UINT)SEEK_SET);
        }
	GlobalUnlock(hbi);
	return hbi;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateBIPalette(LPBITMAPINFOHEADER lpbi)                   *
 *                                                                          *
 *  PURPOSE    : Given a Pointer to a BITMAPINFO struct will create a       *
 *               a GDI palette object from the color table.                 *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
static	HPALETTE	CreateBIPalette(LPBITMAPINFOHEADER lpbi)
{
	LOGPALETTE          *pPal;
	HPALETTE            hpal = 0;
	WORD                nNumColors;
	BYTE                red;
	BYTE                green;
	BYTE                blue;
	WORD                i;
	RGBQUAD        FAR *pRgb;

	if (!lpbi)
		return 0;

	if (lpbi->biSize != sizeof(BITMAPINFOHEADER))
		return 0;

	/* Get a pointer to the color table and the number of colors in it */
	pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + (WORD)lpbi->biSize);
	nNumColors = DibNumColors(lpbi);

	if (nNumColors){
		/* Allocate for the logical palette structure */
/*		pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY)); */
		pPal = (LOGPALETTE *) calloc(1, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
		if (!pPal)
			return 0;

		pPal->palNumEntries = nNumColors;
		pPal->palVersion    = PALVERSION;

		/* Fill in the palette entries from the DIB color table and
		 * create a logical color palette.
		 */
		for (i = 0; i < nNumColors; i++){
			pPal->palPalEntry[i].peRed   = pRgb[i].rgbRed;
			pPal->palPalEntry[i].peGreen = pRgb[i].rgbGreen;
			pPal->palPalEntry[i].peBlue  = pRgb[i].rgbBlue;
			pPal->palPalEntry[i].peFlags = (BYTE)0;
		}
		hpal = CreatePalette(pPal);
		free(pPal);
/*		LocalFree((HANDLE)pPal);  */
	}
	else if (lpbi->biBitCount == 24){
		/* A 24 bitcount DIB has no color table entries so, set the number of
		 * to the maximum value (256).
		 */
		nNumColors = MAXPALETTE;
/*		pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));  */
		pPal = (LOGPALETTE *) calloc(1, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
		if (!pPal)
			return 0;

		pPal->palNumEntries = nNumColors;
		pPal->palVersion    = PALVERSION;

		red = green = blue = 0;

		/* Generate 256 (= 8*8*4) RGB combinations to fill the palette
		 * entries.
		 */
		for (i = 0; i < pPal->palNumEntries; i++){
			pPal->palPalEntry[i].peRed   = red;
			pPal->palPalEntry[i].peGreen = green;
			pPal->palPalEntry[i].peBlue  = blue;
			pPal->palPalEntry[i].peFlags = (BYTE)0;

			if (!(red += 32))
				if (!(green += 32))
					blue += 64;
		}
		hpal = CreatePalette(pPal);
		free(pPal);
/*		LocalFree((HANDLE)pPal);  */
	}
	return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateDibPalette(HANDLE hbi)                               *
 *                                                                          *
 *  PURPOSE    : Given a Global HANDLE to a BITMAPINFO Struct               *
 *               will create a GDI palette object from the color table.     *
 *               (BITMAPINFOHEADER format DIBs only)                                     *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
static	HPALETTE	CreateDibPalette(HANDLE hbi)
{
	HPALETTE hpal;

	if (!hbi)
		return 0;
	hpal = CreateBIPalette((LPBITMAPINFOHEADER)GlobalLock(hbi));
	GlobalUnlock(hbi);
	return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibInfo(HANDLE hbi,LPBITMAPINFOHEADER lpbi)                *
 *                                                                          *
 *  PURPOSE    : Retrieves the DIB info associated with a CF_DIB            *
 *               format memory block.                                       *
 *                                                                          *
 *  RETURNS    : TRUE  - if successful.                                     *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
static	BOOL DibInfo (
    HANDLE hbi,
    LPBITMAPINFOHEADER lpbi)
{
	if (hbi){
		*lpbi = *(LPBITMAPINFOHEADER)GlobalLock (hbi);

		/* fill in the default fields */
		if (lpbi->biSize != sizeof (BITMAPCOREHEADER)){
			if (lpbi->biSizeImage == 0L)
                                lpbi->biSizeImage = WIDTHBYTES(lpbi->biWidth*lpbi->biBitCount) * lpbi->biHeight;

			if (lpbi->biClrUsed == 0L)
                                lpbi->biClrUsed = DibNumColors (lpbi);
		}
		GlobalUnlock (hbi);
		return TRUE;
	}
	return FALSE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PaletteSize(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    :  Calculates the palette size in bytes. If the info. block  *
 *                is of the BITMAPCOREHEADER type, the number of colors is  *
 *                multiplied by 3 to give the palette size, otherwise the   *
 *                number of colors is multiplied by 4.                                                          *
 *                                                                          *
 *  RETURNS    :  Palette size in number of bytes.                          *
 *                                                                          *
 ****************************************************************************/
static	WORD PaletteSize(VOID FAR * pv)
{
	LPBITMAPINFOHEADER lpbi;
	WORD               NumColors;

	lpbi      = (LPBITMAPINFOHEADER)pv;
	NumColors = DibNumColors(lpbi);

	if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
		return (WORD)(NumColors * sizeof(RGBTRIPLE));
	else
		return (WORD)(NumColors * sizeof(RGBQUAD));
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :OpenDIB(LPSTR szFile)                                       *
 *                                                                          *
 *  PURPOSE    :Open a DIB file and create a MEMORY DIB, a memory handle    *
 *              containing BITMAPINFO, palette data and the bits.           *
 *                                                                          *
 *  RETURNS    :A handle to the DIB.                                        *
 *                                                                          *
 ****************************************************************************/
static	HANDLE	OpenDIB(LPSTR szFile)
{
	HFILE               fh;
	BITMAPINFOHEADER    bi;
	LPBITMAPINFOHEADER  lpbi;
	DWORD               dwLen = 0;
	DWORD               dwBits;
	HANDLE              hdib;
	HANDLE              h;
	OFSTRUCT            of;

	/* Open the file and read the DIB information */
	fh = OpenFile(szFile, &of, (UINT)OF_READ);
	if (fh == -1) {
		gMoreHandles(LowFile);
		fh = OpenFile(szFile, &of, (UINT)OF_READ);
	}
	if (fh == -1)
		return 0;

	hdib = ReadDibBitmapInfo(fh);
	if (!hdib)
		return 0;
	DibInfo(hdib, &bi);

	/* Calculate the memory needed to hold the DIB */
	dwBits = bi.biSizeImage;
	dwLen  = bi.biSize + (DWORD)PaletteSize (&bi) + dwBits;

	/* Try to increase the size of the bitmap info. buffer to hold the DIB */
	h = GlobalReAlloc(hdib, dwLen, GHND);
	if (!h){
		GlobalFree(hdib);
		hdib = 0;
	}
	else
		hdib = h;

	/* Read in the bits */
	if (hdib){

		lpbi = (VOID FAR *)GlobalLock(hdib);
		lread(fh, (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi), dwBits);
		GlobalUnlock(hdib);
	}
	_lclose(fh);

	return hdib;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BitmapFromDib(HANDLE hdib, HPALETTE hpal)                  *
 *                                                                          *
 *  PURPOSE    : Will create a DDB (Device Dependent Bitmap) given a global *
 *               handle to a memory block in CF_DIB format                  *
 *                                                                          *
 *  RETURNS    : A handle to the DDB.                                       *
 *                                                                          *
 ****************************************************************************/
static	HBITMAP	BitmapFromDib(HANDLE hdib, HPALETTE hpal)
{
	LPBITMAPINFOHEADER  lpbi;
	HPALETTE            hpalT;
	HDC                 hdc;
	HBITMAP             hbm;

	StartWait();

	if (!hdib)
		return 0;

	lpbi = (VOID FAR *)GlobalLock(hdib);

	if (!lpbi)
		return 0;

	hdc = GetDC(0);

	if (hpal){
		hpalT = SelectPalette(hdc,hpal,FALSE);
		RealizePalette(hdc);     // GDI Bug...????
	}

	hbm = CreateDIBitmap(hdc,
			     (LPBITMAPINFOHEADER)lpbi,
			     (LONG)CBM_INIT,
			     (LPSTR)lpbi + lpbi->biSize + PaletteSize(lpbi),
			     (LPBITMAPINFO)lpbi,
			     DIB_RGB_COLORS );

	if (hpal)
		SelectPalette(hdc,hpalT,FALSE);

	ReleaseDC(0,hdc);
	GlobalUnlock(hdib);

	EndWait();

	return hbm;
}

private	imeth	int	pInitDIBFromResource(object self)
{
	BITMAPINFOHEADER	bi;
	HANDLE			ins = gInstance(Application);
	LPBITMAPINFOHEADER	lpbi;
	WORD FAR *		pw;
	INT			i;
	
	iHbiCurrent = FindResource(ins, MAKEINTRESOURCE(iBitmapID), RT_BITMAP);
	if (!iHbiCurrent)
		return FALSE;
	iHbiCurrent = LoadResource(ins, iHbiCurrent);
	if (!iHbiCurrent)
		return FALSE;

	DibInfo(iHbiCurrent,&bi);
	
	iHpalCurrent = CreateDibPalette(iHbiCurrent);
	if (!iHpalCurrent)
		return FALSE;
	
	lpbi = (VOID FAR *)GlobalLock(iHbiCurrent);
	if (lpbi->biBitCount != 24) {
		pw = (WORD FAR *)((LPSTR)lpbi + lpbi->biSize);

		for (i=0; i<(INT)lpbi->biClrUsed; i++)
			*pw++ = (WORD)i;
	}
	GlobalUnlock(iHbiCurrent);
	iBLegitDraw = TRUE;

	iHdibCurrent = iHbiCurrent;
	
	if ((bi.biCompression != BI_RGB) && !iHdibCurrent){
		gFreeDib(self);
		return FALSE;
	}

	if (iHdibCurrent){
                iHbmCurrent = BitmapFromDib(iHdibCurrent,iHpalCurrent);
                if (!iHbmCurrent){
			gFreeDib(self);
			return FALSE;
                }
	}
	
	return TRUE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : InitDIB(hWnd)                                              *
 *                                                                          *
 *  PURPOSE    : Reads a DIB from a file, obtains a handle to it's          *
 *               BITMAPINFO struct., sets up the palette and loads the DIB. *
 *                                                                          *
 *  RETURNS    : TRUE  - DIB loads ok                                       *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
private imeth 	int	pInitDIB(char *achFileName)
{
	HFILE              fh;
	LPBITMAPINFOHEADER lpbi;
	WORD FAR *         pw;
	INT                i;
	BITMAPINFOHEADER   bi;
	OFSTRUCT           of;

	DWORD      dwOffset;

	gFreeDib(self);

	if (!achFileName) {
		if (iBitmapID)
			return pInitDIBFromResource(self);
		return FALSE;
	}
	
	/* Open the file and get a handle to it's BITMAPINFO */

	
	fh = OpenFile(achFileName, (LPOFSTRUCT)&of, (UINT)OF_READ);
	if (fh == -1) {
		gMoreHandles(LowFile);
		fh = OpenFile(achFileName, (LPOFSTRUCT)&of, (UINT)OF_READ);
	}
	if (fh == -1) {
//		ErrMsg("Can't open file '%ls'", (LPSTR)achFileName);
		return FALSE;
	}
	iHbiCurrent = ReadDibBitmapInfo(fh);

	dwOffset = _llseek(fh, 0L, (UINT)SEEK_CUR);
	_lclose(fh);

	if (iHbiCurrent == 0) {
//		ErrMsg("%ls is not a Legitimate DIB File!", (LPSTR)achFileName);
		return FALSE;
	}
	DibInfo(iHbiCurrent,&bi);

	/* Set up the palette */
	iHpalCurrent = CreateDibPalette(iHbiCurrent);
	if (iHpalCurrent == 0) {
//		ErrMsg("CreatePalette() Failed");
		return FALSE;
	}

	/*  Convert the DIB color table to palette relative indexes, so
	 *  SetDIBits() and SetDIBitsToDevice() can avoid color matching.
	 *  We can do this because the palette we realize is identical
	 *  to the color table of the bitmap, ie the indexes match 1 to 1
	 *
	 *  Now that the DIB color table is palette indexes not RGB values
	 *  we must use DIB_PAL_COLORS as the wUsage parameter to SetDIBits()
	 */
	lpbi = (VOID FAR *)GlobalLock(iHbiCurrent);
	if (lpbi->biBitCount != 24) {
		pw = (WORD FAR *)((LPSTR)lpbi + lpbi->biSize);

		for (i=0; i<(INT)lpbi->biClrUsed; i++)
			*pw++ = (WORD)i;
	}
	GlobalUnlock(iHbiCurrent);
	iBLegitDraw = TRUE;

	/*  If the input bitmap is not in RGB FORMAT the banding code will
	 *  not work!  we need to load the DIB bits into memory.
	 *  if memory DIB, load it all NOW!  This will avoid calling the
	 *  banding code.
	 */
	iHdibCurrent = OpenDIB(achFileName);

	/*  If the RLE could not be loaded all at once, exit gracefully NOW,
	 *  to avoid calling the banding code
	 */
	if ((bi.biCompression != BI_RGB) && !iHdibCurrent){
//		ErrMsg ("Could not load RLE!");
		gFreeDib(self);
		return FALSE;
	}

	if (iHdibCurrent){
                iHbmCurrent = BitmapFromDib(iHdibCurrent,iHpalCurrent);
                if (!iHbmCurrent){
//			ErrMsg ("Could not create bitmap!");
			gFreeDib(self);
			return FALSE;
                }
	}

	return TRUE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : StretchDibBlt( HDC hdc,                                    *
 *                              int x, int y,                               *
 *                              int dx, int dy,                             *
 *                              HANDLE hdib,                                *
 *                              int x0, int y0,                             *
 *                              int dx0, int dy0,                           *
 *                              LONG rop)                                   *
 *                                                                          *
 *  PURPOSE    : Draws a bitmap in CF_DIB format, using StretchDIBits()     *
 *               taking the same parameters as StretchBlt().                *
 *                                                                          *
 *  RETURNS    : TRUE  - if function succeeds.                              *
 *               FALSE - otherwise.                                         *
 *                                                                          *
 ****************************************************************************/
static	BOOL StretchDibBlt (
    HDC hdc,
    INT x,
    INT y,
    INT dx,
    INT dy,
    HANDLE hdib,
    INT x0,
    INT y0,
    INT dx0,
    INT dy0,
    LONG rop)
{
	LPBITMAPINFOHEADER lpbi;
	LPSTR        pBuf;
	BOOL         f;

	if (!hdib)
		return PatBlt(hdc,x,y,dx,dy,rop);

	lpbi = (VOID FAR *)GlobalLock(hdib);

	if (!lpbi)
		return FALSE;

	pBuf = (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi);

	f = StretchDIBits ( hdc,
			    x, y,
			    dx, dy,
			    x0, y0,
			    dx0, dy0,
			    pBuf, (LPBITMAPINFO)lpbi,
			    DIB_RGB_COLORS,
			    rop);

	GlobalUnlock(hdib);
	return f;
}

imeth	int	gSetButtonBorder(int val)
{
	int	prev = iButtonBorder;

	if (prev == val)
		return prev;
	
	iButtonBorder = val;

	pInitDIB(self, iValue ? gStringValue(iValue) : NULL);
	gRedrawWindow(self);
	
	return prev;
}


imeth gWriteXML(FILE *fp)
{

	CTLTYPE_IMAGE_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);

	gGetControlParameters(self, &v);

	fprintf(fp,"\t\t<image>\n");
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));
	fprintf(fp,"\t\t\t<fileName>%s</fileName>\n",gStringToXML(XMLNode,buf,gGetBitmapFileName(self)));
	fprintf(fp,"\t\t\t<imageFileNameLen>%d</imageFileNameLen>\n",v.imageFileNameLen);
	fprintf(fp,"\t\t\t<xpath>%s</xpath>\n",gXPathBinding(self));
	
	fprintf(fp,"\t\t</image>\n");


	return self;
}


cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_IMAGE_t	v;
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
	v.imageFileNameLen=gGetIntFromNode(curnode,"imageFileNameLen");

	// get the screen resolution in the CLD file which is loaded in window.d
	// and the current screen resolution and scale the controls in the cld file
	// so that the cld file will be displayed with the same look
	
	if(gGetScaleFlg(parent)) {    // do the scaling if the scaling flag is set
		double controlScaleFactor = gGetControlScale(parent);
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
	ctl = gAddImageControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	// image file name
	if (v.imageFileNameLen) {
		p = v.imageFileNameLen > BUFLEN ? malloc((unsigned)v.imageFileNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"fileName");
		gSetStringValue(ctl, p);
		if (v.imageFileNameLen > BUFLEN)
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

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}




