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




#include <windows.h>

#define CHANDLESIZE 7

defclass  TrackRect { 
	HWND    iHwnd;              // handle to control's parent window
	HDC     iHDCTrack;          // handle to the DC
	RECT    iTrackRect;         // the rectangle for the tracking rect
	BOOL    iFTrackRectShown;   // flag for track rect
};


static void showTrackRect(ivType *iv);
static void hideTrackRect(ivType *iv);
static void drawTrackRect(ivType *iv, DWORD dwRop);
static void correctRect(RECT *pRect);


/////////////////////////////////////////////////////////////////////////
// gInitTracking
//
// This function initializes a tracking operation. 
// HWND hwnd: handle to control's parent window 
//
/////////////////////////////////////////////////////////////////////////
cmeth gInitTracking(HWND hwnd)
{
	object	obj;
	ivType	*iv;

	if(!hwnd)
		return NULL;

	obj = gNew(super);
	iv = ivPtr(obj);

	iHwnd = hwnd;
	iHDCTrack = GetDC(iHwnd);
	// SetROP2(iHDCTrack, R2_NOT);

	iFTrackRectShown = FALSE;
	return obj;
}



/////////////////////////////////////////////////////////////////////////
// gDrawTrackRect
//
// This routine draws the drag rectangle.  It is assumed that the window
// has been locked for update appropriately or this could leave scratch
// around. The rectangle given is in window coordinates. The hideTrackRect 
// and showTrackRect functions are called to temporarily hide the track 
// rectangle, but this routine must be called again every time that the 
// tracking rectangle is to be changed.
//
// Arguments:
//   PRECT prc     - tracking rectangle to draw (in dialog units).
//   BOOL fDraw    - If TRUE, the rectangle will be drawn.  
//
/////////////////////////////////////////////////////////////////////////
imeth gDrawTrackRect(PRECT prc, BOOL bDraw)
{
	hideTrackRect(iv);

	iTrackRect = *prc;

	if (bDraw)
		showTrackRect(iv);
	return self;
}



/////////////////////////////////////////////////////////////////////////
// showTrackRect
//
// This routine shows the current tracking rectangle.
//
/////////////////////////////////////////////////////////////////////////
static void showTrackRect(ivType *iv)
{
	if (!iFTrackRectShown) {
		drawTrackRect(iv, DSTINVERT);
		iFTrackRectShown = TRUE;
	}
}



/////////////////////////////////////////////////////////////////////////
// hideTrackRect
//
// This routine hides the current tracking rectangle.
//
/////////////////////////////////////////////////////////////////////////
static void hideTrackRect(ivType *iv)
{
	if (iFTrackRectShown) {
		drawTrackRect(iv, DSTINVERT);
		iFTrackRectShown = FALSE;
	}
}



/////////////////////////////////////////////////////////////////////////
// gCancelTracking
//
// This routine is used to cancel the display of the tracking rectangle.
// It is basically the opposite of initTracking.
//
/////////////////////////////////////////////////////////////////////////
imeth gCancelTracking()
{
	if (iFTrackRectShown)
		hideTrackRect(iv);

	ReleaseDC(iHwnd, iHDCTrack);
	return gDispose(super);
}



/////////////////////////////////////////////////////////////////////////
// drawTrackRect
//
// This function draws a one-pixel width rectangle using the given
// raster operation.
//
// Arguments:
//   DWORD dwRop - RasterOp to use (DSTINVERT, BLACKNESS, etc.).
//
/////////////////////////////////////////////////////////////////////////
static void drawTrackRect(ivType *iv, DWORD dwRop)
{
	int x;
	int y;
	POINT pt;

	// top
	x = iTrackRect.right - (pt.x = iTrackRect.left);
	y = iTrackRect.bottom - (pt.y = iTrackRect.top);
	PatBlt(iHDCTrack, pt.x, pt.y, x, 1, dwRop);

	// bottom
	pt.y = iTrackRect.bottom - 1;
	if(y>1 || y<-1)
		PatBlt(iHDCTrack, pt.x, pt.y, x, 1, dwRop);

	// left
	pt.y = iTrackRect.top;
	PatBlt(iHDCTrack, pt.x, pt.y, 1, y, dwRop);

	// right
	pt.x = iTrackRect.right - 1;
	if(x>1 || x<-1)
		PatBlt(iHDCTrack, pt.x, pt.y, 1, y, dwRop);
}




// the following two routines are utility function (like global function)
// it works independently
// cmeth object gClientToScreenRect(HWND hwnd, PRECT prc)
// cmeth object gScreenToClientRect(HWND hwnd, PRECT prc)



/////////////////////////////////////////////////////////////////////////
// gClientToScreenRect
//
// This function converts the coordinates in a rectangle from points
// relative to the client area into points that are relative to the
// screen.
//
// Arguments:
//   HWND hwnd - Window handle for the conversion.
//   PRECT prc - Pointer to the rectangle to convert.
//
///////////////////////////////////////////////////////////////////////////
cmeth object gClientToScreenRect(HWND hwnd, PRECT prc)
{
	ClientToScreen(hwnd, (PPOINT) prc);
	ClientToScreen(hwnd, ((PPOINT) prc) + 1);
	return self;
}



///////////////////////////////////////////////////////////////////////////
// gScreenToClientRect
//
// This function converts the coordinates in a rectangle from points
// relative to the screen into points that are relative to the given
// window's client area.
//
// Arguments:
//   HWND hwnd - Window handle for the conversion.
//   PRECT prc - Pointer to the rectangle to convert.
//
//
///////////////////////////////////////////////////////////////////////////
cmeth object gScreenToClientRect(HWND hwnd, PRECT prc)
{
	ScreenToClient(hwnd, (PPOINT) prc);
	ScreenToClient(hwnd, ((PPOINT) prc) + 1);
	return self;
}



/////////////////////////////////////////////////////////////////////////////
// correctRect
//
// This routine corrects a rectangle so that its right>left and bottom>top.
//
/////////////////////////////////////////////////////////////////////////////
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


cmeth void gCorrectRect(RECT *pRect)
{
	correctRect(pRect);
}



///////////////////////////////////////////////////////////////////////////
// gRectInRect
//
///////////////////////////////////////////////////////////////////////////
cmeth BOOL gRectInRect(PRECT prc1, PRECT prc2)
{
	POINT p1, p2, p3, p4;

	correctRect(prc1);
	correctRect(prc2);

	p1.x = prc2->left;
	p1.y = prc2->top;

	p2.x = prc2->right;
	p2.y = prc2->top;
	
	p3.x = prc2->right;
	p3.y = prc2->bottom;

	p4.x = prc2->left;
	p4.y = prc2->bottom;

	return (PtInRect(prc1,p1) && PtInRect(prc1,p2) && PtInRect(prc1,p3) &&  PtInRect(prc1,p4));
}














