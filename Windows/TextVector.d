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


defclass  TextVector  {
	RECT	  iR;
	iFont;
	iStr;
	COLORREF  iColor;		/*  text color		*/
	int  iFormat;           // Yanghui: text-drawing flags used in DrawText 
};

#define MAX(a, b)	((a) > (b) ? (a) : (b))
#define MIN(a, b)	((a) < (b) ? (a) : (b))


cvmeth	vNew(int row, int col, font, HWND hwnd, char *str, int *vMax,
	     int *hMax, COLORREF color)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iStr = gNewWithStr(String, str);
	gSetVars(font, row, col, &iR, str);
	iFont  = font;
	iColor = color;
	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);
	if (hwnd)
		InvalidateRect(hwnd, &iR, TRUE);

	iFormat = -1;    // Yanghui
	return obj;
}


// Yanghui:
cvmeth	gNewWithRect(LPRECT lpRect, int nFormat, font, HWND hwnd, char *str, int *vMax,
	     int *hMax, COLORREF color)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	CopyRect(&iR, lpRect);
	iFormat = nFormat;

	iStr = gNewWithStr(String, str);

	iFont  = font;
	iColor = color;

	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);
	if (hwnd)
		InvalidateRect(hwnd, &iR, TRUE);
	return obj;
}
// Yanghui


imeth	int	gVertShiftTV(int rows, int *vMax, int *hMax)
{
	iR.top += rows;
	iR.bottom += rows;
	*hMax = iR.right;
	*vMax = iR.bottom;
	return iR.top < 0;
}

imeth	gUpdateMax(int *vMax, int *hMax)
{
	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);
	return self;
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iStr);
	return gDispose(super);
}	

imeth	char	*gStringValue()
{
	return gStringValue(iStr);
}

#define OVERLAPPED(r1, r2)					\
	!(r1.top > r2.bottom  ||  r1.bottom < r2.top  ||	\
	r1.left > r2.right  ||  r1.right < r2.left)

imeth	gOutputVector(PAINTSTRUCT *ps, int vOffset, int hOffset)
{
	RECT	r = iR;
	HGDIOBJ	org;

	r.top    -= vOffset;
	r.bottom -= vOffset;
	r.left   -= hOffset;
	r.right  -= hOffset;
	if (OVERLAPPED(r, ps->rcPaint))  {
		org = SelectObject(ps->hdc, gHandle(iFont));
		SetTextColor(ps->hdc, iColor);

		// Yanghui:
		if (iFormat<0)
			TextOut(ps->hdc, r.left, r.top, gStringValue(iStr), gSize(iStr));
		else
			DrawText(ps->hdc, gStringValue(iStr), gSize(iStr), &r, (unsigned) iFormat);
		// Yanghui

		SelectObject(ps->hdc, org);
	}
	return self;
}

imeth	int	gWithinRange(RECT *r, int *h, int *v)
{
	*h = iR.right;
	*v = iR.bottom;
	return OVERLAPPED(iR, (*r));
}

imeth	gDeepCopy, gCopy ()
{
	object	nobj = gDeepCopy(super);
	ivType	*iv2 = ivPtr(nobj);
	iv2->iFont = gDeepCopy(iFont);
	iv2->iStr  = gDeepCopy(iStr);
	return nobj;
}








