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




defclass  ControlVector  {
	RECT	  iR;
	iCtl;
};

#define MAX(a, b)	((a) > (b) ? (a) : (b))
#define MIN(a, b)	((a) < (b) ? (a) : (b))


cvmeth	vNew(object ctl, HWND hwnd, int *vMax, int *hMax)
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	iCtl = ctl;

	gGetRect(ctl, &iR);

	if (gIsKindOf(ctl, ComboBox)) {
		RECT	rect;

		gGetClientRect(iCtl, &rect);
		iR.bottom = iR.top + rect.bottom;
	}
	
	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);

	if (hwnd)
		InvalidateRect(hwnd, &iR, TRUE);

	gSetVector(ctl, obj);
	
	gSetScalingMode(Application, pm);

	return obj;
}

imeth	int	gVertShiftTV(int rows, int *vMax, int *hMax)
{
	iR.top += rows;
	iR.bottom += rows;
	*hMax = iR.right;
	*vMax = iR.bottom;
	return iR.top < 0;
}

imeth	gUpdateWithOffset(int *vMax, int *hMax, int vOffset, int hOffset)
{
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	gGetRect(iCtl, &iR);
	if (gIsKindOf(iCtl, ComboBox)) {
		RECT	rect;

		gGetClientRect(iCtl, &rect);
		iR.bottom = iR.top + rect.bottom;
	}

	iR.top    += vOffset;
	iR.bottom += vOffset;
	iR.left   += hOffset;
	iR.right  += hOffset;

	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);

	gSetScalingMode(Application, pm);

	return self;
}

imeth	gUpdateMax(int *vMax, int *hMax)
{
	if (gIsKindOf(iCtl, ComboBox)) {
		RECT	rect;

		gGetClientRect(iCtl, &rect);
		iR.bottom = iR.top + rect.bottom;
	}
	*hMax = MAX(*hMax, iR.right);
	*vMax = MAX(*vMax, iR.bottom);
	return self;
}

#define OVERLAPPED(r1, r2)					\
	!(r1.top > r2.bottom  ||  r1.bottom < r2.top  ||	\
	r1.left > r2.right  ||  r1.right < r2.left)

#define	DRAW3D	ClassOf(iCtl) == TextControl || ClassOf(iCtl) == NumericControl ||	\
		ClassOf(iCtl) == TimeControl || ClassOf(iCtl) == DateControl || ClassOf(iCtl) == ListBox
	
private	imeth	pRemoveBorder(object self, HDC dc)
{
	HBRUSH	b = SelectObject(dc, CreateSolidBrush(gColor(gGetBackBrush(gDialog(iCtl)))));
	int	h, w;
	RECT	r = iR;

	h = r.bottom - r.top;
	w = r.right - r.left;

	PatBlt(dc, r.left - 2, r.top - 2, w, 2, PATCOPY);
	PatBlt(dc, r.right, r.top - 2, 2, h, PATCOPY);
	PatBlt(dc, r.left - 2, r.top, 2, h, PATCOPY);
	PatBlt(dc, r.left, r.bottom, w, 2, PATCOPY);

	DeleteObject(SelectObject(dc, b));

	return self;
}

private	imeth	pPaintBorder(object self, HDC dc, RECT r)
{
	HBRUSH	b;
	int	h = r.bottom - r.top;
	int	w = r.right - r.left;

	if (gHiddenStatus(iCtl) == 1) /* Must look for 1 because show is 2 and default is 0. */
		return self;
	
	b = SelectObject(dc, CreateSolidBrush(gColor(gGetBackBrush(gDialog(iCtl)))));
		
	DeleteObject(SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW))));
	PatBlt(dc, r.left - 2, r.top - 2, w + 3, 1, PATCOPY);
	PatBlt(dc, r.left - 2, r.top - 2, 1, h + 3, PATCOPY);

	DeleteObject(SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_WINDOWTEXT))));
	PatBlt(dc, r.left - 1, r.top - 1, w + 1, 1, PATCOPY);
	PatBlt(dc, r.left - 1, r.top - 1, 1, h + 1, PATCOPY);

	DeleteObject(SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_WINDOW))));
	PatBlt(dc, r.left - 2, r.bottom + 1, w + 4, 1, PATCOPY);
	PatBlt(dc, r.right + 1, r.top - 2, 1, h + 4, PATCOPY);

	DeleteObject(SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_BTNFACE))));
	PatBlt(dc, r.left - 1, r.bottom, w + 2, 1, PATCOPY);
	PatBlt(dc, r.right, r.top - 1, 1, h + 2, PATCOPY);
	DeleteObject(SelectObject(dc, b));

	return self;
}



imeth	gOutputVector(PAINTSTRUCT *ps, int vOffset, int hOffset)
{
	RECT	r;
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	if (gIsKindOf(iCtl, ComboBox)) {
		RECT	rect;

		gGetClientRect(iCtl, &rect);
		iR.bottom = iR.top + rect.bottom;
	}

	r = iR;
	
	r.top    -= vOffset;
	r.bottom -= vOffset;
	r.left   -= hOffset;
	r.right  -= hOffset;

	//if (OVERLAPPED(r, ps->rcPaint)) {   // Yanghui

		if (DRAW3D)
			pRemoveBorder(self, ps->hdc);
			
		if (!gModifyMode(iCtl)) {
			gUpdateControlPosition(iCtl, r.top, r.left);
			
			if (DRAW3D)
				pPaintBorder(self, ps->hdc, r);
		}

	//}  // Yanghui

	gSetScalingMode(Application, pm);

	return self;
}



imeth	int	gWithinRange(RECT *r, int *h, int *v)
{
	*h = iR.right;
	*v = iR.bottom;
	return OVERLAPPED(iR, (*r));
}

imeth	gUpdate()
{
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	gGetRect(iCtl, &iR);
	if (gIsKindOf(iCtl, ComboBox)) {
		RECT	rect;

		gGetClientRect(iCtl, &rect);
		iR.bottom = iR.top + rect.bottom;
	}
	
	gSetScalingMode(Application, pm);

	return self;
}

imeth	gControl()
{
	return iCtl;
}












