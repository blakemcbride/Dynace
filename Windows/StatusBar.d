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

#define	MAX_SECTIONS	20


defclass  StatusBar : ChildWindow  {
	int	iBorder;
	int	iHeight;
	iFont;

	int	iNSections;
	object	iText[MAX_SECTIONS];
	int	iID[MAX_SECTIONS];
	int	iMode[MAX_SECTIONS];
	int	iWidth[MAX_SECTIONS];
	
 init:	init_class;
};

static	long	process_wm_paint(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);


cvmeth	vNew(pwnd)
{
	object	wnd;
	int	sm;
	ivType	*iv;

	ChkArgTyp(pwnd, 2, Window);
	sm = gSetScalingMode(Application, SM_PIXELS);
	wnd = vNew(super, pwnd, 20, 80);
	iv = ivPtr(wnd);
	gBackBrush(wnd, vNew(SystemBrush, COLOR_BTNFACE));
	iBorder =(GetSystemMetrics(SM_CXFRAME) + 1) / 2;
	iFont = vNew(ExternalFont, "MS Sans Serif", 8);
	iHeight = 1 + gLineHeight(iFont) + iBorder * 4;
	gSetScalingMode(Application, sm);
	gSetStatusBar(pwnd, wnd);
	gAddHandlerAfter(wnd, (unsigned) WM_PAINT, process_wm_paint);
	gDefaultProcessingMode(wnd, (unsigned) WM_PAINT, 0);  /*  no auto default processing  */
	return wnd;
}

imeth	object	gDispose, gDeepDispose ()
{
	int	i;
	
	gDispose(iFont);
	for (i=0 ; i != iNSections ; i++)
		if (iText[i])
			gDispose(iText[i]);
	return gDispose(super);
}

imeth	gUpdateState()
{
	HWND	pwnd, swnd;
	RECT	rect;
	
	swnd = gHandle(self);
	pwnd = GetParent(swnd);
	GetClientRect(pwnd, &rect);
	SetWindowPos(swnd, 0, 0, rect.bottom - iHeight, rect.right, iHeight, SWP_NOZORDER);
	return self;
}

static	void	paint_border(HDC dc, RECT *rect)
{
	HBRUSH	b;
	int	w, h;

	w = rect->right - rect->left;
	h = rect->bottom - rect->top;
	b = SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_BTNHIGHLIGHT)));
//	PatBlt(dc, rect->right, rect->top, 1, h, PATCOPY);
	PatBlt(dc, rect->left, rect->bottom, w, 1, PATCOPY);
	DeleteObject(SelectObject(dc, CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW))));
	PatBlt(dc, rect->left, rect->top, 1, h, PATCOPY);
	PatBlt(dc, rect->left, rect->top, w, 1, PATCOPY);
	DeleteObject(SelectObject(dc, b));
}

static	void	paint_section(ivType *iv, HDC dc, int i, int wth)
{
	RECT	rect;
	int	pm;
	HGDIOBJ	org;

	rect.left   = wth + iBorder;
	rect.top    = iBorder + 1;
	rect.right  = iWidth[i] + rect.left;
	rect.bottom = iHeight - iBorder;
	paint_border(dc, &rect);

	pm = SetBkMode(dc, TRANSPARENT);
	org = SelectObject(dc, gHandle(iFont));
	InflateRect(&rect, -iBorder, -iBorder);
	if (iText[i])
		DrawText(dc, gStringValue(iText[i]), -1, &rect, iMode[i]);
	SelectObject(dc, org);
	SetBkMode(dc, pm);
}

private	imeth	long	process_wm_paint(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	PAINTSTRUCT	ps;
	RECT		rect;
	int		i, wth;

	BeginPaint(hwnd, &ps);
	GetClientRect(hwnd, &rect);
//	MoveToEx(ps.hdc, 0, 0, NULL);
//	LineTo(ps.hdc, rect.right, 0);
	PatBlt(ps.hdc, 0, 1, rect.right - rect.left, 1, WHITENESS);

	for (i=wth=0 ; i != iNSections ; i++) {
		paint_section(iv, ps.hdc, i, wth);
		wth += iBorder * 2 + iWidth[i];
	}
	
	EndPaint(hwnd, &ps);
	
	return 0L;
}

imeth	gAddSection(int id, int width, int mode)
{
	if (iNSections  ==  MAX_SECTIONS)
		return NULL;
	iID[iNSections] = id;
	iMode[iNSections] = mode | DT_VCENTER;
	gScaleToPixels(Application, NULL, &width, iFont);
	iWidth[iNSections] = width;
	iNSections++;
	return self;
}

imeth	gSetSectionText(int id, char *text)
{
	int	i;
	HWND	h;

	for (i=0 ; i < iNSections && iID[i] != id ; i++);
	if (i == iNSections)
		return NULL;
	if (iText[i])
		gChangeStrValue(iText[i], text);
	else
		iText[i] = gNewWithStr(String, text);
	h = gHandle(self);
	if (h)
		InvalidateRect(h, NULL, TRUE);
	return self;
}

imeth	int	gPixelHeight()
{
	return iHeight;
}

static	void	init_class()
{
	gDontCollect(CLASS);
}









