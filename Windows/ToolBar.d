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

// #define	MAXBITMAPS	30 // Yanghui
#define	MAXBITMAPS	40     // Yanghui

#define TYPE_BUTTON	1
#define TYPE_COMBO	2

typedef	long	(*lfun)();

defclass  ToolBar : ChildWindow  {
	object		iParent;
	int		iTopBorder;
	int		iBorder;
	int		iMaxHeight;
	int		iPressed;	//  button being pressed
	int		iTip;		//  button being tooltipped
	char		iAltColor;	//  using alternate color
	object		iTipWind;	//  tooltip window
	RECT		iRect;		//  rect of button being pressed or tipped

	int		iNBitMaps;
	unsigned 	iID[MAXBITMAPS];
	HBITMAP		iBitmap1[MAXBITMAPS];
	HBITMAP		iBitmap2[MAXBITMAPS];
	int		iHeight[MAXBITMAPS];
	int		iWidth[MAXBITMAPS];
	int		iSpace[MAXBITMAPS];	//  extra space to the left of the bitmap
	int		iState[MAXBITMAPS];
	int		iCtlTypes[MAXBITMAPS];
	object		iCtlObjects[MAXBITMAPS];
	lfun		iFun[MAXBITMAPS];
	object		iTips[MAXBITMAPS];
	int		iDisabled[MAXBITMAPS];
	int		iHidden[MAXBITMAPS];
	int		iRGBHold[MAXBITMAPS][16];
	object		iBitmapFile[MAXBITMAPS];

	ifun	iMouseFunction[MAXBITMAPS][6];
class:
	ifun	cMouseFunction[6];
	ifun	cAccessMode;

	int		cTTBorder;	//  tooltip border (1=yes, 0=no)
	object		cFont;
	object		cTextBrush;
	object		cBackBrush;

 init:	init_class;
};

static	int	get_index(WPARAM p);

static	long	process_wm_paint(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);
static	long	process_wm_lbuttondown(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);
static	long	process_wm_lbuttonup(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);
static	long	process_wm_rbuttondown(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);
static	long	process_wm_mousemove(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);

private	imeth	int	pGetIndexFromID(object self, unsigned id)
{
	int	i;
	
	for (i = 0; i < iNBitMaps  &&  iID[i] != id; i++);
	if (i >= iNBitMaps)
		return -1;

	return i;
}

private	imeth	int	pGetControlIndexFromName(object self, char *name)
{
	int	i;
	int	found;
	
	for (i = 0, found = 0; !found  &&  i < iNBitMaps; i++)
		if (iCtlTypes[i] == TYPE_COMBO  &&  !strcmp(gName(iCtlObjects[i]), name))
			found = 1;
	if (!found)
		return -1;

	return i;
}

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
	iTopBorder = (1 + 3 * GetSystemMetrics(SM_CYFRAME)) / 2;
	iBorder = GetSystemMetrics(SM_CXBORDER);
	gSetScalingMode(Application, sm);
	gSetToolBar(pwnd, wnd);

	gAddHandlerAfter(wnd, (unsigned) WM_PAINT, process_wm_paint);
	gDefaultProcessingMode(wnd, (unsigned) WM_PAINT, 0);  /*  no auto default processing  */

	gAddHandlerAfter(wnd, (unsigned) WM_LBUTTONDOWN, process_wm_lbuttondown);
	gDefaultProcessingMode(wnd, (unsigned) WM_LBUTTONDOWN, 0);  /*  no auto default processing  */

	gAddHandlerAfter(wnd, (unsigned) WM_LBUTTONUP, process_wm_lbuttonup);
	gDefaultProcessingMode(wnd, (unsigned) WM_LBUTTONUP, 0);  /*  no auto default processing  */

	gAddHandlerAfter(wnd, (unsigned) WM_RBUTTONDOWN, process_wm_rbuttondown);

	gAddHandlerAfter(wnd, (unsigned) WM_MOUSEMOVE, process_wm_mousemove);

	iTip = iPressed = -1;
	iParent = pwnd;

	return wnd;
}

imeth	gUpdateState()
{
	HWND	pwnd, swnd;
	RECT	rect;
	
	swnd = gHandle(self);
	pwnd = GetParent(swnd);
	GetClientRect(pwnd, &rect);
	SetWindowPos(swnd, 0, 0, 0, rect.right, iMaxHeight, SWP_NOZORDER);
	return self;
}

static	void	paint_bitmap(ivType *iv, HDC dc, int i, int wth)
{
	HDC	dc2;
	HBITMAP	bm;
	int	b = iBorder, tb = iTopBorder;


	if (!iHidden[i])
		if (!iState[i]) {	//  button up
			if (iAltColor)  {
				dc2 = CreateCompatibleDC(dc);
				bm = SelectObject(dc2, iBitmap1[i]);
				TransparentBlt(dc, wth + iSpace[i] + b, tb + b, iWidth[i], iHeight[i], dc2, 0, 0, iWidth[i], (UINT)iHeight[i],RGB(192,192,192));
				SelectObject(dc2, bm);
				DeleteDC(dc2);

				//  top line
			
				PatBlt(dc, wth + iSpace[i] + b, tb + b, iWidth[i], 2*b, WHITENESS);
				//  left line
		
				PatBlt(dc, wth + iSpace[i] + b, tb + b, 2*b, iHeight[i], WHITENESS);
				//  bottom line
				PatBlt(dc, wth + iSpace[i] +1 , tb+b+iHeight[i], iWidth[i]+1  *b, b , BLACKNESS);
				//  right line
				PatBlt(dc, wth + iSpace[i] + b + iWidth[i], tb+1, b , iHeight[i]  *b, BLACKNESS);
			} else {
				dc2 = CreateCompatibleDC(dc);
				bm = SelectObject(dc2, iBitmap1[i]);
				BitBlt(dc, wth + iSpace[i] + b, tb + b, iWidth[i], iHeight[i], dc2, 0, 0, SRCCOPY);
			
				SelectObject(dc2, bm);
				DeleteDC(dc2);

				//  top line
				PatBlt(dc, wth + iSpace[i], tb, iWidth[i]+3*b, b, BLACKNESS);
				PatBlt(dc, wth + iSpace[i] + b, tb + b, iWidth[i], 2*b, WHITENESS);
				//  left line
				PatBlt(dc, wth + iSpace[i], tb, b, iHeight[i]+3*b, BLACKNESS);
				PatBlt(dc, wth + iSpace[i] + b, tb + b, 2*b, iHeight[i], WHITENESS);
				//  bottom line
				PatBlt(dc, wth + iSpace[i], tb+b+iHeight[i], iWidth[i]+3*b, b*2, BLACKNESS);
				//  right line
				PatBlt(dc, wth + iSpace[i] + b + iWidth[i], tb, b*2, iHeight[i]+3*b, BLACKNESS);

			}
		} else {	//  button down
			dc2 = CreateCompatibleDC(dc);
			bm = SelectObject(dc2, iBitmap2[i] ? iBitmap2[i] : iBitmap1[i]);
			
			if (iAltColor)
				TransparentBlt(dc, wth + iSpace[i] + b, tb + b, iWidth[i], iHeight[i], dc2, 0, 0, iWidth[i], iHeight[i],RGB(192,192,192));
			else
				BitBlt(dc, wth + iSpace[i] + 2*b, tb+ 2*b, iWidth[i], iHeight[i], dc2, 0, 0, SRCCOPY);

			SelectObject(dc2, bm);
			DeleteDC(dc2);

			//  top line
			PatBlt(dc, wth + iSpace[i], tb, iWidth[i]+3*b, 2*b, BLACKNESS);
			//  left line
			PatBlt(dc, wth + iSpace[i], tb, 2*b, iHeight[i]+3*b, BLACKNESS);
			//  bottom line
			PatBlt(dc, wth + iSpace[i], tb+2*b+iHeight[i], iWidth[i]+3*b, b, BLACKNESS);
			//  right line
			PatBlt(dc, wth + iSpace[i] + 2*b + iWidth[i], tb, b, iHeight[i]+3*b, BLACKNESS);
		}
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
/*	PatBlt(ps.hdc, 0, rect.bottom - 1, rect.right - rect.left, 1, BLACKNESS);*/
/*	PatBlt(ps.hdc, 0, 0, rect.right - rect.left, 1, WHITENESS);*/

	for (i=wth=0 ; i != iNBitMaps ; i++) {
		if (iCtlTypes[i] == TYPE_BUTTON)
			paint_bitmap(iv, ps.hdc, i, wth);
		else if (iCtlTypes[i] == TYPE_COMBO) {
			int	sm = gSetScalingMode(Application, SM_PIXELS);
			gSetPosition(iCtlObjects[i], iTopBorder + iBorder - 1, wth + iSpace[i] + iBorder);
			gSetScalingMode(Application, sm);
		}
			
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
	}
	
	EndPaint(hwnd, &ps);
	return 0L;
}

static	int	bitmap_start(ivType *iv, int n)
{
	int	i, wth, left=0;
	
	for (i=wth=0 ; i <= n ; i++) {
		left = wth + iSpace[i] + iBorder;
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
	}
	return left;
}

static	int	which_tool(ivType *iv, LPARAM lParam, RECT *rect)
{
	int	wth, i;
	POINT	p;

	p.x = LOWORD(lParam);
	p.y = HIWORD(lParam);
	rect->top = iTopBorder + iBorder;
	for (wth=i=0 ; i != iNBitMaps ; i++) {
		rect->left = wth + iSpace[i] + iBorder;
		rect->right = rect->left + iWidth[i] + iBorder;
		rect->bottom = rect->top + iHeight[i] + iBorder;
		if (PtInRect(rect, p))
			return i;
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
	}
	return -1;
}

static	int	getToolRect(ivType *iv, int val, RECT *rect)
{
	int	wth, i;

	rect->top = iTopBorder + iBorder;
	for (wth=i=0 ; i != iNBitMaps ; i++) {
		rect->left = wth + iSpace[i] + iBorder;
		rect->right = rect->left + iWidth[i] + iBorder;
		rect->bottom = rect->top + iHeight[i] + iBorder;
		if (i == val) {
			rect->top -= 3;
			rect->bottom += 3;
			rect->left -= 3;
			rect->right += 3;
			return i;
		}
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
	}
	return -1;
}

static	int	number_of_lines(char *s)
{
	int	n;
	for (n=1 ; *s ; s++)
		if (*s == '\n')
			n++;
	return n;
}

static	int	max_width(char *s, object font)
{
	char	buf[200], *p;
	int	m = 0, w;

	*buf = ' ';
	while (*s) {
		p = buf + 1;
		while (*s  &&  *s != '\n')
			*p++ = *s++;
		*p++ = ' ';
		*p = '\0';
		w = gStrPixelWidth(font, buf);
		if (w > m)
			m = w;
		if (*s)
			s++;
	}
	return m;
}

static	void	put_str(object wind, char *s)
{
	char	buf[200], *p;

	*buf = ' ';
	while (*s) {
		p = buf + 1;
		while (*s  &&  *s != '\n')
			*p++ = *s++;
		*p++ = *s;
		*p = '\0';
		gPuts(wind, buf);
		if (*s)
			s++;
	}
}

static	void	create_tip(object self, ivType *iv, LPARAM lParam)
{
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	object	font = cFont ? cFont : gGetFont(Application);
	char	*str = gStringValue(iTips[iTip]);
	int	height = gLineHeight(font) * number_of_lines(str) + iBorder * 2 * cTTBorder;
	POINT	p;
	RECT	r;

	GetWindowRect(gHandle(gGetClientWind(iParent)), &r);
	
	p.x = r.left + bitmap_start(iv, iTip);
	p.y = r.top;

	if (iTipWind)
		gDispose(iTipWind);
	iTipWind = vNew(PopupWindow, "Tool Tip", height, iBorder * cTTBorder * 2 + max_width(str, font));
	gSetParent(iTipWind, gGetClientWind(iParent));
	gSetPosition(iTipWind, p.y, p.x);

	gTextBrush(iTipWind, cTextBrush ? gCopy(cTextBrush) : vNew(SystemBrush, COLOR_CAPTIONTEXT));
	gBackBrush(iTipWind, cBackBrush ? gCopy(cBackBrush) : vNew(SystemBrush, COLOR_ACTIVECAPTION));

	if (cTTBorder)
		gSetStyle(iTipWind, WS_VISIBLE | WS_BORDER);
	else
		gSetStyle(iTipWind, WS_VISIBLE);
	if (cFont)
		gUse(iTipWind, gCopy(cFont));
	put_str(iTipWind, str);
	gShow(iTipWind);

	gSetFocus(iParent);

	SetCapture(gHandle(self));
	gSetScalingMode(Application, sm);
	if (!cFont)
		gDispose(font);
}

imeth void gUseAlternateColor()
{
	iAltColor = 1;
}

private	imeth	long	process_wm_mousemove(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	POINT	p;
	int	tool;

	p.x = LOWORD(lParam);
	p.y = HIWORD(lParam);
	if (iPressed == -1) {
		tool = which_tool(iv, lParam, &iRect);
		if (iTip != -1  &&  tool != iTip) {
			iTipWind = gDispose(iTipWind);
			ReleaseCapture();
   		}
		if (iTip != tool) {
			iTip = tool;
			if (iTip != -1)
				if (!iTips[iTip]  ||  iHidden[iTip])
					iTip = -1;
				else
					create_tip(self, iv, lParam);
		}
	} else if (!PtInRect(&iRect, p)) {
		iState[iPressed] = 0;
		iPressed = -1;
		InvalidateRect(gHandle(self), &iRect, FALSE);
		ReleaseCapture();
	}
	return 0L;
}

private	imeth	long	process_wm_lbuttondown(object	self, 
					       HWND	hwnd, 
					       UINT	mMsg, 
					       WPARAM	wParam, 
					       LPARAM	lParam)
{
	HWND	h;
	int	i;

	iPressed = which_tool(iv, lParam, &iRect);
	if (iPressed == -1  ||  iDisabled[iPressed]  ||  iHidden[iPressed])
		return 0L;
	
	i = get_index(wParam);
	if (i >= 0  &&  iMouseFunction[iPressed][i]) {
		unsigned	id = iPressed >= 0 ? iID[iPressed] : 0;
		
		iMouseFunction[iPressed][i](self, id, (unsigned) wParam);
		iPressed = -1;
		return 0L;
	}

	if (wParam != MK_LBUTTON) {
		iPressed = -1;
		return 0L;
	}
	
	if (iTip != -1) {
		iTipWind = gDispose(iTipWind);
		iTip = -1;
		ReleaseCapture();
	}
	if (iPressed != -1) {
		iState[iPressed] = 1;
		InvalidateRect(h=gHandle(self), &iRect, FALSE);
		SetCapture(h);
	}
	return 0L;
}

private	imeth	long	process_wm_lbuttonup(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	RECT	r;
	POINT	p;
	int	wth, i;

	if (iPressed == -1  ||  iDisabled[iPressed]  ||  iHidden[iPressed])
		return 0L;
	p.x = LOWORD(lParam);
	p.y = HIWORD(lParam);
	r.top = iTopBorder + iBorder;
	for (wth=i=0 ; i != iPressed+1 ; i++) {
		r.left = wth + iSpace[i] + iBorder;
		r.right = r.left + iWidth[i] + iBorder;
		r.bottom = r.top + iHeight[i] + iBorder;
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
	}
	iState[i=iPressed] = 0;
	iPressed = -1;
	InvalidateRect(gHandle(self), &r, FALSE);
	ReleaseCapture();
	if (iFun[i]  &&  PtInRect(&r, p)) {
		long	(*fun)() = iFun[i];

		if (SchemeClassSurrogate  &&  IsObj((object)fun)  &&  ClassOf(fun) == String) {
			char	cmd[100], ns[80];

			sprintf(cmd, "(%s (int->object %ld) %ld)",
				gFunctionName(SchemeClassSurrogate, (object)fun),
				(long) iParent, (long) iID[i]);
			gExecuteInNamespaceNR(SchemeClassSurrogate,
					      gNamespaceName(SchemeClassSurrogate, (object)fun, ns), 
					      cmd);
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)fun)  &&  ClassOf(fun) == JavaCallbackClassSurrogate)
			return gPerformJavaMenuCallback((object)fun, iParent, iID[i]);
		else if (JavaScriptClassSurrogate  &&  IsObj((object)fun)  &&  ClassOf(fun) == JavaScriptString) {
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%ld\"), %ld)", gStringValue((object)fun), (long) iParent, (long) iID[i]);
			gExecuteStringNR(JavaScriptClassSurrogate, cmd);
		} else
			fun(iParent, iID[i]);
	}
	return 0L;
}

static	void	init_class()
{
	gDontCollect(CLASS);
	gTextBrush(CLASS, vNew(SolidBrush, 0, 0, 0));
	gBackBrush(CLASS, vNew(SolidBrush, 255, 255, 225));
	gUse(CLASS, vNew(ExternalFont, "MS Sans Serif", 8));
	gSetBorder(CLASS);
}

static	COLORREF	fix_color(int n)
{
	COLORREF	x = GetSysColor(n);
	return RGB(GetBValue(x), GetGValue(x), GetRValue(x));
}

static	COLORREF	disable_color(COLORREF x)
{
	BYTE	blue	= GetBValue(x);
	BYTE	green	= GetGValue(x);
	BYTE	red	= GetRValue(x);
	BYTE	avg = ((blue + green + red) / 3) < 160 ? 128 : 192;

	return RGB(avg, avg, avg);
}

static	HBITMAP	load_bitmap(ivType *iv, char *file, unsigned id, int state, int pos)
{
	HDC			dc;
	HBITMAP			bm;
	HANDLE			res;
	DWORD			*rgb;
	BITMAPINFOHEADER	*bmi;
	void			*pid;

	if (file) {
#ifdef	WIN32
		bmi = (BITMAPINFOHEADER *) ResourceLoadBitmap(file, MAKEINTRESOURCE(id));
#else
		bmi = 0;
#endif
		if (!bmi)
			return 0;
	} else {
		HANDLE	ins = gInstance(Application);
		res = FindResource(ins, MAKEINTRESOURCE(id), RT_BITMAP);
		if (!res)
			return 0;
		res = LoadResource(ins, res);
		if (!res)
			return 0;
		bmi = (BITMAPINFOHEADER *) LockResource(res);
		if (!bmi) {
			FreeResource(res);
			return 0;
		}
	}
	rgb = (DWORD *) ((char *) bmi + bmi->biSize);
/*  Writing to rgb seems to be no longer possible on recent Win32.  It seems the memory is read-only.
    CLD.EXE was crashing on startup
	if (state > 0) {
		int	i;
		for (i = 0; i < 16; i++) {
			iRGBHold[pos][i] = rgb[i];
			rgb[i] = disable_color(rgb[i]);
		}
	} else if (state < 0) {
		int	i;
		for (i = 0; i < 16; i++)
			rgb[i] = iRGBHold[pos][i];
	}
*/
	pid = (void *) (rgb + 16);
	dc = GetDC((HWND)0);
	bm = CreateDIBitmap(dc, bmi, CBM_INIT, pid, (BITMAPINFO *) bmi, DIB_RGB_COLORS);
	ReleaseDC((HWND)0, dc);
	if (file)
		free(bmi);
	else {
		GlobalUnlock(res);
		FreeResource(res);
	}
	return bm;
}

private	imeth	pAddToolBitmap(char *file, unsigned id1, unsigned id2, int space, long (*fun)(), char *tip)
{
	BITMAP	bm;
	HBITMAP	hbm;
	int	height;
	
	if (iNBitMaps  ==  MAXBITMAPS)
		return NULL;
	hbm = load_bitmap(iv, file, id1, 0, -1);
	if (!hbm)
		return NULL;
	
	if (file && *file)
		iBitmapFile[iNBitMaps] = gNewWithStr(String, file);
	else
		iBitmapFile[iNBitMaps] = gNew(String);
	
	gScaleToPixels(Application, NULL, &space, NULL);
	GetObject(hbm, sizeof(BITMAP), &bm);
	iID[iNBitMaps] = id1;
	iBitmap1[iNBitMaps] = hbm;
	iHeight[iNBitMaps] = bm.bmHeight;
	iWidth[iNBitMaps] = bm.bmWidth;
	height = iTopBorder * 2 + bm.bmHeight + iBorder * 3;
	iSpace[iNBitMaps] = space;
	iFun[iNBitMaps] = fun;
	iCtlTypes[iNBitMaps] = TYPE_BUTTON;
	if (height > iMaxHeight)
		iMaxHeight = height;
	
	if (id2)
		hbm = load_bitmap(iv, file, id2, 0, -1);
	if (id2  &&  hbm) {
		GetObject(hbm, sizeof(BITMAP), &bm);
		iBitmap2[iNBitMaps] = hbm;
		if (iHeight[iNBitMaps] < bm.bmHeight)
			iHeight[iNBitMaps] = bm.bmHeight;
		if (iWidth[iNBitMaps] < bm.bmWidth)
			iWidth[iNBitMaps] = bm.bmWidth;
		height = iTopBorder * 2 + bm.bmHeight + iBorder * 3;
		if (height > iMaxHeight)
			iMaxHeight = height;
	}

	if (tip  &&  *tip)
		iTips[iNBitMaps] = gNewWithStr(String, tip);
	else
		iTips[iNBitMaps] = gNew(String);

	memcpy(iMouseFunction[iNBitMaps], cMouseFunction, sizeof cMouseFunction);

	iNBitMaps++;

	if (cAccessMode  &&  cAccessMode(self, id1))
		gEnableToolBitmap(self, id1, 0);
	
	return self;
}

imeth	gUpdateAccessMode()
{
	int	i;

	if (!cAccessMode)
		return self;
	
	for (i = 0; i < iNBitMaps; i++) {
		if (cAccessMode(self, iID[i]))
			gEnableToolBitmap(self, iID[i], 0);
		else
			gEnableToolBitmap(self, iID[i], 1);
	}

	return self;
}

imeth	gAddToolComboBox(int height, int width, int space, int (*fun)(), char *tip, char *name)
{
	int	b = iBorder;
	int	tb = iTopBorder;
	int	i;
	int	wth;
	int	end;
	object	ctl;
	int	sm;
	int	th = 22;	/* Combobox heights are constant.  This value includes 3d effects.
				   However, this value can change after the control has been created. */

	if (iNBitMaps  ==  MAXBITMAPS)
		return NULL;

	gScaleToPixels(Application, &height, &width, NULL);
	gScaleToPixels(Application, NULL, &space, NULL);
	iHeight[iNBitMaps] = th;
	iWidth[iNBitMaps] = width;
	iSpace[iNBitMaps] = space;
	iCtlTypes[iNBitMaps] = TYPE_COMBO;
	th = iTopBorder * 2 + th + iBorder * 3;
	if (th > iMaxHeight)
		iMaxHeight = th;

	for (i=wth=0 ; i != iNBitMaps ; i++)
		wth += iBorder * 3 + iWidth[i] + iSpace[i];
		
	sm = gSetScalingMode(Application, SM_PIXELS);
	ctl = iCtlObjects[iNBitMaps] = gAddComboBox(self, tb + b - 1, wth + space + b, height, width, &end, name);
	gSetScalingMode(Application, sm);

	gSetStyle(ctl, WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL);
	gSetChgFunction(ctl, fun);
	
	if (tip  &&  *tip)
		iTips[iNBitMaps] = gNewWithStr(String, tip);
	else
		iTips[iNBitMaps] = gNew(String);

//	memcpy(iMouseFunction[iNBitMaps], cMouseFunction, sizeof cMouseFunction);

	iNBitMaps++;

//	if (cAccessMode  &&  cAccessMode(self, id1))
//		gEnableToolComboBox(self, name, 0);
	
	return ctl;
}

imeth	gAddToolBitmap(unsigned id1, unsigned id2, int space, long (*fun)(), char *tip)
{
	return pAddToolBitmap(self, NULL, id1, id2, space, fun, tip);
}

imeth	gAddToolBitmapFromFile(char *file, unsigned id1, unsigned id2, int space, long (*fun)(), char *tip)
{
	return pAddToolBitmap(self, file, id1, id2, space, fun, tip);
}

imeth	object	gDispose, gDeepDispose ()
{
	int	i;
	
	if (iTipWind)
		gDispose(iTipWind);
	for (i=0 ; i != iNBitMaps ; i++) {
		if (iFun[i]  &&  IsObj((object) iFun[i]))
			gDispose((object) iFun[i]);
		if (iBitmap1[i])
			DeleteObject(iBitmap1[i]);
		if (iBitmap2[i])
			DeleteObject(iBitmap2[i]);
		if (iTips[i])
			gDispose(iTips[i]);
	}
	return gDispose(super);
}

imeth	int	gPixelHeight()
{
	return iMaxHeight;
}

cmeth	gTextBrush(brush)
{
	ChkArgTyp(brush, 2, Brush);
	if (cTextBrush)
		gDispose(cTextBrush);
	cTextBrush = brush;
	return self;
}

cmeth	gBackBrush(brush)
{
	ChkArgTyp(brush, 2, Brush);
	if (cBackBrush)
		gDispose(cBackBrush);
	cBackBrush = brush;
	return self;
}

cmeth	gUse(obj)
{
	ChkArg(obj, 2);
	if (gIsKindOf(obj, Font))  {
		cFont = obj;
	} else
		gError(self, "Incorrect 2nd argument to Use::Toolbar");
	return obj;
}

cmeth	gSetBorder()
{
	cTTBorder = 1;
	return self;
}

imeth	int	gEnableToolBitmap(unsigned id, int val)
{
	int	ps;
	int	i;
	HBITMAP	hbm;
	RECT	rect;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return 0;

	if ((iDisabled[i]  &&  !val)  ||  (!iDisabled[i]  &&  val))
		return val;
	
	hbm = load_bitmap(iv, NULL, id, val ? -1 : 1, i);
	if (!hbm)
		return 0;

	if (iBitmap1[i])
		DeleteObject(iBitmap1[i]);
	iBitmap1[i] = hbm;
	ps = iDisabled[i];

	iDisabled[i] = !val;

	if (getToolRect(iv, i, &rect) >= 0)
		InvalidateRect(gHandle(self), &rect, FALSE);
	
	return !ps;
}

imeth	int	gShowToolBitmap(unsigned id, int val)
{
	int	i, ps;
	RECT	rect;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return 0;
	ps = iHidden[i];
	iHidden[i] = !val;

	if (getToolRect(iv, i, &rect) >= 0)
		InvalidateRect(gHandle(self), &rect, TRUE);
	
	return !ps;
}

imeth	ifun	gSetToolFunction(unsigned id, long (*fun)())
{
	ifun	org;
	int	i;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return (ifun) 0;
	org = (ifun) iFun[i];
	iFun[i] = fun;
	if (org  &&  IsObj((object)org))
		org = (ifun) gDispose((object)org);
	return org;
}

imeth	gChangeTip(unsigned id, char *tip)
{
	int	i;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return NULL;
	if (iTips[i])
		gDispose(iTips[i]);
	iTips[i] = gNewWithStr(String, tip);
	return self;
}

imeth	gGetControlStr(char *name)
{
	int	r = pGetControlIndexFromName(self, name);

	if (r >= 0)
		return iCtlObjects[r];

	return NULL;
}

imeth	char	*gGetTip(unsigned id)
{
	int	i;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return "";
	return gStringValue(iTips[i]);
}

static	int	get_index(WPARAM p)
{
	if (p & MK_RBUTTON)
		if (p & MK_SHIFT)
			return 2;
		else if (p & MK_CONTROL)
			return 4;
		else
			return 0;
	else if (p & MK_LBUTTON)
		if (p & MK_SHIFT)
			return 3;
		else if (p & MK_CONTROL)
			return 5;
		else
			return 1;
	else
		return -1;
}

imeth	ofun	gSetToolBarMouseFunction(unsigned id, unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;
	int	j = pGetIndexFromID(self, id);

	if (i >= 0 && j >= 0) {
		org = iMouseFunction[j][i];
		iMouseFunction[j][i] = fun;
	}
	return (ofun) org;
}

cmeth	ofun	gSetMouseFunction(unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;

	if (i >= 0) {
		org = cMouseFunction[i];
		cMouseFunction[i] = fun;
	}
	return (ofun) org;
}

private	imeth	long	process_wm_rbuttondown(object	self, 
					      HWND	hwnd, 
					      UINT	mMsg, 
					      WPARAM	wParam, 
					      LPARAM	lParam)
{
	RECT	rect;
	int	i = get_index(wParam);
	int	pressed = which_tool(iv, lParam, &rect);
	
	if (i >= 0  &&  iMouseFunction[pressed][i]) {
		unsigned	id = pressed >= 0 ? iID[pressed] : 0;
		
		iMouseFunction[pressed][i](self, id, (unsigned) wParam);
	}
	return 0L;
}

cmeth	ifun	gSetAccessModeFunction(ifun f)
{
	ifun	org = cAccessMode;
	cAccessMode = f;
	return org;
}

imeth	char	*gGetBitmapFile(unsigned id)
{
	int	i;

	if ((i = pGetIndexFromID(self, id)) < 0)
		return "";
	return gStringValue(iBitmapFile[i]);
}







