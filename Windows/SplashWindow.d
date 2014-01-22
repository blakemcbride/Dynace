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




defclass SplashWindow : PopupWindow {
	HBITMAP	iBmp;
	HDC	iHdc;
	int	iStartX;
	int	iStartY;
	int	iSizeX;
	int	iSizeY;
	int	iOrgX;
	int	iOrgY;
	int	iNumLines;
};

private imeth	pInitializeSplashWindow(object self, unsigned bmpID);

private	imeth	long	process_wm_paint(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam);

cmeth	gNewSplashWindowWithText(unsigned bmpID, char *text)
{
	object	obj = vNew(super, "", 10, 40);
	int	len = strlen(text);
	char	*buf = gGetBuf(Application);
	int	i, j;
	accessIVsOf(obj);

	*buf = ' ';

	iNumLines = 1;
	for (i = 0, j = 1; i < len ; i++) {
		buf[j++] = text[i];
		if (text[i] == '\n') {
			buf[j++] = ' ';
			iNumLines++;
		}
	}
	buf[j] = '\0';
		
	pInitializeSplashWindow(obj, bmpID);

	gShow(obj);

	gPuts(obj, buf);
	
	return obj;
}

cmeth	gNewSplashWindow(unsigned bmpID)
{
	object	obj = vNew(super, "", 10, 40);

	pInitializeSplashWindow(obj, bmpID);
	
	gShow(obj);

	return obj;
}

private imeth	pInitializeSplashWindow(object self, unsigned bmpID)
{
	BITMAP	bm;
	DWORD	dwSize;
	POINT	ptSize, ptOrg;
	int	prev = gSetScalingMode(Application, SM_PIXELS);
	int	height = GetSystemMetrics(SM_CYSCREEN);
	int	width = GetSystemMetrics(SM_CXSCREEN);

	gWaitCursor(self, 1);
	gSetStyle(self, WS_VISIBLE | WS_BORDER);
	gAddHandlerAfter(self, (unsigned) WM_PAINT, process_wm_paint);
//	gSetZOrder(self, HWND_TOPMOST);
	gSetZOrder(self, HWND_TOP);

	iBmp = LoadBitmap(gInstance(Application), MAKEINTRESOURCE(bmpID));
	iHdc = GetDC(gHandle(self));

	GetObject(iBmp, sizeof(BITMAP), (LPVOID) &bm);
	ptSize.x = bm.bmWidth;
	ptSize.y = bm.bmHeight;
	DPtoLP(iHdc, &ptSize, 1);

	iStartX = (width - bm.bmWidth) / 2;
	iStartY = (height - bm.bmHeight) / 2;

	if (iNumLines) {
		object	font = gGetFont(Application);
		int	lh = gLineHeight(font);
		int	ht = lh * iNumLines + 2;
		object	backbrush = vNew(StockBrush, BLACK_BRUSH);
		object	textbrush = vNew(StockBrush, WHITE_BRUSH);

		gDispose(font);
		
		gBackBrush(self, backbrush);
		gTextBrush(self, textbrush);

		iStartY += ht / 2;
#ifdef ADD_BORDER	
		gSetSize(self, bm.bmHeight + (ht + 2), bm.bmWidth + 2);
		gSetPosition(self, iStartY - (ht + 1), iStartX - 1);
#else
		gSetSize(self, bm.bmHeight + ht, bm.bmWidth);
		gSetPosition(self, iStartY - ht, iStartX);
#endif
	} else {
#ifdef ADD_BORDER
		gSetSize(self, bm.bmHeight + 2, bm.bmWidth + 2);
		gSetPosition(self, iStartY - 1, iStartX - 1);
#else
		gSetSize(self, bm.bmHeight, bm.bmWidth);
		gSetPosition(self, iStartY, iStartX);
#endif
	}

	gSetScalingMode(Application, prev);

	ptOrg.x = 0;
	ptOrg.y = 0;
	DPtoLP(iHdc, &ptOrg, 1);

	iSizeX = ptSize.x;
	iSizeY = ptSize.y;
	iOrgX = ptOrg.x;
	iOrgY = ptOrg.y;

	return self;
}

imeth	gDeepDispose, gDispose ()
{
	gSetState(self, SW_HIDE);
			
	if (iBmp)
		DeleteObject(iBmp);

	return gDispose(super);
}

private	imeth	long	process_wm_paint(object	self, 
					 HWND	hwnd, 
					 UINT	mMsg, 
					 WPARAM	wParam, 
					 LPARAM	lParam)
{
	PAINTSTRUCT	ps;
	HDC		hdcMem;

	BeginPaint(gHandle(self), &ps);

	SetBkMode(ps.hdc, TRANSPARENT);
		
	hdcMem = CreateCompatibleDC(iHdc);
	SelectObject(hdcMem, iBmp);
	SetMapMode(hdcMem, GetMapMode(iHdc));

	BitBlt(iHdc, iStartX, iStartY, iSizeX, iSizeY, hdcMem, iOrgX, iOrgY, SRCCOPY);

	DeleteDC(hdcMem);

	EndPaint(gHandle(self), &ps);

	return 0L;
}










