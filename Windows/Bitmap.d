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




defclass  Bitmap  {
	iWind;				/* Parent window.						*/
	int		iStartX;	/* Starting x postion of bitmap					*/
	int		iStartY;	/* Starting y postion of bitmap					*/
	int		iSizeX;		/* Bitmap Width							*/
	int		iSizeY;		/* Bitmap Height						*/
	int		iOrgX;		/* Bitmap origin, usually zero					*/
	int		iOrgY;		/* Bitmap origin, usually zero					*/
	UINT		iBmpID;		/* ID of bitmap resource.					*/
	HBITMAP		iBmp;		/* Bitmap placed on a window permanently.			*/
	int		iBmpMode;	/* 0 = no bitmap, 1 = positioned, 2 = centered, 3 = tiled.	*/
	int		iCreated;	/* Has the bitmap been created?					*/
	HPALETTE	iHPal;		/* Handle to the palette.					*/
	HDC		iHdc;		/* Handle to the window device context.				*/
};
 
private	imeth	pCreateBitmap(object self);

static	HPALETTE	createDIBPalette (LPBITMAPINFO lpbmi, LPINT lpiNumColors);
static	HBITMAP		loadResourceBitmap(HINSTANCE hInstance, UINT bmpID, HPALETTE FAR* lphPalette);

static	HBITMAP loadResourceBitmapForPrinting(HINSTANCE hInstance, UINT bmpID, HDC hdcPrint);

cmeth	gNewBitmap(UINT bmpID, int mode, int y, int x, object wind)
{
	object	obj = gNew(super);
	accessIVsOf(obj);

	iWind = wind;
	iBmpID = bmpID;
	iStartX = x;
	iStartY = y;
	if ((iBmpMode = mode) != BMP_POSITION) {
		iStartX = 0;
		iStartY = 0;
	}

	gScaleToPixels(Application, &iStartY, &iStartX, gGetFont(wind));
	
	return obj;
}

////////////////////////////////////////////////////////////
// The position x and y are not scaled to the font of wind
////////////////////////////////////////////////////////////
cmeth	gNewBitmap2(UINT bmpID, int mode, int y, int x, object wind)
{
	object	obj = gNew(super);
	accessIVsOf(obj);

	iWind = wind;
	iBmpID = bmpID;
	iStartX = x;
	iStartY = y;
	if ((iBmpMode = mode) != BMP_POSITION) {
		iStartX = 0;
		iStartY = 0;
	}

	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	if (iBmp)
		DeleteObject(iBmp);
	if (iHPal)
		DeleteObject(iHPal);
	if (iHdc)
		ReleaseDC(gHandle(iWind), iHdc);

	return gDispose(super);
}

private	imeth	pDrawSingleBitmapAt(object self, int y, int x, int stretch)
{
	HDC       hdcMem;
	HPALETTE  hpal;
	HDC       hdc;
	HWND      hwnd;
	int       xOffset, yOffset;

	xOffset = 0;
	yOffset = 0;
	
	hdc = GetDC(hwnd=gHandle(iWind));
	hdcMem = CreateCompatibleDC(hdc);

	SelectPalette(hdc, iHPal, FALSE);
	RealizePalette(hdc);
	SelectPalette(hdcMem, iHPal, FALSE);
	RealizePalette(hdcMem);
	
	SelectObject(hdcMem, iBmp);
	SetMapMode(hdcMem, GetMapMode(hdc));

	if(iWind)
		gGetScrollPosition(iWind, &yOffset, &xOffset);

	if (!stretch) {
		// BitBlt(hdc, x, y, iSizeX, iSizeY, hdcMem, iOrgX, iOrgY, SRCCOPY);
		BitBlt(hdc, x-xOffset, y-yOffset, iSizeX, iSizeY, hdcMem, iOrgX, iOrgY, SRCCOPY);
	}
	else {
		int	wh, ww;

		gGetSize(iWind, &wh, &ww);
		// StretchBlt(hdc, x, y, ww, wh, hdcMem, iOrgX, iOrgY, iSizeX, iSizeY, SRCCOPY);
		StretchBlt(hdc, x-xOffset, y-yOffset, ww, wh, hdcMem, iOrgX, iOrgY, iSizeX, iSizeY, SRCCOPY);
	}

	DeleteDC(hdcMem);
	ReleaseDC(hwnd, hdc);

	return self;
}


private	imeth	pDrawSingleBitmapAt2(object self, int x, int y, int stretch, 
		double dCxScale2, double dCyScale2, PRECT pMFMarginRect)
{
	HDC       hdcMem;
	HPALETTE  hpal;
	HDC       hdc;
	HWND      hwnd;
	int       xClient, yClient, xOffset, yOffset;

	hdc = GetDC(hwnd=gHandle(iWind));
	hdcMem = CreateCompatibleDC(hdc);

	SelectPalette(hdc, iHPal, FALSE);
	RealizePalette(hdc);
	SelectPalette(hdcMem, iHPal, FALSE);
	RealizePalette(hdcMem);
	
	SelectObject(hdcMem, iBmp);
	SetMapMode(hdcMem, GetMapMode(hdc));

	xOffset = 0;
	yOffset = 0;

	if(iWind)
		gGetScrollPosition(iWind, &yOffset, &xOffset);

	xClient = -xOffset + (x + pMFMarginRect->left)*dCxScale2 + 0.5;
	yClient = -yOffset + (y + pMFMarginRect->top )*dCyScale2 + 0.5;
	if (!stretch) {
		// BitBlt(hdc, x, y, iSizeX, iSizeY, hdcMem, iOrgX, iOrgY, SRCCOPY);
		StretchBlt(hdc, xClient, yClient, (int)(iSizeX*dCxScale2 + 0.5), (int)(iSizeY*dCyScale2 + 0.5), 
					hdcMem, iOrgX, iOrgY, iSizeX, iSizeY, SRCCOPY);
	}
	else {
		int	wh, ww;
		gGetSize(iWind, &wh, &ww);
		StretchBlt(hdc, xClient, yClient, (int)(ww*dCxScale2 + 0.5), (int)(wh*dCyScale2 + 0.5), 
					hdcMem, iOrgX, iOrgY, iSizeX, iSizeY, SRCCOPY);
	}

	DeleteDC(hdcMem);
	ReleaseDC(hwnd, hdc);

	return self;
}


imeth	gDrawBitmap()	// This function assumes that the call to BeginPaint has already been made.
{
	int	wh, ww;
	int	pm = gSetScalingMode(Application, SM_PIXELS);
	
	if (!iCreated) {
		iCreated = 1;
		pCreateBitmap(self);
	}

	if( iWind && !gGetScaleFlg(iWind) ) {  // the scaling flag is not set (for Integra)
		if (iBmpMode == BMP_POSITION)
			pDrawSingleBitmapAt(self, iStartY, iStartX, 0);
		else if (iBmpMode == BMP_TILED) {
			int	numacross, numdown;
			int	i, j;
			int	xpos, ypos;
		
			gGetSize(iWind, &wh, &ww);
			numacross = (wh / iSizeY) + 1;
			numdown = (ww / iSizeX) + 1;

			for (i = 0, xpos = 0; i < numdown; i++, xpos += iSizeX)
				for (j = 0, ypos = 0; j < numacross; j++, ypos += iSizeY)
					pDrawSingleBitmapAt(self, ypos, xpos, 0);
		} else if (iBmpMode == BMP_CENTER) {
			gGetSize(iWind, &wh, &ww);
			pDrawSingleBitmapAt(self, (wh - iSizeY) / 2, (ww - iSizeX) / 2, 0);
		} else if (iBmpMode == BMP_FILLWIND) {
			pDrawSingleBitmapAt(self, 0, 0, 1);
		}
	}
	else if(iWind) {  // the scaling flag is set 
		double  controlScaleFactor;
		double  dCxScale, dCyScale, dCxScale2, dCyScale2;
		RECT    MFMarginRect;
		
		controlScaleFactor = gGetControlScale(iWind);
		gGetMFCxCyScale(iWind, &dCxScale, &dCyScale);
		gGetMFMargins(iWind, &MFMarginRect);

		dCxScale2 = controlScaleFactor*dCxScale;
		dCyScale2 = controlScaleFactor*dCyScale;

		if (iBmpMode == BMP_POSITION)
			pDrawSingleBitmapAt2(self, iStartX, iStartY, 0, dCxScale2, dCyScale2, &MFMarginRect);
		else if (iBmpMode == BMP_TILED) {
			int	numacross, numdown;
			int	i, j;
			int	xpos, ypos;
		
			gGetSize(iWind, &wh, &ww);
			numacross = (wh / iSizeY) + 1;
			numdown = (ww / iSizeX) + 1;

			for (i = 0, xpos = 0; i < numdown; i++, xpos += iSizeX)
				for (j = 0, ypos = 0; j < numacross; j++, ypos += iSizeY)
					pDrawSingleBitmapAt2(self, xpos, ypos, 0, dCxScale2, dCyScale2, &MFMarginRect);
		} else if (iBmpMode == BMP_CENTER) {
			gGetSize(iWind, &wh, &ww);
			pDrawSingleBitmapAt2(self, (ww - iSizeX) / 2, (wh - iSizeY) / 2, 0, dCxScale2, dCyScale2, &MFMarginRect);
		} else if (iBmpMode == BMP_FILLWIND) {
			pDrawSingleBitmapAt2(self, 0, 0, 1, dCxScale2, dCyScale2, &MFMarginRect);
		}
	}
	
	gSetScalingMode(Application, pm);
	return self;
}

private	imeth	pCreateBitmap(object self)
{
	HANDLE	instance = gInstance(Application);
	BITMAP	bm;
	POINT	ptSize, ptOrg;
	
	iBmp = loadResourceBitmap(instance, iBmpID, &iHPal);
	iHdc = GetDC(gHandle(iWind));
	
	GetObject(iBmp, sizeof(BITMAP), (LPVOID) &bm);
	ptSize.x = bm.bmWidth;
	ptSize.y = bm.bmHeight;
	DPtoLP(iHdc, &ptSize, 1);

	ptOrg.x = 0;
	ptOrg.y = 0;
	DPtoLP(iHdc, &ptOrg, 1);

	iSizeX = ptSize.x;
	iSizeY = ptSize.y;
	iOrgX = ptOrg.x;
	iOrgY = ptOrg.y;
	
	return self;
}

static	HPALETTE createDIBPalette (LPBITMAPINFO lpbmi, LPINT lpiNumColors)
{ 
	LPBITMAPINFOHEADER	lpbi;
	LPLOGPALETTE		lpPal;
	HANDLE			hLogPal;
	HPALETTE		hPal = (HPALETTE) 0;
	int			i;

	lpbi = (LPBITMAPINFOHEADER)lpbmi;
	if (lpbi->biBitCount <= 8)
		*lpiNumColors = (1 << lpbi->biBitCount);
	else
		*lpiNumColors = 0;  // No palette needed for 24 BPP DIB

	if (lpbi->biClrUsed > 0)
		*lpiNumColors = lpbi->biClrUsed;  // Use biClrUsed

	if (*lpiNumColors) {
		hLogPal = GlobalAlloc (GHND, sizeof (LOGPALETTE) +
				       sizeof (PALETTEENTRY) * (*lpiNumColors));
		lpPal = (LPLOGPALETTE) GlobalLock (hLogPal);
		lpPal->palVersion    = 0x300;
		lpPal->palNumEntries = *lpiNumColors;

		for (i = 0;  i < *lpiNumColors;  i++) {
			lpPal->palPalEntry[i].peRed   = lpbmi->bmiColors[i].rgbRed;
			lpPal->palPalEntry[i].peGreen = lpbmi->bmiColors[i].rgbGreen;
			lpPal->palPalEntry[i].peBlue  = lpbmi->bmiColors[i].rgbBlue;
			lpPal->palPalEntry[i].peFlags = 0;
		}
		hPal = CreatePalette (lpPal);

		GlobalUnlock (hLogPal);
		GlobalFree   (hLogPal);
	}

	return hPal;
} 

static	HBITMAP loadResourceBitmap(HINSTANCE hInstance, UINT bmpID, HPALETTE FAR* lphPalette)
{ 
	HRSRC			hRsrc;
	HGLOBAL			hGlobal;
	HBITMAP			hBitmapFinal = (HBITMAP) 0;
	LPBITMAPINFOHEADER	lpbi;
	HDC			hdc;
	int			numColors;

	if (hRsrc = FindResource(hInstance, MAKEINTRESOURCE(bmpID), RT_BITMAP)) {
		hGlobal = LoadResource(hInstance, hRsrc);
		lpbi = (LPBITMAPINFOHEADER)LockResource(hGlobal);

		hdc = GetDC((HANDLE)0);
		*lphPalette =  createDIBPalette ((LPBITMAPINFO)lpbi, &numColors);
		if (*lphPalette) {
			SelectPalette(hdc,*lphPalette,FALSE);
			RealizePalette(hdc);
		}

		hBitmapFinal = CreateDIBitmap(hdc,
					      (LPBITMAPINFOHEADER)lpbi,
					      (LONG)CBM_INIT,
					      (LPSTR)lpbi + lpbi->biSize + numColors *
					      sizeof(RGBQUAD), 
					      (LPBITMAPINFO)lpbi,
					      DIB_RGB_COLORS );

		ReleaseDC((HANDLE)0, hdc);
#ifndef WIN32
		UnlockResource(hGlobal);
#endif
		FreeResource(hGlobal);
	}
	return (hBitmapFinal);
} 


imeth	gPrintCtlScreen(object printerObj, double dScaleX, double dScaleY, int nViewOffsetX, int nViewOffsetY)
{
	HDC      hdcPrinter;
	int	     pm, wh, ww;
	HBITMAP  hbmp;
	BITMAP   bm;

	int      xClient, yClient, cxPage, cyPage, cxBitmap, cyBitmap;
	int      nPhysicalWidth, nPhysicalHeight, nPrinterOffsetX, nPrinterOffsetY;
	
	if(!printerObj)    // validate the printerObj
		return NULL;

	if( !( hdcPrinter=gHandle(printerObj) ) )  // get and validate the print DC
		return NULL;

	if(dScaleX<=0 || dScaleY<=0)  // validate the view to printer scaling factors
		return NULL;

	if( !( RC_STRETCHBLT & GetDeviceCaps(hdcPrinter, RASTERCAPS) ) )
		return NULL;

	gCLDGetPhysicalParameters(printerObj, &nPhysicalWidth, &nPhysicalHeight, &nPrinterOffsetX, &nPrinterOffsetY);

	cxPage = GetDeviceCaps (hdcPrinter, HORZRES) ;
	cyPage = GetDeviceCaps (hdcPrinter, VERTRES) ;

	hbmp = LoadBitmap(gInstance(Application), MAKEINTRESOURCE(iBmpID));  
	                                                       
	if (!hbmp)
		return NULL;

	GetObject(hbmp, sizeof(BITMAP), (LPVOID) &bm);
	cxBitmap = bm.bmWidth;
	cyBitmap = bm.bmHeight;

	pm = gSetScalingMode(Application, SM_PIXELS);

	if( iWind ) {  
		if (iBmpMode == BMP_POSITION) {
			// scale the rect from view DC to the printer DC
			// xClient = (iStartX + nViewOffsetX) * dScaleX - nPrinterOffsetX;
			// yClient = (iStartY + nViewOffsetY) * dScaleY - nPrinterOffsetY;

			xClient = iStartX * GetDeviceCaps(hdcPrinter, LOGPIXELSX)/96 - nPrinterOffsetX;  // FSI
			yClient = iStartY * GetDeviceCaps(hdcPrinter, LOGPIXELSY)/96 - nPrinterOffsetY;  // FSI

			gPrintSingleBitmapAt(self, hdcPrinter, dScaleX, dScaleY, cxPage, cyPage, hbmp,
									xClient, yClient, cxBitmap, cyBitmap, 0);
		}
		else if (iBmpMode == BMP_TILED) {
			int	numacross, numdown;
			int	i, j;
			int	xpos, ypos;
		
			gGetSize(iWind, &wh, &ww);
			numacross = (wh / iSizeY) + 1;
			numdown = (ww / iSizeX) + 1;

			for (i = 0, xpos = 0; i < numdown; i++, xpos += iSizeX)
				for (j = 0, ypos = 0; j < numacross; j++, ypos += iSizeY) {
					xClient = (xpos + nViewOffsetX) * dScaleX - nPrinterOffsetX;
					yClient = (ypos + nViewOffsetY) * dScaleY - nPrinterOffsetY;
					gPrintSingleBitmapAt(self, hdcPrinter, dScaleX, dScaleY, cxPage, cyPage, hbmp,
						xClient, yClient, cxBitmap, cyBitmap, 0);
				}
		} 
		else if (iBmpMode == BMP_CENTER) {
			gGetSize(iWind, &wh, &ww);
			xClient = ((ww-cxBitmap)/2.0 + nViewOffsetX) * dScaleX - nPrinterOffsetX;
			yClient = ((wh-cyBitmap)/2.0 + nViewOffsetY) * dScaleY - nPrinterOffsetY;
			gPrintSingleBitmapAt(self, hdcPrinter, dScaleX, dScaleY, cxPage, cyPage, hbmp,
				xClient, yClient, cxBitmap, cyBitmap, 0);
		} 
		else if (iBmpMode == BMP_FILLWIND) {
			gPrintSingleBitmapAt(self, hdcPrinter, dScaleX, dScaleY, cxPage, cyPage, hbmp, 
				0, 0, cxBitmap, cyBitmap, 1);
		}
	}
	
	gSetScalingMode(Application, pm);

	DeleteObject( (HGDIOBJ)hbmp);

	return self;
}



imeth gPrintSingleBitmapAt(HDC hdcPrinter, double dScaleX, double dScaleY, int cxPage, int cyPage,
                           HBITMAP hbmp, int xClient, int yClient, int cxBitmap, int cyBitmap, int stretch)
{
	HDC	 hdcMem=(HDC)0;

	if(!hdcPrinter || !hbmp)
		return NULL;

	hdcMem = CreateCompatibleDC(hdcPrinter);
	SelectObject(hdcMem, hbmp);

	SetMapMode(hdcMem, MM_TEXT);      
	// SetMapMode(hdcMem, GetMapMode(hdcPrinter));

	if (!stretch) {
		int cxSize, cySize;
		cxSize = (int)(cxBitmap * 0.0112 * GetDeviceCaps(hdcPrinter, LOGPIXELSX) + 0.5);  // FSI
		cySize = (int)(cyBitmap * 0.0088 * GetDeviceCaps(hdcPrinter, LOGPIXELSY) + 0.5);  // FSI

		// StretchBlt (hdcPrinter, xClient, yClient, 
		//            min(cxPage, (int)(cxBitmap*dScaleX+0.5)), min(cyPage, (int)(cyBitmap*dScaleY+0.5)), 
		//            hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY) ;

		StretchBlt (hdcPrinter, xClient, yClient, 
		            min(cxPage, cxSize), min(cyPage, cySize), hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY) ;
	}
	else if (iWind) {  // stretched
		int	wh, ww;
		
		gGetSize(iWind, &wh, &ww);
		wh = (int)(wh*dScaleY + 0.5);
		ww = (int)(ww*dScaleX + 0.5);

		StretchBlt(hdcPrinter, xClient, yClient, ww, wh, hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY);
	}

	DeleteDC(hdcMem);

	return self;
}


