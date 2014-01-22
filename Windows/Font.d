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


defclass  Font  {
	HGDIOBJ		iHFont;		/*  handle to font object	*/
	TEXTMETRIC	iTm;		/*  font metrics		*/

	iName;				/*  Font name, lazy execution.	*/
	int		iPointSize;	/*  Font Point Size, lazy exe.	*/
 class:
	HWND		cScreen;	/*  handle to entire screen	*/
 init:	class_init;
};


static	void	addToPointSizeCombo(object ctl, int nPointSize);

cvmeth	vNew(HGDIOBJ hFont)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	HDC	hdc = GetDC(cScreen);
	HGDIOBJ	org;

	org = SelectObject(hdc, hFont);
	GetTextMetrics(hdc, &iTm);
	SelectObject(hdc, org);
	ReleaseDC(cScreen, hdc);
	iHFont = hFont;
#if 0
	HC_NEW(iHFont, obj);
#endif
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
#if 0
	HC_DELETE(iHFont);
#endif
	if (iName)
		gDispose(iName);
	return gDispose(super);
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHFont;
}

imeth	gGetTM(TEXTMETRIC *tm)
{
	*tm = iTm;
	return self;
}

imeth	gSetVars(int row, int col, RECT *r, char *txt)
{
	SIZE	size;
	HDC	hdc = GetDC(cScreen);
	HGDIOBJ	org;

	r->top  = row;
	r->left = col;
	org = SelectObject(hdc, iHFont);
#ifdef	WIN32
	GetTextExtentPoint32(hdc, txt, strlen(txt), &size);
#else
	GetTextExtentPoint(hdc, txt, strlen(txt), &size);
#endif
	/* -1 causes the bottom of g's to be missing a bit.  ??  */
	r->bottom = r->top  + size.cy /* - 1 */;  
	r->right  = r->left + size.cx /* - 1 */;
	SelectObject(hdc, org);
	ReleaseDC(cScreen, hdc);
	return self;
}

imeth	long	gStrPixelWidth(char *txt)
{
	SIZE	size;
	HDC	hdc = GetDC(cScreen);
	HGDIOBJ	org;

	org = SelectObject(hdc, iHFont);
#ifdef	WIN32
	GetTextExtentPoint32(hdc, txt, strlen(txt), &size);
#else
	GetTextExtentPoint(hdc, txt, strlen(txt), &size);
#endif
	SelectObject(hdc, org);
	ReleaseDC(cScreen, hdc);
	return size.cx;
}

imeth	int	gLineHeight()
{
	return iTm.tmHeight + iTm.tmExternalLeading;
}

imeth	int	gTextHeight()
{
	return iTm.tmHeight;
}

imeth	int	gAveCharWidth()
{
	return iTm.tmAveCharWidth;
}

cmeth	int	gPixelHeight()
{
	HDC	hdc = GetDC(cScreen);
	int	r = GetDeviceCaps(hdc, LOGPIXELSY);
	ReleaseDC(cScreen, hdc);
	return r;
}

static	void	class_init()
{
	cScreen = GetDesktopWindow();
}

private	imeth	pLoadFontInfo(object self)
{
	LOGFONT lfStruct;
	char    buf[80]; // Yanghui
	
	if (GetObject(iHFont, sizeof(lfStruct), (void *) &lfStruct)) {
		// Yanghui:
		// iName = gNewWithStr(String, lfStruct.lfFaceName);

		strcpy(buf, lfStruct.lfFaceName);

		if(lfStruct.lfItalic)
			strcat(buf, " Italic");

		if(lfStruct.lfWeight == FW_BOLD)
			strcat(buf, " Bold");

		if(lfStruct.lfUnderline)
			strcat(buf, " Underline");

		if(lfStruct.lfStrikeOut)
			strcat(buf, " Strikeout");

		iName = gNewWithStr(String, buf);
		// Yanghui
	}
	
	iPointSize = MulDiv(iTm.tmHeight - iTm.tmInternalLeading, 72,
			    GetDeviceCaps(GetDC(cScreen), LOGPIXELSY));
	return self;
}

imeth	char	*gName()
{
	if (!iName)
		pLoadFontInfo(self);
	
	return iName ? gStringValue(iName) : "";
}

imeth	int	gPointSize()
{
	if (!iName)
		pLoadFontInfo(self);

	return iPointSize;
}

static	int	CALLBACK	fontNameEnumFunc(ENUMLOGFONT *lpLogFont,
						 NEWTEXTMETRIC *lpTextMetric,
						 int nFontType,
						 LPARAM lpData)
{
	char	*p;
	object	ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, *((HWND *) lpData));

	if (!ctl)
		return 0;
	
	p = lpLogFont->elfLogFont.lfFaceName;
	if (IsBadReadPtr(lpLogFont,5))
		return 0;
	while (*p) {
		if (*p < ' ' || *p > 128)
			return 0;
		p++;
	}
	
	/* Add this name to the combo box. */
	gAddOption(ctl, lpLogFont->elfLogFont.lfFaceName);
	
	/* Keep on going... */
	return 1;
}

cmeth	gFillFontNameCombo(char *fname, int ptSize, object nmctl, object psctl, object dlg)
{
	HWND		hwndDlg = gHandle(dlg);
	HDC		hDC;
	HWND		hwndCombo;
	TCHAR		szName1[LF_FACESIZE];
	TCHAR		szName2[LF_FACESIZE];
	char		*pszName;
	char		*pszNameLast;
	char		*pszNameTemp;
	int		iIndex;
	int		iItems;
	FONTENUMPROC	lpFontEnumProc;

	gRemoveAll(nmctl);
	gAlphabetize(nmctl);
	
	hwndCombo = gHandle(nmctl);
	
	if (hDC = GetDC(cScreen)) {
		lpFontEnumProc = (FONTENUMPROC) MakeProcInstance((FARPROC) fontNameEnumFunc, gInstance(Application));
#ifdef	WIN32
		EnumFontFamilies(hDC, NULL, (FONTENUMPROC) lpFontEnumProc, (LPARAM) &hwndCombo);
#else
		EnumFontFamilies(hDC, NULL, (FONTENUMPROC) lpFontEnumProc, (LPSTR) &hwndCombo);
#endif
		ReleaseDC(cScreen, hDC);
	}
	/* Strip out any duplicate names in the combobox.  This routine relies
	on the items being sorted first. */
	iItems = gSize(nmctl);
	*szName1 = *szName2 = '\0';
	pszName = szName1;
	pszNameLast = szName2;
	for (iIndex = 0; iIndex < iItems;) {
		/* Get the text of the next item. */
		strcpy(pszName, gStringValue(gValueAt(nmctl, iIndex)));
		/* If it matches the previous item, delete it.  Otherwise, flip the
		buffers to save the current items text and go on to the next
		item. */
		if (!strcmp(pszName, pszNameLast)) {
			gRemoveInt(nmctl, iIndex);
			iItems--;
		} else {
			pszNameTemp = pszNameLast;
			pszNameLast = pszName;
			pszName = pszNameTemp;
			iIndex++;
		}
	}
	/* Initialize the font fields.  The order the fields are set is
	important, because setting the face name clears out the point size
	combo. */
	if (fname && *fname)
		gSetStringValue(nmctl, fname);
	if (gShortValue(nmctl) < 0)
		gSetShortValue(nmctl, 0);
	
	return gFillPointSizeCombo(self, gStringValue(nmctl), ptSize, psctl);
}

static	int	CALLBACK	pointSizeEnumFunc(LOGFONT	*lpLogFont,
						  NEWTEXTMETRIC	*lpTextMetric,
						  DWORD		nFontType,
						  LPARAM 	lpData)
{
	object	ctl = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, *((HWND *) lpData));
	int	nPointSize;
	char	commonPointSize[] = {8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72, 0};


	if (!ctl)
		return 0;
	
	if (nFontType == RASTER_FONTTYPE) {
                /* Convert the pixels to point size.  Note that because of the
		definition of the tmHeight field, the tmInternalLeading has to be
		subtracted from it before converting to get the proper font point
		size.  This is done automatically by the Windows CreateFont call
		if you pass in a nHeight parameter that is negative, so be aware
		of this when doing the reverse calculation to create a font of
		the proper height! */
		nPointSize = MulDiv(lpTextMetric->tmHeight - lpTextMetric->tmInternalLeading, 72,
				    GetDeviceCaps(GetDC(cScreen), LOGPIXELSY));
		addToPointSizeCombo(ctl, nPointSize);
	} else {
		int i = 0;
		/* For scalable (TrueType, ATM or vector) fonts, add the common
		point sizes.  This list was pulled out of the ( commdlg.dll Font
		dialog. */
		while (commonPointSize[i]) {
			addToPointSizeCombo(ctl, commonPointSize[i]);
			i++;
		}
	}
	
	/* Keep on going... */
	return 1;
}

cmeth	gFillPointSizeCombo(char *fname, int ptSize, object ctl)
{
	HDC	hDC;
	HWND	hwndCombo = gHandle(ctl);
	char	buf[256];

	sprintf(buf, "%d", ptSize);

	gRemoveAll(ctl);
	
	if (*fname && (hDC = GetDC(cScreen))) {
#ifdef	WIN32
		EnumFonts(hDC, fname, (FONTENUMPROC) pointSizeEnumFunc, (LPARAM) &hwndCombo);
#else
		EnumFonts(hDC, fname, (FONTENUMPROC) pointSizeEnumFunc, (LPSTR) &hwndCombo);
#endif
		ReleaseDC(cScreen, hDC);
	}
	/* Select a default one.  This is the point size that is currently
	selected if the face name is the current one, or else it is the first
	point size in the list. */
	if (ptSize)
		gSetStringValue(ctl, buf);
	if (gShortValue(ctl) < 0)
		gSetShortValue(ctl, 0);
	
	return self;
}

static	void	addToPointSizeCombo(object ctl, int nPointSize)
{
	TCHAR szPointSize[31];
	int nPoints2;
	int iIndex;
	int iIndexAdd;
	int iItems = gSize(ctl);
	
	for (iIndex = 0, iIndexAdd = -1; iIndex < iItems; iIndex++) {
		nPoints2 = atoi(gStringValue(gValueAt(ctl, iIndex)));
		if (nPoints2 == nPointSize)
			/* A duplicate was found.  Skip this one. */
			return;
		else if (nPoints2 > nPointSize) {
			iIndexAdd = iIndex;
			break;
		}
	}
	/* Add this point size to the combo box. */
	sprintf(szPointSize, "%d", nPointSize);
	gAddOption(ctl, szPointSize);
}










