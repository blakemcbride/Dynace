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

#include <windows.h>
#include <commdlg.h>
#include "logfile.h"


defclass  FontDialog : CommonDialog  {
	CHOOSEFONT	iCF;
	LOGFONT		iFont;
	int		iRet;
};


// Yanghui:
static	void	removeLast(char *v);
static	char	*getLast(char *v);
// Yanghui

cvmeth	vNew(parent)
{
	int	i;
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iCF.lStructSize = sizeof(CHOOSEFONT);
	iCF.hwndOwner = parent ? gHandle(parent) : (HWND) 0;
	iCF.lpLogFont = &iFont;
	iCF.Flags = CF_SCREENFONTS | CF_EFFECTS;
	iCF.rgbColors = RGB(0, 0, 0);
	return obj;
}


// Yanghui:
cmeth	gNewWithInitToLogFontStruct(parent, char * fontName, short fontPointSize)
// fontName includes font FaceName [,Bold, Italic, Underline, Strikeout]
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	HWND hWnd; 
	HDC hDC;

	memset(&iFont, 0, sizeof iFont);

	if(parent) 
		hWnd = gHandle(parent);

	if(fontPointSize > 0) {
		if(hWnd) {
			hDC = GetDC(hWnd);
			iFont.lfHeight = -MulDiv(fontPointSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
			ReleaseDC(hWnd, hDC);
		}
		else
			iFont.lfHeight = -MulDiv(fontPointSize, gPixelHeight(CLASS), 72);
	}

	// iFont.lfCharSet = SYMBOL_CHARSET; // delete this line  please

	if(fontName) {
		char buf[80];
		strcpy(buf, fontName);

		while (buf[0]) {
			char	*p;
		
			p = getLast(buf);
			if (!stricmp(p, "bold"))
				iFont.lfWeight = FW_BOLD;
			else if (!stricmp(p, "italic"))
				iFont.lfItalic = TRUE;
			else if (!stricmp(p, "underline"))
				iFont.lfUnderline = TRUE;
			else if (!stricmp(p, "strikeout"))
				iFont.lfStrikeOut = TRUE;
			else
				break;
			removeLast(buf);
		}
		
		strncpy(iFont.lfFaceName, buf, LF_FACESIZE);
		iFont.lfFaceName[LF_FACESIZE-1] = '\0';
	}

	iCF.lStructSize = sizeof(CHOOSEFONT);
	iCF.hwndOwner = parent ? gHandle(parent) : (HWND) 0;
	iCF.lpLogFont = &iFont;

	iCF.Flags = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS | CF_EFFECTS;
	// iCF.Flags = CF_SCREENFONTS | CF_EFFECTS;     

	iCF.rgbColors = RGB(0, 0, 0);

	return obj;
}



static	char	*getLast(char *v)
{
	int	i = strlen(v) - 1;
	for ( ; i > 0  &&  v[i] != ' ' ; i--);
	return i ? v + i + 1 : v;
}


static	void	removeLast(char *v)
{
	int	i = strlen(v) - 1;
	for ( ; i > 0  &&  v[i] != ' ' ; i--);
	if (i >= 0)
		v[i] = '\0';
}


cmeth	gNewWithInitToLogFontStructAndTextColor(parent, const char * fontName, short fontPointSize, const char *textColor)
// fontName includes font FaceName [,Bold, Italic, Underline, Strikeout]
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	HWND hWnd; 
	HDC hDC;

	memset(&iFont, 0, sizeof iFont);

	if(parent) 
		hWnd = gHandle(parent);

	if(fontPointSize > 0) {
		if(hWnd) {
			hDC = GetDC(hWnd);
			iFont.lfHeight = -MulDiv(fontPointSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
			ReleaseDC(hWnd, hDC);
		}
		else
			iFont.lfHeight = -MulDiv(fontPointSize, gPixelHeight(CLASS), 72);
	}

	if(fontName) {
		char buf[80];
		strcpy(buf, fontName);

		while (buf[0]) {
			char	*p;
		
			p = getLast(buf);
			if (!stricmp(p, "bold"))
				iFont.lfWeight = FW_BOLD;
			else if (!stricmp(p, "italic"))
				iFont.lfItalic = TRUE;
			else if (!stricmp(p, "underline"))
				iFont.lfUnderline = TRUE;
			else if (!stricmp(p, "strikeout"))
				iFont.lfStrikeOut = TRUE;
			else
				break;
			removeLast(buf);
		}
		
		strncpy(iFont.lfFaceName, buf, LF_FACESIZE);
		iFont.lfFaceName[LF_FACESIZE-1] = '\0';
	}

	if (!stricmp(textColor, "Aqua"))
		iCF.rgbColors = RGB(0, 255, 255);

	else if (!stricmp(textColor, "Black"))
		iCF.rgbColors = RGB(0, 0, 0);

	else if (!stricmp(textColor, "Blue"))
		iCF.rgbColors = RGB(0, 0, 255);

	else if (!stricmp(textColor, "Fuchsia"))
		iCF.rgbColors = RGB(255, 0, 255);

	else if (!stricmp(textColor, "Gray"))
		iCF.rgbColors = RGB(128, 128, 128);

	else if (!stricmp(textColor, "Green"))
		iCF.rgbColors = RGB(0, 128, 0);

	else if (!stricmp(textColor, "Lime"))
		iCF.rgbColors = RGB(0, 255, 0);

	else if (!stricmp(textColor, "Magenta"))
		iCF.rgbColors = RGB(255, 0, 255);

	else if (!stricmp(textColor, "Maroon"))
		iCF.rgbColors = RGB(128, 0, 0);

	else if (!stricmp(textColor, "Navy"))
		iCF.rgbColors = RGB(0, 0, 128);

	else if (!stricmp(textColor, "Olive"))
		iCF.rgbColors = RGB(128, 128, 0);

	else if (!stricmp(textColor, "Orange"))
		iCF.rgbColors = RGB(255, 128, 0);

	else if (!stricmp(textColor, "Purple"))
		iCF.rgbColors = RGB(128, 0, 128);

	else if (!stricmp(textColor, "Red"))
		iCF.rgbColors = RGB(255, 0, 0);

	else if (!stricmp(textColor, "Silver"))
		iCF.rgbColors = RGB(192, 192, 192);

	else if (!stricmp(textColor, "Teal"))
		iCF.rgbColors = RGB(0, 128, 128);

	else if (!stricmp(textColor, "Violet"))
		iCF.rgbColors = RGB(128, 0, 128);

	else if (!stricmp(textColor, "White"))
		iCF.rgbColors = RGB(255, 255, 255);

	else if (!stricmp(textColor, "Yellow"))
		iCF.rgbColors = RGB(255, 255, 0);

	else    // default to black  
		iCF.rgbColors = RGB(0, 0, 0);

	iCF.lStructSize = sizeof(CHOOSEFONT);
	iCF.hwndOwner = parent ? gHandle(parent) : (HWND) 0;
	iCF.lpLogFont = &iFont;

	iCF.Flags = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS | CF_EFFECTS;

	return obj;
}
// Yanghui


imeth	int	gPerform()
{
	return iRet = ChooseFont(&iCF);
}


/*
imeth	int	gPerform()
{
	char        charTmp[80];
	LOGFONT		logfont;

	logfont = *(iCF.lpLogFont);
	sprintf(charTmp, "FaceName:%s  CharSet:%d", logfont.lfFaceName, logfont.lfCharSet); 
	MessageBox(NULL, charTmp, "Before gPerform", MB_OK);
	
	iRet = ChooseFont(&iCF);

	logfont = *(iCF.lpLogFont);
	sprintf(charTmp, "FaceName:%s  CharSet:%d", logfont.lfFaceName, logfont.lfCharSet); 
	MessageBox(NULL, charTmp, "After gPerform", MB_OK);

	return iRet;
}
*/

imeth	COLORREF	gGetColor()
{
	return iCF.rgbColors;
}

imeth	gSetColor(COLORREF color)
{
	iCF.rgbColors = color;
	return self;
}

imeth	gSetFlags(DWORD flags)
{
	iCF.Flags = flags;
	return self;
}

imeth	gFont()
{
	return iRet ? gIndirect(ExternalFont, &iFont) : NULL;
}

// Yanghui:
imeth	gFontWithColor()
{
	return iRet ? gIndirectCF(ExternalFontWithColor, &iCF) : NULL;
}
// Yanghui


//imeth	gInitialFont(char *fontName, int pointSize)
//{
//	strcpy(iFont.lfFaceName, fontName);
//	iCF.iPointSize = pointSize;
//
//	return self;
//}





