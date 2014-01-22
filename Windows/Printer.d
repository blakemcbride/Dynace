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


#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#include <commdlg.h>
#include <string.h>
#ifndef _WIN32
#include <print.h>
#endif
//#include "logfile.h"
#include "hdlcache.h"

defclass  Printer : Stream  {
	HDC	iHDC;		/*  handle to device context	*/

	int	iNeedFF;	/*  final ff needed         	*/
	int	iError;		/*  printing error occured  	*/
	int	iAbort;		/*  printing aborted	    	*/

	FARPROC	iProcIns;	/*  abort proc instance	    	*/

	iPWind;			/*  parent window	    	*/

	int	iHSize;		/*  horizontal size in pixels (printable area)  */
	int	iVSize;		/*  vertical size in pixels (printable area)	*/
	int	iLHSize;	/*  logical horizontal size	*/
	int	iLVSize;	/*  logical vertical size	*/
	int	iHRes;		/*  horzontal resolution	*/
	int	iVRes;		/*  vertical resolution		*/
	int	iPWidth;	/*  total physizal page width in pixels   */
	int	iPHeight;	/*  total physical page height in pixels  */
	int	iXOffset;	/*  offset to printable area	*/
	int	iYOffset;
	int	iVOffset;	/*  used to make NR PCL files print the same as parsed NR files with GDI calls  */
	int	iHOffset;
	
	iPrntDlg;		/*  Abort option window		*/

	int	iCurLine;
	int	iCurCol;

	iFont;			/*  Current Font		*/
	iBrush;
	iPen;

	HGDIOBJ	iOrgFont;	/*  To return to before returning iHDC  */
	HGDIOBJ	iOrgBrush;
	HGDIOBJ	iOrgPen;

	int	iLegalPaper;
	int	iLandscape;

	iDataDrops;
	iMetaFile;

	char	iDriverName[256];
	char	iPrinterName[256];
	char	iPortName[256];
	int	iNoStartFlag;

	int	iCopies;
};

extern	void	PrintBMP(HDC hDC, char *file, int yPos, int xPos, int height, int width);

static	int	PrintMetaFile(ivType *iv, char *file, HDC hdc);


#define INIT_OUT			\
	if (iError  ||  iAbort)		\
		return NULL;		\
	if (!iNeedFF)  {		\
		StartPage(iHDC);	\
		if (iFont) SelectObject(iHDC, gHandle(iFont)); /* needed by Windows 95 ???  */ \
		iNeedFF = 1;		\
	}

#if 0   /*  old code  */

static	HDC	GetHDC()
{
	PRINTDLG	pdlg;

	memset(&pdlg, 0, sizeof pdlg);
	pdlg.lStructSize = sizeof pdlg;
	pdlg.Flags = PD_RETURNDEFAULT | PD_RETURNDC;
	PrintDlg(&pdlg);
	return pdlg.hDC;
}

#ifdef	WIN32

static	HDC	GetHDC()
{
	PRINTER_INFO_5	pi[3];
	DWORD	need, rtn;

	if (EnumPrinters(PRINTER_ENUM_DEFAULT, NULL, 5, (LPBYTE) pi, sizeof pi, &need, &rtn))
		return CreateDC(NULL, pi[0].pPrinterName, NULL, NULL);
	else
		return (HDC) 0;
}

#else

static	HDC	GetHDC()
{
	char	buf[80], *dev, *drv, *out;
	char	*strtok();

	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	GetProfileString("windows", "device", ",,,", buf, sizeof buf);
	if ((dev = strtok(buf, ","))  &&
	    (drv = strtok(NULL, ", "))  &&
	    (out = strtok(NULL, ", ")))
		return CreateDC(drv, dev, out, NULL);
	else
		return 0;
}

#endif
#endif    /*  old code  */

#if	0

private	imeth	pGetDefaultPrinterInfo(object self)
{
	PRINTER_INFO_2	pi[3];
	DWORD	need, rtn;

	if (EnumPrinters(PRINTER_ENUM_DEFAULT, NULL, 2, (LPBYTE) pi, sizeof pi, &need, &rtn)) {
		strcpy(iDriverName, pi[0].pDriverName);
		strcpy(iPrinterName, pi[0].pPrinterName);
		strcpy(iPortName, pi[0].pPortName);
	}
	return self;
}

#else

private	imeth	pGetDefaultPrinterInfo(object self)
{
	char	buf[256], *dev, *drv, *out;
	char	*strtok();

	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	GetProfileString("windows", "device", ",,,", buf, sizeof buf);
	if ((dev = strtok(buf, ","))  &&
	    (drv = strtok(NULL, ", "))  &&
	    (out = strtok(NULL, ", "))) {
		strcpy(iDriverName, drv);
		strcpy(iPrinterName, dev);
		strcpy(iPortName, out);
	}
	return self;
}

#endif

#ifdef	_WIN32
static	HDC	GetHDCWithMode(long mode)
{
	PRINTDLG	pdlg;
	HGLOBAL		hDevMode;
	DEVMODE		*dm;
	HDC		hdc = (HDC) 0;

	if (mode) {
		memset(&pdlg, 0, sizeof pdlg);
		pdlg.lStructSize = sizeof pdlg;
		pdlg.Flags = PD_RETURNDEFAULT;
		PrintDlg(&pdlg);
		if (!(hDevMode = pdlg.hDevMode))
			return hdc;
		if (!(dm = (DEVMODE *) GlobalLock(hDevMode)))
			return hdc;
		if (mode & PM_LEGAL)
			dm->dmPaperSize = DMPAPER_LEGAL;
		if (mode & PM_LANDSCAPE)
			dm->dmOrientation = DMORIENT_LANDSCAPE;
		hdc = CreateDC(NULL, dm->dmDeviceName, NULL, dm);
		GlobalUnlock(hDevMode);
	} else {
		memset(&pdlg, 0, sizeof pdlg);
		pdlg.lStructSize = sizeof pdlg;
		pdlg.Flags = PD_RETURNDEFAULT | PD_RETURNDC;
		PrintDlg(&pdlg);
		hdc = pdlg.hDC;
	}
	return hdc;
}
#else
static	HDC	GetHDCWithMode(long mode)
{
	PRINTDLG	pdlg;
	HGLOBAL		hDevMode;
	DEVMODE		*dm = NULL;
	HDC		hdc = (HDC) 0;
	char	buf[80], *dev, *drv, *out;
	char	*strtok();

#ifndef	_WIN32
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
#endif
	if (mode) {
		GetProfileString("windows", "device", ",,,", buf, sizeof buf);
		if (!(dev = strtok(buf, ","))  ||
		    !(drv = strtok(NULL, ", "))  ||
		    !(out = strtok(NULL, ", ")))
			return hdc;
		memset(&pdlg, 0, sizeof pdlg);
		pdlg.lStructSize = sizeof pdlg;
		pdlg.Flags = PD_RETURNDEFAULT;
		PrintDlg(&pdlg);
		if (!(hDevMode = pdlg.hDevMode))
			return hdc;
		if (!(dm = (DEVMODE *) GlobalLock(hDevMode)))
			return hdc;
		if (mode & PM_LEGAL)
			dm->dmPaperSize = DMPAPER_LEGAL;
		if (mode & PM_LANDSCAPE)
			dm->dmOrientation = DMORIENT_LANDSCAPE;
		hdc = CreateDC(drv, dev, out, dm);
		GlobalUnlock(hDevMode);
	} else {
		memset(&pdlg, 0, sizeof pdlg);
		pdlg.lStructSize = sizeof pdlg;
		pdlg.Flags = PD_RETURNDEFAULT | PD_RETURNDC;
		PrintDlg(&pdlg);
		hdc = pdlg.hDC;
	}
	return hdc;
}
#endif /*  _WIN32  */

static	HDC	GetHDC(int legal)
{
	return GetHDCWithMode(legal ? PM_LEGAL : PM_LETTER);
}

static	BOOL	CALLBACK	AbortProc(HDC hdc, int nCode)
{
	MSG	msg;
	HC_VARS;

	HC_UPDATE(HDC_CACHE, hdc);
	while (self  &&  PeekMessage(&msg, (HWND) 0, 0, 0, PM_REMOVE))  {
		if (!IsDialogMessage(gHandle(iPrntDlg), &msg))  {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}
	return TRUE;
}

private	imeth	LRESULT	quitPrint(object self, unsigned id, unsigned cmd)
{
	iAbort = 1;
	return (LRESULT) 0;
}

static	object	init(object self, object pwind, char *rptName, HDC hdc, int noStart)
{
	object	obj, ctl;
	ivType	*iv;
	ABORTPROC	pi;
	DOCINFO	di;
	HWND	pw = pwind ? gHandle(pwind) : 0;
	HANDLE	hInst = gInstance(Application);
	int	sm, height, width, xpos, ypos, x, y, vs, hs;
	int	wh = 60;
	int	ww = 200;
//	POINT	point;

	if (!hdc)
		return NULL;
	if (pw)
		EnableWindow(pw, FALSE);

	pi = (ABORTPROC) MakeProcInstance((FARPROC) AbortProc, hInst);
	SetAbortProc(hdc, pi);
	memset(&di, 0, sizeof di); // required for Windows 95!!
	di.cbSize      = sizeof(DOCINFO);
	di.lpszDocName = rptName;
	di.lpszOutput  = NULL;

	if( !noStart)
		if (StartDoc(hdc, &di) <= 0)  {
			DeleteDC(hdc);
			FreeProcInstance(pi);
			if (pw)
				EnableWindow(pw, TRUE);
			return NULL;
		}

	obj = vNew(super);
	iv = ivPtr(obj);
	iNoStartFlag = noStart;
	iHDC = hdc;
	iProcIns = pi;
	iPWind = pwind;
	HC_NEW(HDC_CACHE, hdc, obj);
	iHSize = GetDeviceCaps(hdc, HORZRES);
	iVSize = GetDeviceCaps(hdc, VERTRES);
	iVRes  = GetDeviceCaps(hdc, LOGPIXELSY);
	iHRes  = GetDeviceCaps(hdc, LOGPIXELSX);

	pGetDefaultPrinterInfo(obj);
	
#ifdef	WIN32
	iPWidth  = GetDeviceCaps(hdc, PHYSICALWIDTH);
	iPHeight = GetDeviceCaps(hdc, PHYSICALHEIGHT);
	iXOffset = GetDeviceCaps(hdc, PHYSICALOFFSETX);
	iYOffset = GetDeviceCaps(hdc, PHYSICALOFFSETY);
#else
	{
		POINT	point;

		Escape(hdc, GETPHYSPAGESIZE, 0, NULL, &point);
		iPWidth  = point.x;
		iPHeight = point.y;

		Escape(hdc, GETPRINTINGOFFSET, 0, NULL, &point);
		iXOffset = point.x;
		iYOffset = point.y;
	}
#endif

	vs = GetDeviceCaps(hdc, VERTSIZE);
	hs = GetDeviceCaps(hdc, HORZSIZE);
	iLegalPaper = vs > 300;
	iLandscape  = hs > vs;
	if (iLandscape) {
		iLHSize = iLegalPaper ? 120 : 100;
		iLVSize = 51;
	} else {
		iLHSize = 80;
		iLVSize = iLegalPaper ? 78 : 66;
	}

	sm = gSetScalingMode(Application, SM_10_PER_SYSCHAR);

	SetTextAlign(hdc, TA_LEFT | TA_BASELINE);

	if (pwind)  {
		gGetSize(pwind, &height, &width);
		gGetPosition(pwind, &ypos, &xpos);
	}

	if (!pwind  ||  height < wh  ||  width < ww) {
		gGetSize(Application, &height, &width);
		xpos = ypos = 0;
	}

	y = ypos + (height - wh) / 2;
	x = xpos + (width  - ww) / 2;

	iPrntDlg = vNew(PopupWindow, (object) "Report in Progress", wh, ww);
	gSetStyle(iPrntDlg, WS_VISIBLE | WS_BORDER | WS_CAPTION);
	gSetParent(iPrntDlg, pwind);
	gSetPosition(iPrntDlg, y, x);
	gShow(iPrntDlg);

	ctl = vNew(ButtonWindow, "Cancel", iPrntDlg, quitPrint, obj);
	gSetSize(ctl, 20, 90);
	gSetPosition(ctl, 10, 55);
	gShow(ctl);

	gSetScalingMode(Application, sm);

	return obj;
}

cmeth	gNewWithHDC(pwind, char *rptName, HDC hdc)
{
	return init(self, pwind, rptName, hdc, 0);
}

cmeth	gOpenDefault(pwind, char *rptName)
{
	return init(self, pwind, rptName, GetHDC(0), 0);
}

cmeth	gOpenDefaultWithoutStart(pwind, char *rptName)
{
	return init(self, pwind, rptName, GetHDC(0), 1);
}

cmeth	gOpenDefaultWithMode(pwind, char *rptName, long mode)
{
	return init(self, pwind, rptName, GetHDCWithMode(mode), 0);
}

//  for compatability with old code
cvmeth	vNew(pwind, char *rptName)
{
	return init(self, pwind, rptName, GetHDC(0), 0);
}

cmeth	gOpenDefaultLegal(pwind, char *rptName)
{
	return init(self, pwind, rptName, GetHDC(1), 0);
}

cmeth	gQueryPrinter(pwind, char *rptName)
{
	return gQueryPrinterWithMode(self, pwind, rptName, PM_LETTER);
}

cmeth	gQueryPrinterWithoutStart(object pwind, char *rptName, int nCopies) 
{
	HDC	hdc;
	object	dlg;
	int	r;
	char	drv[256], dev[256], port[256];
	int multiplePorts = 0;
	object	obj;
	ivType	*iv;

	dlg = gNewWithMode(PrintDialog, pwind, PD_ALLPAGES | PD_NOPAGENUMS | PD_NOSELECTION);
	gSetCopies(dlg, nCopies);
	r = gPerform(dlg);
	if (r) {
		hdc = gHandle(dlg);
		gDeviceNames(dlg, drv, dev, port);
		nCopies = gCopies(dlg);
	}
		
	gDispose(dlg);
	if (!r  ||  !hdc)
		return NULL;

	// if we are printing a document (VMP) and the port name contains a comma then multiple ports
	// are listed so don't overwrite what's already there from the registry.
	// This occurs when using RightFax in a Citrix environment.
	if( strchr( port, ',') != NULL)
		multiplePorts = 1;

	obj = init(self, pwind, rptName, hdc, 1);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);

	strncpy(iDriverName, drv, sizeof( iDriverName)-1);
	strncpy(iPrinterName, dev, sizeof( iPrinterName)-1);

	if( !multiplePorts)
		strncpy(iPortName, port, sizeof( iPortName)-1);

	iCopies = nCopies;

	return obj;
}

cmeth	gQueryPrinterWithOpts(object pwind, char *rptName, int nCopies) 
{
	HDC	hdc;
	object	dlg;
	int	r;
	char	drv[256], dev[256], port[256];
	object	obj;
	ivType	*iv;

	dlg = gNewWithMode(PrintDialog, pwind, PD_ALLPAGES | PD_NOPAGENUMS | PD_NOSELECTION);
	gSetCopies(dlg, nCopies);
	r = gPerform(dlg);
	if (r) {
		hdc = gHandle(dlg);
		gDeviceNames(dlg, drv, dev, port);
		nCopies = gCopies(dlg);
	}
		
	gDispose(dlg);
	if (!r  ||  !hdc)
		return NULL;

	obj = init(self, pwind, rptName, hdc, 0);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);

	strncpy(iDriverName, drv, sizeof( iDriverName)-1);
	strncpy(iPrinterName, dev, sizeof( iPrinterName)-1);
	strncpy(iPortName, port, sizeof( iPortName)-1);

	iCopies = nCopies;

	return obj;
}

imeth	int	gCopies()
{
	return iCopies;
}

cmeth	gQueryPrinterWithMode(pwind, char *rptName, long mode)
{
	HDC	hdc;
	object	dlg;
	int	r;
	char	drv[256], dev[256], port[256];
	object	obj;
	ivType	*iv;

	dlg = gNewWithMode(PrintDialog, pwind, mode);
	r = gPerform(dlg);
	if (r) {
		hdc = gHandle(dlg);
		gDeviceNames(dlg, drv, dev, port);
	}
		
	gDispose(dlg);
	if (!r  ||  !hdc)
		return NULL;

	obj = init(self, pwind, rptName, hdc, 0);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);

	strncpy(iDriverName, drv, sizeof( iDriverName)-1);
	strncpy(iPrinterName, dev, sizeof( iPrinterName)-1);
	strncpy(iPortName, port, sizeof( iPortName)-1);

	return obj;
}

cmeth	gQueryPrinterLegal(pwind, char *rptName)
{
	return gQueryPrinterWithMode(self, pwind, rptName, PM_LEGAL);
}

imeth	gDeviceNames(char *drv, char *dev, char *port)
{
	if (drv)
		strcpy(drv, iDriverName);
	if (dev)
		strcpy(dev, iPrinterName);
	if (port)
		strcpy(port, iPortName);
	
	return self;
}

imeth	object	gDispose, gDeepDispose, gGCDispose ()
{
	gNewPage(self);
	if (!iError && !iNoStartFlag)
		if (iAbort)
			AbortDoc(iHDC);
		else
			EndDoc(iHDC);

	if (iOrgFont)
		SelectObject(iHDC, iOrgFont);
	if (iOrgBrush)
		SelectObject(iHDC, iOrgBrush);
	if (iOrgPen)
		SelectObject(iHDC, iOrgPen);

	if (iPWind)
		EnableWindow(gHandle(iPWind), TRUE);
	FreeProcInstance(iProcIns);
	HC_DELETE(HDC_CACHE, iHDC);
	DeleteDC(iHDC);
	gDispose(iPrntDlg);
	if (iFont)
		gDispose(iFont);
	if (iBrush)
		gDispose(iBrush);
	if (iPen)
		gDispose(iPen);
	if (iDataDrops)
		gDeepDispose(iDataDrops);
	if (iMetaFile)
		gDispose(iMetaFile);
	return gDispose(super);
}

imeth	gNewPage()
{
	if (iError  ||  iAbort)
		return NULL;
	if (iMetaFile) {
		gPlayMetaFile(self, gStringValue(iMetaFile));
		iMetaFile = gDispose(iMetaFile);
	}
	if (iNeedFF)  {
		if (EndPage(iHDC) < 0)
			iError = 1;
		iNeedFF = 0;
		iCurLine = iCurCol = 0;
	}
	return iAbort ? NULL : self;
}	

static	void	scale2(ivType *iv, int *row, int *col)
{
	if (row)
		*row = ((long) *row + (long) iVOffset) * (long) iVSize / (long) iLVSize;
	if (col)
		*col = ((long) *col + (long) iHOffset) * (long) iHSize / (long) iLHSize;
}

static	void	scale(ivType *iv, int *row, int *col)
{
	if (row)
		*row = ((long) *row + (long) iVOffset) * (long) iPHeight / (long) iLVSize - iYOffset;
	if (col)
		*col = ((long) *col + (long) iHOffset) * (long) iPWidth / (long) iLHSize - iXOffset;
}

imeth	gScale(int *row, int *col)
{
	iAbort;
	scale(iv, row, col);
	return self;
}

static	object	textOutLen(object self, ivType *iv, int row, int col, char *text, int len)
{
	INIT_OUT;
	scale2(iv, &row, &col);
	SetTextAlign(iHDC, TA_LEFT | TA_TOP);
	TextOut(iHDC, col, row, text, len);
	SetTextAlign(iHDC, TA_LEFT | TA_BASELINE);
	return self;
}

#define	BUFSIZE	256

#define	BP	(buf+sizeof(WORD))
#define	LEN	*((WORD *) buf)

imeth	gBinaryOut(int len, char *text)
{
	char	buf[BUFSIZE+sizeof(WORD)];
	
	while (len) {
		LEN = len > BUFSIZE ? BUFSIZE : len;
		memcpy(BP, text, LEN);
		Escape(iHDC, PASSTHROUGH, 0, buf, NULL);
		len -= LEN;
		text += LEN;
	}
	return self;
}

imeth	gTextOut(int row, int col, char *text)
{
	INIT_OUT;
	scale(iv, &row, &col);
	SetTextAlign(iHDC, TA_LEFT | TA_BASELINE);   //  required for Windows 95 !?!
	TextOut(iHDC, col, row, text, strlen(text));
	return self;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHDC;
}

imeth	int	gIsAbort()
{
	return iError  ||  iAbort;
}

imeth	int	gWrite(char *str, unsigned rlen)
{
	char	*t;
	int	sm, len, tlen=0;
	
	if (iError  ||  iAbort)
		return -1;
	sm = gSetScalingMode(Application, SM_1_PER_CHAR);
	for (t=str ; rlen  &&  str  &&  *str ; str=t)  {
		if (iCurLine == iLVSize)
			gNewPage(self);
		for (len=0, t=str ; *t  &&  *t != '\n'  &&  *t != '\r' ; ++t, ++len);
		len   = len > (int)rlen ? rlen : len;
		rlen -= len;
		tlen += len;
		if (!textOutLen(self, iv, iCurLine, iCurCol, str, len))
			return -1;
		if (rlen  &&  *t == '\n')  {
			iCurLine++;
			iCurCol = 0;
			t++;
			rlen--;
			tlen++;
		} else if (rlen  &&  *t == '\r')  {
			iCurCol = 0;
			t++;
			rlen--;
			tlen++;
		} else
			iCurCol += len;
	}
	gSetScalingMode(Application, sm);
	return tlen;
}

imeth	gSetScale(int height, int width)
{
	iLHSize = width;
	iLVSize = height;
	return self;
}

imeth	gLoadFont(char *name,	/*  may be object too  */
		  int size)
{
	object	obj;

	if (!(obj = gNewExtFontWithHDC(ExternalFont, name, size, iHDC)))
		return NULL;
	return gUse(self, obj);
}

imeth	gLoadSystemFont(unsigned font)
{
	object	obj;

	if (!(obj=gLoad(SystemFont, font)))
		return NULL;
	return gUse(self, obj);
}

private	imeth	void	use_orig_font(font)
{
	if (font)
		gUse(self, font);
	else {
		if (iOrgFont)
			SelectObject(iHDC, iOrgFont);
		if (iFont)
			iFont = gDispose(iFont);
	}
}

imeth	gUse(obj)
{
	ChkArg(obj, 2);
	if (gIsKindOf(obj, Font))  {
		if (iOrgFont)
			SelectObject(iHDC, gHandle(obj));
		else
			iOrgFont = SelectObject(iHDC, gHandle(obj));
		if (iFont)
			gDispose(iFont);
		iFont = obj;
	} else if (gIsKindOf(obj, Brush))  {
//		if (gIsKindOf(obj, SystemBrush))
//			gError(self, "Printers shouldn't use SystemBrush");
		if (iOrgBrush)
			SelectObject(iHDC, gHandle(obj));
		else
			iOrgBrush = SelectObject(iHDC, gHandle(obj));
		if (iBrush)
			gDispose(iBrush);
		iBrush = obj;
	} else if (gIsKindOf(obj, Pen))  {
		if (iOrgPen)
			SelectObject(iHDC, gHandle(obj));
		else
			iOrgPen = SelectObject(iHDC, gHandle(obj));
		if (iPen)
			gDispose(iPen);
		iPen = obj;
	} else
		gError(self, "Incorrect 2nd argument to Use::Printer");

	return obj;
}

imeth	gLine(int yStart, int xStart, int yEnd, int xEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	MoveToEx(iHDC, xStart, yStart, NULL);
	LineTo(iHDC, xEnd, yEnd);
	return self;
}

imeth	gRectangle(int yStart, int xStart, int yEnd, int xEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	Rectangle(iHDC, xStart, yStart, xEnd, yEnd);
	return self;
}

imeth	gEllipse(int yStart, int xStart, int yEnd, int xEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	Ellipse(iHDC, xStart, yStart, xEnd, yEnd);
	return self;
}

imeth	gRoundRect(int yStart,
		   int xStart,
		   int yEnd,
		   int xEnd,
		   int eHeight, 
		   int eWidth)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	scale(iv, &eHeight, &eWidth);
	RoundRect(iHDC, xStart, yStart, xEnd, yEnd, eWidth, eHeight);
	return self;
}

imeth	gChord (int yStart,
		int xStart,
		int yEnd,
		int xEnd,
		int yLStart,
		int xLStart,
		int yLEnd,
		int xLEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	scale(iv, &yLStart, &xLStart);
	scale(iv, &yLEnd, &xLEnd);
	Chord(iHDC, xStart, yStart, xEnd, yEnd, xLStart, yLStart, xLEnd, yLEnd);
	return self;
}

imeth	gPie (int yStart,
	      int xStart,
	      int yEnd,
	      int xEnd,
	      int yAStart,
	      int xAStart,
	      int yAEnd,
	      int xAEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	scale(iv, &yAStart, &xAStart);
	scale(iv, &yAEnd, &xAEnd);
	Pie(iHDC, xStart, yStart, xEnd, yEnd, xAStart, yAStart, xAEnd, yAEnd);
	return self;
}

imeth	gArc (int yStart,
	      int xStart,
	      int yEnd,
	      int xEnd,
	      int yAStart,
	      int xAStart,
	      int yAEnd,
	      int xAEnd)
{
	INIT_OUT;
	scale(iv, &yStart, &xStart);
	scale(iv, &yEnd, &xEnd);
	scale(iv, &yAStart, &xAStart);
	scale(iv, &yAEnd, &xAEnd);
	Arc(iHDC, xStart, yStart, xEnd, yEnd, xAStart, yAStart, xAEnd, yAEnd);
	return self;
}

imeth	gPlayMetaFile(char *file)
{
	HANDLE	h = 0;

	INIT_OUT;
	SetTextAlign(iHDC, TA_LEFT | TA_TOP);
#ifdef	WIN32
	if (h = GetEnhMetaFile(file)) {
		SaveDC(iHDC);
		PlayEnhMetaFile(iHDC, h, NULL);
		RestoreDC(iHDC, -1);
		DeleteEnhMetaFile(h);
#define	TEMPP 0
#if TEMPP  //  Windows 95 issues an error message if GetMetaFile fails (sometimes)
	} else if  (h = GetMetaFile(file)) {
		SaveDC(iHDC);
		PlayMetaFile(iHDC, h);
		RestoreDC(iHDC, -1);
		DeleteMetaFile(h);
#endif
	} else
		h = (HANDLE) !PrintMetaFile(iv, file, iHDC);
#else
#define	TEMPP 0
#if TEMPP  //  Windows 95 issues an error message if GetMetaFile fails (sometimes)
	if (h = GetMetaFile(file)) {
		SaveDC(iHDC);
		PlayMetaFile(iHDC, h);
		RestoreDC(iHDC, -1);
		DeleteMetaFile(h);
	} else
#endif
		h = (HANDLE) !PrintMetaFile(iv, file, iHDC);
#endif
	SetTextAlign(iHDC, TA_LEFT | TA_BASELINE);
	return h ? self : NULL;
}

imeth	gPrintBinaryFile(char *file)
{
	FILE	*fp;
	char	buf[256];
	int	len;

	fp = fopen(file, "rb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "rb");
	}
	if (!fp)
		return NULL;
	while (0 < (len=fread(buf, 1, sizeof buf, fp)))
		gBinaryOut(self, len, buf);
	fclose(fp);
	return self;
}

imeth	gPrintFile(char *file)
{
	FILE	*fp;
	char	buf[256], *p;
	int	i;

	fp = fopen(file, "r");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "r");
	}
	if (!fp)
		return NULL;
	while (fgets(buf, sizeof buf, fp)) {
		for (i=0, p=buf ;  p[i]  ; )
			if (p[i] == '\f') {
				p[i] = '\0';
				if (*p)
					vPrintf(self, "%s\n", p);
				gNewPage(self);
				p = p + i + 1;
				i = 0;
			} else
				i++;
		if (*p)
			gPuts(self, p);
	}
	fclose(fp);
	return self;
}

#define FILE_OPEN_FAIL 1
#define GBLALLOC_FAIL  2
#define SETBITS_FAIL   3


#if	defined(_WIN32)  &&  !defined(unix)
#include <pshpack2.h>
#endif

typedef struct  {
	DWORD key;
	short hmf;
	struct {
		short	left, top, right, bottom;
	}	bbox;
	WORD inch;
	DWORD reserved;
	WORD checksum;
}	APM;

#if	defined(_WIN32)  &&  !defined(unix)
#include <poppack.h>
#endif


static	void  ScaleMetafile(ivType *iv, HDC hdc, APM *mfHdr);
static	void  ScaleMetafile2(ivType *iv, HDC hdc, APM *mfHdr);

#ifdef unix
#define _O_BINARY	0
#endif

static	int	PrintMetaFile(ivType *iv, char *file, HDC hdc)
{
	HMETAFILE hmf;
	APM	mfHdr;
	BOOL	isPlaceable = FALSE;
	int	hf;
	unsigned	toRead;
#ifdef	_WIN32
	char	*buf;
#else
	HGLOBAL hMem;
	char huge *buf;
#endif
	long filesize, offset, bytesRead;

	hf = open(file, O_RDONLY | _O_BINARY);
	if (hf == -1) {
		gMoreHandles(LowFile);
		hf = open(file, O_RDONLY | _O_BINARY);
	}
	if (hf == -1)
		return FILE_OPEN_FAIL;

	// test for a "Placeable" Metafile format. if it is, then we need to get
	// the placeable header for scaling, but we want to skip it for playing.
	if (sizeof(APM) != read(hf, &mfHdr, sizeof(APM))) {           // read in enough for the placement mf header
		close(hf);
		return FILE_OPEN_FAIL;
	}
	filesize = lseek(hf, 0L, SEEK_END);    // get the metafile's size while we're here

	if (mfHdr.key == 0x9AC6CDD7L) {
		lseek(hf, (long) sizeof(APM), SEEK_SET);           // position beyond header for placement meta file
		isPlaceable = TRUE;            // flag as placment metafile
		filesize -= sizeof(APM);
	}
	else
		lseek(hf, 0L, SEEK_SET);           // otherwise position to start of file for normal mf

	// allocate a buffer big enough to hold the metafile
#ifdef	_WIN32
	buf = (char *) malloc((unsigned) filesize);
	if (!buf) {
		close(hf);
		return GBLALLOC_FAIL;
	}
#else
	hMem = GlobalAlloc(GPTR, filesize);

	if (!hMem) {
		close(hf);
		return GBLALLOC_FAIL;
	}

	// lock the buffer in memory while we read the metafile
	buf = (char huge *)GlobalLock(hMem);
#endif
	offset = 0;
	toRead = min((long) 0xFFFE, filesize);
	while (toRead) {
		bytesRead = read(hf, buf + offset, toRead);
		if (bytesRead < 0)
			break;
		offset += bytesRead;
		toRead = min((long) 0xFFFE, filesize-offset);
	}
	close(hf);

#ifdef	_WIN32
	hmf = SetMetaFileBitsEx((UINT) filesize, buf);
	if (!hmf) {
		free(buf);
		return SETBITS_FAIL;
	}
#else
	// let windows move the memory block now
	GlobalUnlock(hMem);

	// get handle to the metafile ... windows does the conversion for us

	hmf = SetMetaFileBits(hMem);
	if (!hmf) {
		GlobalFree(hMem);
		return SETBITS_FAIL;
	}
#endif

	SaveDC(hdc);  // save the device environment so we can put it back later

	SetMapMode(hdc, MM_HIENGLISH);

	if (isPlaceable)   // if a placement meta file, scale it
		ScaleMetafile2(iv, hdc, &mfHdr);

	PlayMetaFile(hdc, hmf);  // play the metafile

	RestoreDC(hdc, -1);        // restore the device environment

	DeleteMetaFile(hmf);     // remove metafile from memory
#ifdef	_WIN32
	free(buf);
#else
	GlobalFree(hMem);
#endif
	return 0;
}

static	void  ScaleMetafile(ivType *iv, HDC hdc, APM *mfHdr)
{
	RECT rc;
	int xExt, yExt;

	SetMapMode(hdc, MM_ANISOTROPIC);

	//  a fix for centerpiece bug
	if (!mfHdr->inch)
		mfHdr->inch = 300;

	// get the width and height of the image and convert to thousandths
	// of inches from whatever units were specified in header
	xExt = (int)(((long)(mfHdr->bbox.right  - mfHdr->bbox.left) * 1000L) / mfHdr->inch);
	yExt = (int)(((long)(mfHdr->bbox.bottom - mfHdr->bbox.top)  * 1000L) / mfHdr->inch);

	// set extents to match form ... scale viewport to display the full form
	// in the avaiable viewing area.
	SetRect(&rc, xExt, yExt, xExt, yExt);
	LPtoDP(hdc, (LPPOINT)&rc, 2);
	// window extent is in logical units ... use box coords direct
	SetWindowExtEx(hdc, xExt, yExt, NULL);
	// viewport extent is in physical units ... use converted box coords
#if 0  // This line is based on the whole physical page
	SetViewportExtEx(hdc, rc.left, -rc.top, NULL);
#else  // This line is based on the printable area
	SetViewportExtEx(hdc, iv->iHSize, iv->iVSize, NULL);
#endif
}

static	void  ScaleMetafile2(ivType *iv, HDC hdc, APM *mfHdr)
{
	float	r;
	int	width, height;

	SetMapMode(hdc, MM_ANISOTROPIC);
	SetWindowOrgEx(hdc, 0, 0, NULL);

	if (iPWind && gIsKindOf(iPWind, Window) && gGetScaleFlg(iPWind)) {
		double printerScale = gGetPrinterScale(iPWind);
		SetWindowExtEx(hdc, (int)((mfHdr->bbox.right - mfHdr->bbox.left)*printerScale), 
		                    (int)((mfHdr->bbox.bottom - mfHdr->bbox.top)*printerScale), NULL);	
	} else
		SetWindowExtEx(hdc, mfHdr->bbox.right - mfHdr->bbox.left, mfHdr->bbox.bottom - mfHdr->bbox.top, NULL);	

	SetViewportOrgEx(hdc, -iXOffset, -iYOffset, NULL);
	r = ((float)(mfHdr->bbox.right - mfHdr->bbox.left)) / ((float) mfHdr->inch);
	r *= iHRes;
	width = (int)(r + .5);
	r = ((float)(mfHdr->bbox.bottom - mfHdr->bbox.top)) / ((float) mfHdr->inch);
	r *= iVRes;
	height = (int)(r + .5);
	SetViewportExtEx(hdc, width, height, NULL);
}

static	void	long_hline(object pntr, int m, int n, int len)
{
	char	buf[20];

	sprintf(buf, "%d", n);
	if (!gLine(pntr, n, 0, n, len))
		return;
	if (!gTextOut(pntr, n-(m*2), 36*m, buf))
		return;
	if (!gTextOut(pntr, n-(m*2), 800*m, buf))
		return;
}

static	void	long_vline(object pntr, int m, int n, int len)
{
	char	buf[20];

	sprintf(buf, "%d", n);
	if (!gLine(pntr, 0, n, len, n))
		return;
	if (!gTextOut(pntr, 36*m, n+(m*2), buf))
		return;
	if (!gTextOut(pntr, len-(36*m), n+(2*m), buf))
		return;
}

static	void	medium_hline(object pntr, int m, int n, int len)
{
	if (!gLine(pntr, n, 0, n, 45*m))
		return;
	if (!gLine(pntr, n, 805*m, n, 850*m))
		return;
}

static	void	medium_vline(object pntr, int m, int n, int len)
{
	if (!gLine(pntr, 0, n, 45*m, n))
		return;
	if (!gLine(pntr, len-(45*m), n, len, n))
		return;
}

static	void	short_hline(object pntr, int m, int n, int len)
{
	if (!gLine(pntr, n, 0, n, 35*m))
		return;
	if (!gLine(pntr, n, 815*m, n, 850*m))
		return;
}

static	void	short_vline(object pntr, int m, int n, int len)
{
	if (!gLine(pntr, 0, n, 35*m, n))
		return;
	if (!gLine(pntr, len-(35*m), n, len, n))
		return;
}

private	imeth	grid_100()
{	
	int	i;
	int	len = iLegalPaper ? 1400 : 1100;
	object	font = iFont ? gCopy(iFont) : NULL;

	gLoadFont(self, "Times New Roman", 6);
	gSetScale(self, len, 850);

	gUse(self, vNew(CustomPen, 0, 0, 0, PS_DOT, 1));
	for (i=0 ; i <= len ; i += 100)
		long_hline(self, 1, i, len);
	for (i=0 ; i <= 850 ; i += 100)
		long_vline(self, 1, i, len);
	
	gUse(self, vNew(StockPen, BLACK_PEN));
	for (i=0 ; i <= len ; i += 10)
		if (i % 100  &&  i % 50)
			short_hline(self, 1, i, len);
	for (i=0 ; i <= 850 ; i += 10)
		if (i % 100  &&  i % 50)
			short_vline(self, 1, i, len);
		
	for (i=0 ; i <= len ; i += 50)
		if (i % 100)
			medium_hline(self, 1, i, len);
			
	for (i=0 ; i <= 850 ; i += 50)
		if (i % 100)
			medium_vline(self, 1, i, len);
	use_orig_font(self, font);
	return self;
}

private	imeth	grid_300()
{	
	int	i;
	int	len = iLegalPaper ? 4200 : 3300;
	object	font = iFont ? gCopy(iFont) : NULL;

	gLoadFont(self, "Times New Roman", 6);
	gSetScale(self, len, 2550);

	gUse(self, vNew(CustomPen, 0, 0, 0, PS_DOT, 1));
	for (i=0 ; i <= len ; i += 300)
		long_hline(self, 3, i, len);
	for (i=0 ; i <= 2550 ; i += 300)
		long_vline(self, 3, i, len);
	
	gUse(self, vNew(StockPen, BLACK_PEN));
	for (i=0 ; i <= len ; i += 10)
		if (i % 100  &&  i % 50)
			short_hline(self, 3, i, len);
	for (i=0 ; i <= 2550 ; i += 10)
		if (i % 100  &&  i % 50)
			short_vline(self, 3, i, len);
		
	for (i=0 ; i <= len ; i += 50)
		if (i % 300)
			medium_hline(self, 3, i, len);
			
	for (i=0 ; i <= 2550 ; i += 50)
		if (i % 300)
			medium_vline(self, 3, i, len);
	use_orig_font(self, font);
	return self;
}

imeth	gGrid(int dpi)
{
	return dpi == 300 ? grid_300(self) : grid_100(self);
}

imeth	gReadDropTable(char *file)
{
	char	buf[128], name[128];
	int	row, col;
	FILE	*fp = fopen(file, "r");

	if (iDataDrops)
		iDataDrops = gDeepDispose(iDataDrops);
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "r");
	}
	if (!fp)
		return NULL;
	iDataDrops = gNew(StringDictionary);
	while (fgets(buf, sizeof buf, fp))
		if (*buf != '#'  &&  3 == sscanf(buf, "%d %d %s\n", &row, &col, name))
			if (!gAddStr(iDataDrops, name, gNewWithObjObj(ObjectAssociation,
								      gNewWithInt(ShortInteger, row),
								      gNewWithInt(ShortInteger, col)))) {
				sprintf(buf, "DropTable %s contains duplicate name %s", file, name);
				gError(Application, buf);
			}
	fclose(fp);
	return self;
}

imeth	gDropData(char *name, char *text)
{
	object	pc;

	if (!iDataDrops)
		return self;
	pc = gFindValueStr(iDataDrops, name);
	if (!pc)
		return self;
	return gTextOut(self,
			(int) gShortValue(gKey(pc)),
			(int) gShortValue(gValue(pc)),
			text);
}

imeth	gPrintDataDrops()
{
	object	pc, seq, sa;

	if (!iDataDrops)
		return self;
	for (seq=gSequence(iDataDrops) ; sa=gNext(seq) ; ) {
		pc = gValue(sa);
		gTextOut(self,
			 (int) gShortValue(gKey(pc)),
			 (int) gShortValue(gValue(pc)),
			 gStringKey(sa));
	}
	return self;
}

imeth	gUseForm(char *mf, char *pc)
{
	if (!gNewPage(self))
		return NULL;
	if (iMetaFile)
		gDispose(iMetaFile);
	if (mf)
		iMetaFile = gNewWithStr(String, mf);
	if (pc  &&  !gReadDropTable(self, pc))
		return NULL;
//	gLoadFont(self, "Times New Roman", 10);
//	gSetScale(self, iLegalPaper ? 1400 : 1100, 850);  // 100 dots per inch
	gSetScale(self, iLegalPaper ? 4200 : 3300, 2550); // 300 dots per inch
  	return self;
}

imeth	gPrintBitmap(char *file, int yPos, int xPos, int height, int width)
{
	INIT_OUT;
	yPos -= height;  /*  adjust for lower left of bitmap  */
	scale(iv, &yPos, &xPos);
	scale(iv, &height, &width);
	PrintBMP(iHDC, file, yPos, xPos, height, width);
	return self;
}


imeth	gPclCompatibleScaling()
{
//	gSetScale(self, 3175, 2400);
	gSetScale(self, 3300, 2550);
	gSetOffsets(self, 155, 75);
	return self;
}

imeth	gSetOffsets(int v, int h)
{
	iVOffset = v;
	iHOffset = h;
	return self;
}

imeth	gGetParent()
{
	return iPWind;
}



/////////////////////////////////////////////////////////////////////
// gNewPrinter: return the printer object, whose HDC is initiated
//
/////////////////////////////////////////////////////////////////////
cmeth	gNewPrinter(object parentWindow, long mode)
{
	return gOpenDefaultWithMode(Printer, parentWindow, NULL, mode);
}


//////////////////////////////////////////
// gCLDStartPage: start printing a page
//
//////////////////////////////////////////
imeth	gCLDStartPage()
{
	HANDLE	h=0;

	if(iAbort || iError)    // if the printer was aborted or in error, no new page any more
		return NULL;

	if( StartPage(iHDC)<=0 ) {  // error happens, no further printing
		iError = 1;
		return NULL;
	}

	return self;
}


///////////////////////////////////////
// gCLDEndPage: end the printing page
//
///////////////////////////////////////
imeth	gCLDEndPage()
{
	if (EndPage(iHDC) <= 0)
		iError = 1;

	iNeedFF = 0;
	iCurLine = 0;
	iCurCol = 0;

	return iError ? NULL : self;
}


//////////////////////////////////////////
// gCLDStartPage: start printing a page
//
//////////////////////////////////////////
imeth	gCLDPrintMetaFile(char * MFName)
{
	HANDLE	h=0;

	// if the metafile is not valid, the printing should still continue,
	// because the controls in the cld file may need to be printed too
	if(!MFName || !(*MFName))  
		return self;

	// print the metafile:
	SetTextAlign(iHDC, TA_LEFT | TA_TOP);

#ifdef	WIN32
	if (h = GetEnhMetaFile(MFName)) {
		SaveDC(iHDC);
		PlayEnhMetaFile(iHDC, h, NULL);
		RestoreDC(iHDC, -1);
		DeleteEnhMetaFile(h);
	} else
		PrintMetaFile(iv, MFName, iHDC);
#else
		PrintMetaFile(iv, MFName, iHDC);
#endif

	SetTextAlign(iHDC, TA_LEFT | TA_BASELINE);

	return self;
}

imeth gCLDGetPhysicalParameters(int *width, int *height, int *offsetX, int *offsetY)
{
	*width = iPWidth;
	*height = iPHeight;
	*offsetX = iXOffset;
	*offsetY = iYOffset;
	return self;
}






