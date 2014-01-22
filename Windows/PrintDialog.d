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
#ifndef _WIN32
#include <print.h>
#endif
//#include "logfile.h"

defclass  PrintDialog : CommonDialog  {
	PRINTDLG	iPD;
	int		iUsedHDC;	/*  1=hdc requested   */
	int		iReturn;	/*  1=success completion at least once */
	DWORD		iInputFlags;
	long		iDMFlags;
};


cmeth	gNewWithMode(parent, long flags)
{
	object	obj = vNew(PrintDialog, parent);
	ivType	*iv = ivPtr(obj);

	iDMFlags = flags;
	
	return obj;
}

cvmeth	vNew(parent)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iPD.lStructSize = sizeof(PRINTDLG);
	iPD.hwndOwner = parent ? gHandle(parent) : (HWND) 0;
	iPD.nMaxPage = 9999;
	iPD.Flags = PD_RETURNDC | PD_USEDEVMODECOPIES | PD_NOPAGENUMS |
		    PD_NOSELECTION;
	return obj;
}

imeth	object	gDispose, gDeepDispose, gGCDispose ()
{
	if (iReturn)  {
		if (!iUsedHDC  &&  iPD.hDC)
			DeleteDC(iPD.hDC);
		if (iPD.hDevMode)
			GlobalFree(iPD.hDevMode);
		if (iPD.hDevNames)
			GlobalFree(iPD.hDevNames);
	}
	return gDispose(super);
}

imeth	HANDLE	gHandle()
{
	iUsedHDC = 1;		/*  client using it so don't delete it  */
	return (HANDLE) iPD.hDC;
}

imeth	int	gPerform()
{
	int	r;

#ifndef	_WIN32
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
	gMoreHandles(LowFile);
#endif
	if(iDMFlags) {
		DEVMODE	*dm;
		DWORD	flags = iPD.Flags;
		iPD.Flags = PD_RETURNDEFAULT;
		PrintDlg(&iPD);
		iPD.Flags = flags;
		if (iPD.hDevMode  &&  (dm = (DEVMODE *) GlobalLock(iPD.hDevMode))) {
			if(iDMFlags & PM_LEGAL)
				dm->dmPaperSize = DMPAPER_LEGAL;
			if(iDMFlags & PM_LANDSCAPE)
				dm->dmOrientation = DMORIENT_LANDSCAPE;
			GlobalUnlock(iPD.hDevMode);
		}
	}
	iInputFlags = iPD.Flags;
	r = PrintDlg(&iPD);
	iReturn |= r;
	return r;
}

imeth	gSetFlags(DWORD flags)
{
	iPD.Flags = flags | PD_RETURNDC;
	return self;
}

imeth	gGetPageRange(int *start, int *end)
{
	*start = iPD.nFromPage;
	*end   = iPD.nToPage;
	return self;
}

imeth	int	gCopies()
{
	DEVMODE	*dm;
	int	c;

	if (!iPD.hDevMode /*  ||  (iInputFlags & PD_USEDEVMODECOPIES) */)
		return iPD.nCopies;
	if (dm = (DEVMODE *) GlobalLock(iPD.hDevMode)) {
		c = dm->dmCopies;
		GlobalUnlock(iPD.hDevMode);
	} else
		c = 0;
	return c;
}

imeth	HGLOBAL	gGetDevModeHandle()
{
	return iPD.hDevMode;
}

imeth	gSetCopies(int n)
{
	iPD.nCopies = n;
	return self;
}

imeth	gDeviceNames(char *drv, char *dev, char *port)
{
	DEVNAMES	*dn;
	
	if (drv)
		*drv = '\0';
	if (dev)
		*dev = '\0';
	if (port)
		*port = '\0';
	
	if (iPD.hDevNames  &&  (dn = (DEVNAMES *) GlobalLock(iPD.hDevNames))) {
		if (drv)
			strcpy(drv, (char *) dn + dn->wDriverOffset);
		if (dev)
			strcpy(dev, (char *) dn + dn->wDeviceOffset);
		if (port)
			strcpy(port, (char *) dn + dn->wOutputOffset);
	
		GlobalUnlock(iPD.hDevNames);
	} else
		return NULL;
	
	return self;
}





