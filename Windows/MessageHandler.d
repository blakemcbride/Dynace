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


defclass  MessageHandler  {
	iHandlers;			/*  linked list of message handlers  */
	int	iDefaultProcessing;	/*  0=no, 1=after handlers, 2=before */
	int	iCurrentDefaultProcessing;
	int	iType;			/*  1=window, 2=dialog, 3=ctl	     */
	unsigned long	iSequence;
 class:
	unsigned long	cSequence;
};

cvmeth	vNew(int type)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iHandlers = gNew(LinkList);
	iDefaultProcessing = 1;
	iType = type;
	iSequence = ++cSequence;
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (iSequence)		// this is to make sure that if this instance is disposed pending processing won't continue
		iSequence = 0;
	else
		iSequence = 1;
	gDeepDispose(iHandlers);
	return gDeepDispose(super);
}

imeth	gAddWindowHandlerAfter(LRESULT (*fun)())
{
	gAddLast(iHandlers, gNewWithObj(LinkValue, gNewWithPtr(Pointer, fun)));
	return self;
}

imeth	gAddWindowHandlerBefore(LRESULT (*fun)())
{
	gAddFirst(iHandlers, gNewWithObj(LinkValue, gNewWithPtr(Pointer, fun)));
	return self;
}

typedef	LRESULT (*lfun)();

imeth	LRESULT	gProcessWindowMsg(object	wind,
				  HWND		hwnd, 
				  UINT		mMsg, 
				  WPARAM	wParam, 
				  LPARAM	lParam,
				  WNDPROC	orgWndProc)
{
	object	lnk;
	void	*p;
	long	r=0;
	unsigned long seq = iSequence;

	iCurrentDefaultProcessing = iDefaultProcessing;
	if (iCurrentDefaultProcessing == 2)
		if (orgWndProc) {
#ifdef	WIN32
			r = CallWindowProc((WNDPROC) orgWndProc, hwnd, mMsg, wParam, lParam);
#else
			r = CallWindowProc((FARPROC) orgWndProc, hwnd, mMsg, wParam, lParam);
#endif
		} else
			r = DefWindowProc(hwnd, mMsg, wParam, lParam);

	for (lnk = gFirst(iHandlers) ; seq == iSequence  &&  lnk ; )  {
		p = gPointerValue(gValue(lnk));

		r = (*(lfun) p)(wind, hwnd, mMsg, wParam, lParam);

		/*  this is done because windows can autodispose
		    this whole linked list right in the middle  */

		lnk = IsObj(wind)  &&  IsObj(lnk) &&  seq == iSequence ? gNext(lnk) : NULL;
	}

	if (iCurrentDefaultProcessing == 1)
		if (orgWndProc) {
#ifdef	WIN32
			r = CallWindowProc((WNDPROC) orgWndProc, hwnd, mMsg, wParam, lParam);
#else
			r = CallWindowProc((FARPROC) orgWndProc, hwnd, mMsg, wParam, lParam);
#endif
		} else
			r = DefWindowProc(hwnd, mMsg, wParam, lParam);

	return r;
}

typedef	BOOL	(*bfun)();

imeth	BOOL	gProcessDialogMsg(object	dlg,
				  HWND		hdlg, 
				  UINT		mMsg, 
				  WPARAM	wParam, 
				  LPARAM	lParam)
{
	object	lnk;
	void	*p;
	BOOL	r = FALSE;
	unsigned long seq = iSequence;

	for (lnk = gFirst(iHandlers) ; !r  &&  seq == iSequence  && lnk ; )  {
		p = gPointerValue(gValue(lnk));
		r = (*(bfun) p)(dlg, hdlg, mMsg, wParam, lParam);

		/*  this is done because modeless dialogs can autodispose
		    this whole linked list right in the middle  */

		if (!r  &&  IsObj(dlg)  &&  IsObj(lnk) &&  seq == iSequence)
			lnk = gNext(lnk);
		else
			lnk = NULL;
	}

	return r;
}

imeth	gSetDefaultProcessingMode(int mode)
{
	iDefaultProcessing = mode;
	return self;
}

imeth	gOverrideDefaultProcessingMode(int mode)
{
	iCurrentDefaultProcessing = mode;
	return self;
}










