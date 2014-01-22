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



#include "generics.h"
//#include "logfile.h"
#include "hdlcache.h"
#include <time.h>

typedef	struct  _ll  {
	object	dlg;
	HWND	hwnd;
	struct	_ll	*next;
}	LL;


defclass  MessageDispatcher  {
 class:
	LL	*cUsed;		/*  active modeless window handles	*/
	LL	*cFree;		/*  free LL structures			*/
	int	cDone;		/*  1=program done			*/
	int	iInModal;
	time_t	cLastTime;	/*last active time*/
};


static	void	handleReturn(MSG *msg);

static	void	setLastTime(MSG *msg)
{
	if (msg->message == WM_LBUTTONDOWN || msg->message == WM_RBUTTONDOWN ||
	    msg->message == WM_MBUTTONDOWN || msg->message == WM_LBUTTONDBLCLK ||
	    msg->message == WM_RBUTTONDBLCLK || msg->message == WM_MBUTTONDBLCLK ||
	    msg->message == WM_KEYDOWN)
		time(&cLastTime); 
}

cmeth	long	gGetLastTime()
{
	return cLastTime;
}

cmeth	int	gProcessMessages()
{
	MSG	msg;
	LL	*p;

	while (!cDone  &&  GetMessage(&msg, (HWND) 0, 0, 0))  {
		setLastTime(&msg);
		if (msg.message == WM_KEYDOWN  &&  msg.wParam == VK_RETURN)
			handleReturn(&msg);
		if (!(GetWindowLong(msg.hwnd, GWL_STYLE) & ES_WANTRETURN))
			for (p = cUsed ; p ; p = p->next)
				if (IsWindow(p->hwnd)  &&  IsDialogMessage(p->hwnd, &msg))
					goto next;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
next:;
	}
	cDone = 1;
	return msg.wParam;
}

cmeth	int	gProcessModalMessages(int *done, HWND dlg)
{
	MSG	msg;
	LL	*p;

	iInModal++;
	while (!*done) {
		GetMessage(&msg, (HWND) 0, 0, 0);
		setLastTime(&msg);
		if (msg.message == WM_KEYDOWN  &&  msg.wParam == VK_RETURN)
			handleReturn(&msg);
		if (!(GetWindowLong(msg.hwnd, GWL_STYLE) & ES_WANTRETURN))
			if (IsDialogMessage(dlg, &msg))
				goto next;
		for (p = cUsed ; p ; p = p->next)
			if (IsWindow(p->hwnd)  &&  IsDialogMessage(p->hwnd, &msg))
				goto next;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
next:;
	}
	iInModal--;
	return msg.wParam;
}

cmeth	int	gProcessModalWindowMessages(int *done, HWND dlg)
{
	MSG	msg;
	LL	*p;

	iInModal++;
	while (!*done) {
		GetMessage(&msg, (HWND) 0, 0, 0);
		setLastTime(&msg);
		if (msg.message == WM_KEYDOWN  &&  msg.wParam == VK_RETURN)
			handleReturn(&msg);
		for (p = cUsed ; p ; p = p->next)
			if (IsWindow(p->hwnd)  &&  IsDialogMessage(p->hwnd, &msg))
				goto next;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
next:;
	}
	iInModal--;
	return msg.wParam;
}

cmeth	int	gInModal()
{
	return iInModal;
}

cmeth	int	gProcessOneMessage()
{
	MSG	msg;
	LL	*p;

	if (!cDone  &&  GetMessage(&msg, (HWND) 0, 0, 0))  {
		setLastTime(&msg);
		if (msg.message == WM_KEYDOWN  &&  msg.wParam == VK_RETURN)
			handleReturn(&msg);
		for (p = cUsed ; p ; p = p->next)
			if (IsWindow(p->hwnd)  &&  IsDialogMessage(p->hwnd, &msg))
				goto end;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
 end:
		return 1;	/*  valid message processed  */
	}
	cDone = 1;
	return 0;		/*  end program	 */
}

cmeth	int	gProcessMessage(UINT m)
{
	MSG	msg;
	int	n = 0;
		
	while (PeekMessage(&msg, (HWND) 0, m, m, PM_REMOVE)) {
		setLastTime(&msg);
		if (msg.message == WM_QUIT)
			cDone = 1;
		if (msg.message == WM_KEYDOWN  &&  msg.wParam == VK_RETURN)
			handleReturn(&msg);
		TranslateMessage(&msg);
		DispatchMessage(&msg);
		++n;
	}
	return n;
}

cmeth	int	gIsDone()
{
	return cDone;
}

cmeth	gAddModeless(HWND hwnd, dlg)
{
	LL	*p;

	if (cFree)  {
		p = cFree;
		cFree = p->next;
	} else
		p = Talloc(LL);
	p->dlg  = dlg;
	p->hwnd = hwnd;
	p->next = cUsed;
	cUsed = p;
	return self;
}

cmeth	gRemoveModeless(HWND hwnd)
{
	LL	*p, *n;

	for (n=cUsed, p=NULL ; n ; p=n, n=n->next)
		if (n->hwnd == hwnd)  {
			if (p)
				p->next = n->next;
			else
				cUsed = n->next;

			n->next = cFree;
			cFree = n;
			break;
		}
	return self;
}

cmeth	gDisposeModeless()
{
	LL	*p, *n;

	for (p = cUsed ; p ; p = n) {
		n = p->next;
		gDispose(p->dlg);
	}
	return self;
}

static	void	handleReturn(MSG *msg)
{
	object	wnd;
	int	mode = gReturnAsTab(Application,  -1);

	if (mode) {
		wnd = gGetObject(HandleCache, WINDOW_HANDLE_CACHE,  msg->hwnd);
		if (wnd  &&  gIsKindOf(wnd, Control) && !(GetWindowLong(msg->hwnd, GWL_STYLE) & ES_WANTRETURN))
			msg->wParam = VK_TAB;
	}
}

cmeth	int	gPostMessage(object obj, UINT mMsg, WPARAM wParam, LPARAM lParam)
{
	return PostMessage((HWND) gHandle(obj), mMsg, wParam, lParam);
}

cmeth	int	gSendMessage(object obj, UINT mMsg, WPARAM wParam, LPARAM lParam)
{
	return SendMessage((HWND) gHandle(obj), mMsg, wParam, lParam);
}





