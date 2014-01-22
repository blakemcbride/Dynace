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

defclass  PopupWindow : Window  {
 init:	init_class;
};

private imeth long	process_wm_keydown(HWND hwnd, UINT mMsg, WPARAM wParam, LPARAM lParam);

cmeth	gNewPopupWindowWithClass(char *title, int lines, int characters, char *classname)
{
	object	wnd;

	wnd = gNewWithStr(super, classname);
	gSetTitle(wnd, title);
	gSetStyle(wnd, WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SYSMENU);
	gSetSize(wnd, lines, characters);
	gAddHandlerAfter(wnd, (unsigned) WM_KEYDOWN, process_wm_keydown);
	return wnd;
}

cvmeth	vNew(char *title, int lines, int characters)
{
	return gNewPopupWindowWithClass(self, title, lines, characters, NULL);
}

cmeth	gNewPopupWindow(char *title, int lines, int characters)
{
	return vNew(self, title, lines, characters);
}

cmeth	gNewCLDWindow(object parent, char *title, int lines, int characters)
{
	object	obj = vNew(self, title, lines, characters);
	int	pm = gSetScalingMode(Application, SM_PIXELS);
	int	vert, horz;
	
	gSetParent(obj, parent);
	gSetStyle(obj, (WS_VISIBLE | WS_BORDER | WS_POPUPWINDOW | WS_DLGFRAME) & ~WS_SYSMENU);
	
	gBackBrush(obj, gNewSystemBrush(SystemBrush, COLOR_BTNFACE));

	gGetPosition(parent, &vert, &horz);
	gSetPosition(obj, vert + 10, horz + 10);
	gSetScalingMode(Application, pm);
	gAddHandlerAfter(obj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_POPUP | style & ~(WS_OVERLAPPED | WS_CHILD);
	return gSetStyle(super, style);
}

static	void	init_class()
{
	gDontCollect(CLASS);
}

private	imeth	long	process_wm_keydown(object	self, 
					   HWND		hwnd, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	long rval = 0;
	object	parent;

	if (parent=gGetParent(self))
		rval = SendMessage(gHandle(parent), mMsg, wParam, lParam);
	
	return rval;
}





