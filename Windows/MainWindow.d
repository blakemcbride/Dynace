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



defclass  MainWindow : Window  {
 class:
	cMainWindow;
 init:	init_class;
};

cmeth	gUpdateAccessMode()
{
	return gUpdateAccessMode(cMainWindow);
}

cmeth	gMainWindow()
{
	return cMainWindow;
}

cvmeth	vNew(char *title)
{
	object	wnd;
	int	sm = gSetScalingMode(Application, SM_PIXELS);

	wnd = vNew(super);
	gSetTitle(wnd, title);
	gSetStyle(wnd, WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL | WS_VISIBLE);
	gSetPosition(wnd, 0, CW_USEDEFAULT);
	gSetSize(wnd, 0, CW_USEDEFAULT);
	gSetScalingMode(Application, sm);
	gInitHelp(HelpSystem, wnd);
	return cMainWindow = wnd;
}

cmeth	gNewWithStr(char *title)
{
	return vNew(self, title);
}

imeth	gSetStyle(DWORD style)
{
	style = WS_OVERLAPPED | style & ~(WS_CHILD | WS_POPUP);
	return gSetStyle(super, style);
}

imeth	int	gProcessMessages()
{
	int	r;
	
	gShow(self);
	r = gProcessMessages(MessageDispatcher);
	gDispose(self);
	gDisposeExitObjects(Application);
	return r;
}

static	void	init_class()
{
	gDontCollect(CLASS);
}









