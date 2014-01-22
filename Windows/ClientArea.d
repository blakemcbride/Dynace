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


defclass  ClientArea : ChildWindow  {
	iParent;
	int	iTBSize;		//  size of toolbar
	int	iSBSize;		//  size of statusbar
 init:	init_class;
};


cvmeth	vNew(pwnd)
{
	object	wnd;
	ivType	*iv;

	ChkArgTyp(pwnd, 2, Window);
	wnd = vNew(super, pwnd, 20, 80);
	iv = ivPtr(wnd);
	iParent = pwnd;
	if (gUsingAlternateColor(Application))
		gSetStyle(wnd, WS_VISIBLE /*| WS_BORDER*/);
	else
		gSetStyle(wnd, WS_VISIBLE | WS_BORDER);
	return wnd;
}

imeth	int	gShow()
{
	object	tb, sb;
	tb = gGetToolBar(iParent);
	sb = gGetStatusBar(iParent);
	iTBSize = tb ? gPixelHeight(tb) : 0;
	iSBSize = sb ? gPixelHeight(sb) : 0;
	return gShow(super);
}

imeth	gUpdateState()
{
	int	sm;
	HWND	pwnd, swnd;
	RECT	rect;
	
	swnd = gHandle(self);
	pwnd = GetParent(swnd);
	GetClientRect(pwnd, &rect);
	SetWindowPos(swnd, 0, 0, iTBSize, rect.right, rect.bottom - (iTBSize + iSBSize), SWP_NOZORDER);
	return self;
}

imeth	gGetTag()
{
	return gGetTag(iParent);
}

imeth	gSetTag(tag)
{
	return gSetTag(iParent, tag);
}

imeth	gPropertyPut(char *prop, int autoDispose, val)
{
	return gPropertyPut(iParent, prop, autoDispose, val);
}

imeth	gPropertyGet(char *prop)
{
	return gPropertyGet(iParent, prop);
}

imeth	gPropertyRemove(char *prop)
{
	return gPropertyRemove(iParent, prop);
}

static	void	init_class()
{
	gDontCollect(CLASS);
}









