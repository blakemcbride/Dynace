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





#include <windows.h>
#include "logfile.h"
#include "hdlcache.h"

typedef	LRESULT	(*lfun)();

defclass  ButtonWindow : Window  {
	iObj;			/*  an object associated with the button  */
	lfun	iFun;
 init:	init_class;
};


cvmeth	vNew(char *title, parent, lfun fun, obj)
{
	object	wnd;
	ivType	*iv;

	wnd = gNewBuiltIn(self, "button", parent);
	iv = ivPtr(wnd);
	gSetTitle(wnd, title);
	gSetStyle(wnd, WS_CHILD | WS_VISIBLE | BS_PUSHBUTTON);
	iFun = fun;
	iObj = obj;
	return wnd;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

imeth	LRESULT	gProcessCmd(unsigned id,unsigned cmd)
{
	if (iFun)
		return (*iFun)(iObj, id, cmd);
	else
		return (LRESULT) 0;
}

static	void	init_class()
{
	gDontCollect(CLASS);
}









