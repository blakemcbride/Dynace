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

#include <ctype.h>

defclass  ModelessDialog : Dialog  {
init:	init_class;
};

private	imeth	LRESULT	process_wm_keydown(HWND	hdlg, UINT mMsg, WPARAM	wParam, LPARAM lParam);

#define MODAL		0
#define MODELESS	1


cmeth	gNewDialogStr(char *resource, wind)
{
	object newObj;
	newObj = vNew(super, MODELESS, NULL, resource, wind);
	gAddDlgHandlerAfter(newObj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	return newObj;
}

cmeth	gNewDialogFromFile(char *file, unsigned res, wind)
{
#ifdef	WIN32
	object newObj;
	newObj = vNew(super, MODELESS, file, MAKEINTRESOURCE(res), wind);
	gAddDlgHandlerAfter(newObj, (unsigned) WM_KEYDOWN, process_wm_keydown);
	return newObj;
#else
	return NULL;
#endif
}

static	void	init_class()
{
	gDontCollect(CLASS);
}

private	imeth	LRESULT	process_wm_keydown(object	self, 
					   HWND		hdlg, 
					   UINT		mMsg, 
					   WPARAM	wParam, 
					   LPARAM	lParam)
{
	BOOL ret = FALSE;
	object	parent;

	if (parent=gGetParent(self))
		SendMessage(gHandle(parent), mMsg, wParam, lParam);

	return (LRESULT) ret;
}




