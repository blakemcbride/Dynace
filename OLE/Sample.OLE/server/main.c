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



/*   SERVER code  */

#include <windows.h>
#include <sql.h>
#include <sqlext.h>


#include "generics.h"
#include "resource.h"


#include <ole2ver.h>
#include <initguid.h>  //  only in one source file!
#ifndef	_WIN32
#include <dispatch.h>
#endif

#include "../classid.h"


#define	TEST


static	long	file_exit(object wind);


int	start()
{
	object	win;
	char	title[80];
	int	r;
	object	server;

	sprintf(title, "Test OLE Server - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);
#ifdef	TEST
	gSetMaxLines(win, 400);
	gSetStyle(win, WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL | WS_VISIBLE);
#else
	gSetStyle(win, WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL);
#endif

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	server = gNewComServer(ComServer, &CLSID_Server);

	gAddInterface(server, &IID_IDispatch, OLEDispatch);

	Server;
	
	gRegister(server);

	r = gProcessMessages(win);

	gDispose(server);

	return r;
}

static	long	file_exit(object wind)
{
	gQuitApplication(Application, 0);
	return 0L;
}




