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

defclass  InternalMenu : Menu  {
	int	iCount;		/*  next automatic ID		*/
	object	iPopups;	/*  linked list of popup menus  */
};


cvmeth	vNew()
{
	object	obj;
	obj = vNew(super, CreateMenu());
	ivPtr(obj)->iPopups = gNew(LinkObject);
	return obj;
}

imeth	object	gDispose()
{
	gDeepDispose(iPopups);
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	gDeepDispose(iPopups);
	return gDeepDispose(super);
}

imeth	int	gAddMenuOption(char *title, void (*fun)())
{
	AppendMenu(gHandle(self), MF_STRING, ++iCount, title);
	gAssociate(self, iCount, fun);
	return iCount;
}

imeth	gAddPopupMenu(char *title, menu)
{
	ChkArg(menu, 2);
	AppendMenu(gHandle(self), MF_POPUP, (UINT_PTR) gHandle(menu), title);
	gAddLast(iPopups, menu);
	return self;
}

imeth	int	gNextID()
{
	return ++iCount;
}

imeth	gTopMenu()
{
	return self;
}





