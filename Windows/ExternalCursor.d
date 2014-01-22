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


defclass  ExternalCursor : Cursor  {
	unsigned	iId;		/*  id associated with cursor	*/
	char		*iName;		/*  or use name of cursor	*/
	int		iN;		/*  number of times loaded	*/
 class:
	cCursorsInt;			/*  dictionary of cursors	*/
	cCursorsStr;			/*  dictionary of cursors	*/
 init:	init_class;
};



cmeth	gLoad(unsigned id)
{
	HCURSOR	hcursor;
	object	obj;
	ivType	*iv;

	obj = gFindValueInt(cCursorsInt, id);
	if (obj)  {
		ivPtr(obj)->iN++;
		return obj;
	}
	hcursor = LoadCursor(gInstance(Application), MAKEINTRESOURCE(id));
	if (!hcursor)
		return NULL;
	obj = vNew(super, hcursor);
	iv  = ivPtr(obj);
	iN = 1;
	iId = id;
	gAddInt(cCursorsInt, id, obj);
	return obj;
}

cmeth	gLoadStr(char *id)
{
	HCURSOR	hcursor;
	object	obj;
	ivType	*iv;

	obj = gFindValueStr(cCursorsStr, id);
	if (obj)  {
		ivPtr(obj)->iN++;
		return obj;
	}
	hcursor = LoadCursor(gInstance(Application), id);
	if (!hcursor)
		return NULL;
	obj = vNew(super, hcursor);
	iv  = ivPtr(obj);
	iN = 1;
	iName = id;
	gAddStr(cCursorsStr, id, obj);
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (--iN)
		return NULL;
	DestroyCursor(gHandle(self));
	if (iName)
		gRemoveStr(cCursorsStr, iName);
	else
		gRemoveInt(cCursorsInt, iId);
	return gDispose(super);
}

imeth	object	gGCDispose()  /* this will never be called because the
				    instance is in cCursorsInt  */
{
	DestroyCursor(gHandle(self));
	if (iName)
		gRemoveStr(cCursorsStr, iName);
	else
		gRemoveInt(cCursorsInt, iId);
	return gGCDispose(super);
}

imeth	gCopy, gDeepCopy ()
{
	if (iName)
		return gLoadStr(ClassOf(self), iName);
	else
		return gLoad(ClassOf(self), iId);
}

static	void	init_class()
{
	cCursorsInt = gNewWithInt(IntegerDictionary, 9);
	cCursorsStr = gNewWithInt(StringDictionary, 9);
}






