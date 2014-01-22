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


defclass  ExternalIcon : Icon  {
	unsigned	iId;	/*  id associated with icon	*/
	char		*iName; /*  or name of icon		*/
	int		iN;	/*  number of times loaded	*/
 class:
	cIconsInt;		/*  dictionary of icons		*/
	cIconsStr;		/*  dictionary of icons		*/
 init:	init_class;
};



cmeth	gLoad(unsigned id)
{
	HICON	hicon;
	object	obj;
	ivType	*iv;

	obj = gFindValueInt(cIconsInt, id);
	if (obj)  {
		ivPtr(obj)->iN++;
		return obj;
	}
	hicon = LoadIcon(gInstance(Application), MAKEINTRESOURCE(id));
	if (!hicon)
		return NULL;
	obj = vNew(super, hicon);
	iv  = ivPtr(obj);
	iN = 1;
	iId = id;
	gAddInt(cIconsInt, id, obj);
	return obj;
}

cmeth	gLoadResourceFromFile(char *file, unsigned id)
{
	HICON	hicon;
	object	obj;
	ivType	*iv;

	obj = gFindValueInt(cIconsInt, id);
	if (obj)  {
		ivPtr(obj)->iN++;
		return obj;
	}
#ifdef	WIN32
	hicon = ResourceLoadIcon(file, MAKEINTRESOURCE(id));
#else
	hicon = 0;
#endif
	if (!hicon)
		return NULL;
	obj = vNew(super, hicon);
	iv  = ivPtr(obj);
	iN = 1;
	iId = id;
	gAddInt(cIconsInt, id, obj);
	return obj;
}

cmeth	gLoadStr(char *id)
{
	HICON	hicon;
	object	obj;
	ivType	*iv;

	obj = gFindValueStr(cIconsStr, id);
	if (obj)  {
		ivPtr(obj)->iN++;
		return obj;
	}
	hicon = LoadIcon(gInstance(Application), id);
	if (!hicon)
		return NULL;
	obj = vNew(super, hicon);
	iv  = ivPtr(obj);
	iN = 1;
	iName = id;
	gAddStr(cIconsStr, id, obj);
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (--iN)
		return NULL;
	DestroyIcon(gHandle(self));
	if (iName)
		gRemoveStr(cIconsStr, iName);
	else
		gRemoveInt(cIconsInt, iId);
	return gDispose(super);
}

imeth	object	gGCDispose()  /* this will never be called because the
				    instance is in cIconsInt  */
{
	DestroyIcon(gHandle(self));
	if (iName)
		gRemoveStr(cIconsStr, iName);
	else
		gRemoveInt(cIconsInt, iId);
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
	cIconsInt = gNewWithInt(IntegerDictionary, 9);
	cIconsStr = gNewWithInt(StringDictionary, 9);
}








