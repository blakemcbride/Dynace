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



#include <stdlib.h>

#include "logfile.h"
#include "hdlcache.h"


defclass  DynamicLibrary  {
	iFile;		/*  Lib file name	*/
	HINSTANCE  iH;	/*  Lib instance	*/
 class:
	cLoaded;    /*  Dictionary of loaded DLL's & associated instance  */
};


static	int	find(char *file);


cmeth	gLoadLibrary(char *file)
{
	object		obj, f;
	ivType		*iv;
	HINSTANCE	h;

#ifndef	WIN32
	if (find(file))
		return NULL;
#endif
	if (!cLoaded)
		cLoaded = gNewWithInt(Dictionary, 27);
	f = gNewWithStr(String, file);
	obj = gFindValue(cLoaded, f);
	if (obj)  {
		gDispose(f);
		return obj;
	}
	h = LoadLibrary(file);
#ifdef	WIN32
	if (!h)
		return gDispose(f);
#else
	if ((UINT) h < HINSTANCE_ERROR)
		return gDispose(f);
#endif
	obj = vNew(super);
	iv = ivPtr(obj);
	iFile = f;
	iH = h;
	gAddValue(cLoaded, f, obj);
	return obj;
}

#ifndef	WIN32

#define exists(f)	!access((f), 0)

/* The following is needed because Windows 3.1 issues an error message
   if it can't find a DLL.  */

static	int	find(char *file)
{
	char	dir[144], path[200], drive[4];

	if (exists(file))
		return 0;

	GetWindowsDirectory(dir, sizeof dir);
	sprintf(path, "%s\\%s", dir, file);
	if (exists(path))
		return 0;

	GetSystemDirectory(dir, sizeof dir);
	sprintf(path, "%s\\%s", dir, file);
	if (exists(path))
		return 0;

	GetModuleFileName(gInstance(Application), path, sizeof path);
	_splitpath(path, drive, dir, NULL, NULL);
	sprintf(path, "%s%s%s", drive, dir, file);
	if (exists(path))
		return 0;

	return 1;
}

#endif

imeth	object	gDispose, gDeepDispose ()
{
	gRemoveObj(cLoaded, iFile);
	gDispose(iFile);
	FreeLibrary(iH);
	return gDispose(super);
}

cmeth	gFreeAll()
{
	object	seq, assoc;

	if (cLoaded)  {
		for (seq=gSequence(cLoaded) ; assoc=gNext(seq) ; )
			gDispose(gValue(assoc));
		cLoaded = gDispose(cLoaded);
	}
	return self;
}

imeth	FARPROC	 gGetProcAddress(char *fun)
{
	return GetProcAddress(iH, fun);
}

cmeth	gFindStr(char *file)
{
	object		obj, f;

	if (!cLoaded)
		return NULL;
	f = gNewWithStr(String, file);
	obj = gFindValue(cLoaded, f);
	gDispose(f);
	return obj;
}





