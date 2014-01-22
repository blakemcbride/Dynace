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




#if  defined(_WIN32)  ||  defined(WIN32)
#include <io.h>
#define	FINDSTRUCT	_finddata_t
#ifndef	_WIN32
#define	_WIN32
#endif
#else
#include <dos.h>
#include <sys/types.h>
#include <sys/stat.h>
#define	FINDSTRUCT	find_t
#endif


#include "findfile.h"



defclass  FindFile  {
	iFileName;
	unsigned	iAttributes;	//  those defined by Dynace
	struct	FINDSTRUCT	iData;
	long	iHandle;	//  only used by WIN32
	int	iFirst;		//  1=findfirst done already
	int	iEnd;		//  1=seen them all
};

cmeth	gNewFindFile, <vNew> (char *name, int attr)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iFileName = gNewWithStr(String, name);
	iAttributes = attr;
	iHandle = -1L;
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
#ifdef	_WIN32
	if (iHandle != -1L)
		_findclose(iHandle);
#endif
	gDispose(iFileName);
	return gDispose(super);
}

imeth	gGCDispose()
{
#ifdef	_WIN32
	if (iHandle != -1L)
		_findclose(iHandle);
#else
	USE(iHandle);
#endif
	return gDispose(super);
}

#define	SET(a, b)	((a) & (b))

static	int	valid(ivType *iv)
{
	if (SET(iData.attrib, _A_SUBDIR)) {
		if (!SET(iAttributes, FF_DIRECTORY))
			return 0;
	} else
		if (!SET(iAttributes, FF_FILE))
			return 0;
	if (SET(iData.attrib, _A_RDONLY)) {
		if (!SET(iAttributes, FF_READONLY))
			return 0;
	} else
		if (!SET(iAttributes, FF_READWRITE))
			return 0;
	if (SET(iData.attrib, _A_HIDDEN)  &&  !SET(iAttributes, FF_HIDDEN))
		return 0;
	if (SET(iData.attrib, _A_SYSTEM)  &&  !SET(iAttributes, FF_SYSTEM))
		return 0;
	if (SET(iAttributes, FF_ARCHIVE_ONLY)  &&  !SET(iData.attrib, _A_ARCH))
		return 0;
	return 1;
}

imeth	char	*gNextFile()
{
#ifdef	_WIN32
	int	r;
	
	if (iEnd)
		return NULL;
	if (!iFirst) {
		iHandle = _findfirst(gStringValue(iFileName), &iData);
		iFirst = 1;
		r = iHandle < 0L ? 1 : 0;
	} else
		r = _findnext(iHandle, &iData);
	while (!r  &&  !valid(iv))
		r = _findnext(iHandle, &iData);
	if (r) {
		iEnd = 1;
		if (iHandle != -1L) {
			_findclose(iHandle);
			iHandle = -1;
		}
		return NULL;
	} else
		return iData.name;
#else	
	unsigned   r;
	
	if (iEnd)
		return NULL;
	if (!iFirst) {
		r = _dos_findfirst(gStringValue(iFileName),
				   _A_ARCH | _A_HIDDEN | _A_NORMAL | _A_RDONLY | _A_SUBDIR | _A_SYSTEM,
				   &iData);
		iFirst = 1;
	} else
		r = _dos_findnext(&iData);
	while (!r  &&  !valid(iv))
		r = _dos_findnext(&iData);
	if (r) {
		iEnd = 1;
		return NULL;
	} else
		return iData.name;
#endif
}

imeth	long	gLength()
{
	return iFirst && !iEnd ? iData.size : -1L;
}

imeth	char	*gName()
{
	return  iFirst && !iEnd ? iData.name : NULL;
}

#ifndef	_WIN32
static	void	getpath(char *path, char *search, char *name)
{
	char	*p, *last;

	strcpy(path, search);
	for (last=path-1, p=path ;  *p  ;  ++p)
		if (*p == ':'  ||  *p == '/'  ||  *p == '\\')
			last = p;
	strcpy(last+1, name);
}
#endif

imeth	long	gWriteTime()
{
#ifdef	_WIN32
	if (!iFirst  ||  iEnd)
		return -1L;
	return iData.time_write;
#else
	char	path[256];
#ifdef	MSC16
	struct	_stat	sb;
#else
	struct	stat	sb;
#endif
	
	if (!iFirst  ||  iEnd)
		return -1L;
	getpath(path, gStringValue(iFileName), iData.name);
	return _stat(path, &sb) ? -1L : sb.st_mtime;
#endif
}

imeth	unsigned	gAttributes()
{
	unsigned	at = 0;
	
	if (!iFirst  ||  iEnd)
		return at;
	if (SET(iData.attrib, _A_SUBDIR))
		at |= FF_DIRECTORY;
	else
		at |= FF_FILE;
	if (SET(iData.attrib, _A_RDONLY))
		at |= FF_READONLY;
	else
		at |= FF_READWRITE;
	if (SET(iData.attrib, _A_HIDDEN))
		at |= FF_HIDDEN;
	if (SET(iData.attrib, _A_SYSTEM))
		at |= FF_SYSTEM;
	if (SET(iData.attrib, _A_ARCH))
		at |= FF_ARCHIVE_ONLY;
	return at;
}




