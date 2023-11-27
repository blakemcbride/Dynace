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




#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif


#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(_MSC_VER)
#include <io.h>
#include <sys/locking.h>
#include <errno.h>
#endif

#if	!defined(_MSC_VER)
#include <unistd.h>
#define	tell(h)		lseek(h, 0L, SEEK_CUR)
#endif


defclass  LowFile : Stream  {
	iName;
	int	iHandle;
class:
	ifun	cMoreHandles;
};

/* extern	long	lseek(), tell(); */


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

private	imeth	LowFile_initVars(char *name, int handle)
{
	iName = gNewWithStr(String, name);
	iHandle = handle;
	return self;
}

cmeth	gOpenLowFile, <vNew> (char *name, int oflag, int pmode)
{
	int	handle;

	if (IsObj((object) name))
		name = gStringValue((object) name);
#ifdef	PLAN9
	handle = open(name, oflag);
#else
	handle = open(name, oflag, pmode);
#endif
	if (handle == -1) {
		gMoreHandles(LowFile);
#ifdef	PLAN9
		handle = open(name, oflag);
#else
		handle = open(name, oflag, pmode);
#endif
	}
	if (handle == -1)
		return NULL;
	return LowFile_initVars(gNew(super), name, handle);
}

imeth	object	gGCDispose()
{
	close(iHandle);
	return gDispose(super);
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iName);
	return gGCDispose(self);
}

imeth	int	gRead(char *buf, unsigned n)
{
	return read(iHandle, buf, n);
}

imeth	int	gWrite(char *buf, unsigned n)
{
	return write(iHandle, buf, n);
}

imeth	char	*gGets(char *buf, int sz)   /*  very slow - not for real use  */
{
	int	h = iHandle;
	int	i, r;
	char	c;

	if (sz <= 0)
		return NULL;
	if (sz-- == 1)  {
		*buf = '\0';
		return buf;
	}
	for (i=0 ; i < sz ; )  {
		r = read(h, &c, 1);
		if (!r  &&  !i  ||  r < 0)
			return NULL;
		if (!r)
			break;
		buf[i++] = c;
		if (c == '\n')
			break;
	}
	buf[i] = '\0';
	return buf;
}

imeth	long	gAdvance(long n)
{
	int	h = iHandle;
	long	p = tell(h);
	long	r = lseek(h, n, SEEK_CUR);
	return r >= 0L ? r-p : 0L;
}

imeth	long	gRetreat(long n)
{
	int	h = iHandle;
	long	p = tell(h);
	long	r = lseek(h, -n, SEEK_CUR);
	return r >= 0L ? p-r : 0L;
}

imeth	long	gSeek(long n)
{
	long	r = lseek(iHandle, n, SEEK_SET);
	return r;
}

imeth	long	gPosition()
{
	return tell(iHandle);
}

imeth	long	gLength()
{
#ifdef	PLAN9
	return tell(iHandle);
#else
	struct	stat	sb;
	int	r;

	r = fstat(iHandle, &sb);
	return r ? -1L : sb.st_size;
#endif
}

imeth	char	*gName()
{
	return gStringValue(iName);
}

imeth	int	gEndOfStream()
{
#if	defined(_MSC_VER) /*  eof() not available on unix  */
	return eof(iHandle);
#else
	gShouldNotImplement(self, "EndOfStream");
	iHandle = iHandle;   /*  keep the compiler happy  */
	return 0;  /*  never reached  */
#endif
}

imeth	int	gFileHandle()
{
	return iHandle;
}

cmeth	ifun	gSetMoreHandles(ifun fun)
{
	ifun	pfun = cMoreHandles;
	cMoreHandles = fun;
	return pfun;
}

cmeth	gMoreHandles()
{
	if (cMoreHandles)
		cMoreHandles();
	return self;
}

imeth	int	gLock(unsigned long start, unsigned long len, int wait)
{
#ifdef	PLAN9
	gShouldNotImplement(self, "gLock");
	return 0;
#else
#if	defined(_MSC_VER)
	int	r;
	
	_lseek(iHandle, start, SEEK_SET);
	do {
		r = _locking(iHandle, wait ? _LK_LOCK : _LK_NBLCK, len);

/*  the following line is correct but won't work under Windows under multi-threading and non-threading environments
    without being compiled specifically for that environment so I am using the incorrect version so that I don't
    have to worry about two libraries for this one variable.  */
#ifndef _WIN32
	} while (wait  &&  r  &&  (errno == EACCES  ||  errno == EDEADLOCK));
#else
	} while (wait  &&  r);
#endif
	return r;
#else
	struct	flock	fs;

	fs.l_type = F_WRLCK;
	fs.l_whence = SEEK_SET;
	fs.l_start = start;
	fs.l_len = len;
	return -1 == fcntl(iHandle, wait ? F_SETLKW : F_SETLK, &fs) ? -1 : 0;
#endif
#endif
}

imeth	int	gUnLock(unsigned long start, unsigned long len)
{
#ifdef	PLAN9
	gShouldNotImplement(self, "gUnLock");
	return 0;
#else
#if	defined(_MSC_VER)
	_lseek(iHandle, start, SEEK_SET);
	return _locking(iHandle, _LK_UNLCK, len);
#else
	struct	flock	fs;

	fs.l_type = F_UNLCK;
	fs.l_whence = SEEK_SET;
	fs.l_start = start;
	fs.l_len = len;
	return -1 == fcntl(iHandle, F_SETLK, &fs) ? -1 : 0;
#endif
#endif
}







