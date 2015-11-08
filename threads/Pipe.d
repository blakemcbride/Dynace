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





defclass  Pipe : Stream  {
	object	iObj;		/*  pipe object		*/
	char	*iName;
	char	*iBuf;
	int	iBufsiz;
	char	*iWptr;	/*  write pointer (where next byte should go)	*/
	char	*iRptr;	/*  read pointer (where next byte will be)	*/
	object	iRblk;	/*  thread on read block			*/
	object	iWblk;	/*  thread on write block			*/
	int	iRblock;/*  block reads when buffer empty		*/
	int	iWblock;/*  block writes when buffer full		*/
	struct _Pipe_iv_t	*iNext;
 class:
	struct _Pipe_iv_t	*cMpl;	/*  master pipe list		*/
};


#include <string.h>


cmeth	gNewWithStrInt, <vNew> : Pipe_New (object self, char *name, int bufsiz)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	
	iObj = obj;
	if (name)  {
		iName = Tnalloc(char, strlen(name)+1);
		strcpy(iName, name);
	}
	iBuf = Tnalloc(char, bufsiz);
	iBufsiz = bufsiz;
	iWptr = iRptr = iBuf;
	iNext = cMpl;
	cMpl = iv;
	return obj;
}

cmeth	gNew()
{
	return Pipe_New(self, NULL, 128);
}

imeth	int	gWrite(object self, char *buf, unsigned sz)
{
	int	room, rroom, bytes, w=0;
	
	if (iWblk)
		return 0;
	while (sz)  {
		/*  how much room is left on the write end of the buffer?  */

		room = (int)(iBufsiz - (iWptr - iBuf));
		
		/*  if not enough make room from read end   */
	
		if (room < (int) sz)  {
			rroom = (int)(iRptr - iBuf);
			if (rroom)  {
				bytes = (int)(iWptr - iRptr); /* bytes in buffer  */
				memmove(iBuf, iRptr, bytes);
				iRptr = iBuf;
				iWptr = iBuf + bytes;
				room = (int)(iBufsiz - (iWptr - iBuf));
			}
		}

		bytes = (int) sz > room ? room : (int) sz; /*  bytes to but on buffer  */
		if (bytes)  {
			memcpy(iWptr, buf, bytes);
			iWptr += bytes;
			buf += bytes;
			sz -= bytes;
			w += bytes;
			if (iRblk)
				gRelease(iRblk, 0);
		}
		/*  the following line is needed because the above release might have cause
		    more room - if the read thread has a higher priority  */
		room = (int)(iBufsiz - (iWptr - iRptr));
		if (sz  &&  !room  &&  iWblock)  { /*  there is more - block  */
			iWblk = gFindStr(Thread, NULL);
			gHold(iWblk); /*  does a yield  */
			iWblk = NULL;
		}
		if (!iWblock)
			break;
	}
	return(w);
}

imeth	int	gRead(object self, char *buf, unsigned sz)
{
	int	ba;	/*  bytes available	*/
	int	bg;	/*  bytes to get	*/
	int	tr=0;	/*  total bytes read	*/
	
	if (iRblk)
		return 0;
	while (sz)  {
		ba = (int)(iWptr - iRptr);
		bg = ba < (int) sz ? ba : (int) sz;
		if (bg)  {
			memcpy(buf, iRptr, bg);
			buf += bg;
			iRptr += bg;
			sz -= bg;
			tr += bg;
			if (iWblk)
				gRelease(iWblk, 0);
		}
/*  the following line is needed because the above release may add bytes */
		ba = (int)(iWptr - iRptr);
		if (sz  &&  !ba  &&  iRblock)  {
			iRblk = gFindStr(Thread, NULL);
			gHold(iRblk); /*  does a yield  */
			iRblk = NULL;
		}
		if (!iRblock)
			break;
	}
	return(tr);
}

imeth	char	*gGets(object self, char *buf, int sz)
{
	int	ba;	/*  bytes available	*/
	int	bg;	/*  bytes to get	*/
	int	tr=0;	/*  total bytes read	*/
	
	if (!(iWptr - iRptr) && !iRblock  ||  iRblk  ||  sz <= 0)
		return NULL;
	if (sz-- == 1)  {
		*buf = '\0';
		return buf;
	}
	while (sz)  {
		ba = (int)(iWptr - iRptr);
		for (bg=0 ; bg < ba  &&  bg < sz  &&  iRptr[bg++] != '\n' ; );
		if (iRptr[bg-1] == '\n')
			sz = bg;
		if (bg)  {
			memcpy(buf, iRptr, bg);
			buf += bg;
			iRptr += bg;
			sz -= bg;
			tr += bg;
			if (iWblk)
				gRelease(iWblk, 0);
		}
/*  the following line is needed because the above release may add bytes */
		ba = (int)(iWptr - iRptr);
		if (sz  &&  !ba  &&  iRblock)  {
			iRblk = gFindStr(Thread, NULL);
			gHold(iRblk); /*  does a yield  */
			iRblk = NULL;
		}
		if (!iRblock)
			break;
	}
	buf[tr] = '\0';
	return buf;
}

imeth	object	gDispose, gGCDispose, gDeepDispose (object self)
{
	ivType	*t, *pt;

	if (iRblk  ||  iWblk)
		return NULL;
	for (t=cMpl, pt=NULL ; t ; pt=t, t=t->iNext)
		if (t == iv)  {
			if (pt)
				pt->iNext = t->iNext;
			else
				cMpl = t->iNext;
			break;
		}
	if (iName)  {
		free(iName);
		iName = NULL;
	}
	if (iBuf)  {
		free(iBuf);
		iBuf = NULL;
	}
	return gDispose(super);
}

cmeth	object	gFindStr, <vFind> (object self, char *name)
{
	ivType	*p;
	
	USE(self);
	if (!name)
		return NULL;
	for (p=cMpl ; p ; p=p->iNext)
		if (p->iName  &&  !strcmp(p->iName, name))
			return p->iObj;
	return NULL;
}

imeth	long	gLength(object self)
{
#if 0
	if (iRblock  &&  !(iWptr - iRptr))  {
		iRblk = gFindStr(Thread, NULL);
		gHold(iRblk); /*  does a yield  */
		iRblk = NULL;
	}
#endif
	return (long)(iWptr - iRptr);
}

imeth	int	gRoom(object self)
{
#if 0
	if (iWblock  &&  !(iBufsiz - (iWptr - iRptr)))  {
		iWblk = gFindStr(Thread, NULL);
		gHold(iWblk); /*  does a yield  */
		iWblk = NULL;
	}
#endif
	return (int)(iBufsiz - (iWptr - iRptr));
}

imeth	int	gSize(object self)
{
	return iBufsiz;
}

imeth	char	*gName(object self)
{
	return iName;
}

imeth	gMode(object self, int rblock, int wblock)
{
	iRblock = rblock;
	iWblock = wblock;
	return self;
}

imeth	long	gAdvance(object self, long sz)
{
	int	ba;	/*  bytes available	*/
	int	bg;	/*  bytes to get	*/
	long	tr=0;	/*  total bytes read	*/
	
	if (iRblk)
		return 0;
	while (sz)  {
		ba = (int)(iWptr - iRptr);
		bg = (long) ba < sz ? ba : (int) sz;
		if (bg)  {
			iRptr += bg;
			sz -= bg;
			tr += bg;
			if (iWblk)
				gRelease(iWblk, 0);
		}
/*  the following line is needed because the above release may add bytes */
		ba = (int)(iWptr - iRptr);
		if (sz  &&  !ba  &&  iRblock)  {
			iRblk = gFindStr(Thread, NULL);
			gHold(iRblk); /*  does a yield  */
			iRblk = NULL;
		}
		if (!iRblock)
			break;
	}
	return(tr);
}

imeth	long	gPosition(object self)
{
	USE(self);
	return 0L;
}

imeth	int	gEndOfStream(object self)
{
	return !(iWptr - iRptr);
}





