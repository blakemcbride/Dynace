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





defclass  Semaphore  {
	object	iObj;			/*  main object pointer	*/
	char	*iName;			/*  semaphore name	*/
	int	iCount;
	int	iMaximum;
	object	iWaiting_threads;	/*  LinkObject of waiting threads */
	struct _Semaphore_iv_t *iNext;	/*  linked list of semaphores	*/
 class:
	struct _Semaphore_iv_t *cMsl;	/*  master semaphore list	*/
};


#include <string.h>


cmeth	gNewSemaphore, <vNew> : Semaphore_New (object self, char *name, int cnt, int mx)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	
	iObj = obj;
	if (name)  {
		iName = Tnalloc(char, strlen(name)+1);
		strcpy(iName, name);
	}
	iCount = cnt;
	iMaximum = mx;
	iNext = cMsl;
	cMsl = iv;
	return obj;
}

cmeth	gNew()
{
	return Semaphore_New(self, NULL, 1, 1);
}
		
imeth	int	gWaitFor(object self)
{
	object	ct;

	INHIBIT_THREADER;
	if (!(ct = gFindStr(Thread, NULL)))  {
		ENABLE_THREADER;
		return 0;
	}
	if (iCount)  {
		iCount--;
		ENABLE_THREADER;
		return 0;
	}
	gWaitSemaphore(ct, self);
	
	if (!iWaiting_threads)
		iWaiting_threads = gNew(LinkObject);
	gAddLast(iWaiting_threads, ct);
	ENABLE_THREADER;

	__dynace_yield();
	return 0;
}

imeth	gRelease : Release (object self, int cnt)
{
	object	thread;

	iCount += cnt;
	if (iCount > iMaximum)
		iCount = iMaximum;
	INHIBIT_THREADER;
	while (iCount  &&  iWaiting_threads  &&  (thread = gFirst(iWaiting_threads)))  {
		gDisposeFirst(iWaiting_threads);
		gReleaseSemaphore(thread);
		iCount--;
	}
	ENABLE_THREADER;
	return self;
}

imeth	object	gDispose, gDeepDispose (object self)
{
	ivType	*t, *p;
	
	INHIBIT_THREADER;
	while (iWaiting_threads  &&  gFirst(iWaiting_threads))
		Release(self, iMaximum);
	if (iWaiting_threads)
		gDispose(iWaiting_threads);
	if (iName)
		free(iName);
	for (p=NULL, t=cMsl ; t ; p=t, t=t->iNext)
		if (t == iv)  {
			if (p)
				p->iNext = t->iNext;
			else
				cMsl = t->iNext;
			break;
		}
	gDispose(super);
	ENABLE_THREADER;
	return NULL;
}

imeth	object	gGCDispose(object self)
{
	ivType	*t, *p;
	
	INHIBIT_THREADER;
	if (iWaiting_threads  &&  gFirst(iWaiting_threads))  {
		ENABLE_THREADER;
		return NULL;
	}
	if (iName)
		free(iName);
	for (p=NULL, t=cMsl ; t ; p=t, t=t->iNext)
		if (t == iv)  {
			if (p)
				p->iNext = t->iNext;
			else
				cMsl = t->iNext;
			break;
		}
	gDispose(super);
	ENABLE_THREADER;
	return NULL;
}

cmeth	gFindStr, <vFind> (object self, char *name)
{
	ivType	*iv;
	
	USE(self);
	if (!name)
		return NULL;
	for (iv=cMsl ; iv ; iv=iNext)
		if (iName  &&  !strcmp(iName, name))
			return iObj;
	return NULL;
}

imeth	int	gCount(object self)
{
	return iCount;
}

imeth	char	*gName(object self)
{
	return iName;
}

/*  the following method is used by the threader  */

imeth	gRemoveWaits(object self, object thrd)
{
	object	linkSequence, linkValue, thread;

	if (!iWaiting_threads)
		return self;
	INHIBIT_THREADER;
	linkSequence = gSequenceLinks(iWaiting_threads);
	while (linkValue = gNext(linkSequence))  {
		thread = gValue(linkValue);
		if (thread == thrd)  {
			gDispose(linkValue);
			break;
		}
	}
	if (linkValue)
		gDispose(linkSequence);
	ENABLE_THREADER;
	return self;
}

imeth	gCopy, gDeepCopy (object self)
{
	return gShouldNotImplement(self, "Copy/DeepCopy");
}


