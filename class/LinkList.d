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





defclass  LinkList : Link  {
	int	iNelm;
	CRITICALSECTION	iCS;	/*  in support of native threads  */
	int	iCSDisposed;
};

#include <string.h>

#define FIRST	gNext
#define LAST	gPrevious

cmeth	gNew, <vNew> ()
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	INITIALIZECRITICALSECTION(iCS);
	gInitLink(obj, obj, obj, obj);
	return obj;
}

imeth	gIncNelm(int inc)
{
	if (0 > (iNelm += inc))
		iNelm = 0;
	return self;
}

imeth	int	gSize()
{
	return iNelm;
}

imeth	gFirst()
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = FIRST(self);
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gLast()
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = LAST(self);
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gAddFirst(lnk)
{
	ChkArg(lnk, 2);
	ENTERCRITICALSECTION(iCS);
	gAddAfter(self, lnk);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gAddLast(lnk)
{
	ChkArg(lnk, 2);
	ENTERCRITICALSECTION(iCS);
	gAddBefore(self, lnk);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gInsertObjAt(lnk, int idx)
{
	ChkArg(lnk, 2);
	ENTERCRITICALSECTION(iCS);
	if (!idx  ||  idx < -iNelm)
		gAddAfter(self, lnk);  	// beginning
	else if (idx == -1  ||  idx >= iNelm)
		gAddBefore(self, lnk);	// end
	else if (idx > 0)
		gAddAfter(gNth(self, idx), lnk);
	else
		gAddBefore(gNth(self, idx+1), lnk);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gDisposeFirst()
{
	ENTERCRITICALSECTION(iCS);
	if (iNelm)
		gDispose(FIRST(self));
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gDeepDisposeFirst()
{
	ENTERCRITICALSECTION(iCS);
	if (iNelm)
		gDeepDispose(FIRST(self));
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gRemoveFirst()
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNelm ? gRemove(FIRST(self)) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gDisposeLast()
{
	ENTERCRITICALSECTION(iCS);
	if (iNelm)
		gDispose(LAST(self));
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gDeepDisposeLast()
{
	ENTERCRITICALSECTION(iCS);
	if (iNelm)
		gDeepDispose(LAST(self));
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gRemoveLast()
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	if (iNelm)
		r = gRemove(LAST(self));
	else
		r = NULL;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gDisposeAllNodes()
{
	object	obj;

	ENTERCRITICALSECTION(iCS);
	while (obj = FIRST(self))
		gDispose(obj);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gDeepDisposeAllNodes()
{
	object	obj;

	ENTERCRITICALSECTION(iCS);
	while (obj = FIRST(self))
		gDeepDispose(obj);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	object	gDispose()
{
	gDisposeAllNodes(self);
	DELETECRITICALSECTION(iCS);
	iCSDisposed = 1;
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	gDeepDisposeAllNodes(self);
	DELETECRITICALSECTION(iCS);
	iCSDisposed = 1;
	return gDeepDispose(super);
}

imeth	gSequence()
{
	return gNewWithObj(LinkSequence, FIRST(self));
}

imeth	gCopy()
{
	object	nobj;
	ivType	*iv2;
	object	lnk;

	ENTERCRITICALSECTION(iCS);
	nobj = gCopy(super);
	iv2 = ivPtr(nobj);

	memset(&iv2->iCS, 0, sizeof iCS);
	INITIALIZECRITICALSECTION(iv2->iCS);

	iv2->iNelm = 0;
	gInitLink(nobj, nobj, nobj, nobj);
	for (lnk=FIRST(self) ; lnk ; lnk = gNext(lnk))
		gAddBefore(nobj, gCopy(lnk));
	LEAVECRITICALSECTION(iCS);
	return nobj;
}

imeth	gDeepCopy()
{
	object	nobj;
	ivType	*iv2;
	object	lnk;

	ENTERCRITICALSECTION(iCS);
	nobj = gDeepCopy(super);
	iv2 = ivPtr(nobj);

	memset(&iv2->iCS, 0, sizeof iCS);
	INITIALIZECRITICALSECTION(iv2->iCS);

	iv2->iNelm = 0;
	gInitLink(nobj, nobj, nobj, nobj);
	for (lnk=FIRST(self) ; lnk ; lnk = gNext(lnk))
		gAddBefore(nobj, gDeepCopy(lnk));
	LEAVECRITICALSECTION(iCS);
	return nobj;
}

imeth	gStringRep()
{
	object	lnk, s, t;

	ENTERCRITICALSECTION(iCS);
	s = gStringRepValue(super);
	gAppend(s, (object) "  (");
	for (lnk = FIRST(self) ; lnk ; )  {
		t = gStringRepValue(lnk);
		if (lnk = gNext(lnk))
			vBuild(s, NULL, t, ", ", NULL);
		else
			gAppend(s, t);
		gDispose(t);
	}
	gAppend(s, (object) ")\n");
	LEAVECRITICALSECTION(iCS);
	return s;
}

imeth	gEnterCriticalSection()
{
	if (!iCSDisposed)
		ENTERCRITICALSECTION(iCS);
	return self;
}

imeth	gLeaveCriticalSection()
{
	if (!iCSDisposed)
		LEAVECRITICALSECTION(iCS);
	return self;
}






