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





defclass  Link  {
	iPrev;		/*  previous link			*/
	iNext;		/*  next link				*/
	iList;		/*  list where link is a member		*/
};



imeth	gNext()
{
	return iNext == iList ? NULLOBJ : iNext;
}

imeth	gPrevious()
{
	return iPrev == iList ? NULLOBJ : iPrev;
}

imeth	gList()
{
	return iList;
}

imeth	gChangeNext(next)
{
	ChkArgNul(next, 2);
	iNext = next;
	return self;
}

imeth	gChangePrevious(prev)
{
	ChkArgNul(prev, 2);
	iPrev = prev;
	return self;
}

imeth	gInitLink(list, prev, next)
{
	ChkArgNul(list, 2);
	ChkArgNul(prev, 3);
	ChkArgNul(next, 4);
	iList = list;
	iPrev = prev;
	iNext = next;
	return self;
}

imeth	gRemove : Remove ()
{
#ifdef	NATIVE_THREADS
	object	lst;
	if (lst=iList)
		gEnterCriticalSection(iList);
#endif
	if (iNext  &&  iNext != self)
		ivPtr(iNext)->iPrev = iPrev;
	if (iPrev  &&  iPrev != self)
		ivPtr(iPrev)->iNext = iNext;
	if (iList  &&  iList != self)
		gIncNelm(iList, -1);
	iList = iNext = iPrev = NULLOBJ;
#ifdef	NATIVE_THREADS
	if (lst)
		gLeaveCriticalSection(lst);
#endif
	return self;
}

imeth	object	gDispose()
{
	Remove(self);
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	Remove(self);
	return gDeepDispose(super);
}

/*  link lnk before self	*/

imeth	gAddBefore : AddBefore (lnk)
{
	ivType	*iv2;

	ChkArg(lnk, 2);
	if (self == lnk)
		return self;
#ifdef	NATIVE_THREADS
	if (iList)
		gEnterCriticalSection(iList);
#endif
	iv2 = ivPtr(lnk);
	if (iv2->iList  ||  iv2->iNext  ||  iv2->iPrev)
		gError(self, "Attempt to add a node to a list which is a member of another list.");
	if (iv2->iPrev = iPrev)
		ivPtr(iv2->iPrev)->iNext = lnk;
	iv2->iNext = self;
	iPrev  = lnk;
	if (iv2->iList = iList)
		gIncNelm(iList, 1);
#ifdef	NATIVE_THREADS
	if (iList)
		gLeaveCriticalSection(iList);
#endif
	return self;
}

/*  link lnk after self	  */

imeth	gAddAfter : AddAfter (lnk)
{
	ivType	*iv2;

	ChkArg(lnk, 2);
	if (self == lnk)
		return self;
#ifdef	NATIVE_THREADS
	if (iList)
		gEnterCriticalSection(iList);
#endif
	iv2 = ivPtr(lnk);
	if (iv2->iList  ||  iv2->iNext  ||  iv2->iPrev)
		gError(self, "Attempt to add a node to a list which is a member of another list.");
	if (iv2->iNext = iNext)
		ivPtr(iv2->iNext)->iPrev = lnk;
	iv2->iPrev = self;
	iNext  = lnk;
	if (iv2->iList = iList)
		gIncNelm(iList, 1);
#ifdef	NATIVE_THREADS
	if (iList)
		gLeaveCriticalSection(iList);
#endif
	return self;
}

/*  move self to the beginning of the list it's on  */

imeth	gMoveBeginning()
{
	ivType	*iv2;

	if (!iList  ||  self == iList)
		return NULLOBJ;
#ifdef	NATIVE_THREADS
	gEnterCriticalSection(iList);
#endif
	iv2 = ivPtr(iList);
	if (iv2->iNext == self) {
#ifdef	NATIVE_THREADS
		gLeaveCriticalSection(iList);
#endif
		return self;	/*  already at head  */
	}
	Remove(self);
	AddAfter(iList, self);
#ifdef	NATIVE_THREADS
	gLeaveCriticalSection(iList);
#endif
	return self;
}

/*  move self to the end of the list it's on  */

imeth	gMoveEnd()
{
	ivType	*iv2;

	if (!iList  ||  self == iList)
		return NULLOBJ;
#ifdef	NATIVE_THREADS
	gEnterCriticalSection(iList);
#endif
	iv2 = ivPtr(iList);
	if (iv2->iPrev == self) {
#ifdef	NATIVE_THREADS
		gLeaveCriticalSection(iList);
#endif
		return self;	/*  already at end  */
	}
	Remove(self);
	AddBefore(iList, self);
#ifdef	NATIVE_THREADS
	gLeaveCriticalSection(iList);
#endif
	return self;
}

/*  move self after lnk  */

imeth	gMoveAfter(lnk)
{
	ChkArg(lnk, 2);
	Remove(self);
	AddAfter(lnk, self);
	return self;
}

/*  move self before lnk  */

imeth	gMoveBefore(lnk)
{
	ChkArg(lnk, 2);
	Remove(self);
	AddBefore(lnk, self);
	return self;
}

imeth	gStringRepValue()
{
	if (iList  &&  iList == self)
		return gStringRepValue(super);
	else
		return vSprintf(String, "(<%8.8lx>, <%8.8lx>)", iPrev, iNext);
}

imeth	gStringRep()
{
	object	s, t;

	s = gStringRep(super);
	t = gStringRepValue(self);
	vBuild(s, NULL, t, "\n", NULL);
	gDispose(t);
	return s;
}

imeth	gCopy, gDeepCopy ()
{
	object	nobj;
	ivType	*iv2;

	nobj = gCopy(super);
	iv2 = ivPtr(nobj);
	iv2->iList = iv2->iNext = iv2->iPrev = NULLOBJ;
	return nobj;
}

imeth	gNth(int idx)
{
	while (idx > 0  &&  self)  {
		self = gNext(self);
		idx--;
	}
	while (idx < 0  &&  self)  {
		self = gPrevious(self);
		idx++;
	}
	return idx ? NULLOBJ : self;
}





