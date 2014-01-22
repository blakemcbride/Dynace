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




defclass  LinkObject : LinkList;


#define FIRST	gNext
#define LAST	gPrevious

cvmeth	vMakeList(...)
{
	object	lst, obj;
	MAKE_REST(self);

	lst = gNew(self);
	while (obj = GetArg(object))
		gAddBefore(lst, gNewWithObj(LinkValue, obj));  //  same as gAddLast
	return lst;
}

ivmeth	vGetValues(...)
{
	object	*var, seq, obj;
	MAKE_REST(self);

	seq = gSequence(self);
	obj = gNext(seq);
	while (var = GetArg(object *)) {
		*var = obj;
		if (obj)
			obj = gNext(seq);
	}
	if (obj)
		gDispose(seq);
	return self;
}

imeth	gAddFirst, gPush(obj)
{
	object	lnk;

	ChkArgNul(obj, 2);
	lnk = gNewWithObj(LinkValue, obj);
/*	gAddFirst(super, lnk);  */
	gAddAfter(self, lnk);
	return self;
}

imeth	gPop()
{
/*	object	lnk = gFirst(super);   */
	object	lnk = FIRST(self);
	if (lnk) {
		lnk = gValue(lnk);
		gDisposeFirst(self);
	}
	return lnk;
}

imeth	gFirst()
{
/*	object	lnk = gFirst(super);   */
	object	lnk = FIRST(self);
	return lnk ? gValue(lnk) : NULLOBJ;
}

imeth	gAddLast(obj)
{
	object	lnk;

	ChkArgNul(obj, 2);
	lnk = gNewWithObj(LinkValue, obj);
/*	gAddLast(super, lnk);  */
	gAddBefore(self, lnk);
	return self;
}

imeth	gLast()
{
/*	object	lnk = gLast(super);  */
	object	lnk = LAST(self);
	return lnk ? gValue(lnk) : NULLOBJ;
}

imeth	gSequence()
{
	return gNewWithObj(LinkObjectSequence, FIRST(self));
}

imeth	gSequenceLinks()
{
	return gNewWithObj(LinkSequence, FIRST(self));
}

imeth	gNth(int idx)
{
	object	r = gNth(super, idx);
	return r ? gValue(r) : NULLOBJ;
}







