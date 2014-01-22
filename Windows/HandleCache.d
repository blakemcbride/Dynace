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


defclass  HandleCache  {
	iObj;			/*  object being cached  */
	int	iN;		/*  number of times cached
				    (used for stock objects)  */
	iCachePtrs;		/*  linked list of places its being
				    cached  */
 class:
	cHandles[CACHE_TYPES];	/*  get objects from handles  */
 init:	init_class;
};	


cmeth	gAddHandle(int type, HWND hwnd, obj)
{
	object	obj2;
	ivType	*iv;

	obj2 = gFindValueInt(cHandles[type], (unsigned) hwnd);
	if (obj2)     /*  dup load of stock object  */
		ivPtr(obj2)->iN++;
	else  {
		obj2 = vNew(super);
		iv = ivPtr(obj2);
		iObj = obj;
		iCachePtrs = gNew(LinkList);
		iN = 1;
		gAddInt(cHandles[type], (unsigned) hwnd, obj2);
	}
	return obj2;
}

cmeth	gAddCache(int 		type,
		  HWND 		hwnd, 
		  object	*lp)	/*  app's link pointer  */
{
	object	obj;
	ivType	*iv;
	object	lnk;

	if (*lp)
		gDeepDispose(*lp);
	obj = gFindValueInt(cHandles[type], (unsigned) hwnd);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);
	gAddFirst(iCachePtrs, vNew(HandleCacheNode, lp));
	return iObj;
}

cmeth	gGetObject(int type, HWND h)
{
	object	i;
	
	i = gFindValueInt(cHandles[type], (unsigned) h);
	if (!i)
		return NULL;
	return ivPtr(i)->iObj;
}

cmeth	gDeleteHandle(int type, HWND h)
{
	ivType	*iv;
	object	i, ll, f;
	
	i = gFindValueInt(cHandles[type], (unsigned) h);
	if (!i)
		return NULL;
	iv = ivPtr(i);
	if (--iN)
		return self;
	ll = iCachePtrs;
	while (f = gFirst(ll))
		gDeepDispose(f);
	gDeepDispose(ll);
	gRemoveInt(cHandles[type], (unsigned) h);
	gDispose(super i);
	return NULL;
}

static	void	init_class()
{
	int	i;

	for (i=0 ; i < CACHE_TYPES ; ++i)
		cHandles[i] = gNewWithInt(IntegerDictionary, 99);
}









