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


defclass ObjectPool {
	iObjects;
  class:
  	cPools;
  	cCurrentPool;
};


#define	DFLT_SIZE	201


private imeth	init(int siz)
{
	iObjects = gNewWithInt(Dictionary, siz);
	return self;
}

cmeth	gNew()
{
	return init(gNew(super), DFLT_SIZE);
}

cmeth	gNewWithInt(int siz)
{
	return init(gNew(super), siz);
}

cmeth	gNewGlobalPool()
{
	return gNewGlobalPoolWithSize(self, DFLT_SIZE);
}

cmeth	gNewGlobalPoolWithSize(int siz)
{
	if (cCurrentPool) {
		if (!cPools)
			cPools = gNew(LinkObject);
		gAddFirst(cPools, cCurrentPool);
	}
	return cCurrentPool = gNewWithInt(self, siz);
}

imeth	gDispose, gDeepDispose ()
{
	object	ass;
	
	while (ass = gFirst(iObjects))
		gDisposeObject(self, (object) gPointerValue(gKey(ass)));
	gDeepDispose(iObjects);
	return gDispose(super);
}

cmeth	gDisposeGlobalPool()
{
	if (cCurrentPool) {
		cCurrentPool = gDispose(cCurrentPool);
		if (cPools  &&  gSize(cPools))
			cCurrentPool = gPop(cPools);
	}
	return cCurrentPool;
}

imeth	gAddToPool(int deepFlag, obj)
{
	object	val = gNewWithObjObj(ObjectAssociation, gNewWithInt(ShortInteger, deepFlag), gNewWithLong(LongInteger, gObjectSerialNumber(obj)));
	object	pobj = gNewWithPtr(Pointer, obj);
	if (!gAddValue(iObjects, pobj, val)) {
		gDeepDisposeObj(iObjects, pobj);
		gAddValue(iObjects, pobj, val);
	}
	return obj;
}

cmeth	gAddToPool(int deepFlag, obj)
{
	return gAddToPool(cCurrentPool, deepFlag, obj);
}

imeth	gDisposeObject(obj)
{
	object	pobj = gNewWithPtr(Pointer, obj);
	object	val = gFindValue(iObjects, pobj);
	if (val) {
		if (IsObj(obj)) {
			long	v1 = gObjectSerialNumber(obj);
			long	v2 = gLongValue(gValue(val));
			if (v1 == v2)
				gShortValue(gKey(val)) ? gDeepDispose(obj) : gDispose(obj);
		}
		gDeepDisposeObj(iObjects, pobj);
	}
	gDispose(pobj);
	return NULL;
}

cmeth	gDisposeObject(obj)
{
	return gDisposeObject(cCurrentPool, obj);
}

imeth	gRemoveFromPool(obj)
{
	object	pobj = gNewWithPtr(Pointer, obj);
	gDeepDisposeObj(iObjects, pobj);
	gDispose(pobj);
	return obj;
}

cmeth	gRemoveFromPool(obj)
{
	return gRemoveFromPool(cCurrentPool, obj);
}





