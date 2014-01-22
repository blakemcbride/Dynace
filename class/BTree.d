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






defclass  BTree {
	iNodes;
	long	iNum;		//  total number of keys entered
	ifun	iCmpFun;
	CRITICALSECTION	iCS;	/*  in support of native threads  */
};

cmeth	gNew()
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	INITIALIZECRITICALSECTION(iCS);
	return obj;
}

imeth	gDeepDispose()
{
	ENTERCRITICALSECTION(iCS);
	if (iNodes)
		gDeepDispose(iNodes);
	LEAVECRITICALSECTION(iCS);
	DELETECRITICALSECTION(iCS);
	return gDispose(super);
}

imeth	gDispose()
{
	ENTERCRITICALSECTION(iCS);
	if (iNodes)
		gDispose(iNodes);
	LEAVECRITICALSECTION(iCS);
	DELETECRITICALSECTION(iCS);
	return gDispose(super);
}

imeth	gDeepDisposeAllNodes()
{
	ENTERCRITICALSECTION(iCS);
	if (iNodes)
		iNodes = gDeepDispose(iNodes);
	iNum = 0;
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gDisposeAllNodes()
{
	ENTERCRITICALSECTION(iCS);
	if (iNodes)
		iNodes = gDispose(iNodes);
	iNum = 0;
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	int	gSize()
{
	return iNum;
}

imeth	gSetTopNode(newNode)
{
	iNodes = newNode;
	return self;
}

imeth	gAddValue(key, val)
{
	object	old=NULL;
	int	replaced;
	
	ENTERCRITICALSECTION(iCS);
	if (!iNodes) {
		if (!iCmpFun)
			iCmpFun = gCompare;
		iNodes = gNewNode(BTreeNode, self, 2);
	}
	gAddBTreeNode(iNodes, iCmpFun, key, val, 1, &replaced, NULL, &old);
	if (replaced == 1)
		iNum++;
	LEAVECRITICALSECTION(iCS);
	return old;
}

imeth	gFindEQ(key, object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNEQ(iNodes, iCmpFun, key, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindGE(key, object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNGE(iNodes, iCmpFun, key, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindGT(key, object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNGT(iNodes, iCmpFun, key, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindNext(object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	if (*foundKey) {
		object	key = *foundKey;
		r = gFindBTNGT(iNodes, iCmpFun, key, foundKey);
	} else
		r = gFindFirst(self, foundKey);
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindLE(key, object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNLE(iNodes, iCmpFun, key, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindLT(key, object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNLT(iNodes, iCmpFun, key, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindPrev(object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	if (*foundKey) {
		object	key = *foundKey;
		r = gFindBTNLT(iNodes, iCmpFun, key, foundKey);
	} else
		r = gFindLast(self, foundKey);
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindFirst(object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNFirst(iNodes, iCmpFun, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gFindLast(object *foundKey)
{
	object	r;
	ENTERCRITICALSECTION(iCS);
	r = iNodes && iNum ? gFindBTNLast(iNodes, iCmpFun, foundKey) : NULLOBJ;
	LEAVECRITICALSECTION(iCS);
	return r;
}

imeth	gDisposeObj(key)
{
	object	res;
	ENTERCRITICALSECTION(iCS);
	res = iNodes && iNum ? gDeleteBTNode(iNodes, iCmpFun, key, 0, NULL) : NULL;
	if (res)
		iNum--;
	LEAVECRITICALSECTION(iCS);
	return res;
}

imeth	gDeepDisposeObj(key)
{
	object	res;
	ENTERCRITICALSECTION(iCS);
	res = iNodes && iNum ? gDeleteBTNode(iNodes, iCmpFun, key, 1, NULL) : NULL;
	if (res)
		iNum--;
	LEAVECRITICALSECTION(iCS);
	return res;
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	old = (ofun) iCmpFun;
	iCmpFun = fun;
	return old;
}

imeth	gPrint(stream)
{
	ENTERCRITICALSECTION(iCS);
	vPrintf(stream, "BTree [%8.8lx], %ld keys, first node is %8.8lx\n", self, iNum, iNodes);
	if (iNodes) {
		gPuts(stream, "\n------------------------------------------------------------\n");
		gPrint(iNodes, stream);
	}
	LEAVECRITICALSECTION(iCS);
	return self;
}

