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






defclass  Dictionary : Set  {
 init:	init_class;
};


#include "set1.h"


#define LTYPE	1	/*  lookup type	*/

static	gLookup_t	Lookup;		/*  locally cached  */

imeth	gAddValue, <vAdd> (key, value)
{
	ChkArg(key, 2);
	ChkArgNul(value, 3);
	return Lookup(self, key, HT_ADD, 0, LTYPE, value);
}

imeth	gFindValue, <vFindValue> (key)
{
	object	x;

	ChkArg(key, 2);
	x = Lookup(self, key, HT_FIND, 0, LTYPE, NULL);
	return x ? gValue(x) : x;
}

imeth	gFind, <vFind> (key)
{
	ChkArg(key, 2);
	return Lookup(self, key, HT_FIND, 0, LTYPE, NULL);
}

imeth	gChangeValueWithObj, <vChangeValue> (key, val)
{
	object	x;
	ChkArg(key, 2);
	ChkArgNul(val, 3);
	x = Lookup(self, key, HT_FIND, 0, LTYPE, NULL);
	return x ? gChangeValue(x, val) : x;
}

imeth	gFindAddValue, <vFindAdd> (key, value)
{
	ChkArg(key, 2);
	ChkArgNul(value, 3);
	return Lookup(self, key, HT_FINDADD, 0, LTYPE, value);
}

imeth	object	gDispose()
{
	return gDispose1(self);
}

imeth	object	gDisposeAllNodes()
{
	return gDisposeAllNodes1(self);
}

imeth	gRemoveObj, <vRemove> (key)
{
	ChkArg(key, 2);
	return Lookup(self, key, HT_DELETE, 1, LTYPE, NULL);
}

imeth	gDeepDisposeObj, gDisposeObj (key)
{
	ChkArg(key, 2);
	return Lookup(self, key, HT_DELETE, 2, LTYPE, NULL);
}

static	void	init_class(void)
{
	Lookup = imcPointer(Set, gLookup);
}






