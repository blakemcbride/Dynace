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




defclass PropertyList {
	iPropertyList;
};


imeth	gPropertyPut(char *prop, int autoDispose, val)
{
	object	intAssoc;
	
	if (!iPropertyList)
		iPropertyList = gNew(StringDictionary);
	if (!gAddStr(iPropertyList, prop, intAssoc=gNewWithIntObj(IntegerAssociation, autoDispose, val))) {
		object	old = gChangeValueWithStr(iPropertyList, prop, intAssoc);
		if (gIntKey(old))
			gDeepDispose(old);
		else
			gDispose(old);
	}
	return val;
}

imeth	gPropertyGet(char *prop)
{
	object	v = NULL;

	if (iPropertyList) {
		v = gFindValueStr(iPropertyList, prop);
		if (v)
			v = gValue(v);
	}
	return v;
}

imeth	gPropertySetAutoDispose(char *prop, int autoDispose)
{
	object	v = NULL;

	if (iPropertyList) {
		v = gFindValueStr(iPropertyList, prop);
		if (v)
			gChangeIntKey(v, autoDispose);
	}
	return v;
}

imeth	gPropertyRemove(char *prop)
{
	object	strAssoc, intAssoc;
	
	if (!iPropertyList)
		return NULL;
	strAssoc = gFindStr(iPropertyList, prop);
	if (!strAssoc)
		return NULL;
	intAssoc = gValue(strAssoc);
	if (gIntKey(intAssoc))
		gDeepDisposeStr(iPropertyList, gStringKey(strAssoc));
	else {
		gRemoveStr(iPropertyList, gStringKey(strAssoc));
		gDispose(intAssoc);
	}
	return self;
}

imeth	gDisposePropertyList()
{
	object	strAssoc, intAssoc;

	if (!iPropertyList)
		return NULL;
	while (strAssoc = gFirst(iPropertyList)) {
		intAssoc = gValue(strAssoc);
		if (gIntKey(intAssoc))
			gDeepDisposeStr(iPropertyList, gStringKey(strAssoc));
		else {
			gRemoveStr(iPropertyList, gStringKey(strAssoc));
			gDispose(intAssoc);
		}
	}
	iPropertyList = gDeepDispose(iPropertyList);
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gSetPropertyList(pl)
{
	object	old = iPropertyList;
	iPropertyList = pl;
	return old;
}

imeth	gCopy, gDeepCopy ()
{
	object	copy = gCopy(super);
	if (iPropertyList)
		gSetPropertyList(copy, gDeepCopy(iPropertyList));
	return copy;
}






