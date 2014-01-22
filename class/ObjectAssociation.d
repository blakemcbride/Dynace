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





defclass  ObjectAssociation : LookupKey  {
	iValue;
};

cmeth	gNewWithObjObj, <vNew> (key, val)
{
	object	assoc;
	ivType	*iv;

	ChkArgNul(key, 2);
	ChkArgNul(val, 3);
	assoc = gNewWithObj(super, key);
	iv = ivPtr(assoc);
	iValue = val;
	return assoc;
}

imeth	gDeepCopy()
{
	object	nobj;

	nobj = gDeepCopy(super);
	ivPtr(nobj)->iValue = iValue ? gDeepCopy(iValue) : NULL;
	return nobj;
}

imeth	gValue()
{
	return iValue;
}

imeth	gChangeValue(value)
{
	object	ret;
	ChkArgNul(value, 2);
	ret = iValue;
	iValue = value;
	return ret;
}

imeth	object	gDeepDispose()
{
	if (iValue)
		gDeepDispose(iValue);
	return gDeepDispose(super);
}

imeth	gStringRepValue()
{
	object	s, k, v;

	k = gKey(self);
	k = k ? gStringRepValue(k) : gNewWithStr(String,"(null)");
	v = iValue ? gStringRepValue(iValue) : gNewWithStr(String,"(null)");
	s = vBuild(String, "( ", k, ", ", v, " )", NULL);
	gDispose(k);
	gDispose(v);
	return s;
}






