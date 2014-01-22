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





defclass  LinkValue : Link  {
	iValue;		/*  value referenced by this link	*/
};


cmeth	gNewWithObj, <vNew> (value)
{
	object	lnk;
	ivType	*iv;

	ChkArgNul(value, 2);
	lnk = gNew(super);
	iv = ivPtr(lnk);
	iValue = value;
	return lnk;
}

imeth	gDeepCopy()
{
	object	nobj;

	nobj = gDeepCopy(super);
	ivPtr(nobj)->iValue = iValue ? gDeepCopy(iValue) : NULL;
	return nobj;
}

imeth	object	gDeepDispose()
{
	if (iValue)
		gDeepDispose(iValue);
	return gDeepDispose(super);
}

imeth	gValue()
{
	return iValue;
}

imeth	gChangeValue(value)
{
	ChkArgNul(value, 2);
	iValue = value;
	return self;
}

imeth	gStringRepValue()
{
	return iValue ? gStringRepValue(iValue) : gNewWithStr(String, "(null)");
}

imeth	gStringRep()
{
	object	a, b, s;

	a = gStringRepValue(super);
	b = iValue ? gStringRepValue(iValue) : gNewWithStr(String, "(null)");
	s = vBuild(String, gName(ClassOf(self)), " (", a, ", ", b, ")\n", NULL);
	gDispose(a);
	gDispose(b);
	return s;
}




