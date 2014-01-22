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




#include <math.h>

defclass  IntegerAssociation : Association  {
	int	iKey;
	iValue;
};

cmeth	gNewWithIntObj, <vNew> (int key, value)
{
	object	assoc;
	ivType	*iv;

	ChkArgNul(value, 3);
	assoc = gNew(super);
	iv = ivPtr(assoc);
	iKey = key;
	iValue = value;
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

imeth	int	gIntKey()
{
	return iKey;
}

imeth	gChangeIntKey(int key)
{
	iKey = key;
	return self;
}

imeth	object	gDeepDispose()
{
	if (iValue)
		gDeepDispose(iValue);
	return gDispose(super);
}

imeth	gStringRepValue()
{
	object	s, t;

	t = iValue ? gStringRepValue(iValue) : gNewWithStr(String, "(null)");
	s = vSprintf(String, "( %d, ", iKey);
	vBuild(s, NULL, t, " )", NULL);
	gDispose(t);
	return s;
}

imeth	int	gHash()
{
	double	t;

	t = .6125423371	* (unsigned) iKey;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(arg)
{
	int	sv, ov;

	ChkArg(arg, 2);
	if ((sv=iKey) < (ov=gIntKey(arg)))
		return -1;
	else if (sv == ov)
		return 0;
	else
		return 1;
}






