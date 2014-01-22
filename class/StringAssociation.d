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




#include <string.h>
#include <math.h>
#include "memalloc.h"


defclass  StringAssociation : Association  {
	iKey;
	char	*iKeysp;
	iValue;
};

private	imeth	init(char *key, val)
{
	iKey = gNewWithStr(String, key);
	iKeysp = gStringValue(iKey);
	iValue = val;
	return self;
}

cmeth	gNewWithStrObj, <vNew> (char *key, val)
{
	return init(gNew(super), key, val);
}

imeth	gDeepCopy()
{
	return gNewWithStrObj(CLASS, iKeysp, iValue?gDeepCopy(iValue):NULL);
}

imeth	gCopy()
{
	return gNewWithStrObj(CLASS, iKeysp, iValue);
}

imeth	gKey()
{
	return iKey;
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

imeth	char	*gStringKey()
{
	return iKeysp;
}

imeth	gChangeStringKey(char *key)
{
	gDispose(iKey);
	iKey = gNewWithStr(String, key);
	iKeysp = gStringValue(iKey);
	return self;
}

imeth	object	gDispose()
{
	gDispose(iKey);
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	gDispose(iKey);
	if (iValue)
		gDeepDispose(iValue);
	return gDeepDispose(super);
}

imeth	gStringRepValue()
{
	object	s, t;
	
	t = iValue ? gStringRepValue(iValue) : (object) NULL;
	s = vSprintf(String, "( \"%s\", ", iKeysp ? iKeysp : "(null)");
	vBuild(s, NULL, t ? (char *) t : "(null)", " )", NULL);
	if (t)
		gDispose(t);
	return s;
}

imeth	int	gHash()
{
	register char	 c = 'a';
	char	*s = iKeysp;
	double	t;
	register unsigned short	 k=0;  /* must be short	 */

	if (s)
		while (*s)
			k += *s++ ^ c++;
	t = .6125423371	* k;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(arg)
{
	ChkArg(arg, 2);
	return strcmp(iKeysp?iKeysp:"", gStringKey(arg));
}






