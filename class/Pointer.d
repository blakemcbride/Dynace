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
#include <stdint.h>

defclass  Pointer  {
	void	*iVal;
};

cmeth	gNewWithPtr, <vNew> (void *val)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iVal = val;
	return(obj);
}

imeth	gDeepDispose()
{
	if (iVal  &&  IsObj(iVal))
		gDeepDispose(iVal);
	return gDeepDispose(super);
}

imeth	void	*gPointerValue()
{
	return iVal;
}

imeth	gChangeValue(val)
{
	iVal = (void *) val;
	return self;
}

imeth	int	gHash()
{
	double	t;

	t = .6125423371	* (double) (intptr_t) iVal;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(obj)
{
	void	*sv, *ov;

	ChkArg(obj, 2);
	if (ClassOf(obj) != CLASS)
		return gCompare(super, obj);
	if ((sv=iVal) < (ov=ivPtr(obj)->iVal))
		return -1;
	else if (sv == ov)
		return 0;
	else
		return 1;
}

imeth	gStringRepValue()
{
	return vSprintf(String, "%lp", iVal);
}



