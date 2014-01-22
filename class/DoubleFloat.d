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


defclass  DoubleFloat : Number  {
	double	iVal;
};


#ifdef	WIN32
#define	fixnan(x)	(_isnan(x) ? 0.0 : (x))
#else
#define	fixnan(x)	(x)
#endif


cmeth	gNewWithDouble, <vNew> (double val)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iVal = fixnan(val);
	return(obj);
}

imeth	gStringRepValue()
{
	return vSprintf(String, "%f", iVal);
}

imeth	char	gCharValue()
{
	return (char) iVal;
}

imeth	short	gShortValue()
{
	return (short) iVal;
}

imeth	unsigned short	gUnsignedShortValue()
{
	return (unsigned short) iVal;
}

imeth	long	gLongValue()
{
	return (long) iVal;
}

imeth	double	gDoubleValue()
{
	return fixnan(iVal);
}

imeth	void	*gPointerValue()
{
	return (void *) &iVal;
}

imeth	gChangeValue(val)
{
	ChkArg(val, 2);
	iVal = gDoubleValue(val);
	return self;
}

imeth	gChangeCharValue(int val)
{
	iVal = (double) val;
	return self;
}

imeth	gChangeShortValue(int val)
{
	iVal = (double) val;
	return self;
}

imeth	gChangeUShortValue(unsigned val)
{
	iVal = (double) val;
	return self;
}

imeth	gChangeLongValue(long val)
{
	iVal = (double) val;
	return self;
}

imeth	gChangeDoubleValue(double val)
{
	iVal = fixnan(val);
	return self;
}

imeth	gRound(int p)	/*  round n to p places	 */
{
	double	r;

	r = pow(10.0, (double) p);
	if (iVal < 0.0)
		iVal = -(floor(.5 + -iVal * r) / r);
	else
		iVal = floor(.5 + iVal * r) / r;
	return self;
}

imeth	gTruncate(int p)		/* truncate n to p places  */
{
	double	r;

	r = pow(10.0, (double) p);
	if (iVal < 0.0)
		iVal = -(floor(-iVal * r) / r);
	else
		iVal = floor(iVal * r) / r;
	return self;
}

imeth	int	gHash()
{
	double	t;

	t = .6125423371	* iVal;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(obj)
{
	double	sv, ov;

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



