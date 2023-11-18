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




#include "array1.h"


defclass  DoubleFloatArray : NumberArray  {
 init:	init_class;
};


#define TYPE	double


static	gIndex_t	_index;


cvmeth	vNew(unsigned rank, ...)
{
	static	gNewArray_t	cnew = NULL;

	if (!cnew)
		cnew = cmcPointer(Array, gNewArray);
	return cnew(self, AT_DBLE, rank, _rest_);
}

ivmeth	char	vCharValue(...)
{
	return (char) *((TYPE *) _index(self, _rest_));
}

ivmeth	short	vShortValue(...)
{
	return (short) *((TYPE *) _index(self, _rest_));
}

ivmeth	unsigned short	vUnsignedShortValue(...)
{
	return (unsigned short) *((TYPE *) _index(self, _rest_));
}

ivmeth	long	vLongValue(...)
{
	return (long) *((TYPE *) _index(self, _rest_));
}

ivmeth	double	vDoubleValue(...)
{
	return (double) *((TYPE *) _index(self, _rest_));
}

ivmeth	vChangeValue(val, ...)
{
	ChkArg(val, 2);
	*((TYPE *) _index(self, _rest_)) = gDoubleValue(val);
	return self;
}

ivmeth	vChangeCharValue(int val, ...)
{
	*((TYPE *) _index(self, _rest_)) = (TYPE) val;
	return self;
}

ivmeth	vChangeShortValue(int val, ...)
{
	*((TYPE *) _index(self, _rest_)) = (TYPE) val;
	return self;
}

ivmeth	vChangeUShortValue(unsigned val, ...)
{
	*((TYPE *) _index(self, _rest_)) = (TYPE) val;
	return self;
}

ivmeth	vChangeLongValue(long val, ...)
{
	*((TYPE *) _index(self, _rest_)) = (TYPE) val;
	return self;
}

ivmeth	vChangeDoubleValue(double val, ...)
{
	*((TYPE *) _index(self, _rest_)) = (TYPE) val;
	return self;
}
	
static	void	init_class(void)
{
	_index = imcPointer(Array, gIndex);
}








