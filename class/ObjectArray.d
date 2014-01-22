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


defclass  ObjectArray : Array  {
 init:	init_class;
};



#define TYPE	object


static	gIndex_t	_index;


cvmeth	vNew(unsigned rank, ...)
{
	static	gNewArray_t	cnew = NULL;
	MAKE_REST(rank);

	if (!cnew)
		cnew = cmcPointer(Array, gNewArray);
	return cnew(self, AT_OBJ, rank, _rest_);
}

ivmeth	vValue(...)
{
	MAKE_REST(self);
	return *((TYPE *) _index(self, _rest_));
}

ivmeth	vChangeValue(val, ...)
{
	MAKE_REST(val);
	ChkArgNul(val, 2);
	*((TYPE *) _index(self, _rest_)) = val;
	return self;
}

static	void	init_class(void)
{
	_index = imcPointer(Array, gIndex);
}








