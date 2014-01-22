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




#include "logfile.h"
#include "hdlcache.h"


defclass  Brush  {
	HBRUSH		iHBrush;	/*  handle to brush		*/
	COLORREF	iColor;
	int		iCache;		/*  is there a valid handle to cache */
};



cvmeth	vNew(HBRUSH hbrush, COLORREF color, int cache)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iHBrush = hbrush;
	iColor  = color;
#if 0
	if (iCache = cache)
		HC_NEW(iHBrush, obj);
#endif
	return obj;
}

cmeth	gNewBrush(HBRUSH hbrush, COLORREF color, int cache)
{
	return vNew(self, hbrush, color, cache);
}

imeth	object	gDispose, gDeepDispose ()
{
#if 0
	if (iCache)
		HC_DELETE(iHBrush);
#endif
	return gDispose(super);
}

imeth	HANDLE	gHandle()
{
	return iHBrush;
}

imeth	COLORREF gColor()
{
	return iColor;
}








