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


defclass  HatchBrush : Brush  {
	int	iStyle;		/*  hatch style       */
	int	iCopies;	/*  number of copies  */
};


cvmeth	vNew(int red, int green, int blue, int style)
{
	HBRUSH	hbrush;
	object	obj;
	ivType	*iv;

	hbrush = CreateHatchBrush(style, RGB(red, green, blue));
	if (!hbrush)
		return NULL;
	obj = vNew(super, hbrush, RGB(red, green, blue), 1);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);
	iStyle = style;
	iCopies = 1;
	return obj;
}

cmeth	gNewHatchBrush(int red, int green, int blue, int style)
{
	return vNew(self, red, green, blue, style);
}

imeth	object	gDispose, gDeepDispose ()
{
	if (--iCopies)
		return NULL;
	DeleteObject(gHandle(self));
	return gDispose(super);
}

imeth	object	gGCDispose()
{
	DeleteObject(gHandle(self));
	return gDispose(super);
}

imeth	gCopy, gDeepCopy ()
{
	iCopies++;
	return self;
#if 0
	COLORREF	color = gColor(self);

	return vNew(ClassOf(self), (int) GetRValue(color), 
			     (int) GetGValue(color),
			     (int) GetBValue(color),
			     iStyle);
#endif
}








