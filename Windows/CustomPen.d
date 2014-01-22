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


defclass  CustomPen : Pen  {
	int	iStyle;		/*  style  */
	int	iWidth;
};


cvmeth	vNew(int red, int green, int blue, int style, int width)
{
	HPEN	hpen;
	object	obj;
	ivType	*iv;

	hpen = CreatePen(style, width, RGB(red, green, blue));
	if (!hpen)
		return NULL;
	obj = vNew(super, hpen, RGB(red, green, blue), 1);
	if (!obj)
		return NULL;
	iv = ivPtr(obj);
	iStyle = style;
	iWidth = width;
	return obj;
}

imeth	object	gDispose, gDeepDispose, gGCDispose ()
{
	DeleteObject(gHandle(self));
	return gDispose(super);
}

imeth	gCopy, gDeepCopy ()
{
	COLORREF	color = gColor(self);

	return vNew(ClassOf(self), (int) GetRValue(color), 
			     (int) GetGValue(color),
			     (int) GetBValue(color),
			     iStyle, iWidth);
}








