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



#include "dpp.h"


defclass OutputStream2  {
	iS1;		/*  OutputStream 1	*/
	iS2;		/*  OutputStream 2	*/
	int	iUse;	/*  0=both, 1=S1, 2=S2  */
};

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewWithStrStr(char *file, char *ifile)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iS1 = gNewWithStrStr(OutputStream, file, ifile);
	return obj;
}

imeth	gOpenStream2(char *file, char *ifile)
{
	if (iS2)
		gDispose(iS2);
	iS2 = gNewWithStrStr(OutputStream, file, ifile);
	return self;
}

imeth	gUseStream(int n)
{
	iUse = n;
	return self;
}

imeth	int	gFlush()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gFlush(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gFlush(iS2);
	return 0;
}

/*  flush an entire method  */

imeth	gFlushm()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gFlushm(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gFlushm(iS2);
	return self;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gDispose(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gDispose(iS2);
	return gDispose(super);
}

imeth	gPut(tkn)
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gPut(iS1, tkn);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		if (iUse == 2)
			gPut(iS2, tkn);
		else
			gPut(iS2, gCopy(tkn));
	return self;
}

imeth	gPutm(tkn)
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gPutm(iS1, tkn);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		if (iUse == 2)
			gPutm(iS2, tkn);
		else
			gPutm(iS2, gCopy(tkn));
	return self;
}

imeth	int	gPuts(char *str)
{
	int	r = 0;

	if (iS1  &&  (!iUse  ||  iUse == 1))
		r = gPuts(iS1, str);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		r = gPuts(iS2, str);
	return r;
}

ivmeth	int	vPrintf(char *fmt, ...)
{
	char	buf[256];
	int	r=0;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	if (iS1  &&  (!iUse  ||  iUse == 1))
		r = vPrintf(iS1, "%s", buf);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		r = vPrintf(iS2, "%s", buf);
	return r;
}

imeth	gSetOSVars(cname, cvs, ivs)
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gSetOSVars(iS1, cname, cvs, ivs);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gSetOSVars(iS2, cname, cvs, ivs);
	return self;
}

imeth	gTLineDirective()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gTLineDirective(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gTLineDirective(iS2);
	return self;
}

imeth	gSLineDirective()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gSLineDirective(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gSLineDirective(iS2);
	return self;
}

imeth	gForceLineDirective()
{
	if (iS1  &&  (!iUse  ||  iUse == 1))
		gForceLineDirective(iS1);
	if (iS2  &&  (!iUse  ||  iUse == 2))
		gForceLineDirective(iS2);
	return self;
}


