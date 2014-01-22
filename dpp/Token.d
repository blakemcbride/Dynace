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


defclass  Token : Link  {
	iToken;			/*  String rep of token		*/
	long	iLine;		/*  source line found on or 0	*/
	int	iSpace;		/*  1=following char is space   */
};


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewToken(char *tkn, long ln, int sp)
{
	object	obj;
	ivType	*iv;

	obj = gNew(super);
	iv = ivPtr(obj);
	iToken = gNewWithStr(String, tkn);
	iLine  = ln;
	iSpace = sp;
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iToken);
	return gDispose(super);
}

imeth	char	*gStringValue()
{
	return gStringValue(iToken);
}

imeth	gToken()
{
	return iToken;
}

imeth	gChangeToken(char *str)
{
	gChangeStrValue(iToken, str);
	return self;
}

imeth	long	gLineNumber()
{
	return iLine;
}

imeth	int	gSpace()
{
	return iSpace;
}

imeth	gCopy, gDeepCopy ()
{
	object	obj = gCopy(super);
	ivType	*niv = ivPtr(obj);
	niv->iToken = gCopy(iToken);
	return obj;
}

imeth	gStringRepValue()
{
	return gStringRepValue(iToken);
}

imeth	gStringRep()
{
	object	a, b, s;

	a = gStringRepValue(super);
	b = gStringRepValue(iToken);
	s = vBuild(String, gName(ClassOf(self)), " (", a, ", ", b, ")\n", NULL);
	DISPOSE(a);
	DISPOSE(b);
	return s;
}


