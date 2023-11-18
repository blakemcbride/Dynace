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





defclass  Stream  {
 init:  class_init;
};

#include <string.h>


object	stdoutStream_o, stdinStream_o, stderrStream_o, traceStream_o;


imeth	int	gPuts(char *str)	/*  or String object  */
{
	if (IsObj((object)str))
		str = gStringValue((object)str);
	return gWrite(self, str, strlen(str));
}

imeth	int	gPutc(int i)
{
	char	c = (char) i;
	return 1 == (gWrite(self, &c, 1)) ? i : EOF;
}

/* The following is a imeth and not a ivmeth because all vPrintf methods take the exact same arguments.
   ivmeth and cvmeth are reserved for methods that take DIFFERENT arguments.  */

imeth	int	vPrintf(char *fmt, ...)
{
	char	buf[1024];

	buf[1020] = 'E';
	buf[1021] = 'O';
	buf[1022] = 'S';
	buf[1023] = '\0';
	vsprintf(buf, fmt, _rest_);
	if (buf[1020] != 'E'  ||
	    buf[1021] != 'O'  ||
	    buf[1022] != 'S'  ||
	    buf[1023])
		gError(Object, "vPrintf buffer overflow");
	return gWrite(self, buf, strlen(buf));
}

imeth	gCopy, gDeepCopy ()
{
	return gShouldNotImplement(self, "Copy/DeepCopy");
}

static	void	class_init(void)
{
	Dynace;
	RegisterVariable(stdoutStream_o);
	RegisterVariable(stderrStream_o);
	RegisterVariable(stdinStream_o);
	RegisterVariable(traceStream_o);
}






