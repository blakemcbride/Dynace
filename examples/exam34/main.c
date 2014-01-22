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



#include "generics.h"


int main(int argc, char *argv[])
{
	object	a, b, c;

	InitDynace(&argc);

	/*  I must use the garbage collector because I am not manually
	    disposing of the objects I will be creating.  */
	
	gSetMemoryBufferArea(Dynace, 50000L);


	a = gNewWithInt(ShortInteger, 7);
	b = gNewWithDouble(DoubleFloat, 3.131);

	gPrint(a, stdoutStream);
	gPrint(b, stdoutStream);

	c = gFormatNumber(a, "", 0, 2);
	gPrint(c, stdoutStream);

	c = gFormatNumber(b, "", 0, 2);
	gPrint(c, stdoutStream);
	
	b = gNewWithDouble(DoubleFloat, 2345678.87);
	c = gFormatNumber(b, "CD", 0, 2);
	gPrint(c, stdoutStream);

	a = gToday(Date);
	c = gFormatDate(a, "%N/%D/%y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%N/%D/%Y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%W  %M %d%s, %Y  %T");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%m. %d, %Y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%W the %d%s of %M");
	gPrint(c, stdoutStream);

	return 0;
}










