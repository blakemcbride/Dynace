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
	object	myObj;

	InitDynace(&argc);


	/*  Create and use an instance of the ShortInteger class */

	myObj = gNewWithInt(ShortInteger,  6);
	gPrint(myObj, stdoutStream);
	gChangeShortValue(myObj, 77);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);

	/*  Create and use an instance of the String class */

	myObj = gNewWithStr(String, "Hello World.");
	gPrint(myObj, stdoutStream);
	gChangeStrValue(myObj, "New String Value");
	gPrint(myObj, stdoutStream);
	gDispose(myObj);



	/*  Create and use an instance of the DoubleFloat class */

	myObj = gNewWithDouble(DoubleFloat, 3.1415926);
	gPrint(myObj, stdoutStream);
	gChangeDoubleValue(myObj, 7.2144);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);


	/*  Create and use an instance of the Character class */

	myObj = gNewWithChar(Character, 'A');
	gPrint(myObj, stdoutStream);
	gChangeCharValue(myObj, 'B');
	gPrint(myObj, stdoutStream);
	gDispose(myObj);


	/*  Create and use an instance of the Date class */

	myObj = gNewWithLong(Date, 19930802L);
	gPrint(myObj, stdoutStream);
	gChangeLongValue(myObj, 19580608L);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);

	return 0;
}










