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


static	object	(*old_gPrint)(object, object);


static  object	new_gPrint(object self, object stream)
{
	printf("\n\nxxx\n");
	old_gPrint(self, stream);
	printf("xxxx\n\n");
	return self;
}

int main(int argc, char *argv[])
{
	object	myObj;


	InitDynace(&argc);


	/*  Create an instance of the ShortInteger class */

	myObj = gNewWithInt(ShortInteger,  6);

	/*  print it out using the normal gPrint generic  */

	gPrint(myObj, stdoutStream);

	/*  save the old generic function in the variable old_gPrint  */

	old_gPrint = gPrint;

	/*  make gPrint execute new_gPrint  */

	gPrint = new_gPrint;

	/*  execute the new gPrint generic  */
	
	gPrint(myObj, stdoutStream);


	/*  return gPrint to its original generic  */

	gPrint = old_gPrint;

	/*  try it out  */

	gPrint(myObj, stdoutStream);

	gDispose(myObj);


	return 0;
}










