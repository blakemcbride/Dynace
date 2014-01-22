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
	object	linkObject;


	InitDynace(&argc);


	/*  Create a new LinkObject */

	linkObject = gNew(LinkObject);


	/*  Print the entire link object out  */

	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the beginning of the list and print  */

	gAddFirst(linkObject, gNewWithStr(String, "The first element added."));
	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the beginning of the list and print */

	gAddFirst(linkObject, gNewWithDouble(DoubleFloat, 3.14159));
	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the end of the list and print */

	gAddLast(linkObject, gNewWithLong(LongInteger, 186282L));
	gPrint(linkObject, stdoutStream);


	/*  Print the first element of the list and the whole list again  */

	gPrint(gFirst(linkObject), stdoutStream);
	gPrint(linkObject, stdoutStream);


	/*  Dispose of last element and print list  */

	gDeepDisposeLast(linkObject);
	gPrint(linkObject, stdoutStream);


	/*  Dispose of first element and print list  */

	gDeepDisposeFirst(linkObject);
	gPrint(linkObject, stdoutStream);

	

	/*  Dispose of the entire link object and all objects held  */
	/*  (again only necessary if garbage collector not used)    */

	gDeepDispose(linkObject);
	
	
	return 0;
}










