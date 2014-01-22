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
	object	strDict;


	InitDynace(&argc);


	/*  Create a new StringDictionary to hold roughly 49 elements.
	    Note that although the dictionary is set up to hold roughly 49
	    elements, it may actually hold any number,  it's just that
	    the effeciency of the dictionary will start to go down at
	    arount 49 elements.  */

	strDict = gNewWithInt(StringDictionary, 49);


	/*  Print the entire link object out  */

	gPrint(strDict, stdoutStream);


	/*  Add a new object to the dictionary and print  */

	gAddStr(strDict, "Key 1", gNewWithStr(String, "The first value added."));
	gPrint(strDict, stdoutStream);


	/*  Add a new object to the dictionary and print */

	gAddStr(strDict, "Other key", gNewWithDouble(DoubleFloat, 3.14159));
	gPrint(strDict, stdoutStream);

	/*  Add a new object to dictionary and print */

	gAddStr(strDict, "abcd", gNewWithLong(LongInteger, 186282L));
	gPrint(strDict, stdoutStream);


	/*  Find and print the value associated with "Other key" */

	gPrint(gFindValueStr(strDict, "Other key"), stdoutStream);


	/*  Dispose of one of the key/value pairs and print dictionary */

	gDeepDisposeStr(strDict, "Key 1");
	gPrint(strDict, stdoutStream);


	/*  Dispose of the entire link object and all objects held  */
	/*  (again only necessary of garbage collector not used)    */

	gDeepDispose(strDict);
	
	
	return 0;
}










