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
	object	obj;

	InitDynace(&argc);


	/*  Create and print an instance of short integer  */

	obj = gNewWithInt(ShortInteger, 8);
	gPrint(obj, stdoutStream);


	/*  Cause an error by trying to execute a generic which is not
	    associated with the ShortInteger class  (comment out the
	    next line after running the program to allow the following
	    errors to be demonstrated).  */

	gFirst(obj);


	/*  Create another error condition where there is a missing second
	    argument (uncomment out the next line to demo the error -
	    re-comment to see the following error) */

/*	gPrint(obj);    */



	/*  Cause an error by attempting to use an object after it has
	    been disposed.  */

	gDispose(obj);
	gPrint(obj, stdoutStream);

	

	return 0;
}










