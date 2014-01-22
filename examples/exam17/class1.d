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


#include <string.h>

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
  class:
  	int	cNumInstances;
};


cmeth	gNewWithInt(int code)
{
	/*  Declare an object which will hold the new instance object being
	    created.  */
	object	obj;

	/*  Declare a pointer which will point to the instance variables
	    associated with the new instance.  'ivType' refers to a C
	    structure which looks like the instance variable structure
	    defined in the defclass.  The name of the pointer must be 'iv'.
	    This is so Dynace knows how to access the instance variables
	    when they are refered to without the normal C iv-> construct.  */
	ivType	*iv;

	/*  Increment the count as before.  */
	cNumInstances++;

	/*  Create the new instance object as before, only this time put it
	    in obj.  */
	obj = gNew(super);

	/*  Gain access to a pointer to the locally defined instance variables
	    associated with obj and assign it to iv.  Note that this procedure,
	    which is required to access instance variables, is performed
	    automatically on the self object on instance methods.  That is
	    why instance methods may directly access instance variables
	    associated with their self argument without the need for the
	    ivPtr or ivsPtr macros.  */
	iv = ivPtr(obj);

	/*  Since iv is correctly updated it is now safe to directly access
	    the instance variables associated with obj - update iCode with
	    the 'code' value passed to this method.  */
	iCode = code;

	/*  Since we are now through allocating and initializing the new
	    object, simply return it.  */ 
	return obj;
}

/*  Create an instance method to allow an external program to access the
    value in iCode in order to test our New method.  */
    
imeth	int	gGetCode()
{
	return iCode;
}

cmeth	int	gNumInstances()
{
	return cNumInstances;
}

imeth	object	gDeepDispose, gDispose ()
{
	cNumInstances--;
	gDispose(super self);
	return NULL;
}

imeth	gSetName(char *name)
{
	strcpy(iName, name);
	return self;
}

imeth	char	* gGetName : get_name ()
{
	return iName;
}





