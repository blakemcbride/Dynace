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

/*  The New method is being modified to take an additional argument (in
    addition to the default 'object self') of an int.  This argument will
    be used to initialize the iCode instance variable.  Although Dynace
    is able to overload generic names (we could use the same generic name
    with different arguments) we will rename it for clairity.

    We need to be able to access the instance variables associated with a
    new instance from within a class method.  This poses a problem since
    class methods typically can only access class variables.  There are
    two solutions to this problem.  One solution is clean but a little
    less efficient than another, more dirty, solution.  This example will
    show the clean method and the next will illustrate the dirty method.

    We need an instance method in order to access the instance variables of
    the new instance.  Since this instance method will only be used internally
    to this class we can create the method as 'private'.  This causes the
    method to only be available within this class.
*/

private	imeth	init(int code)
{
	iCode = code;
	return self;
}

cmeth	gNewWithInt(int code)
{
	/*  Declare an object which will hold the new instance object being
	    created.  */
	object	obj;

	/*  Increment the count as before.  */
	cNumInstances++;

	/*  Create the new instance object as before, only this time put it
	    in obj.  */
	obj = gNew(super);

	/*  Initialize the new object's instance variable with the instance
	    method.  */

	init(obj, code);

	/*  Since we are now through allocating and initializing the new
	    object, simply return it.  */ 
	return obj;

	/*  The three previous lines could also have been combined as follows:
	        return init(gNew(super), code);
	*/
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






