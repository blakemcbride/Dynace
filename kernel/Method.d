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



public	defclass Method  {
	char	*name;
	object	cls;
	object	generic;
	ofun	meth;
	ofun	fmeth;		/*  fixed arg version of overloaded methods  */
	int	trace;
	object	next;
};

imeth	char	*gName()
{
	return name;
}	

imeth	int	gTrace(int mode)
{
	int	pmode = trace;
	trace = mode;
	return pmode;
}

imeth	ofun	gFunction()
{
	return meth;
}	

imeth	ofun	gChangeFunction(ofun fun)
{
	ofun	org;

	org = meth;
	meth = fun;
	return org;
}	

imeth	gCopy()
{
	return gShouldNotImplement(self, "gCopy/gDeepCopy");
}

objrtn	Method_initialize(void)
{
	static	int	done = 0;

	/*  Class creation and some of the methods are initialized by
	    the kernel  */

	if (done)
		return Method_c;

	done = 1;

/*	Method_c = gNewClass(Class, "Method", sizeof(Method_iv_t), 0, END);	*/

	iMethodFor(Method, gName, Method_im_gName);
	iMethodFor(Method, gTrace, Method_im_gTrace);
	iMethodFor(Method, gFunction, Method_im_gFunction);
	iMethodFor(Method, gChangeFunction, Method_im_gChangeFunction);
	iMethodFor(Method, gCopy, Method_im_gCopy);
	iMethodFor(Method, gDeepCopy, Method_im_gCopy);
	return Method_c;
}

#if 0  /*  code for the benefit of dpp  */

cmeth gNewMethod(char *n, object c, object gen, ofun methf, ofun methf2){}

#endif

