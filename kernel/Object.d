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



public	defclass Object  {
	object	 	cls;
	unsigned short	tag;	/*  info tags		*/
	unsigned short	siz;	/*  used by GC		*/
	unsigned long	sn;	/*  serial number	*/
};

#include <math.h>

static	ifun	cErrorFunction;

cmeth	ifun	gSetErrorFunction(ifun fun)
{
	ifun	ret = cErrorFunction;
	cErrorFunction = fun;
	return ret;
}

imeth	gError(char *msg)
{
	if (cErrorFunction)
		cErrorFunction(msg);
#ifndef	__COSMIC__
	if (File_c)
		gPuts(stderrStream, msg);
	else  {
		if (IsObj((object) msg))
			msg = gStringValue((object) msg);
		fprintf(stderr, "%s", msg);
	}
#endif
	exit(1);
	return self;   /*  if it did return  */
}

ivmeth	vError(char *fmt, ...)
{
	char	buf[4096];
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	return gError(self, buf);
}

imeth	gSubclassResponsibility(char *meth)
{
	char	buf[100];
	object	aCls;

	aCls = IsaClass(self) ? self : ClassOf(self);
	sprintf(buf, "Method %s should be implemented by a subclass of %s\n", 
		meth, gName(aCls));
	return gError(self, buf);
}

imeth	gShouldNotImplement(char *meth)
{
	char	buf[100];

	if (IsaClass(self))
		sprintf(buf, "Method %s is not appropriate for the %s class\n", 
			meth, gName(self));
	else
		sprintf(buf, "Method %s is not appropriate for instances of %s\n", 
			meth, gName(ClassOf(self)));
	return gError(self, buf);
}

#ifndef	NOCLASSLIB

imeth	gStringRep()
{
	object	s, t;

	s = vSprintf(String, "%s:<%8.8lx> [ ", gName(ClassOf(self)), self);
	t = gStringRepValue(self);
	vBuild(s, NULL, t, " ]\n", NULL);
	gDispose(t);
	return s;
}

imeth	gStringRepValue()
{
	return vSprintf(String, "%s:<%8.8lx>", gName(ClassOf(self)), self);
}

imeth	gPrint(object stream)
{
	object	t;

	ChkArg(stream, 2);
	t = gStringRep(self);
	gPuts(stream, (char *) t);
	gDispose(t);
	return self;
}

imeth	gPrintValue(object stream)
{
	object	t;

	ChkArg(stream, 2);
	t = gStringRepValue(self);
	gPuts(stream, (char *) t);
	gDispose(t);
	return self;
}

#endif

imeth	unsigned long	gObjectSerialNumber()
{
	return sn;
}

imeth	int	gHash()
{
	double	t;

	t = .6125423371	* (double)(unsigned long) self;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(object obj2)
{
	if (EQ(self, obj2))
		return 0;
	ChkArg(obj2, 2);
	if ((unsigned long) self < (unsigned long) obj2)
		return -1;
	else
		return 1;
}

imeth	int	gIsKindOf(object aCls)
{
	static	gIsKindOf_t	class_IsKindOf = NULL;

	/*  aCls need not be validated  */

	if (!class_IsKindOf)
		class_IsKindOf = imcPointer(Behavior, gIsKindOf);
	return class_IsKindOf(ClassOf(self), aCls);
}

imeth	gInit()
{
	return self;
}

objrtn	Object_initialize(void)
{
	static	int	done = 0;

	/*  Class creation and some of the methods are initialized by
	    the kernel  */

	if (done)
		return Object_c;

	done = 1;

/*	Object_c = gNewClass(Class, "Object", sizeof(Object_iv_t), 0, END);	*/

	cMethodFor(Object, gSetErrorFunction, Object_cm_gSetErrorFunction);
	iMethodFor(Object, gError, Object_im_gError);
#if	DPP_STRATEGY == 1
	ivMethodFor(Object, vError, Object_ivm_vError, Object_ivm_vError);
#else
	ivMethodFor(Object, vError, Object_ivm_vError, Object_ifm_vError);
#endif
	iMethodFor(Object, gSubclassResponsibility, Object_im_gSubclassResponsibility);
	iMethodFor(Object, gShouldNotImplement, Object_im_gShouldNotImplement);
	iMethodFor(Object, gHash, Object_im_gHash);
	iMethodFor(Object, gCompare, Object_im_gCompare);
	iMethodFor(Object, gIsKindOf, Object_im_gIsKindOf);
	iMethodFor(Object, gInit, Object_im_gInit);
	iMethodFor(Object, gObjectSerialNumber, Object_im_gObjectSerialNumber);

#ifndef	NOCLASSLIB
	iMethodFor(Object, gStringRep, Object_im_gStringRep);
	iMethodFor(Object, gStringRepValue, Object_im_gStringRepValue);
	iMethodFor(Object, gPrint, Object_im_gPrint);
	iMethodFor(Object, gPrintValue, Object_im_gPrintValue);
#endif
	return Object_c;
}

#if 0  /*  code for the benefit of dpp  */

imeth	gDispose, gDeepDispose, gGCDispose (){}
imeth	int	gEqual(object obj2){}
imeth	gCopy, gDeepCopy (){}
imeth	int	gSize(){}
imeth	int	gBasicSize(){}

imeth	gLoadJavaClass(char *name){}


#endif

