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



/*  This file automatically generated by dpp - do not edit  */

#define	DPP_STRATEGY	2
#define	DPP_FASTWIDE	0




#define	CLASS	Object_c
#define	ivType	Object_iv_t

#include "generics.h"

object	Object_c;

#include "Object.iv"


#line 38 "Object.d"
#include <math.h> 
#include <stdint.h> 

static int (*cErrorFunction)(char *); 

cmeth ifun Object_cm_gSetErrorFunction(object self, int (*fun)(char *))
{ 
	ifun ret = cErrorFunction; 
	cErrorFunction = fun; 
	return ret; 
} 

imeth objrtn Object_im_gError(object self, char *msg)
{ 
	if (cErrorFunction) 
		cErrorFunction(msg); 
#ifndef __COSMIC__ 
		if (File_c) 
		gPuts(stderrStream, msg); 
	else { 
		if (IsObj((object) msg)) 
			msg = gStringValue((object) msg); 
		fprintf(stderr, "%s", msg); 
	} 
#endif 

#line 63 "Object.d"
		exit(1); 
	return self; 
} 

ivmeth objrtn Object_ivm_vError(object self, va_list _rest_)
{ char * fmt = va_arg(_rest_, char *);
	char buf[4096]; 
	vsprintf(buf, fmt, _rest_); 
	return gError(self, buf); 
} 

#line 89 "Object.c"

static	objrtn	Object_ifm_vError(object self, ...)
{
	va_list	_rest_;
	objrtn	_ret_;
	va_start(_rest_, self);
	_ret_ = Object_ivm_vError(self, _rest_);
	va_end(_rest_);
	return _ret_;
}



#line 74 "Object.d"
imeth objrtn Object_im_gSubclassResponsibility(object self, char *meth)
{ 
	char buf[100]; 
	object aCls; 

	aCls = IsaClass(self) ? self : ClassOf(self); 
	sprintf(buf, "Method %s should be implemented by a subclass of %s\n", 
		meth, gName(aCls)); 
	return gError(self, buf); 
} 

imeth objrtn Object_im_gShouldNotImplement(object self, char *meth)
{ 
	char buf[100]; 

	if (IsaClass(self)) 
		sprintf(buf, "Method %s is not appropriate for the %s class\n", 
		meth, gName(self)); 
	else 
		sprintf(buf, "Method %s is not appropriate for instances of %s\n", 
		meth, gName(ClassOf(self))); 
	return gError(self, buf); 
} 

#ifndef NOCLASSLIB 

imeth objrtn Object_im_gStringRep(object self)
{ 
	object s, t; 

	s = vSprintf(String, "%s:<%8.8lx> [ ", gName(ClassOf(self)), self); 
	t = gStringRepValue(self); 
	vBuild(s, NULL, t, " ]\n", NULL); 
	gDispose(t); 
	return s; 
} 

imeth objrtn Object_im_gStringRepValue(object self)
{ 
	return vSprintf(String, "%s:<%8.8lx>", gName(ClassOf(self)), self); 
} 

imeth objrtn Object_im_gPrint(object self, object stream)
{ 
	object t; 

	ChkArg(stream, 2); 
	t = gStringRep(self); 
	gPuts(stream, (char *) t); 
	gDispose(t); 
	return self; 
} 

imeth objrtn Object_im_gPrintValue(object self, object stream)
{ 
	object t; 

	ChkArg(stream, 2); 
	t = gStringRepValue(self); 
	gPuts(stream, (char *) t); 
	gDispose(t); 
	return self; 
} 

#endif 


#line 140 "Object.d"
imeth unsigned long Object_im_gObjectSerialNumber(object self)
{ Object_iv_t *iv = GetIVs(Object, self);
	return iv->sn; 
} 

imeth int Object_im_gHash(object self)
{ 
	double t; 

	t = .6125423371 * (double)(intptr_t) self; 
	t = t < 0.0 ? -t : t; 
	return (int) (BIG_INT * (t - floor(t))); 
} 

imeth int Object_im_gCompare(object self, object obj2)
{ 
	if (EQ(self, obj2)) 
		return 0; 
	ChkArg(obj2, 2); 
	if ((intptr_t) self < (intptr_t) obj2) 
		return -1; 
	else 
		return 1; 
} 

imeth int Object_im_gIsKindOf(object self, object aCls)
{ 
	static gIsKindOf_t class_IsKindOf = NULL; 



	if (!class_IsKindOf) 
		class_IsKindOf = imcPointer(Behavior, gIsKindOf); 
	return class_IsKindOf(ClassOf(self), aCls); 
} 

imeth objrtn Object_im_gInit(object self)
{ 
	return self; 
} 

objrtn Object_initialize(void) 
{ 
	static int done = 0; 




	if (done) 
		return Object_c; 

	done = 1; 



	cMethodFor(Object, gSetErrorFunction, Object_cm_gSetErrorFunction); 
	iMethodFor(Object, gError, Object_im_gError); 
#if DPP_STRATEGY == 1 
	ivMethodFor(Object, vError, Object_ivm_vError, Object_ivm_vError); 
#else 


#line 200 "Object.d"
	ivMethodFor(Object, vError, Object_ivm_vError, Object_ifm_vError); 
#endif 


#line 202 "Object.d"
	iMethodFor(Object, gSubclassResponsibility, Object_im_gSubclassResponsibility); 
	iMethodFor(Object, gShouldNotImplement, Object_im_gShouldNotImplement); 
	iMethodFor(Object, gHash, Object_im_gHash); 
	iMethodFor(Object, gCompare, Object_im_gCompare); 
	iMethodFor(Object, gIsKindOf, Object_im_gIsKindOf); 
	iMethodFor(Object, gInit, Object_im_gInit); 
	iMethodFor(Object, gObjectSerialNumber, Object_im_gObjectSerialNumber); 

#ifndef NOCLASSLIB 
	iMethodFor(Object, gStringRep, Object_im_gStringRep); 
	iMethodFor(Object, gStringRepValue, Object_im_gStringRepValue); 
	iMethodFor(Object, gPrint, Object_im_gPrint); 
	iMethodFor(Object, gPrintValue, Object_im_gPrintValue); 
#endif 


#line 216 "Object.d"
	return Object_c; 
} 

#if 0 

imeth objrtn Object_im_gDispose(object self)

#line 221 "Object.d"
{} 


#line 222 "Object.d"
imeth int Object_im_gEqual(object self, object obj2)

#line 222 "Object.d"
{} 


#line 223 "Object.d"
imeth objrtn Object_im_gCopy(object self)

#line 223 "Object.d"
{} 


#line 224 "Object.d"
imeth int Object_im_gSize(object self)

#line 224 "Object.d"
{} 


#line 225 "Object.d"
imeth int Object_im_gBasicSize(object self)

#line 225 "Object.d"
{} 

imeth objrtn Object_im_gLoadJavaClass(object self, char *name)

#line 227 "Object.d"
{} 


#endif 

