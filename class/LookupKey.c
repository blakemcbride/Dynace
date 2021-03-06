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




#define	CLASS	LookupKey_c
#define	ivType	LookupKey_iv_t

#include "generics.h"

object	LookupKey_c;


#line 48 "LookupKey.c"
typedef struct  _LookupKey_iv_t  {
	object iKey;
}	LookupKey_iv_t;



#line 36 "LookupKey.d"
cmeth objrtn LookupKey_cm_gNewWithObj(object self, object key)
{ 
	object luk; 
	ivType *iv; 

	ChkArgNul(key, 2); 
	luk = oSuper(LookupKey_c, gNew, self)(self); 
	iv = ivPtr(luk); 
	iv->iKey = key; 
	return luk; 
} 

imeth objrtn LookupKey_im_gDeepCopy(object self)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	object nobj; 
	ivType *niv; 

	nobj = oSuper(LookupKey_c, gDeepCopy, self)(self); 
	niv = ivPtr(nobj); 
	if (niv->iKey) 
		niv->iKey = gDeepCopy(iv->iKey); 
	return nobj; 
} 

imeth objrtn LookupKey_im_gKey(object self)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	return iv->iKey; 
} 

imeth objrtn LookupKey_im_gChangeKey(object self, object key)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	object old; 
	ChkArgNul(key, 2); 
	old = iv->iKey; 
	iv->iKey = key; 
	return old; 
} 

imeth objrtn LookupKey_im_gDeepDispose(object self)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	if (iv->iKey) 
		gDeepDispose(iv->iKey); 
	return oSuper(LookupKey_c, gDispose, self)(self); 
} 

imeth objrtn LookupKey_im_gStringRepValue(object self)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	return iv->iKey ? gStringRepValue(iv->iKey) : gNew(String); 
} 

imeth int LookupKey_im_gHash(object self)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	return iv->iKey ? gHash(iv->iKey) : 0; 
} 

imeth int LookupKey_im_gCompare(object self, object arg)
{ LookupKey_iv_t *iv = GetIVs(LookupKey, self);
	ChkArgNul(arg, 2); 
	if (!iv->iKey && !arg) 
		return 0; 
	if (!iv->iKey) 
		return -1; 
	if (!arg) 
		return 1; 
	return gCompare(iv->iKey, gKey(arg)); 
} 


#line 124 "LookupKey.c"

objrtn	LookupKey_initialize(void)
{
	static  CRITICALSECTION  cs;
	static  int volatile once = 0;

	ENTERCRITICALSECTION(_CI_CS_);
	if (!once) {
		INITIALIZECRITICALSECTION(cs);
		once = 1;
	}
	LEAVECRITICALSECTION(_CI_CS_);

	ENTERCRITICALSECTION(cs);

	if (LookupKey_c) {
		LEAVECRITICALSECTION(cs);
		return LookupKey_c;
	}
	INHIBIT_THREADER;
	Association_initialize();
	if (LookupKey_c)  {
		ENABLE_THREADER;
		LEAVECRITICALSECTION(cs);
		return LookupKey_c;
	}
	LookupKey_c = gNewClass(Class, "LookupKey", sizeof(LookupKey_iv_t), 0, Association, END);
	cMethodFor(LookupKey, gNewWithObj, LookupKey_cm_gNewWithObj);
	iMethodFor(LookupKey, gCompare, LookupKey_im_gCompare);
	iMethodFor(LookupKey, gChangeKey, LookupKey_im_gChangeKey);
	iMethodFor(LookupKey, gKey, LookupKey_im_gKey);
	iMethodFor(LookupKey, gStringRepValue, LookupKey_im_gStringRepValue);
	iMethodFor(LookupKey, gHash, LookupKey_im_gHash);
	iMethodFor(LookupKey, gDeepDispose, LookupKey_im_gDeepDispose);
	iMethodFor(LookupKey, gDeepCopy, LookupKey_im_gDeepCopy);

	ENABLE_THREADER;

	LEAVECRITICALSECTION(cs);

	return LookupKey_c;
}



