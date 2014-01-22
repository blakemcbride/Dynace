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


#define	COBJMACROS

#include <windows.h>
#include <string.h>


#ifndef	_WIN32
#include <compobj.h>
#include <dispatch.h>
#include <olenls.h>
#endif


#ifndef	_WIN32
#define IDispatch_Invoke(This,dispidMember,riid,lcid,wFlags,pdispparams,pvarResult,pexcepinfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispidMember,riid,lcid,wFlags,pdispparams,pvarResult,pexcepinfo,puArgErr)
#endif


#define	CLEAR(x)	memset((void *)&x, 0, sizeof x)
#define	NUMARGS		(sizeof(args) / sizeof(VARIANTARG))
#define	IDX(i)		(NUMARGS - (i) - 1)
#define	SetChar(i, v)	args[IDX(i)].vt = VT_I2, args[IDX(i)].iVal = (short) (v)
#define	SetShort(i, v)	args[IDX(i)].vt = VT_I2, args[IDX(i)].iVal = (v)
#define	SetLong(i, v)	args[IDX(i)].vt = VT_I4, args[IDX(i)].lVal = (v)
#define	SetFloat(i, v)	args[IDX(i)].vt = VT_R4, args[IDX(i)].fltVal = (v)
#define	SetDouble(i, v)	args[IDX(i)].vt = VT_R8, args[IDX(i)].dblVal = (v)
#define	SetPointer(i,v) args[IDX(i)].vt = VT_I4, args[IDX(i)].lVal = (long) (v)

#define	GetChar()	(char) res.iVal
#define	GetShort()	res.iVal
#define	GetLong()	res.lVal
#define	GetFloat()	res.fltVal
#define	GetDouble()	res.dblVal
#define	GetPointer()	(void *) res.lVal
#define	CpyString(s)	strcpy(s, GetString())
#define	GetMem()	res.parray->pvData
#define	CpyMem(b)	memcpy((void *)(b), GetMem(), (size_t)res.parray->rgsabound[0].cElements)


#define	GetString()	(char *) GetMem()

#define	SetMem(i,v,s)	args[IDX(i)].vt = VT_ARRAY|VT_UI1,		\
	SafeArrayAllocDescriptor(1, &args[IDX(i)].parray),		\
	args[IDX(i)].parray->cbElements = 1,				\
	args[IDX(i)].parray->rgsabound[0].lLbound = 0,			\
	args[IDX(i)].parray->rgsabound[0].cElements = (s),  		\
	SafeArrayAllocData(args[IDX(i)].parray),			\
	SafeArrayLock(args[IDX(i)].parray),				\
	memcpy(args[IDX(i)].parray->pvData, (void *) v, (size_t)(s)),	\
	SafeArrayUnlock(args[IDX(i)].parray)

#define	SetString(i,v)	SetMem((i), (v), strlen(v)+1)

#define	OLERESET				\
	CLEAR(params);				\
	params.cArgs = NUMARGS;			\
	params.rgvarg = args;			\
	VariantInit(&res);			\
	for (_i=0 ; _i != params.cArgs ; )	\
		VariantInit(&args[_i++])


#define	OLEHEADER(n)				\
	HRESULT 	hr;			\
	unsigned long	lerr;			\
	unsigned short	err;			\
	int		_i;			\
	DISPPARAMS	params;			\
	VARIANT		res;			\
	VARIANTARG	args[n+1];		\
	OLERESET


#define	OLEINVOKE3(n)						\
	hr = IDispatch_Invoke((IDispatch*)(oSuper(CLASS, gPointerValue, self)(self)), n, &IID_NULL, LANG_NEUTRAL, DISPATCH_METHOD, &params, &res, NULL, (UINT *)&lerr);\
	if (FAILED(hr))						\
		gError(Object, "OLE communications error.");	\
	err = *(unsigned short *) &lerr;			\
	if (res.vt & VT_ARRAY)					\
		SafeArrayLock(res.parray)

#define	OLEINVOKE(f)	SetString(0, (f)), OLEINVOKE3(2)

#define	OLEINVOKE2(n)						\
	hr = IDispatch_Invoke(iv->iIP, n, &IID_NULL, LANG_NEUTRAL, DISPATCH_METHOD, &params, &res, NULL, (UINT *)&lerr);\
	if (FAILED(hr))						\
		gError(Object, "OLE communications error.");	\
	err = *(unsigned short *) &lerr;			\
	if (res.vt & VT_ARRAY)					\
		SafeArrayLock(res.parray)

#define	OLEEND						\
	if (res.vt & VT_ARRAY) {			\
		SafeArrayUnlock(res.parray);		\
		SafeArrayDestroy(res.parray);		\
	}						\
	for (_i=0 ; _i != params.cArgs ; _i++)		\
		if (args[_i].vt & VT_ARRAY)		\
			SafeArrayDestroy(args[_i].parray); \
	OLERESET
			


#define	ResultIsArray	(res.vt & VT_ARRAY)
			
GUID	*Make_guid(int company, int id);





