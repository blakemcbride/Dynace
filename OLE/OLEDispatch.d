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


#include "oleserv2.h"


defclass OLEDispatch : ComInstance {
	iSelf;
	iInstance;	//  instance of implementation class
	iGenerics;	//  StringDictionary of Generics (don't dispose)
class:
	cClasses;	//  StringDictionary of implementation classes
};



static	IDispatchVtbl vtblDispatch;


#include "cominst.h"



cmeth	gNewInstance(server, IID *iid, void **ppv)
{
	object	obj = gNewComInstance(super, server, iid, ppv, (void *) &vtblDispatch);
	accessIVsOf(obj);

	iSelf = obj;
	return obj;
}

imeth	gGCDispose()
{
	return gGCDispose(super);
}

imeth	gDispose, gDeepDispose ()
{
	if (iInstance)
		gDeepDispose(iInstance);
	return gDispose(super);
}

cmeth	gAddMethod(cls, char *gname, ofun gfun)
{
	object	gens;	//  StringDictionary of generics applicable to the class
	
	if (!cClasses)
		cClasses = gNew(StringDictionary);
	if (!(gens = gFindValueStr(cClasses, gName(cls)))) {
		gens = gNewWithInt(StringDictionary, 101);
		gAddStr(cClasses, gName(cls), gens);
	}
	gAddStr(gens, gname, gNewWithPtr(Pointer, (void *) gfun));
	return gens;
}

static	STDMETHODIMP	getTypeInfoCount(IDispatch 	*dsp,
					 UINT		*num)
{
	GetIV(dsp);
	*num = 0;
	return NOERROR;
}

static	STDMETHODIMP	getTypeInfo(IDispatch 	*dsp,
				    UINT 	tinfo,
				    LCID 	lcid,
				    ITypeInfo 	**ti)
{
	GetIV(dsp);
	*ti = NULL;
	return ResultFromScode(E_NOTIMPL);
}

#ifdef	_WIN32
static	STDMETHODIMP	getIDsOfNames(IDispatch *dsp,
				      REFIID	riid,
				      LPOLESTR 	*names,
				      UINT 	nnames,
				      LCID 	lcid,
				      DISPID 	*dspid)
#else
static	STDMETHODIMP	getIDsOfNames(IDispatch *dsp,
				      REFIID	riid,
				      TCHAR 	**names,
				      UINT 	nnames,
				      LCID 	lcid,
				      DISPID 	*dspid)
#endif
{
	GetIV(dsp);
	if (!IsEqualIID(riid, &IID_NULL))
		return ResultFromScode(DISP_E_UNKNOWNINTERFACE);
	return ResultFromScode(E_NOTIMPL);
//	return NOERROR;
}


#define	RVAL(x)		*(short *)err = (x)

static	STDMETHODIMP	invoke(IDispatch 	*dsp,
			       DISPID		dispid,
			       REFIID		riid,
			       LCID		lcid,
			       WORD		wFlags,
			       DISPPARAMS	*params,
			       VARIANT		*res,
			       EXCEPINFO	*ei,
			       UINT		*err)
{
	int	i;
	object	gobj;
	GetIV(dsp);
	VariantInit(res);
	if (!IsEqualIID(riid, &IID_NULL))
		return ResultFromScode(DISP_E_UNKNOWNINTERFACE);


	for (i=0 ; i != params->cArgs ; i++)
		if (params->rgvarg[i].vt & VT_ARRAY)
			SafeArrayLock(params->rgvarg[i].parray);


	switch (dispid) {
	case 1:				//  Create a new instance
		if (iInstance)
			RVAL(1);
		else if (!cClasses)
			RVAL(2);
		else if (!(iGenerics = gFindValueStr(cClasses, GetString(0))))
			RVAL(3);
		else if (!(iInstance = gNewOleInstance(gFindClass(Class, GetString(0)), params, res)))
			RVAL(4);
		else
			RVAL(0);
		break;
	case 2:				//  Evoke a method
		if (!iInstance  ||  !iGenerics)
			RVAL(1);
		else if (!(gobj = gFindValueStr(iGenerics, GetString(0))))
			RVAL(2);
		else {
			(*(ofun) gPointerValue(gobj))(iInstance, params, res);
			RVAL(0);
		}
		break;
	case 3:				//  Dispose
		if (iInstance) {
			iGenerics = iInstance = gDispose(iInstance);
			RVAL(0);
		} else
			RVAL(1);
		break;
	case 4:				//  DeepDispose
		if (iInstance) {
			iGenerics = iInstance = gDeepDispose(iInstance);
			RVAL(0);
		} else
			RVAL(1);
		break;
	default:
		RVAL(100);
		break;
	}

	for (i=0 ; i != params->cArgs ; i++)
		if (params->rgvarg[i].vt & VT_ARRAY) {
			SafeArrayUnlock(params->rgvarg[i].parray);
		}

	return NOERROR;
}

static	IDispatchVtbl vtblDispatch = {
	NULL, NULL, NULL,
	getTypeInfoCount,
	getTypeInfo,
	getIDsOfNames,
	invoke
};

cmeth	gNewOleInstance(DISPPARAMS *params, VARIANT *res)
{
	//  this shell just used to cause the generic to be defined
	return NULL;
}





