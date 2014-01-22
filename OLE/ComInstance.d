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
#include "cominst.h"


defclass ComInstance {
	_ComInstance	iP;	//  defined in cominst.h
};



#define	VTABLE(x)	((IUnknown *) (x))->lpVtbl

#define	QueryInterface(pif, iid, ppv)	VTABLE(pif)->QueryInterface((IUnknown *)pif, iid, ppv)
#define	AddRef(pif)			VTABLE(pif)->AddRef((IUnknown *)pif)
#define	Release(pif)			VTABLE(pif)->Release((IUnknown *)pif)


static	STDMETHODIMP	IQueryInterface(IUnknown *un, REFIID riid, void **ppv)
{
	ivType *iv = (ivType *) un;
	if (IsEqualIID(riid, &IID_IUnknown)  ||  IsEqualIID(riid, iP.iIID)) {
		*ppv = &iP.iCI;
	} else {
		*ppv = 0;
		return (HRESULT) E_NOINTERFACE;
	}
	AddRef(*ppv);
	return NOERROR;
}

static	STDMETHODIMP_(ULONG)	IAddRef(IUnknown *un)
{
	ivType *iv = (ivType *) un;
	return ++iP.iRef;
}

static	STDMETHODIMP_(ULONG)	IRelease(IUnknown *un)
{
	ivType *iv = (ivType *) un;
	if (--iP.iRef == 0) {
		gDeepDispose(iP.iSelf);
		gIncrement(iP.iServer, -1);
		return 0;
	}
	return iP.iRef;
}


cmeth	gNewComInstance(server, IID *iid, void **ppv, void *vtbl)
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	(void *) iP.iCI.lpVtbl = vtbl;
	iP.iCI.lpVtbl->QueryInterface = IQueryInterface;
	iP.iCI.lpVtbl->AddRef = IAddRef;
	iP.iCI.lpVtbl->Release = IRelease;
	iP.iRef = 1;
	iP.iIID = iid;
	iP.iSelf = obj;
	iP.iServer = server;
	*ppv = (void *) &iP.iCI;
	iP.iLink = gNewWithObj(LinkValue, obj);
	gAddLast(gInstances(server), iP.iLink);
	gIncrement(server, 1);
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	gDispose(iP.iLink);
	return gDispose(super);
}


//  The following method is never used and should be defined by sub-classes of this class
//  It was added here so dpp would know that the generic is needed

cmeth	gNewInstance(server, IID *iid, void **ppv)
{
	return gSubclassResponsibility(self, "gNewInstance");
}









