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



defclass ComServer {
	IClassFactory	iCF;
	int		iRef;
	CLSID		*iCls;
	iSelf;				//  reference to itself
	iInterfaces;			//  Dynace classes implemented by this server (LinkObject)
	int		iNumbInst;	//  number of instances
	iInstances;			//  all the instances of this server (regardless of interface)
	DWORD		iRegistered;
};

#define	VTABLE(x)	((IUnknown *) (x))->lpVtbl

#define	QueryInterface(pif, iid, ppv)	VTABLE(pif)->QueryInterface((IUnknown *)pif, iid, ppv)
#define	AddRef(pif)			VTABLE(pif)->AddRef((IUnknown *)pif)
#define	Release(pif)			VTABLE(pif)->Release((IUnknown *)pif)


static	IClassFactoryVtbl vtblClassFactory;


cmeth	gNewComServer(REFCLSID cls)
{
	object	obj = gNew(super);
	accessIVsOf(obj);

	gUseCOM(Application);
	iCF.lpVtbl = &vtblClassFactory;
	iRef = 1;
	iCls = (CLSID *) cls;
	iSelf = obj;
	iInterfaces = gNew(LinkObject);
	iInstances = gNew(LinkList);
	return obj;
}

imeth	gGCDispose()
{
	if (iRegistered)
		CoRevokeClassObject(iRegistered);
	return gDispose(super);
}

imeth	gDispose, gDeepDispose ()
{
	object	lv;

	gDeepDispose(iInterfaces);
	while (lv = gFirst(iInstances))
		gDeepDispose(gValue(lv));
	gDeepDispose(iInstances);
	return gGCDispose(self);
}

imeth	gAddInterface(REFIID iid, cls)
{
	gAddLast(iInterfaces, gNewInterface(ComInterface, iid, cls));
	return self;
}

imeth	gRegister()
{
	IUnknown	*cf = (IUnknown *) &iCF;
	HRESULT	hr = CoRegisterClassObject(iCls, cf, CLSCTX_LOCAL_SERVER, REGCLS_MULTIPLEUSE, &iRegistered);
	if (!SUCCEEDED(hr)) {
		iRegistered = 0;
		return NULL;
	}
	return self;
}

imeth	gIncrement(int n)
{
	iNumbInst += n;
	if (!iNumbInst)
		gQuitApplication(Application, 0);
	return self;
}

imeth	gInstances()
{
	return iInstances;
}


static	STDMETHODIMP	CFQueryInterface(IClassFactory *cf, REFIID riid, void **ppv)
{
	ivType *iv = (ivType *) cf;
	if (IsEqualIID(riid, &IID_IUnknown)  ||  IsEqualIID(riid, &IID_IClassFactory)) {
		*ppv = &iCF;
	} else {
		*ppv = 0;
		return (HRESULT) E_NOINTERFACE;
	}
	AddRef((IClassFactory *) *ppv);
	return NOERROR;
}

static	STDMETHODIMP_(ULONG)	CFAddRef(IClassFactory *cf)
{
	ivType *iv = (ivType *) cf;
	return ++iRef;
}

static	STDMETHODIMP_(ULONG)	CFRelease(IClassFactory *cf)
{
	ivType *iv = (ivType *) cf;
	if (--iRef == 0) {
//		gDeepDispose(iSelf);
		if (!iNumbInst)
			gQuitApplication(Application, 0);
		return 0;
	}
	return iRef;
}

#ifdef	DEBUGOLE

extern	object	mainWind;

#define	TT(x)	if (IsEqualIID(riid, &x))	return #x

static	char	*find(REFIID riid)
{
	TT(IID_IUnknown);
	TT(IID_IClassFactory);
	TT(IID_NULL);
	TT(IID_IDispatch);
	return "Unknown IID";
}

#endif

static	STDMETHODIMP	CreateInstance(IClassFactory *cf, IUnknown *punkOuter, REFIID riid, void **ppv)
{
	ivType *iv = (ivType *) cf;

	object	seq, in;
	IID	*iid;

#ifdef	DEBUGOLE
	char	buf[80];
	gMessage(mainWind, "Entering CreateInstance");
	sprintf(buf, "Looking for %s", find(riid));
	gMessage(mainWind, buf);
#endif
	for (seq = gSequence(iInterfaces) ; in = gNext(seq) ; )
#ifndef	_WIN32
		if (IsEqualIID(riid, iid=gInterfaceID(in))  ||  1)   //  some sort of a bug
#else
		if (IsEqualIID(riid, iid=gInterfaceID(in)))
#endif
		{
			gNewInstance(gInterfaceClass(in), iSelf, iid, ppv);
			gDispose(seq);
#ifdef	DEBUGOLE
			gMessage(mainWind, "Interface found");
#endif
			return NOERROR;
		}
#ifdef	DEBUGOLE
	gMessage(mainWind, "Exiting CreateInstance without finding anything");
#endif
	return (HRESULT) E_NOINTERFACE;
}

static	STDMETHODIMP	LockServer(IClassFactory *cf, BOOL flock)
{
	ivType *iv = (ivType *) cf;
	if (flock)
		++iNumbInst;
	else {
		--iNumbInst;
		if (!iNumbInst)
			gQuitApplication(Application, 0);
	}
	return NOERROR;
}

static	IClassFactoryVtbl vtblClassFactory = {
	CFQueryInterface, CFAddRef, CFRelease,
	CreateInstance,
	LockServer
};












