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



defclass  ComClient  {
	CLSID	*iClass;
	IID	*iInterface;
	void	*iInstance;
class:
	int	cInitCOM;
};


#define	QueryInterface(pif, iid, ppv)	((IUnknown *) (pif))->lpVtbl->QueryInterface((IUnknown *)pif, iid, ppv)
#define	AddRef(pif)			((IUnknown *) (pif))->lpVtbl->AddRef((IUnknown *)pif)
#define	Release(pif)			((IUnknown *) (pif))->lpVtbl->Release((IUnknown *)pif)

#define	LockServer(pif, v)		((IClassFactory *) (pif))->lpVtbl->LockServer((IClassFactory *)pif, v)
#define	CreateInstance(pif,a,b,c)	((IClassFactory *) (pif))->lpVtbl->CreateInstance((IClassFactory *)pif,a,b,c)



cmeth	gNewComClient(REFCLSID cls, REFIID inter)
{
	HRESULT		hr;
	IClassFactory	*pCF;
	object	obj = gNew(super);
	accessIVsOf(obj);

	if (!cInitCOM) {
		gUseCOM(Application);
		cInitCOM = 1;
	}
#if 1
	hr = CoGetClassObject(cls, CLSCTX_LOCAL_SERVER, NULL, &IID_IClassFactory, (void **) &pCF);  
	if (FAILED(hr)) {
		gDispose(obj);
		return NULL;
	}
	CoLockObjectExternal((IUnknown *) pCF, TRUE, TRUE);
//	LockServer(pCF, TRUE);
	hr = CreateInstance(pCF, NULL, inter, &iInstance);
//	LockServer(pCF, FALSE);
	Release(pCF);
#else
	//  the following code works under 95 & NT 3.51 but doesn't work under NT 4.0
	hr = CoCreateInstance(cls, NULL, CLSCTX_LOCAL_SERVER, inter, &iInstance);
#endif
	if (!SUCCEEDED(hr)) {
		gDispose(obj);
		return NULL;
	}
	iClass = (CLSID *) cls;
	iInterface = (IID *) inter;
	return obj;
}

imeth	gDispose, gDeepDispose, gGCDispose ()
{
	if (iClass)
		Release(iInstance);
	return gDispose(super);
}

imeth	void	*gPointerValue()
{
	return (void *) iInstance;
}







