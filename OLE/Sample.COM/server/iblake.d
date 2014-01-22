#include <windows.h>
#include <sql.h>
#include <sqlext.h>

#include "../idl/iblake.h"


defclass IBlakeClass : ComInstance {
	iSelf;
	char	iValue[100];
};

static	IBlakeVtbl vtblBlake;


#include "cominst.h"


cmeth	gNewInstance(server, IID *iid, void **ppv)
{
	object	obj = gNewComInstance(super, server, iid, ppv, (void *) &vtblBlake);
	accessIVsOf(obj);
	iSelf = obj;
	return obj;
}


static	HRESULT __stdcall __RPC_FAR	BlakeSetValue(IBlake *blake, short n, char *value)
{
	GetIV(blake);
	memcpy(iValue, value, (int) n);
	return NOERROR;
}

static	HRESULT __stdcall __RPC_FAR	BlakeGetValue(IBlake *blake, short n, char *value, short *m)
{
	GetIV(blake);
	if (!value)
		return E_POINTER;
	*m = strlen(iValue)+1;
	*m = min(*m, n);
	memcpy(value, iValue, (int) *m);
	return NOERROR;
}

static	IBlakeVtbl vtblBlake = {
	NULL, NULL, NULL,
	BlakeSetValue,
	BlakeGetValue
};
