
#include <windows.h>
#include <sql.h>
#include <sqlext.h>

#define	COBJMACROS
#include "../idl/iblake.h"

#include <initguid.h>


DEFINE_GUID(CLSID_Test, 0xccbe60a0,0x01a9,0x11d0, 0xbc,0x41,0x00,0x40,0x05,0x18,0x52,0x08);



defclass  IBlakeClass : ComClient {
	IBlake	*iIP;
};



cmeth	gNew()
{
	object	obj = gNewComClient(super, &CLSID_Test, &IID_IBlake);
	if (obj) {
		accessIVsOf(obj);
		iIP = (IBlake *) gPointerValue(super obj);
	}
	return obj;
}

imeth	iSetValue(short n, char *val)
{
	HRESULT hr = IBlake_SetValue(iIP, n, val);
	return SUCCEEDED(hr) ? self : NULL;
}

imeth	iGetValue(short n, char *val, short *r)
{
	HRESULT hr = IBlake_GetValue(iIP, n, val, r);
	return SUCCEEDED(hr) ? self : NULL;
}
