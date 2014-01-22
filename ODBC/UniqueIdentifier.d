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




#include <string.h>

#ifdef _WIN32
#include <rpc.h>
#else
#include <uuid/uuid.h>
typedef uuid_t GUID;
#endif


defclass  UniqueIdentifier  {
	GUID	iGUID;
};

private	imeth	pInitInstance()
{
#ifdef _WIN32
	RPC_STATUS	rstat = RPC_S_OK;

	rstat = UuidCreate(&iGUID);
	if (rstat != RPC_S_OK)
		gError(Dynace, "Unable to generate a globally unique identifier");
#else
	uuid_generate(iGUID);	
#endif
	
	return self;
}

cmeth	gNew()
{
	return pInitInstance(gNew(super));
}

private	imeth	init(GUID val)
{
	iGUID = val;
	return self;
}

cmeth	gNewWithGUID, <vNew> (GUID val)
{
	return init(gNew(super), val);
}

imeth	gDispose, gDeepDispose()
{
#ifndef _WIN32
	uuid_clear(iGUID);
#endif
	return gDispose(super);
}

imeth	int	gHash()
{
	register char	 c = 'a';
	char	*s = (char *) &iGUID;
	int	n = sizeof(iGUID);
	double	t;
	register unsigned short	 k=0;  /* must be short	 */

	while (n--)
		k += *s++ ^ c++;
	t = .6125423371	* k;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare : compare (other)
{
	ivType* ivother;
#ifdef  _WIN32
	RPC_STATUS rstat;
#endif

	if (!gIsKindOf(other, CLASS))
		return gCompare(super, other);
	ivother = ivPtr(other);
#ifdef _WIN32
	rstat = RPC_S_OK;
	return UuidCompare(&iGUID, &(ivother->iGUID), &rstat); 	
#else	
	return uuid_compare(iGUID, ivother->iGUID);
#endif
}

cmeth	gString()
{
	object uuid = gNew(CLASS);
	object str = gString(uuid);
	gDispose(uuid);
	return str;
}

imeth	int	gEqual(object other)
{
	return (! compare(self, other));
}

imeth	gCopy, gDeepCopy()
{
	object copy = gNew(CLASS);
	ivType* ivcopy = ivPtr(copy);
	
#ifdef _WIN32
	memcpy(&(ivcopy->iGUID), &iGUID, sizeof(GUID));
#else
	uuid_copy(ivcopy->iGUID, iGUID)
#endif
	return copy;
}

#define	VAL(i)	((long *)&iv->iGUID)[i]

imeth	gStringRepValue()
{
	iGUID;
	return vSprintf(String, "{%lx, %lx, %lx, %lx}", VAL(0), VAL(1), VAL(2), VAL(3));
}

imeth	gString()
{
	object	retval;
	
#ifdef _WIN32
	RPC_STATUS rstat = RPC_S_OK;
	unsigned char*	uuidstr = 0;
	
	rstat = UuidToString(&iGUID, &uuidstr);
	if (rstat != RPC_S_OK)
		gError(Dynace, "Out of memory while generating UUID string");
	retval = gNewWithStr(String, (char*)uuidstr);
	RpcStringFree(&uuidstr);
#else
	char uuidstr[37];
	
	uuid_unparse(iGUID, uuidstr);
	retval = gNewWithStr(String, uuidstr);
#endif

	return retval;
}

imeth	GUID	*gGUIDValue()
{
	return &iGUID;
}

imeth	void	*gPointerValue()
{
	return (void *) &iGUID;
}

imeth	gChangeValue(val)
{
	ChkArg(val, 2);
	iGUID = *gGUIDValue(val);
	return self;
}

imeth	gChangeGUIDValue(GUID val)
{
	iGUID = val;
	return self;
}




