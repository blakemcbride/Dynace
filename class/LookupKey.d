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




defclass  LookupKey : Association  {
	iKey;
};

cmeth	gNewWithObj, <vNew> (key)
{
	object	luk;
	ivType	*iv;

	ChkArgNul(key, 2);
	luk = gNew(super);
	iv = ivPtr(luk);
	iKey = key;
	return luk;
}

imeth	gDeepCopy()
{
	object	nobj;
	ivType	*niv;

	nobj = gDeepCopy(super);
	niv = ivPtr(nobj);
	if (niv->iKey)
		niv->iKey = gDeepCopy(iKey);
	return nobj;
}

imeth	gKey()
{
	return iKey;
}

imeth	gChangeKey(key)
{
	object	old;
	ChkArgNul(key, 2);
	old = iKey;
	iKey = key;
	return old;
}

imeth	object	gDeepDispose()
{
	if (iKey)
		gDeepDispose(iKey);
	return gDispose(super);
}

imeth	gStringRepValue()
{
	return iKey ? gStringRepValue(iKey) : gNew(String);
}

imeth	int	gHash()
{
	return iKey ? gHash(iKey) : 0;
}

imeth	int	gCompare(arg)
{
	ChkArgNul(arg, 2);
	if (!iKey  &&  !arg)
		return 0;
	if (!iKey)
		return -1;
	if (!arg)
		return 1;
	return gCompare(iKey, gKey(arg));
}






