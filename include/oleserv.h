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

/*  This include file has been superceded.
    Use oleserv2.h instead.  */



#include <windows.h>
#include <string.h>

#ifdef	_WIN32
#include <oaidl.h>
#else
#include <ole2.h>
#include <dispatch.h>
#endif

#define	IDX(i)		(params->cArgs - (i))
#define	GetChar(i)	(char) params->rgvarg[IDX(i)].iVal
#define	GetShort(i)	params->rgvarg[IDX(i)].iVal
#define	GetLong(i)	params->rgvarg[IDX(i)].lVal
#define	GetFloat(i)	params->rgvarg[IDX(i)].fltVal
#define	GetDouble(i)	params->rgvarg[IDX(i)].dblVal
#define	GetPointer(i)	(void *) params->rgvarg[IDX(i)].lVal
#define	CpyString(i, s)	strcpy((s), GetString(i))
#define	GetMem(i)	params->rgvarg[IDX(i)].parray->pvData
#define	CpyMem(i, b)	memcpy((void *)(b), GetMem(i), (size_t)params->rgvarg[IDX(i)].parray->rgsabound[0].cElements)

#define	SetChar(v)	res->vt = VT_I2, res->iVal = (short) (v)
#define	SetShort(v)	res->vt = VT_I2, res->iVal = (v)
#define	SetLong(v)	res->vt = VT_I4, res->lVal = (v)
#define	SetFloat(v)	res->vt = VT_R4, res->fltVal = (v)
#define	SetDouble(v)	res->vt = VT_R8, res->dblVal = (v)
#define	SetPointer(v)	res->vt = VT_I4, res->lVal = (long) (v)


#define	GetString(i)	(char *) GetMem(i)

#define	SetMem(v, s)	res->vt = VT_ARRAY|VT_UI1,		\
	SafeArrayAllocDescriptor(1, &res->parray),		\
	res->parray->cbElements = 1,				\
	res->parray->rgsabound[0].lLbound = 0,			\
	res->parray->rgsabound[0].cElements = (s),		\
	SafeArrayAllocData(res->parray),			\
	SafeArrayLock(res->parray),				\
	memcpy(res->parray->pvData, (void *) v, (size_t) (s)),	\
	SafeArrayUnlock(res->parray)

#define	SetString(v)	SetMem(v, strlen(v)+1)



