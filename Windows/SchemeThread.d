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

defclass  SchemeThread {
	HANDLE			iSchemeExecEvent;
	int			iThreadAvailable;
	int			*iParmBlockStart;
	HANDLE			iCallWaitEvent;
	void			*iSchemeExecFun;
	int			iExecRetVal;
	DWORD			iThreadId;
	CRITICAL_SECTION	iCsect;
	
class:
	object			cThreads;
	HANDLE			cCreateThreadWaitEvent;
	HANDLE			cWaitForCreateThreadWaitEvent;
	DWORD			cCreateThreadId;
};

private imeth	initInstance(object self);
private imeth	checkAvailable(object self);
private imeth	execFun(object self, void *fun, int *parmStart);
private imeth	int	resetAvail(object self);

cmeth	void	gCreateSchemeThreadWait() 
{
	DWORD	ret;
	cCreateThreadId = GetCurrentThreadId();
	while(1)
	{
		ret = WaitForSingleObject(cCreateThreadWaitEvent, INFINITE);
		if (ret == WAIT_OBJECT_0)
		{
			gAddLast(cThreads, gNew(SchemeThread));
			SetEvent(cWaitForCreateThreadWaitEvent);
		}
	}
}

imeth	void	gSchemeThreadWait() {
	while(1)
	{
		DWORD ret;
		iThreadId = GetCurrentThreadId();
		ret = WaitForSingleObject(iSchemeExecEvent, INFINITE);
		if (ret == WAIT_OBJECT_0)
		{
			int parmCnt = *iParmBlockStart;
			int *parmPnt = iParmBlockStart + parmCnt;
			int nStackFix = parmCnt * 4;
			int retVal;
			void *fun = iSchemeExecFun;

			_asm 
			{
				mov		ecx, parmCnt
iter:
				mov		eax, parmPnt
				push		[eax]
				sub		parmPnt, 4
				sub		ecx, 1
				cmp		ecx, 0
				jne		iter
docall:				call		fun
				add		esp,nStackFix
				mov		retVal,eax
			}
			iExecRetVal = retVal;
			SetEvent(iCallWaitEvent);
		}
	}
	return;
}

cmeth	gNew()
{
	return initInstance(gNew(super));
}

private imeth checkAvailable()
{
	object	ret = self;
	EnterCriticalSection(&iCsect);
	if (!iThreadAvailable)
		ret = NULL;
	else
		iThreadAvailable = 0;
	LeaveCriticalSection(&iCsect);
	return ret;
}

private imeth execFun(void *fun, int *parmStart)
{
	iParmBlockStart = parmStart;
	iSchemeExecFun = fun;
	SetEvent(iSchemeExecEvent);
	WaitForSingleObject(iCallWaitEvent, INFINITE);
	return self;
}

imeth	DWORD	gGetThreadId()
{
	return iThreadId;
}

cmeth	int	gCheckThreadInPool()
{
	int	retVal = 0;
	object s;
	object obj;
	if (cCreateThreadId == GetCurrentThreadId())
		return 1;
	for (s=gSequence(cThreads) ; obj = gNext(s) ; )
	{
		if (GetCurrentThreadId() == gGetThreadId(obj))
		{
			retVal = 1;
			gDispose(s);
			break;
		}
	}

	return retVal;
}

cvmeth	int	vExecuteInPoolThread(void *fun, int numberOfParms, ...)
{
	int	ndx;
	int	retVal;
	int	threadFound = 0;
	object	execObj = NULL;
	while(!threadFound)
	{
		object s;
		object obj;
		for (s=gSequence(cThreads) ; obj = gNext(s) ; )
		{
			if (checkAvailable(obj))
			{
				threadFound = 1;
				execObj = obj;
				gDispose(s);
				break;
			}
		}
		if (!threadFound)
		{
			SetEvent(cCreateThreadWaitEvent);
			WaitForSingleObject(cWaitForCreateThreadWaitEvent, INFINITE);
		}
	}
	execFun(execObj, fun, &numberOfParms);
	return resetAvail(execObj);
}

private	imeth	int	resetAvail()
{
	int	retVal = iExecRetVal;
	EnterCriticalSection(&iCsect);
	iThreadAvailable = 1;
	LeaveCriticalSection(&iCsect);
	return retVal;
}

cmeth	void	gInitSchemeThreadPool()
{
	int	ndx;
	char	errbuf[256], execBuf[64];
	int	err = 0;
	cThreads = NULL;
	gExecuteStringWithErrorNR(Scheme, "(if (defined? 'require-file) (require-file \"ThreadPool.scm\" \"System\") (load/use-compiled \"ThreadPool.scm\"))", errbuf, &err);
	if (err)
		gError(Application, errbuf);
	
	cCreateThreadWaitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	cWaitForCreateThreadWaitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	cThreads = gNew(LinkObject);
	if (!cThreads)
		gError(Application, "Out of Memory");
		
	gExecuteStringWithErrorNR(Scheme, "(threadFactoryInit)", errbuf, &err);
	if (err)
		gError(Application, errbuf);
}

private imeth initInstance()
{
	char	errbuf[256], execBuf[64];
	int	err = 0;
	InitializeCriticalSection(&iCsect);
	iSchemeExecEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	iCallWaitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	iThreadAvailable = 1;
	sprintf(execBuf, "(threadInit (int->object %ld))", self);
	gExecuteStringWithErrorNR(Scheme, execBuf, errbuf, &err);
	if (err)
		gError(Application, errbuf);
	return self;
}


