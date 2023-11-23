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



defclass Dynace;

/* These need to be here to avoid causing WDS to be loaded whenever Scheme or JavaScript are used.  */
object	SchemeClassSurrogate;
object	JavaScriptClassSurrogate;
object	JavaCallbackClassSurrogate; 


cmeth	gTracePrint(char *buf)
{
#ifndef __COSMIC__
	if (File_c)
		gPuts(traceStream, buf);
	else
		printf("%s", buf);
#endif
	return self;
}

cmeth	gObjectChecking(int x)
{
	_CheckObjects_ = x;
	if (!x)
		_LastGeneric_ = NULL;
	return self;
}

objrtn	Dynace_initialize(void)
{
	if (Dynace_c)
		return Dynace_c;

	Dynace_c = gNewClass(Class, "Dynace", 0, 0, END);

	cMethodFor(Dynace, gTracePrint, Dynace_cm_gTracePrint);
	cMethodFor(Dynace, gObjectChecking, Dynace_cm_gObjectChecking);
	return Dynace_c;
}

#if 0  /*  code for the benefit of dpp  */

cmeth	gResizeMethodCache(int classes, int generics){}
cmeth	gSetMemoryBufferArea(long sz){}
cmeth	void	gMarkRange(char _HUGE_ **from, char _HUGE_ **to){}
cmeth	gGC(){}
cmeth	void	*gRegisterMemory(void *beg, long size){}
cmeth	void	gRemoveRegisteredMemory(void *pp){}
cmeth	void	*gChangeRegisteredMemory(void *pp, void *beg, long size){}
cmeth	long	gMaxAfterGC(){}
cmeth	long	gMaxMemUsed(){}
cmeth	long	gCurMemUsed(){}
cmeth	long	gNumbGC(){}
cmeth	int	gTrace(int mode){}
cmeth	void	gMarkObject(object obj){}
cmeth	gDumpObjects(char *file, int type){}
cmeth	gDumpObjectsString(int type){}
cmeth	gDumpObjectsDiff(sd1, sd2){}
cmeth	gMarkMemoryBeginning(){}
cmeth	gDumpMemoryDiff(object d1, char *fname){}
cmeth	int	gCheckFreeList(){}

#endif

