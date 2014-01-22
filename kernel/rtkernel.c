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


/*
  Support for RTKernel 

  This module implements Windows 95/NT (Win32)
  critical section facilities.

*/



#include "dynrtk.h"



void	InitializeCriticalSection(CRITICAL_SECTION *cs)
{
	cs->s1 = RTKCreateSemaphore(Resource, 1, "NoName");
	cs->s2 = RTKCreateSemaphore(Resource, 1, "NoName");
	cs->t = (TaskHandle) 0;
	cs->n = 0;
}

void	DeleteCriticalSection(CRITICAL_SECTION *cs)
{
	RTKDeleteSemaphore(&cs->s1);
	RTKDeleteSemaphore(&cs->s2);
}

void	EnterCriticalSection(CRITICAL_SECTION *cs)
{
	RTKWait(cs->s1);
	if (cs->t == RTKCurrentTaskHandle())
		cs->n++;
	else {
		RTKSignal(cs->s1);
		RTKWait(cs->s2);
		RTKWait(cs->s1);
		cs->n = 1;
		cs->t = RTKCurrentTaskHandle();
	}
	RTKSignal(cs->s1);
}

void	LeaveCriticalSection(CRITICAL_SECTION *cs)
{
	RTKWait(cs->s1);
	if (cs->n  &&  cs->t == RTKCurrentTaskHandle())
		if (!--cs->n) {
			cs->t = (TaskHandle) 0;
			RTKSignal(cs->s1);
			RTKSignal(cs->s2);
			return;
		}
	RTKSignal(cs->s1);
}
