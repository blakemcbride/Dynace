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




#if defined(_MSC_VER)
#if !defined(WIN32)  &&  _MSC_VER >= 900
#define	WIN32
#endif
#endif


#include <stdio.h>
#include <windows.h>
#include <mmsystem.h>


#define	CONTEXT_SWITCHES_PER_SECOND	50



 /*  thousands of a second  */
 
#define	TARGET_MILLSECS	(1000/CONTEXT_SWITCHES_PER_SECOND) 



#ifdef TEST
int	_tick_count=10000;
#else
extern	int	_tick_count;
#endif

static	UINT		wTimerRes;
static	UINT		id;

static	void	CALLBACK
timerfun(UINT	idEvent,
	 UINT	uReserved,
	 DWORD	dwUser,
	 DWORD	rwReserved1,
	 DWORD	rwReserved2)
{
	if (_tick_count)
		_tick_count--;
}

static	void	_end_timer(void)
{
	timeKillEvent(id);
	timeEndPeriod(wTimerRes);
}

void	_start_timer()
{
	TIMECAPS	tc;

	timeGetDevCaps(&tc, sizeof(TIMECAPS));
	wTimerRes = min(max(tc.wPeriodMin, TARGET_MILLSECS), tc.wPeriodMax);
	timeBeginPeriod(wTimerRes);
	id = timeSetEvent(wTimerRes, wTimerRes, (LPTIMECALLBACK) timerfun, (DWORD) 0, TIME_PERIODIC);
	atexit(_end_timer);
}

#ifdef TEST

main()
{
	int	n;
	long	i;

	_start_timer();

	for (n=0 ; n++ != 10 ; )  {
		fprintf(stderr, "Timer = %d\n", _tick_count);
		for (i=0L ; i++ != 1000000L ; );
	}
	return(0);
}

#endif





