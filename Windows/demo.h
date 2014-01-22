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


#ifdef	LASTDATE
#define	DEMO
#endif


#ifdef	DEMO

#include <time.h>

void	demo_message(HWND hwnd);

extern	time_t	demo_startup_time;

extern	int	demo_number_of_windows;

#define DEMO_MAX_WINDOWS	15

#define DEMO_MAX_MINUTES	10L

#define DEMO_MSG_FREQ		5

#ifndef	LASTDATE
#define DEMO_CHK_MAX							\
	if (demo_number_of_windows > DEMO_MAX_WINDOWS  ||		\
	    demo_startup_time + DEMO_MAX_MINUTES * 60L < time(NULL))	\
		FatalAppExit(0, "Demo Usage Exceeded")
#else
#define	DEMO_CHK_MAX {							\
			object	date = gToday(Date);			\
			if (gLongValue(date) > LASTDATE  &&		\
			    (demo_number_of_windows > DEMO_MAX_WINDOWS  ||		\
			     demo_startup_time + DEMO_MAX_MINUTES * 60L < time(NULL)))	\
				exit(1);				\
			gDispose(date);					\
	}
#endif

#define DEMO_INIT  	demo_startup_time = time(NULL)

#define	DEMO_INC	demo_number_of_windows++

#define DEMO_VARS	time_t	demo_startup_time;		\
                        int	demo_number_of_windows

#define DEMO_MSG(h)						\
	if (0 == demo_number_of_windows%DEMO_MSG_FREQ)		\
		demo_message(h)

#else

#define DEMO_VARS
#define DEMO_INIT
#define	DEMO_CHK_MAX
#define DEMO_INC
#define DEMO_MSG(h)

#endif
