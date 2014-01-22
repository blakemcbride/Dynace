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





#include <stdio.h>
#include <dos.h>
#include <stdlib.h>

#pragma check_stack( off )
#pragma check_pointer( off )

#define TICKPERMIN  1092L
#define MINPERHOUR  60L

#define __interrupt _interrupt
#define __far _far

static	void (__interrupt __far *oldtimer)();
static	void __interrupt __far newtimer();


#ifdef	__cplusplus
extern "C"  {
#endif
#ifdef TEST
int	_tick_count=10000;
#else
extern	int	_tick_count;
#endif
#ifdef	__cplusplus
}
#endif

static	void	_end_timer(void)
{
	_dos_setvect( 0x1c, oldtimer );
}

void	_start_timer(void)
{
	oldtimer = _dos_getvect( (unsigned)0x1c );
	_dos_setvect( 0x1c, newtimer );
	atexit( _end_timer );
}

static	void __interrupt __far newtimer(void)
{
	if (_tick_count)
		_tick_count--;
        _chain_intr( oldtimer );
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



