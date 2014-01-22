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

#ifndef	_DYNL_H
#define	_DYNL_H

/*  What kind of machine?  */

#ifdef	_MSC_VER
#if	_MSC_VER >= 900  &&  defined(_M_IX86)
#define	MSC32
#else
#define MSC16
#endif
#endif

#ifdef	__WATCOMC__
#ifdef	M_I86
#define	WATCOM16
#else
#define WATCOM32
#endif
#endif


#if	defined(__TURBOC__)  ||  defined(__BORLANDC__)
#ifdef	__MSDOS__
#define BC16
#else
#define BC32
#endif
#endif


#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>


#ifdef _WINDOWS
#if defined(MSC32)  &&  !defined(WIN32)
#define	WIN32
#endif
#include <windows.h>
#include <windowsx.h>
#undef	NULL
#define NULL	((void *)0)
#include "dynwin.h"
#endif


#ifdef	__cplusplus
extern "C"  {
#endif


#if	defined(MSC32)  ||  defined(BC32)
typedef	struct	_object	 *object;
#else
typedef	struct	_object	 * volatile object;
#endif

typedef	object	(*ofun)();

#define	_cat2_(a, b)	a##b
#define _cat_(a, b)	_cat2_(a, b)
#define Generic(x)	_cat_(x, _i)


/*  from the Stream class  */
extern	object	stdoutStream, stdinStream, stderrStream;
extern	object	traceStream;



#if !defined(unix) && !defined(vms) && !defined(__WATCOMC__) && _M_IX86 < 300
#define _HUGE_	huge
#else
#define _HUGE_
#endif



#ifdef	__cplusplus
}
#endif


#endif  /*  _DYNL_H  */





