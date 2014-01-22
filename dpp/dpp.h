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


#ifdef	USE_GC
#define	DISPOSE(x)	(object) NULL
#define	DEEPDISPOSE(x)	(object) NULL
#else
#define	DISPOSE(x)	gDispose(x)
#define	DEEPDISPOSE(x)	gDeepDispose(x)
#endif


#define strne(a, b)	strcmp(a, b)
#define streq(a, b)	!strcmp(a, b)

#define istart(x)	(isalpha(x)  ||  (x) == '_')
#define irest(x)	(isalnum(x)  ||  (x) == '_')

#define isInputArg(s)   (!strcmp(s, "int *")  ||  !strcmp(s, "long *") ||  !strcmp(s, "double *") ||		\
			 !strcmp(s, "object *")  || !strcmp(s, "short *")  ||  !strcmp(s, "unsigned *")  ||	\
			 !strcmp(s, "unsigned long *")  ||  !strcmp(s, "LRESULT *")  ||  			\
			 !strcmp(s, "UDWORD *")  ||  !strcmp(s, "DWORD *")  ||  !strcmp(s, "SWORD *")  ||	\
			 !strcmp(s, "unsigned short *")  || !strcmp(s, "RETCODE *"))
#define isShortArg(s)	(!strcmp(s, "short *")  ||  !strcmp(s, "SWORD *")  || !strcmp(s, "RETCODE *"))
#define isUShortArg(s)	!strcmp(s, "unsigned short *")
#define isLongArg(s)	(!strcmp(s, "int *")  ||  !strcmp(s, "long *") ||  !strcmp(s, "unsigned *")  ||		\
			 !strcmp(s, "unsigned long *")  ||  !strcmp(s, "LRESULT *")  ||  			\
			 !strcmp(s, "UDWORD *")  ||  !strcmp(s, "DWORD *"))
#define isDoubleArg(s)	!strcmp(s, "double *")
#define isObjectArg(s)  !strcmp(s, "object *")




