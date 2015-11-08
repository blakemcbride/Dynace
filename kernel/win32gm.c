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



#ifndef	BOEHM_GC

#ifndef TEST
#include "generics.h"
#define	ABORT(x)	gError(Dynace, x)
#else
#include <windows.h>
#include <stdlib.h>
#define	ABORT(x)	exit(-1)
#endif

extern	int	_gc_chk_ptr(char *p, unsigned len);


static void cond_add_roots(char *base, char *limit)
{
#ifdef TEST
	char	*p = base;
	int volatile i;
	char	buf[40];

	for ( ; p < limit ; p += sizeof(char *))
		i = *(int *)p;
	sprintf(buf, "%lx %lx  %ld", base, limit, limit-base);
	printf("%s\n", buf);
#else
	if ((base > (char *)&base  ||  limit < (char *) &base)  &&  !_gc_chk_ptr(base, (unsigned)(limit-base)))  //  don't mark the stack!
		gMarkRange(Dynace, (char **)base, (char **)limit);
#endif
}



static  DWORD allocation_granularity;

  
typedef	char *ptr_t;

#ifdef	malloc   /*  use real malloc!  */
#undef malloc
#endif

int _is_heap_base(ptr_t p)
{
     
//	register unsigned i;
     
#    ifndef REDIRECT_MALLOC
	static ptr_t malloc_heap_pointer = 0;
     
	if (0 == malloc_heap_pointer) {
		MEMORY_BASIC_INFORMATION buf;
/*		register DWORD result = VirtualQuery(malloc(1), &buf, sizeof(buf));    */
		SIZE_T result = VirtualQuery(malloc(1), &buf, sizeof(buf));
         
		if (result != sizeof(buf)) {
			ABORT("Weird VirtualQuery result");
		}
		malloc_heap_pointer = (ptr_t)(buf.AllocationBase);
	}
	if (p == malloc_heap_pointer)
		return TRUE;
#    endif

#if 0
	for (i = 0; i < n_heap_bases; i++) {
		if (heap_bases[i] == p) return(TRUE);
	}
#endif
	return(FALSE);
}

void _register_global_memory()
{
	MEMORY_BASIC_INFORMATION buf;
	SYSTEM_INFO sysinfo;
/*	DWORD result;  */
	SIZE_T result;
	DWORD protect;
	LPVOID p;
	char * base;
	char * limit, * new_limit;
    
	GetSystemInfo(&sysinfo);
	base = limit = p = sysinfo.lpMinimumApplicationAddress;
	allocation_granularity = sysinfo.dwAllocationGranularity;
	while (p < sysinfo.lpMaximumApplicationAddress) {
		result = VirtualQuery(p, &buf, sizeof(buf));
		if (result != sizeof(buf)) {
			ABORT("Weird VirtualQuery result");
		}
		new_limit = (char *)p + buf.RegionSize;
		protect = buf.Protect;
		if (buf.State == MEM_COMMIT
		    && (protect == PAGE_EXECUTE_READWRITE
			|| protect == PAGE_READWRITE)
		    && !_is_heap_base(buf.AllocationBase)) {
			if ((char *)p == limit) {
				limit = new_limit;
			} else {
				cond_add_roots(base, limit);
				base = p;
				limit = new_limit;
			}
		}
		if (p > (LPVOID)new_limit /* overflow */) break;
		p = (LPVOID)new_limit;
	}
	cond_add_roots(base, limit);
}

#endif  /*  not BOEHM_GC  */

#ifdef TEST

main()
{
	static	int	var;
	printf("%lx\n", &var);
	printf("%lx\n", &allocation_granularity);
	_register_global_memory();
	return 0;
}

#endif
