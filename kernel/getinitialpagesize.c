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



/*******************************************************************
 Notes:

 Dynace allocates large blocks ("pages") of memory and then suballocates from
 those pages to satisfy individual requests, such as when objects are
 created.  The page size has a bearing on Dynace performance because
 Dynace checks each object reference to ensure it is a valid address,
 that is, within one of the large blocks of allocated memory.  The
 larger the number of these pages, the longer it takes to validate
 object references.  So, by telling Dynace to allocate fewer, larger
 blocks, application performance can be improved.

 getpagesize.c and getinitialpagesize.c are used to override Dynace's
 default page allocation strategy.  By default, the initial and
 subsequent allocations are all the same size.  However, memory and
 CPU-intensive applications can benefit from a strategy that allocates
 a fairly lage block initially to cover memory requirements during the
 startup.  Subsequent memory allocations can be smaller, but should still
 be large enough to significantly reduce the number of pages required.

 You can override one or both of these routines, but if you override
 getpagesize, you will almost certainly want to write a version of
 getinitialpagesize that provides for a larger initial memory allocation.

 *******************************************************************/

extern long Dynace_GetPageSize(void);

long	Dynace_GetInitialPageSize()
{
	return Dynace_GetPageSize();
}
