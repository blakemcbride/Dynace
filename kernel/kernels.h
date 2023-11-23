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


/*  misc data types used by the kernel  */

#ifndef _KERNELS_H
#define _KERNELS_H

#include "generics.h" /* object */


typedef	struct	_object_list  {
	object			obj;
	struct	_object_list	*next;
}	object_list;

typedef	struct	_iv_offset_def_list  {
	object	superclass;
	int	iv_size;
	int	iv_offset;
	struct _iv_offset_def_list *next;
}	iv_offset_def_list;

typedef	struct	_instance_block  {
	struct	_instance_block	*next;
#ifdef sparc
	unsigned long dummy;	 /* align on 8 byte boundary */
#endif
}	instance_block;

typedef	struct	_free_list  {
	struct	_free_list	*next;
        unsigned short tag;
        unsigned short siz;
        unsigned long sn;
}	free_list;



#endif


