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




/* The next three lines are used to define the type used for indexing  */

#define INDEX_TYPE	unsigned 

#if defined(unix)  ||  defined(M_I386)
#define AT_INDEX	AT_LONG
#define PRNT_SHAPE	"%u "
#else
#define AT_INDEX	AT_USHT
#define PRNT_SHAPE	"%hu "
#endif

#define BITS_PER_BYTE	8
#define BIT_VAL(x,y) (((char *) (x))[(INDEX_TYPE) (y) / (INDEX_TYPE) BITS_PER_BYTE] & pow1[(INDEX_TYPE) (y) % (INDEX_TYPE) BITS_PER_BYTE])

#define	SET_BIT(p,off,val)	if (val)				   \
	((char *) (p))[off / BITS_PER_BYTE] |= pow1[off % BITS_PER_BYTE];   \
		else							   \
	((char *) (p))[off / BITS_PER_BYTE] &= pow2[off % BITS_PER_BYTE]

/*  Returns the size of the data portion of an array  */

#define SIZE(type,n)  (type != AT_BIT ? (INDEX_TYPE) _A_esize(type) * n	\
                      : (n ? (n+(BITS_PER_BYTE-1)) / BITS_PER_BYTE : 0L))


typedef	unsigned short _ushort;








