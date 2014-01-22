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




/*  This really just a .c file but is copied to a .d file in case someone
    removes all the .c files from the class directory.  Just copy it to
    memalloc.c
*/

/*  Reasonably fast memory allocator which reduces fragmentation and has
    the ability to compact used memory.  */

#include "dynl.h"
#include <string.h>
#include "memalloc.h"


typedef	struct  _header  {
	char	status;		/*  Used/Free/ **froZen**	*/
	struct	_header	*next;	/*  Next free header or pointer to pointer
				    using this memory block	*/
	unsigned  rsize;	/*  request size		*/
	unsigned  size;		/*  mem size			*/
/*	char	mem[];		    Actual memory size varys	*/
}	Header;

typedef	struct  _memBlk  {
	struct	_memBlk	*next;
	unsigned	size;	/*  size of h block  		*/
/*	Header	h[];		    Actual size vary		*/
}	MemBlk;


typedef	struct	{
	unsigned  userBytes;	/*  minimum user bytes per header	*/
	Header	*h;		/*  pointer to first free header	*/
}	MAP;

#define SZ(x)	(sizeof(Header)*x)

#define RND(n)	(((n-1) / sizeof(Header)) + 1) * sizeof(Header);

static	MAP	Map[] = {
	{SZ(1)}, 
	{SZ(2)}, 
	{SZ(4)}, 
	{SZ(8)}, 
	{SZ(16)}, 
	{SZ(32)}, 
	{SZ(64)}, 
	{SZ(128)}, 
	{SZ(256)}, 
	{SZ(512)}, 
	{SZ(1024)}, 
	{SZ(2048)}, 
	{0}
};

	
	
static	MemBlk	*MMBP = NULL;		/*  master memory block pointer  */

static	unsigned	DBS = 1024;	/*  Default block size 	*/

static	void	more_core(unsigned);
static	Header	*compact(MemBlk *);

static	CRITICALSECTION	CS;
static	int	CS_init = 0;

void	*MA_malloc(unsigned n, void *p)
            	/*  requested size  */
    	   	/*  pointer to variable which will hold the return value  */
{
	unsigned  sz;	/*  rounded size needed		*/
	int	i;	/*  map index			*/
	Header	*h;	/*  header			*/
	Header	*ph;	/*  previous header		*/
	Header	*bh;	/*  best header			*/
	Header	*pbh;	/*  previous best header	*/
	unsigned tnb;	/*  total number of blocks	*/
	unsigned nbn;	/*  number of blocks needed	*/
	unsigned nbl;	/*  number of blocks left	*/

	if (!CS_init) {
		INITIALIZECRITICALSECTION(CS);
		CS_init = 1;
	}
	ENTERCRITICALSECTION(CS);
	if (!n)
		++n;
	sz = RND(n);

	/*  find starting bucket  */
	for (i=1 ; Map[i].userBytes  &&  Map[i].userBytes <= sz ; ++i);
	i--;
	

	/*  find best fit  */
	for (bh=pbh=ph=NULL, h=Map[i].h ; h ; ph=h, h=h->next)
		if (h->size == sz)  {  /*  exact match  */
			if (ph)
				ph->next = h->next;
			else
				Map[i].h = h->next;
			h->status = 'U';
			h->next = (Header *) p;
			h->rsize = n;
			LEAVECRITICALSECTION(CS);
			return h+1;
		} else if (h->size > sz  &&  (!bh  ||  bh->size > h->size))  {
			pbh = ph;
			bh = h;
		}
	/*  exact match not found - use best fit if found  */
	if (bh)  {
		if (pbh)
			pbh->next = bh->next;
		else
			Map[i].h = bh->next;
		bh->status = 'U';
		bh->next = (Header *) p;
		bh->rsize = n;
		LEAVECRITICALSECTION(CS);
		return bh+1;
	}


	/*  find a block in any larger block  */
	for (i++ ; Map[i].userBytes  &&  !Map[i].h ; ++i);

	if (!Map[i].userBytes)  {   /*  no block large enough at all  */
		more_core(n);
		LEAVECRITICALSECTION(CS);
		return MA_malloc(n, p);
	}

	/*  larger than necessary block found - split  */

	/*  unlink block to be used  */

	h = Map[i].h;
	Map[i].h = h->next;

	tnb = h->size / sizeof(Header);
	nbn = sz / sizeof(Header);
	nbl = tnb - nbn;
	if (nbl > 5)  {
		bh = h + (nbn + 1);	/*  remainder		*/
		bh->status = 'U';	/*  make believe	*/
		bh->next = NULL;
		bh->rsize = bh->size = (nbl - 1) * sizeof(Header);
		h->size -= nbl * sizeof(Header);
		MA_free(bh+1);
	}
	h->status = 'U';
	h->next = (Header *) p;
	h->rsize = n;
	LEAVECRITICALSECTION(CS);
	return h+1;
}

void	*MA_calloc(unsigned n, void *p)
{
	p = MA_malloc(n, p);
	memset(p, 0, n);
	return p;
}

void	MA_free(void *arg)
{
	int	i;
	Header	*h = (Header *) arg;

	ENTERCRITICALSECTION(CS);
	if (!h  ||  (--h)->status != 'U') {
		LEAVECRITICALSECTION(CS);
		return;
	}

	/*  find bucket  */
	for (i=1 ; Map[i].userBytes  &&  Map[i].userBytes <= h->size ; ++i);
	i--;

	h->status = 'F';
	if (h->next)
		*((char **) h->next) = NULL;
	h->next = Map[i].h;
	Map[i].h = h;
	LEAVECRITICALSECTION(CS);
}

static	void	more_core(unsigned n)
{
	unsigned  sz;	/*  memory area needed		*/
	unsigned  as;	/*  allocation size		*/
	MemBlk	 *mb;
	Header	 *h;

	sz = n > DBS ? n : DBS;
	sz = RND(sz);
	as = sizeof(MemBlk) + sizeof(Header) + sz;
	mb = (MemBlk *) malloc(as);
	if (!mb)  {
#ifndef	__COSMIC__
		fprintf(stderr, "\nOut of memory.\n");
#endif
		exit(1);
	}
	mb->next = MMBP;
	MMBP = mb;
	mb->size = sz + sizeof(Header);

	h = (Header *) (mb+1);
	h->status = 'U';	/*  make believe	*/
	h->next = NULL;
	h->size = sz;
	MA_free(h+1);
}

void	*MA_realloc(void *arg, unsigned n)
{
	char	*m;
	Header	*h = (Header *) arg;

	ENTERCRITICALSECTION(CS);
	if (!h  ||  (--h)->status != 'U') {
		LEAVECRITICALSECTION(CS);
		return NULL;
	}
	if (h->size >= n)  {
		h->rsize = n;
		LEAVECRITICALSECTION(CS);
		return h+1;
	}
	m = (char *) MA_malloc(n, h->next);
	memcpy(m, h+1, h->rsize);
	h->next = NULL;		/*  don't NULL out variable pointer  */
	MA_free(h+1);
	LEAVECRITICALSECTION(CS);
	return m;
}


void	MA_compact(void)
{
	MemBlk	*mb;
	Header	*fh, *mfh = NULL;
	int	i;

	ENTERCRITICALSECTION(CS);
	/*  compact each memBlk   */
	for (mb=MMBP ; mb ; mb = mb->next)  {
		fh = compact(mb);
		if (fh)  {
			fh->next = mfh;
			mfh = fh;
		}
	}
	
	/*  clear out free map  */
	for (i=0 ; Map[i].userBytes ; ++i)
		Map[i].h = NULL;

	/*  rebuild free map  */
	while (fh = mfh)  {
		mfh = fh->next;
		fh->status = 'U';	/*  make believe  */
		fh->next = NULL;
		MA_free(fh+1);
	}
	LEAVECRITICALSECTION(CS);
}

#define NEXTH(h)	(Header *) ((char *) h + (sizeof(Header)+h->size))

static	Header	*compact(MemBlk *mb)
{
	char	*end = (char *) (mb+1) + mb->size;
	Header	*h = (Header *) (mb+1);
	Header	*to, *pto=NULL;

	for (to=h ; (char *) h < end ; h = NEXTH(h))  {
		if (h->status == 'F')
			continue;
		if (to != h)  {
			memcpy(to, h, h->rsize+sizeof(Header));
			if (h->next)
				*((Header **) to->next) = to + 1;
		}
		to->size = RND(to->rsize);
		pto = to;
		to = NEXTH(to);
	}
	if ((char *) to >= end)
		return NULL;
	to->status = 'F';
	to->size = (end - (char *) to) - sizeof(Header);
	if (!to->size)  {
		if (pto)
			pto->size += to->size + sizeof(Header);
		return NULL;
	}			
	return to;
}







