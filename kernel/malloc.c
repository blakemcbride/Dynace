
#include "dynl.h"

#include <stdlib.h>
#include <string.h>
#define us_char  unsigned char
#define us_short unsigned short
#define us_int   unsigned int

#ifndef	max
#define	max(a,b)	((a)>(b)?(a):(b))
#endif

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)malloc.c	5.9 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

/*
 * malloc.c (Caltech) 2/21/82
 * Chris Kingsley, kingsley@cit-20.
 *
 * This is a very fast storage allocator.  It allocates blocks of a small 
 * number of different sizes, and keeps free lists of each size.  Blocks that
 * don't exactly fit are passed up to the next larger size.  In this 
 * implementation, the available sizes are 2^n-4 (or 2^n-10) bytes long.
 * This is designed for use in a virtual memory environment.
 */

#include <sys/types.h>

//#define NULL 0

/*
 * The overhead on a block is at least 4 bytes.  When free, this space
 * contains a pointer to the next free block, and the bottom two bits must
 * be zero.  When in use, the first byte is set to MAGIC, and the second
 * byte is the size index.  The remaining bytes are for alignment.
 * If range checking is enabled then a second word holds the size of the
 * requested block, less 1, rounded up to a multiple of sizeof(RMAGIC).
 * The order of elements is critical: ov_magic must overlay the low order
 * bits of ov_next, and ov_magic can not be a valid ov_next bit pattern.
 */
union   overhead {
	union   overhead *ov_next;  /* when free */
	struct {
		us_char  ovu_magic;  /* magic number */
		us_char  ovu_index;  /* bucket # */
#ifdef RCHECK
		us_short ovu_rmagic; /* range magic number */
		us_int   ovu_size;   /* actual block size */
#endif
	} ovu;
#define ov_magic    ovu.ovu_magic
#define ov_index    ovu.ovu_index
#define ov_rmagic   ovu.ovu_rmagic
#define ov_size     ovu.ovu_size
};

#define MAGIC       0xef        /* magic # on accounting info */
#define RMAGIC      0x5555      /* magic # on range info */

#ifdef RCHECK
#define RSLOP       sizeof (us_short)
#else
#define RSLOP       0
#endif

static void morecore( int bucket );
static int findbucket( union overhead *, int );
static void  *getBlock( long blockSize );

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The overhead information
 * precedes the data area returned to the user.
 */
#define NBUCKETS 30
static  union overhead *nextf[NBUCKETS];
//extern  char *sbrk();

static  long pagesz = 0;         /* page size */
static  int  pagebucket = 0;         /* page size bucket */

#ifdef MSTATS
/*
 * nmalloc[i] is the difference between the number of mallocs and frees
 * for a given block size.
 */
static  us_int nmalloc[NBUCKETS];
#include <stdio.h>
#endif

//#if defined(DEBUG) || defined(RCHECK)
//#define ASSERT(p)   if (!(p)) botch("p")
//#include <stdio.h>
//static botch( char *s )
//{
//    fprintf(stderr, "\r\nassertion botched: %s\r\n", s);
//    (void) fflush(stderr);      /* just in case user buffered it */
//    abort();
//}
//#else
#define ASSERT(p)
//#endif

// These functions are defined in getinitialpagesize.c and
// getpagesize.c, repsectively.  By default, Dynace allocates
// memory in fixed-size blocks.  These functions can be used
// allocate a large initial block, followed by smaller subsequent
// allocations.  This can be important in improving the performance
// of memory-intensive applications.
extern  long Dynace_GetInitialPageSize(void);
extern  long Dynace_GetPageSize(void);

//  I'm using sizes a little shorter than a multiple of 2 to allow the underlying malloc
//  space for its own housekeeping

#if defined(MSC16)  ||  defined(WATCOM16)  ||  defined(BC16)  ||  defined(SYMC16)
#define getpagesize()		16320
#define getinitialpagesize()	16320
#else
#define getpagesize() 		Dynace_GetPageSize()
#define getinitialpagesize() 	Dynace_GetInitialPageSize()
#endif

void  *_Dynace_malloc( long nbytes )
{
	union overhead *op;
	int bucket;
	long n, amt;

	/*
	 * First time malloc is called, setup page size and
	 * align break pointer so all data will be page aligned.
	 */
	if (pagesz == 0) {
		pagesz = n = getpagesize();
//              op = (union overhead *)sbrk(0);
//              n = n - sizeof (*op) - ((int)op & (n - 1));
//              if (n < 0)
//                      n += pagesz;
//              if (n) {
//                      if (sbrk(n) == (char *)-1)
//                              return (NULL);
//        }
		bucket = 0;
		amt = 8;
		while (pagesz > amt) {
			amt <<= 1;
			bucket++;
		}
		pagebucket = bucket;
	}
	/*
	 * Convert amount of memory requested into closest block size
	 * stored in hash buckets which satisfies request.
	 * Account for space used per block for accounting.
	 */
	if (nbytes <= (n = pagesz - sizeof (*op) - RSLOP)) {
#ifndef RCHECK
		amt = 8;    /* size of first bucket */
		bucket = 0;
#else
		amt = 16;   /* size of first bucket */
		bucket = 1;
#endif
		n = -(long)(sizeof (*op) + RSLOP);
	} else {
		amt = pagesz;
		bucket = pagebucket;
	}
	while (nbytes > amt + n) {
		amt <<= 1;
		if (amt == 0)
			return (NULL);

		bucket++;
	}
	/*
	 * If nothing in hash bucket right now,
	 * request more memory from the system.
	 */
	if ((op = nextf[bucket]) == NULL) {
		morecore(bucket);
		if ((op = nextf[bucket]) == NULL)
			return (NULL);
	}
	/* remove from linked list */
	nextf[bucket] = op->ov_next;
	op->ov_magic = MAGIC;
	op->ov_index = bucket;
#ifdef MSTATS
	nmalloc[bucket]++;
#endif
#ifdef RCHECK
	/*
	 * Record allocated size of block and
	 * bound space with magic numbers.
	 */
	op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
	op->ov_rmagic = RMAGIC;
	*(us_short *)((char *)(op + 1) + op->ov_size) = RMAGIC;
#endif
	return ((char *)(op + 1));
}

void  *_Dynace_calloc( long size, long nelem )
{
  char  *rv = ( char  * )_Dynace_malloc(size*nelem);

  if (rv)
	memset( rv, 0, (size_t)(size * nelem));

  return( rv );
}

void _Dynace_free( void *cp )
{   
	long size;
	union overhead *op;

	if (cp == NULL)
		return;
	op = (union overhead *)((char *)cp - sizeof (union overhead));
#ifdef DEBUG
	ASSERT(op->ov_magic == MAGIC);      /* make sure it was in use */
#else
	if (op->ov_magic != MAGIC)
		return;             /* sanity */
#endif
#ifdef RCHECK
	ASSERT(op->ov_rmagic == RMAGIC);
	ASSERT(*(us_short *)((char *)(op + 1) + op->ov_size) == RMAGIC);
#endif
	size = op->ov_index;
	ASSERT(size < NBUCKETS);
	op->ov_next = nextf[size];  /* also clobbers ov_magic */
	nextf[size] = op;
#ifdef MSTATS
	nmalloc[size]--;
#endif
}

/*
 * When a program attempts "storage compaction" as mentioned in the
 * old malloc man page, it realloc's an already freed block.  Usually
 * this is the last block it freed; occasionally it might be farther
 * back.  We have to search all the free lists for the block in order
 * to determine its bucket: 1st we make one pass thru the lists
 * checking only the first block in each; if that fails we search
 * ``realloc_srchlen'' blocks in each list for a match (the variable
 * is extern so the caller can modify it).  If that fails we just copy
 * however many bytes was given to realloc() and hope it's not huge.
 */
static int realloc_srchlen = 4;    /* 4 should be plenty, -1 =>'s whole list */

void  *_Dynace_realloc(void  *cp, long nbytes )
{   
	long onb;
	long i;
	union overhead *op;
	char  *res;
	long was_alloced = 0;

	if (cp == NULL)
		return _Dynace_malloc(nbytes);
	op = (union overhead *)((char *)cp - sizeof (union overhead));
	if (op->ov_magic == MAGIC) {
		was_alloced++;
		i = op->ov_index;
	} else {
		/*
		 * Already free, doing "compaction".
		 *
		 * Search for the old block of memory on the
		 * free list.  First, check the most common
		 * case (last element free'd), then (this failing)
		 * the last ``realloc_srchlen'' items free'd.
		 * If all lookups fail, then assume the size of
		 * the memory block being realloc'd is the
		 * largest possible (so that all "nbytes" of new
		 * memory are copied into).  Note that this could cause
		 * a memory fault if the old area was tiny, and the moon
		 * is gibbous.  However, that is very unlikely.
		 */
		if ((i = findbucket(op, 1)) < 0 &&
			(i = findbucket(op, realloc_srchlen)) < 0)
			i = NBUCKETS;
	}
	onb = 1L << (i + 3);
	if (onb < pagesz)
		onb -= sizeof (*op) + RSLOP;
	else
		onb += pagesz - sizeof (*op) - RSLOP;
	/* avoid the copy if same size block */
	if (was_alloced) {
		if (i) {
			i = 1L << (i + 2);
			if (i < pagesz)
				i -= sizeof (*op) + RSLOP;
			else
				i += pagesz - sizeof (*op) - RSLOP;
		}
		if (nbytes <= onb && nbytes > i) {
#ifdef RCHECK
			op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
			*(us_short *)((char *)(op + 1) + op->ov_size) = RMAGIC;
#endif
			return(cp);
		} else
			_Dynace_free(cp);
	}
	if ((res = _Dynace_malloc(nbytes)) == NULL)
		return (NULL);
	if (cp != res)      /* common optimization if "compacting" */
		memcpy(res, cp, (size_t)((nbytes < onb) ? nbytes : onb));
	return (res);
}

/*
 * Search ``srchlen'' elements of each free list for a block whose
 * header starts at ``freep''.  If srchlen is -1 search the whole list.
 * Return bucket number, or -1 if not found.
 */
static int findbucket( union overhead *freep, int srchlen )
{
	union overhead *p;
	int i, j;

	for (i = 0; i < NBUCKETS; i++) {
		j = 0;
		for (p = nextf[i]; p && j != srchlen; p = p->ov_next) {
			if (p == freep)
				return (i);
			j++;
		}
	}
	return (-1);
}

/*
 * Allocate more memory to the indicated bucket.
 */
static void morecore(int bucket)
{
	union overhead *op;
	long sz;        /* size of desired block */
	long amt;            /* amount to allocate */
	long nblks;          /* how many blocks we get */

	/*
	 * sbrk_size <= 0 only for big, FLUFFY, requests (about
	 * 2^30 bytes on a VAX, I think) or for a negative arg.
	 */
	sz = 1L << (bucket + 3);
#ifdef DEBUG
	ASSERT(sz > 0);
#else
	if (sz <= 0)
		return;
#endif
//  if (sz < pagesz) {
//      amt = pagesz;
//      nblks = amt / sz;
//  } else {
//      amt = sz + pagesz;
//      nblks = 1;
//  }

	/*
	 * Only allocate what we need.  The commented code above will, when
	 * active, allocate an entire page.  In the code below here, we will
	 * subdivide the page into 'nblks'.  These blocks are strung together
	 * into the bucket.
	 *
	 * With the code immediately below, we are only allocating 1 block.
	 * This block is being allocated from a page.  The rest of the page
	 * will be allocated to additional requests.
	 */
	amt = sz;
	nblks = 1;

	op = (union overhead *)getBlock( amt );
	/* no more room! */
	if ( !op  )
		return;
	/*
	 * Add new memory allocated to that on
	 * free list for this hash bucket.
	 */
	op->ov_next = NULL;
	nextf[bucket] = op;
//  while (--nblks > 0) {
//      op->ov_next = (union overhead *)((char *)op + sz);
//      op = (union overhead *)((char *)op + sz);
//  }
}

typedef struct _availList
{
	struct _availList *next;
	long               availAmt;
	char           *memBlock;
} al;

static al *availList = NULL;

extern	void	*_Dynace_getpage(long len);

static	void  *getBlock( long blockSize )
{
	char  *returnBlock;
	al *tempList = availList;
	static long s_pagesz = 0;
	long curr_pagesz = s_pagesz;

	//The first block allocated may be larger than subsequent blocks.
	//Subsequent allocations are assumed to be all the same size.
	if ( !s_pagesz ) {
		curr_pagesz = getinitialpagesize();
		s_pagesz = getpagesize();
	}
	
	while( tempList )
	{
		if ( tempList->availAmt >= blockSize )
			break;

		tempList = tempList->next;
	}

	// Can't find a place to put request.  Allocate a new page.
	if ( !tempList )
	{
		/*
		 * Allocate a page for sub-allocation.
		 */
		returnBlock = _Dynace_getpage( max( curr_pagesz, blockSize + 16 ) );
		if ( !returnBlock )
			return( NULL );

		/*
		 * Build an entry to be put into the availList.
		 */
		tempList = ( al * )returnBlock;
		returnBlock += sizeof( al );

		/*
		 * Our available memory region starts after our overhead.
		 */
		tempList->memBlock = returnBlock;
		tempList->availAmt = max( curr_pagesz, blockSize + 16 ) - sizeof( al );

		tempList->next = availList;
		availList = tempList;
	}

	/*
	 * Extract the amount of memory we need to satisfy the request.
	 */
	returnBlock = tempList->memBlock;
	tempList->memBlock += blockSize;
	tempList->availAmt -= blockSize;

	return( returnBlock );

}

#ifdef MSTATS
/*
 * mstats - print out statistics about malloc
 * 
 * Prints two lines of numbers, one showing the length of the free list
 * for each size category, the second showing the number of mallocs -
 * frees for each size category.
 */
mstats(s)
char *s;
{
	int i, j;
	union overhead *p;
	int totfree = 0,
	totused = 0;

	fprintf(stderr, "Memory allocation statistics %s\nfree:\t", s);
	for (i = 0; i < NBUCKETS; i++) {
		for (j = 0, p = nextf[i]; p; p = p->ov_next, j++)
			;
		fprintf(stderr, " %d", j);
		totfree += j * (1L << (i + 3));
	}
	fprintf(stderr, "\nused:\t");
	for (i = 0; i < NBUCKETS; i++) {
		fprintf(stderr, " %d", nmalloc[i]);
		totused += nmalloc[i] * (1L << (i + 3));
	}
	fprintf(stderr, "\n\tTotal in use: %d, total free: %d\n",
		totused, totfree);
}
#endif
