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


#include "generics.h"

#include <string.h>

#if !defined(__COSMIC__)  &&  !defined(PLAN9)
#include <time.h>
#if	!defined(unix) && !defined(__APPLE__)  &&  !defined(__MWERKS__)  &&  !defined(__minix)
#include <dos.h>
#endif
#endif

#if	defined(MSC32)  ||  defined(BC32)  ||  defined(SYMC32)
#include <signal.h>
#include <setjmp.h>
/*#define	USE_SIGNAL*/
#endif

/*  #define	NO_CACHE  */

#if defined(NATIVE_THREADS) &&  defined(__unix__)
pthread_mutexattr_t _mutex_attr;
#endif

#define ALIGN2
#define ALIGNMENT (2)

#ifdef	BC32
#pragma inline
#endif

#if	(defined(WIN32)  ||  defined(__APPLE__))  &&  !defined(BOEHM_GC)
#define	HEAP_HAS_HOLES
extern	void  *_Dynace_malloc( long nbytes );
extern	void  *_Dynace_calloc( long size, long nelem );
extern	void  _Dynace_free( void *cp );
extern	void  *_Dynace_realloc(void  *cp, long nbytes );

#define	malloc(n)	_Dynace_malloc(n)
#define	calloc(a, b)	_Dynace_calloc((a), (b))
#define	realloc(a,b)	_Dynace_realloc((a), (b))
#define	free(x)		_Dynace_free(x)
#endif

#include "kernels.h"

#include "Object.iv"

#include "Method.iv"

#include "GenericFunction.iv"

#include "Behavior.iv"


/*  keep list of global memory ranges that should be marked during garbage
    collection.  */

typedef	struct	memory_range  {	
	struct	memory_range	*next;
	struct	memory_range	*prev;
	void	*beg;
	long	size;
}	GMR;

#define set_class(a, b)		a->cls = b

#define SCIV(x)	((Behavior_iv_t *) ((object)(x) + 1))  /* Behavior_iv_t IVs */


#ifdef	ALIGN4	/* align 4 bytes */
#define EVEN(x)		((((INT_PTR)(x) + 3) >> 2) << 2)
#else		/*  align 2 bytes  */
#define EVEN(x)		((x) + ((x) % 2))
#endif


int	_CheckObjects_ = 1;		/*  turn object checking on/off  */

object	_LastGeneric_ = NULL;		/*  last generic function called  */

int	_no_context_switch = 0;   /*  if set inhibits all context switching */

int	_tick_count = 1;

CRITICALSECTION  _CI_CS_;		/*  used during class initialization  */

long	_SIG1;	       		/*  Exception handler signature  */
long	_SIG2;


#if	defined(__WATCOMC__)  &&  defined(__cplusplus)
extern	"C"	void	(*__dynace_yield)(void) = NULL;
#else
void	(*__dynace_yield)(void) = NULL;
#endif


static	int	TRACES = DYNACE_TRACE_OFF;	/*  turn tracing off/on		*/
static	object	MCL = NULL;		/*  master class list		*/
static	object	MGL = NULL;		/*  master generic function list  */
static	object	MML = NULL;		/*  master method list		*/
static	int	CID = 0;		/*  next class id		*/
static	int	GID = 0;		/*  next generic id		*/
static	int	Crows = 199;		/*  cache rows (generics)	*/
static	int	Ccols = 59;		/*  cache cols (classes)	*/
static	long	SIG1 = 0L;		/*  class signatures		*/
static	long	SIG2;
static	char	*StackBeg;		/*  pointer to the beginning of the stack  */
static	GMR	*LGMR = NULL;		/*  list of GMR's  		*/
static	GMR	*FGMR = NULL;		/*  free GMR objects		*/
static	int	NUM_CLASSES;		/*  number of classes (max)     */
static	unsigned long	ObjectSN;  	/*  Object Serial Number	*/

static	CRITICALSECTION  MCL_CS;  	/*  and CID  */
static	CRITICALSECTION  MGL_CS;  	/*  and GID  */
static	CRITICALSECTION  MML_CS;
static	CRITICALSECTION  GMR_CS;
static	CRITICALSECTION	 GC_CS;
static	CRITICALSECTION	 CMU_CS;	/*  used for CMU & ObjectSN	*/
static	int	In_GC;			/*  avoid use of slow GC_CS 	*/


/*  storage statistic variables  */

static	long	MBU = 0L;		/*  max bytes used by objects after GC	*/
static	long	GBA = -1L;		/*  garbage buffer area		        */
static	long	CMU = 0L;		/*  current memory usage by objects     */
static	long	MSU = 0L;		/*  maximum storage used by objects     */
static	long	NGC = 0L;		/*  number of garbage collections	*/

#define cache_entry(g, c)		g + c

extern	object	Object_c, Behavior_c, Class_c, MetaClass_c;
extern	object	GenericFunction_c, Method_c, Dynace_c;

static	object	metaObject, metaClass, metaBehavior, metaMetaClass;
static	object	metaMethod, metaGenericFunction;


/*  object status flags  */

#define OBJ_FREE	0x8000
#define OBJ_USED	0x4000
#define OBJ_MARKED	0x2000
#define OBJ_MASK	(OBJ_FREE | OBJ_USED | OBJ_MARKED)

/*  object allocation types  */

#define	ALLOC_HEAP	0x1000
#define	ALLOC_STACK	0x0800
#define	ALLOC_MASK	(ALLOC_HEAP | ALLOC_STACK)

#define	ALL_MASK	(OBJ_MASK | ALLOC_MASK)

#define GMR_BLOCK_SIZE	20


/*  Maximum number of immediate (not including inherited) superclasses per
    class.  Arbitrarly set to an unrealisticly large number which should
    never be reached.
*/

#define MIS	20



/*  Local function declarations   */

LOCAL	void add_ptr(char *p,unsigned len);
LOCAL	struct memory_range *new_gmr(void);
LOCAL	void free_gmr(struct memory_range *gmr);
LOCAL	void extend_storage(Behavior_iv_t *cv);
LOCAL	objrtn Behavior_im_gNew(object self);
LOCAL	void mk_subclass_link(object sc,object c);
LOCAL	void set_superclass(object obj,object sc);
LOCAL	char *strsave(char *x);
LOCAL	void new_class(object c,Behavior_iv_t *cv,char *name);
LOCAL	objrtn defClass(object cls,char *name,int effective_iv_size,int direct_iv_size,int nipib);
LOCAL  objrtn GenericFunction_cm_gNewGeneric(object self, char *name, void *fp);
LOCAL  objrtn GenericFunction_cm_gGetAll(object self);
LOCAL	objrtn Method_cm_gNewMethod(object self, char *name, object cls, object generic, ofun methf, ofun methf2);
LOCAL	objrtn Dynace_cm_gSetMemoryBufferArea(object self,long sz);
LOCAL	Method_iv_t *find_method(object generic,object cls,int level, object *mo);
LOCAL	struct _iv_offset_def_list *mk_sciv_list(struct _iv_offset_def_list *v,object sc);
LOCAL	void remove_dup_sc(struct _iv_offset_def_list *v);
LOCAL	int calc_offsets(struct _iv_offset_def_list *v);
LOCAL	objrtn Object_im_gDispose(object self);
LOCAL	void Dynace_cm_gMarkObject(object self,object obj);
LOCAL	void Dynace_cm_gMarkRange(object self,char _HUGE_ **from ,char _HUGE_ **to );
LOCAL	void rebuild_free_list(Behavior_iv_t *cv,int is);
LOCAL	void gc_sweep(void);
LOCAL	void get_mem_stats(void);
LOCAL	objrtn Dynace_cm_gGC(object self);
LOCAL	void *Dynace_cm_gRegisterMemory(object self,void *beg,long size);
/*LOCAL	void Dynace_cm_gRemoveAllRegisteredMemory(object self); */
LOCAL	void Dynace_cm_gRemoveRegisteredMemory(object self,void *p);
LOCAL	void *Dynace_cm_gChangeRegisteredMemory(object self,void *p,void *beg,long size);
#ifdef	BOEHM_GC
LOCAL	void dispose_boehm_gc(object obj, void *p);
#endif

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
LOCAL	void create_cache(int rows,int cols);
LOCAL	void free_cache(void);
LOCAL	void rebuild_class_indexes(void);
LOCAL	void rebuild_generic_indexes(void);
LOCAL	objrtn Dynace_cm_gResizeMethodCache(object self,int classes,int generics);
LOCAL	void cache_add(int row,int col,object generic,object cls,int level,object meth);
#endif

#if	defined(WIN32)  &&  !defined(BOEHM_GC)    /*  routines located in win32gm.c  */
extern int _is_heap_base(char* p);
extern void _register_global_memory(void);
#endif


void	*Chkmem(void *p, char *file, int line)
{
	char	buf[90];
	
	if (!p)  {
		sprintf(buf, "\nDynace: Out of memory in %s at line %d.\n", file, line);
		gError(Dynace_c, buf);
	}
	return p;
}

#ifdef	SEGMENTED_MEMORY


typedef	struct  _sl  {		/*  keep list of segment and ranges allocated */
	unsigned	seg;
	unsigned	min_off;
	unsigned	max_off;
	struct	_sl	*next;
}	SL;

static	SL	*MSL = NULL;		/*  Master segment list		*/

static	CRITICALSECTION   MSL_CS;


LOCAL	void	add_ptr(char *p, unsigned len)
{
	register unsigned	seg = FP_SEG(p);
	unsigned	off = FP_OFF(p);
	unsigned	end = off + len;
	SL	*sl;

	ENTERCRITICALSECTION(MSL_CS);
	for (sl = MSL ; sl &&  sl->seg != seg ; sl = sl->next);
	if (!sl)  {
		sl = Tcalloc(SL);
		sl->seg = seg;
		sl->min_off = off;
		sl->max_off = end;
		sl->next = MSL;
		MSL = sl;
	} else {
		if (off < sl->min_off)
			sl->min_off = off;
		if (end > sl->max_off)
			sl->max_off = end;
	}
	LEAVECRITICALSECTION(MSL_CS);
}

LOCAL	_INLINE_	int	chk_ptr(char *p, unsigned len)
{
	register unsigned	seg = FP_SEG(p);
	unsigned	off;
	SL	*sl;
	int	r;

	ENTERCRITICALSECTION(MSL_CS);
	for (sl = MSL ; sl &&  sl->seg != seg ; sl = sl->next);
	r = sl && (off=FP_OFF(p)) >= sl->min_off  &&  off+len <= sl->max_off;
	LEAVECRITICALSECTION(MSL_CS);
	return r;
}

#else  /*  not SEGMENTED_MEMORY   */

#ifndef	HEAP_HAS_HOLES

static	char	*lowPtr = NULL;
static	char	*highPtr = NULL;

static	CRITICALSECTION   PTR_CS;

LOCAL	void	add_ptr(char *p, unsigned len)
{
	ENTERCRITICALSECTION(PTR_CS);
	if (!lowPtr  ||  p < lowPtr)
		lowPtr = p;
	p += len;
	if (p > highPtr)
		highPtr = p;
	LEAVECRITICALSECTION(PTR_CS);
}

#ifdef	ALIGN4
#define chk_ptr(p, len)	(!((INT_PTR)(p) & (3)) &&		\
			 (char *)(p) >= lowPtr  &&		\
			 (char *)(p) <= highPtr - (len))
#else
#define	chk_ptr(p, len)	(!((INT_PTR)(p) & 1)  &&  (char *)(p) >= lowPtr  &&  (char *)(p) <= highPtr - (len))
#endif

#else  /*  not SEGMENTED_MEMORY and not HEAP_HAS_HOLES  */


typedef	struct  _mmb  {		/*  keep list of memory blocks allocated */
	char	*memBlock;
	long	size;
	struct _mmb *next;
}	MMB_t;

static	MMB_t	*MMB = NULL;		/*  Master memory block list		*/

static	CRITICALSECTION   MMB_CS;

LOCAL	void	*real_calloc(size_t n, size_t s);

LOCAL	void	add_ptr(char *p, unsigned len)
{
	MMB_t	*mb;

	mb = (MMB_t *) real_calloc(1, sizeof(MMB_t));
	ENTERCRITICALSECTION(MMB_CS);
	mb->memBlock = p;
	mb->size = len;
	mb->next = MMB;
	MMB = mb;
	LEAVECRITICALSECTION(MMB_CS);
}

#ifdef	BOEHM_GC
extern	void (*GC_is_valid_displacement_print_proc)();
static	int	GC_bad_ptr;
static	void	GC_set_bad_ptr(void *p)
{
	GC_bad_ptr = 1;
}
#endif

LOCAL	 _INLINE_  int	chk_ptr(char *p, unsigned len)
{
#ifdef	BOEHM_GC
	static	int	once = 1;
	if (once) {
		GC_is_valid_displacement_print_proc = GC_set_bad_ptr;
		once = 0;
	}
	(void) GC_is_valid_displacement(p);
	return GC_bad_ptr;
#else
	MMB_t	*mb;
	int	r=0;

	/*  chk_ptr is called so much that even a critical section call becomes prohibitively expensive.
	    As an alternative I am making sure that the writes/updates are atomic but that they are done
	    such that a critical section in the reads should not be necessary.  */
/*	ENTERCRITICALSECTION(MMB_CS);    */
	for (mb = MMB ; mb  ; mb = mb->next)
		if (p >= mb->memBlock  &&  p < mb->memBlock + mb->size) {
			r = 1;
			break;
		}
/*	LEAVECRITICALSECTION(MMB_CS);  */
	return r;
#endif
}

int	_gc_chk_ptr(char *p, unsigned len)  /*  used by the GC to determine if a RANGE of memory OVERLAPS an allocated region  */
{
	MMB_t	*mb;
	int	r=0;
	char	*e = p + len;

	for (mb = MMB ; mb  ; mb = mb->next)
		if (!(p < mb->memBlock  &&  e < mb->memBlock  ||
		      p > mb->memBlock + mb->size  &&  e > mb->memBlock + mb->size)) {
			r = 1;
			break;
		}
	return r;
}

void	*_Dynace_getpage(long len)
{
	char	*p = real_calloc(1, (unsigned) len);
	add_ptr(p, (unsigned) len);
	return (void *) p;
}


#endif

#endif  /*  SEGMENTED_MEMORY  */

LOCAL	GMR	*new_gmr(void)
{
	GMR	*r;
	int	i;

	if (!FGMR)  {
		FGMR = Tncalloc(GMR, GMR_BLOCK_SIZE);
		for (i=0 ; i != (GMR_BLOCK_SIZE-1) ; ++i)
			FGMR[i].next = FGMR + (i + 1);
	}
	r = FGMR;
	FGMR = FGMR->next;
	return r;
}

LOCAL	void	free_gmr(GMR *gmr)
{
	gmr->next = FGMR;
	FGMR = gmr;
}

#if defined(__sparc__)  &&  !defined(__svr4__)	/* memmove is not available on SunOS */
void	*memmove(void *vto, const void *vfrom, int n)
{
	char	*to = (char *) vto;
	char	*from = (char *) vfrom;
	
	if (to < from)
		while (n--)
			*to++ = *from++;
	else if (to > from)  {
		to   += n;
		from += n;
		while (n--)
			*--to = *--from;
	}
	return vto;
}
#endif

#ifdef	USE_SIGNAL

static	int	sigsegv_error;
static	jmp_buf	sigsegv_jb;
static	CRITICALSECTION  SSE_CS;

LOCAL	void	handle_sigsegv(int sig)
{
	sigsegv_error = 1;
	longjmp(sigsegv_jb, 1);
}

#endif

#define	chk_sptr(p, len)  ((char _HUGE_ *)(p) > (char _HUGE_ *) &obj  &&  (char _HUGE_ *)(p) <= (char _HUGE_ *)StackBeg - (len))


LOCAL	_INLINE_	int	_IsObj(object obj)
{
	object	cls;
	Behavior_iv_t	*cv;
	int	stack=0, ret=0;
#ifdef	USE_SIGNAL
	void	(*ps)(int);
	ENTERCRITICALSECTION(SSE_CS);
	ps = signal(SIGSEGV, handle_sigsegv);
	if (setjmp(sigsegv_jb))
		goto done;
	sigsegv_error = 0;
#endif

	if (!chk_ptr((char *) obj, sizeof(Object_iv_t))  &&
	    !(stack=chk_sptr(obj, sizeof(Object_iv_t))))
		goto done;
	if (obj->tag & (~ALL_MASK))
		goto done;
	if ((obj->tag & OBJ_MASK) != OBJ_USED  &&  (obj->tag & OBJ_MASK) != OBJ_MARKED)
		goto done;
	if (stack) {
		if ((obj->tag & ALLOC_MASK) != ALLOC_STACK)
			goto done;
	} else
		if ((obj->tag & ALLOC_MASK) != ALLOC_HEAP)
			goto done;
	if ((obj->tag & OBJ_MASK) == OBJ_USED  &&  obj->siz)
		goto done;
	cls = ClassOf(obj);
	if (!chk_ptr((char *) cls, (sizeof(Behavior_iv_t)+sizeof(Object_iv_t))))
		goto done;
	if ((cls->tag & OBJ_MASK) != OBJ_USED  &&  (cls->tag & OBJ_MASK) != OBJ_MARKED  ||
		(cls->tag & ALLOC_MASK) != ALLOC_HEAP)
		goto done;
	cv = SCIV(cls);
	ret = cv->sig1 == SIG1  &&  cv->sig2 == SIG2;
done:
#ifdef	USE_SIGNAL
	signal(SIGSEGV, ps);
	if (sigsegv_error)
		ret = 0;
	LEAVECRITICALSECTION(SSE_CS);
#endif
	return ret;
}

int	IsObj(object obj)
{
	return _IsObj(obj);
}

LOCAL	int	IsClass(object cls)
{
	Behavior_iv_t	*cv;

	if (!chk_ptr((char *) cls, (sizeof(Behavior_iv_t)+sizeof(Object_iv_t))))
		return 0;
	if ((cls->tag & OBJ_MASK) != OBJ_USED  &&  (cls->tag & OBJ_MASK) != OBJ_MARKED  ||
		(cls->tag & ALLOC_MASK) != ALLOC_HEAP)
		return 0;
	cv = SCIV(cls);
	return cv->sig1 == SIG1  &&  cv->sig2 == SIG2;
}

#ifndef	BOEHM_GC

LOCAL	void	extend_storage(Behavior_iv_t *cv)
{
	instance_block	*ib;
	int	i, is, ts;
	free_list	*fl;

	is = cv->effective_iv_size;
	ts = is * cv->nipib + sizeof(instance_block);
	ib = (instance_block *) Tncalloc(char, ts);
#if	defined(SEGMENTED_MEMORY)  ||  !defined(HEAP_HAS_HOLES)
	add_ptr((char *) ib, ts);
#endif
	ib->next = cv->ib;
	cv->ib = ib;
	cv->nib++;
	cv->nai += cv->nipib;
	fl = (free_list *) (ib + 1);
	for (i=0 ; i != cv->nipib ; ++i)  {
		((object) fl)->tag = (OBJ_FREE | ALLOC_HEAP);
		((object) fl)->siz = 0;
		fl->next = cv->fl;
		cv->fl = fl;
		fl = (free_list *) ((char *) fl + is);
	}
}

#endif

imeth	objrtn	Behavior_im_gNew(object self)
{
	object	instance;
	Behavior_iv_t	*cv = SCIV(self);

	ENTERCRITICALSECTION(cv->cs);
#ifdef	BOEHM_GC
	instance = (object) Tncalloc(char, cv->effective_iv_size);
	GC_register_finalizer_ignore_self((GC_PTR)instance, (GC_finalization_proc)dispose_boehm_gc, 0, 0, 0);
#if	!defined(SEGMENTED_MEMORY)  &&  !defined(HEAP_HAS_HOLES)
	add_ptr((char *) instance, cv->effective_iv_size);
#endif
#else
	if (!cv->fl  &&  GBA > 0L  &&  CMU > MBU + GBA)
		gGC(Dynace_c);
	if (!cv->fl)
		extend_storage(cv);
	instance = (object) cv->fl;
	cv->fl = cv->fl->next;
	cv->nai--;
#endif
	cv->ni++;
/*
	instance = (object) Tncalloc(char, cv->effective_iv_size);
*/
	ENTERCRITICALSECTION(CMU_CS);
	CMU += cv->effective_iv_size;
	MSU = CMU > MSU ? CMU : MSU;
	set_class(instance, self);
	instance->tag = (OBJ_USED | ALLOC_HEAP);
	instance->siz = 0;
	if (!++ObjectSN)
		++ObjectSN;
	instance->sn = ObjectSN;
	LEAVECRITICALSECTION(CMU_CS);
	memset(instance+1, 0, cv->effective_iv_size-EVEN(sizeof(Object_iv_t)));
	LEAVECRITICALSECTION(cv->cs);
	return instance;
}

imeth	objrtn	Behavior_im_gStackAlloc(object self, void *p)
{
	object	instance = (object) p;
	Behavior_iv_t	*cv = SCIV(self);

	if (!instance)
		gError(Dynace_c, "\nDynace: stack allocation failed.\n");
	set_class(instance, self);
	instance->tag = (OBJ_USED | ALLOC_STACK);
	instance->siz = 0;
	if (!++ObjectSN)
		++ObjectSN;
	instance->sn = ObjectSN;
	memset(instance+1, 0, cv->effective_iv_size-EVEN(sizeof(Object_iv_t)));
	return instance;
}

LOCAL	void	mk_subclass_link(object sc, object c)
{
	Behavior_iv_t	*sciv;
	object_list	*t;

	sciv = SCIV(sc);
	t = Tcalloc(object_list);
	t->obj = c;
	ENTERCRITICALSECTION(sciv->cs);
	t->next = sciv->direct_subclasses;
	sciv->direct_subclasses = t;
	LEAVECRITICALSECTION(sciv->cs);
}

LOCAL	void	set_superclass(object obj, object sc)
{
	Behavior_iv_t	*cv;

	if (sc)  {
		cv = SCIV(obj);
		cv->direct_superclasses = Tcalloc(object);
		cv->direct_superclasses[0] = sc;
		cv->n_direct_superclasses = 1;
		mk_subclass_link(sc, obj);
		cv->all_superclasses = mk_sciv_list(NULL, sc);
		cv->direct_iv_offset  = calc_offsets(cv->all_superclasses);
	}
}

LOCAL	char	*strsave(char *x)
{
	char	*p = Tncalloc(char, (x ? (int)strlen(x) : 0) + 1);
	if (!p)
		gError(Dynace_c, "Out of memory.\n");
	strcpy(p, x ? x : "");
	return(p);
}

LOCAL	void	new_class(object c, Behavior_iv_t *cv, char *name)
{
	cv->name = strsave(name);
	ENTERCRITICALSECTION(MCL_CS);
	cv->next = MCL;
	MCL = c;
	cv->id = CID++;
	cv->cache_idx = cv->id % Ccols;
	if (!SIG1)  {
#ifdef	__COSMIC__
		SIG1 = 123456789L;
		_SIG1 = 12345678L;
#else
		SIG1 = (long) time(NULL);
		_SIG1 = SIG1 - 1000;
#endif
		SIG2 = ~SIG1;
		_SIG2 = ~_SIG1;
	}
	cv->sig1 = SIG1;
	cv->sig2 = SIG2;
	INITIALIZECRITICALSECTION(cv->cs);
	LEAVECRITICALSECTION(MCL_CS);
}

 /*  defClass:  used to create kernel classes  */

LOCAL	objrtn	defClass(object cls, char *name, int effective_iv_size, int direct_iv_size, int nipib)
{
	object	nc;
	Behavior_iv_t	*cv;

	nc = Behavior_im_gNew(cls);
	cv = SCIV(nc);
	new_class(nc, cv, name);
	cv->effective_iv_size = EVEN(effective_iv_size);
	cv->direct_iv_size    = EVEN(direct_iv_size);
	cv->nipib = nipib;
	return nc;
}

cmeth	objrtn	Class_cm_gGetAll(object self)
{
	object	c;
	Behavior_iv_t	*cv;
	object clslist = gNew(LinkObject);

	USE(self);
	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c = cv->next)  {
		cv = SCIV(c);
		gAddLast(clslist, c);
	}
	LEAVECRITICALSECTION(MCL_CS);
	return clslist;
}

cmeth	objrtn	Class_cm_gFindClass(object self, char *name)
{
	object	c;
	Behavior_iv_t	*cv;

	USE(self);
	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c = cv->next)  {
		cv = SCIV(c);
		if (!strcmp(cv->name, name))
			break;
	}
	LEAVECRITICALSECTION(MCL_CS);
	if (!c && JavaCallbackClassSurrogate)
		c = gLoadJavaClass(JavaCallbackClassSurrogate, name);
	return c;
}

#if 1

#define direct_ivs(i) ((char *) i + SCIV(ClassOf(i))->direct_iv_offset)

#else

LOCAL	void	*direct_ivs(object instance)
{
	object	cls = ClassOf(instance);
	Behavior_iv_t	*cv = SCIV(cls);
	return (char *) instance + cv->direct_iv_offset;
}

#endif

cmeth	objrtn	GenericFunction_cm_gGetAll(object self) 
{
	object	c;
	GenericFunction_iv_t	*cv;
	object clslist = gNew(LinkObject);

	USE(self);
	ENTERCRITICALSECTION(MGL_CS);
	for (c=MGL ; c ; c = cv->next)  {
		cv = (GenericFunction_iv_t *) (c+1);
		gAddLast(clslist, c);
	}
	LEAVECRITICALSECTION(MGL_CS);
	return clslist;
}

cmeth	objrtn	GenericFunction_cm_gFindGeneric(object self, char *name) 
{
	object	g;
	GenericFunction_iv_t	*cv;

	USE(self);
	ENTERCRITICALSECTION(MGL_CS);
	for (g=MGL ; g ; g = cv->next)  {
		cv = (GenericFunction_iv_t *) (g+1);
		if (!strcmp(cv->name, name))
			break;
	}
	LEAVECRITICALSECTION(MGL_CS);
	return g;
}

cmeth objrtn	GenericFunction_cm_gNewGeneric(object self, char *name, void *fp)
{
	object	generic;
	GenericFunction_iv_t	*iv;

	generic = Behavior_im_gNew(self);
	iv = (GenericFunction_iv_t *) direct_ivs(generic);
	iv->name = strsave(name);
	ENTERCRITICALSECTION(MGL_CS);
	iv->id = GID++;
	iv->cache_idx = Ccols * (iv->id % Crows);
	iv->ptrGeneric = fp;
	iv->next = MGL;
	MGL = generic;
	LEAVECRITICALSECTION(MGL_CS);
#if	DPP_FASTWIDE
	iv->mc = Tncalloc(ofun, NUM_CLASSES);
#endif
	return generic;
}

cmeth objrtn
Method_cm_gNewMethod(object	self,
		     char	*name,
		     object	cls,
		     object	generic,
		     ofun	methf,
		     ofun	methf2)
{
	object		meth;
	Method_iv_t	*miv;
/*	GenericFunction_iv_t *giv;   */
	Behavior_iv_t	*civ;
	object_list	*ml;

	ChkArg(cls, 3);
	ChkArg(generic, 4);
	if (!IsClass(cls))
		gError(self, "NewMethod::Method arg 3 is not a class.\n");
	
	meth = Behavior_im_gNew(self);
	miv = (Method_iv_t *) direct_ivs(meth);
	miv->name = strsave(name);
	miv->cls = cls;
	miv->generic = generic;
	miv->meth = methf;
	miv->fmeth = methf2;
	ENTERCRITICALSECTION(MML_CS);
	miv->next = MML;
	MML = meth;
#if 0
	giv = (GenericFunction_iv_t *) direct_ivs(generic);
	ml = Tcalloc(object_list);
	ml->obj = meth;
	ml->next = giv->methods;
	giv->methods = ml;
#endif
	civ = SCIV(cls);
	ml = Tcalloc(object_list);
	ml->obj = meth;
	ENTERCRITICALSECTION(civ->cs);
	ml->next = civ->direct_methods;
	civ->direct_methods = ml;
	LEAVECRITICALSECTION(civ->cs);
	
	LEAVECRITICALSECTION(MML_CS);
	return meth;
}


/*  method cache  */

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)

typedef	struct	_element  {
	object		cls;
	object		generic;
	object		meth;
	int		level;		/*  normal / superclass method  */
	Method_iv_t	*miv;
	struct	_element	*next;
}	element;

typedef	struct	_cache  {
	int	rows;		/*  generics	*/
	int	cols;		/*  classes	*/
	element	**table;
	int	used;
	int	chains;
}	cache;

static	cache	*method_cache;

static	CRITICALSECTION	MC_CS;

LOCAL	void	create_cache(int rows, int cols)
{
	method_cache = Tcalloc(cache);
	method_cache->table = Tncalloc(element *, rows * cols);
	method_cache->rows = rows;
	method_cache->cols = cols;
}

LOCAL	void	free_cache(void)
{
	int	i, n;
	element	*e, *ne;

	n = method_cache->rows * method_cache->cols;
	for (i=0 ; i != n ; ++i)
		for (e=method_cache->table[i] ; e ; e=ne)  {
			ne = e->next;
			free(e);
		}
	free(method_cache->table);
	free(method_cache);
}

LOCAL	void	rebuild_class_indexes(void)
{
	object	c;
	Behavior_iv_t	*cv;

	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c=cv->next)  {
		cv = SCIV(c);
		cv->cache_idx = cv->id % Ccols;
	}
	LEAVECRITICALSECTION(MCL_CS);
}

LOCAL	void	rebuild_generic_indexes(void)
{
	object	c;
	GenericFunction_iv_t *cv;

	ENTERCRITICALSECTION(MGL_CS);
	for (c=MGL ; c ; c=cv->next)  {
		cv = (GenericFunction_iv_t *) direct_ivs(c);
		cv->cache_idx = Ccols * (cv->id % Crows);
	}
	LEAVECRITICALSECTION(MGL_CS);
}

cmeth	objrtn	Dynace_cm_gResizeMethodCache(object self, int classes, int generics)
{
	ENTERCRITICALSECTION(MC_CS);
	free_cache();
	create_cache(Crows = generics, Ccols = classes);
	LEAVECRITICALSECTION(MC_CS);
	rebuild_generic_indexes();
	rebuild_class_indexes();
	return self;
}

LOCAL	void	cache_add(int row, int col, object generic, object cls, int level, object meth)
{
/*	int	pos = method_cache->cols * (row % method_cache->rows) +	col % method_cache->cols;  */
	int	pos = cache_entry(row, col);
	element	*e;

/*
    The following code is needed (instead of the Tcalloc) because in
    out-of-memory conditions the gError call will need more elements
    which won't be available because of the lack of memory.  We can
    just avoid the problem by not adding it to the cache.
*/

	e = (element *) calloc(1, sizeof(element));
	if (!e)
		return;

/*	e = Tcalloc(element);     */

	e->generic = generic;
	e->cls = cls;
	e->meth = meth;
	e->level = level;
	e->miv = (Method_iv_t *) direct_ivs(meth);
	if (e->next = method_cache->table[pos])
		method_cache->chains++;
	method_cache->table[pos] = e;
	method_cache->used++;
}

#endif /* !DPP_FASTWIDE */


cmeth	objrtn	Dynace_cm_gSetMemoryBufferArea(object self, long sz)
{
	GBA = sz;
	return self;
}

/*  Depth-first search  */

LOCAL	Method_iv_t *
find_method(object	generic,
	    object	cls,
	    int		level,
	    object	*mo)
{
	object_list	*ml;
	Method_iv_t	*miv;
	Behavior_iv_t	*civ;
	int	i;

	/*  check if this class implements the generic  */

	civ = SCIV(cls);
	if (level == 1) {
		ENTERCRITICALSECTION(civ->cs);
		for (ml=civ->direct_methods ; ml ; ml=ml->next)  {
			miv = (Method_iv_t *) direct_ivs(ml->obj);
			if (miv->generic == generic)  {
				*mo = ml->obj;
				LEAVECRITICALSECTION(civ->cs);
				return miv;
			}
		}
		LEAVECRITICALSECTION(civ->cs);
	}

	/*  check the super classes  */

	for (i=0 ; i < civ->n_direct_superclasses ; ++i)
		if (miv = find_method(generic, civ->direct_superclasses[i], 1, mo))
			return miv;
	
	return NULL;
}

#define	tname(x)	(x->name ? x->name : "unnamed")
#define T_ON(x)		x->trace == DYNACE_TRACE_ON
#define T_NOFF(x)	x->trace != DYNACE_TRACE_OFF


LOCAL	void	tracefn(object i, GenericFunction_iv_t *giv, object mo)
      	  			/*  first arg to generic		*/
                               	/*  generic instance vars		*/
      	   			/*  method object			*/
{
	int	aClass = IsaClass(i);
	Behavior_iv_t	*cv = aClass ? SCIV(i) : SCIV(ClassOf(i));
	Method_iv_t	*miv = (Method_iv_t *) direct_ivs(mo);
	Behavior_iv_t	*cv2 = SCIV(miv->cls);
	char	buf[100];
	int	none_off, any_on;
	int	traces = TRACES;

	TRACES = DYNACE_TRACE_OFF;	/*  prevent recursive calls  */
	none_off = T_NOFF(cv)  &&  T_NOFF(cv2)  &&  T_NOFF(giv)  &&  T_NOFF(miv);
	any_on = T_ON(cv)  ||  T_ON(cv2)  ||  T_ON(giv)  ||  T_ON(miv);
	if (none_off  &&  (traces == DYNACE_TRACE_ALL  ||  any_on))  {
		if (aClass)
			sprintf(buf, "Trace: %s(class %s) -> ",
			       tname(giv), tname(cv));
		else
			sprintf(buf, "Trace: %s(%s instance) -> ",
			       tname(giv), tname(cv));
		sprintf(buf+strlen(buf), "%s::%s\n", tname(miv), tname(cv2));
		gTracePrint(Dynace_c, buf);
	}
	TRACES = traces;
}


/* very similar to gFindMethod and gFindMethodObject */
/* (if this function changes change the others) */

/*  This one aborts the program if it can't find the method	*/
/*  This one also performs object checking, yielding and tracing*/
/*  Used by generics						*/

ofun	_FindMethod(object i, object generic)
      	  	/*  Instance being sent the message		*/
      	        /*  The generic being used			*/
{
	Method_iv_t	*miv;
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	element	*e;
#endif	
	Behavior_iv_t	*civ;
	GenericFunction_iv_t	*giv;
	object	mo;   	/*  method object found  */
	object	cls;	/*  The class where the search should start	*/

	if (In_GC) {
		ENTERCRITICALSECTION(GC_CS);  /*  just wait here until GC done  */
		LEAVECRITICALSECTION(GC_CS);
	}

	/*  shouldn't have to check generic because _FindMethod is used by
	    macros which set the argument  */

	if (_CheckObjects_)  {
		if (!_IsObj(i))
			gInvalidObject(generic, 1, i);
		_LastGeneric_ = generic;
	}


	YIELD;

	cls = ClassOf(i);

	/*  check cache  */

	civ = SCIV(cls);
/*	giv = (GenericFunction_iv_t *) direct_ivs(generic);   */
	giv = (GenericFunction_iv_t *) (generic+1);

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	/*  This function is called so much that even a critical section call becomes prohibitively expensive.
	    As an alternative I am making sure that the writes/updates are atomic but that they are done
	    such that a critical section in the reads should not be necessary.  */
/*	ENTERCRITICALSECTION(MC_CS);    */
	for (e=method_cache->table[cache_entry(giv->cache_idx, civ->cache_idx)] ; e ; e=e->next)
		if (e->cls == cls  &&  e->generic == generic  &&  e->level == 1)  {
			volatile  ofun	meth;

			if (TRACES > DYNACE_TRACE_OFF)
				tracefn(i, giv, e->meth);
			meth = e->miv->meth;
/*			LEAVECRITICALSECTION(MC_CS);  */
			return meth;
		}
#endif
	
	/*  find it the hard way   */

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	ENTERCRITICALSECTION(MC_CS);
#endif
	if (miv = find_method(generic, cls, 1, &mo))  {
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
		cache_add(giv->cache_idx, civ->cache_idx, generic, cls, 1, mo);
		LEAVECRITICALSECTION(MC_CS);
#endif
		if (TRACES > DYNACE_TRACE_OFF)
			tracefn(i, giv, mo);

		return miv->meth;
	}
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	else
		LEAVECRITICALSECTION(MC_CS);
#endif    
	if (!Generic(gDoesNotImplement))  /* in case kernel hasn't booted yet  */
		gDoesNotImplement(cls, generic);
	gDoesNotImplement(cls, generic);
	return NULL;
}

/* very similar to gFindMethod and gFindMethodObject */
/* (if this function changes change the others) */

/*  This one aborts the program if it can't find the method  */

ofun	_FindMethod2(object cls, object generic, int lev)
{
	Method_iv_t	*miv;
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	element	*e;
#endif
	Behavior_iv_t	*civ;
	GenericFunction_iv_t	*giv;
	object	mo;   /*  method object found  */

	/*  no argument checking done because this function should be called
	    by macros which should use correct parameters  */

	if (In_GC) {
		ENTERCRITICALSECTION(GC_CS);  /*  just wait here until GC done  */
		LEAVECRITICALSECTION(GC_CS);
	}

	/*  check cache  */

	civ = SCIV(cls);
/*	giv = (GenericFunction_iv_t *) direct_ivs(generic);   */
	giv = (GenericFunction_iv_t *) (generic+1);

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	ENTERCRITICALSECTION(MC_CS);
	for (e=method_cache->table[cache_entry(giv->cache_idx, civ->cache_idx)] ; e ; e=e->next)
		if (e->cls == cls  &&  e->generic == generic  &&  e->level == lev) {
			volatile  ofun	meth = e->miv->fmeth;
			LEAVECRITICALSECTION(MC_CS);
			return meth;
		}
#endif	
	/*  find it the hard way   */

	if (miv = find_method(generic, cls, lev, &mo))  {
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
		cache_add(giv->cache_idx, civ->cache_idx, generic, cls, lev, mo);
		LEAVECRITICALSECTION(MC_CS);
#endif
		return miv->fmeth;
	}
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	else
		LEAVECRITICALSECTION(MC_CS);
#endif    
	if (!Generic(gDoesNotImplement))  /* in case kernel hasn't booted yet  */
		gDoesNotImplement(cls, generic);
	gDoesNotImplement(cls, generic);
	return NULL;
}

/*  very similar to _FindMethod and gFindMethodObject  */
/* (if this function changes change the others) */

/*  This one returns NULL if the method is not found  */

imeth	ofun	Behavior_im_gFindMethod(object self, object generic, int lev)
      	     		/*  the class  */
{
	Method_iv_t	*miv;
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	element	*e;
#endif
	Behavior_iv_t	*civ;
	GenericFunction_iv_t	*giv;
	object	mo;   /*  method object found  */


	ChkArg(generic, 2);

	/*  check cache  */

	civ = SCIV(self);
/*	giv = (GenericFunction_iv_t *) direct_ivs(generic);   */
	giv = (GenericFunction_iv_t *) (generic+1);

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	ENTERCRITICALSECTION(MC_CS);
	for (e=method_cache->table[cache_entry(giv->cache_idx, civ->cache_idx)] ; e ; e=e->next)
		if (e->cls == self  &&  e->generic == generic  &&  e->level == lev) {
			volatile  ofun	meth = e->miv->fmeth;
			LEAVECRITICALSECTION(MC_CS);
			return meth;
		}
#endif
	/*  find it the hard way   */

	if (miv = find_method(generic, self, lev, &mo))  {
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
		cache_add(giv->cache_idx, civ->cache_idx, generic, self, lev, mo);
		LEAVECRITICALSECTION(MC_CS);
#endif
		return miv->fmeth;
	}
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	else
		LEAVECRITICALSECTION(MC_CS);
#endif    
	return NULL;
}

/*  very similar to _FindMethod and gFindMethod  */
/* (if this function changes change the others) */

/*  This one returns the method object instead of the C function pointer  */

imeth	objrtn	Behavior_im_gFindMethodObject(object self, object generic, int lev)
      	     		/*  the class  */
{
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	element	*e;
#endif
	Behavior_iv_t	*civ;
	GenericFunction_iv_t	*giv;
	object	mo;   /*  method object found  */

	ChkArgTyp(generic, 2, GenericFunction_c);

	/*  check cache  */

	civ = SCIV(self);
/*	giv = (GenericFunction_iv_t *) direct_ivs(generic);   */
	giv = (GenericFunction_iv_t *) (generic+1);

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	ENTERCRITICALSECTION(MC_CS);
	for (e=method_cache->table[cache_entry(giv->cache_idx, civ->cache_idx)] ; e ; e=e->next)
		if (e->cls == self  &&  e->generic == generic  &&  e->level == lev) {
			struct _Object_iv_t * volatile ret = e->meth;
			LEAVECRITICALSECTION(MC_CS);
			return (objrtn) ret;
		}
#endif
	
	/*  find it the hard way   */

	if (find_method(generic, self, lev, &mo))  {
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
		cache_add(giv->cache_idx, civ->cache_idx, generic, self, lev, mo);
		LEAVECRITICALSECTION(MC_CS);
#endif
		return mo;
	}
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	else
		LEAVECRITICALSECTION(MC_CS);
#endif    
	return NULL;
}

/*  The remaining presumes the kernel is up  */

LOCAL	iv_offset_def_list	*mk_sciv_list(iv_offset_def_list *v, object sc)
{
	iv_offset_def_list	*e, *t, *n;
	Behavior_iv_t	*sciv = SCIV(sc);

	/*  set e to point to the last element of v  */

	if (e = v)
		while (e->next)
			e = e->next;

	/*  copy sc's list to the end of v  */

	if (t = sciv->all_superclasses)
		for ( ; t ; t = t->next)  {
			n = Tcalloc(iv_offset_def_list);
			*n = *t;
			n->next = NULL;
			if (!e)
				e = v = n;
			else  {
				e->next = n;
				e = n;
			}
		}

	/*  create node for sc's direct IVs  */

	if (sciv->direct_iv_size)  {
		n = Tcalloc(iv_offset_def_list);
		n->superclass = sc;
		n->iv_size = sciv->direct_iv_size;
		n->next = NULL;
		if (!e)
			v = n;
		else
			e->next = n;
	}
	return(v);
}

LOCAL	void	remove_dup_sc(iv_offset_def_list *v)
{
	iv_offset_def_list	*m, *p;

	for ( ; v ; v = v->next)
		for (p=v, m=v->next ; m ; )
			if (m->superclass == v->superclass)  {
				p->next = m->next;
				free(m);
				m = p->next;
			}  else  {
				p = m;
				m = m->next;
			}
}

LOCAL	int	calc_offsets(iv_offset_def_list *v)
{
	int	off = 0;

	for ( ; v ; v = v->next)  {
		v->iv_offset = off;
		off += v->iv_size;
	}
	return off;
}

LOCAL	int	calc_nipib(int size)
{
	int	i;
	static struct {
		int	size;
		int	nipib;
	}  v[] = {
		{8, 50},
		{16, 40},
		{32, 30},
		{64, 20},
		{128, 10},
		{256, 6},
		{512, 6},
		{1024, 5},
		{2048, 5}
	};
	for (i=0 ; i < (sizeof(v)/sizeof(*v)) ; i++)
		if (size <= v[i].size)
			return v[i].nipib;
	return 1;
}

LOCAL	objrtn
NewClass3(object	self,
	  char 		*name,
	  int 		ivsize,
	  object 	mc,	        /*  metaclass	*/
	  int 		nipib,/*  number of instances per instance block or 0	*/
	  int 		n,	        /*  number of superclasses  */
	  object 	*superclasses)  /*  superclass array  */
{
	object	c;	/*  new class object			*/
	object	sc;	/*  superclass object			*/
	int	i;	/*  indexing varaible			*/
	Behavior_iv_t	*cv;	/*  new class's CVs		*/

	USE(self);
	c = Behavior_im_gNew(mc);
	cv = SCIV(c);
	new_class(c, cv, name);
	cv->direct_iv_size = EVEN(ivsize);
	cv->direct_superclasses = Tncalloc(object, n);
	cv->n_direct_superclasses = n;

	for (i=0 ; i < n ; i++)  {
		cv->direct_superclasses[i] = sc = superclasses[i];
		cv->all_superclasses = mk_sciv_list(cv->all_superclasses, sc);
		mk_subclass_link(sc, c);
	}

	remove_dup_sc(cv->all_superclasses);
	
	cv->direct_iv_offset  = calc_offsets(cv->all_superclasses);

	cv->effective_iv_size  = cv->direct_iv_size + cv->direct_iv_offset;

	cv->nipib = nipib ? nipib : calc_nipib(cv->effective_iv_size);

	return(c);
}

#if	DPP_STRATEGY == 1
LOCAL	objrtn	Class_cm_gNewStdClass(object self, char *name, int ivsize, object mc, 
			  int nipib, object superclasses, ...)
#else
LOCAL	objrtn	Class_cm_gNewStdClass(object self, char *name, int ivsize, object mc, 
			  int nipib, object superclasses, va_list _rest_)
#endif
{
	object	c;	/*  new class object			*/
	object	sc;	/*  superclass object			*/
	int	n;	/*  number of superclasses		*/
	object	scvp[MIS];/*  array of superclasses		*/
	char	buf[80];
	MAKE_REST(superclasses);

	ChkArg(mc, 4);
	if (!IsClass(mc))
		gError(self, "gNewStdClass argument 4 is not a class.\n");

	if (superclasses)
		for (n=0, sc=superclasses ; sc && n < MIS ; sc = GetArg(object), ++n)  {
			ChkArg(sc, n+6);
			if (!IsClass(sc))  {
				sprintf(buf, "gNewStdClass argument %d is not a class.\n", n+6);
				gError(self, buf);
			}
			scvp[n] = sc;
		}
	else  {
		n = 1;
		scvp[0] = Object_c;
	}
	if (n == MIS)  {
		sprintf(buf, "Attempt to create class %s with more than %d superclasses", name, MIS);
		gError(self, buf);
	}

	n = n > MIS ? MIS : n;

	c = NewClass3(self, name, ivsize, mc, nipib, n, scvp);
	
	return(c);
}

#if	DPP_STRATEGY == 1
LOCAL objrtn	Class_cm_gNewClass(object self, char *name, int ivsize, int cvsize, 
			 object superclasses, ...)
#else
LOCAL objrtn	Class_cm_gNewClass(object self, char *name, int ivsize, int cvsize, 
			 object superclasses, va_list _rest_)
#endif
{
	object	c;	/*  new class object			*/
	object	sc;	/*  superclass object			*/
	int	n;	/*  number of superclasses		*/
	object	scvp1[MIS];/*  array of superclasses		*/
	object	scvp2[MIS];/*  array of meta superclasses	*/
	object	mc;	/*  metaclass				*/
	char	meta[100];	/*  metaclass name		*/
	MAKE_REST(superclasses);
	
	if (superclasses)
		for (n=0, sc=superclasses ; sc && n < MIS ; sc = GetArg(object), ++n)  {
			ChkArg(sc, n+5);
			if (!IsClass(sc))  {
				sprintf(meta, "gNewClass(Class..) argument %d is not a class.\n", n+5);
				gError(self, meta);
			}
			scvp1[n] = sc;
			scvp2[n] = ClassOf(sc);
		}
	else  {
		scvp1[0] = Object_c;
		scvp2[0] = metaObject;
		n = 1;
	}
	if (n == MIS)  {
		sprintf(meta, "Attempt to create class %s with more than %d superclasses", name, MIS);
		gError(self, meta);
	}
	
	strcpy(meta, "meta");
	strcat(meta, name);

	n = n > MIS ? MIS : n;

	mc = NewClass3(self, meta, cvsize, MetaClass_c, 1, n, scvp2);
	c = NewClass3(self, name, ivsize, mc, 0, n, scvp1);
	
	return(c);
}

void	*GetIVptr(object obj, object cls)
{
	Behavior_iv_t		*cv;
	iv_offset_def_list	*v;

	/*  cls not validated for the sake of speed  */

	cv = SCIV(ClassOf(obj));
	if (cls == ClassOf(obj))
		return (char *) obj + cv->direct_iv_offset;
	for (v=cv->all_superclasses ; v ; v=v->next)
		if (v->superclass == cls)
			return (char *) obj + v->iv_offset;
	return NULL;
}

imeth	objrtn	Object_im_gDispose(object self)
{
	Behavior_iv_t	*cv;
#ifndef	BOEHM_GC
	free_list	*fl;
#endif
	if (self->tag & ALLOC_STACK) {
		self->tag = (OBJ_FREE | ALLOC_STACK);
		return NULL;
	}
	if (self->tag & OBJ_FREE)
		return NULL;
	self->tag = (OBJ_FREE | ALLOC_HEAP);
	cv = SCIV(ClassOf(self));
	ENTERCRITICALSECTION(cv->cs);
#ifndef	BOEHM_GC
	fl = (free_list *) self;
	fl->next = cv->fl;
	cv->fl = fl;
	cv->nai++;
/*
	set_class(self, NULL);
	free(self);
*/
#endif
	cv->ni--;
	ENTERCRITICALSECTION(CMU_CS);
	CMU -= cv->effective_iv_size;
	LEAVECRITICALSECTION(CMU_CS);
	LEAVECRITICALSECTION(cv->cs);
	return NULL;
}

/*  garbage collector code   */

/*   non-recursive marker   */

cmeth	void	Dynace_cm_gMarkObject(object self, object obj)
{
#ifdef	BOEHM_GC
	USE(self);
	USE(obj);
#else
	char	**p = NULL;
	short	sz;
	object	rtn = NULL, nxt;

	USE(self);
 recurse:
	obj->tag = OBJ_MARKED | (ALLOC_MASK & obj->tag);
	if (ClassOf(obj) == WeakPointer) {
		sz = 0;
		goto pop_recursion;
	}
	p =  (char **) (obj + 1);
	sz = (short) (SCIV(ClassOf(obj))->effective_iv_size - (EVEN(sizeof(Object_iv_t)) + sizeof(char *)));
	while (sz >= 0)  {
		nxt = *((object *) ((char *) p + sz));
		if (IsObj(nxt))  {
			if (nxt->tag & OBJ_USED)  {
				/*  mark & save sz  */
				obj->tag = OBJ_MARKED | (ALLOC_MASK & obj->tag);
				obj->siz = sz;

				/*  reverse link  */
				*((object *) ((char *) p + sz)) = rtn;
				rtn = obj;

				/*  create new environment  */
				obj = nxt;

				/*  recurse  */
				goto recurse;
			}
 pop_recursion:
			sz -= (short)sizeof(char *);
		} else
			sz -= 2;
	}

	if (!rtn)
		return;    /*  all done  */

	/*  get old sz  */
	sz = (short) rtn->siz;
	rtn->siz = 0;

	p = (char **) (rtn + 1);
	nxt = rtn;
	rtn = *((object *) ((char *) p + sz));
	*((object *) ((char *) p + sz)) = obj;

	/*  pop recursion  */
	obj = nxt;
	goto pop_recursion;
#endif	/*  !BOEHM_GC  */
}

cmeth	void	Dynace_cm_gMarkRange(object self, char _HUGE_ **from, char _HUGE_ **to)
{
#ifdef	BOEHM_GC
	USE(self);
	USE(from);
	USE(to);
#else
	object	obj;
	
	USE(self);
#ifdef	ALIGN4
	from = (char _HUGE_ **) EVEN(from);
	to   = (char _HUGE_ **) EVEN(to);
#else
	if ((INT_PTR) from & 1L)
		from = (char _HUGE_ **) ((char _HUGE_ *) from + 1);
	if ((INT_PTR) to & 1L)
		to = (char _HUGE_ **) ((char _HUGE_ *) to + 1);
#endif
	while (from < to)  {
		if (IsObj((object)*from))  {
			obj = (object) *from;
			if (obj->tag & OBJ_USED)
				Dynace_cm_gMarkObject(Dynace_c, obj);
			from = (char _HUGE_ **)((char _HUGE_ *) from + (int)sizeof(char *));
		} else
			from = (char _HUGE_ **)((char _HUGE_ *) from + 2);
	}
#endif  /*  !BOEHM_GC   */
}

#ifdef	BOEHM_GC

LOCAL	void	dispose_boehm_gc(object obj, void *p)
{
	Behavior_iv_t	*cv;

	USE(p);
	INHIBIT_THREADER;
	cv = SCIV(ClassOf(obj));
	if (obj->tag & OBJ_USED  &&  !cv->ncg)
		gGCDispose(obj);
	ENABLE_THREADER;
}

cmeth	object	Dynace_cm_gDumpObjects(object self, char *file, int type)
{
	return self;
}

cmeth	object	Dynace_cm_gDumpObjectsString(object self, int type)
{
	return NULL;
}

cmeth	object	Dynace_cm_gDumpObjectsDiff(object self, object sd1, object sd2)
{
	return NULL;
}
	
#else

LOCAL	long	count_instances(object cls)
{
	object	obj;
	Behavior_iv_t	*cv = SCIV(cls);
	instance_block	*ib;
	int	is=0, n;
	long	num = 0L;
	char	*p;

	if (ib = cv->ib)
		is =  cv->effective_iv_size;
	while (ib)  {
		p = (char *) (ib + 1);
		for (n=0 ; n++ != cv->nipib ; )  {
			obj = (object) p;
			if (obj->tag & OBJ_USED)
				num++;
			p += is;
		}
		ib = ib->next;
	}
	return num;
}

cmeth	object	Dynace_cm_gDumpObjects(object self, char *file, int type)
{
#ifndef	__COSMIC__
	object	c;
	long	num;
	FILE	*fp;

	fp = fopen(file, "w");
	if (!fp)
		return NULL;
	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c=SCIV(c)->next)
		if (type  ||  !IsaMetaClass(c)  &&  c != MetaClass  &&  c != Method  &&  c != GenericFunction)
			if (num = count_instances(c))
				fprintf(fp, "%s\t%ld\n", SCIV(c)->name, num);
	LEAVECRITICALSECTION(MCL_CS);
	fclose(fp);
#endif
	return self;
}

cmeth	object	Dynace_cm_gDumpObjectsString(object self, int type)
{
#ifndef	__COSMIC__
	object	c, sd, val;
	long	num;

	sd = gNewWithInt(StringDictionary, 201);
	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c=SCIV(c)->next)
		if (type  ||  !IsaMetaClass(c)  &&  c != MetaClass  &&  c != Method  &&  c != GenericFunction) {
			num = count_instances(c);
			if (!strcmp(SCIV(c)->name, "StringDictionary"))
				num--;
			if (num)
				gAddStr(sd, SCIV(c)->name, gNewWithLong(LongInteger, num));
		}
	LEAVECRITICALSECTION(MCL_CS);
	if (val = gFindValueStr(sd, "LongInteger"))
		gChangeLongValue(val, count_instances(LongInteger) - gSize(sd));
	if (val = gFindValueStr(sd, "StringAssociation"))
		gChangeLongValue(val, count_instances(StringAssociation) - gSize(sd));
	if (val = gFindValueStr(sd, "String"))
		gChangeLongValue(val, count_instances(String) - gSize(sd));
	return sd;
#endif
	return NULL;
}

cmeth	object	Dynace_cm_gDumpObjectsDiff(object self, object sd1, object sd2)
{
	object	diff = gNewWithInt(StringDictionary, 201);
	object	seq, sa, val2;
	char	*str;
	long	num, num2;
	int	s1 = gSize(sd1);

	for (seq=gSequence(sd1) ; sa = gNext(seq) ; ) {
		str = gStringKey(sa);
		num = gLongValue(gValue(sa));
		val2 = gFindValueStr(sd2, str);
		if (!val2)
			gAddStr(diff, str, gNewWithLong(LongInteger, -num));
		else {
			num2 = gLongValue(val2);
			if (!strcmp(str, "StringDictionary"))
				num2--;
			if (!strcmp(str, "StringAssociation"))
				num2 -= s1;
			if (!strcmp(str, "LongInteger"))
				num2 -= s1;
			if (!strcmp(str, "String"))
				num2 -= s1;
			if (num != num2)
				gAddStr(diff, str, gNewWithLong(LongInteger, num2-num));
		}
	}
	for (seq=gSequence(sd2) ; sa = gNext(seq) ; ) {
		str = gStringKey(sa);
		if (!gFindStr(sd1, str)) {
			num = gLongValue(gValue(sa));
			if (!strcmp(str, "StringDictionary"))
				num--;
			if (!strcmp(str, "StringAssociation"))
				num -= s1;
			if (!strcmp(str, "LongInteger"))
				num -= s1;
			if (!strcmp(str, "String"))
				num -= s1;
			if (num)
				gAddStr(diff, str, gNewWithLong(LongInteger, num));
		}
	}
	return diff;
}

cmeth	object	Dynace_cm_gMarkMemoryBeginning(object self)
{
	String;
	StringAssociation;
	Set;
	StringDictionary;
	File;
	LongInteger;
	return gDumpObjectsString(Dynace, 0);
}

cmeth	object	Dynace_cm_gDumpMemoryDiff(object self, object d1, char *fname)
{
	object	d2 = gDumpObjectsString(Dynace, 0);
	object	seq, sa, f;
	int	n = 0;
	char	buf[100];

	object diff = gDumpObjectsDiff(Dynace, d1, d2);
	f = gOpenFile(File, fname, "wt");
	seq = gSequence(diff);
	while (sa = gNext(seq)) {
		object k = gKey(sa);
		object v = gValue(sa);
		sprintf(buf, "%s %ld\n", gStringValue(k), gLongValue(v));
		gWrite(f, buf, strlen(buf));
		n++;
	}
	if (!n) {
		strcpy(buf, "No objects found.\n");
		gWrite(f, buf, strlen(buf));
	}
	gDispose(f);
	gDeepDispose(d2);
	gDeepDispose(diff);
	return self;
}

LOCAL	void	rebuild_free_list(Behavior_iv_t *cv, int is)
{
	instance_block	*ib;
	int	i;
	free_list	*fl;

	cv->fl = NULL;
	for (ib=cv->ib ; ib ; ib=ib->next)  {
		fl = (free_list *) (ib + 1);
		for (i=0 ; i++ != cv->nipib ; )  {
			if (((object) fl)->tag & OBJ_FREE)  {
				fl->next = cv->fl;
				cv->fl = fl;
			}
			fl = (free_list *) ((char *) fl + is);
		}
	}
}

LOCAL	void	gc_sweep(void)
{
	object	c, obj;
	Behavior_iv_t	*cv;
	instance_block	*ib;
	instance_block	*pib=NULL;	/*  previous instance block  */
	instance_block	*nib;		/*  next instance block  */
	int	is=0, n;
	int	f;	/*  the number of free objects in a block  */
	int	rfl;	/*  rebuild free list			*/
	char	*p;
	gGCDispose_t	dfun=NULL;	/*  dispose function		*/

	ENTERCRITICALSECTION(MCL_CS);
	MetaClass_c->tag = (OBJ_USED | ALLOC_HEAP);  /*  MetaClass not in any storage bin */
	for (c=MCL ; c ; c=cv->next)  {
		cv = SCIV(c);
		rfl = 0;
		if (ib = cv->ib)  {
			dfun = imcPointer(c, gGCDispose);
			is =  cv->effective_iv_size;
			pib = NULL;
		}
		while (ib)  {
			p = (char *) (ib + 1);
			for (f=n=0 ; n++ != cv->nipib ; )  {
				obj = (object) p;
				if (obj->tag & OBJ_USED)  {
					if (!cv->ncg)  {
						(*(object(*)(object))dfun)(obj);
						++f;
					}
				}  else if (obj->tag & OBJ_MARKED) {
					obj->tag = OBJ_USED | (ALLOC_MASK & obj->tag);
					obj->siz = 0;
				}  else  /*  already free  */
					++f;
				p += is;
			}
			if (f == cv->nipib)  {  /*  all objs freed  */
				if (pib)
					pib->next = nib = ib->next;
				else
					cv->ib = nib = ib->next;
				free(ib);
				ib = nib;
				cv->nib--;
				cv->nai -= cv->nipib;
				rfl = 1;
			}  else  {
				pib = ib;
				ib = ib->next;
			}
		}
		if (rfl)
			rebuild_free_list(cv, is);
	}
	LEAVECRITICALSECTION(MCL_CS);
}

LOCAL	void	mark_non_collecting_classes(void)
{
	object	c, obj;
	Behavior_iv_t	*cv;
	instance_block	*ib;
	int	is=0, n;
	char	*p;

	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c=cv->next)  {
		cv = SCIV(c);
		if (!cv->ncg)
			continue;
		is = cv->effective_iv_size;
		ib = cv->ib;
		while (ib)  {
			p = (char *) (ib + 1);
			for (n=0 ; n++ != cv->nipib ; )  {
				obj = (object) p;
				if (obj->tag & OBJ_USED)
					Dynace_cm_gMarkObject(Dynace_c, obj);
				p += is;
			}
			ib = ib->next;
		}
	}
	LEAVECRITICALSECTION(MCL_CS);
}

#endif   /*  !BOEHM_GC  */


LOCAL	void	get_mem_stats(void)
{
	object	c;
	Behavior_iv_t	*cv;
	int	is;
	long	m = 0L;

	ENTERCRITICALSECTION(MCL_CS);
	for (c=MCL ; c ; c=cv->next)  {
		cv = SCIV(c);
		is = cv->effective_iv_size;
		m += cv->ni * is;
	}
	LEAVECRITICALSECTION(MCL_CS);
	if (m > MBU)
		MBU = m;
	CMU = m;
}


#if	(defined(MSC32) && !defined(_M_X64))  ||  defined(BC32)
#define MARK_REG(r)				\
	__asm	{ mov	c, r  }			\
	if (IsObj(c)  &&  c->tag & OBJ_USED)	\
		Dynace_cm_gMarkObject(Dynace_c, c)
#endif


#if defined(__GNUC__)
#if defined(__i386__)
#define MARK_REG(r)					\
	__asm__	("movl %%" #r ",%0" : "=g" (c));	\
	if (IsObj(c)  &&  c->tag & OBJ_USED)		\
		Dynace_cm_gMarkObject(Dynace_c, c)
#elif defined(__amd64__)
#define MARK_REG(r)					\
	__asm__	("mov %%" #r ",%0" : "=g" (c));	\
	if (IsObj(c)  &&  c->tag & OBJ_USED)		\
		Dynace_cm_gMarkObject(Dynace_c, c)
#endif
#endif


cmeth	objrtn	Dynace_cm_gGC(object self)
{
#ifdef	BOEHM_GC
	ENTERCRITICALSECTION(GC_CS);
	In_GC++;
	INHIBIT_THREADER;
	GC_gcollect();
#else
	static GMR	*p;
	object	c;
	Behavior_iv_t	*cv;

	In_GC++;
	ENTERCRITICALSECTION(GC_CS);
	INHIBIT_THREADER;
#ifdef sparc
	asm("t 3");	/* flush out registers onto the stack */
#endif

#if	0  &&  defined(WIN32)  &&  !defined(BOEHM_GC)
	_register_global_memory();
#endif

	Dynace_cm_gMarkRange(Dynace_c, (char _HUGE_ **) &self, (char _HUGE_ **) StackBeg);
	Dynace_cm_gMarkObject(Dynace_c, MCL);
	Dynace_cm_gMarkObject(Dynace_c, MGL);
	Dynace_cm_gMarkObject(Dynace_c, MML);
	mark_non_collecting_classes();
#if (defined(MSC32) && !defined(_M_X64))  ||  defined(BC32)  ||  (defined(__GNUC__) && defined(__i386__))
	MARK_REG(eax);
	MARK_REG(ebx);
	MARK_REG(ecx);
	MARK_REG(edx);
	MARK_REG(esi);
	MARK_REG(edi);
	MARK_REG(ebp);
#elif defined(__GNUC__)  &&  defined(__amd64__)
	MARK_REG(rax);
	MARK_REG(rbx);
	MARK_REG(rcx);
	MARK_REG(rdx);
	MARK_REG(rbp);
	MARK_REG(rsp);
	MARK_REG(rsi);
	MARK_REG(rdi);
	MARK_REG(r8);
	MARK_REG(r9);
	MARK_REG(r10);
	MARK_REG(r11);
	MARK_REG(r12);
	MARK_REG(r13);
	MARK_REG(r14);
	MARK_REG(r15);
#endif

	for (p=LGMR ; p ; p=p->next)
		Dynace_cm_gMarkRange(Dynace_c, (char _HUGE_ **) p->beg,
				     (char _HUGE_ **)((char *) p->beg + p->size));

	for (c=MCL ; c ; c = cv->next)  {
		cv = SCIV(c);
		if (cv->markfun)
			(*(void(*)(object))cv->markfun)(c);
	}
	gc_sweep();
#endif  /*  !BOEHM_GC  */
	get_mem_stats();
	NGC++;
	ENABLE_THREADER;
	In_GC--;
	LEAVECRITICALSECTION(GC_CS);
	return self;
}

/*  misc kernel methods  */

cmeth	void	*Dynace_cm_gRegisterMemory(object self, void *beg, long size)
{
	GMR	*p;

	USE(self);
	ENTERCRITICALSECTION(GMR_CS);
	p = new_gmr();
	p->prev = NULL;
	if (p->next = LGMR)
		LGMR->prev = p;
	LGMR = p;
	p->beg = beg;
	p->size = size;
	LEAVECRITICALSECTION(GMR_CS);
	return (void *) p;
}

#if 0
//cmeth	void	Dynace_cm_gRemoveAllRegisteredMemory(object self)
{
	GMR	*next;

	USE(self);
	ENTERCRITICALSECTION(GMR_CS);
	while (LGMR) {
		next = LGMR->next;
		LGMR->next = FGMR;
		FGMR = LGMR;
		LGMR = next;
	}
	LEAVECRITICALSECTION(GMR_CS);
}
#endif

cmeth	void	Dynace_cm_gRemoveRegisteredMemory(object self, void *pp)
{
	GMR	*p = (GMR *) pp;
	USE(self);
	ENTERCRITICALSECTION(GMR_CS);
	if (p->next)
		p->next->prev = p->prev;
	if (p->prev)
		p->prev->next = p->next;
	else
		LGMR = p->next;
	free_gmr(p);
	LEAVECRITICALSECTION(GMR_CS);
}

cmeth	void	*Dynace_cm_gChangeRegisteredMemory(object self, void *pp, void *beg, long size)
{
	GMR	*p = (GMR *) pp;
	USE(self);
	p->beg = beg;
	p->size = size;
	return (void *) p;
}

cmeth	long	Dynace_cm_gMaxAfterGC(object self)
{
	USE(self);
	return MBU ? MBU : CMU;
}

cmeth	long	Dynace_cm_gMaxMemUsed(object self)
{
	USE(self);
	return MSU;
}

cmeth	long	Dynace_cm_gCurMemUsed(object self)
{
	USE(self);
	return CMU;
}

cmeth	long	Dynace_cm_gNumbGC(object self)
{
	USE(self);
	return NGC;
}

/* The following Exception code courtesy of Kaelin Colclasure
   Modified by Blake McBride  */

	
static object _dynace_signal(object);

object (*__dynace_signal)(object) = _dynace_signal;

object	_catchKind(object *condp, object cls)
{
	object result = NULL;

	if (gIsKindOf(*condp, cls)) {
		result = *condp;
		*condp = NULL;
	}
	return result;
}

object	_dynace_signal(object cond)
{
	char	*cp = (char *)&cond;
	HandlerCtx *ctxp;
	object	result;

keep_looking:
	cp += ALIGNMENT;
	while (((long *) cp)[0] != _SIG1  ||  ((long *) cp)[1] != _SIG2)
		cp += ALIGNMENT;
	ctxp = (HandlerCtx *)cp;
	if (ctxp->value != Condition)
		goto keep_looking;
    
	if (gIsKindOf(cond, Error)) {
		ctxp->value = cond;
		longjmp(ctxp->unwind, 1); /* Never returns! */
	}
	if (ctxp->rstrt == callNextRestart)
		goto keep_looking;
	if (ctxp->rstrt != NULL)
		result = (*ctxp->rstrt)(cond);
	else
		result = gDefaultRestart(cond);
	return result;
}

imeth	int	Object_im_gEqual(object self, object obj2)
{
	register	char	*p1, *p2;
	register	int	sz;

	if (EQ(self, obj2))
		return 1;
	ChkArg(obj2, 2);
	if (NEQ(ClassOf(self), ClassOf(obj2)))
		return 0;
	sz = SCIV(ClassOf(self))->effective_iv_size - EVEN(sizeof(Object_iv_t));
	p1 = (char *) (self + 1);
	p2 = (char *) (obj2 + 1);
	while (sz--)
		if (*p1++ != *p2++)
			return 0;
	return 1;
}

imeth	objrtn	Object_im_gCopy(object self)
{
	object	obj2;
	register	int	sz;

	obj2 = Behavior_im_gNew(ClassOf(self));
	sz = SCIV(ClassOf(self))->effective_iv_size;
	memcpy(obj2 + 1, self + 1, sz-EVEN(sizeof(Object_iv_t)));
	return obj2;
}

imeth	int	Object_im_gSize(object self)
{
	return SCIV(ClassOf(self))->effective_iv_size -	EVEN(sizeof(Object_iv_t));
}

imeth	int	Object_im_gBasicSize(object self)
{
	return SCIV(ClassOf(self))->effective_iv_size;
}

imeth	int	Behavior_im_gInstanceSize(object self)
{
	return SCIV(self)->effective_iv_size;
}

cmeth	int	Dynace_cm_gTrace(object self, int mode)
{
	int	pmode = TRACES;
	USE(self);
	TRACES = mode;
	return pmode;
}

LOCAL	void	init_critical_sections(void)
{
#if defined(NATIVE_THREADS) &&  defined(__unix__)
	pthread_mutexattr_init(&_mutex_attr);
	pthread_mutexattr_settype(&_mutex_attr, PTHREAD_MUTEX_RECURSIVE);
#endif
	INITIALIZECRITICALSECTION(_CI_CS_);
	INITIALIZECRITICALSECTION(MCL_CS);
	INITIALIZECRITICALSECTION(MGL_CS);
	INITIALIZECRITICALSECTION(MML_CS);
	INITIALIZECRITICALSECTION(GMR_CS);
	INITIALIZECRITICALSECTION(GC_CS);
	INITIALIZECRITICALSECTION(CMU_CS);
#ifdef	SEGMENTED_MEMORY
	INITIALIZECRITICALSECTION(MSL_CS);
#else
#ifndef	HEAP_HAS_HOLES
	INITIALIZECRITICALSECTION(PTR_CS);
#else
	INITIALIZECRITICALSECTION(MMB_CS);
#endif
#endif
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	INITIALIZECRITICALSECTION(MC_CS);
#endif
#ifdef	USE_SIGNAL
	INITIALIZECRITICALSECTION(SSE_CS);
#endif
}

void	InitKernel(void *sb, int nc)  /*  stack beginning, # of classes  */
{
	Behavior_iv_t	*cv;
	int	s;
	static	int	once = 0;

	if (once++)
		return;
#if	defined(WIN32)  &&  !defined(BOEHM_GC)
	_is_heap_base(NULL);
#endif

	init_critical_sections();

	StackBeg = (char *) sb;

	NUM_CLASSES = nc;

	/*  Build MetaClass by hand  */

	s = EVEN(sizeof(Object_iv_t)) + EVEN(sizeof(Behavior_iv_t));
	MetaClass_c = (object) Tncalloc(char, s);
#if	defined(SEGMENTED_MEMORY)  ||  !defined(HEAP_HAS_HOLES)
	add_ptr((char *) MetaClass_c, s);
#endif
	MetaClass_c->tag = (OBJ_USED | ALLOC_HEAP);
	if (!++ObjectSN)
		++ObjectSN;
	MetaClass_c->sn  = ObjectSN;
	cv = SCIV(MetaClass_c);
	new_class(MetaClass_c, cv, "MetaClass");
	cv->direct_iv_size = 0;
	cv->effective_iv_size = s;
	cv->nipib = 6;
	

#define MCLASS(c)		c = defClass(MetaClass_c, #c, s, 0, 1)
#define PCLASS(c, esz, dsz, n)	c##_c = defClass(meta##c, #c, esz, dsz, n)
#define BCLASS(c, dsz, n)	c##_c = defClass(meta##c, #c, EVEN(sizeof(Object_iv_t))+sizeof(dsz), sizeof(dsz), n)



	MCLASS(metaObject);
	PCLASS(Object, sizeof(Object_iv_t), sizeof(Object_iv_t), 1);
	MCLASS(metaBehavior);
	PCLASS(Behavior, s, sizeof(Behavior_iv_t), 1);
	MCLASS(metaClass);
	PCLASS(Class, s, 0, 1);
	MCLASS(metaMetaClass);

	MCLASS(metaMethod);
	MCLASS(metaGenericFunction);

	BCLASS(Method, Method_iv_t, 30);
	BCLASS(GenericFunction, GenericFunction_iv_t, 15);

	set_class(MetaClass_c, metaMetaClass);

	set_superclass(Object_c, NULL);
	set_superclass(Behavior_c, Object_c);
	set_superclass(Class_c, Behavior_c);
	set_superclass(MetaClass_c, Behavior_c);
	set_superclass(metaObject, Class_c);
	set_superclass(metaBehavior, metaObject);
	set_superclass(metaClass, metaBehavior);
	set_superclass(metaMetaClass, metaBehavior);
	set_superclass(Method_c, Object_c);
	set_superclass(metaMethod, metaObject);
	set_superclass(GenericFunction_c, Object_c);
	set_superclass(metaGenericFunction, metaObject);

#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	ENTERCRITICALSECTION(MC_CS);
	create_cache(Crows, Ccols);    /*  generics, classes  */
	LEAVECRITICALSECTION(MC_CS);
#endif

	Generic(gNew) = GenericFunction_cm_gNewGeneric(GenericFunction_c, "gNew", gNew);
	Generic(gNewClass) = GenericFunction_cm_gNewGeneric(GenericFunction_c, "gNewClass", gNewClass);
	Generic(gNewMethod) = GenericFunction_cm_gNewGeneric(GenericFunction_c, "gNewMethod", gNewMethod);
	Generic(gNewGeneric) = GenericFunction_cm_gNewGeneric(GenericFunction_c, "gNewGeneric", gNewGeneric);

	Method_cm_gNewMethod(Method_c, "gNewMethod", metaMethod, Generic(gNewMethod), (ofun) Method_cm_gNewMethod, (ofun) Method_cm_gNewMethod);

	/*  The next line is very critical because it is the first use of
	    the dynamic dispatching.  If using the jumpto assembler start
	    tracing the assembler stuff from here.  */
	gNewMethod(Method_c, "gNewClass", metaClass, Generic(gNewClass), (ofun) Class_cm_gNewClass, (ofun) Class_cm_gNewClass);

	iMethodFor(Behavior, gNew, Behavior_im_gNew);
	cMethodFor(GenericFunction, gNewGeneric, GenericFunction_cm_gNewGeneric);

	InitGenerics();

	cMethodFor(Class, gNewStdClass, Class_cm_gNewStdClass);
	iMethodFor(Behavior, vNew, Behavior_im_gNew);
	iMethodFor(Behavior, gAlloc, Behavior_im_gNew);
	iMethodFor(Behavior, gStackAlloc, Behavior_im_gStackAlloc);

	Object_initialize();
	iMethodFor(Object, gDispose, Object_im_gDispose);
	iMethodFor(Object, gDeepDispose, Object_im_gDispose);
	iMethodFor(Object, gGCDispose, Object_im_gDispose);
	iMethodFor(Object, gEqual, Object_im_gEqual);
	iMethodFor(Object, gSize, Object_im_gSize);
	iMethodFor(Object, gBasicSize, Object_im_gBasicSize);
	iMethodFor(Object, gCopy, Object_im_gCopy);
	iMethodFor(Object, gDeepCopy, Object_im_gCopy);

	GenericFunction_initialize();

	Method_initialize();

	Behavior_initialize();
	iMethodFor(Behavior, gFindMethod, Behavior_im_gFindMethod);
	iMethodFor(Behavior, gFindMethodObject, Behavior_im_gFindMethodObject);
	iMethodFor(Behavior, gInstanceSize, Behavior_im_gInstanceSize);

	MetaClass_initialize();

	Class_initialize();
	iMethodFor(Class, gFindClass, Class_cm_gFindClass);
	iMethodFor(Class, gGetAll, Class_cm_gGetAll);

	Dynace_initialize();
	cMethodFor(Dynace, gGC, Dynace_cm_gGC);
#if	!DPP_FASTWIDE  &&  !defined(NO_CACHE)
	cMethodFor(Dynace, gResizeMethodCache, Dynace_cm_gResizeMethodCache);
#endif
	cMethodFor(Dynace, gSetMemoryBufferArea, Dynace_cm_gSetMemoryBufferArea);
	cMethodFor(Dynace, gRegisterMemory, Dynace_cm_gRegisterMemory);
/*	cMethodFor(Dynace, gRemoveAllRegisteredMemory, Dynace_cm_gRemoveAllRegisteredMemory);   */
	cMethodFor(Dynace, gRemoveRegisteredMemory, Dynace_cm_gRemoveRegisteredMemory);
	cMethodFor(Dynace, gChangeRegisteredMemory, Dynace_cm_gChangeRegisteredMemory);
	cMethodFor(Dynace, gTrace, Dynace_cm_gTrace);
	cMethodFor(Dynace, gMarkRange, Dynace_cm_gMarkRange);
	cMethodFor(Dynace, gMarkObject, Dynace_cm_gMarkObject);
	cMethodFor(Dynace, gMaxMemUsed, Dynace_cm_gMaxMemUsed);
	cMethodFor(Dynace, gCurMemUsed, Dynace_cm_gCurMemUsed);
	cMethodFor(Dynace, gNumbGC, Dynace_cm_gNumbGC);
	cMethodFor(Dynace, gMaxAfterGC, Dynace_cm_gMaxAfterGC);
	cMethodFor(Dynace, gDumpObjects, Dynace_cm_gDumpObjects);
	cMethodFor(Dynace, gDumpObjectsString, Dynace_cm_gDumpObjectsString);
	cMethodFor(Dynace, gDumpObjectsDiff, Dynace_cm_gDumpObjectsDiff);
	cMethodFor(Dynace, gMarkMemoryBeginning, Dynace_cm_gMarkMemoryBeginning);
	cMethodFor(Dynace, gDumpMemoryDiff, Dynace_cm_gDumpMemoryDiff);

	cMethodFor(GenericFunction, gGetAll, GenericFunction_cm_gGetAll);
	cMethodFor(GenericFunction, gFindGeneric, GenericFunction_cm_gFindGeneric);

	gTrace(Generic(gTracePrint), DYNACE_TRACE_OFF);
	gTrace(Generic(gTrace), DYNACE_TRACE_OFF);
	WeakPointer;  /* this class must be initialized before the GC is ever called  */
}


#if	!defined(SEGMENTED_MEMORY)  &&  defined(HEAP_HAS_HOLES)

/*  Need a way to get to the real calloc  */

#ifdef calloc
#undef calloc
#endif

LOCAL	void	*real_calloc(size_t n, size_t s)
{
	return calloc(n, s);
}

#endif



