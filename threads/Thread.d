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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdint.h>



typedef	struct	_priority_queue	  *PQ;



defclass  Thread  {
	object		iObj;		/*  pointer to thread object  */
	char		*iName;
	int		(*iFun)(void *);
	jmp_buf		iRtn;
	char		*iStack_buf;	/*  save buffer for thread stack  */
	char		*iStack_location;  /*  ending real stack position  */
	intptr_t	iStack_buf_size;/*  size of stack_buf		*/
	intptr_t	iStack_size;	/*  space used on stack_buf	*/
	int		iState;		/*  current state of thread	*/
	int		iAutoDispose;	/*  dispose when thread done	*/
	int		iPriority;	/*  thread priority		*/
	void		*iArg;		/*  argument to thread		*/
	int		iPrev_state;	/*  state prior to a hold	*/
	int		iHold_count;	/*  number of times put on hold */
	int		iRtnVal;
	int		iWr;		/*  wait return			*/
	object		iWait;		/*  LinkObject of threads waiting for done	*/
	struct _Thread_iv_t *iTwait;	/*  thread this thread is waiting for */
	struct _Thread_iv_t *iNext, *iPrev;/*  thread queue		*/
	object		iSema;		/*  semaphore waiting for	*/
	int		iNo_switch;	/*  saved _no_context_switch while not running */
/*	void		*iKmrp;		/*  kernel memory range pointer */

 class:
	char	*cTs_stkpos;	/*  thread starting stack position	*/

	PQ	cMpq;		/*  master priority queue		*/
	PQ	cCpq;		/*  current priority queue executing	*/
	ivType	*cCt;		/*  current executing thread		*/

	ivType	*cTnr;		/*  list of threads not running		*/

	ivType	*cGkt;		/*  blocking getkey thread		*/

	PQ	cFpql;		/*  free priority queue list		*/

	object	cThreads;	/*  StringDictionary of threads - also used
				    to prevent GC from collecting threads */
	unsigned long	cThrno;	/*  thread number - for thread name generation */

 init:	init_class;
};


struct	_priority_queue  {
	int	priority;	/*  larger numbers are higher prioritys */
	ivType	*thread;	/*  queue of threads at this priority */
	PQ	next;   	/*  next lower priority queue  */
};


#include <string.h>

#if	defined(_MSC_VER)
#include <conio.h>
#ifdef _MSC_VER
#if _MSC_VER >= 1400
#define kbhit _kbhit
#define getch _getch
#endif
#endif
#else
int	getch(void);
int	kbhit(void);
#endif



jmp_buf	_t_start;	/*  thread startup location		*/



#define STACKINC	100  /*  stack increment size  */
	


#define NTICKS	1

static	objrtn	Release(object self, int yld);

#ifdef	FUNCTIONS

#define NEXT_THREAD	next_thread()

static	void next_thread(void);
static	void add_tnr(ivType *t);
static	void del_tnr(ivType *t);

#else

#define NEXT_THREAD			\
	if (cCpq)			\
		cCpq->thread = cCpq->thread->iNext;\
	if (cGkt  &&  (!cMpq  ||  kbhit()))  {	\
		Release(cGkt->iObj, 0);	\
		cGkt = NULL;		\
	}				\
	if (!(cCpq = cMpq))		\
		no_ready_threads();	\
	cCt = cCpq->thread

#define add_tnr(t)			\
	if (t->iNext = cTnr)		\
		cTnr->iPrev = t;	\
	t->iPrev = NULL;		\
	cTnr = t

#define del_tnr(t)			\
	if (t->iPrev)			\
		t->iPrev->iNext = t->iNext;\
	else				\
		cTnr = t->iNext;	\
	if (t->iNext)			\
		t->iNext->iPrev = t->iPrev

#endif

extern	void	_start_timer(void);


static	void start_thread(ivType *s);
static	void stop_thread(ivType *t);
static	void resolve_waits(ivType *t);
static	void delete_wait(ivType *t);
static	PQ new_pq(void);
static	void free_pq(PQ t);
static	void delete_sema_waits(ivType *t);
static	void _dynace_yield(void);
static  char *strsave(char *);
static	void	no_ready_threads(void);
static	PQ	find_pq(int priority);

#ifdef	FUNCTIONS

static	void	next_thread(void)
{
	if (cCpq)
		cCpq->thread = cCpq->thread->iNext;
	if (cGkt  &&  (!cMpq  ||  kbhit()))  {
		Release(cGkt->iObj, 0);
		cGkt = NULL;
	}
	if (!(cCpq = cMpq))
		no_ready_threads();
	cCt = cCpq->thread;
}

#endif

/*  Called when no thread is ready to run.  If every remaining thread is done
    the program is finished; otherwise the remaining threads are deadlocked.  */

static	void	no_ready_threads(void)
{
	ivType	*t;

	for (t=cTnr ; t ; t=t->iNext)
		if (t->iState != DONE_THREAD)  {
			fprintf(stderr, "Dynace: deadlock - no threads are ready to run\n");
			exit(1);
		}
	exit(0);
}

/*  start_thread takes a thread and puts it in the running threads priority
    queue	*/

static	void	start_thread(ivType *s)
{
	PQ	tpq, ppq, npq;

	for (ppq=NULL, tpq=cMpq ; tpq && s->iPriority < tpq->priority ; ppq=tpq, tpq=tpq->next);
	if (!tpq  ||  s->iPriority != tpq->priority)  {
		npq = new_pq();
		s->iNext = s->iPrev = s;
		npq->priority = s->iPriority;
		npq->thread = s;
		npq->next = tpq;
		if (ppq)
			ppq->next = npq;
		else
			cMpq = npq;
	} else 
		if (tpq == cCpq)  {
			s->iNext = tpq->thread->iNext;
			s->iPrev = tpq->thread;
			tpq->thread->iNext = s;
			s->iNext->iPrev = s;
		} else {
			s->iNext = tpq->thread;
			s->iPrev = tpq->thread->iPrev;
			tpq->thread->iPrev = s;
			s->iPrev->iNext = s;
			tpq->thread = s;
		}
}
		
/*  stop_thread takes a thread off the running threads priority queue  */

static	void	stop_thread(ivType *t)
{
	PQ	pq, ppq;

	for (ppq=NULL, pq=cMpq ; pq  &&  pq->priority != t->iPriority ;
	     ppq=pq, pq=pq->next);
	if (!pq)
		goto error;
	if (t->iNext != t)  {
		t->iPrev->iNext = t->iNext;
		t->iNext->iPrev = t->iPrev;
		if (pq->thread == t)  {
			pq->thread = t->iNext;
			if (pq == cCpq)
				cCpq = NULL;
		}
	}  else  {
		if (pq->thread != t)
			goto error;
		if (pq == cCpq)
			cCpq = NULL;
		if (ppq)
			ppq->next = pq->next;
		else
			cMpq = pq->next;
		free_pq(pq);
	}
	return;
 error:
	fprintf(stderr, "Bad priority queue\n");
	exit(1);
}
		
#ifdef	FUNCTIONS

/*  add thread to the threads-not-running list   */

static	void	add_tnr(ivType *t)
{
	if (t->iNext = cTnr)
		cTnr->iPrev = t;
	t->iPrev = NULL;
	cTnr = t;
}

/*  delete thread from the threads-not-running list   */

static	void	del_tnr(ivType *t)
{
	if (t->iPrev)
		t->iPrev->iNext = t->iNext;
	else
		cTnr = t->iNext;
	if (t->iNext)
		t->iNext->iPrev = t->iPrev;
}

#endif

imeth	gKill : Kill (object self, int rtn)
{
	if (iState == DONE_THREAD)
		return self;
	INHIBIT_THREADER;
	iRtnVal = rtn;
	if (iStack_buf)  {
		free(iStack_buf);
		iStack_buf = NULL;
		iStack_buf_size = iStack_size = 0;
	}
	if (iState == NEW_THREAD  ||  iState == RUNNING_THREAD)  {
		stop_thread(iv);
		add_tnr(iv);
	} else if (iState == WAITING_FOR_THREAD  ||
		   iState == HOLD_THREAD  &&  iPrev_state == WAITING_FOR_THREAD)
		delete_wait(iv);
	iState = DONE_THREAD;
	if (iSema)
		delete_sema_waits(iv);
	if (iWait)
		resolve_waits(iv);
	if (iv == cGkt)
		cGkt = NULL;
#if 0
	if (iKmrp)  {
		static	gRemoveRegisteredMemory_t	rrm = NULL;

		if (!rrm)
			rrm = cmcPointer(Dynace, gRemoveRegisteredMemory);
		(rrm)(Dynace, iKmrp);
		iKmrp = NULL;
	}
#endif
	ENABLE_THREADER;
	if (cCt == iv)  {
		cCt = NULL;
		_dynace_yield();
	}
	return self;
}

imeth	object	gDispose, gDeepDispose : Dispose (object self)
{
	INHIBIT_THREADER;
	if (iStack_buf)
		free(iStack_buf);
	if (iState == NEW_THREAD  ||  iState == RUNNING_THREAD)
		stop_thread(iv);
	else  {
		if (iState == WAITING_FOR_THREAD  ||
		    iState == HOLD_THREAD  &&  iPrev_state == WAITING_FOR_THREAD)
			delete_wait(iv);
		del_tnr(iv);
	}
/*	iRtnVal = r;  */
	iState = DONE_THREAD;
	if (iSema)
		delete_sema_waits(iv);
	if (iWait)  {
		static	gDispose_t	dispose = NULL;

		if (!dispose)
			dispose = imcPointer(LinkObject, gDispose);
		resolve_waits(iv);
		(*dispose)(iWait);
	}
	if (iv == cGkt)
		cGkt = NULL;
#if 0
	if (iKmrp)  {
		static	gRemoveRegisteredMemory_t	rrm = NULL;

		if (!rrm)
			rrm = cmcPointer(Dynace, gRemoveRegisteredMemory);
		(*rrm)(Dynace, iKmrp);
	}
#endif
	gRemoveStr(cThreads, iName);
	free(iName);
	gDispose(super);
	ENABLE_THREADER;
	if (cCt == iv)  {
		cCt = NULL;
		_dynace_yield();
	}
	return NULL;
}

static	char	*strsave(char *s)
{
	char	*p = Tnalloc(char, strlen(s)+1);
	strcpy(p, s);
	return p;
}

cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewThread, <vNew> (object self, char *name, ifun fun, int priority, void *arg, int run, int autoDispose)
{
	char	buf[30];
	object	obj;
	ivType	*iv;
	static	gFindStr_t	find = NULL;
	static	gAddStr_t	add = NULL;

	INHIBIT_THREADER;
	if (!name)  {
		sprintf(buf, "unnamed-%lu", ++cThrno);
		name = buf;
	}
	if (!find)
		find = imcPointer(StringDictionary, gFindStr);
	if ((*find)(cThreads, name))  {
		ENABLE_THREADER;
		return NULL;
	}
	obj = gNew(super);
	iv = ivPtr(obj);
	iObj = obj;
	iName = strsave(name);
		
	iFun = (int (*)(void *)) fun;
	iPriority = priority;
	iArg = arg;
	iAutoDispose = autoDispose;

	if (!add)
		add = imcPointer(StringDictionary, gAddStr);
	(*add)(cThreads, iName, obj);
	ENABLE_THREADER;

	if (run)  {
		iState = NEW_THREAD;
		start_thread(iv);
		if (cCt  &&  cCt->iPriority < priority)
			_dynace_yield();
	}  else  {
		iPrev_state = NEW_THREAD;
		iState = HOLD_THREAD;
		iHold_count = 1;
		add_tnr(iv);
	}

	return obj;
}

void	_start_threader(char *stkpos)
{
	object	t;

	Thread;
	if (cMpq)
		return;

	INHIBIT_THREADER;
	t = gNewThread(CLASS, "main", NULL, DEFAULT_PRIORITY, NULL, 1, 0);
	ENABLE_THREADER;
	cCt = ivPtr(t);

	cCt->iState = RUNNING_THREAD;
	cCpq = cMpq;

	cTs_stkpos = stkpos;

	_start_timer();
}

void	_start_thread(void)
{
	cCt->iState = RUNNING_THREAD;
	_tick_count = NTICKS;
	cCt->iRtnVal = (*cCt->iFun)(cCt->iArg);
	//  Both Kill and Dispose yield away and never return when the thread is current
	if (cCt->iAutoDispose)
		Dispose(cCt->iObj);
	else
		Kill(cCt->iObj, cCt->iRtnVal);
}

imeth	gHold : Hold (object self)
{
	if (iState == DONE_THREAD)
		return self;
	if (iState == HOLD_THREAD)  {
		iHold_count++;
		return self;
	}
	if (iState == NEW_THREAD  ||  iState == RUNNING_THREAD)  {
		stop_thread(iv);
		add_tnr(iv);
	}
	iPrev_state = iState;
	iState = HOLD_THREAD;
	iHold_count = 1;
	if (iv == cCt)
		_dynace_yield();
	return self;
}

imeth	gRelease : Release (object self, int yld)
{
	if (iState == HOLD_THREAD  &&  !--iHold_count)  {
		iState = iPrev_state;
		if (iState == NEW_THREAD  ||  iState == RUNNING_THREAD)  {
			del_tnr(iv);
			start_thread(iv);
			if (yld  &&  iPriority > cCt->iPriority)
				_dynace_yield();
		}
	}
	return self;
}

/*  Thread switching copies each thread's part of the C stack (from the
    threader's starting position down to the switch point) to and from a
    buffer.  The saved region must include all of _dynace_yield's frame, so its
    low end is taken from a frame below it.  A thread's stack is restored from
    a frame below the region being written, then the thread is resumed with
    longjmp.  Stacks are assumed to grow down.  */

/*  Copying the raw stack deliberately reads and writes outside any one
    variable, so AddressSanitizer must not instrument it.  ASan also checks
    memcpy calls even in uninstrumented functions, so a plain loop is used
    under ASan.  */
#if defined(__has_attribute)
#if __has_attribute(no_sanitize_address)
#define	NO_SANITIZE_ADDRESS	__attribute__((no_sanitize_address))
#endif
#endif
#ifndef	NO_SANITIZE_ADDRESS
#define	NO_SANITIZE_ADDRESS
#endif

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define	ASAN_BUILD
#endif
#endif
#ifdef	__SANITIZE_ADDRESS__
#define	ASAN_BUILD
#endif

#ifdef	ASAN_BUILD
NO_SANITIZE_ADDRESS
static	void	stack_copy(char *to, char *from, intptr_t n)
{
	volatile char	*d = to;	/*  volatile keeps the loop from becoming a memcpy call  */

	while (n-- > 0)
		*d++ = *from++;
}
#else
#define	stack_copy(to, from, n)	memcpy((to), (from), (size_t)(n))
#endif

static	void	stack_mark(char **p)
{
	void	* volatile	c;  /*  pointer-typed so the saved region keeps pointer alignment for the GC  */

	*p = (char *) &c;
}

/*  called through a volatile pointer so it can't be inlined into the caller's frame  */
static	void	(* volatile stack_mark_fp)(char **) = stack_mark;

#define	RESTORE_PAD	256

NO_SANITIZE_ADDRESS
static	void	restore_stack(ivType *t)
{
	volatile char	pad[RESTORE_PAD];

	pad[0] = 0;
	if ((char *) pad >= t->iStack_location - 2 * RESTORE_PAD)  {
		restore_stack(t);  /*  grow the stack until this frame is below the region  */
		pad[0]++;	   /*  not a tail call  */
	}
#ifdef sparc
	asm("t 3");	/*  flush out registers */
#endif
	stack_copy(t->iStack_location, t->iStack_buf, t->iStack_size);
	longjmp(t->iRtn, 1);
}

NO_SANITIZE_ADDRESS
static	void	_dynace_yield(void)
{
	char	*mark;

	/*  save current thread stack  */

	if (cCt  &&  cCt->iState != DONE_THREAD)  {
#ifdef	__GNUC__
		/*  Spill callee-saved registers into this frame so the saved stack holds
		    every live object pointer.  setjmp may mangle some (e.g. glibc ebp),
		    hiding them from the GC.  */
		__builtin_unwind_init();
#endif
		(*stack_mark_fp)(&mark);
#ifdef sparc
		/* save current stack frame (approximately 0x70 byte), too */
		mark -= 0x80;
#endif
		cCt->iNo_switch = _no_context_switch;
		cCt->iStack_location = mark;
		if ((cCt->iStack_size = cTs_stkpos - cCt->iStack_location) < 0)
			cCt->iStack_size = 0;
		if (cCt->iStack_size > cCt->iStack_buf_size)  {
			cCt->iStack_buf_size = ((cCt->iStack_size / STACKINC) + 1) * STACKINC;
			if (cCt->iStack_buf)
				cCt->iStack_buf = Tnrealloc(char, cCt->iStack_buf_size, cCt->iStack_buf);
			else
				cCt->iStack_buf = Tnalloc(char, cCt->iStack_buf_size);
		}
		if (cCt->iStack_size)  {
#ifdef sparc
			asm("t 3");	/* flush out registers */
#endif
			stack_copy(cCt->iStack_buf, cCt->iStack_location, cCt->iStack_size);
		}

		/*  save return address  */

		if (setjmp(cCt->iRtn))  {

			/*  resumed by restore_stack, stack already restored  */

			_tick_count = NTICKS;
			return;	
		}
	}

	/*  choose next thread  */

	NEXT_THREAD;

	/*  the inhibit count belongs to the thread that set it  */
	_no_context_switch = cCt->iState == NEW_THREAD ? 0 : cCt->iNo_switch;

	if (cCt->iState == NEW_THREAD)
		longjmp(_t_start, 1);
	else if (cCt->iStack_size)
		restore_stack(cCt);
	else
		longjmp(cCt->iRtn, 1);
}

cmeth	gFindStr, <vFind> (object self, char *name)
{
	USE(self);
	return name ? gFindValueStr(cThreads, name) : (cCt ? cCt->iObj : (object) NULL);
#if 0
	ivType	*t, *s;
	PQ	pq;

	if (!name)
		return cCt ? cCt->iObj : NULL;
	for (pq=cMpq ; pq ; pq=pq->next)  {
		s = t = pq->thread;
		do  {
			if (t->iName  &&  !strcmp(t->iName, name))
				return t->iObj;
			t = t->iNext;
		} while (t != s);
	}
	for (t=cTnr ; t ; t=t->iNext)
		if (t->iName  &&  !strcmp(t->iName, name))
			return t->iObj;
	return NULL;
#endif
}

imeth	int	gIntValue(object self)
{
	return iRtnVal;
}

imeth	int	gState(object self)
{
	return iState;
}

imeth	char	*gName(object self)
{
	return iName;
}

imeth	int	gPriority(object self)
{
	return iPriority;
}

imeth	gChangePriority(object self, int p)
{
	int	f, oldp;

	oldp = iPriority;
	if (f = iState == NEW_THREAD  ||  iState == RUNNING_THREAD)
		stop_thread(iv);
	iPriority = p;
	if (f)  {
		start_thread(iv);
		if (cCt == iv)  {
			/*  stop_thread cleared cCpq; make this thread current in its new queue  */
			cCpq = find_pq(p);
			cCpq->thread = iv;
		}
	}
	if (cCt == iv  &&  p < oldp  &&  cMpq->thread->iPriority > p  ||
	    cCt != iv  &&  f  &&  p > cCt->iPriority)
		_dynace_yield();
	return self;
}

imeth	int	gWaitFor(object self)
{
	if (iv == cCt)
		return 0;
	if (iState != DONE_THREAD)  {
		stop_thread(cCt);
		add_tnr(cCt);

		INHIBIT_THREADER;
		if (!iWait)
			iWait = gNew(LinkObject);
		gAddFirst(iWait, cCt->iObj);
		ENABLE_THREADER;

		cCt->iTwait = iv;
		cCt->iState = WAITING_FOR_THREAD;
		_dynace_yield();
		return cCt->iWr;
	} else
		return iRtnVal;
}

/*  this function signals all the threads waiting for t (used when t is DONE) */

static	void	resolve_waits(ivType *t)
{
	object	thread;
	static	gFirst_t	first=NULL;
	static	gDisposeFirst_t	disposeFirst;

	if (!t->iWait)
		return;
	INHIBIT_THREADER;
	if (!first)  {
		first = imcPointer(LinkObject, gFirst);
		disposeFirst = imcPointer(LinkObject, gDisposeFirst);
	}
	while (thread = (*first)(t->iWait))  {
		ivType	*tt;

		tt = ivPtr(thread);
		if (tt->iState == HOLD_THREAD)
			tt->iPrev_state = RUNNING_THREAD;
		else  {
			tt->iState = RUNNING_THREAD;
			del_tnr(tt);
			start_thread(tt);
		}
		tt->iWr = t->iRtnVal;
		tt->iTwait = NULL;
		(*disposeFirst)(t->iWait);
	}
	ENABLE_THREADER;
}

/*  this functions is used to tell other threads that thread t is no longer
    waiting  */

static	void	delete_wait(ivType *t)
{
	object	linkSequence, linkValue, thread;
	static	gDispose_t	disposeNode, dispose;
	static	gValue_t	value;
	static	gNext_t	next;
	static	gSequenceLinks_t	sequenceLinks = NULL;

	if (!t->iTwait->iWait)
		return;
	INHIBIT_THREADER;
	if (!sequenceLinks)  {
		sequenceLinks = imcPointer(LinkObject, gSequenceLinks);
		next = imcPointer(LinkSequence, gNext);
		value = imcPointer(LinkValue, gValue);
		disposeNode = imcPointer(LinkValue, gDispose);
		dispose = imcPointer(LinkSequence, gDispose);
	}
	linkSequence = (*sequenceLinks)(t->iTwait->iWait);
	while (linkValue = (*next)(linkSequence))  {
		thread = (*value)(linkValue);
		if (thread == t->iObj)  {
			(*disposeNode)(linkValue);
			break;
		}
	}
	if (linkValue)
		(*dispose)(linkSequence);
	ENABLE_THREADER;
}

static	PQ	find_pq(int priority)
{
	PQ	pq;

	for (pq=cMpq ; pq  &&  pq->priority != priority ; pq=pq->next);
	return pq;
}

static	PQ	new_pq(void)
{
	PQ	t;

	if (cFpql)  {
		t = cFpql;
		cFpql = t->next;
		memset(t, 0, sizeof(*t));
	}  else
		t = Tcalloc(struct _priority_queue);
	return(t);
}

static	void	free_pq(PQ t)
{
	t->next = cFpql;
	cFpql = t;
}

static	void	delete_sema_waits(ivType *t)
{
	static	gRemoveWaits_t	removeWaits = NULL;

	INHIBIT_THREADER;
	if (!removeWaits)
		removeWaits = imiPointer(t->iSema, gRemoveWaits);
	(*removeWaits)(t->iSema, t->iObj);
	t->iSema = NULL;
	ENABLE_THREADER;
}

cmeth	int	gBlockingGetkey(object self)
{
	USE(self);
	if (cGkt)
		return -1;
	if (cCt  &&  !kbhit())  {
		cGkt = cCt;
		Hold(cCt->iObj);
	}
	return getch();
}

/*  the following method is used as a subroutine by Semaphore Class  */

imeth	gReleaseSemaphore(object self)
{
	if (iState == WAITING_FOR_SEMAPHORE)  {
		iState = RUNNING_THREAD;
		del_tnr(iv);
		start_thread(iv);
	} else if (iState == HOLD_THREAD  &&  iPrev_state == WAITING_FOR_SEMAPHORE)
		iPrev_state = RUNNING_THREAD;
	iSema = NULL;
	return self;
}

/*  the following method is used as a subroutine by Semaphore Class  */

imeth	gWaitSemaphore(object self, object sema)
{
	stop_thread(iv);
	add_tnr(iv);
	iState = WAITING_FOR_SEMAPHORE;
	iSema = sema;
	return self;
}

static	objrtn	MarkThreadStacks(object self)
{
	ivType	*t, *s;
	PQ	pq;
	static	gMarkRange_t	markRange = NULL;

	if (!markRange)
		markRange = cmcPointer(Dynace, gMarkRange);

	for (pq=cMpq ; pq ; pq=pq->next)  {
		s = t = pq->thread;
		do  {
			if (t->iStack_buf  &&  t->iStack_size)
				(*markRange)(Dynace, (char _HUGE_ **)t->iStack_buf, (char _HUGE_ **)(t->iStack_buf + t->iStack_size));
			t = t->iNext;
		} while (t != s);
	}
	for (t=cTnr ; t ; t=t->iNext)
		if (t->iStack_buf  &&  t->iStack_size)
			(*markRange)(Dynace, (char _HUGE_ **) t->iStack_buf, (char _HUGE_ **)(t->iStack_buf + t->iStack_size));
	return self;
}

#if	defined(unix)  ||  defined(__APPLE__)  ||  defined(PLAN9)  ||  defined(__minix)

#ifdef	PLAN9

/*  not implemented on Plan 9  */

int	kbhit(void)
{
	return 0;
}

#else

#include <sys/select.h>
#include <unistd.h>

/*  true if input is ready on stdin (a whole line when the terminal is line buffered)  */

int	kbhit(void)
{
	fd_set		fds;
	struct timeval	tv;

	FD_ZERO(&fds);
	FD_SET(STDIN_FILENO, &fds);
	tv.tv_sec = tv.tv_usec = 0;
	return select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) > 0;
}

#endif

int	getch(void)
{
	return getchar();
}

#endif

imeth	gCopy, gDeepCopy (object self)
{
	return gShouldNotImplement(self, "Copy/DeepCopy");
}

static	void	init_class(void)
{

/*	gDontCollect(CLASS);	*/

	__dynace_yield = _dynace_yield;

	cThreads = gNewWithInt(StringDictionary, 41);

	gMarkingMethod(CLASS, (ofun) MarkThreadStacks);
}

