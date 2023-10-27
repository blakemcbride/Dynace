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

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

/*  What kind of machine?  */

#ifdef	_MSC_VER
#if	_MSC_VER >= 900  &&  (defined(_M_IX86) || defined(_M_X64))
#define	MSC32
#define	_INLINE_	__forceinline
#else
#define MSC16
#define	_INLINE_	__inline
#endif
#endif

#ifdef	__GNUC__
#define	_INLINE_	inline
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

#ifdef	__SC__		/*  Symantec C  */
#ifdef	_WIN32
#define	SYMC32
#else
#define	SYMC16
#endif
#endif

#ifndef	_INLINE_
#define	_INLINE_
#endif

#ifdef MSC32
#if _MSC_VER > 1200
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif
#ifndef _POSIX_
#define _POSIX_
#endif
#endif
#endif


/* int to pointer conversion  */
/* turns out, this is already defined in Windows  */
#if !defined(_MSC_VER)  &&  !defined(__WINE__)
#ifdef _M_X64
typedef long long  INT_PTR;
#else
typedef unsigned long  INT_PTR;
#endif
#endif

#if defined(_MSC_VER)
#  if _MSC_VER > 1200
#    define PTOLL(p)	((long long)(INT_PTR)p)
#  else
#    define PTOLL(p)	((INT_PTR)p)
#    define ULONG_PTR	DWORD
#    define DWORD_PTR	LPDWORD
#    define SQLLEN	SQLINTEGER
#    define SQLULEN	SQLUINTEGER
#  endif
#else
#  define PTOLL(p)	((long long)(INT_PTR)p)
#endif

#ifdef	PLAN9
#include "plan9.h"
#endif

#ifdef _WIN32
#include <windows.h>
#endif

#include <stdio.h>
/*#include <malloc.h>*/

#include <stdlib.h>
#include <setjmp.h>
#include <stdarg.h>
#include <string.h>

#if	defined(BOEHM_GC)  &&  !defined(BIGLOO)
#include "gc.h"
#define	malloc(x)	GC_malloc_ignore_off_page(x)
#define	free(x)
#define	calloc(n,s)	GC_malloc_ignore_off_page((n)*(s))
#define	realloc(p,s)	GC_realloc(p,s)
#endif

#ifdef	RTKERNEL
#ifndef	NATIVE_THREADS
#define	NATIVE_THREADS
#endif
#include "dynrtk.h"
#endif

#define	NULLOBJ	((object)0)

#ifdef	NATIVE_THREADS
#ifdef  __unix__
	#include <pthread.h>
	typedef pthread_mutex_t			CRITICALSECTION;
	extern pthread_mutexattr_t _mutex_attr;
	#define	INITIALIZECRITICALSECTION(cs)	pthread_mutex_init(&cs, &_mutex_attr)
	#define	DELETECRITICALSECTION(cs)	pthread_mutex_destroy(&cs)
	#define	ENTERCRITICALSECTION(cs)	pthread_mutex_lock(&cs)
	#define	LEAVECRITICALSECTION(cs)	pthread_mutex_unlock(&cs)
#else
	typedef	CRITICAL_SECTION		CRITICALSECTION;
	#define	INITIALIZECRITICALSECTION(cs)	InitializeCriticalSection(&cs)
	#define	DELETECRITICALSECTION(cs)	DeleteCriticalSection(&cs)
	#define	ENTERCRITICALSECTION(cs)	EnterCriticalSection(&cs)
	#define	LEAVECRITICALSECTION(cs)	LeaveCriticalSection(&cs)
#endif
#else
	typedef	int	CRITICALSECTION;
	#define	INITIALIZECRITICALSECTION(cs)	USE(cs)
	#define	DELETECRITICALSECTION(cs)	USE(cs)
	#define	ENTERCRITICALSECTION(cs)	USE(cs)
	#define	LEAVECRITICALSECTION(cs)	USE(cs)
#endif

#include "findfile.h"

#ifdef	__cplusplus
extern "C"  {
#endif

/*  objects must be declared volatile because if the variable is kept in a
    register, it may not be garbage collected  */
/*  This is a problem since 'object' is also used as a function return type.
    Function return types are not allowed to use 'volatile'  */

#if	1  ||  defined(MSC32)  ||  defined(BC32)  ||  (defined(unix) && defined(i386))
typedef	struct	_Object_iv_t	 *object;
#else
typedef	struct	_Object_iv_t	 * volatile object;
#endif

/*  objrtn is used to signify functions which return objects.  It is necessary
    because some compilers (GNU) don't like function return types declared as
    volatile.   */

typedef	struct	_Object_iv_t	 *objrtn;

typedef	objrtn	(*ofun)();
typedef	int	(*ifun)();
typedef void	(*vfun)();

void	InitDynace(void *);
void	InitKernel(void *, int);
ofun	_FindMethod(object, object);
ofun	_FindMethod2(object, object, int);
objrtn	_jumpToMethod(ofun f);
void	*GetIVptr(object, object);
int	IsObj(object);
void	InitGenerics(void);
void	_IC_exec(object *);


#define DYNACE_TRACE_DONT_CARE		0
#define DYNACE_TRACE_OFF		1
#define DYNACE_TRACE_ON			2
#define DYNACE_TRACE_ALL		3

#define	_cat2_(a, b)	a##b
#define _cat_(a, b)	_cat2_(a, b)

#define Generic(x)	_cat_(x, _g)


#define InitClass(c)	c##_initialize()

#define ivPtr(x)	((ivType *) GetIVptr((x), CLASS))
#define ivsPtr		ivPtr(self)
#define accessIVs	ivType *iv = ivsPtr
#define accessIVsOf(x)	ivType *iv = ivPtr(x)

#define	GetIVs(c,x)	((c##_iv_t *) GetIVptr((x), c##_c))
#define	GetCVs(c)	((c##_cv_t *) GetIVptr(c##_c, ClassOf(c##_c)))

#define ivMethodFor(c,g,m,f)	\
	gNewMethod(Method_c, #m, c##_c, Generic(g), (ofun) m, (ofun) f)
#define cvMethodFor(c,g,m,f)	\
	gNewMethod(Method_c, #m, ClassOf(c##_c), Generic(g), (ofun) m, (ofun) f)

#define iMethodFor(c,g,m)	\
	gNewMethod(Method_c, #m, c##_c, Generic(g), (ofun) m, (ofun) m)
#define cMethodFor(c,g,m)	\
	gNewMethod(Method_c, #m, ClassOf(c##_c), Generic(g), (ofun) m, (ofun) m)

#define ClassOf(x)	(object) *((object *) (x))

/*#define	PROFILE */
	
#ifdef	PROFILE
#define	LOCAL
#else
#define	LOCAL		static
#define	PMETHOD		static
#endif

#define imeth		LOCAL
#define cmeth		LOCAL
#define ivmeth		LOCAL
#define cvmeth		LOCAL

#ifndef	PMETHOD
#define	PMETHOD		static
#endif

#define END		(object) 0

#define IsInstanceOf(i,c)	(ClassOf(i) == (c))
#define IsaMetaClass(x)		(ClassOf(x) == MetaClass_c)
#define IsaClass(x)		(IsaMetaClass(ClassOf(x)) || IsaMetaClass(x))

#define EQ(a,b)		((a) == (b))
#define NEQ(a,b)	((a) != (b))


#define tFindMethod(t, c, g, l)  (*(t (*)()) _FindMethod2(c, g, l))


/* get instance method from a class object */
#define imcPointer(c,g)  (g##_t) gFindMethod(c, Generic(g), 1)

/* get class method from a class object */
#define cmcPointer(c,g)  (g##_t) gFindMethod(ClassOf(c), Generic(g), 1)

/* get instance method from an instance object */
#define imiPointer(i,g)  (g##_t) gFindMethod(ClassOf(i), Generic(g), 1)

/* get class method from an instance object */
#define cmiPointer(i,g)  (g##_t) gFindMethod(ClassOf(ClassOf(i)), Generic(g), 1)

#define iSuper(c,g)	(*(g##_t)_FindMethod2(c, Generic(g), 2))
#define cSuper(c,g)	(*(g##_t)_FindMethod2(ClassOf(c), Generic(g), 2))
#define typedSuper(t,g)	(*tFindMethod(t, ClassOf(self), Generic(g), 2))
#define typedGeneric(t,g) 	(*(t (*)()) g)

#if 0
#define oSuper(c, g, x)	\
	((ClassOf(ClassOf(x)) == MetaClass_c) ? cSuper(c, g) : iSuper(c, g))
#else
#define oSuper(c,g,x)	(*(g##_t)_FindMethod2((ClassOf(ClassOf(x)) == MetaClass_c) ? ClassOf(c) : c, Generic(g), 2))
#endif
	
#define RespondsTo(i,g)	(gFindMethod(ClassOf(i), Generic(g), 1) != NULL)

#define RegisterVariable(v)	gRegisterMemory(Dynace_c, (void *)&v, (long) sizeof(object))

	
extern	int	_CheckObjects_;
extern	object	_LastGeneric_;		/*  last generic called  */
extern	CRITICALSECTION  _CI_CS_;	/*  used during class initialization  */


#define GetArg(type)	va_arg(_rest_, type)

#define ChkArg(obj, argn)						\
	if (_CheckObjects_  &&  !IsObj(obj))				\
		gInvalidObject(_LastGeneric_, argn, self)

#define ChkArgTyp(obj, argn, argType)					\
	if (_CheckObjects_)  {						\
		if (!IsObj(obj))					\
			gInvalidObject(_LastGeneric_, argn, self);	\
		if (argType)  {						\
			object	lg = _LastGeneric_;			\
			if (!gIsKindOf(obj, argType))			\
				gInvalidType(lg, argn, self, argType, obj); \
			_LastGeneric_ = lg;				\
		}							\
	}

#define ChkArgNul(obj, argn)           if (obj)  ChkArg(obj, argn)

#define ChkArgTypNul(obj, argn, argType) 	\
	if (obj)  ChkArgTyp(obj, argn, argType)


/*  Thread stuff  */


/*  thread states   */

#define NEW_THREAD		0
#define	RUNNING_THREAD		1
#define HOLD_THREAD		2
#define DONE_THREAD		3
#define EXPUNGE_THREAD		4
#define WAITING_FOR_THREAD	5
#define WAITING_FOR_SEMAPHORE	6

#define DEFAULT_PRIORITY	100

extern	int	_tick_count;
extern	int	_no_context_switch;
extern	jmp_buf	_t_start;

extern	void	(*__dynace_yield)(void);
extern	void	_start_threader(char *stkpos), _start_thread(void);

#define INHIBIT_THREADER	_no_context_switch++
#define ENABLE_THREADER		if (_no_context_switch) _no_context_switch--

#if 0  /*  Debugging or to disable Threads  */
#define YIELD	
#else
#define YIELD	if (!_tick_count  &&  !_no_context_switch)  (*__dynace_yield)()
#endif

#define StartThreader(x)		\
	_start_threader((char *) &x);	\
	if (setjmp(_t_start))		\
		_start_thread()


/*  End of thread specific stuff  */

#ifndef	MSC32
#define IGTYPE	static
#else
#define IGTYPE	static	__declspec(naked) 
#endif

#if	!defined(MSC32)  &&  !defined(__alpha)
#define GenericBody(g)   _jumpToMethod( _FindMethod(self, Generic(g)) )
#endif

#ifdef	__alpha
#define GenericBody(g)  				\
	va_list args; va_start(args, self);             \
	_jumpToMethod( _FindMethod(self, Generic(g)) )
#endif

#ifdef	MSC32
#define GenericBody(g)					\
	_asm {	push	DWORD PTR Generic(g) }		\
	_asm {	push	DWORD PTR [esp+8] }		\
	_asm {	call	_FindMethod	 }		\
	_asm {	add	esp, 8	 }			\
	_asm {	jmp	eax }
#endif



#if 0
#ifndef	__alpha
#define defGeneric(t, g)   					\
	object	Generic(g) = (object) 0;			\
	static	t	_##g(object i, ...)			\
        {							\
		_jumpToMethod( _FindMethod(i, Generic(g)) ); 	\
	}							\
        t	(*g)(object i, ...) = _##g;
#else
#define defGeneric(t, g)                                        \
        object  Generic(g) = (object) 0;                        \
        static  t       _##g(object i, ...)                     \
        {                                                       \
                va_list args; va_start(args, i);                \
                _jumpToMethod( _FindMethod(i, Generic(g)) );    \
        }                                                       \
        t       (*g)(object i, ...) = _##g;
#endif
#endif /* 0 */

#define externGeneric(t, g)					\
	extern	object	Generic(g);				\
	extern	t	(*g)(object i, ...);



#define InitGeneric(x)	Generic(x) = gNewGeneric(GenericFunction_c, #x, x)


#if 0 /* DPP_FASTWIDE  */

typedef	struct  {
	object	fill1;
	unsigned short fill2, fill3;
}	*OHead;

#define	ClassID(c)		*(int *)((OHead)(c) + 1)
#define	MethCache(g)		(*(ofun **)((OHead)Generic(g) + 1))
#define	MethodPointer(g, obj)	MethCache(g)[ClassID(ClassOf(obj))]


#if	DPP_STRATEGY > 2
inline	void	gInvalidObject(object self, int argn, object arg1);
#endif

#if	DPP_STRATEGY <= 2
void	(*gInvalidObject)(object self, int argn, object arg1);
#endif


#define FW_GENERIC(g) 						\
	if (_CheckObjects_)  {					\
		if (!IsObj(self))				\
			gInvalidObject(Generic(g), 1, self);	\
		_LastGeneric_ = Generic(g);			\
	}							\
	YIELD;							\
	if (!(_meth_ = MethodPointer(g, self)))			\
		MethodPointer(g, self) = _meth_ = _FindMethod(self, Generic(g))
	

#endif

#if	DPP_FASTWIDE

typedef	struct  {
	objrtn	fill1;
	unsigned short fill2, fill3;
	int	id;
}	*CHead;

typedef	struct  {
	objrtn	fill1;
	unsigned short fill2, fill3;
	ofun	*mc;
}	*GHead;

#define	MethodPointer(g, obj)   \
        ((GHead)Generic(g))->mc[((CHead)ClassOf(obj))->id]

#if	DPP_STRATEGY > 2
inline	void	gInvalidObject(object self, int argn, object arg1);
#endif

#if	DPP_STRATEGY <= 2
void	(*gInvalidObject)(object self, int argn, object arg1);
#endif

#ifndef	SMALL_FW

#define FW_GENERIC(g) 						\
	if (_CheckObjects_)  {					\
		if (!IsObj(self))				\
			gInvalidObject(Generic(g), 1, self);	\
		_LastGeneric_ = Generic(g);			\
	}							\
	YIELD;							\
	if (!(_meth_ = MethodPointer(g, self)))			\
		MethodPointer(g, self) = _meth_ = _FindMethod(self, Generic(g))

#else

#define FW_GENERIC(g) 						\
	if (!(_meth_ = MethodPointer(g, self)))			\
		MethodPointer(g, self) = _meth_ = _FindMethod(self, Generic(g))

#endif	

#endif



#ifndef  Talloc
#if	0 && defined(WIN32)
extern	void  *_Dynace_malloc( long nbytes );
extern	void  *_Dynace_calloc( long size, long nelem );
extern	void  _Dynace_free( void *cp );
extern	void  *_Dynace_realloc(void  *cp, long nbytes );

#define	malloc(n)	_Dynace_malloc(n)
#define	calloc(a, b)	_Dynace_calloc((a), (b))
#define	realloc(a,b)	_Dynace_realloc((a), (b))
#define	free(x)		_Dynace_free(x)

#endif

extern	void	*Chkmem(void *, char *, int);
#define	 Talloc(t)   (t *) Chkmem(malloc(sizeof(t)), __FILE__, __LINE__)
#define	 Tcalloc(t)  (t *) Chkmem(calloc(1, sizeof(t)), __FILE__, __LINE__)
#define	 Tnalloc(t,n)   (t *) Chkmem(malloc((n) * sizeof(t)), __FILE__, __LINE__)
#define	 Tncalloc(t,n)  (t *) Chkmem(calloc((size_t)(n), sizeof(t)), __FILE__, __LINE__)
#define  Tnrealloc(t,n,b) (t *) Chkmem(realloc((void*)(b), (n) * sizeof(t)), __FILE__, __LINE__)

#endif

#define	StackAlloc(c)	gStackAlloc(c, alloca(gInstanceSize(c)))

#if 0
#if defined(__TURBOC__)  /*  ||  defined(unix)  */
#include "generics.h"
#else
#include <generics.h>
#endif
#endif


/*  Exception Handling code  */

extern object (*__dynace_signal)(object);

extern object _catchKind(object *, object);

extern	long	_SIG1, _SIG2;

typedef struct HandlerCtx_s {
	long	sig1;
	long	sig2;
	ofun	rstrt;
	object	value;
	jmp_buf unwind;
}	HandlerCtx;

#define withRestartHandling(fn)			\
{						\
    HandlerCtx __ctx;				\
    __ctx.sig1  = _SIG1;			\
    __ctx.sig2  = _SIG2;			\
    __ctx.rstrt = fn;				\
    __ctx.value = Condition;			\
    if (setjmp(__ctx.unwind) == 0) {
    
#define onError					\
    }						\
    else {					\
	__ctx.sig1 = 0;				\
	__ctx.sig2 = 0;

#define endHandling				\
	if (__ctx.value != NULL)		\
	    (*__dynace_signal)(__ctx.value);	\
    }						\
    __ctx.sig1 = 0;				\
    __ctx.sig2 = 0;				\
    __ctx.value = NULL;				\
} (void)0

#define callNextRestart		((ofun)1)
#define catchKind(cls)		_catchKind(&__ctx.value, cls)
#define declineHandling(cond)	(__ctx.value = cond)

#define withHandling		withRestartHandling(NULL)

#define withUnwindProtect	withRestartHandling(callNextRestart)

#define onUnwind				\
	__ctx.value = NULL;			\
    }						\
    {						\
	__ctx.sig1 = 0;				\
	__ctx.sig2 = 0;

#define endUnwind endHandling



/*  from the Stream class  */
extern	object	stdoutStream_o, stdinStream_o, stderrStream_o;
extern	object	traceStream_o;
#define stdoutStream	(stdoutStream_o ? stdoutStream_o : (File_initialize(), stdoutStream_o))
#define stdinStream	(stdinStream_o ? stdinStream_o : (File_initialize(), stdinStream_o))
#define stderrStream	(stderrStream_o ? stderrStream_o : (File_initialize(), stderrStream_o))
#define traceStream	(traceStream_o ? traceStream_o : (File_initialize(), traceStream_o))


/* used by XMLNode  */
#ifndef	ELEMENT_NODE
#define	ELEMENT_NODE			1
#define	ATTRIBUTE_NODE			2
#define	TEXT_NODE			3
#define	CDATA_SECTION_NODE		4
#define	PROCESSING_INSTRUCTION_NODE	7
#define	COMMENT_NODE			8
#define	DOCUMENT_NODE			9
#define	DOCUMENT_TYPE_NODE		10
#endif


extern	object	SchemeClassSurrogate;
extern	object	JavaCallbackClassSurrogate;
extern	object	JavaScriptClassSurrogate;

extern	void	Scheme_init_base(void);
extern	void	Scheme_init_app(void);

extern	void	JavaScript_init_base(void);
extern	void	JavaScript_init_app(void);
 


/*  Used by Hash  */

#define BIG_INT		(sizeof(int) > 2 ? 320000001.0 : 32001.0)


//#if !defined(unix) && !defined(__APPLE__)  &&  !defined(vms) && !defined(__WATCOMC__) && _M_IX86 < 300 && !defined(__MWERKS__)  &&  !defined(__COSMIC__)  &&  !defined(PLAN9)  &&  !defined(__MINGW32__)  &&  !defined(__minix)
//#define _HUGE_	huge
//#else
#define _HUGE_
//#endif

/* macro to make believe a variable is used to keep picky compilers happy  */
#define USE(x)		(void) x

#ifdef	__alpha
#define vPrintf		fix_vPrintf
#define vSprintf	fix_vSprintf
#endif

#ifdef	__WATCOMC__
#define	ASSIGN_VA_LIST(t,f)	memcpy(&t, &f, sizeof t)
#else
#ifdef	__va_copy 
#define	ASSIGN_VA_LIST(t,f)	__va_copy(t, f)
#else 
#define	ASSIGN_VA_LIST(t,f)	t = f
#endif
#endif

#if	DPP_STRATEGY == 1
#define	MAKE_REST(lst)		\
	va_list	_rest_;		\
	va_list _rest2_;	\
	va_start(_rest_, lst);	\
	ASSIGN_VA_LIST(_rest2_, _rest_)
#else
#define	MAKE_REST(lst)		\
	va_list	_rest2_; 	\
	ASSIGN_VA_LIST(_rest2_, _rest_)
#endif

#define	RESET_REST	ASSIGN_VA_LIST(_rest_, _rest2_)

#if	defined(__WATCOMC__)  &&  defined(__cplusplus)
#pragma warning 472 9
#endif

#ifdef	__cplusplus
}
#endif


#endif  /*  _DYNL_H  */





