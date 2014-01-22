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


#ifndef	DYN_SCHEME_H
#define DYN_SCHEME_H

#ifdef NATIVE_THREADS
#define WIN32_THREADS
#endif

#define	INCLUDE_WITHOUT_PATHS
#include "MzScheme/scheme.h"


extern	Scheme_Env	*Scheme_global_env;
extern	Scheme_Type	Scheme_Dynace_Object;
extern	Scheme_Type	Scheme_C_Pointer;

extern	void	Scheme_init(void);
extern	void	Scheme_init_base(void);
extern	void	Scheme_init_app(void);
extern	void	Scheme_init_xtable(void);

extern	Scheme_Object	*scheme_new_dynace_object(object obj);
extern	Scheme_Object	*scheme_new_c_pointer(void *obj);
extern	Scheme_Object	*Scheme_execute_string(char *str);
extern	Scheme_Object	*Scheme_execute_file(char *file);
extern	Scheme_Object	*scheme_make_string2(char *str);
extern	object		Scheme_to_Dynace(Scheme_Object *r);

/* located in scheme-vargs.c  */
extern	Scheme_Object	*handle_vargs_object(int argc, Scheme_Object *argv[], ofun func, char *fun);
extern	Scheme_Object	*handle_vargs_double(int argc, Scheme_Object *argv[], double (*func)(), char *fun);
extern	Scheme_Object	*handle_vargs_integer(int argc, Scheme_Object *argv[], int (*func)(), char *fun);
extern	Scheme_Object	*handle_vargs_c_pointer(int argc, Scheme_Object *argv[], ifun func, char *fun);
extern	Scheme_Object	*handle_vargs_null(int argc, Scheme_Object *argv[], ifun func, char *fun);
extern	Scheme_Object	*handle_vargs_string(int argc, Scheme_Object *argv[], char *(*func)(), char *fun);



#define	ADD_FUN(n, f)	scheme_add_global(#n, scheme_make_prim(_##f), Scheme_global_env)
#define	ADD_OBJ(n)	scheme_add_global(#n, scheme_new_dynace_object(n), Scheme_global_env)


#define	SCHEME_FUNCTION(x)	static Scheme_Object *_##x(int argc, Scheme_Object *argv[])

#define	SCHEME_DYNACEP(x)	(SCHEME_TYPE(x) == Scheme_Dynace_Object)
#define	SCHEME_CPOINTERP(x)	(SCHEME_TYPE(x) == Scheme_C_Pointer)
#define SCHEME_MVP(obj)     	(obj == scheme_multiple_values)


#define	SCHEME_ARG_COUNT(n)	if (argc != n)	scheme_wrong_count(fun, n, n, argc, argv)
#define	SCHEME_ARG_VCOUNT(n)	if (argc < n)	scheme_wrong_count(fun, n, -1, argc, argv)

#define	SCHEME_CHK_ARG_STRING(n)  if (!SCHEME_STRINGP(argv[n]))	scheme_wrong_type(fun, "string", n, argc, argv)
#define	SCHEME_CHK_ARG_DYNACE(n)  if (!SCHEME_DYNACEP(argv[n]))	scheme_wrong_type(fun, "Dynace-object", n, argc, argv)
#define	SCHEME_CHK_ARG_CPOINTER(n)  if (!SCHEME_CPOINTERP(argv[n])) scheme_wrong_type(fun, "C-pointer", n, argc, argv)
#define	SCHEME_CHK_ARG_INT(n)  if (!SCHEME_INTP(argv[n])) scheme_wrong_type(fun, "integer", n, argc, argv)
#define	SCHEME_CHK_ARG_DBL(n)  if (!SCHEME_DBLP(argv[n])  &&  !SCHEME_INTP(argv[n])) scheme_wrong_type(fun, "double", n, argc, argv)
#define	SCHEME_CHK_ARG_CHAR(n)  if (!SCHEME_CHARP(argv[n])) scheme_wrong_type(fun, "character", n, argc, argv)
#define	SCHEME_CHK_ARG_BOOL(n)  if (!SCHEME_BOOLP(argv[n])) scheme_wrong_type(fun, "boolean", n, argc, argv)


#define	SCHEME_DBL_VAL2(x)	(SCHEME_DBLP(x) ? SCHEME_DBL_VAL(x) : (double) SCHEME_INT_VAL(x))
#define SCHEME_BOOL_VAL(x)      (SCHEME_FALSEP(x) ? 0 : 1)


#endif







