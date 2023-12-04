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
#include <ctype.h>
#include <string.h>
#include "dpp.h"

#define MACROS

#define BUFSIZE		256
#define MAXWORDSZ	80

/*  Used for Debugging the Interface between generics and methods  */
/*  #define	DBI  */

#ifdef	unix
#define RMODE	"r"
#define WMODE	"w"
#else
#define RMODE	"rt"
#define WMODE	"wt"
#endif

/*  These are not absolute maximums just 'typical' maximums (for speed vs. memory tradeoffs) */
#define	MAX_CLASSES	401
#define	MAX_GENERICS	3001


#ifdef	SYMC32
#include <dos.h>
EXPAND_WILDCARDS	/*  causes command line wild card expansion  */
#endif

#if 0
static	char	MPREFIX[80] = "m_";
static	char	MSUFFIX[80];

static	char	OLMPREFIX[80] = "v_";
static	char	OLMSUFFIX[80];
#endif


static	int	SeeCode;	/*  read tokens inside braces	*/
static	long	CurrentLine;

	int	ErrorCode = 0;
static	int	InKernel  = 0;
static	int	NoProto   = 0;
static	int	Quiet     = 0;
static	int	GenIncludes = 1;
static	int	MacroGuard  = 0;
static	int	Copyright = 0;
static	int	AllowOverloads = 0;
static	int	Strategy = DPP_STRATEGY;/*  1=assembler, 2=C no assem,
					    3=C++ inline,4=C++ inline except
					    vararg generics  */
static	int	FastWideCache = 0;
static	int	Create_iv = 0;
static	int	Create_cv = 0;
static	int	NoInitialize = 0;
	int	LineDirectives = 1;
static	int	ExtraLineDirectives = 0;
static	int	ForceGenerate = 0;
static	int	MultipleGenerics = 0;
static	char	Sfile2[128];  /*  split .c file file name  */
static	object	NoValue;

objrtn	open_file(char *file, char *mode, int real_quiet);
char	*trunc_mname(char *mname);

static	void	add_generic(object generics, object gn, object proto, int check);
static	int need_to_make(object classes,object generics,char *file);
static	void make_h(object classes,object generics,char *file, object argList, object inc_sh, object inc_ah);
static	void make_libscheme(object classes,object generics,char *file, object argList, object inc_sh, object inc_ah);
static	void make_mzscheme(object classes,object generics,char *file, object argList, object inc_sh, object inc_ah);
static	void make_javascript(object classes,object generics,char *file, object argList, object inc_sh, object inc_ah);
static	void make_java_generic_impl(object classes,object generics,char *package,object argList, object inc_sh, object inc_ah);
static	void make_java_class_ref(object classes,object generics,char *package,object argList, object inc_sh, object inc_ah);
static	void make_java_c(object classes,object generics,char *package,object argList, object inc_sh, object inc_ah);
static	void make_mzscheme_macros(object generics, char *file);
static	void output_scheme_macro(object fobj, char *name, object proto);
static	void make_c(object classes,object generics,char *file, object inc_sc, object inc_ac);
static	void make_c2(object generics,char *file, int skip, object inc_sc, object inc_ac);
static	void read_gens(object classes,object generics,char *file,object exceptions, int errorOK);
static	void proc_gen_class(object classes,object fobj);
static	void proc_gen_generic(object generics,object fobj,object exceptions);
static	void read_docs(object classes,object generics,char *file);
static	void proc_doc_cls(object classes,char *buf,char *word,object fobj);
static	void proc_doc_gen(object generics,char *buf,char *word,object fobj);
static	void process_line(char *to,char *from);
static	void read_exceptions(object exceptions,char *file);
static	void read_src(object classes,object generics,char *file,object exceptions);
static	void src_class(object fobj,object classes, char *initmeth);
static	int make_arg(object fobj,object args,int *old, int *vararg);
static	int pp_make_arg(object fobj,object args,int *old, object ivars, object cvars, int *convert);
static	objrtn make_arg_list(object fobj,int *old, int *vararg);
static	objrtn pp_make_arg_list(object fobj,int *old, object ivars, object cvars, int convert);
static	void src_method(object fobj,object methods,int vararg);
static	void pp_method(object tkn, object fobj,object methods,object sgenerics, object sobj,char *mtype, int mclass, int vararg, object ivars, object cvars, object className);
static	void pp_local_method(object  tkn, object fobj, object sobj, char *mtype, int mclass, object  ivars, object  cvars);
static	void src_generic(object fobj,object sgenerics);
static	void src_generic2(object fobj,object sgenerics);
static	void addto_generics(object smethods,object sgenerics,object generics,object exceptions);
static	void preprocess(object classes,object generics,char *file,object exceptions, int mkc);
static	int valid_symbol(char *tkn);
static	void pp_defclass(object tkn, object fobj,object className,object superClasses,object idecls,object cdecls,object ivars,object cvars,object init, object classes);
static	void pp_decl_section(object tkn, char *p,object fobj,object idecls,object ivars, object ovars);
static	int get_var(char *var,char *buf);
/*static	void add_underscore(char *buf);*/
static	void pp_init_section(object tkn, char *p,object fobj,object init);
static	char *getword(char *s,char *w,char c);
static	int get_line(object fobj,char *line);
static	char *gettoken(object fobj);
static	char *gettype(char *s,char *w);
static	void compress(char *f);
static  void pp_gen_head(object sobj, object className,object idecls,object cdecls,object ivars,object cvars, char *file);
static	void pp_gen_initmethod(object  sobj,object superClasses,object  init,object  isgenerics,object  csgenerics,object ismethods, object csmethods, object  ivars,object  cvars, object className, int sup);
static	void pp_body(char *p, object fobj,object sobj,int mclass, object tkn, int convert, object prototype);
static  int touch(char *f);
static	void	touch_all(object argList);
static	void	copyright(object fobj);
static	void	schemecopyright(object fobj);
static	char	*file_extension(char *file);
static	void	strip_extension(char *file);
static	char	*new_ext(char *file, char *ext);
static	int	class_source(char *p);
static	void	pp_gen_iv(char *file,object className,object idecls);
static	void	pp_gen_cv(char *file,object className,object cdecls);
static	void	removeObj(object classes, object generics, char *sym);
static	void	print_help(object stream);
static	int	isGUID(object proto);


int	main(int argc, char **argv)
{
	object	classes;	/*  classes			*/
	object	generics;	/*  generic + prototype		*/
	object	exceptions;	/*  generics with no arg checking   */
	object	argList;	/*  list of command line arguments  */
	object	inc_sc=NULL;	/*  system files to include in generics.c  */
	object	inc_ac=NULL;	/*  app files to include in generics.c  */
	object	inc_sh=NULL;	/*  system files to include in generics.h  */
	object	inc_ah=NULL;	/*  app files to include in generics.h  */
	object	seq, arg;
	char	*p;
	int	g1, c1, g2, c2;
	int	zero = 0, ignore = 0;

	InitDynace(&argc);

#ifdef	USE_GC
	gSetMemoryBufferArea(Dynace, 100000L);
#endif

/*	gObjectChecking(Dynace, 0);   */

	if (argc < 2)  {
		print_help(stdoutStream);
		exit(-1);
	}

	NoValue = gNew(Constant);
	argList = gNewArglist(ArgumentList, argc, argv);
	gDeepDisposeFirst(argList);  /*  don't need command name  */

	classes     = gNew(BTree);
	generics    = gNew(BTree);
	exceptions  = gNewWithInt(Set, MAX_CLASSES);

	for (seq=gSequence(argList), arg=gNext(seq) ; arg ; arg = gNext(seq)) {
		p = gStringValue(arg);
		if (*p == '-')
			if (streq(p+1, "C"))
				Copyright = 1;
			else if (streq(p+1, "k"))
				InKernel = 1;
			else if (streq(p+1, "N"))
				NoProto = 1;
			else if (streq(p+1, "i"))
				ignore = 1;
			else if (streq(p+1, "q"))
				Quiet = 1;
			else if (streq(p+1, "z"))
				zero = 1;
			else if (streq(p+1, "X"))
				AllowOverloads = 1;
			else if (streq(p+1, "nai"))
				GenIncludes = 0;
			else if (streq(p+1, "mg"))
				MacroGuard = 1;
			else if (streq(p+1, "F"))
				FastWideCache = 1;
			else if (p[1] == 'S')
				Strategy = atoi(p+2);
			else if (p[1] == 'M')
				MultipleGenerics = atoi(p+2);
			else if (streq(p+1, "iv"))
				Create_iv = 1;
			else if (streq(p+1, "cv"))
				Create_cv = 1;
			else if (streq(p+1, "ni"))
				NoInitialize = 1;
			else if (streq(p+1, "nld"))
				LineDirectives = 0;
			else if (streq(p+1, "eld"))
				ExtraLineDirectives = 1;
			else if (streq(p+1, "f"))
				ForceGenerate = 1;
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-e"))
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				read_exceptions(exceptions, p);
			}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  (streq(p, "-g")  ||  streq(p, "-G")))  {
			int	errorOK;
			errorOK = p[1] == 'G';
			if (arg = gNext(seq))
				p = gStringValue(arg);
			else
				p = "-";
			if (*p == '-')
				read_gens(classes, generics, "generics.h", exceptions, errorOK);
			for ( ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				read_gens(classes, generics, p, exceptions, errorOK);
			}
		}
	}

	g1 = gSize(generics);
	c1 = gSize(classes);

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-d"))
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				read_docs(classes, generics, p);
			}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-s"))
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				if (class_source(p))
					preprocess(classes, generics, p, exceptions, 0);
				else
					read_src(classes, generics, p, exceptions);
			}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-p")) {
			while (arg  &&  streq(p, "-p"))
				for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
					p = gStringValue(arg);
					if (*p == '-')
						break;
					preprocess(classes, generics, p, exceptions, 1);
					*Sfile2 = '\0';
				}
		} else if (streq(p, "-64")) {
			arg = gNext(seq);
			*Sfile2 = '\0';
			if (arg) {	
				p = gStringValue(arg);
				if (*p != '-')
					strcpy(Sfile2, p);
			}
		}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-r"))
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				removeObj(classes, generics, p);
			}
	}

	g2 = gSize(generics);
	c2 = gSize(classes);

	if (ErrorCode  &&  !ignore)
		goto end;

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-Isc"))  {
			if (!inc_sc)
				inc_sc = gNew(LinkObject);
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				gAddLast(inc_sc, gNewWithStr(String, p));
			}
		}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-Iac"))  {
			if (!inc_ac)
				inc_ac = gNew(LinkObject);
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				gAddLast(inc_ac, gNewWithStr(String, p));
			}
		}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-Ish"))  {
			if (!inc_sh)
				inc_sh = gNew(LinkObject);
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				gAddLast(inc_sh, gNewWithStr(String, p));
			}
		}
	}

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-Iah"))  {
			if (!inc_ah)
				inc_ah = gNew(LinkObject);
			for (arg=gNext(seq) ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				gAddLast(inc_ah, gNewWithStr(String, p));
			}
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-h"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-')
				make_h(classes, generics, p, argList, inc_sh, inc_ah);
			else
				make_h(classes, generics, "generics.h", argList, inc_sh, inc_ah);
			if (arg)
				seq = DISPOSE(seq);
			break;
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-L1"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-')
				make_libscheme(classes, generics, p, argList, inc_sh, inc_ah);
			else
				make_libscheme(classes, generics, "scminter.c", argList, inc_sh, inc_ah);
			if (arg)
				seq = DISPOSE(seq);
			break;
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-L2"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-')
				make_mzscheme(classes, generics, p, argList, inc_sh, inc_ah);
			else
				make_mzscheme(classes, generics, "scminter.c", argList, inc_sh, inc_ah);
			if (arg)
				seq = DISPOSE(seq);
			break;
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-j"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-') {
				make_java_generic_impl(classes, generics, p, argList, inc_sh, inc_ah);
				make_java_class_ref(classes, generics, p, argList, inc_sh, inc_ah);
				make_java_c(classes, generics, p, argList, inc_sh, inc_ah);
			} else {
				make_java_generic_impl(classes, generics, NULL, argList, inc_sh, inc_ah);
				make_java_class_ref(classes, generics, NULL, argList, inc_sh, inc_ah);
				make_java_c(classes, generics, NULL, argList, inc_sh, inc_ah);
			}
			break;
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-js"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-')
				make_javascript(classes, generics, p, argList, inc_sh, inc_ah);
			else
				make_javascript(classes, generics, "jsinter.c", argList, inc_sh, inc_ah);
			if (arg)
				seq = DISPOSE(seq);
			break;
		}
	}

	for (seq=gSequence(argList) ; arg=gNext(seq) ; )  {
		p = gStringValue(arg);
		if (streq(p, "-c"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			if (arg  &&  *p != '-')
				make_c(classes, generics, p, inc_sc, inc_ac);
			else
				make_c(classes, generics, "generics.c", inc_sc, inc_ac);
			if (MultipleGenerics) {
				int	n, i=1;
				char	file[20];
				for (n=MultipleGenerics ;  gSize(generics) > n ; n += MultipleGenerics) {
					sprintf(file, "gens%d.c", i++);
					make_c2(generics, file, n, inc_sc, inc_ac);
				}
			}
			if (arg)
				seq = DISPOSE(seq);
			break;
		}
	}


/*
	gPrint(exceptions, stdoutStream);
	gPrint(classes, stdoutStream);
	gPrint(generics, stdoutStream);
*/

 end:
	argList = DEEPDISPOSE(argList);
	classes = DEEPDISPOSE(classes);
	generics = DEEPDISPOSE(generics);
	exceptions = DEEPDISPOSE(exceptions);
	if (inc_sc)
		inc_sc = DEEPDISPOSE(inc_sc);
	if (inc_ac)
		inc_ac = DEEPDISPOSE(inc_ac);
	if (inc_sh)
		inc_sh = DEEPDISPOSE(inc_sh);
	if (inc_ah)
		inc_ah = DEEPDISPOSE(inc_ah);
#ifdef	USE_GC
	gGC(Dynace);
#endif
	if (!Quiet  &&  (!ErrorCode  ||  ignore))  {
		vPrintf(stdoutStream, "\nClasses  = %d, %d, %d\n", c1, c2-c1, c2);
		vPrintf(stdoutStream, "Generics = %d, %d, %d\n", g1, g2-g1, g2);
#ifdef	USE_GC
		vPrintf(stdoutStream, "Memory = %ld, %ld, %ld\n",
			gMaxMemUsed(Dynace), gCurMemUsed(Dynace), gMaxAfterGC(Dynace));
#endif
	}
#ifdef	PLAN9
	exit(zero ? 0 : ErrorCode);
#endif
	return zero ? 0 : ErrorCode;
}

static	void	removeObj(object classes, object generics, char *sym)
{
	object	s = gNewWithStr(String, sym);
	gDeepDisposeObj(classes, s);
	gDeepDisposeObj(generics, s);
	DISPOSE(s);
}

objrtn	open_file(char *file, char *mode, int real_quiet)
{
	object	fobj;

	fobj = gOpenFile(File, file, mode);
	if (fobj)  {
		if (!Quiet  &&  !real_quiet)
			vPrintf(stdoutStream, "Now %s %s\n",
				streq(mode, RMODE) ? "reading" : "writing",
				file);
	}  else 
		if (!real_quiet) {
			ErrorCode = 1;
			vPrintf(stdoutStream, "Can't open %s for %s\n", file, 
				streq(mode, RMODE) ? "reading" : "writing");
		}
	return fobj;
}

static	int
need_to_make(object classes,
	     object generics,
	     char   *file)
{
	object	cls, gens, gobj, key, val;
	int	diff = 0;

	cls  = gNew(BTree);
	gens = gNew(BTree);

	read_gens(cls, gens, file, NULL, 2);

	if (gSize(cls) != gSize(classes)  ||  gSize(gens) != gSize(generics))
		diff = 1;
	if (!diff)
		for (val=gFindFirst(cls, &key) ; val ; val=gFindNext(cls, &key))
			if (!gFindEQ(classes, key, NULL))  {
				diff = 1;
				break;
			}
	if (!diff)
		for (val=gFindFirst(gens, &key) ; val ; val=gFindNext(gens, &key))
			if (!(gobj=gFindEQ(generics, key, NULL))  ||
			    !gMatchNoError(val, gobj))  {
				diff = 1;
				break;
			}
			
	DEEPDISPOSE(cls);
	DEEPDISPOSE(gens);
	if (!diff  &&  !Quiet)
		vPrintf(stdoutStream, "No need to make %s\n", file);
	return diff;
}

static	void	touch_all(object argList)
{
	object	seq, arg;
	char	*p;

	for (arg=seq=gSequence(argList) ; arg  &&  (arg=gNext(seq)) ; )  {
		p = gStringValue(arg);
		while (arg  &&  streq(p, "-t"))  {
			if (arg = gNext(seq))
				p = gStringValue(arg);
			else
				p = "-";
			if (*p == '-')  {
				touch("generics.h");
				touch("generics.c");
				touch("generics.o");
				touch("generics.obj");
			}
			for ( ; arg ; arg=gNext(seq))  {
				p = gStringValue(arg);
				if (*p == '-')
					break;
				touch(p);
			}
		}
	}
}

static	void	make_macro_name(char *to, char *from)
{
	char	*f;

	for (f=from ; *f ; f++)
		if (*f == '/'  ||  *f == '\\')
			from = f + 1;
	     
	*to++ = '_';
	for ( ; *from ; ++from)
		if (isalnum(*from))
			if (islower(*from))
				*to++ = toupper(*from);
			else
				*to++ = *from;
		else
			*to++ = '_';
	*to = '\0';
}

static	char	*file_extension(char *file)
{
	char	*r = "";

	while (*file)
		if (*file++ == '.')
			r = file;
	return r;
}

static	void	strip_extension(char *file)
{
	char	*r;

	for (r=NULL ; *file ; ++file)
		if (*file == '.')
			r = file;
		else if (*file == '/'  ||  *file == '\\')
			r = NULL;
	if (r)
		*r = '\0';
}

static	char	*new_ext(char *file, char *ext)
{
	static	char	fname[80];
	strcpy(fname, file);
	strip_extension(fname);
#if	!defined(unix)  &&  0
	{
		char	*p;
		for (p=fname ; *p ; p++)
			*p = tolower(*p);
	}
#endif
	strcat(fname, ".");
	strcat(fname, ext);
	return fname;
}

static	void	gen_inline(object fobj, char *name, object proto)
{
	char	*rtn;

	rtn = gStringValue(gReturnType(proto));
	vPrintf(fobj, "inline\t%s\t%s(", rtn, name);
	gPrintArgs(proto, fobj);
	gPuts(fobj, ")\n{\n");
	if (FastWideCache)
		gPuts(fobj, "\tofun _meth_;\n");
	if (gIsVarArg(proto))
		if (streq(rtn, "void"))
			vPrintf(fobj, "\tva_list _rest_;\n\tva_start(_rest_, %s);\n", gLastArg(proto));
		else
			vPrintf(fobj, "\tva_list _rest_;\n\t%s _ret_;\n\tva_start(_rest_, %s);\n", rtn, gLastArg(proto));
	if (FastWideCache)  {
		vPrintf(fobj, "\tFW_GENERIC(%s);\n", name);
		vPrintf(fobj, "\t%s (*(%s_t)_meth_)(",
			streq(rtn, "void")?"":"return", name);
	} else {
		if (streq(rtn, "void"))
			vPrintf(fobj, "\t(*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
		else if (gIsVarArg(proto))
			vPrintf(fobj, "\t_ret_ = (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
		else
			vPrintf(fobj, "\treturn (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
	}
	gPrintVars(proto, fobj);
	gPuts(fobj, ");\n");
	if (gIsVarArg(proto)) {
		vPrintf(fobj, "\tva_end(_rest_);\n");
		if (!streq(rtn, "void"))
			vPrintf(fobj, "\treturn _ret_;\n");
	}
	gPuts(fobj, "}\n\n");
}

static	int	is_pointer(char *x)
{
	while (*x)
		if (*x++ == '*')
			return 1;
	return 0;
}

static	void
make_libscheme(object	classes,
	       object	generics,
	       char	*file,
	       object	argList,
	       object	inc_sh,
	       object	inc_ah)
{
	object	fobj, seq, obj, proto, key, val;
	char	*name, *rtn;
	
	rtn = file_extension(file);
	if (strne(rtn, "c")  &&
	    strne(rtn, "C"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated generic header file names must be valid C or C++ file names.\n");
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);

	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	gPuts(fobj, "#include \"generics.h\"\n");
	gPuts(fobj, "#include \"scheme.h\"\n");
	gPuts(fobj, "\n");

	gPuts(fobj, "static\tScheme_Env\t*global_env;\n");
	gPuts(fobj, "static\tScheme_Object\t*Dynace_Object;\n");
	gPuts(fobj, "static\tScheme_Object\t*C_Pointer;\n\n");

	gPuts(fobj, "#define\tSCHEME_FUNCTION(x)\tstatic Scheme_Object *x(int argc, Scheme_Object *argv[])\n");
	gPuts(fobj, "#define\tSCHEME_DYNACEP(x)\t(SCHEME_TYPE(x) == Dynace_Object)\n");
	gPuts(fobj, "#define\tSCHEME_CPOINTERP(x)\t(SCHEME_TYPE(x) == C_Pointer)\n\n");

	gPuts(fobj, "#define\tADD_FUN(n, f)\tscheme_add_global(#n, scheme_make_prim(f), global_env)\n");
	gPuts(fobj, "#define\tADD_OBJ(n, f)\tscheme_add_global(#n, new_dynace_object(f), global_env)\n");

	gPuts(fobj, "\n\nstatic\tScheme_Object\t*new_dynace_object(object obj)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tScheme_Object\t*so;\n\n");
#if 0
	gPuts(fobj, "\tif (!obj)\n");
	gPuts(fobj, "\t\treturn scheme_null;\n");
#endif
	gPuts(fobj, "\tso = scheme_alloc_object();\n");
	gPuts(fobj, "\tSCHEME_TYPE(so) = Dynace_Object;\n");
	gPuts(fobj, "\tSCHEME_PTR_VAL(so) = obj;\n");
	gPuts(fobj, "\treturn so;\n");
	gPuts(fobj, "}\n\n\n");

	gPuts(fobj, "\n\nstatic\tScheme_Object\t*scheme_c_pointer(void *obj)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tScheme_Object\t*so;\n\n");
#if 0
	gPuts(fobj, "\tif (!obj)\n");
	gPuts(fobj, "\t\treturn scheme_null;\n");
#endif
	gPuts(fobj, "\tso = scheme_alloc_object();\n");
	gPuts(fobj, "\tSCHEME_TYPE(so) = C_Pointer;\n");
	gPuts(fobj, "\tSCHEME_PTR_VAL(so) = obj;\n");
	gPuts(fobj, "\treturn so;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "static\tchar\tErrbuf[128];\n\n");

	gPuts(fobj, "static\tchar\t*wrong_number_of_args(char *fun)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tsprintf(Errbuf, \"%s: wrong number of arguments\", fun);\n");
	gPuts(fobj, "\treturn Errbuf;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "static\tchar\t*wrong_arg1(char *fun)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tsprintf(Errbuf, \"%s: argument 1 must be an object\", fun);\n");
	gPuts(fobj, "\treturn Errbuf;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "static\tchar\t*wrong_argn(char *fun, int argn)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tsprintf(Errbuf, \"%s: argument %d wrong type\", fun, argn);\n");
	gPuts(fobj, "\treturn Errbuf;\n");
	gPuts(fobj, "}\n\n");

	if (inc_sh)  {
		for (seq = gSequence(inc_sh) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (inc_ah)  {
		for (seq = gSequence(inc_ah) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		int	argc, i;
		object	seq, to;
		char	*type;

		if (gIsVarArg(proto))
			continue;
		name = gStringValue(key);
		rtn = gStringValue(gReturnType(proto));
		if (streq(rtn, "objrtn"))
			rtn = "object";
		else if (streq(rtn, "void"))
			rtn = NULL;
		vPrintf(fobj, "SCHEME_FUNCTION(%s_s)\n{\n", name);
		argc = gSize(gParameters(proto));
		vPrintf(fobj, "\tstatic\tchar\tfun[] = \"%s\";\n", name);
		if (rtn)
			vPrintf(fobj, "\t%s\tr;\n\n", rtn);
		if (!gIsVarArg(proto))
			vPrintf(fobj, "\tSCHEME_ASSERT(argc == %d, wrong_number_of_args(fun));\n", argc);
		else
			vPrintf(fobj, "\tSCHEME_ASSERT(argc >= 1, wrong_number_of_args(fun));\n");
		vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_DYNACEP(argv[0]), wrong_arg1(fun));\n");
		for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
			type = gStringValue(to);
			if (streq(type, "object"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_DYNACEP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else if (streq(type, "char *"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_STRINGP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else if (is_pointer(type)  ||
				 streq(type, "ofun")  ||
				 streq(type, "ifun")  ||
				 streq(type, "REFIID")  ||
				 streq(type, "WNDPROC")  ||
				 streq(type, "REFCLSID")  ||
				 streq(type, "HSTMT")  ||
				 streq(type, "HDBC")  ||
				 streq(type, "HENV")  ||
				 streq(type, "va_list")  ||
				 streq(type, "FARPROC"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_CPOINTERP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
				 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
				 streq(type, "HWND")  ||  streq(type, "HANDLE")  ||  streq(type, "SWORD")  ||
				 streq(type, "UDWORD") || streq(type, "DWORD") || streq(type, "SDWORD"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_INTP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else if (streq(type, "double")  ||  streq(type, "float"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_DBLP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else if (streq(type, "char"))
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_CHARP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
			else
				vPrintf(fobj, "\tSCHEME_ASSERT(SCHEME_INTP(argv[%d]), wrong_argn(fun, %d));\n", i, i+1);
/*				vPrintf(fobj, "\tSCHEME_ASSERT(%s..., wrong_argn(fun, %d));\n", type, i+1);  */
		}

		if (rtn)
			gPuts(fobj, "\tr = ");
		else
			gPuts(fobj, "\t");

		vPrintf(fobj, "%s(SCHEME_PTR_VAL(argv[0])", name);
		for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
			type = gStringValue(to);
			if (streq(type, "char *"))
				vPrintf(fobj, ",\n\t\t\tSCHEME_STR_VAL(argv[%d])", i);
			else if (streq(type, "object")  ||
				 is_pointer(type)  ||
				 streq(type, "ofun")  ||
				 streq(type, "ifun")  ||
				 streq(type, "REFIID")  ||
				 streq(type, "WNDPROC")  ||
				 streq(type, "REFCLSID")  ||
				 streq(type, "HSTMT")  ||
				 streq(type, "HDBC")  ||
				 streq(type, "HENV")  ||
				 streq(type, "va_list")  ||
				 streq(type, "FARPROC"))
				vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_PTR_VAL(argv[%d])", type, i);
			else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
				 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
				 streq(type, "HWND")  ||  streq(type, "HANDLE")  ||  streq(type, "SWORD")  ||
				 streq(type, "UDWORD") || streq(type, "DWORD") || streq(type, "SDWORD"))
				vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_INT_VAL(argv[%d])", type, i);
			else if (streq(type, "double")  ||  streq(type, "float"))
				vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_DBL_VAL(argv[%d])", type, i);
			else if (streq(type, "char"))
				vPrintf(fobj, ",\n\t\t\tSCHEME_CHAR_VAL(argv[%d])", i);
			else
				vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_INT_VAL(argv[%d])", type, i);
/*				vPrintf(fobj, ",\n\t\t\t...argv[%d]", i);  */
		}
		gPuts(fobj, ");\n");

		if (!rtn)
			gPuts(fobj, "\treturn scheme_null;\n");
		else if (streq(rtn, "object"))
			gPuts(fobj, "\treturn new_dynace_object(r);\n");
		else if (streq(rtn, "int")  ||  streq(rtn, "short")  ||  streq(rtn, "unsigned")  ||  streq(rtn, "long")  ||
			 streq(rtn, "unsigned short")  ||  streq(rtn, "LRESULT"))
			gPuts(fobj, "\treturn scheme_make_integer((int)r);\n");
		else if (streq(rtn, "char *"))
			gPuts(fobj, "\treturn scheme_make_string2(r);\n");
		else if (streq(rtn, "char"))
			gPuts(fobj, "\treturn scheme_make_char(r);\n");
		else if (streq(rtn, "double")  ||  streq(rtn, "float"))
			gPuts(fobj, "\treturn scheme_make_double(r);\n");
		else if (is_pointer(rtn)  ||
			 streq(rtn, "ofun")  ||
			 streq(rtn, "ifun")  ||
			 streq(rtn, "WNDPROC")  ||
			 streq(rtn, "REFCLSID")  ||
			 streq(rtn, "HSTMT")  ||
			 streq(rtn, "HDBC")  ||
			 streq(rtn, "HENV")  ||
			 streq(rtn, "va_list")  ||
			 streq(rtn, "FARPROC"))
			gPuts(fobj, "\treturn scheme_c_pointer(r);\n");
		else 
			gPuts(fobj, "\treturn scheme_make_integer((int)r);\n");
/*			vPrintf(fobj, "\treturn ...;\n");   */
		gPuts(fobj, "}\n\n");
	}

	gPuts(fobj, "SCHEME_FUNCTION(int_to_object)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tstatic\tchar\tfun[] = \"int-to-object\";\n\n");
	gPuts(fobj, "\tSCHEME_ASSERT(argc == 1, wrong_number_of_args(fun));\n");
	gPuts(fobj, "\tSCHEME_ASSERT(SCHEME_INTP(argv[0]), wrong_argn(fun, 1));\n");
	gPuts(fobj, "\treturn new_dynace_object((object) SCHEME_INT_VAL(argv[0]));\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "SCHEME_FUNCTION(object_to_pointer)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tstatic\tchar\tfun[] = \"object-to-pointer\";\n\n");
	gPuts(fobj, "\tSCHEME_ASSERT(argc == 1, wrong_number_of_args(fun));\n");
	gPuts(fobj, "\tSCHEME_ASSERT(SCHEME_DYNACEP(argv[0]), wrong_argn(fun, 1));\n");
	gPuts(fobj, "\treturn scheme_c_pointer((void *) SCHEME_PTR_VAL(argv[0]));\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "SCHEME_FUNCTION(pointers_equal)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tstatic\tchar\tfun[] = \"pointers-equal?\";\n\n");
	gPuts(fobj, "\tSCHEME_ASSERT(argc == 2, wrong_number_of_args(fun));\n");
	gPuts(fobj, "\tSCHEME_ASSERT(SCHEME_DYNACEP(argv[0])  ||  SCHEME_CPOINTERP(argv[0]), wrong_argn(fun, 1));\n");
	gPuts(fobj, "\tSCHEME_ASSERT(SCHEME_DYNACEP(argv[1])  ||  SCHEME_CPOINTERP(argv[1]), wrong_argn(fun, 2));\n");
	gPuts(fobj, "\treturn SCHEME_PTR_VAL(argv[0]) == SCHEME_PTR_VAL(argv[1]) ? scheme_true : scheme_false;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "void\tScheme_init()\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tstatic\tint\tonce = 1;\n");
	gPuts(fobj, "\tScheme_Object\t*t;\n\n");

	gPuts(fobj, "\tif (global_env)\n");
	gPuts(fobj, "\t\treturn;\n");
	gPuts(fobj, "\tif (once) {\n");
	gPuts(fobj, "\t\tonce = 0;\n");
	gPuts(fobj, "\t\tDynace_Object = scheme_make_type(\"<Dynace-object>\");\n");
	gPuts(fobj, "\t\tC_Pointer = scheme_make_type(\"<C-Pointer>\");\n");
	gPuts(fobj, "\t}\n");
	gPuts(fobj, "\tglobal_env = scheme_basic_env();\n");
	gPuts(fobj, "\tscheme_add_global(\"nil\", scheme_null, global_env);\n\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);
		vPrintf(fobj, "\tADD_OBJ(%s, %s);\n", name, name);
	}

	gPuts(fobj, "\n");
	gPuts(fobj, "\tADD_FUN(int-to-object, int_to_object);\n");
	gPuts(fobj, "\tADD_FUN(object-to-pointer, object_to_pointer);\n");
	gPuts(fobj, "\tADD_FUN(pointers-equal?, pointers_equal);\n");
	gPuts(fobj, "\n");

	for (val=gFindFirst(generics, &key) ; val ; val=gFindNext(generics, &key)) {
		if (gIsVarArg(val))
			continue;
		name = gStringValue(key);
		vPrintf(fobj, "\tADD_FUN(%s, %s_s);\n", name, name);
	}

	gPuts(fobj, "}\n\n");

	gPuts(fobj, "void\tScheme_reset()\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tglobal_env = NULL;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "Scheme_Object\t*Scheme_execute_string(char *str)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tScheme_Object\t*in_port, *obj, *ret=NULL;\n");
	gPuts(fobj, "\tFILE\t\t*fp;\n");
	gPuts(fobj, "\textern\tchar\tscheme_error_message[];\n\n");
	gPuts(fobj, "\tScheme_init();\n");
	gPuts(fobj, "\tin_port = scheme_make_string_input_port(str);\n");
	gPuts(fobj, "\twhile ((obj = scheme_read(in_port)) != scheme_eof) {\n");
	gPuts(fobj, "\t\tret = SCHEME_CATCH_ERROR(scheme_eval(obj, global_env), 0);\n");
	gPuts(fobj, "\t\tif (!ret)\n");
	gPuts(fobj, "\t\t\tgError(Application, scheme_error_message);\n");
	gPuts(fobj, "\t}\n");
	gPuts(fobj, "\tscheme_close_input_port(in_port);\n");
	gPuts(fobj, "\treturn ret;\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "Scheme_Object\t*Scheme_execute_file(char *file)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tchar\tbuf[128];\n\n");
	gPuts(fobj, "\tsprintf(buf, \"(load \\\"%s\\\")\", file);\n");
	gPuts(fobj, "\treturn Scheme_execute_string(buf);\n");
	gPuts(fobj, "}\n\n");

	gPuts(fobj, "object\tScheme_to_Dynace(Scheme_Object *r)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tif (SCHEME_CHARP(r))\n");
	gPuts(fobj, "\t\treturn gNewWithChar(Character, SCHEME_CHAR_VAL(r));\n");
	gPuts(fobj, "\telse if (SCHEME_INTP(r))\n");
	gPuts(fobj, "\t\treturn gNewWithLong(LongInteger, SCHEME_INT_VAL(r));\n");
	gPuts(fobj, "\telse if (SCHEME_DBLP(r))\n");
	gPuts(fobj, "\t\treturn gNewWithDouble(DoubleFloat, SCHEME_DBL_VAL(r));\n");
	gPuts(fobj, "\telse if (SCHEME_STRINGP(r))\n");
	gPuts(fobj, "\t\treturn gNewWithStr(String, SCHEME_STR_VAL(r));\n");
	gPuts(fobj, "\telse if (SCHEME_NULLP(r))\n");
	gPuts(fobj, "\t\treturn NULL;\n");
	gPuts(fobj, "\telse if (SCHEME_DYNACEP(r))\n");
	gPuts(fobj, "\t\treturn (object) SCHEME_PTR_VAL(r);\n");
	gPuts(fobj, "\telse if (SCHEME_CPOINTERP(r))\n");
	gPuts(fobj, "\t\treturn (object) SCHEME_PTR_VAL(r);\n");
	gPuts(fobj, "\treturn NULL;\n");
	gPuts(fobj, "}\n");

	gPuts(fobj, "\n\n\n");

	gDispose(fobj);
}

static char *get_java_type(char *type)
{
	if (streq(type, "object"))
		return "DynaceObject";
	else if (streq(type, "char *"))
		return "String";
	else if  (is_pointer(type)  || streq(type, "ofun")  ||
		  streq(type, "ifun")  ||
		  streq(type, "REFIID")  ||
		  streq(type, "WNDPROC")  ||
		  streq(type, "PRECT")  ||
		  streq(type, "BOOL")  ||
		  streq(type, "LONG")  ||
		  streq(type, "REFCLSID")  ||
		  streq(type, "COLORREF")  ||
		  streq(type, "HSTMT")  ||
		  streq(type, "HDBC")  ||
		  streq(type, "HDC")  ||
		  streq(type, "HENV")  ||
		  streq(type, "FARPROC") ||
		  streq(type, "WPARAM") ||
		  streq(type, "LPARAM") ||
		  streq(type, "LPCSTR") ||
		  streq(type, "LPSTR") ||
		  streq(type, "HINSTANCE") ||
		  streq(type, "HBRUSH") ||
		  streq(type, "HANDLE") ||
		  streq(type, "HBITMAP") ||
		  streq(type, "HGLOBAL") ||
		  streq(type, "const int") ||
		  streq(type, "int")  ||  streq(type, "unsigned")  ||
		  streq(type, "unsigned int")  ||  streq(type, "unsigned long")  ||
		  streq(type, "long") ||  streq(type, "LRESULT")  ||
		  streq(type, "HWND")  ||  streq(type, "HANDLE")  ||
		  streq(type, "UDWORD") || streq(type, "DWORD") || streq(type, "UINT") || streq(type, "SDWORD"))
		return "int";
	else if (streq(type, "short")  ||  streq(type, "SWORD") || streq(type, "unsigned short")
		 || streq(type, "WORD") || streq(type, "short int"))
		return "short";
	else if (streq(type, "double"))
		return "double";
	else if (streq(type, "float"))
		return "float";
	else if (streq(type, "char"))
		return "byte";
	else if (streq(type, "void"))
		return "void";
	else
		return type;
}

static	object	make_java_generic_links(object generics)
{
	object	links, key, val;
	int	gnum = 0;

	links = gNewWithInt(StringDictionary, MAX_GENERICS);
	for (val=gFindFirst(generics, &key) ; val ; val=gFindNext(generics, &key), gnum++)
		gAddStr(links, gStringValue(key), gNewWithLong(LongInteger, gnum));
	return links;
}

static	void
make_java_generic_impl(object	classes,
		       object	generics,
		       char	*package,
		       object	argList,
		       object	inc_sh,
		       object	inc_ah)
{
	object	fobj, seq, obj, proto, links, key;
	char	*name, endfunc[26];
	
	if (NULL == (fobj = open_file("DynaceObject.java", WMODE, 0)))
		return;

	links = make_java_generic_links(generics);

	if (Copyright)
		copyright(fobj);

	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	if (package && *package)
		vPrintf(fobj, "package %s;\n\n", package);
	
	gPuts(fobj, "import Dynace.*;\n");
	gPuts(fobj, "\n");

	gPuts(fobj, "public class DynaceObject extends DynaceClass implements IObjectFactory {\n\n");

	gPuts(fobj, "\tpublic static DynaceObject NullDynObj;\n\n");
	gPuts(fobj, "\tstatic {\n");
        gPuts(fobj, "\t\tDynaceBase.setObjectFactory((IObjectFactory)new DynaceObject());\n");
	gPuts(fobj, "\t\tNullDynObj = (DynaceObject)DynaceObject.attachDynaceBase(\"null-object\", 0);\n");
	gPuts(fobj, "\t}\n\n");
        gPuts(fobj, "\tpublic DynaceObject(){}\n\n");
        gPuts(fobj, "\tprotected DynaceObject(String szClassName, int nAddress) {\n");
        gPuts(fobj, "\t\tsuper(szClassName, nAddress);\n");
        gPuts(fobj, "\t}\n\n");
	gPuts(fobj, "\tpublic DynaceObject(DynaceObject inObj) {\n");
        gPuts(fobj, "\t\tsuper(inObj);\n");
        gPuts(fobj, "\t\tif (inObj.getClassNameFromAddr(inObj.getAddress()) != null) {\n");
	gPuts(fobj, "\t\t\tDynaceObject objNew = inObj.gNew();\n");
	gPuts(fobj, "\t\t\tcopy(objNew);\n");
	gPuts(fobj, "\t\t\tm_bDispose = true;\n");
        gPuts(fobj, "\t\t}\n");
	gPuts(fobj, "\t}\n\n");
	gPuts(fobj, "\tpublic DynaceObject(DynaceObject inObj, String szClassName) {\n");
        gPuts(fobj, "\t\tsuper(inObj);\n");
        gPuts(fobj, "\t\tif (inObj.getClassNameFromAddr(inObj.getAddress()) != null) {\n");
	gPuts(fobj, "\t\t\tDynaceObject objNew = inObj.gNewWithStr(szClassName);\n");
	gPuts(fobj, "\t\t\tcopy(objNew);\n");
	gPuts(fobj, "\t\t\tm_bDispose = true;\n");
        gPuts(fobj, "\t\t} else\n");
	gPuts(fobj, "\t\t\tcopy(inObj);\n");
	gPuts(fobj, "\t}\n\n");
	gPuts(fobj, "\tpublic DynaceObject(DynaceObject inObj, double dval) {\n");
        gPuts(fobj, "\t\tsuper(inObj);\n");
        gPuts(fobj, "\t\tif (inObj.getClassNameFromAddr(inObj.getAddress()) != null) {\n");
	gPuts(fobj, "\t\t\tDynaceObject objNew = inObj.gNewWithDouble(dval);\n");
	gPuts(fobj, "\t\t\tcopy(objNew);\n");
	gPuts(fobj, "\t\t\tm_bDispose = true;\n");
	gPuts(fobj, "\t\t} else\n");
	gPuts(fobj, "\t\t\tcopy(inObj);\n");
	gPuts(fobj, "\t}\n\n");
	gPuts(fobj, "\tpublic DynaceObject(DynaceObject inObj, int ival) {\n");
        gPuts(fobj, "\t\tif (inObj.getClassNameFromAddr(inObj.getAddress()) != null) {\n");
	gPuts(fobj, "\t\t\tDynaceObject objNew = inObj.gNewWithLong(ival);\n");
	gPuts(fobj, "\t\t\tcopy(objNew);\n");
	gPuts(fobj, "\t\t\tm_bDispose = true;\n");
        gPuts(fobj, "\t\t} else\n");
	gPuts(fobj, "\t\t\tcopy(inObj);\n");
	gPuts(fobj, "\t}\n\n");
	gPuts(fobj, "\tpublic DynaceBase addDynaceObject(String szName, int nAddress) {\n");
        gPuts(fobj, "\t\treturn (DynaceClass)new DynaceObject(szName, nAddress);\n");
	gPuts(fobj, "\t}\n\n");

	gPuts(fobj, "\tpublic static void setupGenericAddresses()\n");
	gPuts(fobj, "\t{\n");
	vPrintf(fobj, "\t\tgenericAddresses = new int[%d];\n\n", gSize(links));
	for (seq=gSequence(links) ; obj = gNext(seq) ; )
		vPrintf(fobj, "\t\tgenericAddresses[%d] = DynaceGeneric.getAddress(\"%s\");\n",
			gLongValue(gValue(obj)), gStringKey(obj));
	gPuts(fobj, "\t}\n\n");
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		int	argc, i;
		object	seq, to;
		char	*type, *javaRetType;
		char	cNext, *rtn;

		cNext = 'N';
		if (gIsVarArg(proto))
			continue;
		if (isGUID(proto))
			continue;

		/* check for va_list type and skip if found */
		for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
			type = gStringValue(to);
			if (streq(type, "va_list")) {
				cNext = 'Y';
				break;
			}
		}
		if (cNext == 'Y')
			continue;
		
		name = gStringValue(key);
		rtn = gStringValue(gReturnType(proto));
		if (streq(rtn, "objrtn"))
			rtn = "object";

		vPrintf(fobj, "\tpublic ");
		javaRetType = get_java_type(rtn);
		vPrintf(fobj, "%s ", javaRetType);
		vPrintf(fobj, "%s(", name);
		
		argc = gSize(gParameters(proto));
		for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
			char szParmName[16];
			sprintf(szParmName, "parm%d", i);
			type = gStringValue(to);
			if (i > 1)
				vPrintf(fobj, ", ");
			vPrintf(fobj, "%s %s", get_java_type(type), szParmName);
		}

		vPrintf(fobj, ") {\n");
		if (argc > 1) {
			vPrintf(fobj, "\t\tshort sArgCnt = %d;\n", argc-1);
			vPrintf(fobj, "\t\tObject vParms[] = new Object[sArgCnt*2];\n");

			for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
				char szParmName[16], *pszJavaType;
				sprintf(szParmName, "parm%d", i);
				type = gStringValue(to);
				pszJavaType = get_java_type(type);

				if (streq(pszJavaType, "DynaceObject"))
					vPrintf(fobj, "\t\tvParms[%d] = new Integer(%s.getAddress());", i-1, szParmName);
				else if (streq(pszJavaType, "int"))
					vPrintf(fobj, "\t\tvParms[%d] = new Integer(%s);", i-1, szParmName);
				else if (streq(pszJavaType, "short"))
					vPrintf(fobj, "\t\tvParms[%d] = new Short(%s);", i-1, szParmName);
				else if (streq(pszJavaType, "double"))
					vPrintf(fobj, "\t\tvParms[%d] = new Double(%s);", i-1, szParmName);
				else if (streq(pszJavaType, "float"))
					vPrintf(fobj, "\t\tvParms[%d] = new Float(%s);", i-1, szParmName);
				else if (streq(pszJavaType, "char"))
					vPrintf(fobj, "\t\tvParms[%d] = new Char(%s);", i-1, szParmName);
				else if (streq(pszJavaType, "byte"))
					vPrintf(fobj, "\t\tvParms[%d] = new Byte(%s);", i-1, szParmName);
				else
					vPrintf(fobj, "\t\tvParms[%d] = new %s(%s);", i-1, pszJavaType, szParmName);
				

				vPrintf(fobj, "\n");
			}
			vPrintf(fobj, "\t\tInsertTypes(vParms, sArgCnt);\n");
			strcpy(endfunc, ", sArgCnt, vParms)");
		} else
			strcpy(endfunc, ", (short)0, null)");

		vPrintf(fobj, "\t\tif (getSuperCall())\n");
		if (streq(javaRetType, "DynaceObject")) {
			vPrintf(fobj, "\t\t\treturn (DynaceObject)getReturnObj(superCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s);\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn (DynaceObject)getReturnObj(callToDynace(getAddress(), genericAddresses[%d]%s);\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "int")) {
			vPrintf(fobj, "\t\t\treturn superCallToDynace(getAddress(), getSuperCallCls().getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn callToDynace(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "String")) {
			vPrintf(fobj, "\t\t\treturn getStringFromCharBuff(superCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s);\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn getStringFromCharBuff(callToDynace(getAddress(), genericAddresses[%d]%s);\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "short")) {
			vPrintf(fobj, "\t\t\treturn (short)superCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn (short)callToDynace(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "char")) {
			vPrintf(fobj, "\t\t\treturn (char)superCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn (char)callToDynace(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "byte")) {
			vPrintf(fobj, "\t\t\treturn (byte)superCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn (byte)callToDynace(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "float")) {
			vPrintf(fobj, "\t\t\treturn (float)superCallToDynaceReturnDouble(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn (float)callToDynaceReturnDouble(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "double")) {
			vPrintf(fobj, "\t\t\treturn superCallToDynaceReturnDouble(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\treturn callToDynaceReturnDouble(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		else if (streq(javaRetType, "void")) {
			vPrintf(fobj, "\t\t\tsuperCallToDynace(getAddress(), getSuperCallCls().getAddress(), DynaceGeneric.getObjAddress(\"%s\")%s;\n",
				name, endfunc);
			vPrintf(fobj, "\t\telse\n");
			vPrintf(fobj, "\t\t\tcallToDynace(getAddress(), genericAddresses[%d]%s;\n",
				gLongValue(gFindValueStr(links, name)), endfunc);
		}
		gPuts(fobj, "\t}\n");
	}

	gPuts(fobj, "}\n");
	gDispose(fobj);
	DEEPDISPOSE(links);
}

static	void
make_java_class_ref(object	classes,
		    object	generics,
		    char	*package,
		    object	argList,
		    object	inc_sh,
		    object	inc_ah)
{
	object	fJavaClass, key, val;
	char	*name;
	
	if (NULL == (fJavaClass = open_file("Dynace.java", WMODE, 0)))
		return;
	
	gPuts(fJavaClass, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fJavaClass,     "    be edited by hand.  */\n\n\n");

	if (package && *package)
		vPrintf(fJavaClass, "package %s;\n\n", package);
	gPuts(fJavaClass, "import Dynace.*;\n\n\n");
	vPrintf(fJavaClass, "public class Dynace {\n");
	
        gPuts(fJavaClass, "\tpublic static DynaceObject getDynaceClass(String szName) {\n");
        gPuts(fJavaClass, "\t\treturn (DynaceObject)DynaceClass.getDynaceClass(szName);\n");
	gPuts(fJavaClass, "\t}\n");
        gPuts(fJavaClass, "\tpublic static String getClassNameFromAddr(int nAddr) {\n");
        gPuts(fJavaClass, "\t\treturn DynaceClass.getClassNameFromAddr(nAddr);\n");
	gPuts(fJavaClass, "\t}\n");
	
	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);
		vPrintf(fJavaClass, "\tpublic static DynaceObject %s = (DynaceObject)DynaceClass.getDynaceClass(\"%s\");\n", name, name);
	}
	vPrintf(fJavaClass, "}\n");
	gDispose(fJavaClass);
}

static	void
make_java_c(object	classes,
	    object	generics,
	    char	*package,
	    object	argList,
	    object	inc_sh,
	    object	inc_ah)
{
	object	fobj, key, val;
	char	*name;
	
	if (NULL == (fobj = open_file("LoadDynClasses.c", WMODE, 0)))
		return;
	
	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	vPrintf(fobj, "#include \"generics.h\"\n\n\n");
	if (package && *package)
		vPrintf(fobj, "char\tJavaPackageName[] = \"%s\";\n\n", package);
	else
		vPrintf(fobj, "char\tJavaPackageName[] = \"\";\n\n");
	
	vPrintf(fobj, "void\tLoadDynaceClasses()\n{\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);
		vPrintf(fobj, "\t%s;\n", name);
	}
	vPrintf(fobj, "}\n");
	gDispose(fobj);
}

static	void
output_scheme_macro(object fobj,
		    char   *name,
		    object proto)
{
	object	seq, to;
	int	sz = gSize(gPrototype(proto));
	int	i;
	char	*type;
	
	vPrintf(fobj, "(define-syntax %s\n", name);
	vPrintf(fobj, "  (syntax-rules ()\n");
	vPrintf(fobj, "                ((_");
	for (i = 0 ; i < sz ; i++)
		vPrintf(fobj, " p%d", i + 1);
	vPrintf(fobj, ")\n");
	vPrintf(fobj, "                 (let* (");
	for (i=1, seq=gSequence(gPrototype(proto)) ; to = gNext(seq) ; i++) {
		type = gStringValue(to);
		if (isDoubleArg(type))
			vPrintf(fobj, "(pobj%d (gNewWithDouble DoubleFloat p%d))\n                        ", i, i);
		else if (isLongArg(type))
			vPrintf(fobj, "(pobj%d (gNewWithLong LongInteger p%d))\n                        ", i, i);
		else if (isObjectArg(type))
			vPrintf(fobj, "(pobj%d (gNewWithLong LongInteger (object->int p%d)))"
				"\n                        ", i, i);
		else if (isShortArg(type))
			vPrintf(fobj, "(pobj%d (gNewWithInt ShortInteger p%d))\n                        ", i, i);
		else if (isUShortArg(type))
			vPrintf(fobj, "(pobj%d (gNewWithUnsigned UnsignedShortInteger p%d))\n"
				"                        ", i, i);
	}
	if (i > 10)
		vPrintf(fobj, "(rval   (%s_m", name);
	else
		vPrintf(fobj, "(rval  (%s_m", name);
	for (i=1, seq=gSequence(gPrototype(proto)) ; to = gNext(seq) ; i++) {
		type = gStringValue(to);
		if (isInputArg(type))
			vPrintf(fobj, " (gPointerValue pobj%d)", i);
		else
			vPrintf(fobj, " p%d", i);
	}
	vPrintf(fobj, ")))\n");
	for (i=1, seq=gSequence(gPrototype(proto)) ; to = gNext(seq) ; i++) {
		type = gStringValue(to);
		if (isDoubleArg(type))
			vPrintf(fobj, "                   (set! p%d (gDoubleValue pobj%d))\n"
				"                   (gDispose pobj%d)\n", i, i, i);
		else if (isLongArg(type))
			vPrintf(fobj, "                   (set! p%d (gLongValue pobj%d))\n"
				"                   (gDispose pobj%d)\n", i, i, i);
		else if (isObjectArg(type))
			vPrintf(fobj, "                   (set! p%d (int->object (gLongValue pobj%d)))\n"
				"                   (gDispose pobj%d)\n", i, i, i);
		else if (isShortArg(type))
			vPrintf(fobj, "                   (set! p%d (gShortValue pobj%d))\n"
				"                   (gDispose pobj%d)\n", i, i, i);
		else if (isUShortArg(type))
			vPrintf(fobj, "                   (set! p%d (gUnsignedShortValue pobj%d))\n"
				"                   (gDispose pobj%d)\n", i, i, i);
	}
	vPrintf(fobj, "                   rval))))\n\n");
}

static	int	isGUID(object proto)
{
	object	seq, to;
	int	ret = 0;
	
	if (streq(gStringValue(gReturnType(proto)), "GUID"))
		return 1;
	for (seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; )
		if (streq(gStringValue(to), "GUID")) {
			ret = 1;
			DISPOSE(seq);
			break;
		}
	return ret;
}

static	void
make_mzscheme(object	classes,
	      object	generics,
	      char	*file,
	      object	argList,
	      object	inc_sh,
	      object	inc_ah)
{
	object	fobj, seq, obj, proto, key, val;
	char	*name, *rtn;
	int	needMacros = 0;
	
	rtn = file_extension(file);
	if (strne(rtn, "c")  &&
	    strne(rtn, "C"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated scheme interface file names must be valid C or C++ file names.\n");
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);

	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	gPuts(fobj, "#include \"generics.h\"\n");
	gPuts(fobj, "#include \"scheme.h\"\n");
	gPuts(fobj, "\n");

	if (inc_sh)  {
		for (seq = gSequence(inc_sh) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (inc_ah)  {
		for (seq = gSequence(inc_ah) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		int	argc, i;
		object	seq, to;
		char	*type;

/*
		if (gIsVarArg(proto))
			continue;
*/
		if (isGUID(proto))
			continue;
		
		name = gStringValue(key);
		rtn = gStringValue(gReturnType(proto));
		if (streq(rtn, "objrtn"))
			rtn = "object";
		else if (streq(rtn, "void"))
			rtn = NULL;
		vPrintf(fobj, "SCHEME_FUNCTION(%s_s)\n{\n", name);
		if (gIsVarArg(proto)) {
			if (!rtn)
				vPrintf(fobj, "\treturn handle_vargs_null(argc, argv, %s, \"%s\");\n", name, name);	
			else if (streq(rtn, "object"))
				vPrintf(fobj, "\treturn handle_vargs_object(argc, argv, %s, \"%s\");\n", name, name);	
			else if (streq(rtn, "int")  ||  streq(rtn, "unsigned")  ||  streq(rtn, "long")  ||
				 streq(rtn, "unsigned long")  ||  streq(rtn, "LRESULT"))
				vPrintf(fobj, "\treturn handle_vargs_integer(argc, argv, (int (*)()) %s, \"%s\");\n", name, name);	
			else if (streq(rtn, "short")  ||  streq(rtn, "unsigned short")) {
				vPrintf(fobj, "\tgError(Object, \"Scheme vGenerics can't return short (%s)\");\n", name);
				gPuts(fobj, "\treturn scheme_null;\n");
			} else if (streq(rtn, "char *"))
				vPrintf(fobj, "\treturn handle_vargs_string(argc, argv, %s, \"%s\");\n", name, name);	
			else if (streq(rtn, "char")) {
				vPrintf(fobj, "\tgError(Object, \"Scheme vGenerics can't return char (%s)\");\n", name);
				gPuts(fobj, "\treturn scheme_null;\n");
			} else if (streq(rtn, "double")  ||  streq(rtn, "float"))
				vPrintf(fobj, "\treturn handle_vargs_double(argc, argv, %s, \"%s\");\n", name, name);	
			else if (is_pointer(rtn)  ||
				 streq(rtn, "ofun")  ||
				 streq(rtn, "ifun")  ||
				 streq(rtn, "WNDPROC")  ||
				 streq(rtn, "REFCLSID")  ||
				 streq(rtn, "HSTMT")  ||
				 streq(rtn, "HDBC")  ||
				 streq(rtn, "HENV")  ||
				 streq(rtn, "va_list")  ||
				 streq(rtn, "FARPROC"))
				vPrintf(fobj, "\treturn handle_vargs_c_pointer(argc, argv, (ifun) %s, \"%s\");\n", name, name);	
			else 
				vPrintf(fobj, "\treturn handle_vargs_integer(argc, argv, %s, \"%s\");\n", name, name);	
		} else {
			argc = gSize(gParameters(proto));
			vPrintf(fobj, "\tstatic\tchar\tfun[] = \"%s\";\n", name);
			if (rtn)
				vPrintf(fobj, "\t%s\tr;\n\n", rtn);

			if (!gIsVarArg(proto))
				vPrintf(fobj, "\tSCHEME_ARG_COUNT(%d);\n", argc);
			else
				vPrintf(fobj, "\tSCHEME_ARG_VCOUNT(1);\n");

			vPrintf(fobj, "\tSCHEME_CHK_ARG_DYNACE(0);\n");

			for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
				type = gStringValue(to);
				if (streq(type, "object"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_DYNACE(%d);\n", i);
				else if (streq(type, "char *"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_STRING(%d);\n", i);
				else if (is_pointer(type)  ||
					 streq(type, "ofun")  ||
					 streq(type, "ifun")  ||
					 streq(type, "REFIID")  ||
					 streq(type, "WNDPROC")  ||
					 streq(type, "REFCLSID")  ||
					 streq(type, "HSTMT")  ||
					 streq(type, "HDBC")  ||
					 streq(type, "HENV")  ||
					 streq(type, "va_list")  ||
					 streq(type, "FARPROC"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_CPOINTER(%d);\n", i);
				else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
					 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
					 streq(type, "HWND")  ||  streq(type, "HANDLE")  ||  streq(type, "SWORD")  || streq(type, "SDWORD")  ||
					 streq(type, "UDWORD") || streq(type, "DWORD"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_INT(%d);\n", i);
				else if (streq(type, "double")  ||  streq(type, "float"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_DBL(%d);\n", i);
				else if (streq(type, "char"))
					vPrintf(fobj, "\tSCHEME_CHK_ARG_CHAR(%d);\n", i);
				else
					vPrintf(fobj, "\tSCHEME_CHK_ARG_INT(%d);\n", i);
			}

			if (rtn)
				gPuts(fobj, "\tr = ");
			else
				gPuts(fobj, "\t");

			vPrintf(fobj, "%s((object) SCHEME_PTR_VAL(argv[0])", name);
			for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
				type = gStringValue(to);
				if (streq(type, "char *"))
					vPrintf(fobj, ",\n\t\t\tSCHEME_STR_VAL(argv[%d])", i);
				else if (streq(type, "object")  ||
					 is_pointer(type)  ||
					 streq(type, "ofun")  ||
					 streq(type, "ifun")  ||
					 streq(type, "REFIID")  ||
					 streq(type, "WNDPROC")  ||
					 streq(type, "REFCLSID")  ||
					 streq(type, "HSTMT")  ||
					 streq(type, "HDBC")  ||
					 streq(type, "HENV")  ||
					 streq(type, "va_list")  ||
					 streq(type, "FARPROC"))
					vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_PTR_VAL(argv[%d])", type, i);
				else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
					 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
					 streq(type, "HWND")  ||  streq(type, "HANDLE")  ||  streq(type, "SWORD")  ||
					 streq(type, "UDWORD") || streq(type, "DWORD") || streq(type, "SDWORD"))
					vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_INT_VAL(argv[%d])", type, i);
				else if (streq(type, "double")  ||  streq(type, "float"))
					vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_DBL_VAL2(argv[%d])", type, i);
				else if (streq(type, "char"))
					vPrintf(fobj, ",\n\t\t\tSCHEME_CHAR_VAL(argv[%d])", i);
				else
					vPrintf(fobj, ",\n\t\t\t(%s) SCHEME_INT_VAL(argv[%d])", type, i);
			}
			gPuts(fobj, ");\n");

			if (!rtn)
				gPuts(fobj, "\treturn scheme_null;\n");
			else if (streq(rtn, "object"))
				gPuts(fobj, "\treturn scheme_new_dynace_object(r);\n");
			else if (streq(rtn, "int")  ||  streq(rtn, "short")  ||  streq(rtn, "unsigned")  ||  streq(rtn, "long")  ||
				 streq(rtn, "unsigned short")  ||  streq(rtn, "LRESULT"))
				gPuts(fobj, "\treturn scheme_make_integer((int)r);\n");
			else if (streq(rtn, "char *"))
				gPuts(fobj, "\treturn scheme_make_string2(r);\n");
			else if (streq(rtn, "char"))
				gPuts(fobj, "\treturn scheme_make_char(r);\n");
			else if (streq(rtn, "double")  ||  streq(rtn, "float"))
				gPuts(fobj, "\treturn scheme_make_double(r);\n");
			else if (is_pointer(rtn)  ||
				 streq(rtn, "ofun")  ||
				 streq(rtn, "ifun")  ||
				 streq(rtn, "WNDPROC")  ||
				 streq(rtn, "REFCLSID")  ||
				 streq(rtn, "HSTMT")  ||
				 streq(rtn, "HDBC")  ||
				 streq(rtn, "HENV")  ||
				 streq(rtn, "va_list")  ||
				 streq(rtn, "FARPROC"))
				gPuts(fobj, "\treturn scheme_new_c_pointer(r);\n");
			else 
				gPuts(fobj, "\treturn scheme_make_integer((int)r);\n");
		}
		gPuts(fobj, "}\n\n");
	}

	gPuts(fobj, "void\tScheme_init_base()\n");
	gPuts(fobj, "{\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);
		vPrintf(fobj, "\tADD_OBJ(%s);\n", name);
	}

	gPuts(fobj, "\n");

	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
/*
		if (gIsVarArg(gValue(obj)))
			continue;
*/
		name = gStringValue(key);

		if (isGUID(proto))
			continue;

		if (gHasInputArg(proto)) {
			needMacros = 1;
			vPrintf(fobj, "\tADD_FUN(%s_m, %s_s);\n", name, name);
		} else
			vPrintf(fobj, "\tADD_FUN(%s, %s_s);\n", name, name);
	}
	gPuts(fobj, "}\n\n");

	gDispose(fobj);

	if (needMacros)
		make_mzscheme_macros(generics, file);
}

static	void
make_mzscheme_macros(object	generics,
		     char	*file)
{
	object	fobj, proto, key;
	char	sfile[256]; /* Name of scheme file.  256 is generally max length of file path */
	
	strcpy(sfile, file);
	strip_extension(sfile);
	strcat(sfile, ".scm");
	
	if (NULL == (fobj = open_file(sfile, WMODE, 0)))
		return;

	if (Copyright)
		schemecopyright(fobj);

	gPuts(fobj, "\n\n;  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     ";  be edited by hand.  \n\n\n");
	gPuts(fobj, "(require-library \"synrule.ss\")\n\n");

	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		if (gIsVarArg(proto))
			continue;
		if (gHasInputArg(proto))
			output_scheme_macro(fobj, gStringValue(key), proto);
	}
	gPuts(fobj, "\n\n");

	gDispose(fobj);
}

static	void
make_javascript(object	classes,
		object	generics,
		char	*file,
		object	argList,
		object	inc_sh,
		object	inc_ah)
{
	object	fobj, seq, obj, proto, key, val;
	char	*name, *rtn;
	int	argc;
	
	rtn = file_extension(file);
	if (strne(rtn, "c")  &&
	    strne(rtn, "C"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated JavaScript interface file names must be valid C or C++ file names.\n");
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);

	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	gPuts(fobj, "#include \"generics.h\"\n");
	gPuts(fobj, "\n");
 
	if (inc_sh)  {
		for (seq = gSequence(inc_sh) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (inc_ah)  {
		for (seq = gSequence(inc_ah) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	gPuts(fobj, "#define\tXP_WIN\n\n");

	gPuts(fobj, "#include \"jsapi.h\"\n\n");

	gPuts(fobj, "static\tJSContext\t*Cache_cx;\n");
	gPuts(fobj, "static\tjsval\t\tDO_class_obj, DO_proto, P_class_obj, P_proto;\n");
	gPuts(fobj, "static\tJSClass\t\t*DO_classp, *P_classp;\n\n");

	gPuts(fobj, "/*  This caching scheme only works for single threading/instance/context JavaScript.  */\n\n");

	gPuts(fobj, "static\tvoid\tload_context(JSContext *cx)\n");
	gPuts(fobj, "{\n");
	gPuts(fobj, "\tJS_GetProperty(cx, JS_GetGlobalObject(cx), \"DynaceObject\", &DO_class_obj);\n");
	gPuts(fobj, "\tJS_GetProperty(cx, (JSObject *)DO_class_obj, \"prototype\", &DO_proto);\n");
	gPuts(fobj, "\tDO_classp = JS_GetClass((JSObject *)DO_proto);\n\n");

	gPuts(fobj, "\tJS_GetProperty(cx, JS_GetGlobalObject(cx), \"CPointer\", &P_class_obj);\n");
	gPuts(fobj, "\tJS_GetProperty(cx, (JSObject *)P_class_obj, \"prototype\", &P_proto);\n");
	gPuts(fobj, "\tP_classp = JS_GetClass((JSObject *)P_proto);\n\n");
	
	gPuts(fobj, "\tCache_cx = cx;\n");
	gPuts(fobj, "}\n\n\n");
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		int	i;
		object	seq, to;
		char	*type;


		if (gIsVarArg(proto))
			continue;

		if (isGUID(proto))
			continue;
		
		name = gStringValue(key);
		rtn = gStringValue(gReturnType(proto));
		if (streq(rtn, "objrtn"))
			rtn = "object";
		else if (streq(rtn, "void"))
			rtn = NULL;


		vPrintf(fobj, "static\tJSBool\tJS_%s(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)\n{\n", name);
		gPuts(fobj, "\tobject	self;\n");
		vPrintf(fobj, "\tstatic\tchar\tfun[] = \"%s\";\n", name);

		if (rtn)
			vPrintf(fobj, "\t%s\tr;\n\n", rtn);

		gPuts(fobj, "\tif (Cache_cx != cx)\n");
		gPuts(fobj, "\t\tload_context(cx);\n\n");
		
		if (gIsVarArg(proto)) {
		} else {
			argc = gSize(gParameters(proto));

			vPrintf(fobj, "\tif (argc != %d)\n", argc);
			vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript: %%s expects %%d arguments, given %%d.\", fun, %d, argc);\n\n", argc);

			vPrintf(fobj, "\tif (!JSVAL_IS_OBJECT(argv[0])  ||  JS_TRUE != JS_InstanceOf(cx, (JSObject *) argv[0], DO_classp, NULL))\n");
			vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid object in arg 1\", fun);\n");
			vPrintf(fobj, "\tself = (object) JS_GetPrivate(cx, (JSObject *) argv[0]);\n\n");
			
			for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
				type = gStringValue(to);
				if (streq(type, "object")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_OBJECT(argv[%d])  ||  JS_TRUE != JS_InstanceOf(cx, (JSObject *) argv[%d], DO_classp, NULL))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid object in arg %d\", fun);\n", i+1);
				} else if (streq(type, "char *")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_STRING(argv[%d]))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid string in arg %d\", fun);\n", i+1);
				} else if (is_pointer(type)  ||
					 streq(type, "ofun")  ||
					 streq(type, "ifun")  ||
					 streq(type, "REFIID")  ||
					 streq(type, "WNDPROC")  ||
					 streq(type, "REFCLSID")  ||
					 streq(type, "HSTMT")  ||
					 streq(type, "HDBC")  ||
					 streq(type, "HENV")  ||
					 streq(type, "HWND")  ||
					 streq(type, "HANDLE")  ||
					 streq(type, "va_list")  ||
					 streq(type, "FARPROC")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_OBJECT(argv[%d])  ||  JS_TRUE != JS_InstanceOf(cx, (JSObject *) argv[%d], P_classp, NULL))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid pointer in arg %d\", fun);\n", i+1);
				} else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
					 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
					 streq(type, "SWORD")  || streq(type, "SDWORD")  ||
					 streq(type, "UDWORD") || streq(type, "DWORD")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_INT(argv[%d])  &&  !JSVAL_IS_DOUBLE(argv[%d]))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid integer in arg %d\", fun);\n", i+1);
				} else if (streq(type, "double")  ||  streq(type, "float")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_INT(argv[%d])  &&  !JSVAL_IS_DOUBLE(argv[%d]))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid double in arg %d\", fun);\n", i+1);
				} else if (streq(type, "char")) {
					vPrintf(fobj, "\tif (!JSVAL_IS_INT(argv[%d]))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid integer in arg %d\", fun);\n", i+1);
				} else {
					vPrintf(fobj, "\tif (!JSVAL_IS_INT(argv[%d]))\n", i, i);
					vPrintf(fobj, "\t\tvError(Dynace, \"JavaScript:  %%s passed an invalid integer in arg %d\", fun);\n", i+1);
				}
			}

			if (rtn)
				gPuts(fobj, "\tr = ");
			else
				gPuts(fobj, "\t");

			vPrintf(fobj, "%s(self", name);
			for (i=1, seq=gSequence(gPrototype(proto)), gNext(seq) ; to = gNext(seq) ; i++) {
				type = gStringValue(to);
				if (streq(type, "object"))
					vPrintf(fobj, ",\n\t\t\tJS_GetPrivate(cx, (JSObject *) argv[%d])", i);
				else if (streq(type, "char *"))
					vPrintf(fobj, ",\n\t\t\tJS_GetStringBytes(JSVAL_TO_STRING(argv[%d]))", i);
				else if (is_pointer(type)  ||
					 streq(type, "ofun")  ||
					 streq(type, "ifun")  ||
					 streq(type, "REFIID")  ||
					 streq(type, "WNDPROC")  ||
					 streq(type, "REFCLSID")  ||
					 streq(type, "HSTMT")  ||
					 streq(type, "HDBC")  ||
					 streq(type, "HENV")  ||
					 streq(type, "va_list")  ||
					 streq(type, "HWND")  ||
					 streq(type, "HANDLE")  ||
					 streq(type, "FARPROC"))
					vPrintf(fobj, ",\n\t\t\t(%s) JS_GetPrivate(cx, (JSObject *) argv[%d])", type, i);
				else if (streq(type, "int")  ||  streq(type, "short")  ||  streq(type, "unsigned")  ||  streq(type, "long")  ||
					 streq(type, "unsigned short")  ||  streq(type, "LRESULT")  ||
					 streq(type, "SWORD")  ||
					 streq(type, "UDWORD") || streq(type, "DWORD") || streq(type, "SDWORD"))
					vPrintf(fobj, ",\n\t\t\t(%s) ((JSVAL_IS_INT(argv[%d])) ? (%s)(JSVAL_TO_INT(argv[%d])) : (%s)(*JSVAL_TO_DOUBLE(argv[%d])))", type, i, type, i, type, i);
				else if (streq(type, "double")  ||  streq(type, "float"))
					vPrintf(fobj, ",\n\t\t\t((JSVAL_IS_INT(argv[%d])) ? (%s)(JSVAL_TO_INT(argv[%d])) : (%s)(*JSVAL_TO_DOUBLE(argv[%d])))", i, type, i, type, i);
				else if (streq(type, "char"))
					vPrintf(fobj, ",\n\t\t\t(char) JSVAL_TO_INT(argv[%d])", i);
				else
					vPrintf(fobj, ",\n\t\t\t(%s) JSVAL_TO_INT(argv[%d])", type, i);
			}
			gPuts(fobj, ");\n");
			if (rtn)
				if (streq(rtn, "object"))
					gPuts(fobj, "\t*rval = OBJECT_TO_JSVAL(gNewJSDynaceObject(JavaScript, r));\n");
				else if (streq(rtn, "int")  ||  streq(rtn, "short")  ||  streq(rtn, "unsigned")  ||  streq(rtn, "long")  ||
					 streq(rtn, "unsigned short")  ||  streq(rtn, "LRESULT")) {
					gPuts(fobj, "\tif (INT_FITS_IN_JSVAL(r))\n");
					gPuts(fobj, "\t\t*rval = INT_TO_JSVAL(r);\n");
					gPuts(fobj, "\telse\n");
					gPuts(fobj, "\t\tJS_NewDoubleValue(cx, (double) r, rval);\n");
				} else if (streq(rtn, "char *"))
					gPuts(fobj, "\t*rval = STRING_TO_JSVAL(JS_NewStringCopyZ(cx, r));\n");
				else if (streq(rtn, "char"))
					gPuts(fobj, "\t*rval = INT_TO_JSVAL(r);\n");
				else if (streq(rtn, "double")  ||  streq(rtn, "float"))
					gPuts(fobj, "\tJS_NewDoubleValue(cx, r, rval);\n");
			else if (is_pointer(rtn)  ||
				 streq(rtn, "ofun")  ||
				 streq(rtn, "ifun")  ||
				 streq(rtn, "WNDPROC")  ||
				 streq(rtn, "REFCLSID")  ||
				 streq(rtn, "HSTMT")  ||
				 streq(rtn, "HDBC")  ||
				 streq(rtn, "HENV")  ||
				 streq(rtn, "HWND")  ||
				 streq(rtn, "HANDLE")  ||
				 streq(rtn, "va_list")  ||
				 streq(rtn, "FARPROC"))
					gPuts(fobj, "\t*rval = OBJECT_TO_JSVAL(gNewJSPointer(JavaScript, r));\n");
			else
					gPuts(fobj, "\t*rval = INT_TO_JSVAL((int)r);\n");
			gPuts(fobj, "\treturn JS_TRUE;\n");
		}
		gPuts(fobj, "}\n\n");
	}

	gPuts(fobj, "void\tJavaScript_init_base()\n");
	gPuts(fobj, "{\n");

	//vPrintf(fobj, "\tgSetJSGlobalToObj(JavaScript, \"%s\", %s);\n", "Dynace", "Dynace");
	/*
		Add all objects to the global object as properties.  Those that conflict with important
		built-in JavaScript classes are skipped.
	*/
	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);

		//if (streq(name, "Dynace"))
		//	continue;
		
		//vPrintf(fobj, "\tgSetJSClassPropToObj(JavaScript, \"Dynace\", \"%s\", %s);\n", name, name);
		
		if (! (streq(name,"Object") || streq(name,"Function") || streq(name,"String") || 
				streq(name,"Array") || streq(name,"Boolean") || //streq(name,"Date") ||
				streq(name,"Math") || streq(name,"Number") || streq(name,"RegExp")))
			vPrintf(fobj, "\tgSetJSGlobalToObj(JavaScript, \"%s\", %s);\n", name, name);
	}

	gPuts(fobj, "\n");

	/*
		Add all objects to the Dynace object as properties.
	*/
	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);

		if (streq(name, "Dynace"))
			continue;
		
		//vPrintf(fobj, "\tgSetJSClassPropToObj(JavaScript, \"Dynace\", \"%s\", %s);\n", name, name);
		vPrintf(fobj, "\tgSetJSObjectProp(JavaScript, \"Dynace\", \"%s\", %s);\n", name, name);
	}

	gPuts(fobj, "\n");

	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {

		if (gIsVarArg(proto))
			continue;

		name = gStringValue(key);

		if (isGUID(proto))
			continue;
		argc = gSize(gParameters(proto));
		vPrintf(fobj, "\tgAddJSGeneric(JavaScript, \"%s\", JS_%s, %d);\n", name, name, argc);
	}
	gPuts(fobj, "}\n\n");

	gDispose(fobj);
}

static	void
make_h(object	classes,
       object	generics,
       char	*file,
       object	argList,
       object	inc_sh,
       object	inc_ah)
{
	object	fobj, seq, obj, proto, key, val;
	char	macro[60], *name, *rtn;
	
	rtn = file_extension(file);
	if (strne(rtn, "h")  &&
	    strne(rtn, "H")  &&
	    strne(rtn, "hh")  &&
	    strne(rtn, "HH"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated generic header file names must be valid C or C++ header file names.\n");
		return;
	}

	if (!ForceGenerate  &&  !need_to_make(classes, generics, file))  {
		touch_all(argList);
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);

#ifndef	DBI
	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	make_macro_name(macro, file);

	vPrintf(fobj, "#ifndef\t%s\n", macro);
	vPrintf(fobj, "#define\t%s\n\n\n", macro);

	gPuts(fobj, "#ifdef\tDPP_STRATEGY\n");
	vPrintf(fobj, "#if\tDPP_STRATEGY != %d\n", Strategy);
	gPuts(fobj, "#error Incompatible DPP Strategies\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	vPrintf(fobj, "#define\tDPP_STRATEGY %d\n", Strategy);
	gPuts(fobj, "#endif\n\n");

	gPuts(fobj, "#ifdef\tDPP_FASTWIDE\n");
	vPrintf(fobj, "#if\tDPP_FASTWIDE != %d\n", FastWideCache);
	gPuts(fobj, "#error Incompatible DPP FastWide Status\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	vPrintf(fobj, "#define\tDPP_FASTWIDE %d\n", FastWideCache);
	gPuts(fobj, "#endif\n\n");

	if (inc_sh)  {
		for (seq = gSequence(inc_sh) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

/*	if (GenIncludes)  */
		gPuts(fobj, "#include \"dynl.h\"\n\n\n");

	if (inc_ah)  {
		for (seq = gSequence(inc_ah) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}
	
	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "extern\t\"C\"  {\n");
	gPuts(fobj, "#endif\n\n\n");

	gPuts(fobj, "\n\n#undef\tSTART_CLASSES\n\n");
	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key))
		vPrintf(fobj, "extern\tobject\t%s_c;\n", gStringValue(key));
	gPuts(fobj, "\n#undef\tEND_CLASSES\n\n\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key))
		vPrintf(fobj, "extern\tobjrtn\t%s_initialize(void);\n", gStringValue(key));
	gPuts(fobj, "\n\n\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key)) {
		name = gStringValue(key);
		vPrintf(fobj, "#define\t%s\t(%s_c ? %s_c : %s_initialize())\n", name, name, name, name);
	}
	gPuts(fobj, "\n\n\n");

	for (val=gFindFirst(classes, &key) ; val ; val=gFindNext(classes, &key))
		vPrintf(fobj, "typedef\tobject\t%s_t;\n", gStringValue(key));
	gPuts(fobj, "\n\n\n");
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key))
		vPrintf(fobj, "extern\tobject\tGeneric(%s);\n",
			gStringValue(key));
	gPuts(fobj, "\n\n");
	
	gPuts(fobj, "#undef\tSTART_PROTOTYPES\n\n");
#else
	USE(macro);
	gPuts(fobj, "\n//--------------------   generics.h   --------------------\n\n");
#endif
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		char	td[60];
		
		name = gStringValue(key);
		if (MacroGuard)  {
			make_macro_name(td, name);
			vPrintf(fobj, "#ifndef\t%s\n", td);
			vPrintf(fobj, "#define\t%s\n", td);
		}
		
		vPrintf(fobj, "typedef\t%s\t(*%s_t)(", 
			gStringValue(gReturnType(proto)), name);
		gPrintArgs(proto, fobj);
		gPuts(fobj, ");\n");

		vPrintf(fobj, "typedef\t%s\t(*%s_mt)(", 
			gStringValue(gReturnType(proto)), name);
		gPrintMethArgsH(proto, fobj);
		gPuts(fobj, ");\n");
		
		if (Strategy == 3  ||  Strategy == 4  &&  !gIsVarArg(proto))
			gen_inline(fobj, name, proto);
		if (MacroGuard)
			gPuts(fobj, "#endif\n");
	}
#ifndef	DBI
	gPuts(fobj, "\n\n#undef\tEND_PROTOTYPES\n\n");
	
	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		name = gStringValue(key);
		if (Strategy < 3)
			vPrintf(fobj, "extern\t%s_t\t%s;\n", name, name);
		else if (Strategy == 4 && gIsVarArg(proto))  {
			rtn = gStringValue(gReturnType(proto));
			vPrintf(fobj, "extern\t%s\t%s(", rtn, name);
			gPrintArgs(proto, fobj);
			gPuts(fobj, ");\n");
		}
/*
		else    *  C++ inline  *
			gen_inline(fobj, name, proto);
*/
	}
	gPuts(fobj, "\n\n\n");

	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "}\n");
	gPuts(fobj, "#endif\n\n\n");

	vPrintf(fobj, "#endif\t/*  %s  */\n\n\n", macro);
#endif
	
	gDispose(fobj);
}

static	void	make_c(object	classes,
		       object	generics,
		       char	*file,
		       object	inc_sc,
		       object	inc_ac)
{
	object	fobj, seq, obj, proto, key;
	char	*name, *rtn;
	int	n;
	
	rtn = file_extension(file);
	if (strne(rtn, "c")  &&
	    strne(rtn, "C")  &&
	    strne(rtn, "cc")  &&
	    strne(rtn, "CC")  &&
	    strne(rtn, "cpp")  &&
	    strne(rtn, "CPP"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated generics.c file names must be valid C or C++ source file names.\n");
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);
#ifdef	DBI
	gPuts(fobj, "\n//--------------------   generics.c   --------------------\n\n");
#else
	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	if (inc_sc)  {
		for (seq = gSequence(inc_sc) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (inc_ac)  {
		for (seq = gSequence(inc_ac) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (GenIncludes)
		gPuts(fobj, "#include \"generics.h\"\n\n");

	gPuts(fobj, "#ifdef\tDPP_STRATEGY\n");
	vPrintf(fobj, "#if\tDPP_STRATEGY != %d\n", Strategy);
	gPuts(fobj, "#error Incompatible DPP Strategies\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	gPuts(fobj, "#error DPP_STRATEGY not set.\n");
	gPuts(fobj, "#endif\n\n");

	gPuts(fobj, "#ifdef\tDPP_FASTWIDE\n");
	vPrintf(fobj, "#if\tDPP_FASTWIDE != %d\n", FastWideCache);
	gPuts(fobj, "#error Incompatible DPP FastWide Status\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	gPuts(fobj, "#error DPP_FASTWIDE not set.\n");
	gPuts(fobj, "#endif\n\n");
	
	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "extern\t\"C\"  {\n");
	gPuts(fobj, "#endif\n\n\n");

	for (n=0, proto=gFindFirst(generics, &key) ;
	     proto  &&  (!MultipleGenerics  ||  n < MultipleGenerics) ;
	     proto = gFindNext(generics, &key), n++)
		vPrintf(fobj, "object\tGeneric(%s);\n",	gStringValue(key));
	gPuts(fobj, "\n\n\n");

#endif
	if (Strategy < 3)  {
		for (n=0, proto=gFindFirst(generics, &key) ;
		     proto  &&  (!MultipleGenerics  ||  n < MultipleGenerics) ;
		     proto = gFindNext(generics, &key), n++) {
			name = gStringValue(key);
			rtn = gStringValue(gReturnType(proto));
			if (Strategy == 1)  {
				vPrintf(fobj, "IGTYPE\t%s\t_%s(", rtn, name);
				gPrintArgs(proto, fobj);
				vPrintf(fobj, ")\n{\n\tGenericBody(%s);\n}\n\n", name);
			} else if (Strategy == 2)  {
				vPrintf(fobj, "static\t%s\t_%s(", rtn, name);
				gPrintArgs(proto, fobj);
				gPuts(fobj, ")\n{\n");
				if (FastWideCache)
					gPuts(fobj, "\tofun _meth_;\n");
				if (gIsVarArg(proto))
					if (streq(rtn, "void"))
						vPrintf(fobj, "\tva_list _rest_;\n\tva_start(_rest_, %s);\n", gLastArg(proto));
					else
						vPrintf(fobj, "\tva_list _rest_;\n\t%s _ret_;\n\tva_start(_rest_, %s);\n", rtn, gLastArg(proto));
				if (FastWideCache)  {
					vPrintf(fobj, "\tFW_GENERIC(%s);\n", name);
					vPrintf(fobj, "\t%s (*(%s_t)_meth_)(",
						streq(rtn, "void")?"":"return", name);
				} else {
					if (streq(rtn, "void"))
						vPrintf(fobj, "\t(*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else if (gIsVarArg(proto))
						vPrintf(fobj, "\t_ret_ = (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else
						vPrintf(fobj, "\treturn (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
				}
				gPrintVars(proto, fobj);
				gPuts(fobj, ");\n");
				if (gIsVarArg(proto)) {
					vPrintf(fobj, "\tva_end(_rest_);\n");
					if (!streq(rtn, "void"))
						vPrintf(fobj, "\treturn _ret_;\n");
				}
				gPuts(fobj, "}\n\n");
			}
		}
#ifndef	DBI
		gPuts(fobj, "\n\n\n");
	
		for (n=0, proto=gFindFirst(generics, &key) ;
		     proto  &&  (!MultipleGenerics  ||  n < MultipleGenerics) ;
		     proto = gFindNext(generics, &key), n++) {
			name = gStringValue(key);
			vPrintf(fobj, "%s_t\t%s = _%s;\n", name, name, name);
		}
		gPuts(fobj, "\n\n\n");
#endif
	}
	
	if (Strategy == 4)  {
		for (n=0, proto=gFindFirst(generics, &key) ;
		     proto  &&  (!MultipleGenerics  ||  n < MultipleGenerics) ;
		     proto = gFindNext(generics, &key), n++)
			if (gIsVarArg(proto))  {
				name = gStringValue(key);
				rtn = gStringValue(gReturnType(proto));
				vPrintf(fobj, "%s\t%s(", rtn, name);
				gPrintArgs(proto, fobj);
				gPuts(fobj, ")\n{\n");
				if (FastWideCache)
					gPuts(fobj, "\tofun _meth_;\n");
				if (streq(rtn, "void"))
					vPrintf(fobj, "\tva_list _rest_;\n\tva_start(_rest_, %s);\n", gLastArg(proto));
				else
					vPrintf(fobj, "\tva_list _rest_;\n\t%s\t_ret_;\n\tva_start(_rest_, %s);\n", rtn, gLastArg(proto));	
				if (FastWideCache)  {
					vPrintf(fobj, "\tFW_GENERIC(%s);\n", name);
					vPrintf(fobj, "\t%s (*(%s_t)_meth_)(",
						streq(rtn, "void")?"":"return", name);
				} else {
					if (streq(rtn, "void"))
						vPrintf(fobj, "\t(*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else
						vPrintf(fobj, "\t_ret_ = (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
				}
				gPrintVars(proto, fobj);
				gPuts(fobj, ");\n");
				gPuts(fobj, "\tva_end(_rest_);\n");
				if (!streq(rtn, "void"))
					gPuts(fobj, "\treturn _ret_;\n");
				gPuts(fobj, "}\n\n");
			}
		gPuts(fobj, "\n\n\n");
	}
#ifndef	DBI
	gPuts(fobj, "void\tInitGenerics()\n{\n");

	for (proto=gFindFirst(generics, &key) ; proto ; proto=gFindNext(generics, &key)) {
		char	*gn;
		gn = gStringValue(key);
		/*  don't init generics which have already been initialized by
		    the kernel!   */
		if (strne(gn, "gNew")  &&
		    strne(gn, "gNewClass")  &&
		    strne(gn, "gNewMethod")  &&
		    strne(gn, "gNewGeneric"))
			vPrintf(fobj, "\tInitGeneric( %s );\n", gn);
	}
	gPuts(fobj, "}\n\n\n");
	
	gPuts(fobj, "void\tInitDynace(void *s)\n");
	gPuts(fobj, "{\n");
	vPrintf(fobj, "\tint\tnClasses = %d;\n\n", gSize(classes));

	/*  use *2 since we must account for metaclasses  */
	gPuts(fobj, "\n\tInitKernel(s, nClasses*2);\n\n");
	
	gPuts(fobj, "}\n\n\n");
	
	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "}\n");
	gPuts(fobj, "#endif\n\n\n");
#endif
	gDispose(fobj);
}

/*  The following function creates the carry over generics.c file  */

static	void	make_c2(object	generics,
			char	*file,
			int	skip, 
			object	inc_sc,
			object	inc_ac)
{
	object	fobj, seq, obj, proto, key;
	char	*name, *rtn;
	int	n;
	
	rtn = file_extension(file);
	if (strne(rtn, "c")  &&
	    strne(rtn, "C")  &&
	    strne(rtn, "cc")  &&
	    strne(rtn, "CC")  &&
	    strne(rtn, "cpp")  &&
	    strne(rtn, "CPP"))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Generated generics.c file names must be valid C or C++ source file names.\n");
		return;
	}

	if (NULL == (fobj = open_file(file, WMODE, 0)))
		return;

	if (Copyright)
		copyright(fobj);
#ifdef	DBI
	gPuts(fobj, "\n//--------------------   generics.c   --------------------\n\n");
#else
	gPuts(fobj, "\n\n/*  This file is automatically generated by dpp and should not normally\n");
	gPuts(fobj,     "    be edited by hand.  */\n\n\n");

	if (inc_sc)  {
		for (seq = gSequence(inc_sc) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include <%s>\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (inc_ac)  {
		for (seq = gSequence(inc_ac) ; obj = gNext(seq) ; )
			vPrintf(fobj, "#include \"%s\"\n", gStringValue(obj));
		gPuts(fobj, "\n");
	}

	if (GenIncludes)
		gPuts(fobj, "#include \"generics.h\"\n\n");

	gPuts(fobj, "#ifdef\tDPP_STRATEGY\n");
	vPrintf(fobj, "#if\tDPP_STRATEGY != %d\n", Strategy);
	gPuts(fobj, "#error Incompatible DPP Strategies\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	gPuts(fobj, "#error DPP_STRATEGY not set.\n");
	gPuts(fobj, "#endif\n\n");

	gPuts(fobj, "#ifdef\tDPP_FASTWIDE\n");
	vPrintf(fobj, "#if\tDPP_FASTWIDE != %d\n", FastWideCache);
	gPuts(fobj, "#error Incompatible DPP FastWide Status\n");
	gPuts(fobj, "#endif\n");
	gPuts(fobj, "#else\n");
	gPuts(fobj, "#error DPP_FASTWIDE not set.\n");
	gPuts(fobj, "#endif\n\n");
	
	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "extern\t\"C\"  {\n");
	gPuts(fobj, "#endif\n\n\n");

	for (n=0, proto=gFindFirst(generics, &key) ; n < skip && proto ; proto=gFindNext(generics, &key), n++);
	for (n=0 ; n <  MultipleGenerics  &&  proto ; proto=gFindNext(generics, &key), n++)
		vPrintf(fobj, "object\tGeneric(%s);\n",	gStringValue(key));
	gPuts(fobj, "\n\n\n");

#endif
	if (Strategy < 3)  {
		for (n=0, proto=gFindFirst(generics, &key) ; n < skip && proto ; proto=gFindNext(generics, &key), n++);
		for (n=0 ; n <  MultipleGenerics  &&  proto ; proto=gFindNext(generics, &key), n++) {
			name = gStringValue(key);
			rtn = gStringValue(gReturnType(proto));
			if (Strategy == 1)  {
				vPrintf(fobj, "IGTYPE\t%s\t_%s(", rtn, name);
				gPrintArgs(proto, fobj);
				vPrintf(fobj, ")\n{\n\tGenericBody(%s);\n}\n\n", name);
			} else if (Strategy == 2)  {
				vPrintf(fobj, "static\t%s\t_%s(", rtn, name);
				gPrintArgs(proto, fobj);
				gPuts(fobj, ")\n{\n");
				if (FastWideCache)
					gPuts(fobj, "\tofun _meth_;\n");
				if (gIsVarArg(proto))
					if (streq(rtn, "void"))
						vPrintf(fobj, "\tva_list _rest_;\n\tva_start(_rest_, %s);\n", gLastArg(proto));
					else
						vPrintf(fobj, "\tva_list _rest_;\n\t%s _ret_;\n\tva_start(_rest_, %s);\n", rtn, gLastArg(proto));
				if (FastWideCache)  {
					vPrintf(fobj, "\tFW_GENERIC(%s);\n", name);
					vPrintf(fobj, "\t%s (*(%s_t)_meth_)(",
						streq(rtn, "void")?"":"return", name);
				} else {
					if (streq(rtn, "void"))
						vPrintf(fobj, "\t(*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else if (gIsVarArg(proto))
						vPrintf(fobj, "\t_ret_ = (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else
						vPrintf(fobj, "\treturn (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
				}
				gPrintVars(proto, fobj);
				gPuts(fobj, ");\n");
				if (gIsVarArg(proto)) {
					vPrintf(fobj, "\tva_end(_rest_);\n");
					if (!streq(rtn, "void"))
						vPrintf(fobj, "\treturn _ret_;\n");
				}
				gPuts(fobj, "}\n\n");
			}
		}
#ifndef	DBI
		gPuts(fobj, "\n\n\n");
	
		for (n=0, proto=gFindFirst(generics, &key) ; n < skip && proto ; proto=gFindNext(generics, &key), n++);
		for (n=0 ; n <  MultipleGenerics  &&  proto ; proto=gFindNext(generics, &key), n++) {
			name = gStringValue(key);
			vPrintf(fobj, "%s_t\t%s = _%s;\n", name, name, name);
		}
		gPuts(fobj, "\n\n\n");
#endif
	}
	
	if (Strategy == 4)  {
		for (n=0, proto=gFindFirst(generics, &key) ; n < skip && proto ; proto=gFindNext(generics, &key), n++);
		for (n=0 ; n <  MultipleGenerics  &&  proto ; proto=gFindNext(generics, &key), n++)
			if (gIsVarArg(proto))  {
				name = gStringValue(key);
				rtn = gStringValue(gReturnType(proto));
				vPrintf(fobj, "%s\t%s(", rtn, name);
				gPrintArgs(proto, fobj);
				gPuts(fobj, ")\n{\n");
				if (FastWideCache)
					gPuts(fobj, "\tofun _meth_;\n");
				if (streq(rtn, "void"))
					vPrintf(fobj, "\tva_list _rest_;\n\tva_start(_rest_, %s);\n", gLastArg(proto));
				else
					vPrintf(fobj, "\tva_list _rest_;\n\t%s\t_ret_;\n\tva_start(_rest_, %s);\n", rtn, gLastArg(proto));	
				if (FastWideCache)  {
					vPrintf(fobj, "\tFW_GENERIC(%s);\n", name);
					vPrintf(fobj, "\t%s (*(%s_t)_meth_)(",
						streq(rtn, "void")?"":"return", name);
				} else {
					if (streq(rtn, "void"))
						vPrintf(fobj, "\t(*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
					else
						vPrintf(fobj, "\t_ret_ = (*(%s_mt)_FindMethod(self, Generic(%s)))(", name, name);
				}
				gPrintVars(proto, fobj);
				gPuts(fobj, ");\n");
				gPuts(fobj, "\tva_end(_rest_);\n");
				if (!streq(rtn, "void"))
					gPuts(fobj, "\treturn _ret_;\n");
				gPuts(fobj, "}\n\n");
			}
		gPuts(fobj, "\n\n\n");
	}

#ifndef	DBI
	gPuts(fobj, "#ifdef\t__cplusplus\n");
	gPuts(fobj, "}\n");
	gPuts(fobj, "#endif\n\n\n");
#endif
	gDispose(fobj);
}

static	void
read_gens(object	classes,
	  object	generics,
	  char		*file,
	  object	exceptions,
	  int		errorOK)   /*  0=pass on errors,  1=open error ok,
				       2=all errors ok  */
{
	object	fobj;
	char	*tkn;
	int	save = ErrorCode;
	
	if (NULL == (fobj = open_file(file, RMODE, errorOK)))  {
		if (errorOK)
			ErrorCode = save;
		return;
	}

	gettoken(NULL);  /*  reset state info  */
/*	SeeCode = 0;	*/ /*  don't see between braces   */
	SeeCode = 1;	/*  C++ has { in linkage directive  */
	while ((tkn = gettoken(fobj))  &&  strne(tkn, "START_CLASSES"));
	while ((tkn = gettoken(fobj))  &&  strne(tkn, "END_CLASSES"))
		if (streq(tkn, "extern"))
			proc_gen_class(classes, fobj);
	while ((tkn = gettoken(fobj))  &&  strne(tkn, "START_PROTOTYPES"));
	while ((tkn = gettoken(fobj))  &&  strne(tkn, "END_PROTOTYPES"))
		if (streq(tkn, "typedef"))
			proc_gen_generic(generics, fobj, exceptions);
	gDispose(fobj);
	if (errorOK == 2)
		ErrorCode = save;
}

static	void	proc_gen_class(object classes, object fobj)
{
	object	cn, res;
	char	*tkn;
	
	tkn = gettoken(fobj);
	if (!tkn  ||  strne(tkn, "object"))
		return;
	tkn = gettoken(fobj);
	if (!tkn)
		return;
	cn = gNewWithStr(String, tkn);
	gDrop(cn, -2);  /*  remove trailing _c  */
	res = gAddValue(classes, cn, NoValue);
	if (res)
		DISPOSE(cn);
}

static	void	proc_gen_generic(object generics, object fobj, object exceptions)
{
	object	rtn, gen, args=NULL, proto;
	char	*tkn, buf[200], *gname;
	int	old, exception, vararg;
	
	*buf = '\0';
	while ((tkn = gettoken(fobj))  &&  *tkn != '(')  {
		if (*buf)
			strcat(buf, " ");
		strcat(buf, tkn);
	}
	rtn = gNewWithStr(String, buf);

	if (!(tkn = gettoken(fobj))  ||  *tkn != '*')  {
		DISPOSE(rtn);
		return;
	}

	if (!(tkn = gettoken(fobj))  ||  !istart(*tkn))  {
		DISPOSE(rtn);
		return;
	}
	gen = gNewWithStr(String, tkn);
	gDrop(gen, -2);	/*  get rid of trailing _t  */

	if (!(tkn = gettoken(fobj))  ||  *tkn != ')')  {
		DISPOSE(rtn);
		DISPOSE(gen);
		return;
	}

	if (!(tkn = gettoken(fobj))  ||  *tkn != '(')  {
		DISPOSE(rtn);
		DISPOSE(gen);
		return;
	}
	gname = gStringValue(gen);
	if (gname[strlen(gname)-1] == '_') {
		DISPOSE(rtn);
		DISPOSE(gen);
		return;
	}
	if (!gFindEQ(generics, gen, NULL)) {
		exception = NoProto || exceptions  &&  gFind(exceptions, gen);
		if (!exception)
			args = make_arg_list(fobj, &old, &vararg);
		proto = gNewWithStrStr(Prototype, gname, gStringValue(rtn));
		if (!exception)  {
			gSetArgs(proto, args);
			if (vararg)
				gVarArg(proto);
		} else
			gException(proto);
		add_generic(generics, gen, proto, 0);
	}
	DISPOSE(rtn);
	DISPOSE(gen);
}

static	void	add_generic(object generics, object gn, object proto, int check)
{
	char	buf[80], *gname = gStringValue(gn);

	if (check)
		if (gname[0] == 'g'  &&  isupper(gname[1]))  {
			strcpy(buf, gname);
			*buf = 'v';
			if (gFindEQ(generics, gn, NULL))  {
				vPrintf(stdoutStream, "Warning: Both generics %s and %s exist.\n", gname, buf);
	/*			ErrorCode = 1;   */
			}
		} else if (gname[0] == 'v'  &&  isupper(gname[1]))  {
			strcpy(buf, gname);
			*buf = 'g';
			if (gFindEQ(generics, gn, NULL))  {
				vPrintf(stdoutStream, "Warning: Both generics %s and %s exist.\n", gname, buf);
	/*			ErrorCode = 1;   */
			}
		}
	gAddValue(generics, gNewWithStr(String, gname), proto);
}

static	void	read_docs(object classes, object generics, char *file)
{
	object	fobj;
	char	buf[BUFSIZE], word[MAXWORDSZ+1], *next;
	
	if (NULL == (fobj = open_file(file, RMODE, 0)))
		return;

	while (gGets(fobj, buf, BUFSIZE))  {
		for (next=buf ; isspace(*next) ; ++next);
		if (!*next  ||  *next == ';')
			continue;
		if (!(next = getword(next, word, '\0')))
			continue;
		if (streq(word, "Classes:"))
			proc_doc_cls(classes, buf, word, fobj);
		if (streq(word, "Class:"))
			proc_doc_gen(generics, buf, word, fobj);
	}
	gDispose(fobj);
}

static	void	proc_doc_cls(object classes, char *buf, char *word, object fobj)
{
	char	*next;
	object	cn;
	
	while (gGets(fobj, buf, BUFSIZE))  {
		for (next=buf ; isspace(*next) ; ++next);
		if (!*next  ||  *next == ';')
			continue;
		if (!(next = getword(next, word, '\0')))
			continue;
		if (streq(word, "Class:"))
			return;
		cn = gNewWithStr(String, word);
		if (gAddValue(classes, cn, NoValue))
			DISPOSE(cn);
	}
}

static	void proc_doc_gen(object generics, char *buf, char *word, object fobj)
{
	char	*next, *p;
	object	type, proto, gn;
	
	while (gGets(fobj, buf, BUFSIZE))  {
		for (next=buf ; isspace(*next) ; ++next);
		if (!*next  ||  *next == ';')
			continue;
		for (p=next ; *p ; ++p)
			if (*p == ':')
				continue;
		if (!(next = gettype(next, word)))
			continue;
		type = gNewWithStr(String, word);
		next = getword(next, word, '\0');
		if (!next)  {
			DISPOSE(type);
			continue;
		}
		if (!gFindEQ(generics, gn=gNewWithStr(String, word), NULL))  {
			proto = gNewWithStrStr(Prototype, word, gStringValue(type));
			gVarArg(proto);
			add_generic(generics, gn, proto, 1);
		}
		DISPOSE(gn);
		DISPOSE(type);
	}
}

#define CODE_STATE	0	/*  in normal C code		*/
#define COMMENT_STATE	1	/*  in comment			*/
#define QUOTE_STATE	2	/*  in quoted string		*/

/*  This function copies FROM to TO without comments, strings or
    code between braces.  */

static	void	process_line(char *to, char *from)
{
	static	int	state;
	static	int	bracelevel;
	
	if (!to)  {  /*  reset state  */
		state = CODE_STATE;
		bracelevel = 0;
		return;
	}
	
	while (*from)
		switch (state)  {
		case CODE_STATE:
#if 0
			if (*from == '"')  {
				state = QUOTE_STATE;
				if (!bracelevel  ||  SeeCode)
					*to++ = ' '; /* replace string with space */
			} else
#endif
			if (from[0] == '/'  &&  from[1] == '*')  {
				state = COMMENT_STATE;
				from += 2;
				if (!bracelevel  ||  SeeCode)
					*to++ = ' '; /* replace comment with space */
			} else if (from[0] == '/'  &&  from[1] == '/')
				goto end;
			else if (*from == '\''  ||  *from == '"')  {
				char	type = *from;
				*to++ = *from++;
				for ( ; *from  &&  *from != type ; ++from)  {
					*to++ = *from;
					if (*from == '\\'  &&  from[1])
						*to++ = *++from;
				}
				if (*from)
					*to++ = *from++;
			} else if (*from == '{')  {
				if (!bracelevel++  ||  SeeCode)
					*to++ = *from;
				from++;
			} else if (*from == '}')  {
				if (bracelevel)  {
					if (!--bracelevel  ||  SeeCode)
						*to++ = *from;
				} else if (SeeCode)
					*to++ = *from;
				from++;
			} else if (!bracelevel  ||  SeeCode)
				*to++ = *from++;
			else
				from++;
			break;
		case COMMENT_STATE:
			if (from[0] == '*'  &&  from[1] == '/')  {
				state = CODE_STATE;
				from += 2;
			} else
				from++;
			break;
#if 0
		case QUOTE_STATE:
			if (from[0] == '\\'  &&  from[1])
				from++;
			else if (*from == '"')
				state = CODE_STATE;
			break;
#endif
		}
 end:
	*to = '\0';
}

static	void	read_exceptions(object exceptions, char *file)
{
	object	fobj, gen;
	char	*tkn;		/*  token				*/
	
	if (NULL == (fobj = open_file(file, RMODE, 0)))
		return;

	gettoken(NULL);  /*  reset state info  */
	SeeCode = 0;	/*  don't see between braces  */
	while (tkn=gettoken(fobj))
		if (istart(*tkn))  {
			gen = gNewWithStr(String, tkn);
			if (!gAdd(exceptions, gen))
				DISPOSE(gen);
		}
	gDispose(fobj);
}

static	void
read_src(object classes,
	 object generics,
	 char *file,
	 object exceptions)
{
	object	fobj;
	char	*tkn;		/*  token				*/
	object	ismethods;	/*  instance method + method prototype 	*/
	object	isgenerics;	/*  instance generic + method		*/
	object	csmethods;	/*  class method + method prototype  	*/
	object	csgenerics;	/*  class generic + method		*/
	char	initmeth[80];	/*  init method name			*/
	
	if (NULL == (fobj = open_file(file, RMODE, 0)))
		return;
	if (InKernel)
		strcpy(initmeth, "InitKernel");
	else
		*initmeth = '\0';
	gettoken(NULL);  /*  reset state info  */
	SeeCode = 0;	/*  don't see between braces until InitMethod  */
	isgenerics = gNewWithInt(StringDictionary, 301);
	ismethods  = gNewWithInt(StringDictionary, 301);
	csgenerics = gNewWithInt(StringDictionary, 301);
	csmethods  = gNewWithInt(StringDictionary, 301);
	while (tkn=gettoken(fobj))  {
		if (*tkn == '#')
			src_class(fobj, classes, initmeth);
		else if (streq(tkn, "imeth"))
			src_method(fobj, ismethods, 0);
		else if (streq(tkn, "ivmeth"))
			src_method(fobj, ismethods, 1);
		else if (streq(tkn, "cmeth"))
			src_method(fobj, csmethods, 0);
		else if (streq(tkn, "cvmeth"))
			src_method(fobj, csmethods, 1);
		else if (streq(tkn, "iMethod"))
			src_generic(fobj, isgenerics);
		else if (streq(tkn, "cMethod"))
			src_generic(fobj, csgenerics);
		else if (streq(tkn, "iMethodFor"))
			src_generic2(fobj, isgenerics);
		else if (streq(tkn, "cMethodFor"))
			src_generic2(fobj, csgenerics);
		else if (*initmeth  &&  streq(tkn, initmeth))
			SeeCode = 1;
	}
	gDispose(fobj);
	addto_generics(ismethods, isgenerics, generics, exceptions);
	addto_generics(csmethods, csgenerics, generics, exceptions);
	DEEPDISPOSE(ismethods);
	DEEPDISPOSE(isgenerics);
	DEEPDISPOSE(csmethods);
	DEEPDISPOSE(csgenerics);
}

static	void	src_class(object fobj, object classes, char *initmeth)
{
	object	cn, res;
	char	*tkn;

	tkn = gettoken(fobj);
	if (!tkn  ||  strne(tkn, "define"))  {
		gettoken(NULL);   /*  reset buffer - ignore line remainder  */
		return;
	}
	
	tkn = gettoken(fobj);
	if (!tkn  ||  strne(tkn, "CLASS"))  {
		gettoken(NULL);   /*  reset buffer - ignore line remainder  */
		return;
	}
	
	tkn = gettoken(fobj);
	if (!tkn  ||  !istart(*tkn))  {
		gettoken(NULL);   /*  reset buffer - ignore line remainder  */
		return;
	}
	cn = gNewWithStr(String, tkn);
	gDrop(cn, -2);  /*  get rid of trailing _c  */
	tkn = gStringValue(cn);
	sprintf(initmeth, "%s_initialize", tkn);
	res = gAddValue(classes, cn, NoValue);
	if (res)  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Class %s has been previously defined.\n", tkn);
		DISPOSE(cn);
	}
	gettoken(NULL);   /*  reset buffer - ignore line remainder  */
}

static	int
make_arg(object	fobj,
	 object args,
	 int	*old,       /*  old K&R style function delclarations  	*/
	 int	*vararg)    /*  set to 1 if fun is vararg		*/
{
	char	buf[256], *tkn;
	int	paren = 0;	/*  paren level  	*/
	int	n = 0;		/*  number of tokens	*/
	int	len;

	*buf = '\0';
	while ((tkn = gettoken(fobj))  &&  (len=(int)strlen(buf)) < 200)  {
		if (!paren  &&  (*tkn == ')'  ||  *tkn == ','))
			break;
		if (*tkn == '(')  {
			if (len  &&  irest(buf[len-1]))
				strcat(buf, " (");
			else
				strcat(buf, "(");
			paren++;
		} else if (*tkn == ')')  {
			strcat(buf, ")");
			if (paren)
				paren--;
		}  else  {
			if (*buf  &&  irest(buf[len-1]))
				strcat(buf, " ");
			strcat(buf, tkn);
			if (streq(tkn, "..."))  {
				n++;  /*  be sure not to think old style
					  because only one element  */
				*vararg = 1;
			}
			n++;
		}
	}
	if (!*buf)
		return 0;
	if (n <= 1)
		*old = 1;
	gAddLast(args, gNewWithStr(String, buf));
	return tkn  &&  *tkn != ')';
}

#define	OUT_COMMA					\
	if (!comma)  {					\
		gPut(sobj, gNewToken(Token, ",", 0L, 1));	\
		comma = 1;				\
	}


static	int
pp_make_arg(object	fobj,
	    object	args,
	    int		*old,      /*  old K&R style function delclarations  */
	    object	ivars,
	    object	cvars,
	    int		*convert)   /*  1=convert fixed args to varargs  */
{
	char	buf[256], *p=NULL;
	int	paren = 0;	/*  paren level  	*/
	int	n = 0;		/*  number of tokens	*/
	int	len, more;
	object	tkn, ptkn=NULL;

	*buf = '\0';
	while (1)  {
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		if (!tkn  ||  (len=(int)strlen(buf)) > 200)
			break;
		if (!paren  &&  (*p == ')'  ||  *p == ','))
			break;
		n++;
		if (*p == '(')  {
			if (len  &&  irest(buf[len-1]))
				strcat(buf, " (");
			else
				strcat(buf, "(");
			paren++;
		} else if (*p == ')')  {
			strcat(buf, ")");
			if (paren)
				paren--;
		}  else  {
			if (*buf  &&  irest(buf[len-1]))
				strcat(buf, " ");
			strcat(buf, p);
			if (istart(*p))  {
				object	sym = gToken(tkn);
				if (gFind(ivars, sym)  ||  gFind(cvars, sym)) {
					ErrorCode = 1;
					vPrintf(stdoutStream, "Argument var and instance/class var with same name - line %ld\n", gLineNumber(tkn));
				}
			}
			if (streq(p, "...")) 
				n++;  /*  be sure not to think old style
					  because only one element  */
		}
		if (ptkn)
			if (n == 2  &&  streq(p, "self"))  {
				n = 0;			/*  eat it  */
				*buf = '\0';
				ptkn = DISPOSE(ptkn);
				DISPOSE(tkn);
			} else {
				DISPOSE(ptkn);
				ptkn = tkn;
			}
		else
			ptkn = tkn;
	}
	if (ptkn)  {
		char	*t = gStringValue(ptkn);
		if (n == 1  &&  streq("self", t))  {
			*buf = '\0';		/*  eat it  */
			n = 0;
			DISPOSE(ptkn);
		} else if (Strategy != 1  &&  streq(t, "..."))  {
			*convert = 1;
			DISPOSE(ptkn);
		} else
			DISPOSE(ptkn);
	}
	more = tkn  &&  *p != ')';
	if (tkn)
		tkn = DISPOSE(tkn);

	if (!*buf)
		return more;
	if (n <= 1)
		*old = 1;
	if (n == 1)
		gAddLast(args, vSprintf(String, "object %s", buf));
	else
		gAddLast(args, gNewWithStr(String, buf));
	return more;
}

static	objrtn	make_arg_list(object fobj, int *old, int *vararg)
{
	object	args;
	int	n = 0, va=0;

	*vararg = *old = 0;
	args = gNew(LinkObject);
	while (make_arg(fobj, args, old, &va))  {
		if (n++ == 1  &&  va)
			*vararg = 1;
		va = 0;
	}
	return args;
}

static	objrtn
pp_make_arg_list(object	fobj,
		 int	*old,
		 object ivars,
		 object cvars,
		 int	convert)  /*  1=convert fixed args to varargs  */
{
	object	args;

	*old = 0;
	args = gNew(LinkObject);
	gAddLast(args, gNewWithStr(String, "object self"));  /*  always first arg  */
	while (pp_make_arg(fobj, args, old, ivars, cvars, &convert));
	return args;
}

static	void	src_method(object fobj, object methods, int vararg)
{
	char	type[256], mname[MAXWORDSZ+1], *tkn;
	object	prototype, args;
	int	old, va;

	*type = *mname = '\0';
	while (tkn = gettoken(fobj))  {
		if (*tkn == '(')  {
			if (!gFindStr(methods, mname))  {
				prototype = gNewWithStrStr(Prototype, mname, type);
				args = make_arg_list(fobj, &old, &va);
				if (!old)
					gSetArgs(prototype, args);
				else
					DEEPDISPOSE(args);
				if (vararg)
					gVarArg(prototype);
				tkn = gettoken(fobj);
				if (tkn  &&  *tkn != ','  &&  *tkn != ';')
					gAddStr(methods, mname, prototype);
				else
					DEEPDISPOSE(prototype);  /* it was a declaration  */
			}
			return;
		}

		if (*type)
			strcat(type, " ");
		if (streq(mname, "object"))
			strcat(type, "objrtn");
		else
			strcat(type, mname);
		strcpy(mname, tkn);
	}
}

static	void	method_syntax(object tkn, object generics, object olgens)
{
	if (tkn)  {
		vPrintf(stdoutStream, "Error: Bad method declaration syntax on line %ld\n", gLineNumber(tkn));
		DISPOSE(tkn);
	} else
		vPrintf(stdoutStream, "Error: Bad method declaration syntax\n");
	if (generics)
		DEEPDISPOSE(generics);
	if (olgens)
		DEEPDISPOSE(olgens);
	ErrorCode = 1;
}

static	objrtn	get_olname(object fobj, object tkn, char **pp)
{
	object	tt;
	char	*p;
	
	DISPOSE(tkn);
	tkn = gNextToken(fobj);
	if (!tkn)
		return NULL;
	p = gStringValue(tkn);
	if (!istart(*p))
		return DISPOSE(tkn);
	*pp = p;
	tt = gNextToken(fobj);
	if (!tt)
		return DISPOSE(tkn);
	p = gStringValue(tt);
	if (p[0] != '>'  ||  p[1])
		tkn = DISPOSE(tkn);
	DISPOSE(tt);
	return tkn;
}

static	void
generate_fixed_arg_meth(char 		*olmname,
			char 		*mname,
			object		prototype,
			int		mclass,
			object		sobj)
{
	char	*rtn;
	
	if (!prototype)
		return;
	if (mclass != 1)
		gFlush(sobj);
	gTLineDirective(sobj);
	vPrintf(sobj, "\nstatic\t%s\t%s(",
		rtn = gStringValue(gReturnType(prototype)),
		olmname);
	gPrintFixedArgs(prototype, sobj);
	gPuts(sobj, ")\n{\n");
	gPuts(sobj, "\tva_list\t_rest_;\n");
	if (!streq(rtn, "void"))
		vPrintf(sobj, "\t%s\t_ret_;\n", rtn);
	gPuts(sobj, "\tva_start(_rest_, ");
	vPrintf(sobj, "%s);\n", gLastArg(prototype));
/*	gUseVars(prototype, sobj);  */
	if (streq(rtn, "void"))
		vPrintf(sobj, "\t%s(", mname);
	else
		vPrintf(sobj, "\t_ret_ = %s(", mname);
	gPrintVars(prototype, sobj);
	gPuts(sobj, ");\n");
	gPuts(sobj, "\tva_end(_rest_);\n");
	if (!streq(rtn, "void"))
		gPuts(sobj, "\treturn _ret_;\n");
	gPuts(sobj, "}\n\n");
}

static	void
generate_fixed_arg_vmeth(char 		*olmname,
			 char 		*mname,
			 object		prototype,
			 int		mclass,
			 object		sobj)
{
	char	*rtn;
	
	if (!prototype)
		return;
	if (mclass != 1)
		gFlush(sobj);
	gTLineDirective(sobj);

	vPrintf(sobj, "\nstatic\t%s\t%s(object self, ...)\n{\n",
		rtn = gStringValue(gReturnType(prototype)),
		olmname);
	gPuts(sobj, "\tva_list\t_rest_;\n");
	if (!streq(rtn, "void"))
		vPrintf(sobj, "\t%s\t_ret_;\n", rtn);
	gPuts(sobj, "\tva_start(_rest_, self);\n");
	if (streq(rtn, "void"))
		vPrintf(sobj, "\t%s(", mname);
	else
		vPrintf(sobj, "\t_ret_ = %s(", mname);
	gPuts(sobj, "self, _rest_);\n");
	gPuts(sobj, "\tva_end(_rest_);\n");
	if (!streq(rtn, "void"))
		gPuts(sobj, "\treturn _ret_;\n");
	gPuts(sobj, "}\n\n");
}

static	void
generate_fixed_arg_vmeth2(char 		*olmname,
			 char 		*mname,
			 object		prototype,
			 int		mclass,
			 object		sobj)
{
	char	*rtn, *t;
	object	pvar, vvar, pseq, vseq, pnxt, vnxt;
	int	i=0;
	
	if (!prototype  ||  !(pvar = gPrototype(prototype)))
		return;
	if (!(vvar = gParameters(prototype)))
		return;
	pseq = gSequence(pvar);
	vseq = gSequence(vvar);
	if (mclass != 1)
		gFlush(sobj);
	gTLineDirective(sobj);

	vPrintf(sobj, "\nstatic\t%s\t%s(object self, ...)\n{\n",
		rtn = gStringValue(gReturnType(prototype)),
		olmname);
	if (gSize(gParameters(prototype)) > 1)  {
		gPuts(sobj, "\tva_list\t_rest_;\n");

		while (pnxt = gNext(pseq), vnxt = gNext(vseq))
			if (i++  &&  strne(t=gStringValue(pnxt), "..."))
				vPrintf(sobj, "\t%s\t%s;\n",
					t, gStringValue(vnxt));
		gPuts(sobj, "\tva_start(_rest_, self);\n");
		pseq = gSequence(pvar);
		vseq = gSequence(vvar);
		i = 0;
		while (pnxt = gNext(pseq), vnxt = gNext(vseq))
			if (i++  &&  strne(t=gStringValue(pnxt), "..."))
				vPrintf(sobj, "\t%s = va_arg(_rest_, %s);\n",
					gStringValue(vnxt), t);
		gPuts(sobj, "\tva_end(_rest_);\n");
	}  else  {
		DISPOSE(pseq);
		DISPOSE(vseq);
	}
	if (streq(rtn, "void"))
		vPrintf(sobj, "\t%s(", mname);
	else
		vPrintf(sobj, "\treturn %s(", mname);
	gPrintVars(prototype, sobj);
	gPuts(sobj, ");\n}\n\n");
}

static	void
generate_overloaded_method(char		*olmname,
			   object	olpt,
			   int		mclass,
			   object	sobj,
			   char		*mname,
			   object	prototype)
{
	int	i=0, rest_used = 0;
	char	*t, *rtn;
	object	pseq, vseq, pnxt, vnxt;
	
	if (!prototype  ||  !(pseq = gPrototype(prototype)))
		return;
	if (!(vseq = gParameters(prototype)))
		return;
	pseq = gSequence(pseq);
	vseq = gSequence(vseq);
	if (mclass != 1)
		gFlush(sobj);
	gTLineDirective(sobj);
	vPrintf(sobj, "\n%s\t%s\t%s(object self, va_list _rest_)\n",
		mclass == 1 ? "ivmeth" : "cvmeth",
		rtn = gStringValue(gReturnType(olpt)),
		olmname);
	gPuts(sobj, "{\n");
	while (pnxt = gNext(pseq), vnxt = gNext(vseq))
		if (i++  &&  strne(t=gStringValue(pnxt), "..."))  {
			vPrintf(sobj, "\t%s\t%s = va_arg(_rest_, %s);\n",
				t, gStringValue(vnxt), t);
			rest_used = 1;
		}
	if (!rest_used)
		gPuts(sobj, "\tUSE(_rest_);\n");
	if (streq(rtn, "void"))
		vPrintf(sobj, "\t%s(", mname);
	else
		vPrintf(sobj, "\treturn %s(", mname);
	gPrintVars(prototype, sobj);
	gPuts(sobj, ");\n");
	gPuts(sobj, "}\n\n");
}

static	void
pp_method(object  tkn,
	  object  fobj,
	  object  methods,
	  object  sgenerics,
	  object  sobj,
	  char	  *mtype,
	  int	  mclass,  /*  1=instance,  2=class method  */
	  int	  vararg,   /*  1= force varargs  */
	  object  ivars,
	  object  cvars,
	  object  className)
{
	char	type[256], mname[MAXWORDSZ+1], gname[MAXWORDSZ+1], *p=NULL, *g;
	char	olmname[MAXWORDSZ+1]; /* overloaded method name - name of dpp
				       generated method  */
	char	fmname[MAXWORDSZ+1];  /* fixed arg method name - generated  */
	char	ename[MAXWORDSZ+1];   /* explicit method name  */
	object	prototype, args;
	object	generics;	/*  linked list of generic names  */
	object	olgens;		/*  overloaded generics		  */
	object	seq, obj;	/*  used for sequencing through generics  */
	int	old;
	int	oload = 0;	/*  1=overloaded generic (no arg checking)  */
	object	olpt=NULL;	/*  overloaded prototype   */
	int	famf=0;		/*  1=at least 1 fixed argument method found */

	*type = *gname = *ename = '\0';
	gFlush(sobj);
	if (ExtraLineDirectives)
		gForceLineDirective(sobj);
	if (gLineHasColon(fobj))
		gChangeToken(tkn, "PMETHOD");  /*  for profiling, don't make non-name mangled methods global  */
	else
		gChangeToken(tkn, mtype);
	gPut(sobj, tkn);
	generics = gNew(LinkObject);
	olgens = gNew(LinkObject);
	while (1)  {
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		if (!tkn  ||  *p == '('  ||  *p == ':'  ||  *p == ',')
			break;

		if (p[0] == '<'  &&  !p[1])  {
			tkn = get_olname(fobj, tkn, &p);
			if (!tkn)
				break;
			oload = 1;
		}
		
		if (*type)
			strcat(type, " ");
		if (streq(gname, "object"))
			strcat(type, "objrtn");
		else
			strcat(type, gname);
		strcpy(gname, p);
		DISPOSE(tkn);
	}
	if (!*type)
		strcpy(type, "objrtn");
	if (!*gname  ||  !istart(*gname))  {
		method_syntax(tkn, generics, olgens);
		return;
	}
	if (oload)  {
		if (AllowOverloads  &&  !vararg)
			gAddLast(olgens, gNewWithStr(String, gname));
	}  else  {
		famf = 1;
		gAddLast(generics, gNewWithStr(String, gname));
	}
	if (tkn)
		if (*p == ',')  {
			DISPOSE(tkn);
			while (1)  {
				if (tkn = gNextToken(fobj))
					p = gStringValue(tkn);
				if (!tkn  ||  *p == '('  ||  *p == ':')
					break;
				if (*p == ',')  {
					DISPOSE(tkn);
					continue;
				}
				if (oload = p[0] == '<'  &&  !p[1])  {
					tkn = get_olname(fobj, tkn, &p);
					if (!tkn)
						break;
				}
				if (!istart(*p))  {
					method_syntax(tkn, generics, olgens);
					return;
				}
				if (oload)  {
					if (AllowOverloads  &&  !vararg)
						gAddLast(olgens, gNewWithStr(String, p));
				}  else  {
					famf = 1;
					gAddLast(generics, gNewWithStr(String, p));
				}
				DISPOSE(tkn);
			}
		}
	if (tkn  &&  *p == ':')  {
		DISPOSE(tkn);
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		if (tkn  &&  *p != '(')  {
			if (!istart(*p))  {
				method_syntax(tkn, generics, olgens);
				return;
			}
			strcpy(ename, p);
			sprintf(fmname, "%s_%cfm_%s", gStringValue(className),
				mclass==1?'i':'c', gname);
			sprintf(mname, "%s_%c%sm_%s", gStringValue(className),
				mclass==1?'i':'c', vararg?"v":"", gname);
			if (gSize(olgens))
				sprintf(olmname, "%s_%cvm_%s", gStringValue(className),
					mclass==1?'i':'c', mname);
			else
				*olmname = '\0';
			DISPOSE(tkn);
			if (tkn = gNextToken(fobj))
				p = gStringValue(tkn);
		}
	} else  {
		sprintf(mname, "%s_%c%sm_%s", gStringValue(className),
			mclass==1?'i':'c', vararg?"v":"", gname);
		sprintf(fmname, "%s_%cfm_%s", gStringValue(className),
			mclass==1?'i':'c', gname);
		if (gSize(olgens))
			sprintf(olmname, "%s_%cvm_%s", gStringValue(className),
				mclass==1?'i':'c', gname);
		else
			*olmname = '\0';
	}

	if (tkn  &&  *p == '(')  {
		
		prototype = gNewWithStrStr(Prototype, mname, type);
		args = pp_make_arg_list(fobj, &old, ivars, cvars, vararg);

		/*   never old!
		     if (old)
		     gDeepDispose(args);
		     else
		     */
		gSetArgs(prototype, args);
		if (vararg)
			gVarArg(prototype);

		if (*ename)
			if (Strategy != 1  &&  gIsVarArg(prototype))
				strcpy(fmname, ename);
			else  {
				strcpy(mname, ename);
				gChangeName(prototype, mname);
			}

		for (seq=gSequence(generics) ; obj=gNext(seq) ; )  {
			g = gStringValue(obj);
			if (!gFindStr(sgenerics, g))
				gAddStr(sgenerics, g, gNewWithStr(String, mname));
		}
		generics = DEEPDISPOSE(generics);

		if (Strategy == 1  &&  *olmname)  {
			*olmname = '\1';  /*  kludge to cause the same method
					      to have two signatures  */
			strcpy(olmname+1, mname);
		}
		for (seq=gSequence(olgens) ; obj=gNext(seq) ; )  {
			g = gStringValue(obj);
			if (!gFindStr(sgenerics, g))
				gAddStr(sgenerics, g, gNewWithStr(String, olmname));
		}
		olgens = DEEPDISPOSE(olgens);
	
		gPut(sobj, gNewToken(Token, type, 0L, 1));
		gPut(sobj, gNewToken(Token, mname, 0L, 0));
		gPut(sobj, tkn);
		gPrintMethArgs(prototype, sobj, Strategy, vararg);

		
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		if (tkn)
			if (*p == '{')  { /*  start of method defn  */
				if (famf)
					if (!gFindStr(methods, mname))
						gAddStr(methods, mname, prototype);
					else  {
						vPrintf(stdoutStream, "Duplicate definition for method %s\n", trunc_mname(mname));
						ErrorCode = 1;
						prototype = DEEPDISPOSE(prototype);
					}
				if (prototype  &&  !vararg  &&  *olmname)  {
					olpt = gDeepCopy(prototype);
					gChangeName(olpt, olmname);
					gVarArg(olpt);
					if (!gFindStr(methods, olmname))
						gAddStr(methods, olmname, olpt);
					else  {
						vPrintf(stdoutStream, "Duplicate definition for method %s\n", trunc_mname(olmname));
						ErrorCode = 1;
						olpt = DEEPDISPOSE(olpt);
					}
				}
			} else if (*p == ','  ||  *p == ';') /*  declaration  */
				prototype = DEEPDISPOSE(prototype);
			else  {	/*  old style func defn  */
				vPrintf(stdoutStream, "Method %s is using old style K&R function defn.\n", trunc_mname(mname));
				ErrorCode = 1;
				prototype = DEEPDISPOSE(prototype);
			}
		else
			prototype = DEEPDISPOSE(prototype);

		/*  process method body  */

		pp_body(p, fobj, sobj, mclass, tkn, vararg, prototype);
		if (Strategy != 1)  {
			if (prototype  &&  gIsVarArg(prototype))  {
				if (vararg)
					generate_fixed_arg_vmeth(fmname, mname, prototype, mclass, sobj);
				else
					generate_fixed_arg_meth(fmname, mname, prototype, mclass, sobj);
				gChangeFixedName(prototype, fmname);
			}
			if (olpt)  {
				int	optimize = gIsVarArg(prototype)  &&  2 == gSize(gArgs(prototype));
				if (prototype  &&  !optimize)  {
					char	olmname2[MAXWORDSZ+1];
					sprintf(olmname2, "_%s", olmname);
					generate_fixed_arg_vmeth2(olmname2, mname, prototype, mclass, sobj);
					gChangeFixedName(olpt, olmname2);
				} else if (gIsVarArg(prototype))
					gChangeFixedName(olpt, fmname);
				else
					gChangeFixedName(olpt, mname);
				if (optimize)
					gChangeName(olpt, mname);
				else
					generate_overloaded_method(olmname, olpt, mclass, sobj, mname, prototype);
				if (!famf)
					prototype = DEEPDISPOSE(prototype);
			}
		}
	} else {
/*		pp_body(p, fobj, sobj, 0, tkn, 0, NULL);  */
		gPut(sobj, gNewToken(Token, type, 0L, 1));
		gPut(sobj, gNewToken(Token, mname, 0L, 0));
		method_syntax(tkn, generics, olgens);
	}
}

static	void
pp_local_method(object  tkn,
		object  fobj,
		object  sobj,
		char	*mtype,
		int	mclass,  /*  1=instance,  2=class method  */
		object  ivars,
		object  cvars)
{
	char	type[256], mname[MAXWORDSZ+1], *p=NULL;
	object	args, prototype;
	int	old;

	*type = *mname = '\0';
	gFlush(sobj);
	if (ExtraLineDirectives)
		gForceLineDirective(sobj);
	gChangeToken(tkn, mtype);
	gPut(sobj, tkn);
	while (1)  {
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		if (!tkn  ||  *p == '(')
			break;
		if (*type)
			strcat(type, " ");
		if (streq(mname, "object"))
			strcat(type, "objrtn");
		else
			strcat(type, mname);
		strcpy(mname, p);
		DISPOSE(tkn);
	}
	if (!*type)
		strcpy(type, "objrtn");
	if (!*mname  ||  !istart(*mname))  {
		method_syntax(tkn, NULL, NULL);
		return;
	}

	gPut(sobj, gNewToken(Token, type, 0L, 1));
	gPut(sobj, gNewToken(Token, mname, 0L, 0));

	if (tkn  &&  *p == '(')  {
		gPut(sobj, tkn);
		
		prototype = gNewWithStrStr(Prototype, mname, type);
		args = pp_make_arg_list(fobj, &old, ivars, cvars, 0);

		gSetArgs(prototype, args);
		gPrintMethArgs(prototype, sobj, Strategy, 0);

		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);

		DEEPDISPOSE(prototype);

		/*  process method body  */

		pp_body(p, fobj, sobj, mclass, tkn, 0, NULL);

	} else
/*		pp_body(p, fobj, sobj, 0, tkn, 0, NULL);  */
		method_syntax(tkn, NULL, NULL);
}

#define	PUT(s, x)				\
        tkn = gNewToken(Token, x, 0L, s);	\
	if (mclass == 1)			\
		gPutm(sobj, tkn);		\
	else					\
		gPut(sobj, tkn)

#define	PUT_SEMI				\
        tkn = gNewToken(Token, ";", 0L, 0);	\
	if (mclass == 1)			\
		gPutm(sobj, tkn);		\
	else					\
		gPut(sobj, tkn)

static	void	expand_get_args(object	prototype,
				int	mclass,
				object	sobj)
{
	object	tkn, pseq, vseq, pnxt, vnxt;
	int	i = 0;
	char	*t;

	if (!(pseq = gPrototype(prototype)))
		return;
	if (!(vseq = gParameters(prototype)))
		return;
	pseq = gSequence(pseq);
	vseq = gSequence(vseq);
	if (mclass != 1)
		gFlush(sobj);
	while (pnxt = gNext(pseq), vnxt = gNext(vseq))
		if (i++  &&  strne(t=gStringValue(pnxt), "..."))  {
			PUT(1, t);
			PUT(1, gStringValue(vnxt));
			PUT(1, "=");
			PUT(0, "va_arg");
			PUT(0, "(");
			PUT(0, "_rest_");
			PUT(1, ",");
			PUT(0, t);
			PUT(0, ")");
			PUT_SEMI;
		}
/*
	PUT(0, "va_end");
	PUT(0, "(");
	PUT(0, "_rest_");
	PUT(0, ")");
	PUT_SEMI;
*/
}


/*  process between braces  */

static	void
pp_body(char	*p,
	object  fobj,
	object  sobj,
	int     mclass,   /*  1=instance,  2=class method  0=neither */
	object	tkn,
	int	convert,  /*  1=decode GET_ARGS		*/
	object	prototype)
{
	int	bracelevel = 1;

	if (!tkn)
		return;
	if (*p != '{')  {
		gPut(sobj, tkn);
		return;
	}

	if (mclass == 1)  {		/*  instance  */
		gFlush(sobj);
		gPutm(sobj, tkn);
	}  else
		gPut(sobj, tkn);
		
	if (ExtraLineDirectives  &&  !mclass) {
		gFlush(sobj);
		gForceLineDirective(sobj);
	}
	if (convert  &&  Strategy != 1  &&  prototype)
		expand_get_args(prototype, mclass, sobj);
			
	while (bracelevel  &&  tkn  &&  (tkn = gNextToken(fobj)))  {
		p = gStringValue(tkn);
		if (*p == '}'  &&  bracelevel)
			bracelevel--;
		else if (*p == '{')
			bracelevel++;
#if 0
		if (streq(p, "GET_ARGS"))  {
			if (convert  &&  Strategy != 1  &&  prototype)
				expand_get_args(prototype, mclass, sobj, gLineNumber(tkn));
			tkn = gNextToken(fobj);  /*  get rid of trailing ; */
		} else
#endif
		if (mclass == 1)
			gPutm(sobj, tkn);
		else
			gPut(sobj, tkn);
	}
	if (mclass == 1)
		gFlushm(sobj);
/*				causes problems with macros with several sets
				of braces
	else
		gFlush(sobj);
*/
}

static	void	src_generic(object fobj, object sgenerics)
{
	char	gname[MAXWORDSZ+2], *tkn;

	tkn = gettoken(fobj);
	if (!tkn  ||  *tkn != '(')
		return;
	
	tkn = gettoken(fobj);
	if (!tkn)
		return;

	sprintf(gname, "g%s", tkn);
	
	if (!gFindStr(sgenerics, gname))
		gAddStr(sgenerics, gname, gNewWithStr(String, tkn));
/*  it's ok because sometimes there is a class and instance method which use
    the same generic
	else
		vPrintf(stdoutStream, "Generic %s is associated with more than one method within a single source file\n", gname);
*/
}

static	void	src_generic2(object fobj, object sgenerics)
{
	char	cname[MAXWORDSZ+2], gname[MAXWORDSZ+2], *tkn;

	tkn = gettoken(fobj);
	if (!tkn  ||  *tkn != '(')
		return;
	
	tkn = gettoken(fobj);
	if (!tkn  ||  !istart(*tkn))
		return;
	strcpy(cname, tkn);

	tkn = gettoken(fobj);
	if (!tkn  ||  *tkn != ',')
		return;
	
	tkn = gettoken(fobj);
	if (!tkn  ||  !istart(*tkn))
		return;
	strcpy(gname, tkn);

	tkn = gettoken(fobj);
	if (!tkn  ||  *tkn != ',')
		return;

	tkn = gettoken(fobj);
	if (!tkn  ||  !istart(*tkn))
		return;
	
	if (!gFindStr(sgenerics, gname))
		gAddStr(sgenerics, gname, gNewWithStr(String, tkn));
/*  it's ok because sometimes there is a class and instance method which use
    the same generic
	else
		vPrintf(stdoutStream, "Generic %s is associated with more than one method within a single source file\n", gname);
*/
}

static	void
addto_generics(object smethods,		/*  source methods  		*/
	       object sgenerics,	/*  source generics		*/
	       object generics,		/*  global generics		*/
	       object exceptions)	/*  generic exceptions		*/
{
	object	seq, obj, mproto, gproto, mnobj, gnobj, gn;
	char	*gname, *mname;
	object	usedMethods;	/*  set of used methods  */

	usedMethods = gNewWithInt(Set, 101);
	for (seq = gSequence(sgenerics) ; obj = gNext(seq) ; )  {
		gname = gStringKey(obj);
		mname = gStringValue(mnobj=gValue(obj));
		mproto = gFindValueStr(smethods, mname);
		if (!mproto)  {
			ErrorCode = 1;
			vPrintf(stdoutStream, "Missing method definition %s in source file.\n", mname);
			continue;
		}
		gAdd(usedMethods, mnobj);
		gproto = gFindEQ(generics, gn=gKey(obj), NULL);
		if (!gproto)  {
			gproto = gDeepCopy(mproto);
			gChangeName(gproto, gname);
			gnobj = gNewWithStr(String, gname);
			if (NoProto  ||  gFind(exceptions, gnobj))
				gException(gproto);
			DISPOSE(gnobj);
			add_generic(generics, gn, gproto, 1);
		}  else
			ErrorCode |= gMatch(gproto, mproto);
	}
	for (seq = gSequence(smethods) ; obj = gNext(seq) ; )
		if (!InKernel  &&  !gFind(usedMethods, gGetMGName(gValue(obj))))  {
			ErrorCode = 1;
			vPrintf(stdoutStream, "Method %s has not been assigned to any generic.\n",
				gStringKey(obj));
		}
	DISPOSE(usedMethods);
}

static	void
public_meth(char	*p,
	    object	tkn,
	    object	fobj,
	    object	sobj,
	    object	ismethods,
	    object	csmethods,
	    object	isgenerics, 
	    object	csgenerics,
	    object	ivars,
	    object	cvars,
	    object	className)
{
	if (streq(p, "imeth"))
		pp_method(tkn, fobj, ismethods, isgenerics, sobj,
			  "imeth", 1, 0, ivars, cvars, className);
	else if (streq(p, "cmeth"))
		pp_method(tkn, fobj, csmethods, csgenerics, sobj,
			  "cmeth", 2, 0, ivars, cvars, className);
	else if (streq(p, "ivmeth"))
		pp_method(tkn, fobj, ismethods, isgenerics, sobj,
			  "ivmeth", 1, 1, ivars, cvars, className);
	else if (streq(p, "cvmeth"))
		pp_method(tkn, fobj, csmethods, csgenerics, sobj,
			  "cvmeth", 2, 1, ivars, cvars, className);
}

static	void
private_meth(char	*p,
	     object	tkn,
	     object	fobj,
	     object	sobj,
	     object	ivars,
	     object	cvars)
{
	if (streq(p, "imeth")  ||  streq(p, "ivmeth"))
		pp_local_method(tkn, fobj, sobj, "PMETHOD", 1, ivars, cvars);
	else if (streq(p, "cmeth")  ||  streq(p, "cvmeth"))
		pp_local_method(tkn, fobj, sobj, "PMETHOD", 2, ivars, cvars);
}

#define AMETH(p)	streq(p, "imeth")   ||		\
	                streq(p, "cmeth")   ||		\
			streq(p, "ivmeth")  ||		\
			streq(p, "cvmeth")

static	void
preprocess(object	classes,
	   object 	generics,
	   char		*file,
	   object	exceptions,
	   int		mkc)	/*  1=create .c file, 0=just parse  */
{
	object	fobj;		/*  source file (.d)			*/
	object	sobj;		/*  target source file (.c)		*/
	char	sfile[50];	/*  .c file name  			*/
	object	tkn;		/*  token				*/
	object	className;	/*  String				*/
	object	superClasses;	/*  LinkObject				*/
	object	idecls;		/*  instance declarations - LinkObject  */
	object	cdecls;		/*  class declarations - LinkObject	*/
	object	ivars;		/*  instance variables - Set		*/
	object	cvars;		/*  class variables - Set		*/
	object	init;		/*  init function - String		*/
	object	ismethods;	/*  instance method + method prototype 	*/
	object	isgenerics;	/*  instance generic + method		*/
	object	csmethods;	/*  class method + method prototype  	*/
	object	csgenerics;	/*  class generic + method		*/
	object	ismethods2;	/*  instance method + method prototype 	*/
	object	isgenerics2;	/*  instance generic + method		*/
	object	csmethods2;	/*  class method + method prototype  	*/
	object	csgenerics2;	/*  class generic + method		*/
	char	*p;		/*  pointer to token string		*/
	int	defclass = 0;	/*  number of defclass's		*/
	char	*ext;		/*  file extension			*/
	int	pub = 0;	/*  public keyword seen			*/
	int	priv = 0;	/*  private keyword seen		*/
	int	sciv = Create_iv;  /*  save it				*/
	int	sccv = Create_cv;  /*  save it				*/
	int	outstream=0;	/*  0=both, 1=S1, 2=S2			*/
	
	if (!class_source(file))  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "File %s should end with .d or .dd\n", file);
		return;
	}
	ext = file_extension(file);
	strcpy(sfile, file);
#ifdef	unix
	strcpy(sfile+(ext-file), ext[0] == ext[1] ? "cc" : "c");
#else
	strcpy(sfile+(ext-file), ext[0] == ext[1] ? "cpp" : "c");
#endif

	if (NULL == (fobj = gNewWithStr(InputStream, file)))
		return;
	if (NULL == (sobj = gNewWithStrStr(OutputStream2, mkc ? sfile : (char *) NULL, file)))  {
		gDispose(fobj);
		return;
	}
	if (*Sfile2)
		gOpenStream2(sobj,  Sfile2, file);
	if (Copyright)
		copyright(sobj);
#ifndef	DBI
	gPuts(sobj,"\n\n/*  This file automatically generated by dpp - do not edit  */\n\n");
	vPrintf(sobj, "#define\tDPP_STRATEGY\t%d\n", Strategy);
	vPrintf(sobj, "#define\tDPP_FASTWIDE\t%d\n\n\n", FastWideCache);
#endif

	className = gNew(String);
	superClasses = gNew(LinkObject);
	idecls = gNew(LinkObject);
	cdecls = gNew(LinkObject);
	ivars = gNewWithInt(Set, 21);
	cvars = gNewWithInt(Set, 21);
	init = gNew(String);

#define	ISGENERICS	(outstream == 2 ? isgenerics2 : isgenerics)
#define	ISMETHODS	(outstream == 2 ? ismethods2 : ismethods)
#define	CSGENERICS	(outstream == 2 ? csgenerics2 : csgenerics)
#define	CSMETHODS	(outstream == 2 ? csmethods2 : csmethods)
	
	isgenerics = gNewWithInt(StringDictionary, 301);
	ismethods  = gNewWithInt(StringDictionary, 301);
	csgenerics = gNewWithInt(StringDictionary, 301);
	csmethods  = gNewWithInt(StringDictionary, 301);

	isgenerics2 = gNewWithInt(StringDictionary, 301);
	ismethods2  = gNewWithInt(StringDictionary, 301);
	csgenerics2 = gNewWithInt(StringDictionary, 301);
	csmethods2  = gNewWithInt(StringDictionary, 301);
	while (tkn=gNextToken(fobj))  {
		p = gStringValue(tkn);
		if (streq(p, "public"))  {
			pub = 1;
			priv = 0;
			DISPOSE(tkn);
		} else if (streq(p, "private"))  {
			pub = 0;
			priv = 1;
			DISPOSE(tkn);
		} else if (streq(p, "#out1")) {
			gUseStream(sobj, outstream=1);
			DISPOSE(tkn);
		} else if (streq(p, "#out2")) {
			gUseStream(sobj, outstream=2);
			DISPOSE(tkn);
		} else if (streq(p, "#outboth")) {
			gUseStream(sobj, outstream=0);
			DISPOSE(tkn);
		} else if (streq(p, "defclass"))  {
			if (defclass++)  {
				vPrintf(stdoutStream, "File has multiple defclass's\n");
				ErrorCode = 1;
			}
			if (pub  &&  mkc)
				Create_iv = Create_cv = 1;
			pp_defclass(tkn, fobj, className, superClasses, idecls,
				    cdecls, ivars, cvars, init, classes);
#ifndef DBI
			pp_gen_head(sobj, className, idecls, cdecls,
				    ivars, cvars, file);
#else
			gPuts(sobj, "\n//--------------------   t.c   --------------------");
#endif
			gSetOSVars(sobj, className, cvars, ivars);
			pub = priv = 0;
		} else if (AMETH(p))  {
			if (!defclass)  {
				vPrintf(stdoutStream, "Attempt to define a method prior to a defclass\n");
				ErrorCode = 1;
			} else  {
				if (!priv) {
					if (!outstream)
						gUseStream(sobj, outstream=1);
					public_meth(p, tkn, fobj, sobj, ISMETHODS, CSMETHODS,
						    ISGENERICS, CSGENERICS, ivars, cvars, className);
				} else
					private_meth(p, tkn, fobj, sobj, ivars, cvars);
			}
			pub = priv = 0;
		}  else  {
			if (pub)
				gPut(sobj, gNewToken(Token, "public", 0L, 0));
			if (priv)
				gPut(sobj, gNewToken(Token, "private", 0L, 0));
			pp_body(p, fobj, sobj, 0, tkn, 0, NULL);
			pub = priv = 0;
		}
	}
	if (defclass  &&  !NoInitialize) {
		gUseStream(sobj, 1);
		pp_gen_initmethod(sobj, superClasses, init, isgenerics,
				  csgenerics, ismethods, csmethods,
				  ivars, cvars, className, 0);
		if (*Sfile2) {
			gUseStream(sobj, 2);
			pp_gen_initmethod(sobj, superClasses, init, isgenerics2,
					  csgenerics2, ismethods2, csmethods2,
					  ivars, cvars, className, 1);
		}
	}
	gDispose(fobj);
	gDispose(sobj);
	*Sfile2 = '\0';   /*  don't re-use it  */
	if (defclass)  {
		if (Create_iv)
			pp_gen_iv(file, className, idecls);
		if (Create_cv)
			pp_gen_cv(file, className, cdecls);
	}
	addto_generics(ismethods, isgenerics, generics, exceptions);
	addto_generics(csmethods, csgenerics, generics, exceptions);
	addto_generics(ismethods2, isgenerics2, generics, exceptions);
	addto_generics(csmethods2, csgenerics2, generics, exceptions);
#if 0
	gPuts(stdoutStream, "className = ");
	gPrint(className, stdoutStream);

	gPuts(stdoutStream, "\nsuperClasses = ");
	gPrint(superClasses, stdoutStream);

	gPuts(stdoutStream, "\nidecls = ");
	gPrint(idecls, stdoutStream);

	gPuts(stdoutStream, "\ncdecls = ");
	gPrint(cdecls, stdoutStream);
	
	gPuts(stdoutStream, "\nivars = ");
	gPrint(ivars, stdoutStream);
	
	gPuts(stdoutStream, "\ncvars = ");
	gPrint(cvars, stdoutStream);
	
	gPuts(stdoutStream, "\ninit = ");
	gPrint(init, stdoutStream);
	
	gPuts(stdoutStream, "\nisgenerics = ");
	gPrint(isgenerics, stdoutStream);
	
	gPuts(stdoutStream, "\nismethods = ");
	gPrint(ismethods, stdoutStream);
	
	gPuts(stdoutStream, "\ncsgenerics = ");
	gPrint(csgenerics, stdoutStream);
	
	gPuts(stdoutStream, "\ncsmethods = ");
	gPrint(csmethods, stdoutStream);
	
#endif
	DEEPDISPOSE(className);
	DEEPDISPOSE(superClasses);
	DEEPDISPOSE(idecls);
	DEEPDISPOSE(cdecls);
	DEEPDISPOSE(ivars);
	DEEPDISPOSE(cvars);
	DEEPDISPOSE(init);
	DEEPDISPOSE(ismethods);
	DEEPDISPOSE(isgenerics);
	DEEPDISPOSE(csmethods);
	DEEPDISPOSE(csgenerics);
	DEEPDISPOSE(ismethods2);
	DEEPDISPOSE(isgenerics2);
	DEEPDISPOSE(csmethods2);
	DEEPDISPOSE(csgenerics2);

	Create_iv = sciv;
	Create_cv = sccv;
}

static	void
pp_gen_head(object	sobj,
	    object	className,
	    object	idecls,
	    object	cdecls,
	    object	ivars,
	    object	cvars,
	    char	*file)
{
	object	seq, lnk;
/*	char	*p;	*/
	char	*cname;

	gFlush(sobj);
	gPuts(sobj, "\n");
	vPrintf(sobj, "#define\tCLASS\t%s_c\n", cname=gStringValue(className));
	vPrintf(sobj, "#define\tivType\t%s_iv_t\n", cname);
	gPuts(sobj, "\n");
	if (GenIncludes)
		gPuts(sobj, "#include \"generics.h\"\n\n");
	gUseStream(sobj, 2);
	vPrintf(sobj, "extern\t");
	gUseStream(sobj, 0);
	vPrintf(sobj, "object\t%s_c;\n\n", cname);
	if (gSize(idecls))
		if (Create_iv)
			vPrintf(sobj, "#include \"%s\"\n\n", new_ext(file, "iv"));
		else  {
			gTLineDirective(sobj);
			vPrintf(sobj, "typedef struct  _%s_iv_t  {\n", cname);
			for (seq=gSequence(idecls) ; lnk = gNext(seq) ; )
				vPrintf(sobj, "\t%s\n", gStringValue(lnk));
			vPrintf(sobj, "}\t%s_iv_t;\n\n", cname);
		}
	if (gSize(cdecls))  {
		if (Create_cv)
			vPrintf(sobj, "#include \"%s\"\n\n", new_ext(file, "cv"));
		else  {
			gTLineDirective(sobj);
			vPrintf(sobj, "typedef struct  _%s_cv_t  {\n", cname);
			for (seq=gSequence(cdecls) ; lnk = gNext(seq) ; )
				vPrintf(sobj, "\t%s\n", gStringValue(lnk));
			vPrintf(sobj, "}\t%s_cv_t;\n\n", cname);
		}
		vPrintf(sobj, "static\t%s_cv_t\t*%s_cv;\n\n", cname, cname);
	}
	USE(ivars);
	USE(cvars);
}

static	void
pp_gen_iv(char		*file,
	  object	className,
	  object	idecls)
{
	object	seq, lnk, sobj;
	char	*fname, macro[60];
	char	*cname;

	if (!gSize(idecls))
		return;
	fname = new_ext(file, "iv");
	sobj = open_file(fname, WMODE, 0);
	if (!sobj)
		return;

	make_macro_name(macro, fname);

	cname = gStringValue(className);

	vPrintf(sobj, "\n#ifndef\t%s\n", macro);
	vPrintf(sobj, "#define\t%s\n\n\n", macro);

	vPrintf(sobj, "typedef struct  _%s_iv_t  {\n", cname);
	for (seq=gSequence(idecls) ; lnk = gNext(seq) ; )
		vPrintf(sobj, "\t%s\n", gStringValue(lnk));
	vPrintf(sobj, "}\t%s_iv_t;\n\n", cname);

	vPrintf(sobj, "#endif\t/*  %s  */\n\n\n", macro);

	gDispose(sobj);
}

static	void
pp_gen_cv(char		*file,
	  object	className,
	  object	cdecls)
{
	object	seq, lnk, sobj;
	char	*fname, macro[60];
	char	*cname;

	if (!gSize(cdecls))
		return;
	fname = new_ext(file, "cv");
	sobj = open_file(fname, WMODE, 0);
	if (!sobj)
		return;

	make_macro_name(macro, fname);

	cname = gStringValue(className);
	
	vPrintf(sobj, "\n#ifndef\t%s\n", macro);
	vPrintf(sobj, "#define\t%s\n\n\n", macro);

	vPrintf(sobj, "typedef struct  _%s_cv_t  {\n", cname);
	for (seq=gSequence(cdecls) ; lnk = gNext(seq) ; )
		vPrintf(sobj, "\t%s\n", gStringValue(lnk));
	vPrintf(sobj, "}\t%s_cv_t;\n\n", cname);

	vPrintf(sobj, "#endif\t/*  %s  */\n\n\n", macro);

	gDispose(sobj);
}

static	void	pp_gen_links(char   type,
			     object sgenerics,
			     object methods,
			     object sobj,
			     char   *cname)
{
	object	seq, lnk, mproto, fixedo;
	char	*gen, *meth, *fname;

	for (seq=gSequence(sgenerics) ; lnk=gNext(seq) ; )  {
		/*  lnk is a StringAssociation  */
		gen = gStringKey(lnk);
		meth = gStringValue(gValue(lnk));
		mproto = gFindValueStr(methods, meth);
		if (mproto) {  /*  can be NULL if code has a methos declaration but no definition  */
			/*  in certain curcumstance this name is different!  */
			meth = gStringValue(gGetMGName(mproto));
			if (*meth == '\1')
				meth++;  /*  the other half of a kludge to allow one
					     method name to have two signatures.  */
			if (gIsVarArg(mproto))  {
				fixedo = gGetFixedName(mproto);
				fname = fixedo ? gStringValue(fixedo) : meth;
				vPrintf(sobj, "\t%cvMethodFor(%s, %s, %s, %s);\n", type, cname, gen, meth, fname);
			} else
				vPrintf(sobj, "\t%cMethodFor(%s, %s, %s);\n", type, cname, gen, meth);
		}
	}
}

static	void
pp_gen_initmethod(object  sobj,
		  object  superClasses,
		  object  init,
		  object  isgenerics,
		  object  csgenerics,
		  object  ismethods,
		  object  csmethods, 
		  object  ivars,
		  object  cvars,
		  object  className,
		  int	  sup)
{
	object	seq, lnk;
	char	*cname = gStringValue(className);

	gFlush(sobj);
#ifndef	DBI
	gTLineDirective(sobj);

	if (!sup  &&  *Sfile2)
		vPrintf(sobj, "\nextern\tobjrtn\t%s_initialize2(void);\n\n", cname);
	
	if (sup)
		vPrintf(sobj, "\nobjrtn\t%s_initialize2(void)\n", cname);
	else
		vPrintf(sobj, "\nobjrtn\t%s_initialize(void)\n", cname);
	gPuts(sobj, "{\n");
	if (!sup) {
		gPuts(sobj, "\tstatic  CRITICALSECTION  cs;\n");
		gPuts(sobj, "\tstatic  int volatile once = 0;\n\n");

		gPuts(sobj, "\tENTERCRITICALSECTION(_CI_CS_);\n");
		gPuts(sobj, "\tif (!once) {\n");
		gPuts(sobj, "\t\tINITIALIZECRITICALSECTION(cs);\n");
		gPuts(sobj, "\t\tonce = 1;\n");
		gPuts(sobj, "\t}\n");
		gPuts(sobj, "\tLEAVECRITICALSECTION(_CI_CS_);\n\n");

		gPuts(sobj, "\tENTERCRITICALSECTION(cs);\n\n");

		vPrintf(sobj, "\tif (%s_c) {\n", cname);
		gPuts(sobj, "\t\tLEAVECRITICALSECTION(cs);\n");
		vPrintf(sobj, "\t\treturn %s_c;\n", cname);
		gPuts(sobj, "\t}\n");
		gPuts(sobj, "\tINHIBIT_THREADER;\n");

		if (gSize(superClasses))
			for (seq=gSequence(superClasses) ; lnk = gNext(seq) ; )  {
				vPrintf(sobj, "\t%s_initialize();\n", gStringValue(lnk));
				vPrintf(sobj, "\tif (%s_c)  {\n", cname);
				gPuts(sobj, "\t\tENABLE_THREADER;\n");
				gPuts(sobj, "\t\tLEAVECRITICALSECTION(cs);\n");
				vPrintf(sobj, "\t\treturn %s_c;\n", cname);
				gPuts(sobj, "\t}\n");
			}

		vPrintf(sobj, "\t%s_c = gNewClass(Class, \"%s\", ", cname, cname);
		if (gSize(ivars))
			vPrintf(sobj, "sizeof(%s_iv_t), ", cname);
		else
			gPuts(sobj, "0, ");
		if (gSize(cvars))
			vPrintf(sobj, "sizeof(%s_cv_t), ", cname);
		else
			gPuts(sobj, "0, ");
		if (gSize(superClasses))
			for (seq=gSequence(superClasses) ; lnk = gNext(seq) ; )
				vPrintf(sobj, "%s, ", gStringValue(lnk));
		gPuts(sobj, "END);\n");
	}
#endif
	
	pp_gen_links('c', csgenerics, csmethods, sobj, cname);

	pp_gen_links('i', isgenerics, ismethods, sobj, cname);

	if (!sup  &&  *Sfile2)
		vPrintf(sobj, "\n\t%s_initialize2();\n", cname);

#ifndef	DBI
	gPuts(sobj, "\n");

	if (gSize(cvars))
		vPrintf(sobj, "\t%s_cv = GetCVs(%s);\n\n", cname, cname);

	if (!sup) {
		if (gSize(init))
			vPrintf(sobj, "\t%s();\n\n", gStringValue(init));

		gPuts(sobj, "\tENABLE_THREADER;\n\n");
		gPuts(sobj, "\tLEAVECRITICALSECTION(cs);\n\n");
	}
	vPrintf(sobj, "\treturn %s_c;\n", cname);
	gPuts(sobj, "}\n\n\n");
#else
	USE(lnk);
	USE(seq);
#endif
}

static	int	valid_symbol(char *tkn)
{
	if (!tkn  ||  !istart(*tkn))
		return 0;
	for (++tkn ; *tkn ; ++tkn)
		if (!irest(*tkn))
			return 0;
	return 1;
}

#define INSTANCE_SECTION	1
#define CLASS_SECTION		2
#define INIT_SECTION		3

static	void
pp_defclass(object	tkn,
	    object	fobj,
	    object	className,
	    object	superClasses,
	    object	idecls,
	    object	cdecls,
	    object	ivars,
	    object	cvars,
	    object	init,
	    object	classes)
{
	char	*p=NULL;

	DISPOSE(tkn);
	if (tkn = gNextToken(fobj))
		p = gStringValue(tkn);
	if (!tkn  ||  !valid_symbol(p))  {
		ErrorCode = 1;
		gPuts(stdoutStream, "Class name must follow defclass.\n");
		DISPOSE(tkn);
		return;
	}
	gChangeStrValue(className, p);
	gAddValue(classes, gCopy(className), NoValue);
	DISPOSE(tkn);

	while (1)  {
		tkn = gNextToken(fobj);
		if (!tkn)
			break;
		p = gStringValue(tkn);
		if (*p == '{'  ||  *p == ';')
			break;
		if (*p == ':'  ||  *p == ',')  {
			DISPOSE(tkn);
			continue;
		}
		if (!valid_symbol(p))  {
			ErrorCode = 1;
			vPrintf(stdoutStream, "Superclass name must follow class name.\n");
			DISPOSE(tkn);
			return;
		}
		gAddLast(superClasses, gNewWithStr(String, p));
		DISPOSE(tkn);
	}

	if (tkn  &&  *p == '{')  {
		int	mode = INSTANCE_SECTION;
		DISPOSE(tkn);
		while (1)  {
			tkn = gNextToken(fobj);
			if (!tkn)
				break;
			p = gStringValue(tkn);
			if (*p == '}')
				break;
			if (streq(p, "instance"))  {
				DISPOSE(tkn);
				tkn = gNextToken(fobj);
				if (!tkn)
					break;
				p = gStringValue(tkn);
				if (*p != ':')  {
					ErrorCode = 1;
					vPrintf(stdoutStream, "Missing : after instance in defclass.\n");
					DISPOSE(tkn);
					return;
				}
				DISPOSE(tkn);
				mode = INSTANCE_SECTION;
				continue;
			}
			if (streq(p, "class"))  {
				DISPOSE(tkn);
				tkn = gNextToken(fobj);
				if (!tkn)
					break;
				p = gStringValue(tkn);
				if (*p != ':')  {
					ErrorCode = 1;
					vPrintf(stdoutStream, "Missing : after class in defclass.\n");
					DISPOSE(tkn);
					return;
				}
				DISPOSE(tkn);
				mode = CLASS_SECTION;
				continue;
			}
			if (streq(p, "init"))  {
				DISPOSE(tkn);
				tkn = gNextToken(fobj);
				if (!tkn)
					break;
				p = gStringValue(tkn);
				if (*p != ':')  {
					ErrorCode = 1;
					vPrintf(stdoutStream, "Missing : after init in defclass.\n");
					DISPOSE(tkn);
					return;
				}
				DISPOSE(tkn);
				mode = INIT_SECTION;
				continue;
			}
			switch (mode)  {
			case INSTANCE_SECTION:
				pp_decl_section(tkn, p, fobj, idecls, ivars, cvars);
				break;
			case CLASS_SECTION:
				pp_decl_section(tkn, p, fobj, cdecls, cvars, ivars);
				break;
			case INIT_SECTION:
				pp_init_section(tkn, p, fobj, init);
				break;
			}
		}
		if (tkn)  {
			DISPOSE(tkn);
			if (tkn = gNextToken(fobj))   /*  should be last ;  */
				p = gStringValue(tkn);
		}
	}
	if (!tkn)  {
		ErrorCode = 1;
		gPuts(stdoutStream, "Incomplete defclass section.\n");
	} else 	if (*p != ';')  {
		ErrorCode = 1;
		gPuts(stdoutStream, "Missing ; at end of defclass.\n");
		DISPOSE(tkn);
	} else
		DISPOSE(tkn);
}

static	void
pp_decl_section(object	tkn,
		char	*p,
		object	fobj,
		object	idecls,
		object	ivars, 	  /*  instance or class variable to add to  */
		object	ovars)	  /*  other var list which should not have dup */
{
	char	buf[256], var[100];
	int	numsym;

	strcpy(buf, p);
	DISPOSE(tkn);
	while (1)  {
		if (tkn = gNextToken(fobj))
			p = gStringValue(tkn);
		else
			break;
		if (*p == ';'  ||  *p == '}')
			break;
		strcat(buf, " ");
		strcat(buf, p);
		DISPOSE(tkn);
	}
	if (!tkn  ||  *p != ';')  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Missing ; after variable declaration in defclass.\n");
		if (tkn)
			DISPOSE(tkn);
		return;
	}
	DISPOSE(tkn);
	numsym = get_var(var, buf);
	strcat(buf, ";");
	if (numsym == 1) {
		memmove(buf+7, buf, strlen(buf)+1);
		memcpy(buf, "object ", 7);
	}
/*	add_underscore(buf);		*/
	gAddLast(idecls, gNewWithStr(String, buf));
	if (!gAdd(ivars, tkn=gNewWithStr(String, var)))  {
		vPrintf(stdoutStream, "Class/instance variable name %s is multiply defined.\n", gStringValue(tkn));
		ErrorCode = 1;
		DISPOSE(tkn);
		return;
	}
	if (gFind(ovars, tkn))  {
		vPrintf(stdoutStream, "Class/instance variable name %s is multiply defined.\n", gStringValue(tkn));
		ErrorCode = 1;
	}
}

static	int	get_var(char *var, char *buf)
{
	char	*p, *e, save='\0', *sp = NULL;
	int	numsym = 0;

	*var = '\0';

	/*  get rid of arguments to pointers to functions  */

	for (p=buf ; *p ; ++p)
		if (*p == ')'  &&  p[1] == ' '  &&  p[2] == '('  ||  *p == '[')  {
			sp = p;
			save = *sp;
			*sp = '\0';
			break;
		}

	/*  find begining and end of last symbol  */

	for (e=p=NULL ; *buf ; )
		if (istart(*buf))  {
			p = buf++;
			while (irest(*buf))
				buf++;
			e = buf;
			numsym++;
		} else
			buf++;

	if (p)  {
		char	s = *e;
		*e = '\0';
		strcpy(var, p);
		*e = s;
	}  else  {
		ErrorCode = 1;
		vPrintf(stdoutStream, "Invalid variable declaration in defclass.\n");
	}
	if (sp)
		*sp = save;
	return numsym;
}

#if 0

static	void	add_underscore(char *buf)
{
	char	*p, *e, save, *sp = NULL;

	/*  get rid of arguments to pointers to functions  */

	for (p=buf ; *p ; ++p)
		if (*p == ')'  &&  p[1] == ' '  &&  p[2] == '(')  {
			sp = p;
			save = *sp;
			*sp = '\0';
			break;
		}

	/*  find begining and end of last symbol  */

	for (p=NULL ; *buf ; buf++)
		if (istart(*buf))  {
			p = buf++;
			while (irest(*buf))
				buf++;
			e = buf;
		}

	if (sp)
		*sp = save;
	if (p)  {
		memmove(p+1, p, strlen(p)+1);
		*p = '_';
	}
}

#endif

static	void	pp_init_section(object tkn, char *p, object fobj, object init)
{
	if (!valid_symbol(p))  {
		ErrorCode = 1;
		gPuts(stdoutStream, "Invalid init syntax in defclass.\n");
		DISPOSE(tkn);
		return;
	}
	gChangeStrValue(init, p);
	DISPOSE(tkn);
	if (tkn = gNextToken(fobj))
		p = gStringValue(tkn);
	if (!tkn  ||  *p != ';')  {
		ErrorCode = 1;
		gPuts(stdoutStream, "Missing ; in init section of defclass.\n");
	}
	if (tkn)
		DISPOSE(tkn);
}

static	char	*getword(char *s, char *w, char c)
{
	register int	i;
	
	while (*s  &&  !istart(*s))
		++s;
	if (!*s)
		return NULL;
	for (i=0 ; ++i <= MAXWORDSZ && (irest(*s)  ||  *s == ':'  ||  c  &&  *s != c) ; )
		*w++ = *s++;
	*w = '\0';
	return s;
}

/*  get a line - append continuation lines - get rid of comments, strings
    and code between braces  */

static	int	get_line(object fobj, char *line)
{
	char	*tbuf;
	static	char	buf[10000];
	int	something = 0, i;

	tbuf = buf;
	while (1)  {
		if (!gGets(fobj, tbuf, sizeof buf))
			break;
		CurrentLine++;
		something = 1;
		i = (int)strlen(tbuf) - 1;
		while (i >= 0  &&  (tbuf[i] == '\n' ||  tbuf[i] == '\r'))
			i--;
		if (i < 0  ||  tbuf[i] != '\\')  {
			tbuf[i+1] = '\0';
			break;
		}
		tbuf[i] = '\0';
		tbuf += i;
	}
	process_line(line, buf);
	return something;  /*  0=eof  */
}

#define IS(a, b)	*s == a  &&  s[1] == b

static	char	*gettoken(object fobj)
{
	register int	i;
	char	*w;
	static	char	buf[10000], token[BUFSIZE], *s;

	if (!fobj)  {		/*  reset state  */
		s = buf;
		*buf = '\0';
		CurrentLine = 0;
		process_line(NULL, NULL); /*  init to code state  */
		return NULL;
	}
	while (isspace(*s)  ||  !*s)
		if (!*s)  {
			if (get_line(fobj, buf))
				s = buf;
			else
				return NULL; /*  no more  */
		} else
			s++;
	w = token;
	if (istart(*s))		/*  is identifier  */
		for (i=0 ; ++i <= MAXWORDSZ  &&  irest(*s) ;)
			*w++ = *s++;
	else			/*  something other than an identifier  */

		if (*s == '<'  &&  s[1] == '<'  &&  s[2] == '='  ||
		    *s == '.'  &&  s[1] == '.'  &&  s[2] == '.'  ||
		    *s == '>'  &&  s[1] == '>'  &&  s[2] == '=')  {
			*w++ = *s++;
			*w++ = *s++;
			*w++ = *s++;
		} else if (IS('*', '=')  ||
			   IS('/', '=')  ||
			   IS('%', '=')  ||
			   IS('+', '=')  ||
			   IS('-', '=')  ||
			   IS('&', '=')  ||
			   IS('^', '=')  ||
			   IS('|', '=')  ||
			   IS('&', '&')  ||
			   IS('|', '|')  ||
			   IS('=', '=')  ||
			   IS('!', '=')  ||
			   IS('<', '<')  ||
			   IS('>', '>')  ||
			   IS('<', '=')  ||
			   IS('>', '=')  ||
			   IS('-', '-')  ||
			   IS('+', '+')  ||
			   IS('-', '>'))  {
			*w++ = *s++;
			*w++ = *s++;
		} else if (*s == '"'  ||  *s == '\'')  {
			char	type = *s;
			*w++ = *s++;
			while (*s  &&  *s != type)  {
				*w++ = *s;
				if (*s == '\\'  &&  s[1])
					*w++ = *++s;
				++s;
			}
			if (*s)
				*w++ = *s++;
		} else if (isdigit(*s)  ||  *s == '.'  &&  isdigit(s[1]))  {
			while (isdigit(*s)  ||  *s == '.'  ||  *s == 'e'  ||  *s == 'E'
			       ||  ((*s=='-' ||  *s=='+')  &&
				    (s[-1] == 'e'  ||  s[-1] == 'E')) )
				*w++ = *s++;
			if (*s == 'u'  ||  *s == 'U'  ||  *s == 'l'  ||  *s == 'L')
				*w++ = *s++;
			if (*s == 'u'  ||  *s == 'U'  ||  *s == 'l'  ||  *s == 'L')
				*w++ = *s++;
		} else
			*w++ = *s++;
	*w = '\0';
	/*  printf("Token = %s\n", token);  */
	return token;
}

static	char	*gettype(char *s, char *w)
{
	register int	i;
	char	*b, *bw = w;
	
	while (*s  &&  isspace(*s))
		++s;
	if (!*s  ||  !istart(*s))
		return NULL;
	b = s;
	while (*s  &&  *s != '(')
		++s;
	if (*s != '(')
		return NULL;
	while (s > b  &&  !irest(*s))
		--s;
	while (s > b  &&  irest(*s))
		--s;
	if (s == b)
		return NULL;
	while (s > b  &&  isspace(*s))
		--s;
	++s;
	for (i=0 ; ++i <= MAXWORDSZ  &&  b < s ; )
		*w++ = *b++;
	*w = '\0';
	compress(bw);
	return s;
}

static	void	compress(char *f)
{
	int	state = 1;	/* 1=not space, 0=space  */
	char	*t = f;

	for ( ; *f ; ++f)
		if (state)
			if (isspace(*f))  {
				state = 0;
				*t++ = ' ';
			}  else 
				*t++ = *f;
		else
			if (!isspace(*f))  {
				*t++ = *f;
				state = 1;
			}
}

#ifdef	unix
#define	RPLUS	"r+"
#else
#define RPLUS	"r+b"
#endif

static	int	touch(char *f)
{
	FILE	*fp;
	int	c, err=0;

	if (fp = fopen(f, RPLUS))  {
		fseek(fp, 0L, 2);
		if (ftell(fp) == 0L)
			err = 1;
		else  {
			fseek(fp, 0L, 0);
			c = fgetc(fp);
			fseek(fp, 0L, 0);
			fputc(c, fp);
		}
		fclose(fp);
	}   else
		err = 1;
	if (!err  &&  !Quiet)
		vPrintf(stdoutStream, "Touched %s\n", f);
	return err;
}

char	*trunc_mname(char *mname)
{
	char	*p = mname;

	while (*p  &&  *p != '_')
		p++;
	return *p ? p+1 : mname;
}

#if 0

char	*trunc_mname(char *mname)
{
	static	char	buf[80];
	int	len = strlen(mname), p = strlen(MPREFIX), s = strlen(MSUFFIX);

	if (p  &&  !strncmp(MPREFIX, mname, p))  {
		mname += p;
		len -= p;
	}
	strcpy(buf, mname);
	if (s  &&  len > s  &&  !strncmp(MSUFFIX, buf+(len-s), s))
		buf[len-s] = '\0';
	return buf;
}

static	objrtn	set_name(char *p, object seq)
{
	object	arg = gNext(seq);
	if (!arg)
		return arg;
	if (p[2] == 'p')
		strcpy(MPREFIX, gStringValue(arg));
	else if (p[2] == 's')
		strcpy(MSUFFIX, gStringValue(arg));
	else 
		return arg;
	return gNext(seq);
}	

#endif
	
static	void	copyright(object fobj)
{
	gPuts(fobj, "/*\n");
	gPuts(fobj, "  Copyright (c) 1996 Blake McBride\n");
	gPuts(fobj, "  All rights reserved.\n");
	gPuts(fobj, "\n");
	gPuts(fobj, "  Redistribution and use in source and binary forms, with or without\n");
	gPuts(fobj, "  modification, are permitted provided that the following conditions are\n");
	gPuts(fobj, "  met:\n");
	gPuts(fobj, "\n");
	gPuts(fobj, "  1. Redistributions of source code must retain the above copyright\n");
	gPuts(fobj, "  notice, this list of conditions and the following disclaimer.\n");
	gPuts(fobj, "\n");
	gPuts(fobj, "  2. Redistributions in binary form must reproduce the above copyright\n");
	gPuts(fobj, "  notice, this list of conditions and the following disclaimer in the\n");
	gPuts(fobj, "  documentation and/or other materials provided with the distribution.\n");
	gPuts(fobj, "\n");
	gPuts(fobj, "  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n");
	gPuts(fobj, "  \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n");
	gPuts(fobj, "  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n");
	gPuts(fobj, "  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n");
	gPuts(fobj, "  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n");
	gPuts(fobj, "  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n");
	gPuts(fobj, "  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n");
	gPuts(fobj, "  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n");
	gPuts(fobj, "  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n");
	gPuts(fobj, "  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n");
	gPuts(fobj, "  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n");
	gPuts(fobj, "*/\n\n");
}

static	void	schemecopyright(object fobj)
{
	gPuts(fobj, "\n");
	gPuts(fobj, ";  Copyright (c) 1996 Blake McBride\n");
	gPuts(fobj, ";  All rights reserved.\n");
	gPuts(fobj, ";\n");
	gPuts(fobj, ";  Redistribution and use in source and binary forms, with or without\n");
	gPuts(fobj, ";  modification, are permitted provided that the following conditions are\n");
	gPuts(fobj, ";  met:\n");
	gPuts(fobj, ";\n");
	gPuts(fobj, ";  1. Redistributions of source code must retain the above copyright\n");
	gPuts(fobj, ";  notice, this list of conditions and the following disclaimer.\n");
	gPuts(fobj, ";\n");
	gPuts(fobj, ";  2. Redistributions in binary form must reproduce the above copyright\n");
	gPuts(fobj, ";  notice, this list of conditions and the following disclaimer in the\n");
	gPuts(fobj, ";  documentation and/or other materials provided with the distribution.\n");
	gPuts(fobj, ";\n");
	gPuts(fobj, ";  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n");
	gPuts(fobj, ";  \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n");
	gPuts(fobj, ";  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n");
	gPuts(fobj, ";  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n");
	gPuts(fobj, ";  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n");
	gPuts(fobj, ";  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n");
	gPuts(fobj, ";  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n");
	gPuts(fobj, ";  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n");
	gPuts(fobj, ";  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n");
	gPuts(fobj, ";  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n");
	gPuts(fobj, ";  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n");
	gPuts(fobj, ";\n\n");
}

static	int	class_source(char *p)
{
	char	*ext = file_extension(p);

	return (streq(ext, "d")  ||
		streq(ext, "D")  ||
		streq(ext, "dd")  ||
		streq(ext, "DD")  ||
		streq(ext, "n")  ||
		streq(ext, "N")  ||
		streq(ext, "nn")  ||
		streq(ext, "NN")  ||
		streq(ext, "i")  ||
		streq(ext, "ii")  ||
		streq(ext, "I")  ||
		streq(ext, "II"));
}

static	void	print_help(object stream)
{
	vPrintf(stream, "DPP Version %s %s (%d bit)\n\n", __DATE__, __TIME__, 8*(int)sizeof(char *));
	gPuts(stream, "dpp is Copyright (c) 1993-2000 Blake McBride (blake@mcbridemail.com)\n");
	gPuts(stream, "       All rights reserved.\nSee accompanying license.\n\n");
	gPuts(stream, "usage: dpp  options\n");

	gPuts(stream, "\n    Input options:\n\n");
/*		gPuts(stream, "\t-d file...\tread class doc files (out-of-date feature)\n\n");   */
	gPuts(stream, "\t-g [file]...\tread pre-existing generics (.h) files\n");
	gPuts(stream, "\t\t\t(default generics.h)\n");
	gPuts(stream, "\t-G [file]...\tread pre-existing generics (.h) files,\n");
	gPuts(stream, "\t\t\tno error if missing (default generics.h)\n");
	gPuts(stream, "\t-s file...\tread source (.c, .d) files (no preprocessing output)\n");
	gPuts(stream, "\t-p file...\tpreprocess .d file and create .c file\n");
	gPuts(stream, "\t-e file...\tread prototype exception file\n");

	gPuts(stream, "\n    Output options:\n\n");
	gPuts(stream, "\t-h [file] \tcreate a class header (.h) file (default generics.h)\n");
	gPuts(stream, "\t-c [file] \tcreate generics (.c) file (default generics.c)\n");
	gPuts(stream, "\t-iv       \tcreate external iv structure\n");
	gPuts(stream, "\t-cv       \tcreate external cv structure\n");
	gPuts(stream, "\t-j [package]\tcreate Java interface files\n");
	gPuts(stream, "\t-js [file]\tcreate JavaScript interface file (default jsinter.c)\n");
	gPuts(stream, "\t-L1 [file]\tcreate LibScheme interface file (default scminter.c)\n");
	gPuts(stream, "\t-L2 [file]\tcreate MzScheme interface file (default scminter.c)\n");

	gPuts(stream, "\n    Other options:\n\n");
	gPuts(stream, "\t-C        \tgenerate vendor copyright\n");
	gPuts(stream, "\t-eld      \tgenerate extra #line directives\n");
	gPuts(stream, "\t-f        \tforce creation of generics.h file\n");
	gPuts(stream, "\t-F        \tuse fast-wide method cache\n");
	gPuts(stream, "\t-i        \tignore errors and produce output files\n");
	gPuts(stream, "\t-Isc file...\tadd system include files to generics.c\n");
	gPuts(stream, "\t-Iac file...\tadd application include files to generics.c\n");
	gPuts(stream, "\t-Ish file...\tadd system include files to generics.h\n");
	gPuts(stream, "\t-Iah file...\tadd application include files to generics.h\n");
	gPuts(stream, "\t-mg       \tmacro guard typedefs and inlines\n");
#if 0
	gPuts(stream, "\t-mp [txt] \tdefault method name prefix\n");
	gPuts(stream, "\t-ms [txt] \tdefault method name suffix\n");
#endif
	gPuts(stream, "\t-Mn       \tgenerate multiple generics.c files (n generics per)\n");
	gPuts(stream, "\t-nai      \tno auto include generation\n");
	gPuts(stream, "\t-ni       \tdon't generate initialization functions\n");
	gPuts(stream, "\t-nld      \tdon't generate #line directives\n");
	gPuts(stream, "\t-N        \tforce no prototyping for any generics\n");
	gPuts(stream, "\t-q        \tquiet operation\n");
	gPuts(stream, "\t-r class/generic...\tremove class or generic\n");
	vPrintf(stream, "\t-Sn       \tgenerate code for strategy n (default %d)\n", Strategy);
	gPuts(stream, "\t-t [file]...\tfiles to be touched if header file is up-to-date\n");
#ifdef	unix		
	gPuts(stream, "\t\t\t(defaults to generics.h generics.c generics.o)\n");
#else		
	gPuts(stream, "\t\t\t(defaults to generics.h generics.c generics.obj)\n");
#endif
	gPuts(stream, "\t-X        \tallow C generic overloading\n");
	gPuts(stream, "\t-z        \talways return a 0 error code\n");
	gPuts(stream, "\t-64 file  \tsplit resulting .c file into file\n");
	gPuts(stream, "\n");
}


