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




#include "kernels.h"

public	defclass Behavior  {

	int  	id;	/*  used for hashing and identification  */
	
	char	*name;
	
	CRITICALSECTION	cs;	/*  in support of native threads  */

	object	*direct_superclasses;
	int	n_direct_superclasses;
		
	object_list	*direct_subclasses;
		
	object_list	*direct_methods;
		
	object	next;  /*  keep a linked list of classes  */
		
	int	cache_idx;
		
	unsigned	direct_iv_size; /*  direct instance var size  */
	unsigned	effective_iv_size;/*  total instance variable size */
		
	int	direct_iv_offset;


	/*  list of all superclasses with IVs  */
		
	iv_offset_def_list	*all_superclasses;

	/*  instance allocation housekeeping  */
		
	long	sig1;		/*  class signatures		*/
	long	sig2;
		
	instance_block	*ib;
	free_list	*fl;
	int	nipib;		/*  number of instances per instance block  */
	int	nib;		/*  number of instance blocks		*/
	long	ni;		/*  number of instances			*/
	long	nai;		/*  number of available instances	*/
		
	int	ncg;		/*  no GC this class flag		*/
		
	int	trace;		/*  trace mode				*/
		
	ofun	markfun;	/*  additional marking function		*/
};


imeth	char	*gName()
{
	return name; 
}

imeth	gDontCollect()
{
	ncg = 1;
	return self;
}

imeth	int	gTrace(int mode)
{
	int	pmode = trace;
	trace = mode;
	return pmode;
}

imeth	gMarkingMethod(ofun mf)
{
	markfun = mf;
	return self;
}

imeth	void	gDoesNotImplement(object gen)
{
	char	buf[100];

	ChkArgTyp(gen, 2, GenericFunction);
	if (IsaClass(self))
		sprintf(buf, "\nInstance of class %s doesn't respond to generic %s\n", 
			name, gName(gen));
	else  /*  MetaClass  */
		sprintf(buf, "\nClass %s doesn't respond to generic %s\n", 
			name, gName(gen));
	gError(Dynace, buf);
}

imeth	int	gIsKindOf(object cls2)
{
	int	i;

	/*  cls2 need not be validated  */

	if (self == cls2)
		return 1;

	/*  check the super classes  */

	for (i=0 ; i < n_direct_superclasses ; ++i)
		if (gIsKindOf(direct_superclasses[i], cls2))
			return 1;
	return 0;
}

#ifndef	NOCLASSLIB

imeth	gStringRep()
{
	object	s, t;

	t = gStringRepValue(self);
	s = vBuild(String, "Class  ", name, "  [ ", t, " ]\n", NULL);
	gDispose(t);
	return s;
}

imeth	gStringRepValue()
{
	return vSprintf(String, "<%8.8lx>", self);
}

imeth	gSuperClasses()
{
	int	i;
	object	lst;

	lst = gNew(LinkObject);

	for (i=0 ; i < n_direct_superclasses ; ++i)
		gAddFirst(lst, direct_superclasses[i]);
	return lst;
}

imeth	gSubClasses()
{
	object_list	*sc;
	object	lst;

	lst = gNew(LinkObject);

	for (sc=direct_subclasses ; sc ; sc=sc->next)
		gAddFirst(lst, sc->obj);
	return lst;
}

#endif

imeth	gCopy()
{
	return gShouldNotImplement(self, "gCopy/gDeepCopy");
}

objrtn	Behavior_initialize(void)
{
	static	int	done = 0;

	/*  Class creation and some of the methods are initialized by
	    the kernel  */

	if (done)
		return Behavior_c;

	done = 1;

/*	Behavior_c = gNewClass(Class, "Behavior", sizeof(Behavior_iv_t), 0, END);	*/

	iMethodFor(Behavior, gName, Behavior_im_gName);
	iMethodFor(Behavior, gDontCollect, Behavior_im_gDontCollect);
	iMethodFor(Behavior, gTrace, Behavior_im_gTrace);
	iMethodFor(Behavior, gMarkingMethod, Behavior_im_gMarkingMethod);
	iMethodFor(Behavior, gDoesNotImplement, Behavior_im_gDoesNotImplement);
	iMethodFor(Behavior, gIsKindOf, Behavior_im_gIsKindOf);
	iMethodFor(Behavior, gCopy, Behavior_im_gCopy);
	iMethodFor(Behavior, gDeepCopy, Behavior_im_gCopy);

#ifndef	NOCLASSLIB
	iMethodFor(Behavior, gStringRep, Behavior_im_gStringRep);
	iMethodFor(Behavior, gStringRepValue, Behavior_im_gStringRepValue);
	iMethodFor(Behavior, gSuperClasses, Behavior_im_gSuperClasses);
	iMethodFor(Behavior, gSubClasses, Behavior_im_gSubClasses);
#endif
	return Behavior_c;
}

#if 0  /*  code for the benefit of dpp  */

imeth	gNew, gAlloc (){}
ivmeth	vNew(...){}
imeth	gStackAlloc(void *p){}
imeth	ofun	gFindMethod(object generic, int lev){}
imeth	gFindMethodObject(object generic, int lev){}
imeth	int	gInstanceSize(){}

#endif


