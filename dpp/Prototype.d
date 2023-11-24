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



#include "dpp.h"

defclass  Prototype  {
	iName;			/*  method/generic name			*/
	iFixedName;		/*  associated fixed arg name		*/
	iRtn;			/*  return type				*/
	iArgs;			/*  list of args with vars & types 	*/
	iProto;			/*  args without vars			*/
	iParams;		/*  argument parameters without types	*/
	int	iVararg;	/*  1=various methods associated with the same
				    generic may have different args 	*/
	int	iException;	/*  1=no arg checking for this proto  	*/
	int	iNeedRest;	/*  vararg _rest_ variable needed	*/
	object	iLastArg;	/*  last arg prior to ...		*/
	int	iHasInputArg;	/*  int * or long * or double * or
				    object *                            */
};


#include <ctype.h>
#include <string.h>


extern	char	*trunc_mname(char *mname);


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewWithStrStr(char *nm, char *rt)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iName = gNewWithStr(String, nm);
	iRtn  = gNewWithStr(String, rt);
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iName);
	if (iFixedName)
		gDispose(iFixedName);
	gDispose(iRtn);
	if (iArgs)
		gDeepDispose(iArgs);
	if (iProto)
		gDeepDispose(iProto);
	if (iParams)
		gDeepDispose(iParams);
	if (iLastArg)
		gDeepDispose(iLastArg);
	return gDispose(super);
}

imeth	int	gHash()
{
	return gHash(iName);
}

imeth	int	gCompare(object obj)
{
	ChkArgTyp(obj, 2, CLASS);
	return gCompare(iName, ivPtr(obj)->iName);
}

static	void	remove_variable(char *buf, char *var)
{
	register int	i, vi = -1;
	int	fp;  //  declaration is a function pointer

	for (fp=i=0 ; buf[i] &&  !fp ; i++)
		if (buf[i] == '(')
			fp = 1;

	if (fp) {  // var is function pointer
		for ( ; buf[i] && !istart(buf[i]) ; i++);
		if (buf[i])
			vi = i;
	} else 
		for (i=0 ; buf[i] ; )
			if (istart(buf[i]))  {
				vi = i++;
				while (buf[i]  &&  irest(buf[i]))
					i++;
			} else
				i++;
	
	if (vi >= 0)  {
		for (i=vi ; irest(buf[i]) ; )
			*var++ = buf[i++];
		if (vi  &&  buf[vi-1] == ' ')
			vi--;
		for (i=vi+1 ; buf[i]  &&  irest(buf[i]) ; i++);
		memmove(buf+vi, buf+i, strlen(buf+i) + 1);
	}
	*var = '\0';
}

static	void	make_proto(ivType *iv, object ar)
{
	object	seq, nxt, prv=NULL;
	char	buf[100], var[50];

	iProto = gNew(LinkObject);
	iParams = gNew(LinkObject);
	for (seq=gSequence(ar) ; nxt = gNext(seq) ; )  {
		strncpy(buf, gStringValue(nxt), (sizeof(buf)-1));
		buf[(sizeof(buf)-1)] = '\0';
		remove_variable(buf, var);
		if (!iHasInputArg && isInputArg(buf))
			iHasInputArg = 1;
		gAddLast(iProto, gNewWithStr(String, buf));
		if (!*var)  {	/*  arg must have been ...   */
			iNeedRest = 1;
			if (iLastArg)
				DISPOSE(iLastArg);
			iLastArg = prv ? gCopy(prv) : prv;
		}
		gAddLast(iParams, prv=gNewWithStr(String, *var ? var : "_rest_"));
	}
}

imeth	gPrintArgs(object fobj)
{
	object	seq, nxt;
	int	n=0;

	if (iArgs  &&  !iVararg  &&  !iException)
		for (seq=gSequence(iArgs) ; nxt = gNext(seq) ; )  {
			if (n++)
				gPuts(fobj, ", ");
			gPuts(fobj, (char *) nxt);
		}
	else
		gPuts(fobj, "object self, ...");
	return self;
}

imeth	gPrintMethArgsH(object fobj)
{
	object	seq, nxt;
	int	n=0;

	for (seq=gSequence(iArgs) ; nxt = gNext(seq) ; )  {
		if (n++)
			gPuts(fobj, ", ");
		if (streq(gStringValue(nxt), "..."))
		    gPuts(fobj, "va_list _rest_");
		else
		    gPuts(fobj, (char *) nxt);
	}
	return self;
}

imeth	gPrintFixedArgs(object fobj)
{
	object	seq, nxt;
	int	n=0;

	if (iArgs)
		for (seq=gSequence(iArgs) ; nxt = gNext(seq) ; )  {
			if (n++)
				gPuts(fobj, ", ");
			gPuts(fobj, (char *) nxt);
		}
	else
		gPuts(fobj, "object self, ...");
	return self;
}

imeth	gPrintMethArgs(object fobj, int Strategy, int vararg)
{
	object	seq, nxt;
	int	n=0;

	if (vararg  &&  Strategy != 1)  {
		gPut(fobj, gNewToken(Token, "object self", 0L, 0));
		gPut(fobj, gNewToken(Token, ",", 0L, 1));
		gPut(fobj, gNewToken(Token, "va_list _rest_", 0L, 0));
	} else if (iArgs)
		for (seq=gSequence(iArgs) ; nxt = gNext(seq) ; )  {
			if (n++)
				gPut(fobj, gNewToken(Token, ",", 0L, 1));
			if (Strategy > 1  &&  streq(gStringValue(nxt), "..."))
				gPut(fobj, gNewToken(Token, "va_list _rest_", 0L, 0));
			else
				gPut(fobj, gNewToken(Token, gStringValue(nxt), 0L, 0));
		}
	gPut(fobj, gNewToken(Token, ")", 0L, 0));
	return self;
}

imeth	gPrintVars(object fobj)
{
	object	seq, nxt;
	int	n=0;
	char	*v;

	if (iParams  &&  !iVararg  &&  !iException)
		for (seq=gSequence(iParams) ; nxt = gNext(seq) ; )  {
			if (n++)
				gPuts(fobj, ", ");
			v = gStringValue(nxt);
			if (*v)
				gPuts(fobj, (char *) nxt);
			else
				gPuts(fobj, "_rest_");
		}
	else
		gPuts(fobj, "self, _rest_");
	return self;
}

imeth	gUseVars(object fobj)
{
	object	seq, nxt;
	char	*v;
	int	n = 0;

	if (!iParams)
		return self;
	for (seq=gSequence(iParams) ; nxt = gNext(seq) ; )
		if (n++)  {
			v = gStringValue(nxt);
			if (*v  &&  strne(v, "_rest_"))
				vPrintf(fobj, "\tUSE(%s);\n", v);
		}
	return self;
}

#if 0

static	void	print_args(ivType *iv, object ar)
{
	object	seq, nxt;
	int	n=0;

	vPrintf(stdoutStream, "%s %s(", gStringValue(iRtn), gStringValue(iName));
	for (seq=gSequence(ar) ; nxt = gNext(seq) ; )  {
		if (n++)
			gPuts(stdoutStream, ", ");
		gPuts(stdoutStream, (char *) nxt);
	}
	gPuts(stdoutStream, ")\n");
	if (iVararg)
		gPuts(stdoutStream, "Varargs is set\n");
	else
		gPuts(stdoutStream, "Varargs is not set\n");
	if (iException)
		gPuts(stdoutStream, "Exception is set\n");
	else
		gPuts(stdoutStream, "Exception is not set\n");
}

#endif

imeth	gSetArgs(object ar)
{
	if (iArgs)
		DEEPDISPOSE(iArgs);
	if (iProto)
		iProto = DEEPDISPOSE(iProto);
	if (iParams)
		iParams = DEEPDISPOSE(iParams);
	if (iArgs = ar)
		make_proto(iv, ar);
	if (ar  &&  gSize(ar) == 2  &&  streq(gStringValue(gLast(ar)), "..."))
		iVararg = 1;
#if 0
	print_args(iv, iArgs);
	print_args(iv, iProto);
#endif
	return self;
}

imeth	gReturnType()
{
	return iRtn;
}

imeth	gArgs()
{
	return iArgs;
}

imeth	object	gPrototype()
{
	return iProto;
}

imeth	object	gParameters()
{
	return iParams;
}

imeth	gDeepCopy()
{
	object	nobj;
	ivType	*niv;
	
	nobj = gDeepCopy(super);
	niv  = ivPtr(nobj);
	niv->iName = gDeepCopy(iName);
	niv->iFixedName = iFixedName ? gDeepCopy(iFixedName) : iFixedName;
	niv->iRtn  = gDeepCopy(iRtn);
	if (iArgs)  {
		niv->iArgs = gDeepCopy(iArgs);
		niv->iProto = gDeepCopy(iProto);
		niv->iParams = gDeepCopy(iParams);
	}
	if (iLastArg)
		niv->iLastArg = gDeepCopy(iLastArg);
	return nobj;
}

imeth	int	gMatchNoError(object mproto)
{
	ivType	*miv = ivPtr(mproto);
	object	mseq, gseq, garg, marg;

#if 0
	print_args(iv, iArgs);
	print_args(miv, miv->iArgs);
#endif

	if (gCompare(miv->iRtn, iRtn))
		return 0;

	if ((!miv->iArgs  ||  miv->iVararg  ||  miv->iException)  &&  (!iArgs  ||  iVararg))
		return 1;  /*  both vararg  */

	if (miv->iException  ||  !miv->iArgs  ||  miv->iVararg  ||  !iArgs  ||  iVararg)
		return 0;  /*  one is vararg the other not  */
	
	if (gSize(miv->iArgs) != gSize(iArgs))
		return 0;	/*  different number of arguments  */

	mseq = gSequence(miv->iArgs);
	gseq = gSequence(iArgs);
	garg = (object) 1;    /*  any non-NULL  */
	while (marg = gNext(mseq))  {
		garg = gNext(gseq);
		if (!garg  ||  gCompare(marg, garg))  {
			DISPOSE(mseq);
			if (garg)
				DISPOSE(gseq);
			return 0;
		}
	}
	if (garg)
		DISPOSE(gseq);
	
	return 1;
}

imeth	int	gMatch(object mproto)
{
	ivType	*miv = ivPtr(mproto);
	object	mseq, gseq, garg, marg;
	int	n, error=0;

#if 0
	print_args(iv, iArgs);
	print_args(miv, miv->iArgs);
#endif

	if (gCompare(miv->iRtn, iRtn))  {
		error = 1;
		vPrintf(stdoutStream, "Method %s and generic %s have different return types\n",
			trunc_mname(gStringValue(miv->iName)),
			gStringValue(iName));
	}

	if (iException  ||  miv->iVararg  &&  iVararg)
		return error;

	if (miv->iVararg  ||  iVararg)  {
		vPrintf(stdoutStream, "Method %s and generic %s have different arguments\n",
			trunc_mname(gStringValue(miv->iName)),
			gStringValue(iName));
		return 1;
	}

	if (!miv->iProto)  {
		vPrintf(stdoutStream, "Warning: Method %s has no prototype.\n",
			trunc_mname(gStringValue(miv->iName)));
		return error;
	}

	if (!iProto)
		return error;	/*  should never happen  */
	
	if (gSize(miv->iProto) != gSize(iProto))  {
		vPrintf(stdoutStream, "Method %s and generic %s have a different number of arguments\n",
			trunc_mname(gStringValue(miv->iName)),
			gStringValue(iName));
		return 1;
	}

	mseq = gSequence(miv->iProto);
	gseq = gSequence(iProto);
	garg = (object) 1;    /*  any non-NULL  */
	for (n=1 ; marg = gNext(mseq) ; ++n)  {
		garg = gNext(gseq);
		if (!garg)  {
			error = 1;
			vPrintf(stdoutStream, "Method %s and generic %s have a different number of arguments.\n",
				trunc_mname(gStringValue(miv->iName)),
				gStringValue(iName));
			break;
		}
/*
		if (streq(gStringValue(garg), "..."))
			break;
*/
		if (gCompare(marg, garg))  {
			error = 1;
			vPrintf(stdoutStream, "Method %s and generic %s have different argument types (%d)\n",
				trunc_mname(gStringValue(miv->iName)),
				gStringValue(iName), n);
		}
	}
	if (marg)
		DISPOSE(mseq);
	if (garg)
		DISPOSE(gseq);
	
	return error;
}

imeth	gGetMGName()
{
	return iName;
}

imeth	gChangeName(char *nm)
{
	if (iName)
		gChangeStrValue(iName, nm);
	else
		iName = gNewWithStr(String, nm);
	return self;
}

imeth	gGetFixedName()
{
	return iFixedName;
}

imeth	gChangeFixedName(char *nm)
{
	if (iFixedName)
		gChangeStrValue(iFixedName, nm);
	else
		iFixedName = gNewWithStr(String, nm);
	return self;
}

imeth	gVarArg()
{
	iVararg = 1;
	return self;
}

imeth	int	gIsVarArg()
{
	return iNeedRest || iVararg;
}

imeth	char	*gLastArg()
{
	return iLastArg && !iVararg ? gStringValue(iLastArg) : "self";
}

imeth	gException()
{
	iException = 1;
	return self;
}

imeth	int	gHasInputArg()
{
	return iHasInputArg;
}

