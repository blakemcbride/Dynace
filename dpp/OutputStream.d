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


#define CONVERT_SUPER


#define strne(a, b)	strcmp(a, b)
#define streq(a, b)	!strcmp(a, b)

#define istart(x)	(isalpha(x)  ||  (x) == '_')
#define irest(x)	(isalnum(x)  ||  (x) == '_')


#ifdef	unix
#define RMODE	"r"
#define WMODE	"w"
#else
#define RMODE	"rt"
#define WMODE	"wt"
#endif



defclass OutputStream  {
	iStream;		/*  output stream or NULL if none  */
	iSFile;			/*  source file name	*/
	iTFile;			/*  target file name	*/
	long	iILine;		/*  input line number	*/
	long	iOLine;		/*  current output line	(as seen by the compiler) */
	long	iRLine;		/*  real output line number  */
	int	iBraceLevel;
	int	iParenLevel;
	int	iBlbl;		/*  beginning of line brace level  */
	char	iPllc;		/*  previous line last char	   */
	iOutputList;
	iCVars;			/*  class variables (a Set)	*/
	iIVars;			/*  instance variables (a Set)	*/
	iCName;			/*  class name			*/
};


#include <ctype.h>
#include <stdarg.h>
#include <string.h>



extern	objrtn open_file(char *file,char *mode, int real_quiet);

extern	int	ErrorCode, LineDirectives;


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewWithStrStr(char *file, char *ifile)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	iSFile = gNewWithStr(String, ifile);
	iTFile = gNewWithStr(String, file);
#if	!defined(unix)  &&  0
	gToLower(iSFile);
	gToLower(iTFile);
#endif
	iStream = file ? open_file(file, WMODE, 0) : (object) NULL;
	iOutputList = gNew(LinkList);
	iRLine = iOLine = 1L;
	iPllc = ';';
	iCName = gNewWithStr(String, "unknown");
	return obj;
}

static	int	sub(object tkn, object sym, object vars, char *exp)
{
	long	line;
	if (!vars  ||  !gFind(vars, sym))
		return 0;
	line = gLineNumber(tkn);
	gAddBefore(tkn, gNewToken(Token, exp, line, 0));
	gAddBefore(tkn, gNewToken(Token, "->", line, 0));
	return 1;
}

static	int	do_var_sub(ivType *iv)
{
	object	tkn, sym;
	char	*p, *pstr = NULL;
	int	res = 0;
	char	cvar[80];

	sprintf(cvar, "%s_cv", gStringValue(iCName));
	for (tkn = gFirst(iOutputList) ; tkn ; tkn = gNext(tkn), pstr=p)  {
		p = gStringValue(tkn);
		if (!istart(*p))
			continue;
		if (!pstr  ||  strne(pstr, "->")  &&  strne(pstr, "."))  {
			sym = gToken(tkn);
			if (sub(tkn, sym, iIVars, "iv"))
				res = 1;
			else
				sub(tkn, sym, iCVars, cvar);
		}
	}
	return res;
}

#ifdef	CONVERT_SUPER

/*  convert  gen(super obj, args)   ->
    (_super_save_=(obj), oSuper(cls, gen, _super_save_)(_super_save_, args)) */


static	objrtn	make_arg(object frst, object lst, int *simple)
{
	object	arg;
	int	pos;
	char	*p1=NULL, *p2=NULL, *p3=NULL;

	arg = gNew(LinkList);
	for (pos=0 ; frst != lst ; frst=gNext(frst))  {
		switch (++pos)  {
		case 1:		p1 = gStringValue(frst);	break;
		case 2:		p2 = gStringValue(frst);	break;
		case 3:		p3 = gStringValue(frst);	break;
		}
		gAddLast(arg, gCopy(frst));
	}
	*simple = pos == 1  &&  istart(*p1)  ||
		  pos == 3  &&  streq(p1, "(")  &&  istart(*p2)  &&
			  streq(p3, ")");
	return arg;
}


#define OUTT(t)	if (tkn)				\
			gAddBefore(tkn, t);		\
		else					\
			gAddLast(iOutputList, t)



static	int	convert_super(ivType *iv)
{
	object	tkn;
	char	*p, *pstr = NULL, *ppstr = NULL, *tp;
	int	paren;
	object	frst;	/*  first token after super	*/
	object	lst;	/*  token at end of super arg   */
	object	gen;	/*  generic token		*/
	object	arg;	/*  list of tokens after super 	*/
	object	t;	/*  temp token pointer		*/
	long	line;	/*  input line of token 	*/
	int	simple;	/*  simple case of super arg	*/
	char	cname[80];

	for (tkn = gFirst(iOutputList) ; tkn ; tkn=gNext(tkn), ppstr=pstr, pstr=p)  {
		p = gStringValue(tkn);
		if (strne(p, "super"))
			continue;
		if (!pstr  ||  strne(pstr, "("))
			continue;
		if (!ppstr  ||  !istart(*ppstr))
			continue;

		frst = gNext(tkn);
		if (!frst)
			continue;

		line = gLineNumber(tkn);

		/*  check and handle implied self  */
		tp = gStringValue(frst);
		if (streq(tp, ",")  ||  streq(tp, ")"))  {
			t = gNewToken(Token, "self", line, 0);
			gAddBefore(frst, t);
			frst = t;
		}

		paren = streq(gStringValue(frst), "(");

		/*  get end of obj  */

		for (lst=frst ; 1 ; )  {
			lst = gNext(lst);
			if (!lst)
				break;
			tp = gStringValue(lst);
			if (streq(tp, ")"))
				paren--;
			else if (streq(tp, "("))
				paren++;
			if (streq(tp, ",")  &&  !paren  ||  paren < 0)
				break;
		}

		/*  make tkn point to the generic  */

		tkn = gPrevious(tkn);
		tkn = gPrevious(tkn);

		/*  get copy of generic name  */

		gen = gCopy(tkn);

		/*  make list of tokens after super  */

		arg = make_arg(frst, lst, &simple);

		/*  delete  gen(super arg from iOutputList  */

		for ( ; tkn != lst ; tkn = t) {
			t = gNext(tkn);
			gDispose(tkn);
		}

		/*  make tkn point to whatever follows gen(super obj  */

		tkn = lst;

		/*  create desired code  */

		if (simple)  {
			t = gNewToken(Token, "oSuper", line, 0);
			OUTT(t);

			t = gNewToken(Token, "(", line, 0);
			OUTT(t);

			sprintf(cname, "%s_c", gStringValue(iCName));
			t = gNewToken(Token, cname, line, 0);
			OUTT(t);

			t = gNewToken(Token, ",", line, 1);
			OUTT(t);

			OUTT(gen);

			t = gNewToken(Token, ",", line, 1);
			OUTT(t);

			for (t=gFirst(arg) ; t ; t=gNext(t))
				OUTT(gCopy(t));

			t = gNewToken(Token, ")", line, 0);
			OUTT(t);

			t = gNewToken(Token, "(", line, 0);
			OUTT(t);
			
			while (t = gFirst(arg))  {
				gRemove(t);
				OUTT(t);
			}

			DISPOSE(arg);

		} else {
			t = gNewToken(Token, "(", line, 0);
			OUTT(t);

			t = gNewToken(Token, "_super_save_", line, 0);
			OUTT(t);

			t = gNewToken(Token, "=", line, 0);
			OUTT(t);

			t = gNewToken(Token, "(", line, 0);
			OUTT(t);

			while (t = gFirst(arg))  {
				gRemove(t);
				OUTT(t);
			}
			DISPOSE(arg);

			t = gNewToken(Token, ")", line, 0);
			OUTT(t);

			t = gNewToken(Token, ",", line, 1);
			OUTT(t);

			t = gNewToken(Token, "oSuper", line, 0);
			OUTT(t);

			t = gNewToken(Token, "(", line, 0);
			OUTT(t);

			sprintf(cname, "%s_c", gStringValue(iCName));
			t = gNewToken(Token, cname, line, 0);
			OUTT(t);

			t = gNewToken(Token, ",", line, 1);
			OUTT(t);

			OUTT(gen);

			t = gNewToken(Token, ",", line, 0);
			OUTT(t);

			t = gNewToken(Token, "_super_save_", line, 0);
			OUTT(t);

			t = gNewToken(Token, ")", line, 0);
			OUTT(t);

			t = gNewToken(Token, "(", line, 0);
			OUTT(t);

			t = gNewToken(Token, "_super_save_", line, 0);
			OUTT(t);

			/*  find the ) at the end of the whole thing  */

			paren = 1;
			while (paren  &&  tkn)  {
				p = gStringValue(tkn);
				if (streq(p, "("))
					paren++;
				else if (streq(p, ")"))
					paren--;
				if (paren)
					tkn = gNext(tkn);
			}
			if (!tkn)  {
				ErrorCode = 1;
				vPrintf(stdoutStream, "Error on line %ld\n", line);
				vPrintf(stdoutStream, "Expression containing super - bad syntax\n");
			}

			t = gNewToken(Token, ")", tkn ? gLineNumber(tkn) : line, 0);
			OUTT(t);
		}			

		return 1;   /*  could be more - re-scan  */
	}
	return 0;   /*  none found - no re-scan  */
}

#endif

/*  flush a single line  */

imeth	int	gFlush()
{
	int	i, pp, endif;
	object	tkn;
	char	*p=NULL, c=0;

	if (!iStream)
		return 0;

	if (iIVars  ||  iCVars)
		do_var_sub(iv);

#ifdef	CONVERT_SUPER
	while (convert_super(iv));
#endif

	tkn = gFirst(iOutputList);
	if (tkn  &&  iILine  &&  iOLine != iILine)
		if (iILine > iOLine  &&  iILine < iOLine+5)
			while (iOLine != iILine)  {
				gPuts(iStream, "\n");
				iOLine++;
				iRLine++;
			}
		else
			gSLineDirective(self);
	
	pp = tkn  &&  *(p=gStringValue(tkn)) == '#';
	endif = pp  &&  (streq(p, "#endif")  ||  streq(p, "#else")  ||  streq(p, "#elif"));
	if (tkn  &&  !pp)
		for (i=iBlbl-(*p == '}')+(*p != '{' && iPllc != ';'  &&  iPllc != '{' &&  iPllc != '}' && iPllc != ':') ; i-- > 0 ; )
			gPuts(iStream, "\t");
	for ( ; tkn ; tkn = gFirst(iOutputList))  {
		p = gStringValue(tkn);
		if (irest(*p)  &&  irest(c))
			gPuts(iStream, " ");
		gPuts(iStream, p);
		if (gSpace(tkn))  {
			gPuts(iStream, " ");
			c = 0;
		} else
			c = *p;
		iPllc = *p;
		gDeepDisposeFirst(iOutputList);
	}
	if (pp)
		iPllc = ';';
	gPuts(iStream, "\n");
	iOLine++;
	iRLine++;
	iILine = 0L;
	iBlbl = iBraceLevel;
	if (endif)
		gSLineDirective(self);
	return 0;
}

/*  flush an entire method  */

imeth	gFlushm()
{
	int	i, endif=0;
	object	tkn;
	char	*p, c=0, *cname, ctype[80];

	if (!iStream)
		return self;
	
#define	ADD(s, t)	gAddAfter(tkn, gNewToken(Token, t, 0L, s))
	
	if ((iIVars  ||  iCVars)  &&  do_var_sub(iv))  {
		tkn = gFirst(iOutputList);
		sprintf(ctype, "%s_iv_t", cname = gStringValue(iCName));
		ADD(0, ";");
		ADD(0, ")");
		ADD(0, "self");
		ADD(1, ",");
		ADD(0, cname);
		ADD(0, "(");
		ADD(0, "GetIVs");
		ADD(1, "=");
		ADD(1, "iv");
		ADD(0, "*");
		ADD(1, ctype);
	}

#ifdef	CONVERT_SUPER
	while (convert_super(iv));
#endif

	for (tkn=gFirst(iOutputList) ; tkn ; tkn=gFirst(iOutputList))  {
		iILine = gLineNumber(tkn);
		if (iILine  &&  iOLine != iILine)  {
			if (iILine > iOLine  &&  iILine < iOLine+5) {
				while (iOLine != iILine)  {
					gPuts(iStream, "\n");
					iOLine++;
					iRLine++;
				}
				if (endif) {
					gSLineDirective(self);
					endif = 0;
				}
			} else
				gSLineDirective(self);
			p = gStringValue(tkn);
			iBlbl = iBraceLevel;
			if (*p != '#')
				for (i=iBlbl-(*p == '}')+(*p != '{' && iPllc != ';'  &&  iPllc != '{' &&  iPllc != '}' && iPllc != ':') ; i-- > 0 ; )
					gPuts(iStream, "\t");
		} else
			p = gStringValue(tkn);
		if (!endif  &&  *p == '#')
			endif = streq(p, "#endif")  ||  streq(p, "#else")  ||  streq(p, "#elif");
		if (irest(*p)  &&  irest(c))
			gPuts(iStream, " ");
		gPuts(iStream, p);
		iPllc = c = *p;
		if (c == '(')
			iParenLevel++;
		else if (c == ')')  {
			if (iParenLevel)
				iParenLevel--;
		} else if (c == '{')
			iBraceLevel++;
		else if (c == '}')  {
			if (iBraceLevel)
				iBraceLevel--;
		}
		if (gSpace(tkn))  {
			gPuts(iStream, " ");
			c = 0;
		}
		gDeepDisposeFirst(iOutputList);
	}
	gPuts(iStream, "\n");
	iOLine++;
	iRLine++;
	iILine = 0L;
	iBlbl = iBraceLevel;
	return self;
}

imeth	object	gDispose, gDeepDispose ()
{
	gFlush(self);
	if (iStream)
		gDispose(iStream);
	gDispose(iSFile);
	gDispose(iTFile);
	gDeepDispose(iOutputList);
	return gDispose(super);
}

imeth	gPut(tkn)
{
	char	c = *gStringValue(tkn);
	long	ln = gLineNumber(tkn);

	if (!iStream)  {
		DISPOSE(tkn);
		return self;
	}

	if (!iILine)
		iILine = ln;
	else if (ln  &&  iILine != ln)  {
		gFlush(self);
		iILine = ln;
	}

	gAddLast(iOutputList, tkn);
	if (!ln  &&  (!iParenLevel  &&  c == ';'  ||  c == '}'  ||  c == '{'  ||  c == ':'))
		gFlush(self);
	if (c == '(')
		iParenLevel++;
	else if (c == ')')  {
		if (iParenLevel)
			iParenLevel--;
	} else if (c == '{')
		iBraceLevel++;
	else if (c == '}')  {
		if (iBraceLevel)
			iBraceLevel--;
	}
	return self;
}

/*  put token at end of list but don't output (collect entire method)  */

imeth	gPutm(tkn)
{
	if (iStream)
		gAddLast(iOutputList, tkn);
	else
		DISPOSE(tkn);
	return self;
}

static	int	count_nl(char *s)
{
	int	n = 0;
	for ( ; *s ; s++)
		if (*s == '\n')
			n++;
	return n;
}

imeth	int	gPuts(char *str)
{
	iOLine = -20L;
	iRLine += count_nl(str);
	iILine = 0L;
	return iStream ? gPuts(iStream, str) : strlen(str);
}

ivmeth	int	vPrintf(char *fmt, ...)
{
	char	buf[256];
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	iOLine = -20L;
	iRLine += count_nl(buf);
	iILine = 0L;
	return iStream ? gPuts(iStream, buf) : strlen(buf);
}

imeth	gSetOSVars(cname, cvs, ivs)
{
	DISPOSE(iCName);
	iCName = cname;
	iCVars = gSize(cvs) ? cvs : (object) NULL;
	iIVars = gSize(ivs) ? ivs : (object) NULL;
	return self;
}

imeth	gTLineDirective()
{
	if (iStream  &&  LineDirectives)  {
		iRLine += 2;
		iOLine = -20L;
		vPrintf(iStream, "\n#line %ld \"%s\"\n", iRLine, gStringValue(iTFile));
	}
	return self;
}

imeth	gSLineDirective()
{
	if (iStream) {
		if (LineDirectives  &&  iILine)  {
			vPrintf(iStream, "\n#line %ld \"%s\"\n", iILine, gStringValue(iSFile));
			iRLine += 2;
			iOLine = iILine;
		} else {
			gPuts(iStream, "\n");
			iRLine++;
			iOLine = iILine ? iILine : -20L;
		}
	}
	return self;
}

imeth	gForceLineDirective()
{
	iOLine = -20L;
	return self;
}


