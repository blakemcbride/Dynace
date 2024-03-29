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



/*  This file automatically generated by dpp - do not edit  */

#define	DPP_STRATEGY	2
#define	DPP_FASTWIDE	0



#line 31 "OutputStream.d"
#include "dpp.h" 


#define CONVERT_SUPER 


#define strne(a, b) strcmp(a, b) 
#define streq(a, b) !strcmp(a, b) 

#define istart(x) (isalpha(x) || (x) == '_') 
#define irest(x) (isalnum(x) || (x) == '_') 


#ifdef unix 
#define RMODE "r" 
#define WMODE "w" 
#else 


#line 48 "OutputStream.d"
#define RMODE "rt" 
#define WMODE "wt" 
#endif 


#define	CLASS	OutputStream_c
#define	ivType	OutputStream_iv_t

#include "generics.h"

object	OutputStream_c;


#line 73 "OutputStream.c"
typedef struct  _OutputStream_iv_t  {
	object iStream;
	object iSFile;
	object iTFile;
	long iILine;
	long iOLine;
	long iRLine;
	int iBraceLevel;
	int iParenLevel;
	int iBlbl;
	char iPllc;
	object iOutputList;
	object iCVars;
	object iIVars;
	object iCName;
}	OutputStream_iv_t;


#line 72 "OutputStream.d"
#include <ctype.h> 
#include <stdarg.h> 
#include <string.h> 



extern objrtn open_file(char *file,char *mode, int real_quiet); 

extern int ErrorCode, LineDirectives; 


cmeth objrtn OutputStream_cm_gNew(object self)
{ 
	return gShouldNotImplement(self, "gNew"); 
} 

cmeth objrtn OutputStream_cm_gNewWithStrStr(object self, char *file, char *ifile)
{ 
	object obj = oSuper(OutputStream_c, gNew, self)(self); 
	ivType *iv = ivPtr(obj); 
	iv->iSFile = gNewWithStr(String, ifile); 
	iv->iTFile = gNewWithStr(String, file); 
#if !defined(unix) && 0 
	gToLower(iv->iSFile); 
	gToLower(iv->iTFile); 
#endif 


#line 98 "OutputStream.d"
	iv->iStream = file ? open_file(file, WMODE, 0) : (object) NULL; 
	iv->iOutputList = gNew(LinkList); 
	iv->iRLine = iv->iOLine = 1L; 
	iv->iPllc = ';'; 
	iv->iCName = gNewWithStr(String, "unknown"); 
	return obj; 
} 

static int sub(object tkn, object sym, object vars, char *exp) 
{ 
	long line; 
	if (!vars || !gFind(vars, sym)) 
		return 0; 
	line = gLineNumber(tkn); 
	gAddBefore(tkn, gNewToken(Token, exp, line, 0)); 
	gAddBefore(tkn, gNewToken(Token, "->", line, 0)); 
	return 1; 
} 

static int do_var_sub(ivType *iv) 
{ 
	object tkn, sym; 
	char *p, *pstr = NULL; 
	int res = 0; 
	char cvar[80]; 

	sprintf(cvar, "%s_cv", gStringValue(iv->iCName)); 
	for (tkn = gFirst(iv->iOutputList) ; tkn ; tkn = gNext(tkn), pstr=p) { 
		p = gStringValue(tkn); 
		if (!istart(*p)) 
			continue; 
		if (!pstr || strne(pstr, "->") && strne(pstr, ".")) { 
			sym = gToken(tkn); 
			if (sub(tkn, sym, iv->iIVars, "iv")) 
				res = 1; 
			else 
				sub(tkn, sym, iv->iCVars, cvar); 
		} 
	} 
	return res; 
} 

#ifdef CONVERT_SUPER 

#line 146 "OutputStream.d"
static objrtn make_arg(object frst, object lst, int *simple) 
{ 
	object arg; 
	int pos; 
	char *p1=NULL, *p2=NULL, *p3=NULL; 

	arg = gNew(LinkList); 
	for (pos=0 ; frst != lst ; frst=gNext(frst)) { 
		switch (++pos) { 
			case 1: p1 = gStringValue(frst); break; 
			case 2: p2 = gStringValue(frst); break; 
			case 3: p3 = gStringValue(frst); break; 
		} 
		gAddLast(arg, gCopy(frst)); 
	} 
	*simple = pos == 1 && istart(*p1) || 
		pos == 3 && streq(p1, "(") && istart(*p2) && 
		streq(p3, ")"); 
	return arg; 
} 

#line 171 "OutputStream.d"
#define OUTT(t) if (tkn) gAddBefore(tkn, t); else gAddLast(iv->iOutputList, t) 



static int convert_super(ivType *iv) 
{ 
	object tkn; 
	char *p, *pstr = NULL, *ppstr = NULL, *tp; 
	int paren; 
	object frst; 
	object lst; 
	object gen; 
	object arg; 
	object t; 
	long line; 
	int simple; 
	char cname[80]; 

	for (tkn = gFirst(iv->iOutputList) ; tkn ; tkn=gNext(tkn), ppstr=pstr, pstr=p) { 
		p = gStringValue(tkn); 
		if (strne(p, "super")) 
			continue; 
		if (!pstr || strne(pstr, "(")) 
			continue; 
		if (!ppstr || !istart(*ppstr)) 
			continue; 

		frst = gNext(tkn); 
		if (!frst) 
			continue; 

		line = gLineNumber(tkn); 


		tp = gStringValue(frst); 
		if (streq(tp, ",") || streq(tp, ")")) { 
			t = gNewToken(Token, "self", line, 0); 
			gAddBefore(frst, t); 
			frst = t; 
		} 

		paren = streq(gStringValue(frst), "("); 



		for (lst=frst ; 1 ; ) { 
			lst = gNext(lst); 
			if (!lst) 
				break; 
			tp = gStringValue(lst); 
			if (streq(tp, ")")) 
				paren--; 
			else if (streq(tp, "(")) 
				paren++; 
			if (streq(tp, ",") && !paren || paren < 0) 
				break; 
		} 



		tkn = gPrevious(tkn); 
		tkn = gPrevious(tkn); 



		gen = gCopy(tkn); 



		arg = make_arg(frst, lst, &simple); 



		for ( ; tkn != lst ; tkn = t) { 
			t = gNext(tkn); 
			gDispose(tkn); 
		} 



		tkn = lst; 



		if (simple) { 
			t = gNewToken(Token, "oSuper", line, 0); 
			OUTT(t); 

			t = gNewToken(Token, "(", line, 0); 
			OUTT(t); 

			sprintf(cname, "%s_c", gStringValue(iv->iCName)); 
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

			while (t = gFirst(arg)) { 
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

			while (t = gFirst(arg)) { 
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

			sprintf(cname, "%s_c", gStringValue(iv->iCName)); 
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



			paren = 1; 
			while (paren && tkn) { 
				p = gStringValue(tkn); 
				if (streq(p, "(")) 
					paren++; 
				else if (streq(p, ")")) 
					paren--; 
				if (paren) 
					tkn = gNext(tkn); 
			} 
			if (!tkn) { 
				ErrorCode = 1; 
				vPrintf(stdoutStream, "Error on line %ld\n", line); 
				vPrintf(stdoutStream, "Expression containing super - bad syntax\n"); 
			} 

			t = gNewToken(Token, ")", tkn ? gLineNumber(tkn) : line, 0); 
			OUTT(t); 
		} 

		return 1; 
	} 
	return 0; 
} 

#endif 


#line 376 "OutputStream.d"
imeth int OutputStream_im_gFlush(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	int i, pp, endif; 
	object tkn; 
	char *p=NULL, c=0; 

	if (!iv->iStream) 
		return 0; 

	if (iv->iIVars || iv->iCVars) 
		do_var_sub(iv); 

#ifdef CONVERT_SUPER 
		while (convert_super(iv)); 
#endif 


#line 392 "OutputStream.d"
		tkn = gFirst(iv->iOutputList); 
	if (tkn && iv->iILine && iv->iOLine != iv->iILine) 
		if (iv->iILine > iv->iOLine && iv->iILine < iv->iOLine+5) 
		while (iv->iOLine != iv->iILine) { 
		gPuts(iv->iStream, "\n"); 
		iv->iOLine++; 
		iv->iRLine++; 
	} 
	else 
		gSLineDirective(self); 

	pp = tkn && *(p=gStringValue(tkn)) == '#'; 
	endif = pp && (streq(p, "#endif") || streq(p, "#else") || streq(p, "#elif")); 
	if (tkn && !pp) 
		for (i=iv->iBlbl-(*p == '}')+(*p != '{' && iv->iPllc != ';' && iv->iPllc != '{' && iv->iPllc != '}' && iv->iPllc != ':') ; i-- > 0 ; ) 
		gPuts(iv->iStream, "\t"); 
	for ( ; tkn ; tkn = gFirst(iv->iOutputList)) { 
		p = gStringValue(tkn); 
		if (irest(*p) && irest(c)) 
			gPuts(iv->iStream, " "); 
		gPuts(iv->iStream, p); 
		if (gSpace(tkn)) { 
			gPuts(iv->iStream, " "); 
			c = 0; 
		} else 
			c = *p; 
		iv->iPllc = *p; 
		gDeepDisposeFirst(iv->iOutputList); 
	} 
	if (pp) 
		iv->iPllc = ';'; 
	gPuts(iv->iStream, "\n"); 
	iv->iOLine++; 
	iv->iRLine++; 
	iv->iILine = 0L; 
	iv->iBlbl = iv->iBraceLevel; 
	if (endif) 
		gSLineDirective(self); 
	return 0; 
} 



imeth objrtn OutputStream_im_gFlushm(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	int i, endif=0; 
	object tkn; 
	char *p, c=0, *cname, ctype[80]; 

	if (!iv->iStream) 
		return self; 

#define ADD(s, t) gAddAfter(tkn, gNewToken(Token, t, 0L, s)) 

		if ((iv->iIVars || iv->iCVars) && do_var_sub(iv)) { 
		tkn = gFirst(iv->iOutputList); 
		sprintf(ctype, "%s_iv_t", cname = gStringValue(iv->iCName)); 
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

#ifdef CONVERT_SUPER 
		while (convert_super(iv)); 
#endif 


#line 466 "OutputStream.d"
		for (tkn=gFirst(iv->iOutputList) ; tkn ; tkn=gFirst(iv->iOutputList)) { 
		iv->iILine = gLineNumber(tkn); 
		if (iv->iILine && iv->iOLine != iv->iILine) { 
			if (iv->iILine > iv->iOLine && iv->iILine < iv->iOLine+5) { 
				while (iv->iOLine != iv->iILine) { 
					gPuts(iv->iStream, "\n"); 
					iv->iOLine++; 
					iv->iRLine++; 
				} 
				if (endif) { 
					gSLineDirective(self); 
					endif = 0; 
				} 
			} else 
				gSLineDirective(self); 
			p = gStringValue(tkn); 
			iv->iBlbl = iv->iBraceLevel; 
			if (*p != '#') 
				for (i=iv->iBlbl-(*p == '}')+(*p != '{' && iv->iPllc != ';' && iv->iPllc != '{' && iv->iPllc != '}' && iv->iPllc != ':') ; i-- > 0 ; ) 
				gPuts(iv->iStream, "\t"); 
		} else 
			p = gStringValue(tkn); 
		if (!endif && *p == '#') 
			endif = streq(p, "#endif") || streq(p, "#else") || streq(p, "#elif"); 
		if (irest(*p) && irest(c)) 
			gPuts(iv->iStream, " "); 
		gPuts(iv->iStream, p); 
		iv->iPllc = c = *p; 
		if (c == '(') 
			iv->iParenLevel++; 
		else if (c == ')') { 
			if (iv->iParenLevel) 
				iv->iParenLevel--; 
		} else if (c == '{') 
			iv->iBraceLevel++; 
		else if (c == '}') { 
			if (iv->iBraceLevel) 
				iv->iBraceLevel--; 
		} 
		if (gSpace(tkn)) { 
			gPuts(iv->iStream, " "); 
			c = 0; 
		} 
		gDeepDisposeFirst(iv->iOutputList); 
	} 
	gPuts(iv->iStream, "\n"); 
	iv->iOLine++; 
	iv->iRLine++; 
	iv->iILine = 0L; 
	iv->iBlbl = iv->iBraceLevel; 
	return self; 
} 

imeth objrtn OutputStream_im_gDispose(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	gFlush(self); 
	if (iv->iStream) 
		gDispose(iv->iStream); 
	gDispose(iv->iSFile); 
	gDispose(iv->iTFile); 
	gDeepDispose(iv->iOutputList); 
	return oSuper(OutputStream_c, gDispose, self)(self); 
} 

imeth objrtn OutputStream_im_gPut(object self, object tkn)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	char c = *gStringValue(tkn); 
	long ln = gLineNumber(tkn); 

	if (!iv->iStream) { 
		DISPOSE(tkn); 
		return self; 
	} 

	if (!iv->iILine) 
		iv->iILine = ln; 
	else if (ln && iv->iILine != ln) { 
		gFlush(self); 
		iv->iILine = ln; 
	} 

	gAddLast(iv->iOutputList, tkn); 
	if (!ln && (!iv->iParenLevel && c == ';' || c == '}' || c == '{' || c == ':')) 
		gFlush(self); 
	if (c == '(') 
		iv->iParenLevel++; 
	else if (c == ')') { 
		if (iv->iParenLevel) 
			iv->iParenLevel--; 
	} else if (c == '{') 
		iv->iBraceLevel++; 
	else if (c == '}') { 
		if (iv->iBraceLevel) 
			iv->iBraceLevel--; 
	} 
	return self; 
} 



imeth objrtn OutputStream_im_gPutm(object self, object tkn)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	if (iv->iStream) 
		gAddLast(iv->iOutputList, tkn); 
	else 
		DISPOSE(tkn); 
	return self; 
} 

static int count_nl(char *s) 
{ 
	int n = 0; 
	for ( ; *s ; s++) 
		if (*s == '\n') 
		n++; 
	return n; 
} 

imeth int OutputStream_im_gPuts(object self, char *str)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	iv->iOLine = -20L; 
	iv->iRLine += count_nl(str); 
	iv->iILine = 0L; 
	return iv->iStream ? gPuts(iv->iStream, str) : (int)strlen(str); 
} 





imeth int OutputStream_im_vPrintf(object self, char *fmt, va_list _rest_)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	char buf[256]; 

	vsprintf(buf, fmt, _rest_); 
	iv->iOLine = -20L; 
	iv->iRLine += count_nl(buf); 
	iv->iILine = 0L; 
	return iv->iStream ? gPuts(iv->iStream, buf) : (int)strlen(buf); 
} 

#line 629 "OutputStream.c"

static	int	OutputStream_ifm_vPrintf(object self, char *fmt, ...)
{
	va_list	_rest_;
	int	_ret_;
	va_start(_rest_, fmt);
	_ret_ = OutputStream_im_vPrintf(self, fmt, _rest_);
	va_end(_rest_);
	return _ret_;
}



#line 607 "OutputStream.d"
imeth objrtn OutputStream_im_gSetOSVars(object self, object cname, object cvs, object ivs)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	DISPOSE(iv->iCName); 
	iv->iCName = cname; 
	iv->iCVars = gSize(cvs) ? cvs : (object) NULL; 
	iv->iIVars = gSize(ivs) ? ivs : (object) NULL; 
	return self; 
} 

imeth objrtn OutputStream_im_gTLineDirective(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	if (iv->iStream && LineDirectives) { 
		iv->iRLine += 2; 
		iv->iOLine = -20L; 
		vPrintf(iv->iStream, "\n#line %ld \"%s\"\n", iv->iRLine, gStringValue(iv->iTFile)); 
	} 
	return self; 
} 

imeth objrtn OutputStream_im_gSLineDirective(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	if (iv->iStream) { 
		if (LineDirectives && iv->iILine) { 
			vPrintf(iv->iStream, "\n#line %ld \"%s\"\n", iv->iILine, gStringValue(iv->iSFile)); 
			iv->iRLine += 2; 
			iv->iOLine = iv->iILine; 
		} else { 
			gPuts(iv->iStream, "\n"); 
			iv->iRLine++; 
			iv->iOLine = iv->iILine ? iv->iILine : -20L; 
		} 
	} 
	return self; 
} 

imeth objrtn OutputStream_im_gForceLineDirective(object self)
{ OutputStream_iv_t *iv = GetIVs(OutputStream, self);
	iv->iOLine = -20L; 
	return self; 
} 


#line 686 "OutputStream.c"

objrtn	OutputStream_initialize(void)
{
	static  CRITICALSECTION  cs;
	static  int volatile once = 0;

	ENTERCRITICALSECTION(_CI_CS_);
	if (!once) {
		INITIALIZECRITICALSECTION(cs);
		once = 1;
	}
	LEAVECRITICALSECTION(_CI_CS_);

	ENTERCRITICALSECTION(cs);

	if (OutputStream_c) {
		LEAVECRITICALSECTION(cs);
		return OutputStream_c;
	}
	INHIBIT_THREADER;
	OutputStream_c = gNewClass(Class, "OutputStream", sizeof(OutputStream_iv_t), 0, END);
	cMethodFor(OutputStream, gNew, OutputStream_cm_gNew);
	cMethodFor(OutputStream, gNewWithStrStr, OutputStream_cm_gNewWithStrStr);
	iMethodFor(OutputStream, gFlush, OutputStream_im_gFlush);
	iMethodFor(OutputStream, gPuts, OutputStream_im_gPuts);
	iMethodFor(OutputStream, gPutm, OutputStream_im_gPutm);
	iMethodFor(OutputStream, gForceLineDirective, OutputStream_im_gForceLineDirective);
	iMethodFor(OutputStream, gDispose, OutputStream_im_gDispose);
	iMethodFor(OutputStream, gPut, OutputStream_im_gPut);
	iMethodFor(OutputStream, gSLineDirective, OutputStream_im_gSLineDirective);
	iMethodFor(OutputStream, gFlushm, OutputStream_im_gFlushm);
	ivMethodFor(OutputStream, vPrintf, OutputStream_im_vPrintf, OutputStream_ifm_vPrintf);
	iMethodFor(OutputStream, gSetOSVars, OutputStream_im_gSetOSVars);
	iMethodFor(OutputStream, gTLineDirective, OutputStream_im_gTLineDirective);
	iMethodFor(OutputStream, gDeepDispose, OutputStream_im_gDispose);

	ENABLE_THREADER;

	LEAVECRITICALSECTION(cs);

	return OutputStream_c;
}



