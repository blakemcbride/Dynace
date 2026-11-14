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



#include <ctype.h>
#include <string.h>

#include "dpp.h"

#define MAXBUF		4096	/*  initial line buffer size; it grows as needed  */
#define MAXWORDSZ	256


#define istart2(x)	(isalpha(x)  ||  (x) == '_'  ||  (x) == '#')


#ifdef	unix
#define RMODE	"r"
#define WMODE	"w"
#else
#define RMODE	"rt"
#define WMODE	"wt"
#endif

defclass InputStream  {
	iStream;
	long	iLine;
	char	*iBuf;
	int	iBufSz;		/*  allocated size of iBuf	*/
	char	*iPtr;		
};


extern	objrtn open_file(char *file,char *mode, int real_quiet);


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewWithStr(char *file)
{
	object	s, obj;
	ivType	*iv;

	s = open_file(file, RMODE, 0);
	if (!s)
		return NULL;
	obj = gNew(super);
	iv = ivPtr(obj);
	iStream = s;
	iBuf = Tnalloc(char, iBufSz=MAXBUF);
	*iBuf = '\0';
	iPtr = iBuf;
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	gDispose(iStream);
	free(iBuf);
	return gDispose(super);
}

imeth	object	gGCDispose()
{
	free(iBuf);
	return gGCDispose(super);
}

/*  Read one whole physical line into iBuf at offset at, growing iBuf as
    needed.  Returns the length of iBuf's contents, or -1 at end of file.  */

static	int	read_line(ivType *iv, int at)
{
	int	len = at;

	while (1)  {
		if (iBufSz - len < 128)
			iBuf = Tnrealloc(char, iBufSz*=2, iBuf);
		if (!gGets(iStream, iBuf+len, iBufSz-len))
			return len > at ? len : -1;
		len += (int)strlen(iBuf+len);
		if (len  &&  iBuf[len-1] == '\n')
			return len;
	}
}

/*  get a line - append continuation lines  */

static	int	get_line(ivType *iv)
{
	int	something = 0, len = 0, n, i;

	*iBuf = '\0';
	while ((n = read_line(iv, len)) >= 0)  {
		iLine++;
		something = 1;
		for (i=n-1 ; i >= len  &&  (iBuf[i] == '\n' ||  iBuf[i] == '\r') ; i--);
		if (i < len  ||  iBuf[i] != '\\')  {
			iBuf[i+1] = '\0';
			break;
		}
		iBuf[i] = '\0';
		len = i;
	}
	iPtr = iBuf;	/*  iBuf may have moved  */
	return something;  /*  0=eof  */
}


#define IS(a, b)	*iPtr == a  &&  iPtr[1] == b
#define IS3(a, b, c)	*iPtr == a  &&  iPtr[1] == b  &&  iPtr[2] == c

#define CODE_STATE	1
#define COMMENT_STATE	2

/*  set iPtr to next non-comment token  */

static	int	next_token(ivType *iv)
{
	int	state = CODE_STATE;

	while (1)  {
		if (!*iPtr)
			if (get_line(iv))
				iPtr = iBuf;
			else
				return 0; /*  no more  */
		else if (state == CODE_STATE)  {
			if (isspace(*iPtr))
				iPtr++;
			else if (IS('/', '/'))
				*iPtr = '\0';
			else if (IS('/', '*'))  {
				state = COMMENT_STATE;
				iPtr += 2;
			} else
				return 1;  /*  token found  */
		} else {
			if (IS('*', '/'))  {
				iPtr += 2;
				state = CODE_STATE;
			} else
				iPtr++;
		}
	}
/*	return 1;   never reached  */
}

imeth	int	gLineHasColon()
{
	char	*p = iPtr;

	while (*p)
		if (*p++ == ':')
			return 1;
	return 0;
}

imeth	gNextToken()
{
	char	*w, sbuf[256], *token = sbuf;	/*  a token can be as long as the line (e.g. a string literal)  */
	int	i;
	size_t	n;
	object	tkn;

	if (!next_token(iv))
		return NULL;
	if ((n = strlen(iPtr)) >= sizeof sbuf)
		token = Tnalloc(char, n+1);
	w = token;
	if (istart2(*iPtr)) {		/*  is identifier  */
		*w++ = *iPtr++;
		while (irest(*iPtr))	/*  token is as big as the line, so no cap is needed  */
			*w++ = *iPtr++;
	} else			/*  something other than an identifier  */
		if (IS3('<', '<', '=')  ||
		    IS3('>', '>', '=')  ||
		    IS3(':', ':', '*')  ||     /*  C++   */
		    IS3('-', '>', '*')  ||     /*  C++   */
		    IS3('.', '.', '.'))  {
			*w++ = *iPtr++;
			*w++ = *iPtr++;
			*w++ = *iPtr++;
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
			   IS(':', ':')  ||      /*  C++ and Dynace  */
			   IS('.', '*')  ||      /*  C++             */
			   IS('-', '>'))  {
			*w++ = *iPtr++;
			*w++ = *iPtr++;
		} else if (*iPtr == '"'  ||  *iPtr == '\'')  {
			char	type = *iPtr;
			*w++ = *iPtr++;
			while (*iPtr  &&  *iPtr != type)  {
				*w++ = *iPtr;
				if (*iPtr == '\\'  &&  iPtr[1])
					*w++ = *++iPtr;
				++iPtr;
			}
			if (*iPtr)
				*w++ = *iPtr++;
		} else if (isdigit(*iPtr)  ||  *iPtr == '.'  &&  isdigit(iPtr[1]))  {
			/*  a C preprocessing number: any suffix, exponent sign and
			    digit separator belong to it (1.5f, 1ULL, 0x1p-3, 1'000)  */
			*w++ = *iPtr++;
			while (isalnum(*iPtr)  ||  *iPtr == '_'  ||  *iPtr == '.'  ||
			       (*iPtr == '-'  ||  *iPtr == '+')  &&  strchr("eEpP", iPtr[-1])  ||
			       *iPtr == '\''  &&  isalnum(iPtr[-1])  &&  (isalnum(iPtr[1])  ||  iPtr[1] == '_'))
				*w++ = *iPtr++;
		} else
			*w++ = *iPtr++;
	*w = '\0';
	/*  printf("Token = %s\n", token);  */
	i = isspace(*iPtr)  ||  !*iPtr  ||
		iPtr[0] == '/'  &&  iPtr[1] == '*'  ||
		iPtr[0] == '/'  &&  iPtr[1] == '/';
	tkn = gNewToken(Token, token, iLine, i);
	if (token != sbuf)
		free(token);
	return tkn;
}


