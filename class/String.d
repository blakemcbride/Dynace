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





#if 0			/*  1 = auto reduction of string size if fum set */
#define	ADJUST
#endif

/* The following defines are also defined in textctl.d.
   If they are changed here, they will need to be changed
   there as well. */
#define MASK_MAX	128
#define	MASK_BASE	(unsigned char) 128
#define MASK_LEFT	'>'
#define MASK_RIGHT	'<'
#define MASK_FILLER	' '	/*  Specific to the string class, acts as the default	*/
#define MASK_LITERAL	'\\'	/*  Used only in the string class			*/

defclass  String : Stream {
	char	*iStr;
	int	iSize;		/*  size of str buffer area	*/
	int	iLen;		/*  length of string		*/
	int	iBlksz;		/*  allocation block size	*/
	int	iCalcLen;
//#ifdef	ADJUST
	int	iFum;		/*  free unused memory flag	*/
//#endif

	int	iFiller;

class:
	unsigned char	cMaskVal[MASK_MAX];
	ifun		cMaskFun[MASK_MAX];
	char		cMaskBuf[MASK_MAX];

init:	class_init;
};


#include "memalloc.h"
#include <ctype.h>
#include <string.h>
#include <math.h>



/*  calculates correct size based on blksz  */

#define SETSIZE(x)	iSize = ((x) ? (((x)-1)/iBlksz + 1) * iBlksz : 0)

#define ABS	40	/*  Default append block size	*/

#define	UPDATE_LEN	if (iCalcLen)	iLen = strlen(iStr)


#ifndef	max
#define	 max(a,b)    ((a) > (b)	? (a) :	(b))
#endif


#ifdef	ADJUST
#define FREE_UNUSED						\
	if (iFum  &&  iLen+1 <= iSize-iBlksz)  {		\
		SETSIZE(iLen+1);				\
		iStr = (char *) MA_realloc(iStr, iSize);	\
	}
#else
#define FREE_UNUSED
#endif

static	char	getNextCharacter(ivType *iv, char **p, char *text, int right, int (*fun)());
static	int	get_string(object, char **, char *, int);

static	int	f_isdigit(char c)
{
	return isdigit(c);
}

static	int	f_isalpha(char c)
{
	return isalpha(c);
}

static	int	f_isalnum(char c)
{
	return isalnum(c);
}

static	void	class_init(void)
{
	int	i = 0;

	cMaskVal['#'] = MASK_BASE + i++;
	cMaskFun[cMaskVal['#'] - MASK_BASE] = (ifun) f_isdigit;
	
	cMaskVal['@'] = MASK_BASE + i++;
	cMaskFun[cMaskVal['@'] - MASK_BASE] = (ifun) f_isalpha;
	
	cMaskVal['&'] = MASK_BASE + i++;
	cMaskFun[cMaskVal['&'] - MASK_BASE] = (ifun) f_isalnum;
}

cmeth	gNewWithStr, <vNew> : String_New (char *so)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	char	*str;
	int	len;

	len = get_string((object) so, &str, "New", 2);
	iBlksz = 1;
	SETSIZE(len + 1);
	iStr = (char *) MA_malloc(iSize, &iStr);
	if (len)
		memcpy(iStr, str, len);
	iStr[len] = '\0';
	iLen = len;
	iFiller = MASK_FILLER;
	return(obj);
}

cmeth	gNewWithInt(int len)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iBlksz = 1;
	SETSIZE(len + 1);
	iStr = (char *) MA_malloc(iSize, &iStr);
	*iStr = '\0';
	iCalcLen = 1;
	iLen = 0;
	iFiller = MASK_FILLER;
	return(obj);
}

cmeth	gNewWithObj(so)
{
	return String_New(self, (char *) so);
}

cmeth	gNew()
{
	return String_New(self, "");
}

imeth	char	*gStringValue()
{
	return iStr;
}

imeth	int	gSize()
{
	UPDATE_LEN;
	return iLen;
}

imeth	gChangeValue : ChangeValue (obj)
{
	char	*str;
	int	len;

	len = get_string(obj, &str, "ChangeValue", 2);
	iLen = len;
	if (len >= iSize)  {
		SETSIZE(iLen + 1);
		iStr = (char *) MA_realloc(iStr, iSize);
	}
	if (len)
		memcpy(iStr, str, len);
	iStr[len] = '\0';
	FREE_UNUSED;
	return self;
}

imeth	gChangeStrValue(char *str)
{
	return ChangeValue(self, (object) str);
}

imeth	object	gDispose, gDeepDispose, gGCDispose ()
{
	if (iStr)
		MA_free(iStr);
	return gDispose(super);
}

imeth	int	gHash()
{
	register char	 c = 'a';
	char	*s = iStr;
	double	t;
	register unsigned short	 k=0;  /* must be short	 */

	while (*s)
		k += *s++ ^ c++;
	t = .6125423371	* k;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(obj)
{
	char	*s, *t;
	int	tlen, slen;

	if (IsObj(obj))  {
		ivType	*iv2;

		if (!gIsKindOf(obj, CLASS))
			return gCompare(super, obj);
		iv2 = ivPtr(obj);
		t = iv2->iStr;
		if (iv2->iCalcLen)
			iv2->iLen = strlen(t);
		tlen = iv2->iLen;
	} else {
		t = (char *) obj;
		tlen = t ? strlen(t) : 0;
	}
	UPDATE_LEN;
	slen = iLen;
	s = iStr;
	while (slen  &&  tlen  &&  *s == *t)  {
		s++;
		t++;
		slen--;
		tlen--;
	}
	if (!slen  &&  !tlen)
		return 0;
	if (!slen)
		return -1;
	if (!tlen)
		return 1;
	return *(unsigned char *)s - *(unsigned char *)t;
}

imeth	int	gCompareI(obj)
{
	char	*s, *t;
	int	tlen, slen;

	if (IsObj(obj))  {
		ivType	*iv2;

		if (!gIsKindOf(obj, CLASS))
			gError(self, "CompareI::String:  Arg 2 incorrect type.\n");
		iv2 = ivPtr(obj);
		t = iv2->iStr;
		if (iv2->iCalcLen)
			iv2->iLen = strlen(t);
		tlen = iv2->iLen;
	} else {
		t = (char *) obj;
		tlen = t ? strlen(t) : 0;
	}
	UPDATE_LEN;
	slen = iLen;
	s = iStr;
	while (slen  &&  tlen  &&  tolower(*s) == tolower(*t))  {
		s++;
		t++;
		slen--;
		tlen--;
	}
	if (!slen  &&  !tlen)
		return 0;
	if (!slen)
		return -1;
	if (!tlen)
		return 1;
	return tolower(*(unsigned char *)s) - tolower(*(unsigned char *)t);
}

imeth	int	gCompareN(obj, int n)
{
	char	*s, *t;
	int	tlen, slen;

	if (IsObj(obj))  {
		ivType	*iv2;

		if (!gIsKindOf(obj, CLASS))
			gError(self, "CompareN::String:  Arg 2 incorrect type.\n");
		iv2 = ivPtr(obj);
		t = iv2->iStr;
		if (iv2->iCalcLen)
			iv2->iLen = strlen(t);
		tlen = iv2->iLen;
	} else {
		t = (char *) obj;
		tlen = t ? strlen(t) : 0;
	}
	UPDATE_LEN;
	slen = iLen;
	s = iStr;
	while (n  &&  slen  &&  tlen  &&  *s == *t)  {
		s++;
		t++;
		n--;
		slen--;
		tlen--;
	}
	if (!n  ||  !slen  &&  !tlen)
		return 0;
	if (!slen)
		return -1;
	if (!tlen)
		return 1;
	return *(unsigned char *)s - *(unsigned char *)t;
}

imeth	int	gCompareNI(obj, int n)
{
	char	*s, *t;
	int	tlen, slen;

	if (IsObj(obj))  {
		ivType	*iv2;

		if (!gIsKindOf(obj, CLASS))
			gError(self, "CompareNI::String:  Arg 2 incorrect type.\n");
		iv2 = ivPtr(obj);
		t = iv2->iStr;
		if (iv2->iCalcLen)
			iv2->iLen = strlen(t);
		tlen = iv2->iLen;
	} else {
		t = (char *) obj;
		tlen = t ? strlen(t) : 0;
	}
	UPDATE_LEN;
	slen = iLen;
	s = iStr;
	while (n  &&  slen  &&  tlen  &&  tolower(*s) == tolower(*t))  {
		s++;
		t++;
		n--;
		slen--;
		tlen--;
	}
	if (!n  ||  !slen  &&  !tlen)
		return 0;
	if (!slen)
		return -1;
	if (!tlen)
		return 1;
	return tolower(*(unsigned char *)s) - tolower(*(unsigned char *)t);
}

imeth	gStringRepValue()
{
	return vSprintf(String, "\"%s\"", iStr ? iStr : "(null)");
}

imeth	int	gPrintLength(int t)	  /* return the	print length of	s  */
{
	register int n=0;
	int	l=0;
	char	*s;

	UPDATE_LEN;
	if (!iLen)
		return(0);
	for (s=iStr ; *s  &&  *s != '\n' ; ++s)
		switch (*s)  {
		case '\t':
			n += t - n % t;
			break;
		case '\b':
			if (n)
				--n;
			break;
		case '\r':
			l = max(n, l);
			n = 0;
			break;
		default:
			if (isprint(*s))
				++n;
			break;
		}
	return max(l, n);
}

#define more	n
#define next	s++, n--

static	int	Val(char c, int b)
{
	int	n;

	if (isdigit(c))		n = c -	'0';
	else  if (isalpha(c))	n = toupper(c) - ('A' -	10);
	else			return(-1);
	if (n >= b)		return(-1);	/* not in correct base */
	return(n);
}

imeth	gProcess()		/* string process  */
{
	char   *s, *d;
	int     a, i, n;

	UPDATE_LEN;
	s = d = iStr;
	for (n=iLen ; more ; )
		if (*s != '\\')  {
			*d++ = *s;
			next;
		}  else  {
			next;
			if (!more)
				break;
			switch (*s) {
			case 'N':
			case 'n':
				*d++ = '\n';
				next;
				break;
			case 'T':
			case 't':
				*d++ = '\t';
				next;
				break;
			case 'B':
			case 'b':
				*d++ = '\b';
				next;
				break;
			case 'R':
			case 'r':
				*d++ = '\r';
				next;
				break;
			case 'F':
			case 'f':
				*d++ = '\f';
				next;
				break;
			case 'E':
			case 'e':
				*d++ = 27;	/*  escape  */
				next;
				break;
			case 'A':
			case 'a':
				*d++ = 7;	/* BELL  */
				next;
				break;
			case 'V':
			case 'v':
				*d++ = '\v';	/*  vert tab ^K  */
				next;
				break;
			case '^':
				next;
				if (!more)
					goto end;
				*d++ = tolower(*s) - ('a' - 1);
				next;
				break;
			case 'X':
			case 'x':
				*d = '\0';
				for (i = 0, next ; more && (a = Val(*s, 16)) != -1 && i != 2; next, ++i)
					*d = 16 * *d + a;
				++d;
				break;
			case 'D':
			case 'd':
				*d = '\0';
				for (i = 0, next ; more && (a = Val(*s, 10)) != -1 && i != 3; next, ++i)
					*d = 10 * *d + a;
				++d;
				break;
			case '\0':
				break;
			default:
				if (isdigit(*s)) {
					*d = '\0';
					for (i = 0; more && (a = Val(*s, 8)) != -1 && i != 3; next, ++i)
						*d = 8 * *d + a;
					++d;
					break;
				}
				*d++ = *s;
				next;
				break;
			}
		}
 end:
	*d = '\0';
	iLen = d - iStr;
	FREE_UNUSED;
	return self;
}

imeth	gAppend(obj)
{
	char	*str;
	int	len, need;

	if (!obj)
		return self;
	UPDATE_LEN;
	len = get_string(obj, &str, "Append", 2);
	need = iLen + 1 + len;
	if (need > iSize)  {
		if (iBlksz == 1)
			iBlksz = ABS;
		if (len > iBlksz)
			iBlksz = (len / ABS + 1) * ABS;
		SETSIZE(need);
		iStr = (char *) MA_realloc(iStr, iSize);
	}
	memcpy(iStr + iLen, str, len);
	iLen += len;
	iStr[iLen] = '\0';
	return self;
}

imeth	int	gWrite(char *buf, unsigned len)
{
	int	need;

	UPDATE_LEN;
	need = iLen + 1 + len;
	if (need > iSize)  {
		if (iBlksz == 1)
			iBlksz = ABS;
		if (len > iBlksz)
			iBlksz = (len / ABS + 1) * ABS;
		SETSIZE(need);
		iStr = (char *) MA_realloc(iStr, iSize);
	}
	memcpy(iStr + iLen, buf, len);
	iLen += len;
	iStr[iLen] = '\0';
	return len;
}

imeth	int	gRead(char *buf, unsigned n)
{
	int	len;

	UPDATE_LEN;
	len = n > (unsigned) iLen ? iLen : (int) n;
	memcpy(buf, iStr, len);
	memmove(iStr, iStr+len, iLen-len+1);
	iLen -= len;
	FREE_UNUSED;
	return len;
}

imeth	char	*gGets(char *buf, int n)
{
	int	len;

	UPDATE_LEN;
	if (!iLen)
		return NULL;
	if (n <= 0)
		return NULL;
	if (n-- == 1)  {
		*buf = '\0';
		return buf;
	}
	for (len=0 ; len < iLen  &&  iStr[len++] != '\n' ; );
	memcpy(buf, iStr, len);
	memmove(iStr, iStr+len, iLen-len+1);
	iLen -= len;
	FREE_UNUSED;
	buf[len] = '\0';
	return buf;
}

imeth	long	gAdvance(long n)
{
	int	len;

	UPDATE_LEN;
	len = n > (long) iLen ? iLen : (int) n;
	memmove(iStr, iStr+len, iLen-len+1);
	iLen -= len;
	FREE_UNUSED;
	return (long) len;
}

imeth	long	gPosition()
{
	USE(self);
	return 0L;
}

imeth	long	gLength()
{
	UPDATE_LEN;
	return (long) iLen;
}

imeth	int	gEndOfStream()
{
	UPDATE_LEN;
	return !iLen;
}

cvmeth	vSprintf(char *fmt, ...)
{
	char	buf[1024];
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	return gNewWithStr(self, buf);
}

ivmeth	vBuild(char *f, ...)
{
	object	obj;
	char	*str, *pbuf;
	va_list		ap;
	int	len, argn, tlen;
	static	char	fun[] = "Build";
	MAKE_REST(f);

	
	/*  Calculate total length  */

	UPDATE_LEN;
	if (f)
		tlen = get_string((object) f, &str, fun, 2);
	else
		tlen = iLen;
	ASSIGN_VA_LIST(ap, _rest_);
	for (argn=3 ; obj = va_arg(ap, object) ; )
		tlen += get_string(obj, &str, fun, argn++);


	/*  Make sure buffer is big enough  */

	if (tlen >= iSize)  {
		if (iBlksz == 1  &&  !f)
			iBlksz = ABS;
		if (tlen > iBlksz)
			iBlksz = (tlen / ABS + 1) * ABS;
		SETSIZE(tlen + 1);
		iStr = (char *) MA_realloc(iStr, iSize);
	}

	
	/*  Build string  */

	pbuf = iStr;
	if (f)  {
		len = get_string((object)f, &str, fun, 2);
		if (len)
			memcpy(pbuf, str, len);
		pbuf += len;
	}  else 
		pbuf += iLen;
	for (argn=3 ; obj = GetArg(object) ; )  {
		len = get_string(obj, &str, fun, argn++);
		if (len)  {
			memcpy(pbuf, str, len);
			pbuf += len;
		}
	}
	*pbuf = '\0';
	iLen = tlen;
	return self;
}

cvmeth	vBuild(...)
{
	object	obj;
	char	*str, *pbuf;
	va_list		ap;
	int	len, argn, tlen=0;
	static	char	fun[] = "Build";
	object	newObj = gNew(super);
	ivType	*iv = ivPtr(newObj);
	MAKE_REST(self);

	
	/*  Calculate total length  */

	ASSIGN_VA_LIST(ap, _rest_);
	for (argn=2 ; obj = va_arg(ap, object) ; )
		tlen += get_string(obj, &str, fun, argn++);

	/*  Create buffer  */

	iBlksz = 1;
	SETSIZE(tlen + 1);
	iStr = (char *) MA_malloc(iSize, &iStr);

	
	/*  Build string  */

	pbuf = iStr;
	for (argn=2 ; obj = GetArg(object) ; )  {
		len = get_string(obj, &str, fun, argn++);
		if (len)  {
			memcpy(pbuf, str, len);
			pbuf += len;
		}
	}
	*pbuf = '\0';
	iLen = tlen;
	return newObj;
}

static	int	get_string(object obj, char **str, char *fun, int argn)
{
	ivType	*iv2;
	char	buf[80];

	if (!obj)  {
		*str = "";
		return 0;
	} else if (!IsObj(obj))  {
		*str = (char *) obj;
		return strlen((char *) obj);
	} 
	if (!gIsKindOf(obj, CLASS))  {
		sprintf(buf, "%s::String:  Arg %d incorrect type.\n", fun, argn);
		gError(Dynace, buf);
	}
	iv2 = ivPtr(obj);
	*str = iv2->iStr;
	return iv2->iCalcLen ? (iv2->iLen=strlen(iv2->iStr)) : iv2->iLen;
}

imeth	char	gCharValueAt(int i)
{
	UPDATE_LEN;
	if (i < 0  ||  i > iLen)
		gError(self, "CharValue::String:  Index out of range.\n");
	return iStr[i];
}

imeth	gChangeCharAt(int i, int c)
{
	UPDATE_LEN;
	if (i < 0  ||  i > iLen)
		gError(self, "ChangeCharAt::String:  Index out of range.\n");
	if (i == iLen)  {
		char	buf[2];
		buf[0] = c;
		buf[1] = '\0';
		return gAppend(self, (object) buf);
	}
	iStr[i] = c;
	return self;
}

imeth	gToLower()
{
	char	*s;
	int	len;

	UPDATE_LEN;
	s = iStr;
	len = iLen;
	for ( ; len-- ; s++)
		*s = tolower(*s);
	return self;
}

imeth	gToUpper()
{
	char	*s;
	int	len;

	UPDATE_LEN;
	s = iStr;
	len = iLen;
	for ( ; len-- ; s++)
		*s = toupper(*s);
	return self;
}

imeth	gSubString(int beg, int num)
{
	object	obj = gNew(super CLASS);
	ivType	*iv2 = ivPtr(obj);

	UPDATE_LEN;
	if (beg < 0)
		beg = iLen + beg;
	if (num < 0)  {
		beg += num + 1;
		num = -num;
	}

	if (beg >= iLen  ||  beg < 0)
		num = 0;
	else  {
		int	n = iLen - beg;
		num = num < n ? num : n;
	}
	iv2->iBlksz = 1;
	SETSIZE(num+1);
	iv2->iLen  = num;
	iv2->iStr  = (char *) MA_malloc(iv2->iSize, &iv2->iStr);
	if (num)
		memcpy(iv2->iStr, iStr+beg, num);
	iv2->iStr[num] = '\0';
	return obj;
}

imeth	gTake(int n)		/*  APL like take for string	*/
{
	int     i, len;
	char	*s;

	UPDATE_LEN;
	len = n < 0 ? -n : n;
	if (len == iLen)
		return self;
	if (len >= iSize)  {
		SETSIZE(len + 1);
		iStr = (char *) MA_realloc(iStr, iSize);
	}
	s = iStr;
	if (n >= 0) {
		for (i=iLen ; i < len ; i++)
			s[i] = ' ';
		goto end;
	}
	if (iLen < len) {
		n = len - iLen;
		memmove(s + n, s, iLen);
		for (i = 0; i < n ; i++)
			s[i] = ' ';
		goto end;
	}
	memmove(s, s + (iLen - len), len);
 end:
	s[len] = '\0';
	iLen = len;
	FREE_UNUSED;
	return self;
}

imeth	gDrop(int n)		/* APL like drop for strings  */
{
	int	an;

	if (!n)
		return self;
	UPDATE_LEN;
	an = n < 0 ? -n : n;
	if (an >= iLen)
		iLen = 0;
	else  {
		if (n > 0)
			memmove(iStr, iStr + n, iLen - n);
		iLen -= an;
	}
	iStr[iLen] = '\0';
	FREE_UNUSED;
	return self;
}

#define	 ISspace(c)  ((c) == ' ' || (c)	== '\t'	|| (c) == '\n' || (c) == '\r')

imeth	gStripLeft()
{
	int	i, n;
	char	*s = iStr;

	UPDATE_LEN;
	for (i=0, n=iLen ; n  &&  ISspace(s[i]) ; ++i, --n);
	if (!i)
		return self;
	if (n)
		memmove(s, s + i, n);
	iLen = n;
	s[n] = '\0';
	FREE_UNUSED;
	return self;
}

imeth	gStripRight()
{
	int	n;
	char	*s = iStr;

	UPDATE_LEN;
	for (n = iLen - 1 ; n >= 0 && ISspace(s[n]) ; n--);
	s[iLen = n + 1] = '\0';
	FREE_UNUSED;
	return self;
}

imeth	gStripCenter()
{
	int	i, n;
	char	*s = iStr;
	
	UPDATE_LEN;
	for (i=0, n=iLen ; n  &&  ISspace(s[i]) ; ++i, --n);
	if (n  &&  i)
		memmove(s, s + i, n);
	for (--n ; n >= 0 && ISspace(s[n]) ; n--);
	s[iLen = n + 1] = '\0';
	FREE_UNUSED;
	return self;
}

imeth	gJustifyLeft()
{
	char	*s = iStr;
	int	n, i;

	UPDATE_LEN;
	n = iLen;
	for (i=0 ; i < n  &&  ISspace(s[i]) ; ++i);
	if (!i)
		return self;
	memmove(s, s + i, n-i);
	for (i=n-i ; i < n ; )
		s[i++] = ' ';
	return self;
}

imeth	gJustifyRight()
{
	char	*s = iStr;
	int	n, i, p;

	UPDATE_LEN;
	n = iLen;
	for (i=n-1 ; i >= 0  &&  ISspace(s[i]) ; --i);
	p = n - (i + 1);
	if (!p)
		return self;
	memmove(s + p, s, n-p);
	for (i=0 ; i < p ; )
		s[i++] = ' ';
	return self;
}

imeth	gJustifyCenter()
{
	char	*s = iStr;
	int	n, left, right, left2, p, i;

	UPDATE_LEN;
	n = iLen;
	for (left=0 ; left < n  &&  ISspace(s[left]) ; ++left);
	for (right=0, i=n-1 ; i >= 0  &&  ISspace(s[i]) ; --i, ++right);
	left2 = (left + right) / 2;
	if (left2 == left)
		return self;
	if (left2 < left)  {	/*  shift left  */
		p = left - left2;
		memmove(s, s + p, n - p);
		for (i=n-(p+1) ; i < n ; )
			s[i--] = ' ';
	} else {		/*  shift right  */
		p = left2 - left;
		memmove(s + p, s, n - p);
		for (i=0 ; i < p ; )
			s[i++] = ' ';
	}
	return self;
}

imeth	int	gEqual(obj)
{
	char	*str;
	int	len;

	if (EQ(self, obj))
		return 1;
	if (IsObj(obj))  {
		ivType	*iv2;
		if (NEQ(ClassOf(self), ClassOf(obj)))
			return 0;
		iv2 = ivPtr(obj);
		if (iv2->iCalcLen)
			iv2->iLen = strlen(iv2->iStr);
		len = iv2->iLen;
		str = iv2->iStr;
	}  else  {
		str = (char *) obj;
		len = str ? strlen(str) : 0;
	}
	UPDATE_LEN;
	if (iLen != len)
		return 0;
	if (len)
		return memcmp(iStr, str, len) ? 0 : 1;
	else
		return 1;
}

imeth	gCopy, gDeepCopy ()
{
	return gNewWithObj(ClassOf(self), self);
}

imeth	gPiece(char d, int n)
{
	object	newObj;
	char	*s = iStr, *e;
	int	size;
	ivType	*iv2;

	if (!iStr)
		return NULL;
	while (n && *s)
		if (*s++ == d)	
			n--;
	if (n)
		return NULL;
	for (size=0, e=s ; *e != d  &&  *e ; size++)
		e++;
	newObj = gNewWithInt(String, size+1);
	iv2 = ivPtr(newObj);
	for (e=iv2->iStr ; size-- ; )
		*e++ = *s++;
	*e = '\0';
	iv2->iLen = strlen(iv2->iStr);
	return newObj;
}

imeth	int	gNumbPieces(char d) /* number of strings in s seperated by d  */
{
	int	n;
	char	*s = iStr;

	if (!s  ||  !*s)
		return(0);
	for (n=1 ; *s ;	)
		if (*s++ == d)		
			n++;
	return(n);
}

cmeth	char	*gLoadMask(char *inmask)
{
	char	*p1 = inmask;
	char	*p2 = cMaskBuf;

	if (!inmask || !*inmask)
		return NULL;

	if (*p1 != MASK_LEFT && *p1 != MASK_RIGHT)
		*p2++ = MASK_LEFT;
	
	while (*p1) {
		if (*p1 == MASK_LITERAL)
			*p2++ = *(++p1);
		else if (cMaskVal[(int)*p1])
			*p2++ = cMaskVal[(int)*p1];
		else if ((unsigned char) *p1 >= MASK_BASE)
			*p2++ = ' ';
		else
			*p2++ = *p1;
		if (*p1)
			p1++;
	}
	*p2 = '\0';

	return cMaskBuf;
}

imeth	gApplyMask(char *inmask, char *intext)
{
	char	mask[MASK_MAX];
	char	*text = intext ? intext : iStr;
	char	*p1;
	char	*p2;
	char	*p3;
	int	right = *inmask == MASK_RIGHT;
	int	inc = right ? -1 : 1;
	char	buf[MASK_MAX];
	int	loadMask = 1;

	if (!inmask  ||  !*inmask)
		return text == iStr ? self : gChangeStrValue(self, text);

	for (p1 = inmask; *p1 && loadMask; p1++)
		if ((unsigned char) *p1 >= MASK_BASE)
			loadMask = 0;
	
	strcpy(mask, loadMask ? gLoadMask(String, inmask) + 1 : inmask + 1);

	if (right) {
		p1 = mask + (strlen(mask) - 1);
		p2 = buf + (strlen(mask));
		*p2-- = '\0';
		p3 = strlen(text) ? text + (strlen(text) - 1) : text;
		
	} else {
		p1 = mask;
		p2 = buf;
		p3 = text;
	}
	
	while (!right && *p1 || right && p1 >= mask) {
		if ((unsigned char) *p1 >= MASK_BASE)
			*p2 = getNextCharacter(iv, &p3, text, right, cMaskFun[(unsigned char) *p1 - MASK_BASE]);
		else {
			*p2 = *p1;
			if ((!right && *p3 || right && p3 >= text) && *p2 == *p3)
				p3 += inc;
		}
		p1 += inc;
		p2 += inc;
	}
	if (!right)
		*p2 = '\0';

	return gChangeStrValue(self, buf);
}

static	char	getNextCharacter(ivType *iv, char **p, char *text, int right, int (*fun)())
{
	char	ch;
	int	inc = right ? -1 : 1;

	if (fun)
		for ( ; (!right && **p || right && *p >= text) && **p != ' ' && !fun(**p); *p += inc);
	ch = !right && **p || right && *p >= text ? **p != ' ' ? **p : iFiller : iFiller;
	if (!right && **p || right && *p >= text)
		*p += inc;

	return ch;
}

imeth	gRemoveMask(char *inmask, char *text)
{
	char	mask[MASK_MAX];
	char	buf[MASK_MAX];
	char	*p1 = mask;
	char	*p2 = text ? text : iStr;
	char	*p3 = buf;
	int	loadMask = 1;

	if (!inmask || !*inmask)
		return p2 == iStr ? self : gChangeStrValue(self, text);

	for (p1 = inmask; *p1 && loadMask; p1++)
		if ((unsigned char) *p1 >= MASK_BASE)
			loadMask = 0;
	
	strcpy(mask, loadMask ? gLoadMask(String, inmask) + 1 : inmask + 1);

	p1 = mask;
	*p3 = '\0';
	
	while (*p1) {
		if ((unsigned char) *p1 >= MASK_BASE)
			*p3++ = *p2 && *p2 != iFiller ? *p2 : ' ';
		p1++;
		if (*p2)
			p2++;
	}
	*p3 = '\0';

	return gChangeStrValue(self, buf);
}

cmeth	ifun	gMaskFunction(char ch)
{
	return cMaskFun[(unsigned char) ch - MASK_BASE];
}

imeth	char	gSetMaskFiller(char ch)
{
	char	pf = iFiller;

	iFiller = ch;
	return pf;
}

imeth	gUpdateLength()
{
	iLen = strlen(iStr);
	return self;
}

imeth	int	gBufferSize()
{
	return iSize;
}





