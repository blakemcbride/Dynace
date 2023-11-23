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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>


typedef	struct {
	FILE	*fp;
	char	*buf;
	char	*ptr;
	
	/*  error values  */
	int	error;
	long	line;
	long	col;
	long	pos;
}	InputStream;


#define	GETC(is)	(is->fp ? (getc(is->fp)) : (*is->ptr ? *is->ptr++ : EOF))
#define	UNGETC(c, is)	(is->fp ? ungetc(c, is->fp) : *--is->ptr)
#define	REWIND(is)	if (is->fp) rewind(is->fp); else is->ptr = is->buf
#define	FTELL(is)	(is->fp ? ftell(is->fp) : is->ptr - is->buf)



defclass XMLNode {
	int	iType;
	char	*iName;
	char	*iValue;
	iAttributes;
	iFirstChild;
	iLastChild;
	iParent;
	iPrevNode;
	iNextNode;
	
	//  internal stuff
	int	iSelfClosing;
	int	iClosed;
	int	iAllSpace;
	char	iQuote;
};

typedef	struct {
	char	*buf;
	int	sz;
}	string;



private	cmeth	object	parse(InputStream *is);
static	char	*strsave(char *x);
private	cmeth	object	new_tag(InputStream *is, string *pstr);
private	cmeth	object	parse_attributes(char c, InputStream *is, char *buf, object owner, string *pstr);
private	imeth	void	dump_xml(int lvl, object fp, int next);
private	cmeth	object	parse_comment(InputStream *is, string *pstr);
private	cmeth	object	parse_cdata(InputStream *is, string *pstr);
static	void	set_error(InputStream *is);


#define	isname(x)	(isalnum(x)  ||  (x) == '-'  ||  (x) == '_'  ||  (x) == '.'  ||  (x) == ':')


cmeth	gParseFile(char *file, long *line, long *col, long *pos)
{
	InputStream	is;
	object	r;
	
	is.fp = fopen(file, "rb");
	is.error = 0;
	if (!is.fp)
		return NULL;
	r = parse(self, &is);
	fclose(is.fp);
	if (is.error) {
		if (line)
			*line = is.line;
		if (col)
			*col = is.col;
		if (pos)
			*pos = is.pos;
	} else {
		object	n = gNew(super);
		ivType	*iv = ivPtr(n);
		iType = DOCUMENT_NODE;
		iFirstChild = iLastChild = r;
		r = n;
	}
	return r;
}

cmeth	gParseString(char *str, long *line, long *col, long *pos)
{
	InputStream	is;
	object	r;

	is.fp = NULL;
	is.buf = is.ptr = str;
	is.error = 0;
	r = parse(self, &is);
	if (is.error) {
		if (line)
			*line = is.line;
		if (col)
			*col = is.col;
		if (pos)
			*pos = is.pos;
	} else {
		object	n = gNew(super);
		ivType	*iv = ivPtr(n);
		iType = DOCUMENT_NODE;
		iFirstChild = iLastChild = r;
		r = n;
	}
	return r;
}

#define	CHECK_SIZE							\
	if (p - pstr->buf == pstr->sz - 3) {			       	\
		char	*tp = pstr->buf;				\
		pstr->sz *= 2;						\
		pstr->buf = (char *) realloc(pstr->buf, pstr->sz);	\
		p = pstr->buf + (p-tp);					\
	}

#define	ADDCHR(c) {	\
	CHECK_SIZE;	\
	*p++ = c;	\
}

private	cmeth	object	parse(InputStream *is)
{
	object	n = NULL, head = NULL, t;
	char	c, *p;
	string	str, *pstr = &str;
	ivType	*tiv, *niv;

	while (EOF != (c=GETC(is))  &&  c != '<');
	if (c == EOF)
		return NULL;
	str.sz = 10000;
	str.buf = (char *) malloc(str.sz);
	if (!(head = n = new_tag(self, is, pstr)))
		goto er1;
	niv = ivPtr(n);
	for (p=str.buf ; EOF != (c=GETC(is)) ; ) {
		if (c == '<')  {
			if (p != str.buf) {
				char	*bb = str.buf;
				*p = '\0';
				while (*(p=bb))
					if (isspace(*p)) {
						for (; *p  &&  isspace(*p) ; p++);
						c = *p;
						*p = '\0';
						t = gNew(self);
						tiv = ivPtr(t);
						tiv->iType = TEXT_NODE;
						tiv->iValue = strsave(bb);
						tiv->iAllSpace = 1;
						if (niv->iType != ELEMENT_NODE  ||  niv->iClosed)  {
							tiv->iParent = niv->iParent;
							niv->iNextNode = t;
							tiv->iPrevNode = n;
							if (niv->iParent)
								ivPtr(niv->iParent)->iLastChild = t;
						} else {
							tiv->iParent = n;
							niv->iFirstChild = niv->iLastChild = t;
						}
						n = t;
						niv = tiv;
						*p = c;
						bb = p;
					} else {
						char *s = p;
						for (; *p ; p++);
						for (p-- ; p >= s && isspace(*p) ; --p);
						p++;
						c = *p;
						*p = '\0';
						t = gNew(self);
						tiv = ivPtr(t);
						tiv->iType = TEXT_NODE;
						tiv->iValue = strsave(bb);
						tiv->iAllSpace = 0;
						if (niv->iType != ELEMENT_NODE  ||  niv->iClosed)  {
							tiv->iParent = niv->iParent;
							niv->iNextNode = t;
							tiv->iPrevNode = n;
							if (niv->iParent)
								ivPtr(niv->iParent)->iLastChild = t;
						} else {
							tiv->iParent = n;
							niv->iFirstChild = niv->iLastChild = t;
						}
						n = t;
						niv = tiv;
						*p = c;
						bb = p;
					}
			}
			c = GETC(is);
			if (c == EOF)
				goto er1;
			if (c == '/') {
				while (EOF != (c = GETC(is))  &&  isspace(c));
				if (c == EOF)
					goto er1;
				p = str.buf;
				while (isname(c)) {
					ADDCHR(c);
					if (EOF == (c=GETC(is)))
						goto er1;
				}
				while (EOF != c  &&  isspace(c))
					c = GETC(is);
				if (c == EOF)
					goto er1;
				if (c != '>')
					goto er1;
				*p = '\0';
				if (niv->iType != ELEMENT_NODE  ||  niv->iClosed) {
					n = niv->iParent;
					if (n)
						niv = ivPtr(n);
				}
				if (!n  ||  niv->iType != ELEMENT_NODE  ||  strcmp(niv->iName, str.buf)  ||  niv->iClosed)
					goto er1;
				niv->iClosed = 1;
			} else {
				UNGETC(c, is);
				if (!(t = new_tag(self, is, pstr)))
					goto er1;
				tiv= ivPtr(t);
				if (niv->iType != ELEMENT_NODE  ||  niv->iClosed) {
					tiv->iParent = niv->iParent;
					niv->iNextNode = t;
					tiv->iPrevNode = n;
					if (niv->iParent)
						ivPtr(niv->iParent)->iLastChild = t;
				} else {
					tiv->iParent = n;
					niv->iFirstChild = niv->iLastChild = t;
				}
				n = t;
				niv = tiv;
			}
			p = str.buf;
		} else if (c == '&') {
			char	esc[10];
			int	i = 0;

			CHECK_SIZE;
			do {
				c = GETC(is);
				if (c == EOF  ||  i > 8)
					goto er1;
				if (c == ';')
					break;
				esc[i++] = c;
			} while (1);
			esc[i] = '\0';
			if (!strcmp(esc, "amp"))
				*p++ = '&';
			else if (!strcmp(esc, "lt"))
				*p++ = '<';
			else if (!strcmp(esc, "gt"))
				*p++ = '>';
			else if (!strcmp(esc, "apos"))
				*p++ = '\'';
			else if (!strcmp(esc, "quot"))
				*p++ = '"';
			else
				goto er1;
		} else 
			ADDCHR(c);
	}
	while (n)  {
		if (niv->iType != ELEMENT_NODE  ||  !niv->iClosed)
			goto er1;
		n = niv->iParent;
	}
	free(str.buf);
	return head;
er1:
	set_error(is);
	gDeepDispose(head);
	free(str.buf);
	return NULL;
}

static	void	set_error(InputStream *is)
{
	long	i=0;
	char	c;
	
	is->pos = FTELL(is);
	is->line = 1;
	is->col = 0;
	REWIND(is);
	for (; i < is->pos ; i++)
		if ('\n' == (c=GETC(is))) {
			is->line++;
			is->col = 0;
		} else if (c != '\r')
			is->col++;
	is->error = 1;
}

#define	NEXTC(c)	if (c != GETC(is)) return NULL

private	cmeth	object	parse_cdata(InputStream *is, string *pstr)
{
	char	c, *p = pstr->buf;
	
	NEXTC('C');
	NEXTC('D');
	NEXTC('A');
	NEXTC('T');
	NEXTC('A');
	NEXTC('[');
	while (1) {
		if (EOF == (c=GETC(is)))
			return NULL;
		if (c == ']') {
			int	i = 0;
			while (c == ']') {
				i++;
				if (EOF == (c=GETC(is)))
					return NULL;
			}
			for (; i > 2 ; i--)
				ADDCHR(']');
			if (i == 2  &&  c == '>') {
				object	n = gNew(self);
				ivType	*niv = ivPtr(n);
				niv->iType = CDATA_SECTION_NODE;
				*p = '\0';
				niv->iValue = strsave(pstr->buf);
				return n;
			} else  {
				while (i--)
					ADDCHR(']');
				ADDCHR(c);
			}
		} else
			ADDCHR(c);
	}
}

private	cmeth	object	parse_comment(InputStream *is, string *pstr)
{
	char	c, *p = pstr->buf;
	
	NEXTC('-');
	while (1) {
		if (EOF == (c=GETC(is)))
			return NULL;
		if (c == '-') {
			int	i = 0;
			while (c == '-') {
				i++;
				if (EOF == (c=GETC(is)))
					return NULL;
			}
			for (; i > 2 ; i--)
				ADDCHR('-');
			if (i == 2  &&  c == '>') {
				object	n = gNew(self);
				ivType *niv = ivPtr(n);
				niv->iType = COMMENT_NODE;
				*p = '\0';
				niv->iValue = strsave(pstr->buf);
				return n;
			} else  {
				while (i--)
					ADDCHR('-');
				ADDCHR(c);
			}
		} else
			ADDCHR(c);
	}
}

private	cmeth	object	parse_doctype(InputStream *is, char *buf, char c)
{
	char	*p, target[128];

	for (p=target ; isname(c) ;) {
		*p++ = c;
		if (EOF == (c=GETC(is)))
			return NULL;
	}
	if (p == buf)
		return NULL;
	*p = '\0';
	UNGETC(c, is);
	for (p=buf ; 1 ;) {
		if (EOF == (c=GETC(is)))
			return NULL;
		if (c == '>') {
			object	n = gNew(self);
			ivType *niv = ivPtr(n);
			niv->iType = DOCUMENT_TYPE_NODE;
			*p = '\0';
			niv->iValue = strsave(*buf==' '?buf+1:buf);
			niv->iName = strsave(target);
			return n;
		} else
			*p++ = c;
	}
}

private	cmeth	object	parse_instruction(InputStream *is, char *buf)
{
	char	c, *p, target[128];
	
	while (EOF == (c=GETC(is))  &&  isspace(c));
	if (c == EOF)
		return NULL;
	for (p=target ; isname(c) ;) {
		*p++ = c;
		if (EOF == (c=GETC(is)))
			return NULL;
	}
	if (p == buf)
		return NULL;
	*p = '\0';
	UNGETC(c, is);
	for (p=buf ; 1 ;) {
		if (EOF == (c=GETC(is)))
			return NULL;
		if (c == '?') {
			int	i = 0;
			while (c == '?') {
				i++;
				if (EOF == (c=GETC(is)))
					return NULL;
			}
			for (; i > 1 ; i--)
				*p++ = '?';
			if (i == 1  &&  c == '>') {
				object	n = gNew(self);
				ivType *niv = ivPtr(n);
				niv->iType = PROCESSING_INSTRUCTION_NODE;
				*p = '\0';
				niv->iValue = strsave(*buf==' '?buf+1:buf);
				niv->iName = strsave(target);
				return n;
			} else  {
				while (i--)
					*p++ = '?';
				*p++ = c;
			}
		} else
			*p++ = c;
	}
}

private	cmeth	object	new_tag(InputStream *is, string *pstr)
{
	object	n;
	ivType	*niv;
	char	c, *p = pstr->buf;

	if (EOF == (c = GETC(is)))
		return NULL;
	if (c == '!') {
		if (EOF == (c = GETC(is)))
			return NULL;
		if (c == '[')
			return parse_cdata(self, is, pstr);
		else if (c == '-')
			return parse_comment(self, is, pstr);
		else if (isalpha(c))
			return parse_doctype(self, is, pstr->buf, c);
		else
			return NULL;
	} else if (c == '?')
		return parse_instruction(self, is, pstr->buf);
	while (EOF != c  &&  isspace(c))
		c = GETC(is);
	if (c == EOF)
		return NULL;
	while (EOF != c  &&  isname(c)) {
		ADDCHR(c);
		c = GETC(is);
	}
	if (c == EOF  ||  p == pstr->buf)
		return NULL;
	*p = '\0';
	n = gNew(self);
	niv = ivPtr(n);
	niv->iType = ELEMENT_NODE;
	niv->iName = strsave(pstr->buf);
	while (isspace(c))
		c = GETC(is);
	if (c ==  EOF)
		return gDispose(n);
	if (isname(c)) {
		if (!(niv->iAttributes = parse_attributes(self, c, is, pstr->buf, n, pstr)))
			return gDispose(n);
		c = GETC(is);
		if (c ==  EOF)
			return gDispose(n);
	}
	niv->iClosed = niv->iSelfClosing = c == '/';
	while (c != '>') {
		c = GETC(is);
		if (c == EOF)
			return gDispose(n);
	}
	return n;
}

private	cmeth	object	parse_attributes(char c, InputStream *is, char *buf, object owner, string* pstr)
{
	object	s = NULL, a, pn = NULL;
	ivType	*aiv, *piv=NULL;
	char	*p = buf, q;
	char	abuf[20];
	char	*ap;
	
	while (1) {
		a = gNew(self);
		aiv = ivPtr(a);
		aiv->iType = ATTRIBUTE_NODE;
		aiv->iParent = owner;
		if (!s)
			s = a;
		if (pn) {
			piv->iNextNode = a;
			aiv->iPrevNode = pn;
		}
		pn = a;
		piv = aiv;
		for (p=buf ; isname(c) ; ) {
			ADDCHR(c);
//			*p++ = c;
			c = GETC(is);
			if (c == EOF  ||  !isname(c)  &&  c != '='  &&  !isspace(c))
				return gDeepDispose(s);
		}
		*p = '\0';
		aiv->iName = strsave(buf);
		while (isspace(c)) {
			c = GETC(is);
			if (c == EOF)
				return gDeepDispose(s);
		}
		if (c != '=') 
			return gDeepDispose(s);	
		if (EOF == (c = GETC(is)))
			return gDeepDispose(s);
		while (isspace(c)) {
			c = GETC(is);
			if (c == EOF)
				return gDeepDispose(s);
		}
		if (c != '\''  &&  c != '"')
			return gDeepDispose(s);
		aiv->iQuote = q = c;
		c = GETC(is);
		for (p = buf ; c != q ; ) {
			if (c == EOF)
				return gDeepDispose(s);
			if (c == '&') {
				int	i;
				
				for (i = 0, ap = abuf, c = GETC(is) ; c != ';' ; i++) {
					if (c == EOF  ||  i > 10)
						return gDeepDispose(s);
					*ap++ = c;
					c = GETC(is);
				}
				*ap = '\0';
				if (!strcmp(abuf, "amp"))
					*p++ = '&';
				else if (!strcmp(abuf, "lt"))
					*p++ = '<';
				else if (!strcmp(abuf, "gt"))
					*p++ = '>';
				else if (!strcmp(abuf, "apos"))
					*p++ = '\'';
				else if (!strcmp(abuf, "quot"))
					*p++ = '"';
				else
					return gDeepDispose(s);
			} else
				ADDCHR(c);
//				*p++ = c;
			c = GETC(is);
		}
		*p = '\0';
		aiv->iValue = strsave(buf);
		if (EOF == (c = GETC(is)))
			return gDeepDispose(s);
		while (isspace(c)) {
			c = GETC(is);
			if (c == EOF)
				return gDeepDispose(s);
		}
		if (!isname(c)) {
			UNGETC(c, is);
			break;
		}
	}
	return s;
}

imeth	gDispose, gDeepDispose()
{
	object	nxt;
	
	while (self) {
		nxt = iNextNode;
		if (iName)
			free(iName);
		if (iValue)
			free(iValue);
		switch (iType) {
		case DOCUMENT_NODE:
		case ELEMENT_NODE:
			if (iAttributes)
				gDeepDispose(iAttributes);
			if (iFirstChild)
				gDeepDispose(iFirstChild);
			break;
		case ATTRIBUTE_NODE:
		case TEXT_NODE:
		case COMMENT_NODE:
		case CDATA_SECTION_NODE:
		case DOCUMENT_TYPE_NODE:
		case PROCESSING_INSTRUCTION_NODE:
			break;
		};
		gDispose(super);
		if (self = nxt)
			iv = ivPtr(self);
	}
	return NULL;
}

cmeth	gNewDocument()
{
	object	n = gNew(super);
	ivType	*iv = ivPtr(n);
	iType = DOCUMENT_NODE;
	iClosed = 1;
	return n;
}

cmeth	gCreateElement(char *name)
{
	object	node = gNew(super);
	ivType	*iv = ivPtr(node);
	iType = ELEMENT_NODE;
	iName = strsave(name);
	iClosed = 1;
	return node;
}	

cmeth	gCreateTextNode(char *text)
{
	object	node = gNew(super);
	ivType	*iv = ivPtr(node);
	iType = TEXT_NODE;
	iValue = strsave(text);
	return node;
}	

cmeth	gCreateComment(char *text)
{
	object	node = gNew(super);
	ivType	*iv = ivPtr(node);
	iType = COMMENT_NODE;
	iValue = strsave(text);
	iClosed = 1;
	return node;
}	

cmeth	gCreateCDATASection(char *text)
{
	object	node = gNew(super);
	ivType	*iv = ivPtr(node);
	iType = CDATA_SECTION_NODE;
	iValue = strsave(text);
	iClosed = 1;
	return node;
}	

cmeth	gCreateAttribute(char *name, char *value)
{
	object	node = gNew(super);
	ivType	*iv = ivPtr(node);
	iType = ATTRIBUTE_NODE;
	iName = strsave(name);
	iValue = strsave(value);
	iQuote = '\'';
	if (value)
		while (*value)
			if (*value++ == '\'') {
				iQuote = '"';
				break;
			}
	return node;
}

imeth	gInsertBefore(node)
{
	ivType	*niv = ivPtr(node);

	niv->iParent = iParent;
	if (!(niv->iPrevNode = iPrevNode)  &&  iParent) {
		ivType	*parent_iv = ivPtr(iParent);
		parent_iv->iFirstChild = node;
	} else if (niv->iPrevNode) {
		ivType	*prev_iv = ivPtr(niv->iPrevNode);
		prev_iv->iNextNode = node;
	}
	niv->iNextNode = self;
	iPrevNode = node;
	return self;
}

imeth	gAppendChild(node)
{
	ivType	*new_iv = ivPtr(node);

	if (iLastChild) {
		ivType	*c_iv = ivPtr(iLastChild);
		new_iv->iPrevNode = iLastChild;
		iLastChild = node;
		new_iv->iNextNode = NULL;
		c_iv->iNextNode = node;
	} else {
		new_iv->iPrevNode = new_iv->iNextNode = NULL;
		iFirstChild = iLastChild = node;
	}
	new_iv->iParent = self;
	return self;
}

imeth	gRemoveNode()
{
	ivType	*tiv;

	if (iPrevNode) {
		tiv = ivPtr(iPrevNode);
		tiv->iNextNode = iNextNode;
	}
	if (iNextNode) {
		tiv = ivPtr(iNextNode);
		tiv->iPrevNode = iPrevNode;
	}
	if (iParent) {
		tiv = ivPtr(iParent);
		if (tiv->iFirstChild == self)
			tiv->iFirstChild = iNextNode;
		if (tiv->iLastChild == self)
			tiv->iLastChild = iPrevNode;
		if (tiv->iAttributes == self)
			tiv->iAttributes = iNextNode;
	}

	//  Now delete any white space nodes
	if (iPrevNode) {
		tiv = ivPtr(iPrevNode);
		if (tiv->iType == TEXT_NODE  &&  tiv->iAllSpace)
			gDispose(gRemoveNode(iPrevNode));
	}
	if (iNextNode) {
		tiv = ivPtr(iNextNode);
		if (tiv->iType == TEXT_NODE  &&  tiv->iAllSpace)
			gDispose(gRemoveNode(iNextNode));
	}
	iParent = iPrevNode = iNextNode = NULL;
	return self;
}

imeth	gDisposeItem()
{
	return gDispose(gRemoveNode(self));
}

imeth	gSetNodeName(char *txt)
{
	if (iName)
		free(iName);
	iName = strsave(txt);
	return self;
}

imeth	gSetNodeValue(char *txt)
{
	if (iValue)
		free(iValue);
	iValue = strsave(txt);
	if (iType == ATTRIBUTE_NODE) {
		iQuote = '\'';
		while (*txt)
			if (*txt++ == '\'') {
				iQuote = '"';
				break;
			}
	}
	return self;
}

static	char	*strsave(char *s)
{
	if (s) {
		char	*n = (char *) malloc(strlen(s)+1);
		strcpy(n, s);
		return n;
	} else
		return NULL;
}

imeth	int	gSaveXML(char *file)
{
	object	fp = gOpenFile(File, file, "wb");
	if (fp) {
		dump_xml(self, 0, fp, 1);
		gDispose(fp);
		return 0;
	}
	return 1;
}

imeth	gPrint(stm)
{
	dump_xml(self, 0, stm, 1);
	return self;
}

imeth	gDumpXML(int level, object stm, int next)
{
	dump_xml(self, level, stm, next);
	return self;
}

cmeth	char	*gXMLToString(char *to, char *from)
{
	char	*p = to;

	while (*from)
		if (*from == '&') {
			if (!strncmp("&apos;", from, 6)) {
				*p++ = '\'';
				from += 6;
			} else 	if (!strncmp("&quot;", from, 6)) {
				*p++ = '"';
				from += 6;
			} else 	if (!strncmp("&amp;", from, 5)) {
				*p++ = '&';
				from += 5;
			} else 	if (!strncmp("&lt;", from, 4)) {
				*p++ = '<';
				from += 4;
			} else 	if (!strncmp("&gt;", from, 4)) {
				*p++ = '>';
				from += 4;
			} else
				*p++ = *from++;
		} else
			*p++ = *from++;
	*p = '\0';
	return to;
}

cmeth	char	*gStringToXML(char *to, char *from)
{
	char	*p = to;
	
	if (!from) {
		to[0]=0;
		return to;
	}
	
	for (; *from ; from++)
		switch (*from) {
		case '\'':
			strcpy(p, "&apos;");
			p += 6;
			break;
		case '"':
			strcpy(p, "&quot;");
			p += 6;
			break;
		case '&':
			strcpy(p, "&amp;");
			p += 5;
			break;
		case '<':
			strcpy(p, "&lt;");
			p += 4;
			break;
		case '>':
			strcpy(p, "&gt;");
			p += 4;
			break;
		case '\t':
		case '\r':
		case '\n':
			*p++ = ' ';
			break;
		default:
			*p++ = *from >= ' '  &&  *from <= 126 ? *from : ' ';
			break;
		}
	*p = '\0';
	return to;
}

static	void	dump_value(object fp, char *p)
{
	for ( ; *p ; p++)
		switch (*p) {
		case '\'':
			vPrintf(fp, "&apos;");
			break;
		case '"':
			vPrintf(fp, "&quot;");
			break;
		case '&':
			vPrintf(fp, "&amp;");
			break;
		case '<':
			vPrintf(fp, "&lt;");
			break;
		case '>':
			vPrintf(fp, "&gt;");
			break;
		default:
			gPutc(fp, *p);
		}
}
	
private	imeth	dump_attribute_value(object self, object fp)
{
	char	*p;

	vPrintf(fp, " %s=%c", iName, iQuote);
	for (p=iValue ; *p ; p++)
		switch (*p) {
		case '\'':
			vPrintf(fp, "&apos;");
			break;
		case '"':
			vPrintf(fp, "&quot;");
			break;
		case '&':
			vPrintf(fp, "&amp;");
			break;
		case '<':
			vPrintf(fp, "&lt;");
			break;
		case '>':
			vPrintf(fp, "&gt;");
			break;
		default:
			gPutc(fp, *p);
		}
	gPutc(fp, iQuote);
	return self;
}
					 
private	imeth	void	dump_xml(int lvl, object fp, int next)
{
#if 0
	int	t;
	char	tabs[256];
	
	for (t=0 ; t < lvl ; )
		tabs[t++] = '\t';
	tabs[t] = '\0';
	tabs[0] = '\0';
#endif
	for (; self ; self=next?iNextNode:NULL) {
		iv = ivPtr(self);
		if (iType == DOCUMENT_NODE) {
			if (iFirstChild)
				dump_xml(iFirstChild, lvl+1, fp, next);
		} else if (iType == ELEMENT_NODE) {
			object	a;
			ivType *aiv;
			
			vPrintf(fp, "<%s", iName);
			for (a=iAttributes ; a ; a=aiv->iNextNode) {
				aiv = ivPtr(a);
				dump_attribute_value(a, fp);
//				vPrintf(fp, " %s=%c%s%c", aiv->iName, aiv->iQuote, aiv->iValue, aiv->iQuote);
			}
			if (iSelfClosing)
				gPutc(fp, '/');
			gPutc(fp, '>');
			if (iFirstChild)
				dump_xml(iFirstChild, lvl+1, fp, next);
			if (iClosed  &&  !iSelfClosing)
				vPrintf(fp, "</%s>", iName);
		} else if (iType == TEXT_NODE)
			dump_value(fp, iValue);
		else if (iType == COMMENT_NODE) {
			gPuts(fp, "<!--");
			gPuts(fp, iValue);
			gPuts(fp, "-->");
		} else if (iType == CDATA_SECTION_NODE) {
			gPuts(fp, "<![CDATA[");
			gPuts(fp, iValue);
			gPuts(fp, "]]>");
		} else if (iType == DOCUMENT_TYPE_NODE)
			vPrintf(fp, "<!%s %s>", iName, iValue);
		else if (iType == PROCESSING_INSTRUCTION_NODE)
			vPrintf(fp, "<?%s %s?>", iName, iValue);
		else if (iType == ATTRIBUTE_NODE)
			vPrintf(fp, "%s='%s'", iName, iValue);
	}
}

#if 0
static	int	isAllSpace(char *s)
{
	for (; *s ; s++)
		if (!isspace(*s))
			return 0;
	return 1;
}
#endif

imeth	char	*gName, gGetName ()
{
	return iName;
}

imeth	char	*gStringValue()
{
	return iValue;
}

imeth	int	gIntValue()
{
	char *val=gStringValue(self);

	if (val)
		return atoi(val);
	else
		return 0;
}

imeth	char gCharValue()
{
	char *val=gStringValue(self);

	if (val)
		return val[0];
	else
		return 0;
}

imeth	double gDoubleValue()
{
	char *val=gStringValue(self);

	if (val)
		return atof(val);
	else
		return 0.0;
}

imeth	int	gType()
{
	return iType;
}

imeth	gGetParent, gParent ()
{
	return iParent;
}

imeth	gChild, gFirst ()
{
	for (self = iFirstChild ; self ; self = iNextNode) {
		iv = ivPtr(self);
		if (!iAllSpace)
			break;
	}
	return self;
}

imeth	gLast()
{
	for (self = iLastChild ; self ; self = iPrevNode) {
		iv = ivPtr(self);
		if (!iAllSpace)
			break;
	}
	return self;
}

imeth	gPrevious()
{
	for (self = iPrevNode ; self ; self = iPrevNode) {
		iv = ivPtr(self);
		if (!iAllSpace)
			break;
	}
	return self;
}

imeth	gNext()
{
	for (self = iNextNode ; self ; self = iNextNode) {
		iv = ivPtr(self);
		if (!iAllSpace)
			break;
	}
	return self;
}

imeth	gFirstAttribute()
{
	return iAttributes;
}

imeth	gGetAttribute(char *name)
{
	object	attr = iAttributes;

	while (attr && strcmp(gName(attr), name))
		attr = gNext(attr);
	return attr;
}

imeth	char	*gGetAttributeValue(char *name)
{
	object	attr = gGetAttribute(self, name);
	return attr ? gStringValue(attr) : NULL;
}

imeth	gSetAttribute(char *name, char *value)
{
	object	attr = gGetAttribute(self, name);
	ivType *aiv;
	
	if (attr)
		gSetNodeValue(attr, value);
	else {
		attr = gCreateAttribute(XMLNode, name, value);
		aiv = ivPtr(attr);
		aiv->iParent = self;
		if (aiv->iNextNode = iAttributes) {
			aiv = ivPtr(iAttributes);
			aiv->iPrevNode = attr;
		}
		iAttributes = attr;
	}
	return self;
}

imeth	gRemoveAttribute(char *name)
{
	object	attr = gGetAttribute(self, name);
	return attr ? gRemoveNode(attr) : attr;
}

imeth	gDisposeAttribute(char *name)
{
	object	attr = gGetAttribute(self, name);
	return attr ? gDispose(gRemoveNode(attr)) : attr;
}

private	imeth	pFindNode(object self, char *name, char *txt)
{
	for (; self ; self = iNextNode) {
		iv = ivPtr(self);
		if (iType == ELEMENT_NODE  &&  (*name == '*'  ||  !strcmp(iName, name)))
			if (txt) {
				object	child;
				ivType	*civ;
				
				for (child=iFirstChild ; child ; child=civ->iNextNode) {
					civ = ivPtr(child);
					if (civ->iType == TEXT_NODE  &&  !civ->iAllSpace  &&  !strcmp(civ->iValue, txt))
						return self;  // found
				}
			} else
				break;  // found
	}
	return self; 
}

imeth	gFindNode(char *name)
{
	return pFindNode(self, name, NULL);
}

imeth	gFindNextNode(char *name)
{
	if (iNextNode)
		return pFindNode(iNextNode, name, NULL);
	return NULL;
}

imeth	gFindChildNode(char *name)
{
	if (iFirstChild)	
		return pFindNode(iFirstChild, name, NULL);
	return NULL;
}

imeth	gFindChildNodeWithText(char *name, char *txt)
{
	if (iFirstChild)	
		return pFindNode(iFirstChild, name, txt);
	return NULL;
}

static	char	*elim_space(char *v)
{
	char	*s = v, *e = v, q='\0';
	
	for ( ; *e ; e++)
		if (q) {
			if (*e == q)
				q = '\0';
			*s++ = *e;
		} else if (*e == '\''  ||  *e == '"')
			*s++ = q = *e;
		else if (!isspace(*e))
			*s++ = *e;
	*s = '\0';
	return v;
}

#define	SUB_NONE		0
#define	SUB_INDEX		1
#define	SUB_LAST		2
#define	SUB_ATTR		3
#define	SUB_ATTRVAL		4
#define	SUB_CHILD		5
#define	SUB_CHILDTEXT		6

#define	OPR_NONE		0
#define	OPR_EQ			1
#define	OPR_NE			2

#define	DIR_DOWN		0
#define	DIR_UP			1
#define	DIR_SAME		2
#define	DIR_TEXT		3
#define	DIR_ATTR		4

private	imeth	int	pValidNode(char *name, int sub_type, char *sub_val, int index, int sub_index, int sub_op, char *sub_attr)
{
	if (iType != ELEMENT_NODE || (!name  ||  *name != '*'  &&  strcmp(iName, name)))
		return 0;
	switch (sub_type) {
	case SUB_NONE:
		return 1;
	case SUB_INDEX:
		return index == sub_index;
	case SUB_LAST:
		return 1;  /*  could be last  */
	case SUB_ATTR:
		return !!gGetAttribute(self, sub_attr);
	case SUB_ATTRVAL:  {
		char	*av = gGetAttributeValue(self, sub_attr);
		if (sub_op == OPR_EQ)
			return av  &&  !strcmp(av, sub_val);
		else if (sub_op == OPR_NE)
			return av  &&  strcmp(av, sub_val);
	    }
	case SUB_CHILD:
		return !!gFindChildNode(self, sub_attr);
	case SUB_CHILDTEXT:
		return !!gFindChildNodeWithText(self, sub_attr, sub_val);
	}
	return 1;
}

static	int	parse_sub(char *name, int *sub_index, char *sub_attr, int *sub_op, char *sub_val)
{
	char	*p;
	int	sub_type = SUB_NONE;
	char	sub[128]; 
	
	for (p=name ; *p ; p++)
		if (*p == '[') {
			char	*ps;
			*p++ = '\0';
			for (ps=sub ; *p  &&  *p != ']' ; )
				*ps++ = *p++;
			*ps = '\0';
			elim_space(sub);
			if (isdigit(*sub)) {
				*sub_index = atoi(sub);
				sub_type = SUB_INDEX;
			} else if (!strcmp(sub, "last()"))
				sub_type = SUB_LAST;
			else if (*sub == '@') {
				for (ps=sub+1, p=sub_attr ; *ps  &&  *ps != '='  &&  *ps != '!' ; )
					*p++ = *ps++;
				*p = '\0';
				if (*ps == '=') {
					char	q = 0;
					ps++;
					if (*ps == '\''  ||  *ps == '"')
						q = *ps++;
					for (p=sub_val ; *ps  &&  *ps != q ; )
						*p++ = *ps++;
					*p = '\0';
					*sub_op = OPR_EQ;
					sub_type = SUB_ATTRVAL;
				} else if (ps[0] == '!'  &&  ps[1] == '=') {
					char	q = 0;
					ps += 2;
					if (*ps == '\''  ||  *ps == '"')
						q = *ps++;
					for (p=sub_val ; *ps  &&  *ps != q ; )
						*p++ = *ps++;
					*p = '\0';
					*sub_op = OPR_NE;
					sub_type = SUB_ATTRVAL;
				} else {
					*sub_op = OPR_NONE;
					sub_type = SUB_ATTR;
				}
			} else if (isname(*sub)) {
				for (ps=sub, p=sub_attr ; *ps  &&  *ps != '='  &&  *ps != '!' ; )
					*p++ = *ps++;
				*p = '\0';
				if (*ps == '=') {
					char	q = 0;
					ps++;
					if (*ps == '\''  ||  *ps == '"')
						q = *ps++;
					for (p=sub_val ; *ps  &&  *ps != q ; )
						*p++ = *ps++;
					*p = '\0';
					*sub_op = OPR_EQ;
					sub_type = SUB_CHILDTEXT;
				} else if (ps[0] == '!'  &&  ps[1] == '=') {
					char	q = 0;
					ps += 2;
					if (*ps == '\''  ||  *ps == '"')
						q = *ps++;
					for (p=sub_val ; *ps  &&  *ps != q ; )
						*p++ = *ps++;
					*p = '\0';
					*sub_op = OPR_NE;
					sub_type = SUB_CHILDTEXT;
				} else {
					*sub_op = OPR_NONE;
					sub_type = SUB_CHILD;
				}
			}
			break;
		}
	return sub_type;
}

private	imeth	void	xpath_recurse(object *res2, char *name, int sub_type, char *sub_val, int sub_index, int sub_op, char *sub_attr)
{
	int	index;
	
	while (self  &&  iType != ELEMENT_NODE)
		if (self = iNextNode)
			iv = ivsPtr;
	for (index=1 ; self ; ) {
		if (pValidNode(self, name, sub_type, sub_val, index, sub_index, sub_op, sub_attr)) {
			index++;
			if (!*res2)
				*res2 = gNew(LinkObject);
			gAddLast(*res2, self);
		}
		if (iFirstChild)
			xpath_recurse(iFirstChild, res2, name, sub_type, sub_val, sub_index, sub_op, sub_attr);
		if (self = iNextNode) {
			iv = ivsPtr;
			while (self  &&  iType != ELEMENT_NODE)
				if (self = iNextNode)
					iv = ivsPtr;
		}
	}
}


/*
	The following XPath syntax has been implemented:
	
	/ABC/DEF
	ABC/DEF
	/ABC/*
	/ABC[4]
	/ABC/*[3]
	/ABC[last()]
	/ABC[@attr]
	/ABC[@attr='val']
	/ABC[@attr!='val']
	../ABC
	./ABC
	//ABC
	/ABD//DEF
	/ABC | /DEF
	/ABC/.[...]
	/ABC/DEF[...][...]
	/ABC[DEF]
	/ABC[DEF='xxx']
	/ABC/text()  (no //text() yet)
	/ABC/@attr   (no //@attr yet)
*/

private	imeth	pXPath(char *path)
{
	char	name[128], *p, sub_attr[128], sub_val[128];
	object	res=NULL, seq, obj, res2;
	int	sub_type, sub_op=0, sub_index=0, index, dir, recurse;

	if (*path == '/')
		while (iParent) {
			self = iParent;
			iv = ivsPtr;
		}
	while (*path)  {
		recurse = 0;
		if (*path == '/') {
			path++;
			if (*path == '/') {
				recurse = 1;
				path++;
			}
		}
		for (p=name ; *path  &&  *path != '/' ;)
			*p++ = *path++;
		if (p == name)
			return res;
		*p = '\0';
		if (!strcmp(name, "."))
			continue;
		if (*name == '.')
			if (name[1] == '.')
				dir = DIR_UP;
			else
				dir = DIR_SAME;
		else if (!strcmp(name, "text()"))
			dir = DIR_TEXT;
		else if (*name == '@')
			dir = DIR_ATTR;
		else
			dir = DIR_DOWN;
		sub_type = parse_sub(name, &sub_index, sub_attr, &sub_op, sub_val);
		res2 = NULL;
		for (seq=res?gSequence(res):NULL ; obj=seq?gNext(seq):self ; )  {
			object	last;
			
			last = NULL;
			switch (dir) {
			case DIR_DOWN:
				self = gFindChildNode(obj, "*");
				break;
			case DIR_UP:
				self = gParent(obj);
				break;
			case DIR_SAME:
			case DIR_TEXT:
				self = obj;
				strcpy(name, "*");
				break;
			case DIR_ATTR:
				self = obj;
				break;
			}
			if (self)
				iv = ivsPtr;
			switch (dir) {
			case DIR_DOWN:
				for (index=1 ; self ; ) {
					if (pValidNode(self, name, sub_type, sub_val, index, sub_index, sub_op, sub_attr)) {
						if (!res2)
							res2 = gNew(LinkObject);
						if (sub_type == SUB_LAST)
							last = self;
						else {
							index++;
							gAddLast(res2, self);
						}
					}
					if (recurse  &&  iFirstChild)
						xpath_recurse(iFirstChild, &res2, name, sub_type, sub_val, sub_index, sub_op, sub_attr);
					if (self = iNextNode) {
						iv = ivsPtr;
						while (self  &&  iType != ELEMENT_NODE)
							if (self = iNextNode)
								iv = ivsPtr;
					}
				}
				if (sub_type == SUB_LAST  &&  last)
					gAddLast(res2, last);
				break;
			case DIR_SAME:
				for (index=1 ; self ; ) {
					if (pValidNode(self, name, sub_type, sub_val, index, sub_index, sub_op, sub_attr)) {
						if (!res2)
							res2 = gNew(LinkObject);
						if (sub_type == SUB_LAST)
							last = self;
						else {
							index++;
							gAddLast(res2, self);
						}
					}
					if (recurse  &&  iFirstChild)
						xpath_recurse(iFirstChild, &res2, name, sub_type, sub_val, sub_index, sub_op, sub_attr);
					self = NULL;
				}
				if (sub_type == SUB_LAST  &&  last)
					gAddLast(res2, last);
				break;
			case DIR_TEXT:  {
				object	tn;
				ivType	*tiv;

				for (tn=iFirstChild ; tn  ; tn = tiv->iNextNode) {
					tiv = ivPtr(tn);
					if (tiv->iType == TEXT_NODE  &&  !tiv->iAllSpace) {
						if (!res2)
							res2 = gNew(LinkObject);
						gAddLast(res2, tn);
						break;
					}
				}
			}
				break;
			case DIR_ATTR:  {
				object	attr = gGetAttribute(self, name+1);

				if (attr) {
					if (!res2)
						res2 = gNew(LinkObject);
					gAddLast(res2, attr);
				}
			}
				break;
			case DIR_UP: {
				object	seq, obj;

				if (res2) {
					//  Don't put the same parent on the result list twice
					for (seq=gSequence(res2) ; obj=gNext(seq) ; )
						if (obj == self) {
							gDispose(seq);
							break;
						}
					if (!obj)
						gAddLast(res2, self);
				} else {
					res2 = gNew(LinkObject);
					gAddLast(res2, self);
				}
			}
				break;
			}
			self = NULL;
			iv = NULL;
		}
		if (res)
			gDispose(res);
		res = res2;
	}
	return res;
}

imeth	gXPath(char *path)
{
	char	buf[1024], *bp, *ep;
	object	res=NULL, r;

	for (bp=buf ; *path ; )
		if (*path == ']'  &&  path[1] == '[') {
			*bp++ = *path++;
			*bp++ = '/';
			*bp++ = '.';
		} else
			*bp++ = *path++;
	*bp = '\0';
	for (bp=buf ; *bp ; bp = ep) {
		ep = strchr(bp, '|');
		if (!ep)
			ep = bp + strlen(bp);
		else
			*ep++ = '\0';
		r = pXPath(self, bp);
		if (r)
			if (!res)
				res = r;
			else {
				object	seq, node, node2;

				while (node = gPop(r)) {
					for (seq=gSequence(res) ; node2 = gNext(seq) ; )
						if (node == node2) {
							gDispose(seq);
							break;
						}
					if (!node2)
						gAddLast(res, node);
				}
				gDispose(r);
			}
	}
	return res;
	
}

imeth gPopulateStringFromNode(char *to, char *node)
{
	object n = gFindChildNode(self, node);
	char *val = NULL;
	
	if (n)
		n = gChild(n);
	
	if (n)
		val = gStringValue(n);

	if (val)
		strcpy (to,val);
	else
		*to = 0;
		
	return self;
}

imeth char gGetCharFromNode(char *node)
{
	object n = gFindChildNode(self, node);
	
	if (n)
		n = gChild(n);
		
	return gCharValue(n);
}

imeth int gGetIntFromNode(char *node)
{
	object n = gFindChildNode(self, node);
	
	if (n)
		n = gChild(n);
	return gIntValue(n);
}

imeth float gGetFloatFromNode(char *node)
{
	object n = gFindChildNode(self, node);
	
	if (n)
		n = gChild(n);
		
	return gDoubleValue(n);
}  


#ifdef TEST

main(int argc, char *argv[])
{
	object	dom, res;
	long	line, col, pos;
	
	InitDynace(&argc);
#if 1
	if (argc != 3) {
		fprintf(stderr, "Usage:  %s  <file>  <xpath>\n", argv[0]);
		return 1;
	}
	dom = gParseFile(XMLNode, argv[1], &line, &col, &pos);
	if (!dom) {
		fprintf(stderr, "Error parsing %s on line %ld, column %ld\n", argv[1], line, col);
		return 2;
	}
	res = gXPath(dom, argv[2]);
	if (!res)
		printf("No nodes selected\n");
	else {
		int	i;
		object	seq, obj;
		
		for (seq = gSequence(res), i=1 ; obj = gNext(seq) ; i++) {
			vPrintf(stdoutStream, "[%d] ", i);
			dump_xml(obj, 0, stdoutStream, 0);
			gPutc(stdoutStream, '\n');
		}
	}
#else
	dom = gNewDocument(XMLNode);
	gAppendChild(dom, res=gCreateElement(XMLNode, "FatCat"));
	gSetAttribute(res, "COLOR", "Red");
	gAppendChild(res, gCreateTextNode(XMLNode, "My Value"));
	gPrint(dom, stdoutStream);
#endif
	return 0;
}

#endif


/*
 *
 *	Copyright (c) 1993-1996 Blake McBride (blake@mcbride.name)
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */




