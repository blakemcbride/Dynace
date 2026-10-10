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
#include <stdlib.h>
#include <string.h>

defclass  ArgumentList : LinkObject;

static	void	process_file(object, char *);

cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewArglist(int argc, char **argv)
{
	int	i;
	object	obj = gNew(super);
	for (i=0 ; i < argc ; i++)
		if (argv[i][0] == '@')
			process_file(obj, 1+argv[i]);
		else
			gAddLast(obj, gNewWithStr(String, argv[i]));
	return obj;
}

static	void	process_word(char *word, object list, int noexpand)
{
	int	wild=0;
	char	*w;

	if (!noexpand)
		for (w=word ; *w ; w++)
			if (*w == '*'  ||  *w == '?')  {
				wild = 1;
				break;
			}
	if (!wild)  {
		gAddLast(list, gNewWithStr(String, word));
		return;
	}
	
	/*  process wild some day...   */

	gAddLast(list, gNewWithStr(String, word));
}


#define space(x) 	((x) <= ' ')

static	char	*process_token(char *p, object list)
{
	char	word[4096], *w;
	int	inquote, noexpand;

	while (*p  &&  space(*p))
		p++;
	if (noexpand = inquote = *p == '"')
		p++;
	for (w=word ; *p  &&  (!space(*p) || inquote) ; p++)
		if (inquote  &&  *p == '"')
			inquote = 0;
		else if (w < word + sizeof(word) - 1)
			*w++ = *p;
		else  {
			vPrintf(stdoutStream, "dpp: argument too long: %.60s...\n", word);
			exit(1);
		}
	*w = '\0';
	if (*word == '@')
		process_file(list, word+1);
	else if (*word)
		process_word(word, list, noexpand);
	return p;
}

static	void	process_file(object list, char *file)
{
	char	buf[256], *line = NULL, *p;
	size_t	len = 0, n;
	object	fobj;

	fobj = gOpenFile(File, file, "r");
	if (!fobj)  {
		vPrintf(stdoutStream, "Can't open %s\n", file);
		exit(1);
	}
	/*  gather each whole line, so a long one isn't split into two arguments  */
	while (gGets(fobj, buf, sizeof buf))  {
		n = strlen(buf);
		if (!(p = realloc(line, len + n + 1)))  {
			vPrintf(stdoutStream, "dpp: out of memory reading %s\n", file);
			exit(1);
		}
		line = p;
		memcpy(line + len, buf, n + 1);
		len += n;
		if (n  &&  buf[n-1] == '\n')  {
			for (p=line ; *(p=process_token(p, list)) ; );
			len = 0;
		}
	}
	if (len)	/*  last line had no newline  */
		for (p=line ; *(p=process_token(p, list)) ; );
	free(line);
	gDispose(fobj);
}






