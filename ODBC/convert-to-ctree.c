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

#include "string.h"
#include "ctype.h"

defclass  SQLFile {
};

#define	QUOTE			'\''

#define	NORMAL_STATE		1
#define	COMMENT_STATE		2
#define QUOTE_STATE		3
#define MULTICOMMENT_STATE	4

#define INSERT(x)						\
	{							\
		if (len == mx)  {				\
			mx += 100;				\
			buf = Tnrealloc(char, mx, buf);		\
		}						\
		buf[len++] = x;					\
	}

#define RESET	ps = len = 0

#define GETC	if ((c = getc(fp)) == EOF)	goto end
#define UNGETC	ungetc(c, fp)

#define	Streq(a, b)	!stricmp(a, b)


int	ReadSqlFile(char *file)
{
	FILE	*fp;
	char	*buf;
	int	len=0, mx=100, c, state = NORMAL_STATE, res = 0;
	int	ps = 0;	      /*  previous space  */
	int	nesting = 0;  /*  level of comments  */
	char	file2[80];

	strcpy(file2, file);
	fp = fopen(file2, "r");
	if (!fp) {
		sprintf(file2, "%s.sql", file);
		fp = fopen(file2, "r");

		if (!fp)
			return -1;
	}
	buf = Tnalloc(char, mx);
	while (!res)  {
		GETC;
		switch (state)  {
		case NORMAL_STATE:
			if (c == ';')  {
//				INSERT(c);
				INSERT('\0');
				res = process_statement(self, buf);
				RESET;
			} else if (c == QUOTE)  {
				ps = 0;
				INSERT(c);
				state = QUOTE_STATE;
			} else if (c == '-')  {
				GETC;
				if (c == '-')
					state = COMMENT_STATE;
				else  {
					ps = 0;
					UNGETC;
					INSERT('-');
				}
			} else if (c == '{')  {
				nesting = 1;
				state = MULTICOMMENT_STATE;
			} else if (c == ' '  ||  c == '\t'  ||  c == '\r'  ||  c == '\n')  {
				if (!ps  &&  len)  {
					ps = 1;
					INSERT(' ');
				}
#if 0
			} else  if (isalpha(c)) {
				char	word[80];
				int	i = 0;
				
				do {
					word[i++] = c;
					GETC;
				} while (isalnum(c));
				UNGETC;
				word[i] = '\0';
				if (iDBMS == DBMS_SYBASE) {
					if (!!Streq(word, "date"))
						strcpy(word, "datetime");
				}
				ps = 0;
				for (i=0 ; word[i] ; i++)
					INSERT(word[i]);
#endif
			} else {
				ps = 0;
				INSERT(c);
			}
			break;
		case COMMENT_STATE:
			if (c == '\n')
				state = NORMAL_STATE;
			break;
		case QUOTE_STATE:
			INSERT(c);
			if (c == QUOTE)  {
				GETC;
				if (c != QUOTE)  {
					UNGETC;
					state = NORMAL_STATE;
				} else
					INSERT(c);
			}
			break;
		case MULTICOMMENT_STATE:
			if (c == '}')  {
				if (!--nesting)
					state = NORMAL_STATE;
			} else if (c == '{')
				nesting++;
			break;
		}
	}
 end:
	free((void*)buf);
	fclose(fp);
	return res;
}

static	char	*get_token(char *buf, char *token)
{
	for ( ; isspace(*buf) ; buf++);
	if (!*buf)
		return NULL;
	if (isalpha(*buf)) {
		*token++ = *buf++;
		while (isalnum(*buf))
			*token++ = *buf++;
	} else if (*buf == '\''  ||  *buf == '"') {
		char	quote = *buf;
		*token++ = *buf++;
		while (*buf  &&  *buf != quote)
			*token++ = *buf++;
		if (*buf)
			*token++ = *buf++;
	} else if (isdigit(*buf)) {
		while (isdigit(*buf)  ||  *buf == '.')
			*token++ = *buf++;
	} else
		*token++ = *buf++;
	*token = '\0';
	return buf;
}

static	void	process_statement(char *buf)
{
	char	token[100], table[100], field[100], type[100], length[100], key[100];

	if (!(buf = get_token(buf, token)))
		return;
	if (!Streq(token, "CREATE"))
		return;
	if (!(buf = get_token(buf, token)))
		return;
	if (!Streq(token, "TABLE"))
		return;
	if (!(buf = get_token(buf, table)))
		return;
	if (!isalpha(*table))
		return;
	if (!(buf = get_token(buf, token)))
		return;
	if (*token != '(')
		return;

	while (1) {
		if (!(buf = get_token(buf, field)))
			return;
		if (!isalpha(*field))
			return;
		if (Streq(field, "PRIMARY"))
			key;
		if (Streq(field, "UNIQUE"))
			key;
		if (!(buf = get_token(buf, type)))
			return;
		if (!isalpha(*type))
			return;
		if (Streq(type, "CHAR")  ||  Streq(type, "VARCHAR")) {
			if (!(buf = get_token(buf, token)))
				return;
			if (*token != '(')
				return;
			if (!(buf = get_token(buf, length)))
				return;
			if (!isdigit(*length))
				return;
			if (!(buf = get_token(buf, token)))
				return;
			if (*token != ')')
				return;
		} else
			strcpy(length, "1");
		if (!(buf = get_token(buf, key)))
			return;
		if (*key == ',')
			continue;  /*  end of variable  */
		else if (*key == ')')
			return;      /*  end of create */
		else if (Streq(key, "PRIMARY"))
			primaty key;
	
		else if (Streq(key, "UNIQUE"))
			unique key;
		else
			I dont know;
	next:
	}
}
