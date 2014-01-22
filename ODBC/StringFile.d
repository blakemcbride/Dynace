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



defclass  StringFile  {
	iFileName;	//  file strings are stored in
	long	iFileSize;
	iString;	//  dictionary by string
	iIndex;		//  dictionary by index
	
	iSocket;
};


#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <errno.h>
#include <ctype.h>

#include <winsock.h>


#define	MAX_STRING	128
#define	LOCK_BYTES	1000000L

static	long	file_size(FILE *fp);
static	char	*make_lower(char *buf, char *str);

private	imeth	void	append_file(FILE *fp, char *s);
private	imeth	void	read_file(object fp, long filelen);

private	imeth	void	sendServerCmd(char *cmd);
private	imeth	int		readIntFromServer();

typedef	unsigned long  int uint32_t;

cmeth	gNewWithStr(char *file)
{
	object	obj = gNew(super), fileobj;
	FILE	*fp;
	accessIVsOf(obj);

	iFileName = gNewWithStr(String, file);
	iString   = gNewWithInt(Dictionary, 3001);
	iIndex    = gNewWithInt(Dictionary, 3001);
	fp = fopen(file, "rb");
	if (fp) {
		gTPLock(TransactionProcessing, fp);
		iFileSize = file_size(fp);
		rewind(fp);
		fileobj = gNewWithStream(File, file, fp);
		read_file(obj, fileobj, iFileSize);
		gDispose(fileobj);
		gTPUnlock(TransactionProcessing, fp);
		fclose(fp);
	}
	
	return obj;
}

cmeth	gNewWithServer(object socket)
{
	object	obj = gNew(super);
	int filelen;
	accessIVsOf(obj);

	
	//connect to server and get file
	iSocket = socket;
	iString   = gNewWithInt(Dictionary, 3001);
	iIndex    = gNewWithInt(Dictionary, 3001);
	
	//send cmd to get string file
	sendServerCmd(obj, "getstrings");
	//get length
	if (!gReadInt32(iSocket, &filelen))
		gError(Object, "StringFile Read Error in DataSync server");
	iFileSize = filelen;
	read_file(obj, iSocket, iFileSize);
	
	return obj;
}

imeth	gDispose, gDeepDispose()
{
	if (iFileName)
		iFileName = gDispose(iFileName);
	
	gDispose(iString);
	gDeepDispose(iIndex);
	return gDispose(super);
}

imeth	long	gGetStringIndex(char *str)
{
	char	buf[MAX_STRING];
	object	fileobj;
	object	s = gNewWithStr(String, str=make_lower(buf, str));
	object	r = gFindValue(iString, s);

	if (!r) {
		if (!iSocket) {
			FILE	*fp = fopen(gStringValue(iFileName), "a+b");
			gTPLock(TransactionProcessing, fp);
			if (file_size(fp) != iFileSize) {
				gDispose(iString);
				gDeepDispose(iIndex);
				iString = gNewWithInt(Dictionary, 3001);
				iIndex  = gNewWithInt(Dictionary, 3001);
				iFileSize = file_size(fp);
				rewind(fp);
				fileobj = gNewWithStream(File, gStringValue(iFileName), fp);
				read_file(self, fileobj, iFileSize);
				gDispose(fileobj);
				gDispose(s);
				gTPUnlock(TransactionProcessing, fp);
				fclose(fp);
				return gGetStringIndex(self, str);
			}
			r = gNewWithLong(LongInteger, (long)(gSize(iString)+1));
			gAddValue(iString, s, r);
			gAddValue(iIndex, r, s);
			append_file(self, fp, str);
			gTPUnlock(TransactionProcessing, fp);
			fclose(fp);
		} else {
			int filelen;
			
			sendServerCmd(self, "addstring");
			if (!gWriteInt32(iSocket, strlen(str)) == sizeof(int))
				gError(Object, "StringFile Write Error in DataSync server");
			if (!gWrite(iSocket, str, strlen(str)) == strlen(str))
				gError(Object, "StringFile Write Error in DataSync server");
			if (!gReadInt32(iSocket, &filelen))
				gError(Object, "StringFile Read Error in DataSync server");
			iFileSize = filelen;
			gDispose(iString);
			gDeepDispose(iIndex);
			iString = gNewWithInt(Dictionary, 3001);
			iIndex  = gNewWithInt(Dictionary, 3001);
			read_file(self, iSocket, iFileSize);
			return gGetStringIndex(self, str);
		}
	} else
		gDispose(s);
	return gLongValue(r);
}

imeth	char	*gStringFromIndex(long idx)
{
	object	r = gNewWithLong(LongInteger, idx);
	object	s = gFindValue(iIndex, r);
	gDispose(r);
	return s ? gStringValue(s) : NULL;
}

private	imeth	void	sendServerCmd(char *cmd)
{
	int cmdlen = strlen(cmd);

	if (!gWriteInt32(iSocket, cmdlen) == sizeof(int))
		gError(Object, "StringFile Write Error in DataSync server");
	if (!gWrite(iSocket, cmd, cmdlen) == cmdlen)
		gError(Object, "StringFile Write Error in DataSync server");
}

private	imeth	void	append_file(FILE *fp, char *p)
{
	for ( ; *p ; p++)
		putc(*p, fp);
	putc('\n', fp);
	fflush(fp);
	_commit(_fileno(fp));
	iFileSize = file_size(fp);
}

static	long	file_size(FILE *fp)
{
#if 0
	struct _stat sb;
	_fstat(_fileno(fp), &sb);
	return sb.sb_size;
#else
	return _filelength(_fileno(fp));
#endif
}

private	imeth	void	read_file(object fp, long filelen)
{
	int	c, len;
	long ndx, readcnt = 0;
	char	buf[MAX_STRING];
	long	n = 0L;
	object	str, num;

	while (readcnt < filelen) 
	{
		readcnt += gRead(fp, (char *)&c, 1);
		len = 0;
		buf[len++] = (char) c;
		while (len < (sizeof(buf)-1))
		{
			readcnt += gRead(fp, (char *)&c, 1);
			if ((char) c == '\n')
				break;
			buf[len++] = (char) c;
		}
		buf[len] = '\0';
		str = gNewWithStr(String, buf);
		num = gNewWithLong(LongInteger, ++n);
		gAddValue(iString, str, num);
		gAddValue(iIndex, num, str);
	}
}

static	char	*make_lower(char *buf, char *str)
{
	char	*p = buf;

	for ( ; *str ; str++)
		*p++ = tolower(*str);
	*p = '\0';
	return buf;
}



