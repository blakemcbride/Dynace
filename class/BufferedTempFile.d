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





defclass BufferedTempFile
{
	char	*iBuffer;
	/* the actual iBuffer is 1 byte longer than the stated iBuffer. */
	/* this last byte contains a 0x00 character. */
	int	iBufSize;  
	int	iCurrentOffset;
	object	iTmpFile;
	int	iSize;
};

#include <string.h>

#ifndef	max
#define	 max(a,b)    ((a) > (b)	? (a) :	(b))
#endif
#ifndef	min
#define	 min(a,b)    ((a) < (b)	? (a) :	(b))
#endif

private imeth BufferedTempFile_initVars()
{
	iBufSize = 1024;
	iBuffer = (char*)calloc(1, iBufSize + 1);
	return self;
}

cmeth	gOpenTempFile()
{
	return BufferedTempFile_initVars(gNew(super));
}

imeth	gGCDispose()
{
	free(iBuffer);
	return gDispose(super);
}

imeth	gDispose, gDeepDispose()
{
	if (iTmpFile)
		gDispose(iTmpFile);
	return gGCDispose(self);
}

imeth	int	gRead(char *buf, unsigned n)
{
	int bytesRead = min(n, iBufSize - iCurrentOffset);

	if (iTmpFile)
		return gRead(iTmpFile, buf, n);
	else
	{
		memcpy(buf, iBuffer + iCurrentOffset, bytesRead);
		iCurrentOffset += bytesRead;
		return bytesRead;
	}
}

private imeth void createTempFile()
{
	iTmpFile = gOpenTempFile(File);
	gWrite(self, iBuffer, iSize);
	gFlush(self);
}

imeth	int	gWrite(char *buf, unsigned n)
{
	if (iTmpFile)
		return gWrite(iTmpFile, buf, n);
	else
	{
		if (n + iCurrentOffset > iBufSize)
		{
			createTempFile(self);
			return gWrite(self, buf, n);
		}	
		else
		{
			memcpy(iBuffer + iCurrentOffset, buf, n);
			iCurrentOffset += n;
			iSize = max(iCurrentOffset, iSize);
			return n;
		}
	}
}

imeth	char	*gGets(char *buf, int sz)
{
	char *tmpBuffer;

	if (iTmpFile)
		return gGets(iTmpFile, buf, sz);
	else
	{
		/* This is one reason for allocating iBuffer to be */
		/* one byte larger than stated. Since the last char */
		/* is NULL we known that strstr will not bypass the end of the iBuffer */
		tmpBuffer = strstr(iBuffer + iCurrentOffset, "\n");
		/* we never found a newline */
		if (tmpBuffer == NULL)
		{
			gRead(self, buf, sz - 1);
			buf[sz - 1] = 0x00;
		}
		else if (tmpBuffer - iBuffer + iCurrentOffset <= sz - 1)
		{
			gRead(self, buf, tmpBuffer - iBuffer + iCurrentOffset + 1);
			buf[tmpBuffer - iBuffer + iCurrentOffset] = 0x00;
		}
		else
		{
			gRead(self, buf, sz - 1);
			buf[sz - 1] = 0x00;
		}

		return buf;
	}
}

imeth	long	gAdvance(long n)
{
	if (iTmpFile)
		return gAdvance(iTmpFile, n);
	else
		if (n > iSize - iCurrentOffset)
			return 0;
		else
		{
			iCurrentOffset += n;
			return n;
		}
}

imeth	long	gRetreat(long n)
{
	int bytesRetreated = min(iCurrentOffset, n);

	if (iTmpFile)
		return gRetreat(iTmpFile, n);
	else
		if (n > iCurrentOffset)
			return 0;
		else
		{
			iCurrentOffset -= n;
			return n;
		}
}

imeth	long	gSeek(long n)
{
	if (iTmpFile)
		return gSeek(iTmpFile, n);
	else
	{
		iCurrentOffset = min(n, iBufSize);
		return min(n, iBufSize);
	}
}

imeth	long	gPosition()
{
	return iTmpFile ?  gPosition(iTmpFile) :  iCurrentOffset;
}

imeth	long	gLength()
{
	return iTmpFile ? gLength(iTmpFile) : iSize;
}

imeth	char	*gName()
{
	if (iTmpFile)
		return gName(iTmpFile);
	else
	{
		/*	Since they have asked for a name, we must assume they are expecting
			it to be an actual file. So, we will create one and give it to them. */

		createTempFile(self);
		return gName(self);
	}
}

imeth	int	gEndOfStream()
{
	return iTmpFile ? gEndOfStream(iTmpFile) :  iCurrentOffset != iSize;
}

imeth	void	*gPointerValue()
{
	if (iTmpFile)
		return gPointerValue(iTmpFile);
	else
	{
		/*	Since they have asked for the file pointer, we must assume they are expecting
			it to be an actual file. So, we will create one and give it to them. */

		createTempFile(self);
		return gPointerValue(self);
	}
}

imeth	int	gFlush()
{
	return iTmpFile ? gFlush(iTmpFile) : iSize;
}
