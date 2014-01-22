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


#include <stdio.h>
#include <windows.h>
#include <errno.h>
#include <ctype.h>

static	char	*format(char *rbuf, int wth, __int64 n);


main(int argc, char *argv[])
{
	ULARGE_INTEGER	cfb, tnb, tnfb;
	char	b1[30], b2[30], drive[30];
	
	if (argc != 2)  {
		fprintf(stderr, "Usage:  df  drive\n");
		exit(-1);
	}
	if (!argv[1][1])
		sprintf(drive, "%c:", argv[1][0]);
	else
		strcpy(drive, argv[1]);
	if (!GetDiskFreeSpaceEx(drive, &cfb, &tnb, &tnfb)) {
		fprintf(stderr, "Error getting free disk space for %s\n", drive);
		return -1;
	}
	printf("Drive:        %s\n", drive);
	printf("Total bytes:  %s\n", format(b1, 17, tnb.QuadPart));
	printf("Free bytes:   %s\n", format(b2, 17, tnfb.QuadPart));
	return 0;
}

static	char	*format(char *rbuf, int wth, __int64 n)
{
	char	buf[30], *f, *t;
	int	i=0, w=0;

	sprintf(buf, "%29I64d", n);
	rbuf[sizeof(buf)-1] = '\0';
	for (f=buf+sizeof(buf)-2, t=rbuf+sizeof(buf)-2 ; isdigit(*f) ; i++, w++) {
		if (i == 3) {
			*t-- = ',';
			i = 0;
		}
		*t-- = *f--;
	}
	if (wth  &&  w < wth)
		for (; w < wth ; w++)
			*t-- = ' ';
	return t + 1;
}
