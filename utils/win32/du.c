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
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

static	__int64 du(char *path, int recurse, int all, int verbose);
static	char	*format(__int64 n);
static	void	usage();


main(int argc, char *argv[])
{
	int	i, n = 0, recurse=0, all=0, verbose=0;
	__int64	sz = 0L;

	while (argc > 1  &&  argv[1][0] == '-')  {
		for (i=1 ; argv[1][i] ; ++i)
			switch (argv[1][i])  {
			case 'a':
			case 'A':
				all = 1;
				break;
			case 'h':
			case 'H':
				usage();
				break;
			case 'r':
			case 'R':
				recurse = 1;
				break;
			case 'v':
			case 'V':
				verbose = 1;
				break;
			default:
				fprintf(stderr, "du:  Unknown flag %c\n", argv[1][i]);
				break;
			}
		argv++;
		argc--;
	}

	if (argc == 1)
		sz = du(".", recurse, all, verbose);
	else while (argc > 1)  {
		sz += du(argv[1], recurse, all, verbose);
		n++;
		argv++;
		argc--;
	}
	if (n > 1  ||  (!all  &&  !verbose))
		printf("\nTotal = %s\n", format(sz));
	return 0;
}


static	__int64 du(char *path, int recurse, int all, int verbose)
{
	int	r, flg;
	HANDLE	h;
	WIN32_FIND_DATA	fi;
	struct	_stati64	sb;
	__int64	sz = 0L;
	char	buf[256];

	r = strlen(path) - 1;
	if (flg = (path[r] == '/'  ||  path[r] == '\\'  ||  path[r] == ':'))
		sprintf(buf, "%s*.*", path);
	else
		sprintf(buf, "%s/*.*", path);
	h = FindFirstFile(buf, &fi);
	r = h != INVALID_HANDLE_VALUE;
	while (r)  {
		if (strcmp(fi.cFileName, ".")  &&  strcmp(fi.cFileName, ".."))  {
			if (flg)
				sprintf(buf, "%s%s", path, fi.cFileName);
			else
				sprintf(buf, "%s/%s", path, fi.cFileName);
			r = _stati64(buf, &sb);
			if (!r)  {
				if (sb.st_mode & _S_IFDIR)  {
					if (recurse)
						sz += du(buf, recurse, all, verbose);
				}  else  {
					if (all)
						printf("%s %s\n", buf, format(sb.st_size));
					sz += sb.st_size;
				}
			}
		}
		r = FindNextFile(h, &fi);
	}
	if (h != INVALID_HANDLE_VALUE)
		FindClose(h);
	if (verbose || all)
		printf("%s %s\n", path, format(sz));
	return sz;
}


static	void	usage(void)
{
	printf("Usage:\tdu  [-options]  [dir]...\n"); 
	printf("Options:\n");
	printf("\ta\tdisplay all files\n");
	printf("\th\thelp\n");
	printf("\tr\trecurse into sub-directories\n");
	printf("\tv\tverbose - list directory totals\n");
	exit(1); 
}


static	char	*format(__int64 n)
{
	char	buf[30], *f, *t;
	static	char	rbuf[sizeof buf];
	int	i=0;

	sprintf(buf, "%29I64d", n);
	rbuf[sizeof(rbuf)-1] = '\0';
	for (f=buf+sizeof(buf)-2, t=rbuf+sizeof(rbuf)-2 ; isdigit(*f) ; i++) {
		if (i == 3) {
			*t-- = ',';
			i = 0;
		}
		*t-- = *f--;
	}
	return t + 1;
}



