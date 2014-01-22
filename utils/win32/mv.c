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
#include <sys/types.h>
#include <sys/stat.h>
#include <dos.h>
#include <windows.h>

static	void	strip_path();
static	void	usage();
static	int	rm();


int	main(argc, argv)
int	argc;
char	*argv[];
{
	int	r, i, isdir, err=0, quiet=0, force=0, existing=0, verbose=0, zero=0;
	struct	stat	sb;
	char	buf[128], tmp[128];
	
	if (argc < 3)
		usage();
	strip_path(tmp, argv[argc-1]);
	if (!*tmp  ||  !strcmp(tmp, ".")  ||  !strcmp(tmp, ".."))
		isdir = 1;
	else  {
		r = stat(argv[argc-1], &sb);
		isdir = !r  &&  (sb.st_mode & S_IFDIR);
	}
	argc--;
	for ( ; 1 < argc  &&  argv[1][0] == '-' ; argc--, argv++)
		for (r=1 ; argv[1][r] ; ++r)
			switch (argv[1][r])  {
			case 'e':
			case 'E':
				existing = 1;
				break;
			case 'f':
			case 'F':
				force = 1;
				break;
			case 'q':
			case 'Q':
				quiet = 1;
				verbose = 0;
				break;
			case 'v':
			case 'V':
				verbose = 1;
				quiet = 0;
				break;
			case 'z':
			case 'Z':
				zero = 1;
				break;
			default:
				fprintf(stderr, "mv:  Unknown flag %c\n", argv[i][r]);
				usage();
				break;
			}
	if (argc > 2  &&  !isdir  ||  argc < 2)
		usage();
	for (i=1 ; i < argc ; ++i)  {
		r = stat(argv[i], &sb);
		if (r)  {
			if (!quiet)
				fprintf(stderr, "mv:  source file %s doesn't exist\n", argv[i]);
			err = 1;
			continue;
		}
		if ((sb.st_mode & S_IFDIR)  &&  isdir) {
			if (!quiet)
				fprintf(stderr, "mv:  %s and %s are both directories\n", argv[i], argv[argc]);
			err = 1;
			continue;
		}
		if (isdir)  {
			char	c;

			strip_path(tmp, argv[i]);
			strcpy(buf, argv[argc]);
			c = buf[strlen(buf)-1];
			if (c != '/'  &&  c != '\\'  &&  c != ':')
				strcat(buf, "/");
			strcat(buf, tmp);
			if (!existing)
				rm(buf, force);
			r = rename(argv[i], buf);
			if (r)  {
				if (!quiet)
					fprintf(stderr, "mv:  Can't move %s to %s\n", argv[i], buf);
				err = 1;
			} else if (verbose)
				fprintf(stderr, "Moving %s -> %s\n", argv[i], buf);
		}  else  {
			if (!existing)
				rm(argv[argc], force);
			r = rename(argv[i], argv[argc]);
			if (r)  {
				if (!quiet)
					fprintf(stderr, "mv:  Can't move %s to %s\n", argv[i], argv[argc]);
				err = 1;
			} else if (verbose)
				fprintf(stderr, "Moving %s -> %s\n", argv[i], argv[argc]);
		}
	}
	return zero ? 0 : err;
}

static	void	usage()
{
	printf("Usage:\tmv  [-options]  file  file\n"); 
	printf("\tmv  [-options]  file...  directory\n");
	printf("Options:\n");
	printf("\te\tdon't distroy pre-existing files\n");
	printf("\tf\tforce (even if read only)\n");
	printf("\tq\tquiet (no error messages)\n");
	printf("\tv\tverbose\n");
	printf("\tz\talways return 0 exist status\n");
	exit(1); 
}

static	void	strip_path(to, from)
char	*from, *to;
{
	char	*t;

	for (t=from ; *t ; ++t)
		if (*t == '/'  ||  *t == '\\'  ||  *t == ':')
			from = t + 1;
	strcpy(to, from);
}

static	int	rm(f, force)
char	*f;
int	force;
{
	int	r;

	r = unlink(f);
	if (r  &&  force)  {
		SetFileAttributes(f, FILE_ATTRIBUTE_NORMAL);
		r = unlink(f);
	}
	return(r);
}




