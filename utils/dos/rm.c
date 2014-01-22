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

static	void	strip_path();
static	void	usage();
static	int	rm(), erase_dir();;


static	int	verbose = 0;


int	main(argc, argv)
int	argc;
char	*argv[];
{
	int	r, i, isdir, err=0, force=0, quiet=0, recur=0, zero=0;
	struct	stat	sb;
	
	if (argc < 2)
		usage();
	for (i=1 ; i < argc ; ++i)  {
		if (argv[i][0] == '-')  {
			for (r=1 ; argv[i][r] ; ++r)
				switch (argv[i][r])  {
				case 'f':
				case 'F':
					force = 1;
					break;
				case 'q':
				case 'Q':
					quiet = 1;
					verbose = 0;
					break;
				case 'r':
				case 'R':
					recur = 1;
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
					fprintf(stderr, "rm:  Unknown flag %c\n", argv[i][r]);
					usage();
					break;
				}
			continue;
		}
		r = stat(argv[i], &sb);
		if (!r  &&  recur  &&  (sb.st_mode & S_IFDIR))
			err |= erase_dir(argv[i], force, quiet);
		else
			err |= rm(argv[i], force, quiet);
	}
	return zero ? 0 : err;
}

static	void	usage()
{
	printf("Usage:\trm  [-option]  file...\n"); 
	printf("Options:\n");
	printf("\tf\tforce (good for read only files)\n");
	printf("\tq\tquiet (no error messages)\n");
	printf("\tr\trecurse sub-directories\n");
	printf("\tv\tverbose\n");
	printf("\tz\talways return 0\n");
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

static	int	rm(f, force, quiet)
char	*f;
int	force, quiet;
{
	int	r;

	if (verbose)
		fprintf(stderr, "Removing %s\n", f);
	r = unlink(f);
	if (r  &&  force)  {
		_dos_setfileattr(f, _A_NORMAL);
		r = unlink(f);
	}
	if (r  &&  !quiet)
		fprintf(stderr, "rm:  Can't remove %s\n", f);
	return(r);
}

static	int	erase_dir(d, force, quiet)
char	*d;
int	force, quiet;
{
	char	path[80];
	struct	stat	statbuf;
	struct	find_t	dir;
	int	err=0, r;
	
	sprintf(path, "%s/*.*", d);
	if (_dos_findfirst(path, _A_NORMAL|_A_RDONLY|_A_HIDDEN|_A_SYSTEM|_A_SUBDIR|_A_ARCH, &dir))  
		goto end;
	do {
		if (strcmp(dir.name, ".")  &&  strcmp(dir.name, ".."))  {
			sprintf(path, "%s/%s", d, dir.name);
			stat(path, &statbuf);
			if (statbuf.st_mode & S_IFDIR)  /*  its a directory	*/
				err |= erase_dir(path, force, quiet);
			else
				err |= rm(path, force, quiet);
		}
	}  while  (!_dos_findnext(&dir));
 end:
	if (strcmp(d, ".")  &&  strcmp(d, "..")  &&
	    strcmp(d, "/")  &&  strcmp(d, "\\"))  {
		r = rmdir(d);
		if (r  &&  !quiet)
			fprintf(stderr, "rm:  Can't remove directory %s\n", d);
		return r;
	}  else
		return 0;
}


