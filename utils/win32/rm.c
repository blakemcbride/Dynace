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
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dos.h>
#include <io.h>
#include <windows.h>
#include <time.h>
#include <string.h>

#include "regex.h"

static	void	strip_path();
static	void	usage();
static	int	rm(), erase_dir();
static	int	matching_file(char *f);


static	int	verbose = 0;
static	int	showonly;
static	char	*missing_path;
static	time_t	age;
static	int	clear;


#define	SKIP(f)		(pb  &&  re_match(pb, (f), strlen(f), 0, NULL) < 0  ||  matching_file(f))



int	main(argc, argv)
int	argc;
char	*argv[];
{
	int	r, i, isdir, err=0, force=0, quiet=0, recur=0, zero=0, dosnames=0, missing=0;
	struct	stat	sb;
	struct	re_pattern_buffer	pb;
	
	if (argc < 2)
		usage();
	for (i=1 ; i < argc ; ++i)  {
		if (argv[i][0] == '-')  {
			for (r=1 ; argv[i][r] ; ++r)
				switch (argv[i][r])  {
				case 'a':
				case 'A':  {
					char	buf[80];
					int	t, tr;

					for (t=0, ++r ; isdigit(argv[i][r]) ; )
						buf[t++] = argv[i][r++];
					if (t) {
						buf[t] = '\0';
						switch (argv[i][r]) {
						case 'h':
						case 'H':
							age = time(NULL) - atoi(buf) * 60 * 60;
							break;
						case 'm':
						case 'M':
							age = time(NULL) - atoi(buf) * 60;
							break;
						case 'd':
						case 'D':
							age = time(NULL) - atoi(buf) * 60 * 60 * 24;
							break;
						default:
							r--;
							break;
						}
					} else
						r--;
				}
				break;
				case 'c':
				case 'C':
					clear = 1;
					break;
				case 'd':
				case 'D':
					if (!dosnames) {
						static	char	pat[] = ".*~[0-9]+$\\|.*~[0-9]+\\..+";

						dosnames = 1;
						pb.translate = NULL;
						pb.fastmap = 0;
						pb.buffer = NULL;
						pb.allocated = 0;
						re_set_syntax(RE_INTERVALS);
						re_compile_pattern(pat, strlen(pat), &pb);
					}
					break;
				case 'f':
				case 'F':
					force = 1;
					break;
				case 'm':
				case 'M':
					missing = 1;
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
				case 's':
				case 'S':
					showonly = 1;
					verbose = 1;
					quiet = 0;
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
		} else if (missing == 1) {
			missing_path = argv[i];
			missing++;
		} else {
			r = stat(argv[i], &sb);
			if (!r  &&  recur  &&  (sb.st_mode & S_IFDIR))
				err |= erase_dir(argv[i], force, quiet, dosnames ? &pb : NULL);
			else
				err |= rm(argv[i], force, quiet, dosnames ? &pb : NULL);
		}
	}
	if (dosnames)
		regfree(&pb);
	return zero ? 0 : err;
}

static	void	usage()
{
	printf("Usage:\trm  [-option]  file...\n"); 
	printf("Usage:\trm  [-m[options]]  path  file...\n"); 
	printf("Options:\n");
	printf("\taN[mhd]\tN minutes/hours/days old or older\n");
	printf("\tc\tclear file contents before removing\n");
	printf("\td\tall long names with ~number in them\n");
	printf("\tf\tforce (good for read only files)\n");
	printf("\tm\tremove files not also found under path/file\n");
	printf("\tq\tquiet (no error messages)\n");
	printf("\tr\trecurse sub-directories\n");
	printf("\ts\tshow what would be removed (don't actually remove anything)\n");
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

static	void	zero_file(char *f)
{
	__int64 size;
	char	buf[1024];
	int	n, fh = open(f, _O_BINARY | _O_WRONLY);
	if (fh == -1)
		return;
	memset(buf, 0, sizeof buf);
	size = _filelengthi64(fh);
	_lseeki64(fh, (__int64)0, SEEK_SET);
	while (size) {
		n = sizeof(buf) > size ? size : sizeof(buf);
		write(fh, buf, n);
		size -= n;
	}
	_commit(fh);
	close(fh);
}

static	int	rm(f, force, quiet, pb)
char	*f;
int	force, quiet;
struct	re_pattern_buffer	*pb;
{
	int	r=0;
	
	if (SKIP(f))
		return r;
	if (age) {
		struct _stat sb;
		if (!_stat(f, &sb)  &&  sb.st_mtime > age)
			return r;
	}
	
	if (verbose)
		fprintf(stderr, "Removing %s\n", f);
	if (!showonly) {
		if (clear)
			zero_file(f);
		r = unlink(f);
	}
	if (r  &&  force)  {
		SetFileAttributes(f, FILE_ATTRIBUTE_NORMAL);
		r = unlink(f);
	}
	if (r  &&  !quiet)
		fprintf(stderr, "rm:  Can't remove %s\n", f);
	return(r);
}

static	int	erase_dir(d, force, quiet, pb)
char	*d;
int	force, quiet;
struct	re_pattern_buffer	*pb;
{
	char	path[256];
	struct	stat	statbuf;
	struct	_finddata_t	dir;
	int	err=0, r;
	long	fh;
	
	sprintf(path, "%s/*", d);
	if (-1L == (fh = _findfirst(path, &dir)))
		goto end;
	do {
		if (strcmp(dir.name, ".")  &&  strcmp(dir.name, ".."))  {
			sprintf(path, "%s/%s", d, dir.name);
			stat(path, &statbuf);
			if (statbuf.st_mode & S_IFDIR)  /*  its a directory	*/
				err |= erase_dir(path, force, quiet, pb);
			else
				err |= rm(path, force, quiet, pb);
		}
	}  while  (!_findnext(fh, &dir));
	_findclose(fh);
 end:
	if (strcmp(d, ".")  &&  strcmp(d, "..")  &&
	    strcmp(d, "/")  &&  strcmp(d, "\\")  &&  !SKIP(d))  {
		if (verbose)
			fprintf(stderr, "Removing %s\n", d);
		if (showonly)
			r = 0;
		else {
			r = rmdir(d);
			if (r  &&  force) {
				SetFileAttributes(d, FILE_ATTRIBUTE_NORMAL);
				r = rmdir(d);
			}
		}
		if (r  &&  !quiet)
			fprintf(stderr, "rm:  Can't remove directory %s\n", d);
		return r;
	}  else
		return 0;
}

static	int	matching_file(char *file)
{
	char	buf[256];

	if (missing_path) {
		char	c = missing_path[strlen(missing_path)-1];
		if (c == '/'  ||  c == '\\'  ||  c == ':')
			sprintf(buf, "%s%s", missing_path, file);
		else
			sprintf(buf, "%s/%s", missing_path, file);
		return !access(buf, 0);
	} else
		return 0;
}




