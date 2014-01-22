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
#include <fcntl.h>
#include <io.h>
#include <malloc.h>
#include <dos.h>
#include <errno.h>


static	void	strip_path(), print_error();
static	void	usage();
static	int	copy_file();

#define	here	fprintf(stderr, "%s (%d)\n", __FILE__, __LINE__);

int	main(argc, argv)
int	argc;
char	*argv[];
{
	int	r, i, isdir, err=0, quiet=0, verbose=0, existing=0, zero=0;
	int	recurse=0;
	char	buf[128], tmp[128];
	
	if (argc < 3)
		usage();
	isdir = isDir(argv[argc-1]);
	argc--;
	for ( ; 1 < argc  &&  argv[1][0] == '-' ; argc--, argv++)
		for (r=1 ; argv[1][r] ; ++r)
			switch (argv[1][r])  {
			case 'e':
			case 'E':
				existing = 1;
				break;
			case 'q':
			case 'Q':
				quiet = 1;
				verbose = 0;
				break;
			case 'r':
			case 'R':
				recurse = 1;
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
				fprintf(stderr, "cp:  Unknown flag %c\n", argv[1][r]);
				break;
			}
	if (!isdir  &&  recurse)  {
		if (makeDir(argv[argc]))  {
			fprintf(stderr, "cp:  Can't create directory %s\n", argv[argc]);
			exit(zero ? 0 : 1);
		}
		isdir = 1;
	}
	if (argc > 2  &&  !isdir  ||  argc < 2)
		usage();
	for (i=1 ; i < argc ; ++i)  {
		if (isdir)  {
			if (recurse  &&  isDir(argv[i]))  {
				if (copy_dir(argv[i], argv[argc], verbose, existing, quiet))
					err = 1;
			} else {
				char	c;

				strip_path(tmp, argv[i]);
				strcpy(buf, argv[argc]);
				c = buf[strlen(buf)-1];
				if (c != '/'  &&  c != '\\'  &&  c != ':')
					strcat(buf, "/");
				strcat(buf, tmp);
				r = copy_file(argv[i], buf, verbose, existing);
				if (r)  {
					if (!quiet)
						print_error(r, argv[i], buf);
					err = 1;
				}
			}
		}  else  {
			r = copy_file(argv[i], argv[argc], verbose, existing);
			if (r)  {
				if (!quiet)
					print_error(r, argv[i], argv[argc]);
				err = 1;
			}
		}
	}
	return zero ? 0 : err;
}

int	copy_dir(fd, td, verbose, existing, quiet)
char	*fd, *td;
int	verbose, existing, quiet;
{
	char	fpath[80], tdir[80], tpath[80];
	int	r, flg, err=0;
	struct	_find_t	fi;

	strip_path(fpath, fd);
	if (!strcmp(fpath, ".")  ||  !strcmp(fpath, ".."))
		return 0;
	if (*fpath) {
		char	c = td[strlen(td)-1];
		if (c == '/'  ||  c == '\\'  ||  c == ':')
			sprintf(tdir, "%s%s", td, fpath);
		else
			sprintf(tdir, "%s/%s", td, fpath);
	} else
		strcpy(tdir, td);
	if (makeDir(tdir))  {
		if (!quiet)
			print_error(12, NULL, tdir);
		return 12;
	}


	r = strlen(fd) - 1;
	if (flg = (fd[r] == ':'  ||  fd[r] == '/'  ||  fd[r] == '\\'))
		sprintf(fpath, "%s*.*", fd);
	else
		sprintf(fpath, "%s/*.*", fd);
	r = _dos_findfirst(fpath, (_A_ARCH|_A_HIDDEN|_A_NORMAL|_A_RDONLY|_A_SUBDIR|_A_SYSTEM), &fi);
	while (!r)  {
		if (flg)
			sprintf(fpath, "%s%s", fd, fi.name);
		else
			sprintf(fpath, "%s/%s", fd, fi.name);
		if (isDir(fpath))
			copy_dir(fpath, tdir, verbose, existing, quiet);
		else  {
			sprintf(tpath, "%s/%s", tdir, fi.name);
			r = copy_file(fpath, tpath, verbose, existing);
			if (r)  {
				if (!quiet)
					print_error(r, fpath, tpath);
				err = 1;
			}
		}
		r = _dos_findnext(&fi);
	}
	return err;
}

static	void	print_error(r, ff, tf)
int	r;
char	*ff, *tf;
{
	switch (r)  {
	case 1:
		fprintf(stderr, "cp:  source file %s doesn't exist\n", ff);
		break;
	case 2:
		fprintf(stderr, "cp:  can't create %s\n", tf);
		break;
	case 3:
		fprintf(stderr, "cp:  error writing to %s\n", tf);
		break;
	case 4:
		fprintf(stderr, "cp:  error reading %s\n", ff);
		break;
	case 7:
		fprintf(stderr, "cp:  attempt to overwrite existing file %s\n", tf);
		break;
	case 10:
		fprintf(stderr, "cp:  out of memory\n", ff);
		break;
	case 11:
		fprintf(stderr, "cp:  source file %s is a directory\n", ff);
		break;
	case 12:
		fprintf(stderr, "cp:  Can't create directory %s\n", tf);
		break;
	default:
		fprintf(stderr, "cp:  Can't copy %s to %s (error %d)\n", ff, tf, r);
		break;
	}
}

static	void	usage()
{
	printf("Usage:\tcp  [-option]  file  file\n"); 
	printf("\tcp  [-option]  file...  directory\n");
	printf("Options:\n");
	printf("\te\tdon't overwrite existing files\n");
	printf("\tq\tquiet (no error messages)\n");
	printf("\tr\trecurse into sub-directories\n");
	printf("\tv\tverbose\n");
	printf("\tz\talways return 0 exit status\n");
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

static	int	copy_file(ff, tf, verbose, existing)
char	*ff, *tf;
int	verbose;
int	existing;   /*  don't overwrite existing files  */
{
	int	fh, th, n;
	unsigned	date, time;
	static	char	*buf = NULL;
	static	unsigned	bs = 16384;
	
	if (existing  &&  !access(tf, 0))
		return 7;

	if (0 > (fh = open(ff, O_BINARY | O_RDONLY, 0)))
		return isDir(ff) ? 0 : 1;
	
	if (0 > (th = open(tf, O_BINARY | O_CREAT | O_TRUNC | O_WRONLY, S_IREAD | S_IWRITE)))  {
		close(fh);
		return(2);
	}
	
	if (!buf)
		while (bs > 127  &&  !(buf = malloc(bs)))
			bs /= 2;
	if (!buf)  {
		close(fh);
		close(th);
		return(10);	/*  out of memory	*/
	}
	
	if (verbose)
		printf("Copying %s -> %s\n", ff, tf);

	while (0 < (n = read(fh, buf, bs)))
		if (n != write(th, buf, n))  {
			close(fh);
			close(th);
			return(3);	/*  write error	  */
		}

	if (n)  {
		close(fh);
		close(th);
		return(4);	/*  read error   */
	}
	
	_dos_getftime(fh, &date, &time);
	_dos_setftime(th, date, time);

	if (close(fh))  {
		close(th);
		return(5);
	}
	if (close(th))
		return(6);
	return(0);
}

int	makeDir(f)
char	*f;
{
	char	path[100], *p;
	struct	stat	sb;
	
	p = path;
	if (*f  &&  f[1] == ':')  {
		*p++ = *f++;
		*p++ = *f++;
	}
	while (*f)  {
		for (*p++ = *f++ ; *f  &&  *f != '/' && *f != '\\' ; )
			*p++ = *f++;
		*p = '\0';
		if (stat(path, &sb))  {  /*  path doesn't exist  */
			if (mkdir(path))
				return 1;   /*  can't create  */
		} else if (!(sb.st_mode&S_IFDIR))
			return 1; 	/*  not a directory  */
	}
	return(0);
}

int	isDir(f)
char	*f;
{
	struct	stat	sb;
	char	buf[5];

	if (*f  &&  f[1] == ':'  &&  !f[2])  {
		strcpy(buf, f);
		strcat(buf, ".");
		f = buf;
	}
	if (stat(f, &sb))
		return 0;
	return sb.st_mode & S_IFDIR;
}




