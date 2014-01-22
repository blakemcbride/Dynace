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




#include <windows.h>
#include <stdio.h> 
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <io.h>
#include <malloc.h>
#include <dos.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>

#define	MAXDIR	256

static	void	strip_path(), print_error();
static	void	usage();
static	int	copy_file();
static	int	touch(char *f);

#define	here	fprintf(stderr, "%s (%d)\n", __FILE__, __LINE__);

static	int	date_diff, force, only_binary, only_ascii, debug, touch_file;



int	main(int argc, char *argv[])
{
	int	r, i, isdir, err=0, quiet=0, verbose=0, existing=0, zero=0;
	int	recurse=0;
	char	buf[MAXDIR], tmp[MAXDIR];

	if (argc < 3)
		usage();
	isdir = isDir(argv[argc-1]);
	argc--;
	for ( ; 1 < argc  &&  argv[1][0] == '-' ; argc--, argv++)
		for (r=1 ; argv[1][r] ; ++r)
			switch (argv[1][r])  {
			case 'a':
			case 'A':
				only_ascii = 1;
				break;
			case 'b':
			case 'B':
				only_binary = 1;
				break;
			case 'd':
			case 'D':
				date_diff = 1;
				break;
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
			case 'r':
			case 'R':
				recurse = 1;
				break;
			case 't':
			case 'T':
				touch_file = 1;
				break;
			case 'u':
			case 'U':
				debug = 1;
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
			} else if (!isDir(argv[i])) {
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
		}  else if (!isDir(argv[i])) {
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
	char	fpath[MAXDIR], tdir[MAXDIR], tpath[MAXDIR];
	int	r, flg, err=0;
	struct	_finddata_t	fi;
	long	fh;

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
		sprintf(fpath, "%s*", fd);
	else
		sprintf(fpath, "%s/*", fd);
	fh = _findfirst(fpath, &fi);
	r = fh == -1L;
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
		r = _findnext(fh, &fi);
	}
	if (fh != -1L)
		_findclose(fh);
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
	case 13:
		fprintf(stderr, "cp:  %s is read-only\n", tf);
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
	printf("\ta\tonly copy ascii files (not binary)\n");
	printf("\tb\tonly copy binary files (not ascii)\n");
	printf("\td\tonly copy if dates or sizes are different or non-existant file\n");
	printf("\te\tdon't overwrite existing files\n");
	printf("\tf\tforce overwrite of read-only files\n");
	printf("\tq\tquiet (no error messages)\n");
	printf("\tr\trecurse into sub-directories\n");
	printf("\tt\ttouch source before copy (to fix date compare problems)\n");
	printf("\tu\tdisplay debug info\n");
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

static	int	is_file_binary(char *file)
{
	FILE	*fp = fopen(file, "rb");
	int	c, binary = 0;

	if (!fp)
		return -1;
	while (!binary) {
		c = getc(fp);
		if (c == EOF  &&  (feof(fp)  ||  ferror(fp)))
			break;
		if (!isprint(c)  &&  /*  0x20 - 0x7E  */
		    !isspace(c)  &&  /*  0x09 - 0x0D  */
		    c != 7  &&  /*  bell  */
		    c != 8  &&  /*  backspace  */
		    c != 26 &&  /*  ^Z  */
		    c != 27  && /*  ESC  */
		    c != 31  &&   /*  used by .info files  */
		    c != 127)  /*  used by .info files  */
			binary = 1;
	}
	fclose(fp);
	return binary;
}

static	int	copy_file(ff, tf, verbose, existing)
char	*ff, *tf;
int	verbose;
int	existing;   /*  don't overwrite existing files  */
{
	if (date_diff) {
		struct	_stat	fsb, tsb;
#if 1
		/*  I had to change to the following code because my backup software changed the file modification
		    time by approx 1 sec sometimes.  */
		/*  Another problem I discovered is that Windows gets confused regarding time zones and daylight savings times.
		    The code I use fixes all that.  */
		long	ft, tt;

		if (!_stat(tf, &tsb)  &&  !_stat(ff, &fsb)  &&  GetUTCFileModTime (ff, &ft)  &&  GetUTCFileModTime (tf, &tt)) {
			long d = ft - tt;
			if (d < 0L)
				d = -d;
			if (debug)
				printf("time diff = %ld, size1 = %ld, size2 = %ld\n", d, (long) fsb.st_size, (long) tsb.st_size);
			if (d < 4  &&  fsb.st_size  == tsb.st_size)
				return 0;
		}
#else
		if (!stat(tf, &tsb)  &&
		    !stat(ff, &fsb)  &&
		    fsb.st_mtime == tsb.st_mtime  &&
		    fsb.st_size  == tsb.st_size)
			return 0;
#endif
	}
	if (only_binary  ||  only_ascii  &&  !(only_binary  &&  only_ascii)) {
		int	binary = is_file_binary(ff);
		if (only_binary  &&  !binary)
			return 0;
		if (only_ascii  &&  binary)
			return 0;
	}

	/*  deal with pre-existing target  */
	if (!access(tf, 0)) {
		if (existing)
			return 0;
		if (access(tf, 2))
			if (force)
				chmod(tf, _S_IREAD | _S_IWRITE);
			else
				return 13;  /*  read only  */
	}

	if (touch_file)
		touch(ff);

	if (verbose)
		printf("Copying %s -> %s\n", ff, tf);
	return !CopyFile(ff, tf, existing);
}

int	makeDir(f)
char	*f;
{
	char	path[MAXDIR], *p;
	struct	stat	sb;
	int	n;
	
	p = path;
	if (*f  &&  f[1] == ':')  {
		*p++ = *f++;
		*p++ = *f++;
	} else if (f[0] == '\\'  &&  f[1] == '\\')
		for (n=0 ; *f  &&  n < 4 ; ) {
			if (*f == '\\'  ||  *f == '/')
				n++;
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

static	int	touch(char *f)
{
	FILE	*fp;
	int	c, err=0;
	struct	stat	sb;

	if (!stat(f, &sb)  &&  (sb.st_mode & S_IFDIR))
		return 0;  /*  is directory  */
	if (NULL != (fp = fopen(f,"r+b")))  {
		fseek(fp, 0L, 2);
		if (ftell(fp) == 0L)  {
			fclose(fp);
			unlink(f);
			if (NULL == (fp = fopen(f, "w")))  {
				fprintf(stderr, "touch:  can't create %s", f);
				err = 1;
			}
			fclose(fp);
		}  else  {
			fseek(fp, 0L, 0);
			c = fgetc(fp);
			fseek(fp, 0L, 0);
			fputc(c, fp);
			fclose(fp);
		}
	}  else  {
		if (NULL == (fp = fopen(f, "w")))  {
			fprintf(stderr, "touch:  can't create %s", f);
			err = 1;
		}
		fclose(fp);
	}
	return err;
}




