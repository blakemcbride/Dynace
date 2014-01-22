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


static	void	print_error(int r, char *f1, char *f2);
static	void	strip_path(char *to, char *from);
static	void	usage();
static	int	cmp_file(char *f1, char *f2, int dates);

#define	here	fprintf(stderr, "%s (%d)\n", __FILE__, __LINE__);

int	main(int argc, char *argv[])
{
	int	r, i, isdir, err=0, quiet=0, dates=0, rev=0, verbose=0;
	struct	stat	sb;
	char	buf[128], tmp[128], *f2;
	
	if (argc < 3)
		usage();
	strip_path(tmp, argv[argc-1]);
	if (!*tmp  ||  !strcmp(tmp, ".")  ||  !strcmp(tmp, ".."))
		isdir = 1;
	else  {
		r = stat(argv[argc-1], &sb);
		isdir = !r  &&  (sb.st_mode & S_IFDIR);
	}
	if (argc > 3+(argv[1][0]=='-')  &&  !isdir)
		usage();
	argc--;
	for (i=1 ; i < argc ; ++i)  {
		if (argv[i][0] == '-')  {
			for (r=1 ; argv[i][r] ; ++r)
				switch (argv[i][r])  {
				case 'd':
				case 'D':
					dates = 1;
					break;
				case 'q':
				case 'Q':
					quiet = 1;
					verbose = 0;
					break;
				case 'r':
				case 'R':
					rev = 1;
					break;
				case 'v':
				case 'V':
					verbose = 1;
					quiet = 0;
					break;
				default:
					fprintf(stderr, "cmp:  Unknown flag %c\n", argv[i][r]);
					break;
				}
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
			r = cmp_file(argv[i], f2=buf, dates);
		}  else
			r = cmp_file(argv[i], f2=argv[argc], dates);
		if (r)
			err = 1;
		if (!quiet)
			if (!r  &&  (verbose  ||  rev))
				fprintf(stdout, "%s and %s compare OK\n", argv[i], f2);
			else if (r  &&  (verbose  ||  !rev))
				print_error(r, argv[i], f2);
	}
	return err;
}

static	void	print_error(int r, char *f1, char *f2)
{
	switch (r)  {
	case 1:
		fprintf(stdout, "%s doesn't exist.\n", f1);
		break;
	case 2:
		fprintf(stdout, "%s doesn't exist.\n", f2);
		break;
	case 3:
		fprintf(stdout, "%s and %s aren't the same mode.\n", f1, f2);
		break;
	case 4:
		fprintf(stdout, "%s and %s aren't the same size.\n", f1, f2);
		break;
	case 5:
		fprintf(stdout, "Can't open %s\n", f1);
		break;
	case 6:
		fprintf(stdout, "Can't open %s\n", f2);
		break;
	case 7:
		fprintf(stderr, "Out of memory.\n");
		exit(-1);
	case 8:
		fprintf(stdout, "Error reading %s\n", f2);
		break;
	case 9:
		fprintf(stdout, "%s and %s are different.\n", f1, f2);
		break;
	case 10:
		fprintf(stdout, "Error reading %s\n", f1);
		break;
	case 13:
		fprintf(stderr, "Files %s and %s have different dates.\n", f1, f2);
		break;
	default:
		fprintf(stdout, "cmp:  Can't compare %s to %s (error %d)\n", f1, f2, r);
		break;
	}
}

static	void	usage(void)
{
	printf("Usage:\tcmp  [-options]  file  file\n"); 
	printf("\tcmp  [-options]  file...  directory\n");
	printf("Options:\n");
	printf("\td\tcompare file dates as well as contents\n");
	printf("\tq\tquiet (prints no messages - used for return result)\n");
	printf("\tr\tprint files which compare instead of not compare\n");
	printf("\tv\tprint status of all files\n");
	exit(1); 
}

static	void	strip_path(char *to, char *from)
{
	char	*t;

	for (t=from ; *t ; ++t)
		if (*t == '/'  ||  *t == '\\'  ||  *t == ':')
			from = t + 1;
	strcpy(to, from);
}

static	int	cmp_file(char *f1, char *f2, int dates)
{
	int	h1, h2, n, ret = 0;
	struct	_stati64	sb1, sb2;
	static	char	*buf1 = NULL, *buf2 = NULL;
	static	unsigned	bs = 16384;
	
	if (_stati64(f1, &sb1))
		return 1;
	if (sb1.st_mode & S_IFDIR)
		return 0;
	if (_stati64(f2, &sb2))
		return 2;
/*
	if (sb1.st_mode != sb2.st_mode)
		return 3;
*/
	if (sb1.st_size != sb2.st_size)
		return 4;

	if (0 > (h1 = open(f1, O_BINARY | O_RDONLY, 0)))
		return 5;
	
	if (0 > (h2 = open(f2, O_BINARY | O_RDONLY, 0)))  {
		close(h1);
		return 6;
	}
	
	if (!buf1) 
		buf1 = malloc(bs);
	if (!buf2)
		buf2 = malloc(bs);
	
	if (!buf1  ||  !buf2)  {
		close(h1);
		close(h2);
		return 7;	/*  out of memory	*/
	}
	
	while (0 < (n = read(h1, buf1, bs)))  {
		if (n != read(h2, buf2, n))  {
			close(h1);
			close(h2);
			return 8;	/*  read error	  */
		}
		if (memcmp(buf1, buf2, n))  {
			ret = 9;
			break;
		}
	}

	if (n < 0)  {
		close(h1);
		close(h2);
		return 10;	/*  read error   */
	}
	
	if (close(h1))  {
		close(h2);
		return 11;
	}
	if (close(h2))
		return 12;

	if (dates  &&  sb1.st_mtime != sb2.st_mtime)
		return 13;
	    
	return ret;
}



