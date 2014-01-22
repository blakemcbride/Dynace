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
#include <ctype.h>

#define XCD_DIR		"c:/"
#define DIRFILE		"pathmem"
#define PUSHFILE	"pushmem"
#define TMPFILE		"tmpfile"
#define NUMBUF		256

extern	char	*fgets(), *getcwd(), *getenv(), *strstr();
extern	FILE	*fopen();

static	void	godir();

static	void	savepwd(), pop(), poplist(), popclear();
static	void	delete(), pushsearch(), usage();
static	char	*rs(), *fname(), *search2();
static	FILE	*openf();
static	int	push(), search();

static	void	createCDfile(char *path);


static	char	XDFILE[] = "c:\\hXD.cmd";

main(argc, argv)
int	argc;
char	*argv[];
{
	char	file[256], *pwd;
	int	i, ret=0;
	FILE	*fp;

	createCDfile("");
	if (argc == 1)
		usage();
	
	fname(DIRFILE, file);
	
	pwd = getcwd(NULL, 256);
	fp = openf(file);
	savepwd(fp, pwd);
	for (i=1 ; i < argc ; ++i)
		if (*argv[i] == '-')
			if (isdigit(argv[i][1]))
				godir(fp, atoi(&argv[i][1]));
			else
				switch (argv[i][1])  {
				case 'c':
				case 'C':
					fclose(fp);
					unlink(file);
					fp = openf(file);
					break;
				case 'L':
				case 'l':
					list(fp);
					break;
				case 'S':
				case 's':
					ret = search(fp, argv[++i], pwd);
					break;
				case 'p':
				case 'P':
					switch (argv[i][2])  {
					case 'u':
					case 'U':
						if (argv[++i])  {
							if (!(ret=push(pwd, argv[i])))
								savepwd(fp, pwd=getcwd(NULL, 256));
						}  else
							pop(pwd);
						break;
					case 'o':
					case 'O':
						pop(NULL);
						break;
					case 'l':
					case 'L':
						poplist();
						break;
					case 'c':
					case 'C':
						popclear();
						break;
					case 's':
					case 'S':
						pushsearch(fp, argv[++i], pwd);
						break;
					default:
						usage();
					}
					break;
				default:
					usage();
				}
		else
			if (!(ret=my_chdir(argv[i])))
				savepwd(fp, pwd=getcwd(NULL, 256));
	fclose(fp);
	return ret;
}

static	void	savepwd(fp, pwd)
FILE	*fp;
char	*pwd;
{
	char	buf[NUMBUF];
	
	rewind(fp);
	while (fgets(buf, NUMBUF, fp))
		if (!strcmp(rs(buf), pwd))
			return;
	fseek(fp, 0L, SEEK_END);
	fprintf(fp, "%s\n", pwd);
}

static	void	godir(fp, n)
FILE	*fp;
int	n;
{
	int	x;
	char	buf[NUMBUF], *t;
	
	rewind(fp);
	for (x=0 ; x < n  &&  (t=fgets(buf, NUMBUF, fp)) ; ++x);
	if (!t)  {
		fprintf(stderr, "There is no directory #%d\n", n);
		exit(-1);
	}
	if (my_chdir(rs(buf)))
		delete(fp, buf);
}

static	int	search(fp, path, pwd)
FILE	*fp;
char	*path, *pwd;
{
	char	buf[NUMBUF];
	
	if (!path)
		return 0;
	while (my_chdir(search2(fp, path, pwd, buf)))
		delete(fp, buf);
	return 0;
}

static	void	pushsearch(fp, path, pwd)
FILE	*fp;
char	*path, *pwd;
{
	char	buf[NUMBUF];
	
	if (!path)
		return;
	while (push(pwd, search2(fp, path, pwd, buf)))
		delete(fp, buf);
}

static	int	match(buf, srch, slash)
char	*buf, *srch;
int	slash;
{
	int	i = strlen(buf) - 1;

	if (i < 0)
		return 0;
	for ( ; slash  &&  i > 0  ; i--)
		if (buf[i] == '\\')
			slash--;
	return !!strstr(buf+i+2, srch);
}

static	char	*search2(fp, path, pwd, buf)
FILE	*fp;
char	*path, *pwd, *buf;
{
	int	i, line, found, current, slash;
	char	sbuf[NUMBUF], ubuf[NUMBUF];
	
	for (slash=1, i=0 ; path[i] ; ++i)  {
		if (path[i] == '/')
			sbuf[i] = '\\';
		else
			sbuf[i] = toupper(path[i]);
		if (sbuf[i] == '\\')
			slash++;
	}
	sbuf[i] = '\0';

	rewind(fp);
	current = found = line = 0;
	while (fgets(buf, NUMBUF, fp))  {
		++line;
		strcpy(ubuf, buf);
		_strupr(ubuf);
		if (match(ubuf, sbuf, slash))
			found = line;
		if (!_stricmp(rs(buf), pwd))
			current = line;
	}

	if (!found)  {
		fprintf(stderr, "No match.\n");
		exit(-1);
	}

	i = found > current ? current : 0;
	rewind(fp);
	while (fgets(buf, NUMBUF, fp))  {
		strcpy(ubuf, buf);
		_strupr(ubuf);
		if (!i  &&  match(ubuf, sbuf, slash))
			return rs(buf);
		if (i)
			--i;
	}
	return NULL;  /*  should never happen  */
}

static	int	my_chdir(buf)
char	*buf;
{
#if 0
	unsigned  od;
	
	if (buf[1] == ':')  {
		od = _getdrive();
		_chdrive((unsigned) (toupper(buf[0]) - ('A'-1)));
	}
	if (chdir(buf))  {
		if (buf[1] == ':')
			_chdrive(od);
		fprintf(stderr, "Directory %s does not exist.\n", buf);
		return 1;
	}
#else
	createCDfile(buf);
#endif
	return 0;
}

static	list(fp)
FILE	*fp;
{
	char	buf[NUMBUF];
	int	i = 1;
	
	rewind(fp);
	while (fgets(buf, NUMBUF, fp)) 
		printf("%2d  %s", i++, buf);
}

static	char	*rs(x)
char	*x;
{
	char	*p = x;

	for ( ; *p ; p++)
		if (*p == ' ' || *p == '\r' || *p == '\n')  {
			*p = '\0';
			break;
		}
	return x;
}

static	char	*fname(file, buf)
char	*file, *buf;
{
	static	char	*xcdpath = NULL;
	static	int	i;

	if (!xcdpath)  {
		if (NULL == (xcdpath = getenv("XDFILE")))
			xcdpath = XCD_DIR;
		i = strlen(xcdpath) - 1;
	}
	strcpy(buf, xcdpath);
	if (buf[i] != '/'  &&  buf[i] != '\\')
		strcat(buf, "/");
	strcat(buf, file);
	return buf;
}

static	int	push(pwd, ndir)
char	*pwd, *ndir;
{
	char	file[256];
	FILE	*fp;

	if (my_chdir(ndir))
		return 1;
	fname(PUSHFILE, file);
	fp = openf(file);
	fseek(fp, 0L, SEEK_END);
	fprintf(fp, "%s\n", pwd);
	fclose(fp);
	return 0;
}

static	void	pop(pwd)
char	*pwd;
{
	char	file[256], tmpfile[256], path[NUMBUF], prev[NUMBUF];
	FILE	*fp, *tfp;

	fname(PUSHFILE, file);
	fname(TMPFILE, tmpfile);
	unlink(tmpfile);
	fp = openf(file);
	tfp = openf(tmpfile);
	*path = *prev = '\0';
	while (fgets(path, NUMBUF, fp))  {
		if (*prev)
			fputs(prev, tfp);
		strcpy(prev, path);
	}
	if (pwd)
		fprintf(tfp, "%s\n", pwd);
	fclose(fp);
	fclose(tfp);
	unlink(file);
	rename(tmpfile, file);
	if (*prev)
		my_chdir(rs(prev));
}

static	void	poplist()
{
	char	file[256], path[NUMBUF];
	FILE	*fp;

	fname(PUSHFILE, file);
	fp = openf(file);
	while (fgets(path, NUMBUF, fp))
		fputs(path, stdout);
	fclose(fp);
}

static	void	popclear()
{
	char	file[256];

	fname(PUSHFILE, file);
	unlink(file);
}

static	FILE	*openf(file)
char	*file;
{
	FILE	*fp;
	
	if (NULL == (fp = fopen(file, "r+")))
		if (NULL == (fp = fopen(file, "w+")))  {
			fprintf(stderr, "Can't create %s\n", file);
			exit(-1);
		}
	return fp;
}

static	void	delete(fp, dir)
FILE	*fp;
char	*dir;
{
	char	file[256], tmpfile[256], path[NUMBUF];
	FILE	*tfp;

	fname(DIRFILE, file);
	fname(TMPFILE, tmpfile);
	unlink(tmpfile);
	tfp = openf(tmpfile);
	rewind(fp);
	while (fgets(path, NUMBUF, fp))
		if (strcmp(dir, rs(path)))
			fprintf(tfp, "%s\n", path);
	fclose(tfp);
	freopen(tmpfile, "r+", fp);
	unlink(file);
	rename(tmpfile, file);
}

static	void	createCDfile(char *path)
{
	FILE	*fp;
	char	buf[256], *p;

	for (p=buf ; *path ; path++)
		*p++ = *path == '/' ? '\\' : *path;
	*p = '\0';
	
	SetFileAttributes(XDFILE, FILE_ATTRIBUTE_NORMAL);
	fp = fopen(XDFILE, "w");
	if (!fp) {
		fprintf(stderr, "Can't create %s\n", XDFILE);
		exit(1);
	}
	fprintf(fp, "@echo off\n");
	if (*buf) {
		if (buf[1] == ':')
			fprintf(fp, "%c:\n", *buf);
		fprintf(fp, "cd %s\n", buf);
	}
	fclose(fp);
	SetFileAttributes(XDFILE, FILE_ATTRIBUTE_HIDDEN);
}

static	void	usage()
{
	printf("Usage:     xd  [-option]  [dir]\n");
	printf("xd is used to change directories (much like cd).\n");
	printf("The dir may contain either slash (/ or \\) to separate directories.\n");
	printf("If a drive is specified, it will be changed to.\n");
	printf("Options:\n");
	printf("\ts\tgoto a directory by searching a list of previous dirs\n");
	printf("\tl\tlist previously accessed directories\n");
	printf("\tc\tclear list of previous directories\n");
	printf("\tpu dir\tpush current dir onto LIFO stack and goto dir\n");
	printf("\tpu\tswap last entry in LIFO stack with current dir\n");
	printf("\tpo\tpop off last entry in LIFO stack and goto it\n");
	printf("\tpl\tlist LIFO entrys\n");
	printf("\tpc\tclear LIFO stack\n");
	printf("\tps dir\tdoes a push to a directory searched in history list\n");
	exit(-1);
}
