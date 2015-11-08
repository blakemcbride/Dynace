

#ifdef	PLAN9
#include "plan9.h"
#endif

#include <stdio.h>

#ifndef PLAN9
#include <string.h>
#include <stdlib.h>
#if defined(__minix) ||  !defined(_MSC_VER)
#include <unistd.h>
#endif
#endif


#ifdef	unix
#define RMODE	"r"
#define WMODE	"w"
#else
#define RMODE	"rb"
#define WMODE	"wb"
#endif

static	void	copy(FILE *ffp, FILE *tfp);

int main(int argc, char *argv[])
{
	FILE	*ffp, *tfp;
	char	*ffile, tfile[20];

	strcpy(tfile, "tfXXXXXX");
	mktemp(tfile);
	while (--argc)  {
		if (NULL == (ffp = fopen(ffile=*++argv, RMODE)))  {
			fprintf(stderr, "Can't open %s\n", ffile);
			continue;
		}
		if (NULL == (tfp = fopen(tfile, WMODE)))  {
			fprintf(stderr, "Can't create %s\n", tfile);
			fclose(ffp);
			continue;
		}
		copy(ffp, tfp);
		fclose(ffp);
		fclose(tfp);
		unlink(ffile);
		rename(tfile, ffile);
	}
	exit(0);
#ifdef PLAN9
	return 0; /* never reached but needed by Plan-9  */
#endif
}

static	void	copy(FILE *ffp, FILE *tfp)
{
	register int	c;

	while (1)  {
		c = getc(ffp);
		if (c == EOF  &&  feof(ffp))
			return;
		if (c != '\r'  &&  c != 26)
			putc(c, tfp);
	}
}

