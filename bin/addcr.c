

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef	unix
#define RMODE	"r"
#define WMODE	"w"
#else
#define RMODE	"rb"
#define WMODE	"wb"
#endif

static	void	copy(FILE *ffp, FILE *tfp);

main(int argc, char *argv[])
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
}

static	void	copy(FILE *ffp, FILE *tfp)
{
	register int	c, pc;

	for (pc=0 ; 1 ; pc = c)  {
		c = getc(ffp);
		if (c == EOF  &&  feof(ffp))
			return;
		if (c == '\n'  &&  pc != '\r')
			putc('\r', tfp);
		putc(c, tfp);
	}
}

