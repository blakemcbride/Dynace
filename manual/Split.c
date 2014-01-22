

#include <stdio.h>
#include <string.h>
#include <ctype.h>


#define	BUFSIZE	510


static	void	section(FILE *in, char *buf);

main(int argc, char *argv[])
{
	FILE	*in;
	char	buf[BUFSIZE+2];
	
	if (argc != 2)
		return 1;
	in = fopen(argv[1], "rt");
	while (fgets(buf, BUFSIZE, in))
		while (!strncmp(buf, "@section", 8))
			section(in, buf);
	fclose(in);
	return 0;
}

static	void	section(FILE *in, char *buf)
{
	FILE	*out;
	char	name[128], *f, *t;
	
	for (t=name, f=buf+9 ; *f == ' '  ||  *f == '\t' ; f++);
	while (*f  &&  !isspace(*f))
		*t++ = *f++;
	*t = '\0';
	strcat(name, ".tex");
/*	printf("@include %s\n", name);  */
	printf("cvs add %s\n", name);
	out = fopen(name, "wt");
	fputs("\n", out);
	fputs(buf, out);
	while (fgets(buf, BUFSIZE, in)  &&  strncmp(buf, "@section", 8))
		fputs(buf, out);	
	fclose(out);
}
