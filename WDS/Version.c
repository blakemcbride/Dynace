
/*  Copyright 1995 Algorithms Corporation  */


#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#include <stdio.h>
#include <string.h>

#ifdef	TEST

static	char	volatile  version[] = "###+++ 1.00 +++###";

main(int argc, char *argv[])
{
	char	ver[80], dflt[20];
	int	m;

	if (argc < 2) {
		fprintf(stderr, "Usage:  version  file  [match string]\n");
		return 1;
	}
	strcpy(dflt, "###");
	strcat(dflt, "+++");
	m = Version(argv[1], argc > 2 ? argv[2] : dflt, ver, sizeof ver);
	if (m)
		printf("No version\n");
	else
		printf("Version = \"%s\"\n", ver);
	return 0;
}

#endif

#define	EOF_CHECK								\
			if (c == EOF)						\
				if (feof(fp)) {					\
					fclose(fp);				\
					return 2;   /* version not found */	\
				} else if (ferror(fp)) {			\
					fclose(fp);				\
					return 3;  /*  read error  */		\
				}


int	Version(char *file, char *pat, char *version, int vlen)
{
	FILE	*fp = fopen(file, "rb");
	int	r = 2;  /*  version not found  */
	char	epat[80];
	int	i, plen, c, mi=0, match=0;

	/*  push back vars  */
	char	buf[80];
	int	nbuf=0, ibuf=0;
	
	if (!fp)
		return 1;  /*  can't open file  */
	plen = strlen(pat);
	for (i=0 ; i < plen ; i++)
		epat[plen-i-1] = pat[i];
	epat[plen] = '\0';
	while (!match) {
		if (nbuf) {
			c = buf[ibuf++];
			if (ibuf == nbuf)
				nbuf = 0;
		} else {
			c = getc(fp);
			EOF_CHECK;
		}
		if (pat[mi] == c) {
			if (++mi == plen)
				match = 1;
		} else if (nbuf) {
			if (--nbuf)
				memmove(buf, buf+1, nbuf);
			ibuf = mi = 0;
		} else if (mi) {
			if (mi-- > 1)
				memcpy(buf, pat+1, mi);
			buf[mi] = c;
			nbuf = mi + 1;
			ibuf = mi = 0;
		}
			
	}

	while ((c = getc(fp)) == ' ');
	EOF_CHECK;
	vlen--;
	for (match=mi=i=0 ; !match ; i++) {
		if (i < vlen)
			version[i] = c;
		if (epat[mi] == c) {
			if (++mi == plen)
				match = 1;
		} else if (mi)
			mi = epat[0] == c;
		c = getc(fp);
		EOF_CHECK;
	}
	fclose(fp);
	if (match) {
		mi = i - plen;
		if (mi > vlen)
			mi = vlen;
		version[mi--] = '\0';
		while (mi >= 0  &&  version[mi] == ' ')
			version[mi--] = '\0';
		return 0;	/*  version found  */
	} else
		return 2;	/*  version not found  */
}

