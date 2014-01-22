
int	SP_SaveValidation(char *file1, char *file2, char *seed, void *data, int size, char chkdrv);
int	SP_CheckValidation(char *file1, char *file2, char *seed, void *d1, int size);
int	SP_RemoveValidation(char *file1, char *file2);
long	SP_MakeCode3(char *seed, int n, void *buf);
long	SP_MakeCode2(char *seed, long a);
long	SP_MakeCode1(char *seed);
void	SP_FixString(char *v);
long	SP_DriveSN(char dr);
long	SP_HideSN(char *seed, long sn);



int	_SP_CheckValidation(char *file1, char *file2, char *seed, void *d1, int size,
			    long *c1a, long *c1b,
			    long *c2a, long *c2b,
			    long *sna, long *snb);

#define	SP_CHECKVALIDATION(file1, file2, seed, size)				\
	{									\
		long	c1a, c1b, c2a, c2b, sna, snb;				\
		int	r = _SP_CheckValidation(file1, file2, seed, NULL, size,	\
			    &c1a, &c1b,						\
			    &c2a, &c2b,						\
			    &sna, &snb);					\
		if (r  ||  c1a != c1b  ||  c2a != c2b  ||  sna != snb)		\
			exit(1);						\
	}
