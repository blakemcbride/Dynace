
#ifndef _PLAN9_H_
#define	_PLAN9_H_

#include <u.h>
#include <libc.h>

typedef	ulong	size_t;
typedef	long		time_t;

#define	volatile
#define	rmdir(d)	remove(d)
#define	lseek(fd, n, type)	((long) seek((fd), (vlong) (n), (type)))
#define	_lseek(fd, n, type)	((long) seek((fd), (vlong) (n), (type)))

int	unlink(char *file);
int	rename(char *from, char *to);
void	exit(int n);
int	mkdir(char *dir);

#endif
