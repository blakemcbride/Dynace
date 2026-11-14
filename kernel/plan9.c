

#include <u.h>
#include <libc.h>
#include <stdio.h>

static	void	split(char *, char **, char **);
static	int	samefile(char *, char *);
static	int	mv(char *from, char *todir, char *toelem);
static	int	copy1(int fdf, int fdt, char *from, char *to);
static	int	hardremove(char *);

int	errno;		/*  set only by the functions here (see plan9.h)  */
#ifndef	EIO
#define	EIO	5	/*  same values in plan9.h  */
#endif
#ifndef	EEXIST
#define	EEXIST	17
#endif

int	unlink(char *file)
{
	return remove(file);
}

void	exit(int n)
{
	char	buf[128];

	if (!n)
		exits(0);
	sprintf(buf, "Exit code %d", n);
	exits(buf);
}


/*  rename, adapted from mv(1).  Returns 0, or -1 if it fails.  split()
    cuts its argument, so it works on copies and the caller's strings are
    left alone.  */

int	rename(char *from, char *to)
{
	Dir *dirto, *dirfrom;
	char *todir, *toelem;
	char fbuf[4096], tbuf[4096];

	if (strlen(from) >= sizeof fbuf  ||  strlen(to) >= sizeof tbuf)
		return -1;
	strcpy(fbuf, from);
	strcpy(tbuf, to);
	if ((dirto = dirstat(tbuf)) != nil && (dirto->mode&DMDIR)) {
		if ((dirfrom = dirstat(fbuf)) !=nil && (dirfrom->mode & DMDIR))
			split(tbuf, &todir, &toelem);
		else	{
			todir = tbuf;
			toelem = 0;	/* toelem will be fromelem */
		}
		free(dirfrom);
	} else
		split(tbuf, &todir, &toelem);
	free(dirto);
	return mv(fbuf, todir, toelem) < 0 ? -1 : 0;
}

/*  from is cut by split(), so the caller passes a copy  */

static	int	mv(char *from, char *todir, char *toelem)
{
	Dir *dirb, *dirt, null;
	char toname[4096], fromname[4096], oldname[4096], oldelem[64];
	int fdf, fdt, i, j, aside = 0;
	int stat = -1;
	char *fromdir, *fromelem;

	dirb = dirstat(from);
	if(dirb == nil){
		fprint(2, "mv: can't stat %s: %r\n", from);
		return -1;
	}
	strncpy(fromname, from, sizeof fromname);
	split(from, &fromdir, &fromelem);
	if(toelem == 0)
		toelem = fromelem;
	i = strlen(toelem);
	if(i==0){
		fprint(2, "mv: null last name element moving %s\n", fromname);
		goto out;
	}
	j = strlen(todir);
	if(i + j + 2 > sizeof toname){
		fprint(2, "mv: path too big (max %d): %s/%s\n", sizeof toname, todir, toelem);
		goto out;
	}
	memmove(toname, todir, j);
	toname[j] = '/';
	memmove(toname+j+1, toelem, i);
	toname[i+j+1] = 0;
	if(samefile(fromdir, todir)){
		if(samefile(fromname, toname)){
			fprint(2, "mv: %s and %s are the same\n", fromname, toname);
			goto out;
		}
		/*  wstat can't rename onto an existing name, so an existing
		    target is renamed aside first and put back if the rename
		    fails, rather than removed before it is known to work  */
		dirt = dirstat(toname);
		if(dirt != nil){
			free(dirt);
			snprint(oldelem, sizeof oldelem, ".mv%d.old", getpid());
			if(j + strlen(oldelem) + 2 > sizeof oldname){
				fprint(2, "mv: path too big (max %d): %s/%s\n", sizeof oldname, todir, oldelem);
				goto out;
			}
			snprint(oldname, sizeof oldname, "%s/%s", todir, oldelem);
			nulldir(&null);
			null.name = oldelem;
			if(dirwstat(toname, &null) < 0){
				fprint(2, "mv: can't move %s aside: %r\n", toname);
				goto out;
			}
			aside = 1;
		}
		nulldir(&null);
		null.name = toelem;
		if(dirwstat(fromname, &null) >= 0){
			if(aside  &&  hardremove(oldname) < 0){
				/*  a non-empty directory: undo both renames, as
				    POSIX rename fails  */
				nulldir(&null);
				null.name = fromelem;
				dirwstat(toname, &null);
				nulldir(&null);
				null.name = toelem;
				dirwstat(oldname, &null);
				goto out;
			}
			stat = 0;
			goto out;
		}
		if(aside){
			nulldir(&null);
			null.name = toelem;
			dirwstat(oldname, &null);
		}
		if(dirb->mode & DMDIR){
			fprint(2, "mv: can't rename directory %s: %r\n", fromname);
			goto out;
		}
	}
	/*
	 * Renaming won't work --- must copy
	 */
	if(dirb->mode & DMDIR){
		fprint(2, "mv: %s is a directory, not copied to %s\n", fromname, toname);
		goto out;
	}
	fdf = open(fromname, OREAD);
	if(fdf < 0){
		fprint(2, "mv: can't open %s: %r\n", fromname);
		goto out;
	}
	dirt = dirstat(toname);
	if(dirt != nil && (dirt->mode & DMAPPEND)  &&  hardremove(toname) < 0){	/* because create() won't truncate file */
		free(dirt);
		close(fdf);
		goto out;
	}
	free(dirt);
	fdt = create(toname, OWRITE, dirb->mode);
	if(fdt < 0){
		fprint(2, "mv: can't create %s: %r\n", toname);
		close(fdf);
		goto out;
	}
	if ((stat = copy1(fdf, fdt, fromname, toname)) != -1) {
		nulldir(&null);
		null.mtime = dirb->mtime;
		null.mode = dirb->mode;
		dirfwstat(fdt, &null);	/* ignore errors; e.g. user none always fails */
		if (remove(fromname) < 0) {
			fprint(2, "mv: can't remove %s: %r\n", fromname);
			stat = -1;
		}
	}
	close(fdf);
	close(fdt);
 out:
	free(dirb);
	return stat;
}

static	int	copy1(int fdf, int fdt, char *from, char *to)
{
	char buf[8192];
	long n, n1;

	for(;;) {
		n = read(fdf, buf, sizeof buf);
		if(n <= 0)
			break;
		n1 = write(fdt, buf, n);
		if(n1 != n) {
			fprint(2, "mv: error writing %s: %r\n", to);
			return -1;
		}
	}
	if(n < 0) {
		fprint(2, "mv: error reading %s: %r\n", from);
		return -1;
	}
	return 0;
}

static	void	split(char *name, char **pdir, char **pelem)
{
	char *s;

	s = utfrrune(name, '/');
	if(s) {
		*s = 0;
		*pelem = s+1;
		*pdir = name;
	} else if (strcmp(name, "..") == 0) {
		*pdir = "..";
		*pelem = ".";
	} else {
		*pdir = ".";
		*pelem = name;
	}
}

int
samefile(char *a, char *b)
{
	Dir *da, *db;
	int ret;

	if(strcmp(a, b) == 0)
		return 1;
	da = dirstat(a);
	db = dirstat(b);
	ret = (da !=nil ) &&
		(db != nil) &&
		(da->qid.type==db->qid.type) &&
		(da->qid.path==db->qid.path) &&
		(da->qid.vers==db->qid.vers) &&
		(da->dev==db->dev) &&
		da->type==db->type;
	free(da);
	free(db);
	return ret;
}

/*  Returns -1 if a can't be removed (mv(1) exits instead).  */

static	int	hardremove(char *a)
{
	if(remove(a) == -1){
		fprint(2, "mv: can't remove %s: %r\n", a);
		return -1;
	}
	do; while(remove(a) != -1);
	return 0;
}

/*  POSIX results: 0, or -1 with errno EEXIST or EIO  */

int	mkdir(char *dir)
{
	int f;

	if (access(dir, 0) == AEXIST)  {
		errno = EEXIST;
		return -1;
	}
	f = create(dir, OREAD, DMDIR | 0777L);
	if (f < 0)  {
		errno = EIO;
		return -1;
	}
	close(f);
	return 0;
}
