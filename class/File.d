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


#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#else
#include <unistd.h>
#endif


#include <sys/types.h>
#if	!defined(vms)  &&  !defined(__MWERKS__)
#include <sys/stat.h>
#endif
#if	defined(sparc)  ||  defined(unix)
#include <unistd.h>
#endif
#include <time.h>
#if defined(_MSC_VER)
#include <direct.h>
#endif

#include <string.h>

defclass  File : Stream  {
	iName;
	FILE	*iFP;
	int	iTempFile;
	int	iFileAlreadyOpened;
 class:
	unsigned int cNumTempFiles;
	char cTempSubDir[32];
	char cTempFileDir[256];
	long cProcessID;

 init:	class_init;
};

#if	defined(unix)  ||  defined(__APPLE__)  ||  defined(__minix) || defined(__HAIKU__)
#define	MKDIR(d)	mkdir(d, 0777)
#else
#define	MKDIR(d)	mkdir(d)
#endif

cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

private	imeth	File_initVars(char *name, FILE *fp)
{
	iName = gNewWithStr(String, name);
	iFP = fp;
	return self;
}

cmeth	gNewWithStream(char *name, FILE *fp)
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	iFileAlreadyOpened = 1;
	return File_initVars(obj, name, fp);
}

cmeth	gOpenFile, <vNew> (char *name, char *mode)
{
	FILE	*fp;

	if (IsObj((object) name))
		name = gStringValue((object) name);
	if (IsObj((object) mode))
		mode = gStringValue((object) mode);
	fp = fopen(name, mode);
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(name, mode);
	}
	if (!fp)
		return NULL;
	return File_initVars(gNew(super), name, fp);
}

private	imeth	set_temp()
{
	iTempFile = 1;
	return self;
}

cmeth	gOpenTempFile()
{
	char	file[300];
	static	unsigned	tfn = 0;
	object	obj;

	if (!cProcessID) {
		int	len;
		char	tp[256];
		static	char	*pp = NULL;

		if (!pp) {
			pp = getenv("TMP");
			if (!pp)
				pp = getenv("TEMP");
			if (!pp)
				pp = ".";
		}
		strcpy(tp, pp);
		len = strlen(tp);
		if (len >= 1  &&  tp[len-1] != '\\'  &&  tp[len-1] != '/') {
#ifdef unix
			strcat(tp, "/");
#else
			strcat(tp, "\\");
#endif
		}
		

		//check for existence of top level name
		//create directory if found
		if (*cTempSubDir) {

			strcat(tp, cTempSubDir);
			MKDIR(tp);
#ifdef unix
			strcat(tp, "/");
#else
			strcat(tp, "\\");
#endif
		}



#if  defined(_WIN32)  &&  !defined(__WINE__)
		cProcessID = GetCurrentProcessId();
#else
		cProcessID = time(NULL);
#endif	      

		//add counter to the end of process id to
		//insure uniqueness of the eventual directory name
		//in the case of process id duplication
		//across a citrix farm.
		//increment this counter in a loop until
		//a directory name is built with it that
		//doesn't already exist in the file system.

		sprintf(cTempFileDir, "%s%u", tp, (unsigned) cProcessID);
		while (MKDIR(cTempFileDir))
			sprintf(cTempFileDir, "%s%u", tp, (unsigned) ++cProcessID);
	}
#ifdef unix
	sprintf(file, "%s/%u-%u.tmp", cTempFileDir, (unsigned) cProcessID, ++tfn);
	obj = gOpenFile(self, file, "w+");
#else
	sprintf(file, "%s\\%u-%u.tmp", cTempFileDir, (unsigned) cProcessID, ++tfn);
	obj = gOpenFile(self, file, "w+b");
#endif
	if (obj) {
		cNumTempFiles++;
		return set_temp(obj);
	} else
		return NULL;
}

private	cmeth	StreamObject(object self, char *name, FILE *fp)
{
	return File_initVars(gNew(super), name, fp);
}

private cmeth	unlinkTempFile(object self, char *name)
{
	unlink(name); 
	cNumTempFiles--;

	if (!cNumTempFiles) {
		rmdir(cTempFileDir);
		cProcessID = 0L;
	}
	return self;
}

imeth	gGCDispose()
{
	fclose(iFP);
	if (iTempFile  &&  IsObj(iName))
		unlinkTempFile(CLASS, gStringValue(iName));
	return gDispose(super);
}

imeth	object	gDispose, gDeepDispose ()
{
	if (!iFileAlreadyOpened)
		fclose(iFP);
	if (iTempFile)
		unlinkTempFile(CLASS, gStringValue(iName));
	gDispose(iName);
	return gDispose(super);
}

imeth	int	gRead(char *buf, unsigned n)
{
	return fread(buf, 1, n, iFP);
}

imeth	int	gWrite(char *buf, unsigned n)
{
	return fwrite(buf, 1, n, iFP);
}

imeth	char	*gGets(char *buf, int sz)
{
/*	char	*fgets(char *, int, FILE *);  */
	return fgets(buf, sz, iFP);
}

imeth	long	gAdvance(long n)
{
	int	r = fseek(iFP, n, SEEK_CUR);
	return r ? 0L : n;
}

imeth	long	gRetreat(long n)
{
	int	r = fseek(iFP, -n, SEEK_CUR);
	return r ? 0L : n;
}

imeth	long	gSeek(long n)
{
	int	r = fseek(iFP, n, SEEK_SET);
	return r ? 0L : n;
}

imeth	long	gPosition()
{
/*	long	ftell(FILE *);  */
	return ftell(iFP);
}

imeth	long	gLength()
{
#if	!defined(vms)  &&  !defined(__MWERKS__)  &&  !defined(PLAN9)
	struct	stat	sb;
	int	r;

	r = fstat(fileno(iFP), &sb);
	return r ? -1L : sb.st_size;
#else
	long	sav = ftell(iFP);
	long	len;

	fseek(iFP, 0L, SEEK_END);
	len = ftell(iFP);
	fseek(iFP, sav, SEEK_SET);
	return len;
#endif
}

imeth	char	*gName()
{
	return gStringValue(iName);
}

imeth	int	gEndOfStream()
{
	return feof(iFP);
}

imeth	void	*gPointerValue()
{
	return (void *) iFP;
}

cmeth	gSetTempSubDir(char *dir)
{
	strcpy(cTempSubDir, dir);
	return self;
}

imeth	int	gFlush()
{
	return fflush(iFP);
}

cmeth	int	gFlush()
{
	return fflush(NULL);
}

static	void	class_init(void)
{
	String;  /*  must be initialized!  */
	stdoutStream_o = StreamObject(CLASS, "stdout", stdout);
	stderrStream_o = StreamObject(CLASS, "stderr", stderr);
	stdinStream_o  = StreamObject(CLASS, "stdin", stdin);
	traceStream_o  = stdoutStream_o;
	cProcessID = 0L;
}






