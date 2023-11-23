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



/*  This file automatically generated by dpp - do not edit  */

#define	DPP_STRATEGY	2
#define	DPP_FASTWIDE	0



#line 23 "Logger.d"
#include <stdio.h> 
#include <sys/wait.h> 
#include <sys/prctl.h> 
#include <unistd.h> 
#include <sys/types.h> 
#include <sys/stat.h> 
#include <fcntl.h> 
#include <time.h> 
#include <string.h> 
#include <stdlib.h> 

#define IGNORE_RETURN (void)! 

#define	CLASS	Logger_c
#define	ivType	Logger_iv_t

#include "generics.h"

object	Logger_c;


#line 61 "Logger.c"
typedef struct  _Logger_iv_t  {
	char iLogFileName [ 1024 ];
	int iMode;
	int iLevel;
}	Logger_iv_t;



#line 42 "Logger.d"
PMETHOD void out(object self, char *type, char *sfname, int line, char *msg); 
static int hasNewline(char *s); 

cmeth objrtn Logger_cm_gNew(object self)
{ 
	object obj = oSuper(Logger_c, gNew, self)(self); 
	ivType *iv = ivPtr(obj); 
	*iv->iLogFileName = '\0'; 
	iv->iMode = LOG_MODE_STDERR; 
	iv->iLevel = 0; 
	return obj; 
} 

cmeth objrtn Logger_cm_gNewWithStr(object self, char *lfname)
{ 
	object obj = oSuper(Logger_c, gNew, self)(self); 
	ivType *iv = ivPtr(obj); 
	if (lfname && *lfname) { 
		strcpy(iv->iLogFileName, lfname); 
		iv->iMode = LOG_MODE_NONE; 
	} else { 
		*iv->iLogFileName = '\0'; 
		iv->iMode = LOG_MODE_STDERR; 
	} 
	iv->iLevel = 0; 
	return obj; 
} 

imeth int Logger_im_gSetLogMode(object self, int mode)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	int m = iv->iMode; 
	iv->iMode = mode; 
	return m; 
} 

imeth int Logger_im_gGetLogMode(object self)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	return iv->iMode; 
} 

imeth objrtn Logger_im_gSetLogFileName(object self, char *fname)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	if (fname) 
		strcpy(iv->iLogFileName, fname); 
	else 
		*iv->iLogFileName = '\0'; 
	return self; 
} 

imeth int Logger_im_gSetLogLevel(object self, int level)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	int plevel = iv->iLevel; 
	iv->iLevel = level; 
	return plevel; 
} 

imeth int Logger_im_gGetLogLevel(object self)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	return iv->iLevel; 
} 

imeth void Logger_im_gLoggerMessage(object self, int level, char *sfname, int line, char *msg)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	char *levelStr; 

	if (iv->iLevel < level || iv->iMode == LOG_MODE_NONE) 
		return; 
	switch (level) { 
		case LOG_LEVEL_FATAL: 
		levelStr = "FATAL"; 
		break; 
		case LOG_LEVEL_ERROR: 
		levelStr = "ERROR"; 
		break; 
		case LOG_LEVEL_WARN: 
		levelStr = "WARN"; 
		break; 
		case LOG_LEVEL_INFO: 
		levelStr = "INFO"; 
		break; 
		case LOG_LEVEL_DEBUG: 
		levelStr = "DEBUG"; 
		break; 
		case LOG_LEVEL_ALL: 
		levelStr = "ALL"; 
		break; 
		default: 
		levelStr = ""; 
		break; 
	} 
	out(self, levelStr, sfname, line, msg); 
} 

PMETHOD void out(object self, char *type, char *sfname, int line, char *msg)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	time_t tb; 
	struct tm *ts; 
	int fd; 
	time(&tb); 
	ts = localtime(&tb); 
	char where[50], sline[24], buf[80]; 
	static const char msk[] = "[%-5.5s] [%-40.40s] - %d-%02d-%02d %2d:%02d:%02d  "; 

	sprintf(sline, "%d", line); 
	sprintf(where, "%.36s:%.6s", sfname, sline); 

	fd = gOpenLogFile(self); 
	sprintf(buf, msk, type, where, 1900 + ts->tm_year, 1 + ts->tm_mon, ts->tm_mday, ts->tm_hour, ts->tm_min, ts->tm_sec); 
	IGNORE_RETURN write(fd, buf, strlen(buf)); 
	if (msg) 
		IGNORE_RETURN write(fd, msg, strlen(msg)); 
	if (!hasNewline(msg)) 
		IGNORE_RETURN write(fd, "\n", 1); 

	if (iv->iMode == LOG_MODE_BOTH) { 
		IGNORE_RETURN write(STDOUT_FILENO, buf, strlen(buf)); 
		if (msg) 
			IGNORE_RETURN write(fd, msg, strlen(msg)); 
		if (!hasNewline(msg)) 
			IGNORE_RETURN write(fd, "\n", 1); 
	} 
	gCloseLogFile(self, fd); 
} 

imeth int Logger_im_gOpenLogFile(object self)
{ Logger_iv_t *iv = GetIVs(Logger, self);
	int fd; 
	struct flock lock; 

	if (iv->iMode == LOG_MODE_STDOUT) 
		return STDOUT_FILENO; 
	else if (iv->iMode == LOG_MODE_STDERR) 
		return STDERR_FILENO; 
	fd = open(iv->iLogFileName, O_WRONLY | O_APPEND | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH); 
	if (fd == -1) 
		return fd; 

	lock.l_type = F_WRLCK; 
	lock.l_start = 0; 
	lock.l_whence = SEEK_SET; 
	lock.l_len = 0; 
	lock.l_pid = getpid(); 
	fcntl(fd, F_SETLKW, &lock); 
	return fd; 
} 

PMETHOD void unlock(object self, int fd)
{ 
	struct flock lock; 

	if (fd == STDOUT_FILENO || fd == STDERR_FILENO || fd == -1) 
		return; 
	lock.l_type = F_UNLCK; 
	lock.l_start = 0; 
	lock.l_whence = SEEK_SET; 
	lock.l_len = 0; 
	lock.l_pid = getpid(); 
	fcntl(fd, F_SETLKW, &lock); 
} 

imeth void Logger_im_gCloseLogFile(object self, int fd)
{ 
	if (fd == STDOUT_FILENO || fd == STDERR_FILENO || fd == -1) 
		return; 
	unlock(self, fd); 
	close(fd); 
} 

static int hasNewline(char *s) 
{ 
	if (!s || !*s) 
		return 0; 
	for ( ; *(s+1) ; s++); 
	return *s == '\n'; 
} 

#line 247 "Logger.c"

objrtn	Logger_initialize(void)
{
	static  CRITICALSECTION  cs;
	static  int volatile once = 0;

	ENTERCRITICALSECTION(_CI_CS_);
	if (!once) {
		INITIALIZECRITICALSECTION(cs);
		once = 1;
	}
	LEAVECRITICALSECTION(_CI_CS_);

	ENTERCRITICALSECTION(cs);

	if (Logger_c) {
		LEAVECRITICALSECTION(cs);
		return Logger_c;
	}
	INHIBIT_THREADER;
	Logger_c = gNewClass(Class, "Logger", sizeof(Logger_iv_t), 0, END);
	cMethodFor(Logger, gNewWithStr, Logger_cm_gNewWithStr);
	cMethodFor(Logger, gNew, Logger_cm_gNew);
	iMethodFor(Logger, gCloseLogFile, Logger_im_gCloseLogFile);
	iMethodFor(Logger, gOpenLogFile, Logger_im_gOpenLogFile);
	iMethodFor(Logger, gUnlockLogFile, unlock);
	iMethodFor(Logger, gSetLogFileName, Logger_im_gSetLogFileName);
	iMethodFor(Logger, gGetLogMode, Logger_im_gGetLogMode);
	iMethodFor(Logger, gLoggerMessage, Logger_im_gLoggerMessage);
	iMethodFor(Logger, gGetLogLevel, Logger_im_gGetLogLevel);
	iMethodFor(Logger, gSetLogMode, Logger_im_gSetLogMode);
	iMethodFor(Logger, gSetLogLevel, Logger_im_gSetLogLevel);

	ENABLE_THREADER;

	LEAVECRITICALSECTION(cs);

	return Logger_c;
}


