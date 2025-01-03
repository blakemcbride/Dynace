

/*
  Class to handle application logging.

  iMode 0 = LOG_MODE_NONE   = output goes no where
        1 = LOG_MODE_FILE   = output goes to log file
        2 = LOG_MODE_STDOUT = output goes to stdout
	3 = LOG_MODE_STDERR = output goes to stderr (default)
	4 = LOG_MODE_BOTH   = output goes to log file and stdout

  iLevel 0 = LOG_LEVEL_NONE  = logging off
         1 = LOG_LEVEL_FATAL = fatal errors
	 2 = LOG_LEVEL_ERROR = errors
	 3 = LOG_LEVEL_WARN  = warnings
	 4 = LOG_LEVEL_INFO  = info
	 5 = LOG_LEVEL_DEBUG = debug
	 6 = LOG_LEVEL_ALL   = all

  iLevel setting display things at their level and all lower settings.
*/

#include <stdio.h>
#include <fcntl.h>


#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #define close _close
    #define write _write
    #define open _open
    #define read _read
    #define S_IRUSR _S_IREAD
    #define S_IWUSR _S_IWRITE
    #define S_IRGRP 0
    #define S_IROTH 0
    #define S_IWGRP 0
    #define S_IWOTH 0
    #define STDOUT_FILENO 1
    #define STDERR_FILENO 2
    #define STDIN_FILENO 0
#else
    #include <unistd.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#define IGNORE_RETURN (void)!

defclass Logger {
	char iLogFileName[1024];
	int iMode;
	int iLevel;
};

private imeth	void	out(char *type, char *sfname, int line, char *msg);
static	int	hasNewline(char *s);

cmeth	gNew()
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	*iLogFileName = '\0';
	iMode = LOG_MODE_STDERR;
	iLevel = 0;
	return obj;
}

cmeth	gNewWithStr(char *lfname)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	if (lfname && *lfname) {
		strcpy(iLogFileName, lfname);
		iMode = LOG_MODE_NONE;
	} else {
		*iLogFileName = '\0';
		iMode = LOG_MODE_STDERR;
	}
	iv->iLevel = 0;
	return obj;
}

imeth	int	gSetLogMode(int mode)
{
	int m = iMode;
	iMode = mode;
	return m;
}

imeth	int	gGetLogMode()
{
	return iMode;
}

imeth	gSetLogFileName(char *fname)
{
	if (fname)
		strcpy(iLogFileName, fname);
	else
		*iLogFileName = '\0';
	return self;
}

imeth	int	gSetLogLevel(int level)
{
	int plevel = iLevel;
	iLevel = level;
	return plevel;
}

imeth	int	gGetLogLevel()
{
	return iLevel;
}

imeth	void	gLoggerMessage(int level, char *sfname, int line, char *msg)
{
	char	*levelStr;

	if (iLevel < level || iMode == LOG_MODE_NONE)
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

private imeth	void	out(char *type, char *sfname, int line, char *msg)
{
	time_t tb;
	struct tm *ts;
	int fd;
	time(&tb);
	ts = localtime(&tb);
	char where[50], sline[24], buf[128];
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

	if (iMode == LOG_MODE_BOTH) {
		IGNORE_RETURN write(STDOUT_FILENO, buf, strlen(buf));
		if (msg)
			IGNORE_RETURN write(fd, msg, strlen(msg));
		if (!hasNewline(msg))
			IGNORE_RETURN write(fd, "\n", 1);
	}
	gCloseLogFile(self, fd);
}

imeth int gOpenLogFile()
{
#ifdef _WIN32
    HANDLE hFile;
    DWORD dwDesiredAccess = FILE_APPEND_DATA;
    DWORD dwShareMode = 0;
    DWORD dwCreationDisposition = OPEN_ALWAYS;
    OVERLAPPED overlapped = { 0 };

    if (iMode == LOG_MODE_STDOUT)
        return _fileno(stdout);
    else if (iMode == LOG_MODE_STDERR)
        return _fileno(stderr);

    hFile = CreateFileA(iLogFileName, dwDesiredAccess, dwShareMode, NULL, dwCreationDisposition, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return -1;

    // Setting up the lock - Lock the file exclusively
    overlapped.Offset = 0;
    overlapped.OffsetHigh = 0;
    if (LockFileEx(hFile, LOCKFILE_EXCLUSIVE_LOCK, 0, MAXDWORD, MAXDWORD, &overlapped) == 0) {
        CloseHandle(hFile);
        return -1;
    }

    // Converting the Windows HANDLE to a file descriptor
    int fd = _open_osfhandle((intptr_t)hFile, _O_APPEND);
    if (fd == -1) {
        CloseHandle(hFile);
        return -1;
    }

    return fd;
#else
	int	fd;
	struct flock lock;

	if (iMode == LOG_MODE_STDOUT)
		return STDOUT_FILENO;
	else if (iMode == LOG_MODE_STDERR)
		return STDERR_FILENO;
	fd = open(iLogFileName, O_WRONLY | O_APPEND | O_CREAT, S_IRUSR | S_IWUSR |  S_IRGRP | S_IWGRP |  S_IROTH | S_IWOTH);
	if (fd == -1)
		return fd;
	
	lock.l_type = F_WRLCK;
	lock.l_start = 0;
	lock.l_whence = SEEK_SET;
	lock.l_len = 0;
	lock.l_pid = getpid();
    	fcntl(fd, F_SETLKW, &lock);
	return fd;
#endif
}

imeth	void	gUnlockLogFile : unlock(int fd)
{
#ifdef _WIN32
    HANDLE hFile;
    OVERLAPPED overlapped = { 0 };

    if (fd == _fileno(stdout) || fd == _fileno(stderr) || fd == -1)
        return;

    hFile = (HANDLE)_get_osfhandle(fd);
    if (hFile == INVALID_HANDLE_VALUE)
        return;

    overlapped.Offset = 0;
    overlapped.OffsetHigh = 0;
    UnlockFileEx(hFile, 0, MAXDWORD, MAXDWORD, &overlapped);
#else
	struct flock lock;

	if (fd == STDOUT_FILENO || fd == STDERR_FILENO || fd == -1)
		return;
	lock.l_type = F_UNLCK;
	lock.l_start = 0;
	lock.l_whence = SEEK_SET;
	lock.l_len = 0;
	lock.l_pid = getpid();
	fcntl(fd, F_SETLKW, &lock);
#endif
}

imeth	void	gCloseLogFile(int fd)
{
	if (fd == STDOUT_FILENO || fd == STDERR_FILENO || fd == -1)
		return;
	unlock(self, fd);
	close(fd);
}

static	int	hasNewline(char *s)
{
	if (!s  ||  !*s)
		return 0;
	for ( ; *(s+1) ; s++);
	return *s == '\n';
}


