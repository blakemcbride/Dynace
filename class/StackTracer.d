
/*
  Print a stack trace.  Depends on GDB.
*/ 

defclass StackTracer
{
	object iLogger;
	int iDisplayOn;
	int iLogOn;
	int	(*iFunction)();	//  message function
	object	iErrorMessage;
	object	iStackDumpText;
	int iFake;
	int iShowUsage;
	int iNskip;

class:
	object cTracer;
	char cAppName[255];
	int cOutputMode; // 1=file, 2=stdout, 3=stderr, 4=file and stdout
	
init: init_class;
};

#include <stdio.h>
#include <sys/wait.h>
#ifdef __linux__
#include <sys/prctl.h>
#endif
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#define IGNORE_RETURN (void)!

static	int	command_line_error_handler(char *msg);
static void	print_trace(object logger);

#define	MIN_SIZE	60000
#define	MAX_SIZE	1000000


imeth void gLogMessage(char *pszMessage)
{
	int	h;

	if (!iLogOn)
		return;
	LOG_INFO(iLogger, pszMessage);
}

cmeth void gLogMessage(char *msg)
{
	gLogMessage(cTracer, msg);
}

static	void	init_class()
{
	char *v = getenv("DEBUG_OUTPUT_MODE");

	if (!v || !*v)
		cOutputMode = 1;
	else
		cOutputMode = *v - '0';
	
	cTracer = gNew(StackTracer);

	gSetErrorFunction(Object, command_line_error_handler);
}

imeth	gSetLogger(logger)
{
	iLogger = logger;
	return self;
}

imeth void gSetLogOn(int bOn) 
{ 
	iLogOn = bOn; 
}

imeth void gSetDisplayOn(int bOn) 
{ 
	iDisplayOn = bOn; 
}

imeth int gIsLogOn() 
{ 
	return iLogOn; 
}

imeth int gIsDisplayOn() 
{ 
	return iDisplayOn; 
}

imeth    gGetLogFileName()
{
	return gNewWithStr(String, "");
}

cmeth   gGetTracer()
{
	return cTracer;
}

cmeth   gNewWithLogger(logger)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iDisplayOn = 0;
	iLogOn = 1;
	
	iNskip = 4;
	iShowUsage = 1;  //  turn on WDS tracking for headless detection

	iLogger = logger;

	return obj;
}

imeth    gDispose, gDeepDispose()
{
	if (IsObj((object)iFunction))
		gDispose((object)iFunction);
	if (IsObj(iErrorMessage))
		gDispose(iErrorMessage);
	if (IsObj(iStackDumpText))
		gDispose(iStackDumpText);
	return gDispose(super);
}

static	int	command_line_error_handler(char *msg)
{
	static	int	busy = 0;

	if (busy) {	//  error while dumping - don't recurse
		if (msg) {
			IGNORE_RETURN write(STDERR_FILENO, msg, strlen(msg));
			IGNORE_RETURN write(STDERR_FILENO, "\n", 1);
		}
		return 0;
	}
	busy = 1;
	gStackDump(gGetTracer(StackTracer), msg);
	busy = 0;
	return 0;
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	ret = (ofun) iFunction;
	iFunction = fun;
	return ret;
}

imeth    gGetStackDumpText()
{
	return gNewWithStr(String, "Method not available.");
}

imeth	char *gGetStackDumpString()
{
	if (IsObj(iStackDumpText))
		return gStringValue(iStackDumpText);
	return "";
}

imeth	char *gGetErrorMessage()
{
	if (IsObj(iErrorMessage))
		return gStringValue(iErrorMessage);
	return "";
}

imeth void gStackDump(char *pszMessage)
{
	char szText[64000];
	int h;
	time_t tb;
	struct tm *ts;
	time(&tb);
	ts = localtime(&tb);

	if (IsObj(iErrorMessage))
		gDispose(iErrorMessage);
	iErrorMessage = gNewWithStr(String, pszMessage);
	
	snprintf(szText, sizeof(szText), "// =====================================================\nDateTime: %d-%02d-%02d %2d:%02d\nAppName: %s\n\n", 1900 + ts->tm_year, 1 + ts->tm_mon, ts->tm_mday, ts->tm_hour, ts->tm_min, cAppName);

	if (pszMessage) {
		size_t	len = strlen(szText);
		snprintf(szText + len, sizeof(szText) - len, "%s\n\n", pszMessage);
	}
	if (iLogger) {
		LOG_INFO(iLogger, szText);
	} else
		IGNORE_RETURN write(STDERR_FILENO, szText, strlen(szText));
	print_trace(iLogger);
}

cmeth void gStackDump(char *pszMessage)
{
	gStackDump(cTracer, pszMessage);
}

private imeth void pShowUsage(int flg)
{
	iShowUsage = flg;
}

cmeth void gShowUsage(int flg)
{
	pShowUsage(cTracer, flg);
}

private imeth void pTraceCall()
{
	if (iShowUsage) {
		int nskip = iNskip;
		iNskip = 5;
		gStackDump(cTracer, "TRACE");
		iNskip = nskip;
	}
}

cmeth void gTraceCall()
{
	pTraceCall(cTracer);
}

static void print_trace(object logger)
{
	int fh = logger ? gOpenLogFile(logger) : -1;
	if (fh < 0)
		fh = STDERR_FILENO;
	char pid_buf[30];
	sprintf(pid_buf, "%d", getpid());
	char name_buf[512];
	ssize_t name_len = readlink("/proc/self/exe", name_buf, sizeof(name_buf)-1);
	if (name_len < 0)
		name_len = 0;
	name_buf[name_len] = 0;
#ifdef __linux__
	prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
#endif
	int child_pid = fork();
	if (!child_pid) {
		dup2(fh, 1);
		dup2(fh, 2);
		execl("/usr/bin/gdb", "gdb", "--batch", "-n", "-ex", "thread", "-ex", "bt", name_buf, pid_buf, NULL);
		_exit(127); /* If gdb failed to start */
	}
	if (child_pid > 0)
		waitpid(child_pid, NULL, 0);
	if (logger  &&  fh != STDERR_FILENO)
		gCloseLogFile(logger, fh);
}
