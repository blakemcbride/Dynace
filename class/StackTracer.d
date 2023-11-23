
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
#include <sys/prctl.h>
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
	gStackDump(gGetTracer(StackTracer), msg);
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

	iErrorMessage = gNewWithStr(String, pszMessage);
	
	sprintf(szText, "// =====================================================\nDateTime: %d-%02d-%02d %2d:%02d\nAppName: %s\n\n", 1900 + ts->tm_year, 1 + ts->tm_mon, ts->tm_mday, ts->tm_hour, ts->tm_min, cAppName);

	if (pszMessage) {
		strcat(szText, pszMessage);
		strcat(szText, "\n\n");
	}
	LOG_INFO(iLogger, szText);
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
	int fh = gOpenLogFile(logger);
	char pid_buf[30];
	sprintf(pid_buf, "%d", getpid());
	char name_buf[512];
	name_buf[readlink("/proc/self/exe", name_buf, 511)]=0;
	prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
	int child_pid = fork();
	if (!child_pid) {
		dup2(fh, 1);
		dup2(fh, 2);
		execl("/usr/bin/gdb", "gdb", "--batch", "-n", "-ex", "thread", "-ex", "bt", name_buf, pid_buf, NULL);
		abort(); /* If gdb failed to start */
	} else {
		waitpid(child_pid, NULL, 0);
		gCloseLogFile(logger, fh);
	}
}
