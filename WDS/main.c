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


#include "generics.h"
#include "scheme.h"
#include "resource.h"

#include <devices.h>

extern	void	Scheme_init_RTPatch(void);
extern	void	Scheme_init_Version(void);
extern	void	Scheme_init_BAR(void);

static char    volatile  version[] = "###+++ 1.08 +++###";

static	void	usage(void);


int	start()
{
	char	buf[256], *argp;
	int	nargs, arg = 0;

	if ((nargs=gNumArgs(Application)) < 1) {
		usage();
		return 0;
	}
//		gError(Application, "Usage:  WDS  file");
#if 0
	if (Today() >= 20030814L)
		gError(Application, "Program error");
#endif
	while (*(argp = gGetArg(Application, arg)) == '-') {
		arg++;
		nargs--;
		if (!stricmp(argp, "-j"))
			if (nargs < 5) {
				usage();
				return 0;
			} else {
				processJavaArgs(arg);
				return 0;
			}
		if (!stricmp(argp, "-ftp"))
			if (nargs < 5) {
				usage();
				return 0;
			} else {
				object	ftp = gNewFtp(Ftp,
						      gGetArg(Application, arg),
						      gGetArg(Application, arg+1),
						      gGetArg(Application, arg+2));
				if (!ftp)
					gError(Application, "Can't open FTP site.");
				if (strcmp(argp=gGetArg(Application, arg+3), "."))
					if (!gSetDirectory(ftp, argp))
						gError(Application, "Can't change ftp directory.");
				argp = gGetArg(Application, arg+4);
				if (!gFtpGetFile(ftp, argp, argp, 0))
					gError(Application, "Can't FTP file.");
				gExecuteSchemeFile(Scheme, argp);
				gDispose(ftp);
				arg += 5;
				nargs -= 5;
			}
	}
	if (nargs > 0) {
#if 0
		sprintf(buf, "(load \"%s.scm\")", gGetArg(Application, arg));
		gExecuteString(Scheme, buf);
#else
		sprintf(buf, "%s.zo", gGetArg(Application, arg));
		if (access(buf, 0))
			sprintf(buf, "%s.scm", gGetArg(Application, arg));
		gExecuteSchemeFile(Scheme, buf);
#endif
	}
	return 0;
}

void	Scheme_init_app(void)
{
	Scheme_init_ads();
	
	Scheme_init_Version();
	Scheme_init_RTPatch();
	Scheme_init_BAR();
}	

_link_mt_ads()   //  this causes adscm-mt.lib to be linked in
{
	GET_OPN_TYP();
	DL_close();
}

static	int	processJavaArgs(int arg)
{
	int	argc = gNumArgs(Application), debugport = 0, clspathsize = 0;
	char	*classpath = NULL, *libpath, *bootclasspath = NULL, *clspathstart;
	char	*envtemp, *packageName;
	object	jcls;
	BOOL	bErr;
	
	clspathstart = Getenv("CLASSPATH");
	envtemp = gGetArg(Application, arg);
	if (envtemp && *envtemp == '$')
		envtemp = Getenv(envtemp+1);

	if (envtemp && *envtemp != '\0')
		clspathsize += strlen(envtemp) + 2;

	if (clspathstart && *clspathstart)
		clspathsize += strlen(clspathstart) + 2;

	if (clspathsize)
	{
		classpath = (char *)malloc(clspathsize);
		*classpath = '\0';
		if (clspathstart && *clspathstart != '\0')
		{
			strcat(classpath, clspathstart);
			strcat(classpath, ";");
		}
		if (envtemp && *envtemp != '\0')
			strcat(classpath, envtemp);
	}
			
	
	if (!classpath || *classpath == '\0')
	{
		gMessage(Application, Sprintf(NULL, "Invalid java classpath-%s", gGetArg(Application, arg)));
		return 0;
	}

	
	envtemp = gGetArg(Application, arg + 1);
	if (envtemp && *envtemp == '$')
		envtemp = Getenv(envtemp+1);
	libpath = envtemp;
	if (!libpath || *libpath == '\0')
	{
		gMessage(Application, Sprintf(NULL, "Invalid java libpath argument-%s", gGetArg(Application, arg + 1)));
		return 0;
	}

	if (argc > 6)
	{
		gSetJavaDebugSuspend(Java);
		debugport = atoi(gGetArg(Application, arg + 5));
	}

	packageName = gGetArg(Application, arg + 2);
	if (!stricmp(packageName, "null"))
		packageName = NULL;

	gSetJavaInitParameters(Java, classpath, libpath, packageName, NULL);
	if (debugport)
		gSetJavaDebugPort(Java, debugport);

	jcls = gNewWithStr(JavaClass, gGetArg(Application, arg + 3));
	if (!jcls)
	{
		gMessage(Application, gGetLastJavaError(Java));
		return 0;
	}

	vCallJavaMethod(jcls, gGetArg(Application, arg + 4), "()V", &bErr);
	if (bErr)
	{
		gMessage(Application, gGetLastJavaError(Java));
		return 0;
	}
	gDispose(jcls);
	free(classpath);
	return 0;
}

static	void	usage(void)
{
	object	wind = gNewWithStr(MainWindow, "WDS Command Line Arguments");

	gLoadIcon(wind, ALGOCORP_ICON);

	vPrintf(wind, "WDS Usage");

	vPrintf(wind, "\n\nScheme\n------\n");
	vPrintf(wind, "wds  <file>\n");
	vPrintf(wind, "      Scheme file <file>.zo or <file>.scm will be executed\n");

	vPrintf(wind, "\n\nJava\n----\n");
	vPrintf(wind, "wds  -j  <classpath>  <libpath>  <objpkg>  <class>  <methodname>  <debug port>\n");
	vPrintf(wind, "    where:\n");
	vPrintf(wind, "        <classpath> string or $+env variable with java classpath\n");
	vPrintf(wind, "        <libpath> or $+env variable with path to JavaDynace.dll\n");
	vPrintf(wind, "        <objpkg> object class package-package of object class or 'NULL' if none\n");
	vPrintf(wind, "        <class> java class name to load\n");
	vPrintf(wind, "        <methodname> static method to invoke on class\n");
	vPrintf(wind, "        <debug port> ip port to listen for JPDA debugger. application will wait until attached\n\n");
	vPrintf(wind, "        All Paths must use forward slashes only.\n");


						
	vPrintf(wind, "\n\nInternet\n--------\n");
	vPrintf(wind, "wds  -ftp  <site>  <user>  <password>  <directory>  <file>\n");

	gProcessMessages(wind);
}

void	JavaScript_init_base()
{
}

void	JavaScript_init_app()
{
}
