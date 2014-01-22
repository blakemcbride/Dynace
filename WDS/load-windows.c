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



#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <string.h>
#include <io.h>
#include <ctype.h>

static	char	volatile  version[] = "###+++ 1.00 +++###";


static	int	processCmdLine(char *cCmdLine, char *argv[]);


int	WINAPI	WinMain(HINSTANCE hInstance, 
			HINSTANCE hPrevInstance, 
			LPSTR	  lpszCmdLine, 
			int	  cmdShow)
{
	char	exe[128], uex[128];
	int	n, argc=0;
	char	*argv[40];
	
	argc = processCmdLine(lpszCmdLine, argv);
	if (argc <= 0)
		return 1;
	n = strlen(argv[0]);
	if (n > 4  &&  !stricmp(argv[0]+n-4, ".exe")) {
		strcpy(exe, argv[0]);
		strcpy(uex, argv[0]);
		uex[n-4] = '\0';
		strcat(uex, ".uex");
	} else {
		strcpy(exe, argv[0]);
		strcat(exe, ".exe");
		strcpy(uex, argv[0]);
		strcat(uex, ".uex");
	}
	if (!_access(uex, 4)) {
		_unlink(exe);
		rename(uex, exe);
	}
#if 0
	{
		int	i;
		char	buf[132];
		for (i=0 ; i < argc ; i++ ) {
			sprintf(buf, "argv[%d] = \"%s\"", i, argv[i]);
			MessageBox((HWND) 0, buf, "Message Window", MB_OK | MB_TASKMODAL);  
		}
	}
#endif
	_execv(argv[0], argv);
	return 2;
}

static	void	add_quotes(char *v)
{
	int	i;

	for (i=0 ; v[i]  &&  v[i] != ' ' ; i++);
	if (v[i]) {
		i = strlen(v);
		memcpy(v+1, v, i);
		v[0] = v[i+1] = '"';
		v[i+2] = '\0';
	}
}

static	int	processCmdLine(char *cCmdLine, char *argv[])
{
	char	hold[256];
	char	*w = hold;
	char	*p = cCmdLine;
	int	cNumArgs = 0;

	if (!p  ||  !strlen(p))
		return cNumArgs;

	for ( ; isspace(*p) ; ++p);
	while (*p) {
		if (*p == '"'  ||  *p == '\'')  {
			char	type = *p++;
			while (*p  &&  *p != type)  {
				if (*p == '\\'  &&  p[1])
					p++;
				*w++ = *p++;
			}
			if (*p)
				++p;
		} else if (*p  &&  isspace(*p)) {
			*w = '\0';
			if (w != hold) {
				argv[cNumArgs] = malloc(strlen(hold)+3);
				strcpy(argv[cNumArgs], w=hold);
				add_quotes(argv[cNumArgs++]);
			}
			for ( ; isspace(*p) ; ++p);
		} else
			while (*p  &&  !isspace(*p)) {
				if (*p == '\\'  &&  p[1])
					p++;
				*w++ = *p++;
			}
	}
	*w = '\0';
	if (w != hold) {
		argv[cNumArgs] = malloc(strlen(hold)+3);
		strcpy(argv[cNumArgs], hold);
		add_quotes(argv[cNumArgs++]);
	}
	argv[cNumArgs] = NULL;
	return cNumArgs;
}
