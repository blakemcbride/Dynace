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


#include <stdio.h>
#include <process.h>
#include <string.h>
#include <io.h>

static	char	volatile  version[] = "###+++ 1.00 +++###";

main(int argc, char *argv[])
{
	char	exe[128], uex[128];
	int	n;
	
	if (argc <= 1)
		return 1;
	n = strlen(argv[1]);
	if (n > 4  &&  !stricmp(argv[1]+n-4, ".exe")) {
		strcpy(exe, argv[1]);
		strcpy(uex, argv[1]);
		uex[n-4] = '\0';
		strcat(uex, ".uex");
	} else {
		strcpy(exe, argv[1]);
		strcat(exe, ".exe");
		strcpy(uex, argv[1]);
		strcat(uex, ".uex");
	}
	if (!_access(uex, 4)) {
		_unlink(exe);
		rename(uex, exe);
	}
	_execv(argv[1], argv+1);
	return 2;
}
