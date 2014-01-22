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

#define MAXLINE		256
#define	MINUTE		60L
#define	HOUR		(60L * 60L)
#define	DAY		(60L * 60L * 24L)

main(argc, argv)
int	argc;
char	*argv[];
{
	long	b, e, d, time();
	char	c[MAXLINE];
	int	i, day, hour, minute, second;
	
	if (argc == 1)  {
		fprintf(stderr, "usage:  timec  command  arg1 arg2 ...\n");
		exit(-1);
	}
	*c = '\0';
	for (i=1 ; i < argc ; )  {
		strcat(c, argv[i++]);
		strcat(c, " ");
	}
	time(&b);
	system(c);
	time(&e);
	d = e - b;
	day = d / DAY;
	d -= day * DAY;
	hour = d / HOUR;
	d -= hour * HOUR;
	minute = d / MINUTE;
	d -= minute * MINUTE;
	second = d;
	printf("Command = %s\n", c);
	printf("days = %d,  hours = %d, minutes = %d, seconds = %d\n",
		day, hour, minute, second);
	exit(0);
}


