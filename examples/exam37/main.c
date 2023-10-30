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


int main(int argc, char *argv[])
{
	object	server;
	char	buf[16];
	int	n;

	InitDynace(&argc);

#if 1
//	server = gSocketConnect(Socket, "192.168.202.3", 3490, 0);
	server = gSocketConnect(Socket, "192.168.202.67", 3490, 0);
#else
	server = gSocketConnect(Socket, "blake.florida-software.com", 3490, 0);
#endif
	if (!server) {
		fprintf(stderr, "Couldn't connect\n");
		exit(1);
	}
#if 0
	for (n=1 ; n > 0 ; ) {
		n = gRead(server, buf, sizeof(buf)-1);
//		n = gWrite(server, "Hello, World!\n", 15);

		if (n < 0)
			fprintf(stderr, "read error\n");
		else if (!n)
			fprintf(stderr, "nothing read\n");
		else
			printf("Received:  %s", buf);
	}
#endif
	n = gSendFile(server, "main.c", "main.c");
	if (n)
		printf("Transmission failed\n");
	else
		printf("Transmission succeeded\n");
	gDispose(server);
	
	return 0;
}










