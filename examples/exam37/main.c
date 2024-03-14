
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
	server = gSocketConnect(Socket, "some.url.com", 3490, 0);
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










