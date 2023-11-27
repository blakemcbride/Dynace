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




#if	!defined(_MSC_VER)
#define	USE_FCNTL
#endif


#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#include <stdio.h>
#if	defined(_MSC_VER)
#include <winsock.h>
#endif
#include <stdlib.h>
#ifdef __WINE__
#else
#include <errno.h>
#endif
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	!defined(_MSC_VER)
#include <unistd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <arpa/inet.h>
#endif

#ifdef USE_FCNTL
#include <fcntl.h>
#endif


#if	!defined(_MSC_VER)
#define	closesocket     close
#define	ioctlsocket	    ioctl
#define	SOCKET          int
#define	INVALID_SOCKET	-1
#define	SOCKET_ERROR	-1
#define	WSAEINTR	    EINTR
#define	WSAEWOULDBLOCK	EWOULDBLOCK
#define	TRUE		    1
#endif


#define BACKLOG 20            // how many pending connections queue will hold 

#ifdef MY_SSL
#include "ssleay.h"

struct  SSL_SOCKET {
	short int flag;
	union {
		SSL* ssfd;
		SOCKET sfd;
	} SSL_union;
};


typedef struct SSL_SOCKET       G_SOCKET;

#define CLIENT_VERIFY_OPTION    SSL_VERIFY_PEER|SSL_VERIFY_FAIL_IF_NO_PEER_CERT
// other options:               SSL_VERIFY_NONE|SSL_VERIFY_PEER
//                              |SSL_VERIFY_FAIL_IF_NO_PEER_CERT


#define SSL_HANDSHAKE_MAX_WAIT    500        // if SSL initial handshakes exceed this number, 
#define MAX_OF_HANDSHAKES         2400       // the thread will be terminated 

#define CERTF "PEM\\PEM_CLI\\clicert.pem"    // the client's certificate, it is optional
#define KEYF  "PEM\\PEM_CLI\\clikey.pem"     // the path of CERTF

#define CAfile  "PEM\\PEM_CA\\cacert.pem"    // certificate authority's certificate, it is mandatory
#define CApath  ".\\PEM\\PEM_CA"             // the path of CAfile

#define SERVER_AUTH_CLIENT        0          // server authenticates client, i.e., checks client's certificate
                                             // Note: server is required to be always authenticated, 
                                             // but client can be authenticated or not. 

static int s_server_verify=SSL_VERIFY_NONE;
// other options:          SSL_VERIFY_NONE|SSL_VERIFY_PEER
//                        |SSL_VERIFY_FAIL_IF_NO_PEER_CERT

#else
typedef SOCKET G_SOCKET;

typedef int    SSL_CTX;
#endif

defclass  Socket : Stream {
	G_SOCKET    	iFP;

	struct sockaddr_in  iAddress;

	// the following instance variables are used in SSL case only
	int         	iVerifyError;
	SSL_CTX		*iCtx;
	char		*iCipher;        
	int		iServerAuthClient;

	int		iServerSSL;
	long	iTotalBytesRead;

class:
	int         cNumbInstances;
	int         cError;
	char*	    cErrorStr;
};


#ifdef	MY_SSL
static	int init_Server_SSL_CTX(ivType *iv);
static int SSL_Server_Handshake(ivType *iv, SSL* ssl);
static	int verify_callback(int ok, X509_STORE_CTX *ctx);
static int verify_depth=1;
static int verify_error=X509_V_OK;
#endif


#define	t_makeword(a, b)  ((WORD) (((BYTE) (a)) | ((WORD) ((BYTE) (b))) << 8)) 
#define	MAX_WAIT      120	  // Maximunm waiting time in seconds
#define	SMALLBUFLEN	  256
#define	BUFLEN	      2048

static int    initSockets(void);
static int    is_ip_address(char *s);
static int    what_error(void);
static void   sleepInMilliSeconds(unsigned time);

static int    updateErrorStr(char * strBuf);
static object decrementUsage(void);
static int    sufSSLConnect(ivType *iv, SOCKET sockfd, short int flag);


#ifdef MY_SSL
static int    init_Client_SSL_CTX(ivType *iv);
static int    init_SSL(ivType *iv, SSL** pssl, SOCKET sockfd);
static int    SSL_Client_Handshake(ivType* iv, SSL* ssl);
static int    set_cert_stuff(SSL_CTX* ctx, char* cert_file, char* key_file);
static int    verify_Server(ivType* iv, SSL* ssl);

static void   print_stats(SSL_CTX* ssl_ctx);
static void   print_Server_Certificate(SSL* ssl);
// static int verify_callback(int ok, X509_STORE_CTX *ctx);
#endif


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}



///////////////////////////////////////////////////////////////////
// This subroutine implements WSAStartup(), which gives WINSOCK.DLL
// implementstions a chance to do initialization, such as data area
// allocation, on a per user basis.
///////////////////////////////////////////////////////////////////
static int initSockets()
{
#if	defined(_MSC_VER)
	WORD    wVersionRequested;
	WSADATA wsaData;
	int     err;
	char    strBuf[SMALLBUFLEN];

	if (!cNumbInstances) {
		memset(&wsaData,0,sizeof(wsaData));
		wVersionRequested = t_makeword(1, 1); // get the version
	  
		if(WSAStartup(wVersionRequested, &wsaData)!=0) {
			err=WSAGetLastError();
		
			cError = 1;
			sprintf(strBuf, "unable to start WINSOCK (cError) in initSockets(), error code=%d.",err);
			updateErrorStr(strBuf);
			return 0;
		}
		//else {
		//printf("WSA version %d\n", wsaData.wVersion);
		//printf("WSA highest version %d\n", wsaData.wHighVersion);
		//printf("WSA maximum sockets %d\n", wsaData.iMaxSockets);
		//printf("WSA maximum length in Bytes of a datagram %d\n", wsaData.iMaxUdpDg);
		//}
	}
#endif
	return 1;
}


//  Client side socket creation

cmeth gSocketConnect(char *addr, int port, short int flag)
{
	return gProxyConnect(self, addr, port, flag, NULL, 0, NULL, NULL);
}

static	object	errorReturn(object obj, int e, char *msg)
{
	cError = e;
	updateErrorStr(msg);
	return gDispose(obj);
}

cmeth gProxyConnect(char *addr, int port, short int flag, char *proxy_addr, int proxy_port, char *proxy_user, char *proxy_pw)
{
	struct  hostent *he = NULL;
	struct  sockaddr_in their_addr;   // server's address information
	int     isip, isproxy = !!proxy_addr;
	SOCKET	sockfd;
	char    strBuf[SMALLBUFLEN];
	object  obj;
	ivType *iv;

	if (!cNumbInstances  &&  !initSockets())
		return NULL;

	cNumbInstances++;

	isip = is_ip_address(addr);

	if (!isip)
		if ((he=gethostbyname(addr)) == NULL) {  // get the host info 
			cError = 2;
			sprintf(strBuf, "Invalid IP address (cError) in gSocketConnect.");
			updateErrorStr(strBuf);
			return decrementUsage();
		}

	// Create a socket and connect to server using normal socket calls.
	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
		cError = 3;
		sprintf(strBuf, "INVALID_SOCKET (cError=3) in gSocketConnect.");
		updateErrorStr(strBuf);
		return decrementUsage();
	}

	memset(&their_addr, 0, sizeof their_addr);
	their_addr.sin_family = AF_INET;                       // host byte order 
	their_addr.sin_port = htons((unsigned short) (isproxy ? proxy_port : port));    // short, network byte order 
	if (isip  ||  isproxy)
		their_addr.sin_addr.s_addr = inet_addr(isproxy ? proxy_addr : addr);
	else
		their_addr.sin_addr = *((struct in_addr *)he->h_addr);

	if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof(struct sockaddr)) == SOCKET_ERROR) {
		cError = 4;
		sprintf(strBuf, "SOCKET_ERROR (cError=4) in gSocketConnect.");
		updateErrorStr(strBuf);
		closesocket(sockfd);
		return decrementUsage();
	}

#ifdef USE_FCNTL
	{	// set to non-blocking  
		int	flags = fcntl(sockfd, F_GETFL, 0);
		if (flags < 0) {
			cError = 5;
			sprintf(strBuf, "flags<0 (cError=5) in gSocketConnect.");
			updateErrorStr(strBuf);
			closesocket(sockfd);
			return decrementUsage();
		}

		flags |= O_NONBLOCK;
		if (fcntl(sockfd, F_SETFL, flags) < 0) {
			cError = 6;
			sprintf(strBuf, "flags<0 (cError=6) in gSocketConnect.");
			updateErrorStr(strBuf);
			closesocket(sockfd);
			return decrementUsage();
		}
	}
#else
	{	//  set to non-blocking
		ULONG	flg = TRUE;
		if (ioctlsocket(sockfd, FIONBIO, &flg) == SOCKET_ERROR) {
			cError = 7;
			sprintf(strBuf, "SOCKET_ERROR (cError=7) in gSocketConnect.");
			updateErrorStr(strBuf);
			closesocket(sockfd);
			return decrementUsage();
		}
	}
#endif

	obj = gNew(super);
	iv = ivPtr(obj);
	cError = 0;

	if (isproxy) {
		unsigned char	c, buf[81];
		int	i = 0, r;
		unsigned short dport = htons((unsigned short) port);

#ifdef	MY_SSL
		iFP.SSL_union.sfd = sockfd;  //  not SSL yet
#else
		iFP = sockfd;
#endif
		
		if (proxy_user) {
#if 1
			if (4 != gWrite(obj, "\x05\x02\x00\x02", 4))
				return errorReturn(obj, 8, "write error 1");
			if (2 != (r=gRead(obj, (char *)buf, 2)))
				return errorReturn(obj, 8, "read error 1");
			if (buf[1]  &&  buf[1] != 2)
				return errorReturn(obj, 8, "Proxy authentication other than name/password required");
#else
			if (5 != gWrite(obj, "\x05\x03\x00\x01\x02", 5))
				return errorReturn(obj, 8, "write error 1");
			if (2 != (r=gRead(obj, (char *)buf, 2)))
				return errorReturn(obj, 8, "read error 1");
			if (buf[1]  &&  buf[1] != 2  &&  buf[1] != 1)
				return errorReturn(obj, 8, "Proxy authentication other than GSSAPI and name/password required");
			if (buf[1] == 1)
				return errorReturn(obj, 8, "Proxy authentication GSSAPI required");
#endif
			if (buf[1]) {
				buf[0] = 1;
				buf[1] = strlen(proxy_user);
				strcpy((char *)buf+2, proxy_user);
				if (buf[1]+2 != gWrite(obj, (char *)buf, buf[1]+2))
					return errorReturn(obj, 8, "write error 7");
				buf[0] = proxy_pw ? strlen(proxy_pw) : 0;
				if (proxy_pw)
					strcpy((char *)buf+1, proxy_pw);
				if (buf[0]+1 != gWrite(obj, (char *)buf, buf[0]+1))
					return errorReturn(obj, 8, "write error 8");
				if (2 != (r=gRead(obj, (char *)buf, 2)))
					return errorReturn(obj, 8, "read error 8");
				if (buf[1])
					return errorReturn(obj, 8, "Proxy username/password not accepted");
			}
		} else {
			if (3 != gWrite(obj, "\x05\x01\x00", 3))
				return errorReturn(obj, 8, "write error 1");
			if (2 != (r=gRead(obj, (char *)buf, 2)))
				return errorReturn(obj, 8, "read error 1");
			if (buf[1])
				return errorReturn(obj, 8, "Proxy authentication required");
		}

		if (3 != gWrite(obj, "\x05\x01\x00", 3))
			return errorReturn(obj, 8, "write error 2");
		if (isip) {
			unsigned long ad = inet_addr(addr);
			c = '\x01';
			if (1 != gWrite(obj, (char *)&c, 1))
				return errorReturn(obj, 8, "write error 3");
			if (4 != gWrite(obj, (char *) &ad, 4))
				return errorReturn(obj, 8, "write error 4");
		} else {
			buf[0] = '\x03';
			strcpy((char *)buf+2, addr);
			buf[1] = strlen((char *)buf+2);
			if ((int) buf[1] + 2 != gWrite(obj, (char *)buf, (int) buf[1] + 2))
				return errorReturn(obj, 8, "write error 5");
		}
		if (2 != gWrite(obj, (char *) &dport, 2))
			return errorReturn(obj, 8, "write error 6");
		if (4 != gRead(obj, (char *)buf, 4))
			return errorReturn(obj, 8, "read error 2");
		if (buf[1])
			return errorReturn(obj, 8, "general SOCKS server failure");
		if (buf[3] == 1) {
			if (6 != gRead(obj, (char *)buf, 6))
				return errorReturn(obj, 8, "read error 3");
		} else if (buf[3] == 3) {
			if (1 != gRead(obj, (char *)&c, 1))
				return errorReturn(obj, 8, "read error 4");
			if (c+2 != gRead(obj, (char *)buf, c+2))
				return errorReturn(obj, 8, "read error 5");
		} else if (buf[3] == 4) {
			if (18 != gRead(obj, (char *)buf, 18))
				return errorReturn(obj, 8, "read error 6");
		} else
			return errorReturn(obj, 8, "invalid ATYP");
	}

	if (!sufSSLConnect(iv, sockfd, flag))
		return gDispose(obj);

	return obj;
}

static int sufSSLConnect(ivType *iv, SOCKET sockfd, short int flag)
{
#ifdef MY_SSL
	iVerifyError = X509_V_OK;

	iCipher      = NULL;
	iCtx         = NULL;

	iFP.flag = flag;

	if (iFP.flag == 0)
		iFP.SSL_union.sfd = sockfd;
	else {
		if (!init_Client_SSL_CTX(iv)) 
			return 0;
		
		if (!init_SSL(iv, &iFP.SSL_union.ssfd, sockfd))   // ssl get a new value back
			return 0;
			
		if (!SSL_Client_Handshake(iv, iFP.SSL_union.ssfd)) 
			return 0;
	}
#else
	iFP = sockfd;
#endif
	return 1;
}

imeth	gDispose, gDeepDispose, gGCDispose ()
{
	SOCKET sockfd=0;

	// clean up
#ifdef MY_SSL
	if (iFP.flag != 1)
		sockfd = iFP.SSL_union.sfd;
	else {
		if (iFP.SSL_union.ssfd != NULL) {
			sockfd = SSL_get_fd(iFP.SSL_union.ssfd);
			SSL_free(iFP.SSL_union.ssfd);
			iFP.SSL_union.ssfd = NULL;
		}

		if (iCtx) {
			SSL_CTX_free(iCtx);
			iCtx = NULL;
		}
	}
	if (sockfd)
		closesocket(sockfd);
#else
	if (iFP)
		closesocket(iFP);
#endif
	decrementUsage();
	return gDispose(super);
}

imeth	int	gRead(char *buf, unsigned n)
{
	return gTimedRead(self, buf, n, MAX_WAIT);
}

imeth	int	gTimedRead(char *buf, unsigned n, int max_wait)
{
	return gTimedReadMS(self, buf, n, max_wait, 0L);
}

imeth	int	gTimedReadMS(char *buf, unsigned n, int max_wait, long microsec_wait)
{
	unsigned    tot = 0;
	int	        t, e;

	SOCKET      sfd;

#ifdef MY_SSL
	SSL*        ssl=NULL;
	if(iFP.flag==0) {
		sfd=iFP.SSL_union.sfd;
	}
	else {
		ssl = iFP.SSL_union.ssfd;
		sfd=SSL_get_fd(ssl);
	}
#else
	sfd=iFP;
#endif

	while (n) {
#ifdef MY_SSL
		if (iFP.flag)
			t = SSL_read(ssl, buf, n);   // secure socket reading
		else
			t = recv(sfd, buf, n, 0);
#else
		t = recv(sfd, buf, n, 0);
#endif

		if (t < 0)
			if ((e=what_error()) == WSAEINTR)
				t = 0;
			else if (e == WSAEWOULDBLOCK) {
				fd_set	setRead;
				struct timeval timeout;
				FD_ZERO(&setRead);    // initializes the socket set 
				                      // variable setRead to be ready
				FD_SET(sfd, &setRead);       // adds a socket sfd to the set for read
				timeout.tv_sec = max_wait;   // seconds
				timeout.tv_usec = microsec_wait;         // and microseconds
				t = select(sfd+1, &setRead, NULL, NULL, &timeout);
				// the first parameter of function select is not used 
				// for WSA but it is included here just for 
				// compatibility with the BSD sockets select() command.

				// Note: t will be the number of sockets in read set which are ready.
				if (t <= 0)
					return -1;
				t = 0;
			} else
				return t;
		else if (!t)
			break;  //  EOF  
		tot += t;
		buf += t;
		n -= t;
		iTotalBytesRead += t;
	}
	return tot;
}



imeth	int	gWrite(char *buf, unsigned n)
{
	return gTimedWrite(self, buf, n, MAX_WAIT);
}

imeth	int	gWriteInt32(int val)
{
	uint32_t network_val;

	network_val = htonl(val);
	return gWrite(self, (char *)&network_val, sizeof(network_val));
}

imeth	int	gReadInt32(int *val)
{
	uint32_t network_val;

	if (gRead(self, (char *)&network_val, sizeof(network_val)) != sizeof(network_val))
		return 0;
	*val = ntohl(network_val);
	return 1;
}


imeth	int	gTimedWrite(char *buf, unsigned n, int max_wait)
{
	unsigned	tot = 0;
	int	        t, e;
	SOCKET      sfd;

#ifdef MY_SSL
	SSL*        ssl=NULL;
	if(iFP.flag==0) {
		sfd=iFP.SSL_union.sfd;
	}
	else {
		ssl = iFP.SSL_union.ssfd;
		sfd=SSL_get_fd(ssl);
	}
#else
	sfd = iFP;
#endif

	while (n) {
#ifdef MY_SSL
		if (iFP.flag)
			t = SSL_write(ssl, buf, n);  // secure socket writing
		else
			t = send(sfd, buf, n, 0);
#else
		t = send(sfd, buf, n, 0);
#endif

		if (t <= 0)
			if ((e=what_error()) == WSAEINTR)
				t = 0;
			else if (e == WSAEWOULDBLOCK) {
				fd_set	setWrite;
				struct timeval timeout;
				FD_ZERO(&setWrite);
				FD_SET(sfd, &setWrite);
				timeout.tv_sec = max_wait;
				timeout.tv_usec = 0;
				t = select(sfd+1, NULL, &setWrite, NULL, &timeout);
				if (t <= 0)
					return -1;
				t = 0;
			} else
				return t;
		tot += t;
		buf += t;
		n -= t;
	}
	return tot;
}


cmeth	int	gGetErrorCode()
{
	return cError;
}


cmeth	char *	gGetErrorStr()
{
	return cErrorStr;
}



////////////////////////////////////////////////////////
// checks to see if the input string is an IP address
////////////////////////////////////////////////////////
static	int	is_ip_address(char *s)
{
	int	dots = 0;

	for ( ; *s ; s++)
		if (*s == '.')
			dots++;
		else if (!isdigit(*s))
			return 0;
	return dots == 3;
}



#if	(defined(unix) || defined(__APPLE__) || defined(__minix))  &&  !defined(__WINE__)
#define	MODE	""
#else
#define	MODE	"b"
#endif

extern	unsigned long BufCRC(unsigned long crc, char *buf, int size);
extern	unsigned long InitCRC(void);

#define SEND(x)	if (sizeof(x) != gWrite(self, (char *) &x, sizeof(x)))	goto er1
#define RECV(x)	if (sizeof(x) != gRead(self, (char *) &x, sizeof(x)))	goto er1


//////////////////////////////////////////////////////
// return: 0      --- successful
//         others --- failure
//////////////////////////////////////////////////////
imeth int	gSendStr(char *str)
{
	char	buf[80];
	strcpy(buf, str);
	return 80!=gWrite(self, buf, sizeof(buf));
}


//////////////////////////////////////////////////////
// return: 0 --- successful
//         1 --- open file failure
//        -1 --- other failures
//////////////////////////////////////////////////////
imeth	int	gSendFile(char *fromFile, char *toFile)
{
	FILE	*fp = fopen(fromFile, "r" MODE);
	uint32_t size, crc, crc2;
	struct stat sb;
	int	len;
	uint16_t	nlen, code = htons(1);	//  send file code  
	char	buf[BUFLEN];
	
	if (!fp)
		return 1;
	if (fstat(fileno(fp), &sb))
		goto er1;
	size = htonl(sb.st_size);
	crc = InitCRC();

	fseek(fp, 0L, SEEK_SET);
	SEND(code);
	SEND(size);

	if (!toFile)
		toFile = fromFile;
	len = strlen(toFile) + 1;
	nlen = htons((short)len);
	SEND(nlen);
	if (len != gWrite(self, toFile, len))
		goto er1;

	while ((len = fread(buf, 1, BUFLEN, fp)) > 0)
		if (len != gWrite(self, buf, len))
			goto er1;
		else
			crc = BufCRC(crc, buf, len);
	if (ferror(fp))
		goto er1;
	crc = htonl(crc);
	SEND(crc);
	RECV(crc2);
	if (crc != crc2)  /*  both in network byte order */
		goto er1;
	fclose(fp);
	return 0;
er1:
	fclose(fp);
	return -1;
}


//////////////////////////////////////////////////////
// return: 0 --- successful
//         1 --- failure
//////////////////////////////////////////////////////
imeth int gGetStr(char *str)
{
	char	buf[80];
	int	r = gRead(self, buf, 80);
	if (r != 80)
		return 1;
	buf[79] = '\0';
	strcpy(str, buf);
	return 0;
}



///////////////////////////////////////////
// return: 0 --- successful
//         1 --- open file failure
//        -1 --- other failures
///////////////////////////////////////////
imeth	int	gRecvFile(char *toFile)
{
	FILE	*fp = fopen(toFile, "w" MODE);
	uint32_t size, crc, crc2;
	int	len;
	uint16_t	nlen, code;
	char	buf[BUFLEN];
	
	if (!fp)
		return 1;
	crc = InitCRC();

	RECV(code);
	if (code != htons(1))
		goto er1;

	RECV(size);
	size = ntohl(size);

	RECV(nlen);
	len = ntohs(nlen);
	if (len  &&  len != gRead(self, buf, len))
		goto er1;
	
	while (size) {
		unsigned	rlen;

		rlen = size > BUFLEN ? BUFLEN : size;
		len = gRead(self, buf, rlen);
		if (len < 0)
			goto er1;
		crc = BufCRC(crc, buf, len);
		if (len != fwrite(buf, 1, len, fp))
			goto er1;
		size -= len;
	}

	crc = htonl(crc);
	RECV(crc2);
	SEND(crc);
	if (crc != crc2)  /*  both in network byte order */
		goto er1;
	fclose(fp);
	return 0;
er1:
	fclose(fp);
	unlink(toFile);
	return -1;
}

imeth	long	gGetTotalBytesRead()
{
	return iTotalBytesRead;
}

static object decrementUsage()
{
	if (cNumbInstances  &&  !--cNumbInstances)
#if	defined(_MSC_VER)
		WSACancelBlockingCall(), 
		WSACleanup()
#endif
			;
	return NULL;
}


static	int	what_error()
{
#if	!defined(_MSC_VER)
	return errno;
#else
	return WSAGetLastError();
#endif
}



static int updateErrorStr(char * strBuf)
{
	if (cErrorStr) {
		free(cErrorStr);
		cErrorStr = NULL;
	}
	
	cErrorStr = (char *)malloc(strlen(strBuf)+1);
	if (cErrorStr) {
		strcpy(cErrorStr, strBuf);
		return 1;
	} else {
		cError = -9999;
		return 0;
	}
}



//////////////////////////////////////////////////////////////////
// suspends the execution of the current thread for a specified 
// time in seconds. 
//////////////////////////////////////////////////////////////////
static void sleepInMilliSeconds(unsigned time)
{
#if	!defined(_MSC_VER)
	struct timeval  tval;
	tval.tv_sec = time/1000;
	tval.tv_usec = time%1000;
	select(0, NULL, NULL, NULL, &tval);
	// sleep(time); in seconds
#else
	Sleep(time);
#endif
}



imeth char* gGetPeerCertificateIssuerName()
{
	char* retStr=NULL;
	char buf[SMALLBUFLEN];
#ifdef MY_SSL
	X509 *peer=NULL;
	if(iFP.flag == 0) {
		strcpy(buf, "SSL flag off");
		retStr = (char *)malloc(strlen(buf)+1);
		strcpy(retStr, buf);
	}
	else{
		peer=SSL_get_peer_certificate(iFP.SSL_union.ssfd);

		if (peer != NULL) {
			X509_NAME_oneline(X509_get_issuer_name(peer),buf,SMALLBUFLEN);
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr,buf);
			X509_free(peer);

		}
		else {
			strcpy(buf,"No peer certificate");
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr, buf);
		}
	}
#else
	strcpy(buf, "MY_SSL not defined");
	retStr = (char *)malloc(strlen(buf)+1);
	strcpy(retStr, buf);
#endif
	return retStr;
}



imeth char* gGetPeerCertificateSubjectName()
{
	char* retStr=NULL;
	char buf[SMALLBUFLEN];

#ifdef MY_SSL
	X509 *peer=NULL;
	if(iFP.flag == 0) {
		strcpy(buf, "SSL flag off");
		retStr = (char *)malloc(strlen(buf)+1);
		strcpy(retStr, buf);
	}
	else{
		peer=SSL_get_peer_certificate(iFP.SSL_union.ssfd);

		if (peer != NULL) {
			X509_NAME_oneline(X509_get_subject_name(peer),buf,SMALLBUFLEN);
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr,buf);
			X509_free(peer);
		}
		else {
			strcpy(buf, "No peer certificate");
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr, buf);
		}
	}
#else
	strcpy(buf, "MY_SSL not defined");
	retStr = (char *)malloc(strlen(buf)+1);
	strcpy(retStr, buf);
#endif
	return retStr;
}



imeth int gHowManyBitsOfPeerPublicKey()
{
#ifdef MY_SSL
	X509* peer=NULL;
	int   i;
	if(iFP.flag == 0) {
		return -1;
	}
	else{
		peer=SSL_get_peer_certificate(iFP.SSL_union.ssfd);
		if (peer != NULL) {
			i =	EVP_PKEY_bits(X509_get_pubkey(peer));
			X509_free(peer);
			return i;
		}
		else {
			return -1;
		}
	}
#else
	return -1;
#endif
}



imeth char* gGetCipher()
{
	char*       retStr=NULL;
	char        buf[SMALLBUFLEN];
#ifdef MY_SSL
	SSL_CIPHER* c;
	if(iFP.flag == 0) {
		strcpy(buf, "SSL flag off");
		retStr = (char *)malloc(strlen(buf)+1);
		strcpy(retStr, buf);
	}
	else{
		c=SSL_get_current_cipher(iFP.SSL_union.ssfd);
		if (c != NULL) {
			strcpy(buf, SSL_CIPHER_get_version(c));
			strcat(buf, ": ");
			strcat(buf, SSL_CIPHER_get_name(c));
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr, buf);
		}
		else {
			strcpy(buf,"No peer certificate");
			retStr = (char *)malloc(strlen(buf)+1);
			strcpy(retStr, buf);
		}
	}
#else
	strcpy(buf, "MY_SSL not defined");
	retStr = (char *)malloc(strlen(buf)+1);
	strcpy(retStr, buf);
#endif
	return retStr;
}


// I divide the session information into 3 strings so that vPrintf can handle
imeth gGetSessionInfo(char **session1, char **session2, char **session3)
{
	char buf[SMALLBUFLEN];
   

#ifdef MY_SSL
	int i;
	char str[128],*s;
	SSL_SESSION *x;	

	if(iFP.flag == 0) {
		strcpy(buf, "SSL flag off");
		*session1 = (char *)malloc(strlen(buf)+1);
		strcpy(*session1, buf);
		*session2 = NULL;
		*session3 = NULL;
	}
	else {
		x = SSL_get_session(iFP.SSL_union.ssfd);

		if (x != NULL)  {
		
			// session1
			if (x->ssl_version == SSL2_VERSION)
				s="SSLv2";
			else if (x->ssl_version == SSL3_VERSION)
				s="SSLv3";
			else if (x->ssl_version == TLS1_VERSION)
				s="TLSv1";
			else
				s="unknown";
			sprintf(str, "    Protocol:      %s\n", s);
			strcpy(buf, str);

			sprintf(str,"    Cipher:         %s\n",(x->cipher == NULL)?"unknown":x->cipher->name);
			strcat(buf, str);

			strcat(buf,"    Session-ID:  ");
			for (i=0; i<(int)x->session_id_length; i++){
				sprintf(str,"%02X",x->session_id[i]);
				strcat(buf, str);
			}
			strcat(buf,"\n");

			*session1 = (char *)malloc(strlen(buf)+1);
			strcpy(*session1, buf);


			// session2
			strcpy(buf,"    Master-Key: ");
			for (i=0; i<(int)x->master_key_length; i++) {
				sprintf(str,"%02X",x->master_key[i]);
				strcat(buf, str);
			}
			strcat(buf,"\n");

			*session2 = (char *)malloc(strlen(buf)+1);
			strcpy(*session2, buf);
 
			
			// session3
			strcpy(buf, "    Key-Arg:       ");
			if (x->key_arg_length == 0)
				strcat(buf,"None");
			else {
				for (i=0; i<(int)x->key_arg_length; i++) {
					sprintf(str,"%02X",x->key_arg[i]);
					strcat(buf,str);
				}
			}

			strcat(buf, "\n");

			if (x->time != 0L){
				struct tm * tmTime;
				char timeStr[30];
				tmTime = localtime((time_t*)&(x->time));
				strcpy(timeStr, asctime(tmTime));
				timeStr[24]=' ';
				timeStr[25] = 0;
				sprintf(str,"    Start Time:   %s", timeStr);
				strcat(buf,str);
			}
			strcat(buf, "\n");

			if (x->timeout != 0L){
				sprintf(str,"    Timeout:      %d (Sec)", x->timeout);
				strcat(buf, str);
			}

			*session3 = (char *)malloc(strlen(buf)+1);
			strcpy(*session3, buf);
		}
		else {
			strcpy(buf, "Session error");
			*session1 = (char *)malloc(strlen(buf)+1);
			strcpy(*session1, buf);
			*session2 = NULL;
			*session3 = NULL;
		}
	}
#else
	strcpy(buf, "MY_SSL not defined");
	*session1 = (char *)malloc(strlen(buf)+1);
	strcpy(*session1, buf);
	*session2 = NULL;
	*session3 = NULL;
#endif
	return self;
}


/**********************************************************************************
  This subroutine prepares TCP socket for server to receive connections
***********************************************************************************/
static	SOCKET	makeServer(int port)
{
	struct sockaddr_in serv_addr; // Server's address information */
	SOCKET	listen_sd;


	if (!cNumbInstances  &&  !initSockets())
		return -1;

	cNumbInstances++;

	if ((listen_sd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
		return -1;
	memset(&serv_addr, 0, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;         // host byte order 
	serv_addr.sin_port = htons((unsigned short) port);   // short, network byte order 
	serv_addr.sin_addr.s_addr = INADDR_ANY; // automatically fill with server IP 
	if (bind(listen_sd, (struct sockaddr *)&serv_addr, sizeof(struct sockaddr)) == SOCKET_ERROR) {
		closesocket(listen_sd);
		return -1;
	}
	if (listen(listen_sd, BACKLOG) == SOCKET_ERROR) {
		closesocket(listen_sd);
		return -1;
	}
	return listen_sd;
}

/**********************************************************************************
  This subroutine prepares TCP socket for server to receive connections
  and setup the server SSL_CTX
***********************************************************************************/
cmeth	gMakeServer(int port, int ssl)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

#ifdef MY_SSL
	if (iFP.flag = ssl) {
		iServerSSL = ssl;
		if (-1 == init_Server_SSL_CTX(iv))
			return NULL;
	}
	iFP.SSL_union.sfd = makeServer(port);
#else
	iFP = makeServer(port);
#endif
	return obj;
}


private	imeth	newSock(SOCKET newsock, int flag, int ssl, struct sockaddr_in *addr)
{
#ifdef MY_SSL
	iFP.flag = flag;
	iFP.SSL_union.sfd = newsock;
	iServerSSL = ssl;
#else
	iFP = newsock;
#endif
	iAddress = *addr;
	++cNumbInstances;
	return self;
}

imeth	unsigned long	gIPAddress()
{
	return iAddress.sin_addr.s_addr;
}

imeth	char	*gIPAddressStr(char *buf)
{
#if	!defined(_MSC_VER)
	char *addr = inet_ntoa(iAddress.sin_addr);
	strcpy(buf, addr);
#else
	sprintf(buf, "%d.%d.%d.%d",
		(int) iAddress.sin_addr.S_un.S_un_b.s_b1,
		(int) iAddress.sin_addr.S_un.S_un_b.s_b2,
		(int) iAddress.sin_addr.S_un.S_un_b.s_b3,
		(int) iAddress.sin_addr.S_un.S_un_b.s_b4);
#endif
	return buf;
}
		

/*************************************************************************
  Server uses this subroutine to accept the client's connection requirement
*************************************************************************/
imeth	gAccept()
{
	struct sockaddr_in client_addr; // Client's address information 
	int sin_size = sizeof(struct sockaddr_in);
	SOCKET	newsock;
#ifdef	MY_SSL
	SOCKET	sockfd = iFP.SSL_union.sfd;
	int	flag = iFP.flag;
#else
	SOCKET	sockfd = iFP;
	int	flag = 0;
#endif

	newsock = accept(sockfd, (struct sockaddr *)&client_addr, (unsigned int *)&sin_size);
	if (newsock == INVALID_SOCKET) {
#if	defined(_MSC_VER)
		int	e = WSAGetLastError();
#endif
		return NULL;
	}
#ifdef USE_FCNTL
	{	// get socket descriptor status flags
		int	flags = fcntl(newsock, F_GETFL, 0); 
		    
		if (flags < 0) {
			closesocket(newsock);
			return NULL;
		}

		// set to non-blocking mode
		flags |= O_NONBLOCK;
		if (fcntl(newsock, F_SETFL, flags) < 0) {
			closesocket(newsock);
			return NULL;
		}
	}
#else
	{	//  set to non-blocking mode 
		ULONG	flg = TRUE;
		if (ioctlsocket(newsock, FIONBIO, &flg) == SOCKET_ERROR) {
			closesocket(newsock);
			return NULL;
		}
	}
#endif
	// printf ("Connection from %lx, port %x\n\n",
	//   client_addr.sin_addr.s_addr, client_addr.sin_port);

	return newSock(gNew(super CLASS), newsock, flag, iServerSSL, &client_addr);
}

imeth	gServerSocketConnect()
{
#ifdef MY_SSL
	int         tmp;
	SOCKET      sockfd;
	G_SOCKET    gs;
	SSL* ssl = NULL;

	if (iServerSSL) {
		gs.flag = iServerSSL;
		tmp = init_SSL(iv, &ssl, iFP.SSL_union.sfd);  // ssl get a new value back
		if (tmp == -1)
			return NULL;
		tmp = SSL_Server_Handshake(iv, ssl);     
		if (tmp == -1)
			return NULL;
		gs.SSL_union.ssfd = ssl;
		iFP = gs;
	}
#endif
	return self;
}



#ifdef MY_SSL

static int  init_SSL(ivType *iv, SSL** pssl, SOCKET sockfd)
{
	char strBuf[SMALLBUFLEN];
	*pssl = (SSL *)SSL_new(iCtx);

	// printf("init_Client_SSL:\n");
	if(*pssl == NULL) {
		cError = 11;
		sprintf(strBuf, "init_Client_SSL: ssl=NULL (cError=11) in init_SSL");
		updateErrorStr(strBuf);
	  return 0;
	}
		
	SSL_set_fd(*pssl, sockfd);
	SSL_set_connect_state(*pssl);
	return 1;
}

static int set_cert_stuff(SSL_CTX* ctx, char* cert_file, char* key_file)
{
	char strBuf[SMALLBUFLEN];
	if (cert_file != NULL)
	{
		if (SSL_CTX_use_certificate_file(ctx,cert_file,	SSL_FILETYPE_PEM) <= 0)
		{
			cError = 14;
			sprintf(strBuf, "unable to get certificate from '%s' (cError=14) in set_cert_stuff", cert_file);
			updateErrorStr(strBuf);
			return 0;
		}

		if (key_file == NULL) key_file=cert_file;
		if (SSL_CTX_use_PrivateKey_file(ctx,key_file, SSL_FILETYPE_PEM) <= 0)
		{
			cError = 15;
			sprintf(strBuf, "unable to get private key from '%s' (cError=15) in set_cert_stuff", key_file);
			updateErrorStr(strBuf);
			return 0;
		}
		
		// Now we know that a key and cert have been set against
		// the SSL context 
		if (!SSL_CTX_check_private_key(ctx))
		{
			cError = 16;
			sprintf(strBuf, "Private key does not match the certificate public key (cError=16) in set_cert_stuff");
			updateErrorStr(strBuf);
			return 0;
		}
	}
	return 1;
}



static int verify_Server(ivType* iv, SSL* ssl)
{
	char strBuf[SMALLBUFLEN];

	iVerifyError=SSL_get_verify_result(ssl);
	
	switch (iVerifyError) 
	{
	case X509_V_OK:
		return 1;
	case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
		cError = 17;
		sprintf(strBuf, "X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT (cError=17) in verify_Server");
		// X509_NAME_oneline(X509_get_issuer_name(ctx->current_cert),strBuf,SMALLBUFLEN);
		// printf("issuer= %s\n", strBuf);
		break;
	case X509_V_ERR_CERT_NOT_YET_VALID:
		cError = 18;
		sprintf(strBuf, "X509_V_ERR_CERT_NOT_YET_VALID (cError=18) in verify_Server");
		break;
	case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
		cError = 19;
		sprintf(strBuf, "X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD (cError=19) in verify_Server");
		// printf("notBefore=");
		// ASN1_UTCTIME_print(stdout, X509_get_notBefore(ctx->current_cert));
		break;
	case X509_V_ERR_CERT_HAS_EXPIRED:
		cError = 20;
		sprintf(strBuf, "X509_V_ERR_CERT_HAS_EXPIRED (cError=20) in verify_Server");
		break;
	case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
		cError = 21;
		sprintf(strBuf, "X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD (cError=21) in verify_Server");
		// printf("notAfter=");
		// ASN1_UTCTIME_print(stdout, X509_get_notAfter(ctx->current_cert));
		break;
	default:
		cError = 22;
		sprintf(strBuf, "verify server error (cError=22) in verify_Server");
	}
	
	updateErrorStr(strBuf);
	return 0;
}


/********************************
static int verify_callback(int ok, X509_STORE_CTX *ctx)
{
	char buf[SMALLBUFLEN];
	X509 *err_cert;
	int err,depth;
	static int verify_depth=1;
	static int verify_error=X509_V_OK;

	err_cert=X509_STORE_CTX_get_current_cert(ctx);
	err=	X509_STORE_CTX_get_error(ctx);
	depth=	X509_STORE_CTX_get_error_depth(ctx);

	// X509_NAME_oneline(X509_get_subject_name(err_cert),buf,256);
	if (!ok)
		{
		sprintf(buf, "verify error: num= %d : %s\n", err, X509_verify_cert_error_string(err));
		MessageBox(NULL, buf, "verify_callback", MB_OK);

		if (verify_depth >= depth)
			{
			ok=1;
			verify_error=X509_V_OK;
			}
		else
			{
			ok=0;
			verify_error=X509_V_ERR_CERT_CHAIN_TOO_LONG;
			}
		}
	switch (ctx->error)
		{
	case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
		X509_NAME_oneline(X509_get_issuer_name(ctx->current_cert), buf, SMALLBUFLEN);
		MessageBox(NULL, buf, "verify_callback", MB_OK);
		// BIO_printf(bio_err,"issuer= %s\n",buf);
		break;
	case X509_V_ERR_CERT_NOT_YET_VALID:
		MessageBox(NULL, "X509_V_ERR_CERT_NOT_YET_VALID", "verify_callback", MB_OK);
		break;
	case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
		MessageBox(NULL, "X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD", "verify_callback", MB_OK);
		// BIO_printf(bio_err,"notBefore=");
		// ASN1_UTCTIME_print(bio_err,X509_get_notBefore(ctx->current_cert));
		// BIO_printf(bio_err,"\n");
		break;
	case X509_V_ERR_CERT_HAS_EXPIRED:
		MessageBox(NULL, "X509_V_ERR_CERT_HAS_EXPIRED", "verify_callback", MB_OK);
		break;
	case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
		MessageBox(NULL, "X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD", "verify_callback", MB_OK);
		// BIO_printf(bio_err,"notAfter=");
		// ASN1_UTCTIME_print(bio_err,X509_get_notAfter(ctx->current_cert));
		// BIO_printf(bio_err,"\n");
		break;
		}

	return(ok);
	}
********************************/





	
static void print_stats(SSL_CTX* ssl_ctx)
{
	printf("%4ld items in the session cache\n", SSL_CTX_sess_number(ssl_ctx));
	printf("%4d client connects (SSL_connect())\n", SSL_CTX_sess_connect(ssl_ctx));
	printf("%4d client renegotiates (SSL_connect())\n",	SSL_CTX_sess_connect_renegotiate(ssl_ctx));
	printf("%4d client connects that finished\n", SSL_CTX_sess_connect_good(ssl_ctx));
	printf("%4d server accepts (SSL_accept())\n", SSL_CTX_sess_accept(ssl_ctx));
	printf("%4d server renegotiates (SSL_accept())\n", SSL_CTX_sess_accept_renegotiate(ssl_ctx));
	printf("%4d server accepts that finished\n", SSL_CTX_sess_accept_good(ssl_ctx));
	printf("%4d session cache hits\n",SSL_CTX_sess_hits(ssl_ctx));
	printf("%4d session cache misses\n",SSL_CTX_sess_misses(ssl_ctx));
	printf("%4d session cache timeouts\n",SSL_CTX_sess_timeouts(ssl_ctx));
	printf("%4d callback cache hits\n",SSL_CTX_sess_cb_hits(ssl_ctx));
	printf("%4d cache full overflows (%d allowed)\n", SSL_CTX_sess_cache_full(ssl_ctx),
		SSL_CTX_sess_get_cache_size(ssl_ctx));
}



static int init_Client_SSL_CTX(ivType *iv)
{
	int         off  = 0;
	SSL_METHOD* meth = NULL; 
	char        strBuf[SMALLBUFLEN];

	meth = SSLv2_client_method();
	// meth = SSLv3_client_method();
	// meth = TLSv1_method();
	SSLeay_add_ssl_algorithms();
	SSL_load_error_strings();
	
	iCtx = SSL_CTX_new (meth);
	if(!iCtx) {
	  cError = 8;
	  sprintf(strBuf, "iCtx=NULL (cError=8) in init_Client_SSL_CTX.");
	  updateErrorStr(strBuf);
	  return 0;
	}

	// off|=SSL_OP_NO_TLSv1;
	// off|=SSL_OP_NO_SSLv2;
	// off|=SSL_OP_NO_SSLv3;

	SSL_CTX_set_options(iCtx, SSL_OP_ALL|off);

	// printf("server_auth_client = %d\n", iSeverAuthClient);

	if(SERVER_AUTH_CLIENT) {
	  if (!set_cert_stuff(iCtx,  CERTF,  KEYF)) {
		cError = 9;
		sprintf(strBuf, "Client certificate and key not matched (cError=9) in init_Client_SSL_CTX.\n");
		strcat(strBuf, cErrorStr);
	    updateErrorStr(strBuf);
		return 0;
	  }
	}
	 
	if (iCipher != NULL)
		SSL_CTX_set_cipher_list(iCtx, iCipher);
	else
		iCipher=getenv("SSL_CIPHER");

	if (!SSL_CTX_load_verify_locations(iCtx,CAfile,CApath)) 
	{
		cError = 10;
		sprintf(strBuf, "Load CA certificate error (cError=10) in init_Client_SSL_CTX.");
	    updateErrorStr(strBuf);
		return 0;
	}

	// SSL_CTX_set_verify(iCtx, CLIENT_VERIFY_OPTION, verify_callback);
		
	return 1;
}



static int SSL_Client_Handshake(ivType* iv, SSL* ssl)
{
	unsigned long int       tmp;
	char     strBuf[SMALLBUFLEN];
	int      t1;

	// SSL handshake:
	// print_Server_Certificate(ssl);

	tmp=0;
	// printf("ssl state %d %s\n", tmp, SSL_state_string_long(ssl));
	while(SSL_in_init(ssl))
	{
	  // printf("ssl state %d %s\n", tmp, SSL_state_string_long(ssl));
	  if(tmp++ > MAX_OF_HANDSHAKES) {
		cError = 12;
		sprintf(strBuf, "SSL hadshake fails (cError=12): ssl in state %d %s", 
				tmp, SSL_state_string_long(ssl));
	    updateErrorStr(strBuf);
		return 0;	  // SSL hadshake fails
	  }

	  // SSL hadshake continues
	  SSL_connect(ssl);
	  
	  sleepInMilliSeconds(SSL_HANDSHAKE_MAX_WAIT); 
	}

	// print the ssl state
	// printf("\nTried handshakes = %d: %s\n", tmp, SSL_state_string_long(ssl));

	if( !verify_Server(iv, ssl) )
		return 0;

	return 1;
}


	
///////////////////////////////////////////////////////////////////////////////
// This subroutine is called by a client,  which prints server's certificate
// after SSL negotiation. What this subroutine implements are optional and not
// required for data exchange to be successful. 
///////////////////////////////////////////////////////////////////////////////
static void print_Server_Certificate(SSL* ssl)
{
  char     buf[BUFLEN];
  X509*    server_cert;

  // Get the cipher - opt 
  printf ("SSL connection using %s\n", SSL_get_cipher (ssl));

  // Get server's certificate (note: beware of dynamic allocation) - opt 

  server_cert = SSL_get_peer_certificate (ssl);      

  if (server_cert != NULL) {
    printf ("Server certificate:\n");
  
    X509_NAME_oneline (X509_get_subject_name (server_cert),  buf,  BUFLEN);
    printf ("\t subject: %s\n", buf);
    
    X509_NAME_oneline (X509_get_issuer_name (server_cert),  buf,  BUFLEN);
    printf ("\t issuer: %s\n", buf);
    
  // We could do all sorts of certificate verification stuff here before
  // deallocating the certificate. 

    X509_free (server_cert);
  } else
    printf ("Server does not have certificate.\n");
}

static	int init_Server_SSL_CTX(ivType *iv)
{
	int off=0;
		
	SSL_METHOD* meth=NULL; 

	//  SSL preliminaries. We keep the certificate and key with the context.
	SSL_load_error_strings();
	SSLeay_add_ssl_algorithms();

	meth = SSLv2_method();
	// meth = SSLv2_server_method();
	// other methods available
	// SSLv3_method(), TLSv1_method(), SSLv23_method()
	// Note the chosen method should be compatible to following "off" parameter

	off|=SSL_OP_NO_SSLv2;
	off|=SSL_OP_NO_TLSv1;
	
	iCtx = SSL_CTX_new (meth);  
	if (iCtx == NULL)
	{
	  // printf("ctx=NULL\n");
	  return -1;
	}

	SSL_CTX_set_quiet_shutdown(iCtx,1);
	SSL_CTX_set_options(iCtx, off);
	SSL_CTX_sess_set_cache_size(iCtx, 128);
	
	if (!set_cert_stuff(iCtx,  CERTF,  KEYF))
		return (-1);
	// printf("Server certificate and key matched\n");

	if (iCipher != NULL)
		SSL_CTX_set_cipher_list(iCtx, iCipher);
	
	// SSL_CTX_set_tmp_rsa_callback(iCtx,tmp_rsa_cb);

	// printf("server_auth_client = %d\n", iServerAuthClient);
	if(iServerAuthClient) {
	    if (!SSL_CTX_load_verify_locations(iCtx,CAfile,CApath)) 
	    {
		// printf("CAfile in trouble\n");
		return -1;
	    }
	}

	// default CApath  = ./demoCA 
	// default CAfile  = cacert.pem
	// The default environment is defined in ssl/lib/ssleay.cnf 
	// you should use the UNIX version SSLeay for access
	
	if(iServerAuthClient) {
	    SSL_CTX_set_verify(iCtx,s_server_verify,verify_callback);
		SSL_CTX_set_client_CA_list(iCtx, SSL_load_client_CA_file(CAfile));
	}

	return 0;
}

static int SSL_Server_Handshake(ivType *iv, SSL* ssl)
{
	unsigned int        tmp;

	// if(iServerAuthClient)
	//    printf("Verify client certificate ...\n");

	// SSL handshake:
	// init_ssl_connection(ssl);
	tmp=0;
	//printf("ssl state %d: %s\n", tmp, SSL_state_string_long(ssl));
	while(SSL_in_init(ssl))
	{ 
	  //printf("ssl in initial state\n"); 
	  if(tmp++ > MAX_OF_HANDSHAKES) {
		  printf("\nSSL hadshake fails\n"); 
		  printf("ssl state %d: %s\n", tmp, SSL_state_string_long(ssl));
		  return -1;
	  }
	  // SSL hadshake continues
	  SSL_accept(ssl);
	  
	  sleepInMilliSeconds(SSL_HANDSHAKE_MAX_WAIT);
	}
	
	// printf("\nTried handshakes = %d: %s\n", tmp, SSL_state_string_long(ssl));

	if (iServerAuthClient) {
	    verify_error=SSL_get_verify_result(ssl);
	
	    if (verify_error != X509_V_OK)
		{
	 	  // printf("Verify error:%s\n",
		  //	X509_verify_cert_error_string(verify_error));
		return (-1);
		}

	   // print_Client_Certificate(ssl);
	}

	return 0;
}

/*************************************************************************
 This subroutine is called by a server,  which prints client's certificate
 after the server accepts the client and finish SSL negotiation. What this
 subroutine implements are optional and not required for data exchange to be
 successful. 
**************************************************************************/
#if 0
static void print_Client_Certificate(SSL* ssl)
{
  char     buf[SMALL_BUF_LEN];
  X509*    client_cert;
  
  printf ("SSL connection using %s\n", SSL_get_cipher (ssl));
  
  // Get client's certificate (note: beware of dynamic allocation) - opt 

  client_cert = SSL_get_peer_certificate (ssl);
  if (client_cert != NULL) {
    printf ("Client certificate:\n");
    
    X509_NAME_oneline (X509_get_subject_name (client_cert), buf, SMALL_BUF_LEN);
	printf ("\t subject: %s\n", buf);
        
    X509_NAME_oneline (X509_get_issuer_name  (client_cert),  buf,  SMALL_BUF_LEN);
    printf ("\t issuer: %s\n", buf);
        
    // We could do all sorts of certificate verification stuff here before
    //   deallocating the certificate. 
    
    X509_free (client_cert);
  } else
    printf ("Client does not have certificate.\n");
}	
#endif

static	int verify_callback(int ok, X509_STORE_CTX *ctx)
{
	char buf[256];
	X509 *err_cert;
	int err,depth;

	err_cert=X509_STORE_CTX_get_current_cert(ctx);
	err=	X509_STORE_CTX_get_error(ctx);
	depth=	X509_STORE_CTX_get_error_depth(ctx);

	X509_NAME_oneline(X509_get_subject_name(err_cert),buf,256);
	// printf("depth=%d %s\n",depth,buf);
	if (!ok)
	{
		// printf("verify error:num=%d:%s\n",err,
		//	X509_verify_cert_error_string(err));
		if (verify_depth >= depth)
		{
			ok=1;
			verify_error=X509_V_OK;
		}
		else
		{
			ok=0;
			verify_error=X509_V_ERR_CERT_CHAIN_TOO_LONG;
		}
	}
	switch (ctx->error)
	{
	case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
		X509_NAME_oneline(X509_get_issuer_name(ctx->current_cert),buf,256);
		// printf("issuer= %s\n",buf);
		break;
	case X509_V_ERR_CERT_NOT_YET_VALID:
	case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
		// printf("notBefore=");
		// ASN1_UTCTIME_print(stdout,X509_get_notBefore(ctx->current_cert));
		printf("X509_V_ERR_CERT_NOT_YET_VALID\n");
		break;
	case X509_V_ERR_CERT_HAS_EXPIRED:
	case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
		// printf("notAfter=");
		// ASN1_UTCTIME_print(stdout, X509_get_notAfter(ctx->current_cert));
		printf("X509_V_ERR_CERT_HAS_EXPIRED\n");
		break;
	}
	// printf("verify return:%d\n",ok);
	return(ok);
}

#endif






