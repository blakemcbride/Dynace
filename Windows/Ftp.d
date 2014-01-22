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
#include <wininet.h>


defclass  Ftp {
	HINTERNET		ihIntSession;
	HINTERNET		ihFtpSession;
};

private	imeth	initSession(char *server, char *user, char *pw)
{
	ihIntSession = InternetOpen("Dynace", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);

	if (ihIntSession == NULL)
		return gDispose(self);

	ihFtpSession = InternetConnect(ihIntSession, server, 
				       INTERNET_DEFAULT_FTP_PORT, 
				       user,
				       pw,
				       INTERNET_SERVICE_FTP,
				       0, 0);
	if (ihFtpSession == NULL)
		return gDispose(self);

	return self;
}

cmeth	gNewFtp(char *server, char *user, char *pw)
{
	return initSession(gNew(super), server, user, pw);
}

imeth	gDispose, gDeepDispose ()
{
	if (ihFtpSession)
		InternetCloseHandle(ihFtpSession);
	if (ihIntSession)
		InternetCloseHandle(ihIntSession);
	return gDispose(super);
}

imeth	gSetDirectory(char *dir)
{
	return TRUE == FtpSetCurrentDirectory(ihFtpSession, dir) ? self : NULL;
}

imeth	gFtpGetFile(char *ff, char *tf, int ascii_mode)
{
	return TRUE == FtpGetFile(ihFtpSession, ff, tf, FALSE, FILE_ATTRIBUTE_NORMAL,
				  ascii_mode ? FTP_TRANSFER_TYPE_ASCII : FTP_TRANSFER_TYPE_BINARY,
				  (unsigned long) 0) ? self : NULL;
}

imeth	gFtpPutFile(char *ff, char *tf, int ascii_mode)
{
	return TRUE == FtpPutFile(ihFtpSession, ff, tf,
				  ascii_mode ? FTP_TRANSFER_TYPE_ASCII : FTP_TRANSFER_TYPE_BINARY,
				  (unsigned long) 0) ? self : NULL;
}

imeth	gDeleteFile(char *file)
{
	return TRUE == FtpDeleteFile(ihFtpSession, file) ? self : NULL;
}




