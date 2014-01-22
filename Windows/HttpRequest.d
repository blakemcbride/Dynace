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



#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif


#define _WIN32_WINNT 0x0400  //required for wincrypt.h include


#include <windows.h>
#include <wininet.h>
#include <wincrypt.h>

#define INTERNET_OPTION_CLIENT_CERT_CONTEXT      84

defclass HttpRequest {
	iUrl;

	char		*iResponseStr;
	unsigned	iResponseStrLen;
	unsigned	iResponseStrUsed;

	iHeaders;
	iContent;
	iVerb;
	iUser;
	iPass;
	iOutputFileName;
	iErrorMessage;
	iResponseHeaders;
	int	iSecure;
	DWORD	iError;
	HANDLE	iOpenRequest;
	HANDLE	iInternetOpen;
	HANDLE	iInternetConnect;
	int	iTimeoutSeconds;
	void	*iCertificate;
	iInternetOpenOptions;
	int iResponseSize;
};



private	imeth	initInstance(object self, char *url, char *verb);
private	imeth	DWORD	pGetErrorInfo(object self, int line, char *errbuf, int bufsize);
private	imeth	void	pCloseHandles(object self);
private	imeth	void	pResetHandle(object self, HINTERNET *handle);

cmeth	gNewHttpRequest(char *url, char *verb)
{
	if (!url || !verb)
		return NULL;
	return initInstance(gNew(super), url, verb);
}

private	imeth	initInstance(object self, char *url, char *verb)
{
	iVerb = gNewWithStr(String, verb);
	iUrl = gNewWithStr(String, url);
	iHeaders = gNew(LinkObject);
	iInternetOpenOptions = gNew(IntegerDictionary);
	iResponseSize = 0;
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iResponseStr)
		free(iResponseStr);
	if (iErrorMessage)
		gDispose(iErrorMessage);
	if (iUser)
		gDispose(iUser);
	if (iPass)
		gDispose(iPass);
	if (iVerb)
		gDispose(iVerb);
	if (iContent)
		gDispose(iContent);
	if (iUrl)
		gDispose(iUrl);
	if (iHeaders)
		gDeepDispose(iHeaders);
	if (iResponseHeaders)
		gDeepDispose(iResponseHeaders);
	if (iOutputFileName)
		gDispose(iOutputFileName);
	if (iInternetOpenOptions)
		gDeepDispose(iInternetOpenOptions);
	pCloseHandles(self);
	return gDispose(super);
}

imeth	gSetSecure(int secure)
{
	iSecure = secure;
	return self;
}

imeth	gSetOutputFileName(char *name) 
{
	if (iOutputFileName)
		gDispose(iOutputFileName);
	if (!name)
		return self;
	iOutputFileName = gNewWithStr(String, name);
	return self;
}

imeth	gAddHeader(char *name, char *value)
{
	if (!name || !value)
		return NULL;
	gAddLast(iHeaders, vBuild(String, name, ": ", value, END));
	return self;
}

imeth void gInternetSetOption(int option, char *buffer, int buflen) 
{
	object bufferObj = gNew(String);
	if (buflen <= 0)
		buflen = strlen(buffer);
	gWrite(bufferObj, buffer, buflen);
	gAddInt(iInternetOpenOptions, option, bufferObj);
}

imeth	void	gSetCertificate(void *cert)
{
	iCertificate = cert;
}

imeth	gSetContent(char *content)
{
	if (iContent)
		iContent = gDispose(iContent);
	if (!content)
		return NULL;
	iContent = gNewWithStr(String, content);
	return self;
}

imeth	gSetContentFromFile(char *filename)
{
	FILE	*fbuf;
	char	buf[256];
	int		readCnt;
	if (iContent)
		iContent = gDispose(iContent);
	if (!filename)
		return NULL;
	fbuf = fopen(filename, "rb");
	if (fbuf == NULL)
		return NULL;
	iContent = gNewWithStr(String, "");
	while (!feof(fbuf)) {
		readCnt = fread(buf, 1, sizeof(buf) - 1, fbuf);
		if (readCnt > 0) {
			buf[readCnt] = '\0';
			gAppend(iContent, (object)buf);
		}
	}
	
	fclose(fbuf);
	return self;
}

imeth	gSetAuth(char *user, char *pass) 
{
	if (!user || !pass)
		return NULL;
	
	if (iUser)
		iUser = gDispose(iUser);
	iUser = gNewWithStr(String, user);
	
	if (iPass)
		iPass = gDispose(iPass);
	iPass = gNewWithStr(String, pass);
	return self;
}

cmeth	int	gPostAndSaveResponseAsFile(char *url, char *contentType, char *content, int flags, char *outputFileName)
{
	object req = gNewHttpRequest(HttpRequest, url, "POST");
	int	res;
	if (content) {
		gSetContent(req, content);
		gAddHeader(req, "Content-Type", contentType ? contentType : "text/plain");
	}
	gSetOutputFileName(req, outputFileName);
	res = gSendRequest(req, flags);
	gDispose(req);
	return res;
}

imeth	DWORD	gSendRequest(DWORD connFlags)
{
	char			*buf;
	DWORD			dwBytesAvail, dwBytesRead, bufLen, ret;
	object			timeobj, headerBuff, seq, header;
	FILE	*outputFile = NULL;
	URL_COMPONENTS	urlcomp;
	char hostName[128] = {'\0'}, urlPath[1024] = {'\0'}, urlExtra[512] = {'\0'};
	object optionobj;
	int success, maxbufsize;
	
	iResponseSize = 0;
	maxbufsize = 1024;
	buf = malloc(maxbufsize);

	pResetHandle(self, &iOpenRequest);
	if (!iInternetOpen) {
		iInternetOpen = InternetOpen("DestinyLOS", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
		if (!iInternetOpen) {
			ret = pGetErrorInfo(self, __LINE__, buf, maxbufsize);
			free(buf);
			return ret;
		}
	
		success = 1;
		for (seq=gSequence(iInternetOpenOptions) ; optionobj = gNext(seq) ; ) {
			int option;
			object value;
			if (!success)
				continue;
			option = gIntKey(optionobj);
			value = gValue(optionobj);
			success = (int)InternetSetOption(iInternetOpen, option, 
							 (LPVOID)gStringValue(value), gSize(value));
		}
	
		if (!success) {
			ret = pGetErrorInfo(self, __LINE__, buf, maxbufsize);
			free(buf);
			pResetHandle(self, &iInternetOpen);
			return ret;
		}
	}	
	memset(&urlcomp, 0, sizeof(urlcomp));
	urlcomp.dwStructSize = sizeof(URL_COMPONENTS);
	urlcomp.nScheme = INTERNET_SCHEME_HTTP;
	urlcomp.lpszHostName = hostName;
	urlcomp.dwHostNameLength = sizeof(hostName);
	urlcomp.lpszUrlPath = urlPath;
	urlcomp.dwUrlPathLength = sizeof(urlPath);
	urlcomp.lpszExtraInfo = urlExtra;
	urlcomp.dwExtraInfoLength = sizeof(urlExtra);

	if (!InternetCrackUrl(gStringValue(iUrl), gSize(iUrl), 0, &urlcomp)) {
		if (iErrorMessage)
			iErrorMessage = gDispose(iErrorMessage);
		iErrorMessage = gNewWithStr(String, "Web URL Format Invalid");
		free(buf);
		return -1;
	}
	if (urlcomp.nPort == 0)
		if (iSecure)
			urlcomp.nPort = 443;
		else
			urlcomp.nPort = 80;

	if (!iInternetConnect) {
		iInternetConnect = InternetConnect(iInternetOpen,				// WinInet internet handle
						   urlcomp.lpszHostName,			// HostName
						   urlcomp.nPort,				// ServerPort
						   (!iUser) ? NULL : gStringValue(iUser),
						   (!iPass) ? NULL : gStringValue(iPass),	
						   INTERNET_SERVICE_HTTP,			// Service
						   0,						// FTP passive flag
						   0);						// Context

		if (!iInternetConnect) {
			ret = pGetErrorInfo(self, __LINE__, buf, maxbufsize);
			free(buf);
			return ret;
		}
	}
	strcat(urlPath, urlExtra);
	if (iSecure)
		connFlags |= INTERNET_FLAG_SECURE;
	
	iOpenRequest = HttpOpenRequest(iInternetConnect,						// WinInet connection handle
							gStringValue(iVerb),				// Verb
							urlPath,					// Object
							NULL,						// Version
							"",						// Referer
							NULL,						// Accept Types
							connFlags,					// Flags
							0);						// Context

	if (!iOpenRequest) {
		ret = pGetErrorInfo(self, __LINE__, buf, maxbufsize);
		free(buf);
		return ret;
	}
	
	headerBuff = gNewWithStr(String, "");
	for (seq=gSequence(iHeaders) ; header = gNext(seq) ; ) {
		gAppend(headerBuff, header);
		gAppend(headerBuff, (object)"\r\n");
	}
	if (iContent) {
		char clheader[64];
		sprintf(clheader, "Content-Length: %d\r\n", gSize(iContent));
		gAppend(headerBuff, (object)clheader);
	}
	gAppend(headerBuff, (object)"\r\n");

	if (!HttpSendRequest(iOpenRequest, gStringValue(headerBuff), -1L, 
		(LPVOID)(iContent ? gStringValue(iContent) : NULL), iContent ? gSize(iContent) : 0)) {
		int sendRet;
		success = 0;
		sendRet = GetLastError();
		if (sendRet == 12044 && iCertificate) {
			InternetSetOption(iOpenRequest, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                  iCertificate, sizeof(CERT_CONTEXT));
			if (HttpSendRequest(iOpenRequest, gStringValue(headerBuff), -1L, 
					(LPVOID)(iContent ? gStringValue(iContent) : NULL), 
					iContent ? gSize(iContent) : 0)) {
				success = 1;
				sendRet = 0;
			} else {
				sendRet = GetLastError();
				success = 0;
			}
		}
		if (sendRet == 12045) {
			DWORD dwFlags;
			DWORD dwBuffLen = sizeof(dwFlags);

			InternetQueryOption(iOpenRequest, INTERNET_OPTION_SECURITY_FLAGS,
				(LPVOID)&dwFlags, &dwBuffLen);
			dwFlags |= SECURITY_FLAG_IGNORE_UNKNOWN_CA;
			InternetSetOption(iOpenRequest, INTERNET_OPTION_SECURITY_FLAGS,
				&dwFlags, sizeof(dwFlags));
			if (HttpSendRequest(iOpenRequest, gStringValue(headerBuff), -1L, 
					(LPVOID)(iContent ? gStringValue(iContent) : NULL), 
					iContent ? gSize(iContent) : 0))
				success = 1;
		}
		if (!success) {
			gDispose(headerBuff);
			ret = pGetErrorInfo(self, __LINE__, buf, maxbufsize);
			free(buf);
			return ret;
		}
	}
	gDispose(headerBuff);

	if (iContent)
		iContent = gDispose(iContent);
	
	timeobj = gNow(Time);
	if (!iTimeoutSeconds)
		gAddSeconds(timeobj, 60L);
	else
		gAddSeconds(timeobj, iTimeoutSeconds);
	
	iResponseStrUsed = 0;
	if (!iResponseStrLen)
		iResponseStr = malloc(iResponseStrLen = 1000);
	
	InternetQueryDataAvailable(iOpenRequest, &dwBytesAvail, 0, 0);
	// wait for response loop
	while (!dwBytesAvail) {
		Sleep(2000);
		InternetQueryDataAvailable(iOpenRequest, &dwBytesAvail, 0, 0);
		if (gLongValue(gNow(Time)) > gLongValue(timeobj)) {
			gDispose(timeobj);
			if (iErrorMessage)
				iErrorMessage = gDispose(iErrorMessage);
			iErrorMessage = gNewWithStr(String, "Timeout waiting for response");
			free(buf);
			return -1;
		}
	}
	gDispose(timeobj);

	if (iOutputFileName) {
		outputFile = fopen(gStringValue(iOutputFileName), "wb");
		if (!outputFile) {
			iErrorMessage = vBuild(String, "Unable to open output file - ", gStringValue(iOutputFileName), END);
			free(buf);
			return -1;
		}
	}
	while (dwBytesAvail) {
		if ((int)dwBytesAvail > maxbufsize) {
			maxbufsize = dwBytesAvail;
			buf = realloc(buf, maxbufsize);
		}
		InternetReadFile(iOpenRequest, (LPVOID)buf, maxbufsize, &dwBytesRead);
		if (outputFile)
			fwrite(buf, 1, dwBytesRead, outputFile);
		else {
			iResponseSize += dwBytesRead;
			if (dwBytesRead + iResponseStrUsed + 1 >= iResponseStrLen) {
				while (dwBytesRead + iResponseStrUsed + 1 >= iResponseStrLen)
					iResponseStrLen *= 1.5;
				iResponseStr = realloc(iResponseStr, iResponseStrLen);
			}
			memcpy(iResponseStr+iResponseStrUsed, buf, dwBytesRead);
			iResponseStrUsed += dwBytesRead;
			iResponseStr[iResponseStrUsed] = '\0';
		}
		InternetQueryDataAvailable(iOpenRequest, &dwBytesAvail, 0, 0);
	}
	if (outputFile)
		fclose(outputFile);

	bufLen = maxbufsize;
	if (!HttpQueryInfo(iOpenRequest, HTTP_QUERY_STATUS_CODE, buf, &bufLen, NULL))
     		if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
			// realloc buf to correct size and call again
			buf = realloc(buf, bufLen);
			maxbufsize = bufLen;
			HttpQueryInfo(iOpenRequest, HTTP_QUERY_STATUS_CODE, buf, &bufLen, NULL);
		}

	if (*buf != '2' && *buf != '1') {
		int respCode = atoi(buf);
		bufLen = maxbufsize;
		if (!HttpQueryInfo(iOpenRequest, HTTP_QUERY_STATUS_TEXT, buf, &bufLen, NULL)) {
			if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
				// realloc buf to correct size and call again
				buf = realloc(buf, bufLen);
				maxbufsize = bufLen;
				HttpQueryInfo(iOpenRequest, HTTP_QUERY_STATUS_TEXT, buf, &bufLen, NULL);
			}
		}
		if (iErrorMessage)
			iErrorMessage = gDispose(iErrorMessage);
		iErrorMessage = vBuild(String, "HTTP Error Response - ", buf, END);
		free(buf);
		return respCode;
	}

	bufLen = maxbufsize;
	if (!HttpQueryInfo(iOpenRequest, HTTP_QUERY_RAW_HEADERS, buf, &bufLen, NULL))
        	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
			// realloc buf to correct size and call again
            		buf = realloc(buf, bufLen);
			maxbufsize = bufLen;
			HttpQueryInfo(iOpenRequest, HTTP_QUERY_RAW_HEADERS, buf, &bufLen, NULL);
		}

	if (bufLen) {
		char *bufPos = buf;
		if (iResponseHeaders)
			iResponseHeaders = gDeepDispose(iResponseHeaders);
		iResponseHeaders = gNew(StringDictionary);
		while (*bufPos != '\0') {
			int curBufLen = strlen(bufPos) + 1;
			char *nameEndPos = strchr(bufPos, ':');
			if (nameEndPos) {
				char *value = nameEndPos + 1;
				*nameEndPos = '\0';
				while (*value == ' ' && *value != '\0')
					value++;
				gAddStr(iResponseHeaders, strlwr(bufPos), gNewWithStr(String, value));
			}
			bufPos += curBufLen;
		}
	}

	free(buf);

	return 0;
}

imeth	char	*gGetErrorMessage()
{
	if (!iErrorMessage)
		return NULL;
	return gStringValue(iErrorMessage);
}

imeth	char	*gGetResponse()
{
	return iResponseStr;
}

private	imeth	DWORD	pGetErrorInfo(object self, int line, char *errbuf, int bufsize)
{
	int	len;
	DWORD errcode = GetLastError();
	sprintf(errbuf, "(%d) ", line);
	len = strlen(errbuf);
	FormatMessage( 
		FORMAT_MESSAGE_FROM_SYSTEM | 
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		errcode,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		errbuf+len,
		bufsize-len,
		NULL);
	
	if (iErrorMessage)
		gDispose(iErrorMessage);
	iErrorMessage = gNewWithStr(String, errbuf);
//	pCloseHandles(self);
	pResetHandle(self, &iOpenRequest);
	return errcode;
}

imeth HANDLE gGetWinApiRequestHandle()
{
	return iOpenRequest;
}

private imeth void pCloseHandles(object self)
{
	pResetHandle(self, &iOpenRequest);
	pResetHandle(self, &iInternetConnect);
	pResetHandle(self, &iInternetOpen);
}

private	imeth	void	pResetHandle(object self, HINTERNET *handle)
{
	if (!handle)
		return;
	InternetCloseHandle(*handle);
	*handle = NULL;
}

imeth	void	gSetResponseTimeoutSeconds(int seconds)
{
	iTimeoutSeconds = seconds;
}

imeth	gGetResponseHeaders()
{
	return iResponseHeaders;
}

imeth	char	*gGetResponseHeader(char *headerName)
{
	object val;
	char tmpbuf[256];
	if (!iResponseHeaders)
		return NULL;
	strncpy(tmpbuf, headerName, sizeof(tmpbuf) - 1);
	val = gFindValueStr(iResponseHeaders, strlwr(tmpbuf));
	return val ? gStringValue(val) : NULL;
}


