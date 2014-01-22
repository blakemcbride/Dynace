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

#include <windows.h>
#include <mapi.h>

typedef	ULONG (PASCAL *MAPISendMail_t)(ULONG, ULONG, MapiMessage*, FLAGS, ULONG);


defclass Mapi {
	MapiRecipDesc	iFrom;
	MapiMessage	iMessage;
	iSubject;
	iText;
class:
	int	cNumberOfInstances;
	cLibrary;
	MAPISendMail_t cMAPISendMail;
};

#include <string.h>


private imeth	pInit()
{
	iMessage.lpOriginator = &iFrom;
	iFrom.ulRecipClass = MAPI_ORIG;
	return self;
}

cmeth	gNew()
{
	if (!cLibrary) {
		cLibrary = gLoadLibrary(DynamicLibrary, "MAPI32.DLL");
		if (!cLibrary)
			return NULL;
		cMAPISendMail = (MAPISendMail_t) gGetProcAddress(cLibrary, "MAPISendMail");
		if (!cMAPISendMail)
			return cLibrary = gDispose(cLibrary);
	}
	cNumberOfInstances++;
	return pInit(gNew(super));
}

private	imeth	void	pFree()
{
	int	i;
	
	if (iMessage.lpRecips) {
		for (i = 0; i < iMessage.nRecipCount; i++)
			free(iMessage.lpRecips[i].lpszName);
		free(iMessage.lpRecips);
		iMessage.lpRecips = NULL;
	}

	if (iMessage.lpFiles) {
		for (i = 0; i < iMessage.nFileCount; i++) {
			free(iMessage.lpFiles[i].lpszPathName);
/*	 		free(iMessage.lpFiles[i].lpszFileName);  */
		}
		free(iMessage.lpFiles);
		iMessage.lpFiles = NULL;
	}
	if (iSubject)
		iSubject = gDispose(iSubject);
	if (iText)
		iText = gDispose(iText);
}

imeth	gDispose, gDeepDispose ()
{
	if (!--cNumberOfInstances)
		cLibrary = gDispose(cLibrary);
	pFree(self);
	return gDispose(super);
}

private	imeth	pTo(char *recip, int type)
{
	iMessage.lpRecips = (MapiRecipDesc *) realloc(iMessage.lpRecips, (iMessage.nRecipCount + 1) * sizeof(MapiRecipDesc));
	if (!iMessage.lpRecips)
		gError(Application, "Out of memory.");
	memset(&iMessage.lpRecips[iMessage.nRecipCount], 0, sizeof(MapiRecipDesc));
	iMessage.lpRecips[iMessage.nRecipCount].lpszName = (LPTSTR) malloc(strlen(recip) + 1);
	strcpy(iMessage.lpRecips[iMessage.nRecipCount].lpszName, recip);
	iMessage.lpRecips[iMessage.nRecipCount].ulRecipClass = type;
	iMessage.nRecipCount++;
	return self;
}

imeth	gTo(char *recip)
{
	return pTo(self, recip, MAPI_TO);
}

imeth	gCc(char *recip)
{
	return pTo(self, recip, MAPI_CC);
}

imeth	gAttachFile(char *path)
{
	iMessage.lpFiles = (MapiFileDesc *) realloc(iMessage.lpFiles, (iMessage.nFileCount + 1) * sizeof(MapiFileDesc));
	if (!iMessage.lpFiles)
		gError(Application, "Out of memory.");
	memset(&iMessage.lpFiles[iMessage.nFileCount], 0, sizeof(MapiFileDesc));
	iMessage.lpFiles[iMessage.nFileCount].lpszPathName = (LPTSTR) malloc(strlen(path) + 1);
	strcpy(iMessage.lpFiles[iMessage.nFileCount].lpszPathName, path);
	iMessage.nFileCount++;
	return self;
}

imeth	gSubject(char *sub)
{
	if (iSubject)
		gChangeStrValue(iSubject, sub);
	else
		iSubject = gNewWithStr(String, sub);
	return self;
}

imeth	gText(char *sub)
{
	if (iText)
		gChangeStrValue(iText, sub);
	else
		iText = gNewWithStr(String, sub);
	return self;
}

imeth	int	gSendMail(pwind)
{
	int	r, i;

	if (iMessage.nFileCount) {
		int	offset;
		
		if (!iText)
			iText = gNew(String);
		gTake(iText, (offset=gLength(iText)) + iMessage.nFileCount);
		for (i=0 ; i < iMessage.nFileCount ; i++)
			iMessage.lpFiles[i].nPosition = offset++;
	}
	iMessage.lpszNoteText = iText ? gStringValue(iText) : "";
	iMessage.lpszSubject = iSubject ? gStringValue(iSubject) : "";
		
	r = cMAPISendMail(0, (ULONG) pwind, &iMessage, MAPI_LOGON_UI, 0);

	pFree(self);

	return r;
}










