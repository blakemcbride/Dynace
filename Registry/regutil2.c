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
extern char *_fullpath();
#endif
#endif

#include <windows.h>
#include <stdio.h>
#include <direct.h>
#include <stdlib.h>

#ifdef _MSC_VER
#if _MSC_VER >= 1400
#define stricmp _stricmp
#endif
#endif



void	RegStoreApplicationPath(char *company, char *product, char *version)
{
	HKEY	h;
	LONG	r;
	char	dir[_MAX_PATH], name[30], data[_MAX_PATH], key[128];
	DWORD	name_len = sizeof(name), type, data_len = sizeof(data);
	int	i, found = 0, highest=0, val;

	sprintf(key, "SOFTWARE\\%s\\%s\\%s\\Application Paths", company, product, version);
	r = RegCreateKeyEx(HKEY_LOCAL_MACHINE,
			   key,
			   0,
			   "",
			   REG_OPTION_NON_VOLATILE,
			   KEY_ALL_ACCESS,
			   NULL,
			   &h,
			   NULL);
	if (r != ERROR_SUCCESS) {
//		gMessage(Application, "Key NOT Created");
		return;
	} 
	_getcwd(dir, _MAX_PATH);
	for (i=0 ; ERROR_SUCCESS == RegEnumValue(h, i, name, &name_len, NULL, &type, data, &data_len) ; i++) {
//		vPrintf(wind, "\"%s\" = \"%s\"\n", name, data);
		if (!stricmp(dir, data)) {
			found = 1;
			break;
		}
		val = atoi(name);
		if (val > highest)
			highest = val;
		name_len = sizeof(name);
		data_len = sizeof(data);
	}
	if (!found) {
		sprintf(name, "%d", highest+1);
		RegSetValueEx(h, name, 0, REG_SZ, dir, (DWORD)strlen(dir)+1);
	}
	RegCloseKey(h);
}

void	RegStoreDataPath(char *company, char *product, char *version, char *path)
{
	HKEY	h;
	LONG	r;
	char	dir[_MAX_PATH], name[30], data[_MAX_PATH], key[128];
	DWORD	name_len = sizeof(name), type, data_len = sizeof(data);
	int	i, found = 0, highest=0, val;

	sprintf(key, "SOFTWARE\\%s\\%s\\%s\\Data Paths", company, product, version);
	r = RegCreateKeyEx(HKEY_LOCAL_MACHINE,
			   key,
			   0,
			   "",
			   REG_OPTION_NON_VOLATILE,
			   KEY_ALL_ACCESS,
			   NULL,
			   &h,
			   NULL);
	if (r != ERROR_SUCCESS) {
//		gMessage(Application, "Key NOT Created");
		return;
	} 
	_fullpath(dir, path, _MAX_PATH);
	for (i=0 ; ERROR_SUCCESS == RegEnumValue(h, i, name, &name_len, NULL, &type, data, &data_len) ; i++) {
//		vPrintf(wind, "\"%s\" = \"%s\"\n", name, data);
		if (!stricmp(dir, data)) {
			found = 1;
			break;
		}
		val = atoi(name);
		if (val > highest)
			highest = val;
		name_len = sizeof(name);
		data_len = sizeof(data);
	}
	if (!found) {
		sprintf(name, "%d", highest+1);
		RegSetValueEx(h, name, 0, REG_SZ, dir, (DWORD)strlen(dir)+1);
	}
	RegCloseKey(h);
}

/**
 * Fetch an individual application path item based on the index
 */
char	*RegGetApplicationPath(char *company, 
			       char *product,
			       char *version,
			       int  idx,
			       char *pathFound)
{
	HKEY	hwnd;
        LONG    rc;
        DWORD   type;
        DWORD   data_len;
        DWORD   name_len;
        char    name[30];
	char	key[128];
        char    data[_MAX_PATH];

	name_len = sizeof(name);
        data_len = sizeof(data);

	sprintf(key, "SOFTWARE\\%s\\%s\\%s\\Application Paths", company, product, version);

	rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
			  key,
			  0,
			  KEY_READ,
			  &hwnd);
	
        if (rc == ERROR_SUCCESS  &&  hwnd)  {
		rc = RegEnumValue(hwnd, idx, name, &name_len, NULL, &type, data, &data_len);
		if (rc == ERROR_SUCCESS)
			strcpy(pathFound, data);
		else
			*pathFound = '\0';
		RegCloseKey(hwnd);
		return *pathFound ? pathFound : NULL;
	} else
		return NULL;
}

/**
 * Find the current count of application paths in the system
 */
int     RegGetApplicationPathCount(char *company, char *product, char *version)
{
	HKEY	hwnd;
        LONG    rc;
        DWORD   index         = 0;   // Enum Idx
        DWORD   keyCount      = 0;   // Count keys
        DWORD   type;
        DWORD   data_len;
        DWORD   name_len;
        char    name[30];
	char	key[128];
        char    data[_MAX_PATH];

	name_len = sizeof(name);
        data_len = sizeof(data);

	sprintf(key, "SOFTWARE\\%s\\%s\\%s\\Application Paths", company, product, version );

	rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
			  key,
			  0,
			  KEY_READ,
			  &hwnd);

        if (rc == ERROR_SUCCESS  &&  hwnd)  {
		while (rc != ERROR_NO_MORE_ITEMS) {
			// Enumerate through the registry keys
			rc =  RegEnumValue(hwnd,
					   index,
					   name,
					   &name_len,
					   NULL,
					   &type,
					   data,
					   &data_len);
			if (rc != ERROR_NO_MORE_ITEMS)
				keyCount++;           
			index++;  
		}
		RegCloseKey(hwnd);
	}
	return keyCount;
}






