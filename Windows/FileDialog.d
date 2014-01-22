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


#define	COBJMACROS

#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#include <windows.h>
#include <commdlg.h>
#include <string.h>
#include <direct.h>
#include <ctype.h>
#include "logfile.h"

#if	defined(WIN32)
#define	ENABLE_BROWSE_DIR
#include <shlobj.h>
#endif

#ifndef	ENABLE_BROWSE_DIR
#define	BROWSEINFO	int
#endif

defclass  FileDialog : CommonDialog  {
	OPENFILENAME	iOFN;
	BROWSEINFO	iBI;   //  only works in 32 bits - used for getting a directory instead of a file name
	char		iFile[1024];   //  must be large to accommodate multiple file selections

	char		iFilter[512];
	int		iFilterSize;
	int		iUserSetFilter;

	char		iInitDir[128];

	int		iUserSetFlags;
};

static int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
	// Set the default path on the dialog box.
	if (uMsg == BFFM_INITIALIZED)
		SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);

	return 0;  // Return value is always zero, according to MSDN docs.
}

cvmeth	vNew(parent)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iOFN.lStructSize = sizeof(OPENFILENAME);
	iOFN.hwndOwner = parent ? gHandle(parent) : (HWND) 0;

	iOFN.lpstrFile = iFile;
	iOFN.nMaxFile = sizeof(iFile);
	
	iOFN.lpstrFilter = iFilter;
	gAppendFilter(obj, "All Files", "*.*");
	iUserSetFilter = 0;
#ifdef	ENABLE_BROWSE_DIR
	iBI.hwndOwner = parent ? gHandle(parent) : (HWND) 0;
	iBI.pszDisplayName = iFile;
	iBI.lpszTitle = "Please select the folder";
	iBI.ulFlags = BIF_RETURNONLYFSDIRS;
#endif
	return obj;
}

cmeth	gNewFileDialog(parent)
{
	return vNew(self, parent);
}

static	int	my_chdir(char *path)
{
	int r = 0;
#if 0
	char	buf[100];

	sprintf(buf, "chdir(%s) = %d", path, r);
	gMessage(Application, buf);
#endif
	if (path[1] == ':') {
		r |= _chdrive(tolower(*path) - 'a' + 1);
#if 0
		sprintf(buf, "chdrive(%d) = %d", tolower(*path) - 'a' + 1, r);
		gMessage(Application, buf);
#endif
	}
	r |= _chdir(path);

	return r;
}	

imeth	int	gGetOpenFile()
{
	char	cwd[128];
	int	r;

	if (!iUserSetFlags)
		iOFN.Flags = OFN_FILEMUSTEXIST;
	_getcwd(cwd, sizeof cwd);
	r = GetOpenFileName(&iOFN);
//	my_chdir(cwd);
	return r;
}

imeth	int	gGetSaveFile()
{
	char	cwd[128];
	int	r;

	_getcwd(cwd, sizeof cwd);
	r = GetSaveFileName(&iOFN);
//	my_chdir(cwd);
	return r;
}

imeth	int	gGetPath()
{
#ifdef	ENABLE_BROWSE_DIR
	LPMALLOC	pm;
	LPITEMIDLIST	r;
	int	ret = 0;
	gUseCOM(Application);  // required by SHBrowseForFolder
	if (NOERROR != SHGetMalloc(&pm))
		return -2;  //  COM didn't initialize
	r = SHBrowseForFolder(&iBI);
	if (r) {
		if (SHGetPathFromIDList(r, iFile))
			ret = 1;   //  good
		else
			ret = -1;  //  invalid selection
		IMalloc_Free(pm, r);
	} else
		ret = 0;  //  user hit Cancel
	IMalloc_Release(pm);
	return ret;
#else
	return 0;
#endif
}

imeth	gSetFlags(DWORD flags)
{
	iOFN.Flags = flags;
	iUserSetFlags = 1;
	return self;
}

imeth	gSetFile(char *file)
{
	if (IsObj((object) file))
		file = gStringValue((object) file);
	strcpy(iFile, file);
	return self;
}

imeth	char	*gGetFile()
{
	return iFile;
}

imeth	gAppendFilter(char *name, char *filter)
{
	int	len;

	if (!iUserSetFilter)  {
		iUserSetFilter = 1;
		iFilterSize = 0;
	}
	memcpy(iFilter+iFilterSize, name, len=strlen(name)+1);
	iFilterSize += len;
	memcpy(iFilter+iFilterSize, filter, len=strlen(filter)+1);
	iFilterSize += len;
	iFilter[iFilterSize] = '\0';
	return self;
}

imeth	gInitialDir(char *path)
{
	char	*p;
	
	iOFN.lpstrInitialDir = iInitDir;
	strcpy(iInitDir, path);
	
#ifdef ENABLE_BROWSE_DIR	
	for (p = iInitDir; *p; p++)
		if (*p == '/')
			*p = '\\';
	
	iBI.lpfn = BrowseCallbackProc;  // These two allow us to select the initial path
	iBI.lParam = (LPARAM)iInitDir;  // when browsing for folders (gGetPath).
#endif
	
	return self;
}

imeth	gSetTitle(char *title)
{
	iOFN.lpstrTitle = title;
	return self;
}

imeth	gDefExt(char *ext)
{
	iOFN.lpstrDefExt = ext;
	return self;
}







