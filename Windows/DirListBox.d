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




#include "logfile.h"
#include "hdlcache.h"

#define	PATH_SIZE	256

defclass  DirListBox : ListBox  {
	iDlg;			/*  dialog object		*/
	char	iPath[PATH_SIZE];
	char	iFile[PATH_SIZE/2];
	char	iFull[PATH_SIZE];
	char	iSearch[80];
	int	(*iDCFun)();	/*  function - double click	*/
	iCurrentCtl;		/*  static control used to display current
				    drive and path  */
	unsigned	iFileType;
};
      

#include <direct.h>
#include <ctype.h>


static	char	savePath[PATH_SIZE];

static	void	save_path()
{
	_getcwd(savePath, PATH_SIZE);
}

static	void	restore_path()
{
	_chdrive(toupper(*savePath) - ('A' - 1));
	_chdir(savePath);
}

static	int	go_up(char *path)
{                               
	int	i = strlen(path) - 1;
	char	*last;
	
	if (path[i] == '/'  ||  path[i] == '\\')
		path[i] = '\0';
	for (last=NULL, i=0 ; path[i] ; i++)
		if (path[i] == '/'  ||  path[i] == '\\')
			if (!i  ||  i == 2  &&  path[1] == ':')
				last = path + i + 1;
			else
				last = path + i;
	if (last)
		*last = '\0';
	return 1;	
}      	

static	int	change_dir(char *path, char *chg)
{                               
	int	i = strlen(path) - 1;
	
	if (path[i] != '/'  &&  path[i] != '\\')
		strcat(path, "\\");
	strcat(path, chg);
	path[strlen(path)-1] = '\0';
	return 1;	
}      	

static	int	add_file(char *file, char *chg)
{                               
	strcpy(file, chg);
	return 0;	
}      	

static	int	update_path(char *file, char *path, char *chg)
{  
	*file = '\0';
	if (*chg  &&  chg[1] == ':')
		return 2;
	else if (!strcmp(chg, "..\\"))
		return go_up(path);
	else if (*chg  &&  chg[strlen(chg)-1] == '\\')
		return change_dir(path, chg);
	else if (*chg)
		return add_file(file, chg); 
	else
		return 0;
}

static	int	is_wild(char *path)
{
	for ( ; *path ; path++)
		if (*path == '?'  ||  *path == '*')
			return 1;
	return 0;
}

private	imeth	int	double_click(object dlg)
{
	char	path[PATH_SIZE];
	int	r, r2;
	
	DlgDirSelectEx(gHandle(iDlg), path, PATH_SIZE, gGetCtlID(self));
	r = update_path(iFile, iPath, path);
	if (r)  {
		if (r != 2)
			strcpy(path, iPath);
		strcat(path, iSearch);
		save_path();
		r2 = DlgDirList(gHandle(iDlg), path, gGetCtlID(self),
		   		iCurrentCtl ? gGetCtlID(iCurrentCtl) : 0, iFileType);
		if (r == 2  &&  r2)
			_getcwd(iPath, PATH_SIZE);
		restore_path();
	}
	return iDCFun ? (*iDCFun)(self, iDlg) : 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	char	path[PATH_SIZE];
	int	r;

	iDlg = dlg;
	gInitialize(super, hDlg, dlg);
	sprintf(path, "%s%s", iPath, iSearch);
	save_path();
	r = DlgDirList(hDlg, path, gGetCtlID(self),
		       iCurrentCtl ? gGetCtlID(iCurrentCtl) : 0, iFileType);
	if (r)
		_getcwd(iPath, PATH_SIZE);
	restore_path();
	gSetFunction(super, double_click);
	return self;
}

imeth	gUpdate : update ()
{
	char	path[PATH_SIZE];

	if (iDlg  &&  gInDialog(iDlg)) {
		sprintf(path, "%s%s", iPath, iSearch);
		save_path();
		DlgDirList(gHandle(iDlg), path, gGetCtlID(self),
			   iCurrentCtl ? gGetCtlID(iCurrentCtl) : 0, iFileType);
		restore_path();
		if (is_wild(iPath))
			go_up(iPath);
	}
	return self;
} 

static	void	split_search(char *srch, char *full)
{
	char	*s, *f;
	
	if (!is_wild(full))  {
		*srch = '\0';
		return;
	}
	for (f=s=full ; *f ; f++)
		if (*f == '\\'  ||  *f == '/'  ||  *f == ':')
			s = f + 1;
	sprintf(srch, "\\%s", s);
	go_up(full);
}

imeth	gSetDirCtl(char *path, unsigned type, pathCtl)
{
	strcpy(iPath, path);
	split_search(iSearch, iPath);
	iFileType = type;
	iCurrentCtl = pathCtl;
	update(self);
	return self;
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	org = (ofun) iDCFun;
	iDCFun = fun;
	return org;
}

imeth	char	*gStringValue()
{
	int	i;
	char	file[PATH_SIZE]; 
	
	if (iDlg  &&  gInDialog(iDlg))      
		DlgDirSelectEx(gHandle(iDlg), file, PATH_SIZE, gGetCtlID(self));
	else
		strcpy(file, iFile);
	strcpy(iFull, iPath);
	i = strlen(iFull) - 1;
	if (!*iFull  ||  iFull[i] != '\\'  &&  iFull[i] != '/')
		strcat(iFull, "\\");
	i = strlen(file) - 1;                                          
	if (*file  &&  file[i] != '\\'  &&  file[i] != '/'  &&  file[i] != ':')
		strcat(iFull, file);
	return iFull;
}

imeth	gSetStringValue(char *val)
{
	int	i = strlen(val) - 1;
	strcpy(iPath, val);         
	if (i > 0  &&  (val[i] == '\\'  ||  val[i] == '/')  &&  val[i-1] != ':')
		iPath[i] = '\0';
	split_search(iSearch, iPath);
	update(self);
	return self;
}





