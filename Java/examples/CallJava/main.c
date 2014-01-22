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
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_dialog(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_MESSAGE, file_message);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	long	file_message(object wind, unsigned id)
{
	object	jcls, jobj;
	int	res;
	BOOL	bErr;
	
	gSetJavaInitParameters(Java, ".;../../Java/Dynace.jar", "../../DLL", "Dynace.generics", NULL);
//	gSetJavaInitParameters(Java, ".;./Dynace.jar", ".", "Dynace.generics", NULL);
	if (gNewJavaVM(Java)) {
		char	*err = gGetLastJavaError(Java);
		vPrintf(wind, "Could not create Java VM\n%s\n", err);
		return 0L;
	} else
		vPrintf(wind, "Successfully created Java VM\n");
	jcls = gNewWithStr(JavaClass, "Test");
	if (!jcls) {
		char	*err = gGetLastJavaError(Java);
		vPrintf(wind, "Could not find Java class\n%s\n", err);
		return 0L;
	} else
		vPrintf(wind, "Successfully found Java class\n");

	res = vCallJavaMethod(jcls, "add33", "(I)I", &bErr, 22);
	if (bErr) {
		char	*err = gGetLastJavaError(Java);
		vPrintf(wind, "%s\n", err);
		return 0L;
	} else
		vPrintf(wind, "Result = %d\n", res);
	

	jobj = vNewJavaObject(jcls, "()V");
	if (!jobj) {
		char	*err = gGetLastJavaError(Java);
		vPrintf(wind, "%s\n", err);
		return 0L;
	} else
		vPrintf(wind, "Got new instance\n");
	

	res = vCallJavaMethod(jobj, "add44", "(I)I", &bErr, 22);
	if (bErr) {
		char	*err = gGetLastJavaError(Java);
		vPrintf(wind, "%s\n", err);
		return 0L;
	} else
		vPrintf(wind, "Result = %d\n", res);
	
	
	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
