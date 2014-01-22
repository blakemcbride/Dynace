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




defclass SchemeDialogTask : DialogTask {
	iSchemeObj;
	iParent;

	iWorkSpace;

	char	iErrorBuf[256];
	int	iErrorNumber;
};

private	imeth	pInitInstance(object self, object parent, char *workspace)
{
	gSetTag(self, iParent=parent);

	iWorkSpace = gNewWithStr(String, workspace);

	return self;
}

cmeth	gNewSchemeTaskItem(object parent, char *workspace)
{
	return pInitInstance(gNewTask(super, parent), parent, workspace);
}

imeth	gInitSchemeTaskItem(char *lib, char *parameters)
{
	char	*ws = gStringValue(iWorkSpace);
	object	str = vSprintf(String,
"(if (defined? 'require-file) "
"(require-file (build-path (car (current-library-collection-paths)) \"%s\" \"%s.scm\")) "
"(load/use-compiled (build-path (car (current-library-collection-paths)) \"%s\" \"%s.scm\")))",
			       lib, ws, lib, ws);

	gExecuteStringWithErrorNR(Scheme, gStringValue(str), iErrorBuf, &iErrorNumber);
	gDispose(str);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);
	else {
		str = vSprintf(String, "(Make-Pointer "
			       "(sInitTaskItem (NewInstance %s) (int->object %ld)%s%s))",
			       gStringValue(iWorkSpace), self, parameters && *parameters ? " " : "",
			       parameters && *parameters ? parameters : "");
		iSchemeObj = gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace),
							  gStringValue(str), iErrorBuf, &iErrorNumber);
		gDispose(str);
		if (iErrorNumber)
			gMessage(Application, iErrorBuf);
	}

	return self;
}

imeth	gRunRun()
{
	char	buf[50];

	sprintf(buf, "(sRunRun (Deref-Pointer %ld))", gLongValue(iSchemeObj));
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);

	if (iErrorNumber) {
		gMessage(Application, iErrorBuf);
		gNextTask(iParent, 0);
	}

	return self;
}

imeth	gRunSave()
{
	char	buf[50];

	sprintf(buf, "(sRunSave (Deref-Pointer %ld))", gLongValue(iSchemeObj));
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);

	return self;
}

imeth	gRunClean()
{
	char	buf[50];

	sprintf(buf, "(sRunClean (Deref-Pointer %ld))", gLongValue(iSchemeObj));
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);

	return self;
}

imeth	gRunClose()
{
	char	buf[50];

	sprintf(buf, "(sRunClose (Deref-Pointer %ld))", gLongValue(iSchemeObj));
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);

	return gRunClose(super);
}

imeth	gDispose, gDeepDispose ()
{
	long	soaddr;
	char	buf[50];

	if (!iSchemeObj)
		return gDispose(super);

	soaddr = gLongValue(iSchemeObj);
	
	sprintf(buf, "(sDispose (Deref-Pointer %ld))", soaddr);
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);
				     
	sprintf(buf, "(Free-Pointer %ld)", soaddr);
	gExecuteInNamespaceWithError(Scheme, gStringValue(iWorkSpace), buf, iErrorBuf, &iErrorNumber);
	if (iErrorNumber)
		gMessage(Application, iErrorBuf);

	gDispose(iWorkSpace);
	gDispose(iSchemeObj);

	return gDispose(super);
}

imeth	gGetSchemeObject()
{
	return iSchemeObj;
}

imeth	char	*gGetNamespace()
{
	return gStringValue(iWorkSpace);
}












