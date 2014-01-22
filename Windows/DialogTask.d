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




defclass DialogTask : Task {
	iDialog;
	int	iRow;	//  last row and col position
	int	iCol;
};


static	int	completion(object dlg, int res);


imeth	gNewDialog(WORD resource, wind)
{
	int	y, x, pmode;
	object	tl = gTaskList(self);
	object	dlg = gNewDialog(ModelessDialog, resource, wind);
	iDialog = dlg;
	gSetTask(dlg, self);
	gPropertyPut(dlg, "DialogTask", 0, self);
	gSetTag(dlg, self);
	gDisableObject(dlg, tl);
	gCompletionFunction(dlg, completion);
	pmode = gSetScalingMode(Application, SM_PIXELS);
	gGetPosition(tl, &y, &x);
	if (y > -1)
		gSetPosition(dlg, y, x);
	gSetScalingMode(Application, pmode);
	return dlg;
}

imeth	gNewDialogFromFile(char *file, unsigned resource, wind)
{
	int	y, x, pmode;
	object	tl = gTaskList(self);
	object	dlg = gNewDialogFromFile(ModelessDialog, file, resource, wind);
	iDialog = dlg;
	gSetTask(dlg, self);
	gPropertyPut(dlg, "DialogTask", 0, self);
	gDisableObject(dlg, tl);
	gCompletionFunction(dlg, completion);
	pmode = gSetScalingMode(Application, SM_PIXELS);
	gGetPosition(tl, &y, &x);
	if (y > -1)
		gSetPosition(dlg, y, x);
	gSetScalingMode(Application, pmode);
	return dlg;
}

imeth	gRunClose()
{
	if (iDialog)
		iDialog = gDispose(iDialog);
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iDialog) {
		gRunClean(self);
		gRunClose(self);
	}
	return gDispose(super);
}

static	int	completion(object dlg, int res)
{
	int	pmode;
	object	self = gPropertyGet(dlg, "DialogTask");
	ivType	*iv;
	
	if (!IsObj(self))
		return res;
	iv = ivsPtr;
	pmode = gSetScalingMode(Application, SM_PIXELS);
	gGetPosition(iDialog, &iRow, &iCol);
	gSetScalingMode(Application, pmode);
	iDialog = NULL;
	if (res > -1)
		gNextTask(super, res);
	return res;
}

imeth	gSetPosition(int y, int x)
{
	iRow = y;
	iCol = x;
	gScaleToPixels(Application, &iRow, &iCol, NULL);
	return iDialog ? gSetPosition(iDialog, y, x) : NULL;
}

imeth	gGetPosition(int *y, int *x)
{
	if (iDialog)
		gGetPosition(iDialog, y, x);
	else {
		*y = iRow;
		*x = iCol;
		gScaleToCurrentMode(Application, y, x, NULL);
	}
	return self;
}

imeth	gSetFocus()
{
	if (iDialog)
		gSetFocus(iDialog);
	return self;
}

imeth	int	gCheckValue()
{
	return iDialog ? gCheckValue(iDialog) : 0;
}

imeth	gDialog()
{
	return iDialog;
}







