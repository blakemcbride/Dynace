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




defclass WindowTask : Task {
	iWind;
	int	iRow;	//  last row and col position
	int	iCol;
	int	iResult;
};


static	int	completion(object wind);
static	long	onOK(object wind, unsigned id);
static	long	onCancel(object wind, unsigned id);

static	long	process_wm_close(object	wind, 
				 HWND	hwnd, 
				 UINT	mMsg, 
				 WPARAM	wParam, 
				 LPARAM	lParam)
{
	object	self = gPropertyGet(wind, "WindowTask");
	accessIVs;
	
	iResult = 0;
	return 0L;
}

imeth	gNewWindow(pwind, char *title, int height, int width)
{
	int	y, x, pmode;
	object	tl = gTaskList(self);
	object	wind = gNewPopupWindow(PopupWindow, title, height, width);

	gSetParent(wind, pwind);
	gSetStyle(wind, WS_VISIBLE | WS_BORDER | WS_POPUPWINDOW | WS_DLGFRAME
		  | WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_SYSMENU);
	gBackBrush(wind, gNewSystemBrush(SystemBrush, COLOR_BTNFACE));
	
	gAddHandlerBefore(wind, (unsigned) WM_CLOSE, process_wm_close);

	iWind = wind;
	gSetTask(wind, self);
	gPropertyPut(wind, "WindowTask", 0, self);
	gDisableObject(wind, tl);
	gCompletionFunction(wind, completion);
	pmode = gSetScalingMode(Application, SM_PIXELS);
	gGetPosition(tl, &y, &x);
	if (y > -1)
		gSetPosition(wind, y, x);
	gSetScalingMode(Application, pmode);

	iResult = -1;

	return wind;
}

imeth	gAddOKCancel(unsigned ok, unsigned cancel)
{
	if (!iWind)
		return NULL;
	
	if (ok)
		gAddToolBitmap(iWind, ok, 0, 2, onOK, "OK");
	if (cancel)
		gAddToolBitmap(iWind, cancel, 0, 2, onCancel, "Cancel");

	return self;
}

imeth	gRunClose()
{
	if (iWind)
		iWind = gDispose(iWind);

	return gRunClose(super);
}

imeth	gDispose, gDeepDispose ()
{
	if (iWind) {
		gRunClean(self);
		gRunClose(self);
	}
	return gDispose(super);
}

static	int	completion(object wind)
{
	int	pmode;
	object	self = gPropertyGet(wind, "WindowTask");
	ivType	*iv;
	int	rval;
	
	if (!IsObj(self))
		return -1;
	iv = ivsPtr;

	pmode = gSetScalingMode(Application, SM_PIXELS);
	gGetPosition(iWind, &iRow, &iCol);
	gSetScalingMode(Application, pmode);
	iWind = NULL;
	if (iResult > -1)
		gNextTask(super, iResult);
	
	iResult = -1;
	
	return -1;
}

imeth	gSetPosition(int y, int x)
{
	iRow = y;
	iCol = x;
	gScaleToPixels(Application, &iRow, &iCol, NULL);
	return iWind ? gSetPosition(iWind, y, x) : NULL;
}

imeth	gGetPosition(int *y, int *x)
{
	if (iWind)
		gGetPosition(iWind, y, x);
	else {
		*y = iRow;
		*x = iCol;
		gScaleToCurrentMode(Application, y, x, NULL);
	}
	return self;
}

imeth	gSetFocus()
{
	if (iWind)
		gSetFocus(iWind);
	return self;
}

imeth	int	gCheckValue()
{
	return iWind ? gCheckValue(iWind) : 0;
}

imeth	gOnTaskOK()
{
	onOK(iWind, 0);

	return self;
}

imeth	gOnTaskCancel()
{
	onCancel(iWind, 0);

	return self;
}

static	long	onOK(object wind, unsigned id)
{
	object	self = gPropertyGet(wind, "WindowTask");
	accessIVs;

	gNextTask(super, IDOK);

	return 0;
}

static	long	onCancel(object wind, unsigned id)
{
	object	self = gPropertyGet(wind, "WindowTask");
	accessIVs;

	gNextTask(super, 0);
	
	return 0;
}

imeth	gDialog()
{
	return iWind;
}






