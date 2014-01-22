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



defclass StatusWindow : PopupWindow;

cmeth	gNewStatusWindow(parent, char *title, char *msg)
{
	int	prev = gSetScalingMode(Application, SM_1_PER_CHAR);
	object	obj = vNew(super, title ? title : "Status Window" , 3, strlen(msg) + 6);
	object	message = gNewWithStr(String, msg);
	int	ww, wh;
	int	height, width, xpos, ypos, x, y;
	object	font = gGetFont(Application);
	int	i, numlines;
	int	len = gSize(message);

	gProcessMessage(MessageDispatcher, WM_PAINT);
	
	gWaitCursor(obj, 1);
	
	for (i = 0, numlines = 1; i < len; i++)
		if (msg[i] == '\n')
			numlines++;

	wh = numlines + 4;
	wh *= gLineHeight(font);

	gSetScalingMode(Application, SM_PIXELS);
	if (parent) {
		gGetSize(parent, &height, &width);
		gGetPosition(parent, &ypos, &xpos);
		gDisable(parent);
		gSetParent(obj, parent);
		gWaitCursor(parent, 1);
	} else {
		gGetSize(Application, &height, &width);
		xpos = ypos = 0;
	}

	
	gTake(message, strlen(msg) + 10);
	gJustifyCenter(message);
	ww = gStrPixelWidth(font, gStringValue(message)) + 10;
	gDispose(font);

	gSetSize(obj, wh, ww);

	y = ypos + (height - wh) / 2;
	x = xpos + (width  - ww) / 2;

	gBackBrush(obj, gNewSystemBrush(SystemBrush, COLOR_BTNFACE));
	gSetStyle(obj, WS_VISIBLE | WS_BORDER | WS_CAPTION);

	vPrintf(obj, "\n%s", gStringValue(message));
//	gTextOut(obj, 15, 0, gStringValue(message));
	
	gSetPosition(obj, y, x);
	gShow(obj);

	gDispose(message);

	gSetScalingMode(Application, prev);

	return obj;
}

imeth	object	gDeepDispose, gDispose ()
{
	object	parent = gGetParent(self);
	object	rtn;

	if (parent) {
		gEnable(parent);
		gSetZOrder(parent, HWND_TOP);
		gWaitCursor(parent, 0);
	}

	rtn = gDispose(super);
	gProcessMessage(MessageDispatcher, WM_PAINT);
	return rtn;
}

ivmeth	vSetStringValue(char *fmt, ...)
{
	char	*buf = gGetBuf(Application);
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	return gSetStringValue(self, buf);
}

imeth gSetStringValue(char *msg)
{
	object	message = gNewWithStr(String, msg);
	int	prev = gSetScalingMode(Application, SM_PIXELS);
	int	ww, wh;
	int	height, width, xpos, ypos, x, y, vs;
	object	font = gGetFont(Application);
	object	parent;
	int	pw, ph;
	int	i, numlines;
	int	len = gSize(message);

	for (i = 0, numlines = 1; i < len; i++)
		if (msg[i] == '\n')
			numlines++;

	wh = numlines + 4;
	wh *= gLineHeight(font);

	gEraseAll(self);
	
	gTake(message, strlen(msg) + 10);
	gJustifyCenter(message);
	ww = gStrPixelWidth(font, gStringValue(message)) + 10;
	gDispose(font);

	gGetSize(self, &ph, &pw);

	if (ww > pw)
		gSetSize(self, wh, ww);
	else
		ww = pw;
	
	gTextOut(self, 15, 0, gStringValue(message));

	gDispose(message);

	if (parent = gGetParent(self)) {
		gGetSize(parent, &height, &width);
		gGetPosition(parent, &ypos, &xpos);
	} else {
		gGetSize(Application, &height, &width);
		xpos = ypos = 0;
	}

	y = ypos + (height - wh) / 2;
	x = xpos + (width  - ww) / 2;
	
	gSetPosition(self, y, x);

	gSetScalingMode(Application, prev);

	gUpdate(self);

	gProcessMessage(MessageDispatcher, WM_PAINT);
	return self;
}











