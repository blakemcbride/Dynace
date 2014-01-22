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


defclass GenericSelection : ODBCListbox {
	iWnd;
	iParent;
	int	iEndDialog;
	char	iDisabledButtons;
	char	iHiddenButtons;

	int	iHidden;

	iSelListBox;

	iOKButton;
	iCancelButton;
	iAddButton;
	iEditButton;
	iDeleteButton;

	int	iVirtual;
	int	iLBPos;
	int	iLBStyle;
	iLBStr;

	int	iInDialog;

	int	(*iFun)();
	int	(*iAddFun)();
	int	(*iEditFun)();
	int	(*iDelFun)();
};

#define	BTN_OK		1
#define	BTN_CANCEL	2
#define	BTN_ADD		4
#define	BTN_EDIT	8
#define	BTN_DELETE	16

static	int	getIDFromName(char *name);
static	object	buildSelectWindow(object pwnd);
static	int	getIDFromName(char *name);
private	imeth	int	pEnableButton(object self, int btn, int enable);

cmeth	gNewGenericSelection(object wind, object stmt, char *sel, char *sw, char *so, int virtual)
{
	object	obj = vNew(super, NULL, NULL, stmt, sel, sw, so, virtual);
	accessIVsOf(obj);
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	iVirtual = virtual;
	iParent = wind;
	iHiddenButtons = BTN_ADD | BTN_EDIT | BTN_DELETE;

	iWnd = buildSelectWindow(wind);
	gPropertyPut(iWnd, "GenericSelection", 0, obj);
	
	gSetScalingMode(Application, pm);

	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	if (iWnd)
		gDispose(iWnd);

	if (iLBStr)
		gDispose(iLBStr);
	
	return gDispose(super);
}

static	int	onOK(object ctl, object wnd)
{
	object	self = gPropertyGet(wnd, "GenericSelection");
	accessIVs;

	iEndDialog = !gReadRecord(self);
	
	iEndDialog = 1;
	return 0;
}

static	int	onCancel(object ctl, object wnd)
{
	object	self = gPropertyGet(wnd, "GenericSelection");
	accessIVs;
	
	iEndDialog = -1;
	return 0;
}

static	int	onAdd(object ctl, object wnd)
{
	object	self = gPropertyGet(wnd, "GenericSelection");
	accessIVs;

	gReadRecord(self);
	
	if (iAddFun)
		iAddFun(self, wnd);

	if (gSize(self)) {
		pEnableButton(self, BTN_EDIT, 1);
		pEnableButton(self, BTN_DELETE, 1);
	}
	
	return 0;
}

static	int	onEdit(object ctl, object wnd)
{
	object	self = gPropertyGet(wnd, "GenericSelection");
	accessIVs;
	
	gReadRecord(self);
	
	if (iEditFun)
		iEditFun(self, wnd);
	
	return 0;
}

static	int	onDelete(object ctl, object wnd)
{
	object	self = gPropertyGet(wnd, "GenericSelection");
	accessIVs;
	
	gReadRecord(self);
	
	if (iDelFun)
		iDelFun(self, wnd);

	if (!gSize(self)) {
		pEnableButton(self, BTN_EDIT, 0);
		pEnableButton(self, BTN_DELETE, 0);
	}

	return 0;
}

static	int	getIDFromName(char *name)
{
	if (!strcmp(name, "OK"))
		return BTN_OK;
	if (!strcmp(name, "Cancel"))
		return BTN_CANCEL;
	if (!strcmp(name, "Add"))
		return BTN_ADD;
	if (!strcmp(name, "Edit"))
		return BTN_EDIT;
	if (!strcmp(name, "Delete"))
		return BTN_DELETE;

	return 0;
}

private	imeth	pInitControls(object self)
{
	int	end;
	int	pm = gSetScalingMode(Application, SM_PIXELS);

	iSelListBox = gAddListBox(iWnd, 12, 12, 210, 495, &end, "SelectionList");
	gSetListbox(self, iSelListBox);
	gSetFunction(iSelListBox, iFun ? iFun : onOK);
	gSetFont(iSelListBox, vNew(ExternalFont, "Courier New", 9));
	if (iLBStyle)
		gSetStyle(iSelListBox, iLBStyle);

	iOKButton = gAddPushButton(iWnd, 12, 520, 100, &end, onOK, "OK", "OK");
	gSetFont(iOKButton, vNew(ExternalFont, "MS Sans Serif", 8));
	if (iDisabledButtons & BTN_OK)
		gDisable(iOKButton);
	if (iHiddenButtons & BTN_OK)
		gHide(iOKButton);

	iCancelButton = gAddPushButton(iWnd, 54, 520, 100, &end, onCancel, "Cancel", "Cancel");
	gSetFont(iCancelButton, vNew(ExternalFont, "MS Sans Serif", 8));
	if (iDisabledButtons & BTN_CANCEL)
		gDisable(iCancelButton);
	if (iHiddenButtons & BTN_CANCEL)
		gHide(iCancelButton);

	iAddButton = gAddPushButton(iWnd, 96, 520, 100, &end, onAdd, "Add", "Add");
	gSetFont(iAddButton, vNew(ExternalFont, "MS Sans Serif", 8));
	if (iDisabledButtons & BTN_ADD)
		gDisable(iAddButton);
	if (iHiddenButtons & BTN_ADD)
		gHide(iAddButton);

	iEditButton = gAddPushButton(iWnd, 138, 520, 100, &end, onEdit, "Edit", "Edit");
	gSetFont(iEditButton, vNew(ExternalFont, "MS Sans Serif", 8));
	if (iDisabledButtons & BTN_EDIT)
		gDisable(iEditButton);
	if (iHiddenButtons & BTN_EDIT)
		gHide(iEditButton);

	iDeleteButton = gAddPushButton(iWnd, 180, 520, 100, &end, onDelete, "Delete", "Delete");
	gSetFont(iDeleteButton, vNew(ExternalFont, "MS Sans Serif", 8));
	if (iDisabledButtons & BTN_DELETE)
		gDisable(iDeleteButton);
	if (iHiddenButtons & BTN_DELETE)
		gHide(iDeleteButton);

	gSetScalingMode(Application, pm);

	return self;
}

imeth	short	gShortValue()
{
	if (iVirtual)
		return 0;

	if (iInDialog)
		return gShortValue(iSelListBox);
	
	return iLBPos;
}

imeth	char	*gStringValue()
{
	if (iVirtual)
		return "";

	if (iInDialog)
		return gStringValue(iSelListBox);

	return iLBStr ? gStringValue(iLBStr) : "";
}

imeth	gSetShortValue(int pos)
{
	if (iVirtual)
		return NULL;

	if (iLBStr)
		iLBStr = gDispose(iLBStr);
	iLBPos = pos;

	if (iInDialog)
		gSetShortValue(iSelListBox, pos);
	
	return self;
}

imeth	gSetStringValue(char *cp)
{
	if (iVirtual)
		return NULL;

	iLBPos = 0;
	iLBStr = gNewWithStr(String, cp);

	if (iInDialog)
		gSetStringValue(iSelListBox, cp);
	
	return self;
}

imeth	int	gPerform()
{
	if (iHidden)
		ShowWindow(gHandle(iWnd), SW_SHOW);
	else {
		gShow(iWnd);

		pInitControls(self);
	
		gPerform(iWnd);
	}

	iInDialog = 1;
	
	gSetFocus(iSelListBox);

	if (gSize(self)) {
		if (iLBStr)
			gSetStringValue(iSelListBox, gStringValue(iLBStr));
		else
			gSetShortValue(iSelListBox, iLBPos);
	} else {
		pEnableButton(self, BTN_EDIT, 0);
		pEnableButton(self, BTN_DELETE, 0);
	}

	iEndDialog = 0;
	gProcessModalMessages(MessageDispatcher, &iEndDialog, gHandle(iWnd));
	
	gEnable(iParent);

	iInDialog = 0;
	iHidden = 1;
	ShowWindow(gHandle(iWnd), SW_HIDE);
	
	return iEndDialog > 0 ? 1 : 0;
}

static	object	buildSelectWindow(object pwnd)
{
	object	wnd;
	int	vert, horz;

	wnd = vNew(PopupWindow, "Select Item", 247, 640);
	gSetStyle(wnd, (WS_VISIBLE | WS_BORDER | WS_POPUPWINDOW | WS_DLGFRAME)
		  & (~WS_MAXIMIZEBOX & ~WS_MINIMIZEBOX & ~WS_SYSMENU));
	gLoadFont(wnd, "MS Sans Serif", 8);
	gBackBrush(wnd, vNew(SystemBrush, COLOR_BTNFACE));
	
	gSetParent(wnd, pwnd);
	gDisable(pwnd);

	gGetPosition(pwnd, &vert, &horz);
	gSetPosition(wnd, vert + 79, horz + 6);

	return wnd;
}

private	imeth	pGetControl(object self, int btn)
{
	switch(btn) {
	case BTN_OK:
		return iOKButton;
	case BTN_CANCEL:
		return iCancelButton;
	case BTN_ADD:
		return iAddButton;
	case BTN_EDIT:
		return iEditButton;
	case BTN_DELETE:
		return iDeleteButton;
	}
	return NULL;
}	

private	imeth	int	pEnableButton(object self, int btn, int enable)
{
	int	pv = iDisabledButtons & btn;
	
	if (enable < 0)
		return !pv;
	
	if (!enable)
		iDisabledButtons |= btn;
	else
		iDisabledButtons &= ~btn;

	if (iInDialog) {
		object	ctl = pGetControl(self, btn);
		if (!enable)
			gDisable(ctl);
		else
			gEnable(ctl);
	}
	
	return !pv;
}

private	imeth	int	pDisplayButton(object self, int btn, int show)
{
	int	pv = iHiddenButtons & btn;

	if (show < 0)
		return !pv;
	
	if (!show)
		iHiddenButtons |= btn;
	else
		iHiddenButtons &= ~btn;

	if (iInDialog) {
		object	ctl = pGetControl(self, btn);
		if (!show)
			gHide(ctl);
		else
			gDisplay(ctl);
	}
	
	return !pv;
}

imeth	int	gEnableButton(char *btn, int val)
{
	return pEnableButton(self, getIDFromName(btn), val);
}

imeth	int	gDisplayButton(char *btn, int val)
{
	return pDisplayButton(self, getIDFromName(btn), val);
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	org = (ofun) iFun;
	iFun = fun;

	if (iSelListBox)
		gSetFunction(iSelListBox, fun);

	return org;
}

imeth	gSetButtonFunctions(int (*aFun)(), int (*eFun)(), int (*dFun)())
{
	iAddFun = aFun;
	pDisplayButton(self, BTN_ADD, aFun ? 1 : 0);
	iEditFun = eFun;
	pDisplayButton(self, BTN_EDIT, eFun ? 1 : 0);
	iDelFun = dFun;
	pDisplayButton(self, BTN_DELETE, dFun ? 1 : 0);

	return self;
}

imeth	gDialog()
{
	return iWnd;
}

imeth	gSetListStyle(int flags)
{
	if (iVirtual)
		return NULL;

	iLBStyle = flags;

	if (iInDialog)
		gSetStyle(iSelListBox, iLBStyle);

	return self;
}








