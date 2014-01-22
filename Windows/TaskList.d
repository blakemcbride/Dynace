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




defclass TaskList : PropertyList {
	iParentWindow;	

	iSelectionDlg;
	iListBox;

	iTasks;

	HWND	iZOrder;

	int	iSModal;

	int	iCurrentTask;

	int	iInDispose;

	iFunctions;		/*  Function called when tasklist completes    */

	int	iRow;		//  position of previous dialog
	int	iCol;

	int	iInRunRun;


	//  WorkFlow reset vars
	long	iTldid;

	iTag;				/*  arbitrary tag object associated with dlg */
	int	iAutoDisposeTag;
};


static	int	selectProcess(object ctl, object dlg);
static	int	completion(object dlg, int res);
static	int	check_dialog_values(object lb, object val, char *errmsg);


cmeth	gNewTaskList(unsigned dlg_id, unsigned lb_id, pwind, ifun fun)
{
	object	obj = gNew(super);
	accessIVsOf(obj);

	iParentWindow = pwind;

	iSelectionDlg = mNewDialog(ModelessDialog, dlg_id, pwind);
	gSetPosition(iSelectionDlg, 12, 72);
	gSetZOrder(obj, HWND_TOPMOST);
	gPropertyPut(iSelectionDlg, "TaskList", 0, obj);
	gCompletionFunction(iSelectionDlg, completion);
	if (fun)
		gInitFunction(iSelectionDlg, fun);

	iListBox = mAddControl(iSelectionDlg, ListBox, lb_id);
	gSetChgFunction(iListBox, selectProcess);
	gCheckFunction(iListBox, check_dialog_values);

	iTasks = gNew(LinkList);

	iRow = iCol = iCurrentTask = -1;

	gDisposeAtExit(Application, obj);

	return obj;
}

imeth	gDispose, gDeepDispose() 
{
	if (iInDispose)
		return NULL;
	iInDispose = 1;

	gDispose(iSelectionDlg);
	gDeepDispose(iTasks);

	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);

	if (iFunctions)
		gDeepDispose(iFunctions);
	
	gRemoveAtExit(Application, self);
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gAddTask(proc, char *option)
{
	object	link = gNewWithObj(LinkValue, proc);
	gAddLast(iTasks, link);
	gAddOption(iListBox, option);
	return link;
}

imeth	int	gPerform()
{
	if (-1 == gShortValue(iListBox))
		gSetShortValue(iListBox, 0);
	gPerform(iSelectionDlg);
	selectProcess(iListBox, iSelectionDlg);
	return 0;
}

imeth	int	gPerformModal()
{
	if (iParentWindow) {
		gDisable(iParentWindow);
		iSModal = 1;
	}
	return gPerform(self);
}

private	imeth	pResetTaskList(int a)
{
	long	tldid = iTldid;
	int	n = gShortValue(iListBox);

	iTldid = 0L;
	iInDispose++;
	gDeepDispose(iTasks);
	iInDispose--;
	iTasks = gNew(LinkList);
	gRemoveAll(iListBox);
	gPerformWorkFlow(self, tldid, (long) a * gTaskID(self, n), -1);
	return self;
}

static	int	selectProcess(object ctl, object dlg)
{
	int	ival = gShortValue(ctl);
	object	self = gPropertyGet(dlg, "TaskList");
	object	link, active;
	accessIVs;

	while (1) {
		link = gNth(iTasks, ival+1);
		if (!link  ||  !(active = gValue(link))) {
			gSetShortValue(ctl, iCurrentTask);
			return 0;
		}

		/*  if switching to a new process make sure all dialog field values are ok  */
		if (gCheckValue(self)) {
			gSetShortValue(ctl, iCurrentTask);
			return 0;
		}

		if (iCurrentTask >= 0) {
			link = gNth(iTasks, iCurrentTask+1);
			if (link) {
				int	pmode;
				active = gValue(link);
				pmode = gSetScalingMode(Application, SM_PIXELS);
				gGetPosition(active, &iRow, &iCol);
				gSetScalingMode(Application, pmode);
				gRunSave(active);
				gRunClose(active);
			}
			iCurrentTask = -1;
		}

		if (iTldid)
			pResetTaskList(self, 1);
		else if (ival >= 0) {
			link = gNth(iTasks, ival+1);
			if (link) {
				active = gValue(link);
				iCurrentTask = ival;
				iInRunRun++;
				gRunRun(active);
				iInRunRun--;
				if (iCurrentTask != ival) {
					//  someone called gNextTask from within their RunRun
					iCurrentTask = ival;
					if (++ival >= gSize(iTasks))
						ival = 0;
					gSetShortValue(ctl, ival);
					continue;
				}
			}
		}
		break;
	}
	return 0;
}

imeth	gNextTask(int result)
{
	int	nextTask, nTasks = gSize(iTasks);
	object	link, active;

	if (iInRunRun) {
		iCurrentTask++;
		return self;
	}
	nextTask = iCurrentTask + 1;
	while (1) {
		if (iCurrentTask >= 0) {
			link = gNth(iTasks, iCurrentTask+1);
			if (link) {
				int	pmode;
				active = gValue(link);
				pmode = gSetScalingMode(Application, SM_PIXELS);
				gGetPosition(active, &iRow, &iCol);
				gSetScalingMode(Application, pmode);
				if (result > 0)
					gRunSave(active);
				else
					gRunClean(active);
				gRunClose(active);
			}
		}
		iCurrentTask = -1;

		if (iTldid)
			pResetTaskList(self, -1);
		else {
			while (1) {
				if (nextTask >= nTasks)
					nextTask = 0;
				link = gNth(iTasks, nextTask+1);
				if (link  &&  (active=gValue(link)))
					break;
				nextTask++;
			}
			iCurrentTask = nextTask;
			gSetShortValue(iListBox, nextTask);
			iInRunRun++;
			gRunRun(active);
			iInRunRun--;
			if (iCurrentTask != nextTask) {
				//  someone called gNextTask from within their RunRun
				iCurrentTask = nextTask++;
				continue;
			}
			gSetShortValue(iListBox, nextTask);
		}
		break;
	}
	return self;
}

imeth	gGetParent()
{
	return iParentWindow;
}

imeth	gCompletionFunction(int (*fun)())
{
	if (iFunctions)
		gDeepDispose(iFunctions);
	
	iFunctions = gNew(FunctionList);
	
	gAddFunctionBefore(iFunctions, fun);
	return self;
}

imeth	gAddCompletionFunctionBefore(int (*fun)())
{
	if (!iFunctions)
		iFunctions = gNew(FunctionList);

	gAddFunctionBefore(iFunctions, fun);
	
	return self;
}

imeth	gAddCompletionFunctionAfter(int (*fun)())
{
	if (!iFunctions)
		iFunctions = gNew(FunctionList);

	gAddFunctionAfter(iFunctions, fun);
	
	return self;
}

static	int	completion(object dlg, int res)
{
	object	link, active, obj = gPropertyGet(dlg, "TaskList");
	accessIVsOf(obj);

	if (iCurrentTask >= 0) {
		link = gNth(iTasks, iCurrentTask+1);
		if (link) {
			active = gValue(link);
			/*  if res then check_dialog_values() already executed RunSave() otherwise...  */
			if (!res)
				gRunClean(active);
			gRunClose(active);
		}
	}
	iCurrentTask = -1;
	
	if (iFunctions)
		gExecuteFunctionsObj(iFunctions, obj);

	gDispose(obj);
	if (iSModal  &&  iParentWindow) {
		gEnable(iParentWindow);
		gSetFocus(iParentWindow);
	}
	return	res;
}

imeth	gGetPosition(int *y, int *x)
{
	*y = iRow;
	*x = iCol;
	gScaleToCurrentMode(Application, y, x, NULL);
	return self;
}

imeth	gDisable()
{
	gSetZOrder(iSelectionDlg, HWND_NOTOPMOST);
	gDisable(iSelectionDlg);
	return self;
}

imeth	gEnable()
{
	gEnable(iSelectionDlg);
	gSetZOrder(iSelectionDlg, iZOrder);
	if (iCurrentTask >= 0  &&  iCurrentTask < gSize(iTasks))
		gSetFocus(gValue(gNth(iTasks, iCurrentTask+1)));
	return self;
}

imeth	gSetZOrder(HWND m)
{
	gSetZOrder(iSelectionDlg, iZOrder=m);
	return self;
}

imeth	gDialog()
{
	return iSelectionDlg;
}

imeth	gSetShortValue(int val)
{
	gSetShortValue(iListBox, val);
	return self;
}

imeth	int	gCheckValue()
{
	object	link;

	if (iCurrentTask >= 0)
		if (link = gNth(iTasks, iCurrentTask+1))
			return gCheckValue(gValue(link));
	return 0;
}

/*  WorkFlow changed and this class hasn't been updated.
imeth	gResetWorkFlow(long tldid)
{
	iTldid = tldid;
	return self;
}
*/

static	int	check_dialog_values(object lb, object val, char *errmsg)
{
	object	tldlg = gDialog(lb);
	object	self = gPropertyGet(tldlg, "TaskList");
	object	link, active=NULL;
	accessIVs;
	
	*errmsg = '\0';
	if (iCurrentTask >= 0)
		if (link = gNth(iTasks, iCurrentTask+1))
			active = gValue(link);
	if (gCheckValue(self)) {
		strcpy(errmsg, "Click OK or Cancel on the dialog to save or abort data.");
		return 1;
	}
	if (active)
		gRunSave(active);
	return 0;
}

imeth	gAutoDisposeTag()
{
	iAutoDisposeTag = 1;
	return self;
}

imeth	gSetTag(tag)
{
	object	ptag = iTag;
	iTag = tag;
	if (ptag  &&  iAutoDisposeTag)
		return gDeepDispose(ptag);
	else
		return ptag;
}

imeth	gGetTag()
{
	return iTag;
}

imeth	gCurrentTask()
{
	object	rval = NULL;
	
	if (iCurrentTask >= 0) {
		object	link = gNth(iTasks, iCurrentTask+1);
		
		if (link) {
			int	pmode;
			rval = gValue(link);
		}
	}
	return rval;
}







