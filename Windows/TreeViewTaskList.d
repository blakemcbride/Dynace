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




defclass TreeViewTaskList : PropertyList {
	iParentWindow;	

	iSelectionDlg;
	iTreeView;
	int	iAlphabetize;

	HWND	iZOrder;

	int	iSModal;

	iCurrentTask;		//  TreeViewItem instance
	iDesiredTask;

	int	iInDispose;

	iFunctions;		//  Function called when tasklist completes
	int	(*iChgFun)();	//  Called when different task selected

	int	iRow;		//  position of previous dialog
	int	iCol;

	int	iInRunRun;


	//  WorkFlow reset vars
	int	iReset;
	long	iOrder;
	long	iSubOrder;
	int	iBeginning;

	iTag;				/*  arbitrary tag object associated with dlg */
	int	iAutoDisposeTag;
};


#ifndef	_WIN32
object	TreeView_c;
#endif

static	int	completion(object dlg, int res);
static	int	check_dialog_values(object tv, object val, char *errmsg);
static	int	selectProcess(object tv, object ftvi, object ttvi);


private	imeth	init(unsigned dlg_id, unsigned tv_id, pwind, ifun fun)
{
	iParentWindow = pwind;

	iSelectionDlg = mNewDialog(ModelessDialog, dlg_id, pwind);
	gSetPosition(iSelectionDlg, 12, 72);
	gSetZOrder(self, HWND_TOPMOST);
	gPropertyPut(iSelectionDlg, "TreeViewTaskList", 0, self);
	gCompletionFunction(iSelectionDlg, completion);
	if (fun)
		gInitFunction(iSelectionDlg, fun);

	iTreeView = mAddControl(iSelectionDlg, TreeView, tv_id);
	gSetSelFun(iTreeView, selectProcess);
	gCheckFunction(iTreeView, check_dialog_values);
	if (iAlphabetize)
		gAlphabetize(iTreeView);
	iRow = iCol = -1;
	iCurrentTask = NULL;

	gDisposeAtExit(Application, self);

	return self;
}

cmeth	gNewTaskList(unsigned dlg_id, unsigned tv_id, pwind, ifun fun)
{
	return init(gNew(super), dlg_id, tv_id, pwind, fun);
}

imeth	gDispose, gDeepDispose() 
{
	if (iInDispose)
		return NULL;
	iInDispose = 1;

	if (iSelectionDlg)
		gDispose(iSelectionDlg);

	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);

	if (iFunctions)
		gDeepDispose(iFunctions);

	gRemoveAtExit(Application, self);
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gAddTaskWithImage(proc, char *option, int image)
{
	object	itm = gAddTVItemWithImage(iTreeView, NULL, image, option);
	gPropertyPut(itm, "Task", 1, proc);
	return itm;
}

imeth	gAddSubTaskWithImage(proc, char *option, parent, int image)
{
	object	itm = gAddTVItemWithImage(iTreeView, parent, image, option);
	gPropertyPut(itm, "Task", 1, proc);
	return itm;
}

imeth	gAddTask(proc, char *option)
{
	return gAddTaskWithImage(self, proc, option, 0);
}

imeth	gAddSubTask(proc, char *option, parent)
{
	return gAddSubTaskWithImage(self, proc, option, parent, 0);
}

private	imeth	pNextTask()
{
	object	nextTask = iCurrentTask;

	do {
		nextTask = nextTask ? gNext(nextTask) : NULL;
		if (!nextTask)
			nextTask = gFirst(iTreeView);
	} while (nextTask  &&  !gPropertyGet(nextTask, "Task"));  //  while it is a disabled item
	return nextTask;
}	

// mode = 0 -- by ID
// mode = 1 -- by Name
private	imeth	pGetTask(object self, int mode, long taskid, char *tname)
{
	object	nextTask = iCurrentTask;
	object	taskObj;
	long	firstID = 0L, id = 0L;
	int	ok = 1;
	char	*cname;
	
	if ((!mode && !taskid) || (mode && !tname))
		return pNextTask(self);
	
	do {
		nextTask = nextTask ? gNext(nextTask) : NULL;
		if (!nextTask)
			nextTask = gFirst(iTreeView);
		if (taskObj = gPropertyGet(nextTask, "Task"))
			id = gLongValue(gPropertyGet(taskObj, "taskid"));
		cname = gStringValue(nextTask);
		
		if (!firstID)
			firstID = id;
		else if (firstID == id)
			ok = 0;
	} while (ok  &&  nextTask  &&  (!taskObj  || (!mode && id != taskid) || (mode && stricmp(cname, tname))));
	
	return nextTask;
}	

imeth	int	gPerform()
{
	object	first;

	gPerform(iSelectionDlg);
	if (first = pNextTask(self))
		gSelect(first);
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

private	imeth	pResetTaskList(int next)
{
	long	order = iOrder;
	long	suborder = iSubOrder;

	iOrder = iSubOrder = 0L;
	iInDispose++;
	gRemoveAll(iTreeView);
	iInDispose--;
//	gPerform(self);
	gPerformTVWorkFlow(self, order, suborder, next, -1);
	return self;
}

static	int	selectProcess(object ctl, object ftvi, object ttvi)
{
	object	dlg  = gDialog(ctl);
	object	self = gPropertyGet(dlg, "TreeViewTaskList");
	accessIVs;
	object	active, current = iCurrentTask;

	while (1) {
		if (ttvi  &&  !gPropertyGet(ttvi, "Task")) {
			if (ftvi)
				gSelect(ftvi);
			return 0;
		}
		if (!ttvi  ||  gCheckValue(self)) {
			if (iCurrentTask)
				gSelect(iCurrentTask);
			else
				gDeselect(ctl);
			return 0;
		}
			
		if (iCurrentTask) {
			int	pmode;
			active = gPropertyGet(iCurrentTask, "Task");
			pmode = gSetScalingMode(Application, SM_PIXELS);
			gGetPosition(active, &iRow, &iCol);
			gSetScalingMode(Application, pmode);
			gRunSave(active);
			gRunClose(active);
			iCurrentTask = NULL;
		}

		if (iChgFun)
			iChgFun(self, current, ttvi);

		if (gTaskListDetail(self)) {
			if (ttvi  &&  !iBeginning) {
				object	task = gPropertyGet(ttvi, "Task");
				object	order, suborder;

				order = gPropertyGet(task, "order");
				suborder = gPropertyGet(task, "suborder");
				iOrder = gLongValue(order);
				iSubOrder = gLongValue(suborder);
			} else {
				iOrder = iSubOrder = 0;
				iBeginning = 0;
			}
			if (iReset) {
				pResetTaskList(self, 0);
				iReset = 0;
			}
			ttvi = gGetSelection(iTreeView);
		}
		if (ttvi) {
			active = gPropertyGet(ttvi, "Task");
			iCurrentTask = ttvi;
			iInRunRun++;
			gRunRun(active);
			iInRunRun--;
			if (iCurrentTask != ttvi) {
				//  someone called gNextTask from within their RunRun
				iCurrentTask = ftvi = ttvi;
				if (iDesiredTask) {
					ttvi = iDesiredTask;
					iDesiredTask = NULL;
				} else if (IsObj(self))
					ttvi = pNextTask(self);
				else
					break;
				gSelect(ttvi);
				continue;
			}
		}
		break;
	}
	return 0;
}

// mode = 0 -- by ID
// mode = 1 -- by Name
private	imeth	pGoToTask(object self, int result, int mode, long taskid, char *tname)
{
	object	nextTask, active, current = iCurrentTask, nextTask2;
	int	desTask = 0;

	iDesiredTask = NULL;
	if (iInRunRun) {
		iDesiredTask = pGetTask(self, mode, taskid, tname);
		iCurrentTask = NULL;  //  signal NextTask
		return self;
	}
	nextTask = iCurrentTask;
	while (1) {
		if (desTask)
			nextTask2 = nextTask;
		else if (IsObj(self))
			nextTask2 = pGetTask(self, mode, taskid, tname);
		else
			break;
		if (iCurrentTask) {
			int	pmode;
			active = gPropertyGet(iCurrentTask, "Task");
			pmode = gSetScalingMode(Application, SM_PIXELS);
			gGetPosition(active, &iRow, &iCol);
			gSetScalingMode(Application, pmode);
			if (result > 0)
				gRunSave(active);
			else
				gRunClean(active);
			gRunClose(active);
			iCurrentTask = NULL;
		}

		if (iChgFun)
			iChgFun(self, current, nextTask);

		if (gTaskListDetail(self)) {
#if 1
			if (nextTask  &&  !iBeginning) {
				object	task = gPropertyGet(nextTask, "Task");
				object	order, suborder;

				order = gPropertyGet(task, "order");
				suborder = gPropertyGet(task, "suborder");
				iOrder = gLongValue(order);
				iSubOrder = gLongValue(suborder);
			} else {
				iOrder = iSubOrder = 0;
				iBeginning = 0;
			}
#else
			if (iBeginning) {
				iOrder = iSubOrder = 0;
				iBeginning = 0;
			}
#endif
			if (iReset) {
				pResetTaskList(self, 1);
//				pResetTaskList(self, 0);
				iReset = 0;
				nextTask = gGetSelection(iTreeView);
			} else
				nextTask = nextTask2;
		}
		if (nextTask) {
#if 0
			while (1) {
					nextTask = 0;
				link = gNth(iTasks, nextTask+1);
				if (link  &&  (active=gValue(link)))
					break;
				nextTask++;
			}
#endif
			active = gPropertyGet(nextTask, "Task");
			gSelect(nextTask);
			iCurrentTask = nextTask;
			iInRunRun++;
			gRunRun(active);
			iInRunRun--;
			if (iCurrentTask != nextTask) {
				//  someone called gNextTask from within their RunRun
				iCurrentTask = nextTask;
				if (iDesiredTask) {
					nextTask = iDesiredTask;
					iDesiredTask = NULL;
					desTask = 1;
				}
//				nextTask = pNextTask(self);
//				iCurrentTask = NULL;
//				nextTask = pNextTask(self);
				gSelect(nextTask);
				continue;
			}
			gSelect(nextTask);
		}
		break;
	}
	return self;
}

imeth	gNextTask(int result)
{
	return pGoToTask(self, result, 0, 0L, NULL);
}

#if 0
//imeth	gGoToTaskByID(int result, long taskid)
{
	return pGoToTask(self, result, 0, taskid, NULL);
}
#endif

imeth	gGoToTask(int result, char *tname)
{
	return pGoToTask(self, result, 1, 0L, tname);
}

imeth	gSelectItem(tvi)
{
	selectProcess(iTreeView, gGetSelection(iTreeView), tvi);
	return self;
}

imeth	gGetSelection()
{
	return gGetSelection(iTreeView);
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

imeth	ofun	gSetChgFunction(int (*fun)())
{
	ifun	orgfun = iChgFun;
	iChgFun = fun;
	return (ofun) orgfun;
}

static	int	completion(object dlg, int res)
{
	object	active, obj = gPropertyGet(dlg, "TreeViewTaskList");
	accessIVsOf(obj);

	if (iCurrentTask) {
		active = gPropertyGet(iCurrentTask, "Task");
			/*  if res then check_dialog_values() already executed RunSave() otherwise...  */
		if (!res)
			gRunClean(active);
		gRunClose(active);
	}
	iCurrentTask = NULL;
	
	if (iFunctions)
		gExecuteFunctionsObjInt(iFunctions, obj, res);

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
/*
	if (iCurrentTask)
		gSetFocus(gValue(gNth(iTasks, iCurrentTask+1)));
*/
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

imeth	int	gCheckValue()
{
	if (iCurrentTask)
		return gCheckValue(gPropertyGet(iCurrentTask, "Task"));
	return 0;
}

imeth	gResetWorkFlow()
{
	if (iCurrentTask) {
		object	task = gPropertyGet(iCurrentTask, "Task");
		object	order, suborder;

		order = gPropertyGet(task, "order");
		suborder = gPropertyGet(task, "suborder");
		iOrder = gLongValue(order);
		iSubOrder = gLongValue(suborder);
		iBeginning = 0;
	}
	iReset = 1;
	return self;
}

imeth	gResetWorkFlowFromBeginning()
{
	iOrder = 0L;
	iSubOrder = 0L;
	iBeginning = 1;
	iReset = 1;
	return self;
}

static	int	check_dialog_values(object tv, object val, char *errmsg)
{
	object	tldlg = gDialog(tv);
	object	self = gPropertyGet(tldlg, "TreeViewTaskList");
	object	active=NULL;
	accessIVs;
	
	*errmsg = '\0';
	if (iCurrentTask)
		active = gPropertyGet(iCurrentTask, "Task");
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

imeth	int	gInternalMessage(int n)
{
	return gInternalMessage(iTreeView, n);
}

imeth	gCurrentTask()
{
	return iCurrentTask;
}

imeth	gNewImageList(int cx, int cy, unsigned flags, int numb, int grow)
{
	return gNewImageList(iTreeView, cx, cy, flags, numb, grow);
}

imeth	int	gLoadBitmap(unsigned resID)
{
	return gLoadBitmap(iTreeView, resID);
}

imeth	gAlphabetize()
{
	iAlphabetize = 1;
	if (iTreeView)
		gAlphabetize(iTreeView);
	return self;
}

imeth	gAlphabetizeOff()
{
	iAlphabetize = 0;
	if (iTreeView)
		gAlphabetizeOff(iTreeView);
	return self;
}

imeth	gResort()
{
	if (iTreeView)
		gResort(iTreeView);
	return self;
}






