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




defclass TreeViewWorkFlow : TreeViewTaskList, WorkFlow {
	iDB;
	int	iDataMode;
	long	iOrder;
	long	iSubOrder;
	int	iNext;
	int	iAlreadyPerformed;
	int	iInDispose;
};



static	int	initFun(object dlg);


cmeth	gNewWorkFlow(db, unsigned dlg, unsigned lb, wind)
{
	object	obj = gNewTaskList(super, dlg, lb, wind, initFun);
	ivType	*iv = ivPtr(obj);
	iDB = db;
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	if (iInDispose)
		return NULL;
	iInDispose = 1;
	gDisposeTaskListDetail(self);
	return gDeepDispose(super);
}

typedef	void	(*vfun)();

static	int	initFun(object dlg)
{
	object	proc, self = gIsKindOf(dlg, TreeViewWorkFlow) ? dlg : gPropertyGet(dlg, "TreeViewTaskList"), parent=NULL;
	accessIVs;
	object	wfa, itm, pos = NULL, tldr, val, anyItem=NULL;
	int	r, next=0;
	long	lastOrder = 0;
	char	buf[128];
	object	tld;
	int	set_next_item = 0;

	val = gFindFirst(tld=gTaskListDetail(self), &tldr);
	while (val) {
		long	order = gGetOrder(tldr);
		long	suborder = gGetSubOrder(tldr);
		long	next_suborder;
		if (!suborder  ||  order == iOrder) {
			char	state;
			int	image;
			object	io;

			io = gPropertyGet(tldr, "Image");
			image = io ? gLongValue(io) : 0;
			state = gCharValue(gPropertyGet(tldr, "State"));
			if (state != 'H') {
				long	taskid = gLongValue(gPropertyGet(tldr, "TaskID"));
				char	subTasks = gCharValue(gPropertyGet(tldr, "SubTasks"));
/*
                                if (suborder  &&  iOrder != order) {
                                        if (order != lastOrder) {
                                                lastOrder = order;
                                                itm = gAddSubTaskWithImage(self, NULL, "Expanding...", parent, image);
                                        }
                                } else
*/
				if (state != 'D') {
					char	type = gCharValue(gPropertyGet(tldr, "TaskType"));

					wfa = gFindValueInt(CLASS, (int) taskid);
					if (!wfa) {
						sprintf(buf, "Task ID %ld is not associated with a function.", gGetTaskID(tldr));
						gError(self, buf);
					}
					if (type == 'S'  ||  type == 'W'  ||  type == 'G' || type == 'J')
						proc = gFunction(wfa)(gClass(wfa), self, gStringValue(gPropertyGet(tldr, "TaskClass")));
					else
						proc = gFunction(wfa)(gClass(wfa), self, taskid);

					gPropertyPut(proc, "taskid", 1, gNewWithLong(LongInteger, taskid));
					gPropertyPut(proc, "order", 1, gNewWithLong(LongInteger, order));
					gPropertyPut(proc, "suborder", 1, gNewWithLong(LongInteger, suborder));
					gPropertyPut(proc, "subsubTasks", 1, gNewWithChar(Character, subTasks));
					gPropertyPut(proc, "TrackTask", 1, gCopy(gPropertyGet(tldr, "TrackTask")));

					gSetTag(proc, self);
					anyItem = itm = gAddSubTaskWithImage(self, proc, gStringValue(gPropertyGet(tldr, "TaskName")),
							  suborder ? parent : NULL, image);
					gPropertyPut(itm, "order", 1, gNewWithLong(LongInteger, order));
					gPropertyPut(itm, "suborder", 1, gNewWithLong(LongInteger, suborder));
					gPropertyPut(tldr, "TreeViewItem", 0, itm);
					if (next) {
						pos = itm;
						next = 0;
					}
					if (set_next_item) {
						pos = itm;
						set_next_item = 0;
					}
					if (!pos)
						pos = itm;
					if (iOrder == order  &&  iSubOrder == suborder)
						pos = itm;
				} else {
					sprintf(buf, "* %s", gStringValue(gPropertyGet(tldr, "TaskName")));
					itm = gAddSubTaskWithImage(self, NULL, buf, suborder ? parent : NULL, image);
					gPropertyPut(itm, "order", 1, gNewWithLong(LongInteger, order));
					gPropertyPut(itm, "suborder", 1, gNewWithLong(LongInteger, suborder));
					gPropertyPut(tldr, "TreeViewItem", 0, itm);
					if (iOrder == order  &&  iSubOrder == suborder)
						next = 1;
				}
				{
					object	tldr2;
					val = gFindGT(tld, tldr, &tldr2);
					next_suborder = val ? gGetSubOrder(tldr2) : 0;
				}
				if (subTasks == 'Y'  &&  (iOrder != order  ||  iNext &&  next  &&  !next_suborder)) {
					object titm = gAddSubTaskWithImage(self, NULL, "Expanding...", itm, image);
					gPropertyPut(titm, "order", 1, gNewWithLong(LongInteger, order));
					gPropertyPut(titm, "suborder", 1, gNewWithLong(LongInteger, suborder));
					gPropertyPut(tldr, "TreeViewItem", 0, titm);
				}
				if (!suborder)
					parent = itm;
			} else if (iOrder == order  &&  iSubOrder == suborder) {
				iNext = 0;
				set_next_item = 1;
			}
		}
		val = gFindNext(tld, &tldr);
	}
	if (pos) {
		char	*name;
		
		if (iNext)
			if (!(pos = gNext(pos)))
				pos = gFirst(anyItem);
		if (pos) {
			while (1) {
				name = gStringValue(pos);
				if (name[0] == '*'  &&  name[1] == ' ') {
					pos = gNext(pos);
					if (!pos)
						pos = gFirst(anyItem);
				} else
					break;
			}
			if (gChildren(pos)) {
				object	child = gChild(pos);
				name = gStringValue(child);
				if (strcmp(name, "Expanding..."))
					gExpand(pos);
			}
			gSelect(pos);
		}
	}
	return 0;
}

imeth	gPerformTVWorkFlow(long order, long suborder, int next, int dm)
{
	if (dm != -1)
		iDataMode = dm;
	iOrder = order;
	iSubOrder = suborder;
	iNext = next;
	gPerform(self);
	if (iAlreadyPerformed)
		initFun(self);
	else {
		iAlreadyPerformed = 1;
		gNextTask(self, 0);  //  get the first RunRun going
	}
	return self;
}

imeth	int	gGetDataMode()
{
	return iDataMode;
}

imeth	int	gSetDataMode(int dm)
{
	iDataMode = dm;
	return iDataMode;
}









