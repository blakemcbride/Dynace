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





defclass WorkFlow {
	iTaskListDetail;
class:
	cTasks;
init:	class_init;
};

#define	DICTSIZE	1001


cmeth	gAddWFTask(long id, cls, ofun task)
{
	if (gFindInt(cTasks, (int) id))
		gDeepDisposeInt(cTasks, (int) id);
	gAddInt(cTasks, (int) id, vNew(WorkFlowAssociation, cls, task));
	return self;
}

cmeth	gFindValueInt(int tid)
{
	return gFindValueInt(cTasks, tid);
}

imeth	gSetWorkFlowState(db, long taskid, char state)
{
	object	key=NULL, val;
	int	r=1;

	while (val=gFindNext(iTaskListDetail, &key))
		if (gGetTaskID(key) == taskid) {
			gPropertyPut(key, "State", 1, gNewWithChar(Character, state)); 
			r = 0;
			break;
		}
	return r ? NULL : self;
}

imeth	gCreateWorkFlow(db, long tgid)
{
	return gCreateWorkFlowWithFun(self, db, tgid, NULL, NULL);
}

imeth  	gCreateWorkFlowWithFun(db, long tgid, void (*fun)(), object arg)
{
	object	gstmt = gNewStatement(db), tldr;
	char	select[256];
	int	r, order;
	long	taskid;
	static	object	val;

	if (iTaskListDetail)
		gDeepDispose(iTaskListDetail);
	iTaskListDetail = gNew(TaskListDetail);
	if (!val)
		val = gNew(Constant);

	sprintf(select, "select TGID, TGDOrder, Tasks.TaskID, SubTasks, TaskName, TaskType, TaskClass, TrackTask"
		 " from TaskGroupDetail, Tasks where TGID=%ld AND Tasks.TaskID = TaskGroupDetail.TaskID", tgid);
	r = gDBSelectOne(gstmt, select);
	while (!r) {
		tldr = gNewTaskListDetailRecord(TaskListDetailRecord, order=gFldGetShort(gstmt, "TGDOrder"), 0);

		gPropertyPut(tldr, "SubTasks", 1, gNewWithChar(Character, gFldGetChar(gstmt, "SubTasks") == 'Y' ? 'Y' : 'N'));
		gPropertyPut(tldr, "TaskID", 1, gNewWithLong(LongInteger, taskid=gFldGetLong(gstmt, "TaskID")));
		gPropertyPut(tldr, "State", 1, gNewWithChar(Character, 'A'));
		gPropertyPut(tldr, "TaskName", 1, gNewWithStr(String, gFldGetString(gstmt, "TaskName")));
		gPropertyPut(tldr, "TaskType", 1, gNewWithChar(Character, gFldGetChar(gstmt, "TaskType")));
		gPropertyPut(tldr, "TaskClass", 1, gNewWithStr(String, gFldGetString(gstmt, "TaskClass")));
		gPropertyPut(tldr, "TrackTask", 1, gNewWithChar(Character, gFldGetChar(gstmt, "TrackTask")));

		gAddValue(iTaskListDetail, tldr, val);
		if (fun)
			fun(arg, tldr, order, 0, taskid);
		r = gNextRecord(gstmt);
	}
	gDispose(gstmt);
	return iTaskListDetail;
} 

imeth	gDisposeTaskListDetail()
{
	if (iTaskListDetail)
		iTaskListDetail = gDeepDispose(iTaskListDetail);
	return NULL;
}

imeth	gTaskListDetail()
{
	return iTaskListDetail;
}

static	void	class_init()
{
	cTasks = gNewWithInt(IntegerDictionary, DICTSIZE);
}









