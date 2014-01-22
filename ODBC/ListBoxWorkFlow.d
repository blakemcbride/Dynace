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




defclass ListBoxWorkFlow : TaskList, WorkFlow {
	iDB;
	int	iDataMode;
	iTasks;

	int	iInDispose;
};



cmeth	gNewWorkFlow(db, unsigned dlg, unsigned lb, wind)
{
	object	obj = gNewTaskList(super, dlg, lb, wind, NULL);
	ivType	*iv = ivPtr(obj);
	iDB = db;
	iTasks = gNew(LinkObject);
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	if (iInDispose)
		return NULL;
	iInDispose = 1;
	
	gDeepDispose(iTasks);
	return gDeepDispose(super);
}

imeth	gPerformWorkFlow(long tldid, long taskid, int dm)
{
	object	stmt = gNewStatement(iDB), wfa, proc;
	char	select[256];
	int	r, pos=0, n, sign, next=0;

	if (sign = taskid < 0L)
		taskid = -taskid;
	if (gSize(iTasks)) {
		gDeepDispose(iTasks);
		iTasks = gNew(LinkObject);
	}
	if (dm != -1)
		iDataMode = dm;
	sprintf(select,
		"select TLDID, TLDOrder, TLD.TaskID, Tasks.TaskID, State, TaskName, TaskType, TaskClass "
		"from TaskListDetail AS TLD, Tasks "
		"where TLDID=%ld and TLD.TaskID = Tasks.TaskID "
		"order by TLDID, TLDOrder",
		tldid);
	r = gDBSelectOne(stmt, select);
	for (n=0 ; !r ; ) {
		char	state;

		state = *gFldGetString(stmt, "State");
		if (state != 'H') {
			long	taskid2 = gFldGetLong(stmt, "TaskID");
			if (state != 'D') {
				char	type = *gFldGetString(stmt, "TaskType");

				wfa = gFindValueInt(CLASS, (int) taskid2);
				if (!wfa) {
					sprintf(select, "Task ID %ld is not associated with a function.", gFldGetLong(stmt, "TaskID"));
					gError(self, select);
				}
				if (type == 'S'  ||  type == 'W'  ||  type == 'G' || type == 'J')
					proc = gFunction(wfa)(gClass(wfa), self, gFldGetString(stmt, "TaskClass"));
				else
					proc = gFunction(wfa)(gClass(wfa), self, taskid2);
				gAddTask(self, proc, gFldGetString(stmt, "TaskName"));
				if (next) {
					pos = n;
					next = 0;
				}
			} else {
				sprintf(select, "* %s", gFldGetString(stmt, "TaskName"));
				gAddTask(self, NULL, select);
			}
			gAddLast(iTasks, gNewWithLong(LongInteger, taskid2));
			if (taskid == taskid2)
				if (sign)
					next = 1;
				else
					pos = n;
			n++;
		}
		r = gNextRecord(stmt);
	}
	gDispose(stmt);
	gSetShortValue(self, pos);
	gPerform(self);
	return self;
}

imeth	long	gTaskID(int n)
{
	if (n < 0  ||  n >= gSize(iTasks))
		return 0L;
	return gLongValue(gNth(iTasks, n+1));
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









