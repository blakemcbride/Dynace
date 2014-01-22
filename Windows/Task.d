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




defclass Task : PropertyList  {
	iTaskList;
	iTag;				/*  arbitrary tag object associated with dlg */
	int	iAutoDisposeTag;
};



cmeth	gNewTask(tl)
{
	object	obj = gNew(super);
	accessIVsOf(obj);

	iTaskList = tl;
	
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gGetParent()
{
	return gGetParent(iTaskList);
}

imeth	gNextTask(int result)
{
	return gNextTask(iTaskList, result);
}

imeth	gRunRun()
{
	return self;
}

imeth	gRunClean()
{
	return self;
}

imeth	gRunSave()
{
	return self;
}

imeth	gRunClose()
{
	return self;
}

imeth	gTaskList()
{
	return iTaskList;
}

imeth	gSetPosition(int y, int x)
{
	return self;
}

imeth	gGetPosition(int *y, int *x)
{
	return self;
}

imeth	gSetFocus()
{
	return self;
}

imeth	int	gCheckValue()
{
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

imeth	long	gGetTaskID()
{
	object	t = gPropertyGet(self, "taskid");
	return t ? gLongValue(t) : 0L;
}

imeth	long	gGetOrder()
{
	object	t = gPropertyGet(self, "order");
	return t ? gLongValue(t) : 0L;
}

imeth	long	gGetSubOrder()
{
	object	t = gPropertyGet(self, "suborder");
	return t ? gLongValue(t) : 0L;
}






