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



defclass  TableListbox {
	iListbox;
	iStatement;
	iKeyFieldNames;
	iKeyFieldNames2;	//  for WHERE clause when field aliases not supported by DB (table.field)
	iKeyFieldValues;
	iSelectAll;
	iSelectOne;

	iDisplayFieldNames;
	ifun	iRFfun;
};

#include <sql.h>
#include <sqlext.h>
#include <string.h>

static	char	*getfield(char *p, char *buf);
static	object	makeStrObj(char *str);


cmeth	gNewTableListbox(lb, stmt, char *kf, char *sa, char *so)
{
	return gNewTableListbox2(self, lb, stmt, kf, NULL, sa, so);
}

cmeth	gNewTableListbox2(lb, stmt, char *kf, char *wf, char *sa, char *so)
{
	char	field[100];
	object	obj = gNew(super);
	accessIVsOf(obj);
	iListbox = lb;
	iStatement = stmt;
	iKeyFieldNames = gNew(LinkObject);
	while (kf = getfield(kf, field))
		gAddLast(iKeyFieldNames, gNewWithStr(String, field));
	if (wf) {
		iKeyFieldNames2 = gNew(LinkObject);
		while (wf = getfield(wf, field))
			gAddLast(iKeyFieldNames2, gNewWithStr(String, field));
	}
	iKeyFieldValues = gNew(LinkObject);
	iSelectAll = makeStrObj(sa);
	iSelectOne = makeStrObj(so);
	return obj;
}

imeth	gDisplayFieldNames(char *df)
{
	char	field[100];

	if (iDisplayFieldNames)
		gDeepDispose(iDisplayFieldNames);
	iDisplayFieldNames = gNew(LinkObject);
	while (df = getfield(df, field))
		gAddLast(iDisplayFieldNames, gNewWithStr(String, field));
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iDisplayFieldNames)
		gDeepDispose(iDisplayFieldNames);
	gDeepDispose(iKeyFieldNames);
	if (iKeyFieldNames2)
		gDeepDispose(iKeyFieldNames2);
	gDeepDispose(iKeyFieldValues);
	gDispose(iSelectAll);
	gDispose(iSelectOne);
	return gDispose(super);
}

private	imeth	void	update_listbox_names()
{
	object	seq, field, data;
	char	line[500], buf[60];

	*line = '\0';
	for (seq=gSequence(iDisplayFieldNames) ; field = gNext(seq) ; ) {
		data = gFldGetValue(iStatement, gStringValue(field));
		if (*line)
			strcat(line, "\t");
		if (ClassOf(data) == Character) {
			buf[0] = gCharValue(data);
			buf[1] = '\0';
			strcat(line, buf);
		} else if (ClassOf(data) == String)
			strcat(line, gStringValue(data));
		else if (ClassOf(data) == ShortInteger) {
			sprintf(buf, "%d", (int) gShortValue(data));
			strcat(line, buf);
		} else if (ClassOf(data) == LongInteger) {
			sprintf(buf, "%ld", gLongValue(data));
			strcat(line, buf);
		} else if (ClassOf(data) == DoubleFloat) {
			sprintf(buf, "%f", gDoubleValue(data));
			strcat(line, buf);
		} else if (ClassOf(data) == Date) {
			object	t = gFormatDate(data, "%n/%d/%Y");
			strcat(line, gStringValue(t));
			gDispose(t);
		}
	}
	gAddOption(iListbox, line);
}

private	imeth	void	update_listbox_fun()
{
	char	line[500];

	iRFfun(iStatement, line);
	gAddOption(iListbox, line);
}

private	imeth	void	update_keylist()
{
	object	seq, field, data, list;

	list = gNew(LinkObject);
	for (seq=gSequence(iKeyFieldNames) ; field = gNext(seq) ; ) {
		data = gFldGetValue(iStatement, gStringValue(field));
		gAddLast(list, gCopy(data));
	}
	gAddLast(iKeyFieldValues, list);
}

imeth	gReadRecords()
{
	int	r = gDBSelect(iStatement, gStringValue(iSelectAll));
	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun)
		return NULL;
	while (!gNextRecord(iStatement)) {
		if (iRFfun)
			update_listbox_fun(self);
		else
			update_listbox_names(self);
		update_keylist(self);
	}
	return self;
}

#define	MAX_KEYS	10

imeth	vSelect(...)
{
	object	keys[MAX_KEYS], seq, lo, loseq, kv;
	int	num, i, r;
	MAKE_REST(self);

	for (num=0 ; num < MAX_KEYS  &&  (keys[num] = GetArg(object)) ; num++);
	for (r=0, seq = gSequence(iKeyFieldValues) ; lo = gNext(seq) ; r++) {
		for (i=0, loseq = gSequence(lo) ; (kv = gNext(loseq))  &&  i < num  &&  gEqual(keys[i], kv) ; ++i);
		if (kv)
			gDispose(loseq);
		if (!kv  ||  i >= num) {	//  found
			gDispose(seq);
			gSetShortValue(iListbox, r);
			return self;
		}
	}
	return NULL;
}

imeth	gReadRecordNumb : TableListbox_rrnumb (int i)
{
	char	where[256], *buf;
	object	fseq, field, list, dseq, data;
	int	and = 0;

	if (i < 0)
		return NULL;

	buf = gGetBuf(Application);

	strcpy(where, " WHERE ");
	list = gNth(iKeyFieldValues, i+1);
	dseq = gSequence(list);
	for (fseq=gSequence(iKeyFieldNames2 ? iKeyFieldNames2 : iKeyFieldNames) ; field = gNext(fseq) ; ) {
		if (and++)
			strcat(where, " AND ");
		strcat(where, gStringValue(field));
		strcat(where, " = ");
		data = gNext(dseq);
		if (ClassOf(data) == Character) {
			sprintf(buf, "~%c~", gCharValue(data));
			strcat(where, buf);
		} else if (ClassOf(data) == String) {
			strcat(where, "~");
			strcat(where, gStringValue(data));
			strcat(where, "~");
		} else if (ClassOf(data) == ShortInteger) {
			sprintf(buf, "%d", (int) gShortValue(data));
			strcat(where, buf);
		} else if (ClassOf(data) == LongInteger) {
			sprintf(buf, "%ld", gLongValue(data));
			strcat(where, buf);
		} else if (ClassOf(data) == DoubleFloat) {
			sprintf(buf, "%f", gDoubleValue(data));
			strcat(where, buf);
		} else if (ClassOf(data) == Date) {
			object	t = gFormatDate(data, "%n/%d/%Y");
			strcat(where, gStringValue(t));
			gDispose(t);
		}
	}
	sprintf(buf, "%s %s", gStringValue(iSelectOne), where);
	return gDBSelectOne(iStatement, buf) ? NULL : iStatement;
}

imeth	gStatement()
{
	return iStatement;
}

imeth	gReadRecord()
{
	return TableListbox_rrnumb(self, gShortValue(iListbox));
}

imeth	gRecordFormatFunction(ifun fun)
{
	iRFfun = fun;
	return self;
}

static	char	*getfield(char *p, char *buf)
{
	if (!*p)
		return NULL;
	while (*p  &&  *p != ',')
		*buf++ = *p++;
	*buf = '\0';
	return *p ? p+1 : p;
}

//  This is needed because Select sometimes needs extra storage to
//  handle embedded quotes which must be doubled up 

static	object	makeStrObj(char *str)
{
	object	obj = gNewWithInt(String, strlen(str)+30);
	return gChangeStrValue(obj, str);
}









