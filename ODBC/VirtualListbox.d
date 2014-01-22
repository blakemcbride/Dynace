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



#define	SCROLL_MAX	100

defclass  VirtualListbox : PropertyList {
	iListbox;
	iScrollBar;
	int	iNumbRows;	//  number of rows in the listbox
	int	iTotalRows;	//  rows currently on screen

	iStatement;
	iKeyFieldNames;
	iKeyFieldValues;

	iDisplayFieldNames;
	int	(*iRFfun)();
	int	(*iInitFun)();

	iTag;
	int	iAutoDisposeTag;

	iSelect;
	iWhere;
	iOrderBy;
	iOrderByDesc;
};

#include <sql.h>
#include <sqlext.h>
#include <string.h>


static	char	*getfield(char *p, char *buf);
static	int	process_scrollbar(object sb, unsigned msg, unsigned npos);
static	object	makeStrObj(char *str);
static	object	makeOrderByObj(object kfNames, int desc);
static	char	*formatDataForSelect(object data, char *buf);
static	char	*removeOrderByString(char *kf);
static	int	lbInit(object lb);

private	imeth	char	*pBuildSelect(object self, int desc);
private	imeth	int	pGroupFun(object self, int lessThan, object stmt, object ll);
private	imeth	object	pReadRecs(object self, int desc);

static	long	process_wm_keydown(object	lb, 
				   HWND		hwnd, 
				   UINT		mMsg, 
				   WPARAM	wParam, 
				   LPARAM	lParam);

cvmeth	vNew(lb, sb, stmt, char *sel, char *sw, char *kf)
{
	char	field[100];
	object	obj = gNew(super);
	accessIVsOf(obj);

	iListbox = lb;
	gPropertyPut(lb, "VirtualListbox", 0, obj);
	gInitFunction(lb, lbInit);
	gAddHandlerAfter(iListbox, (unsigned) WM_KEYDOWN, process_wm_keydown);
	
	if (sb) {
		iScrollBar = sb;
		gSetFunction(iScrollBar, process_scrollbar);
		gPropertyPut(iScrollBar, "VirtualListbox", 0, obj);
		gScrollBarRange(iScrollBar, 0, SCROLL_MAX, SCROLL_MAX / 10, 1);
	}
//	iNumbRows = nr;
	iStatement = stmt;
		
	iKeyFieldNames = gNew(LinkObject);
	kf = removeOrderByString(kf);
	while (kf = getfield(kf, field))
		gAddLast(iKeyFieldNames, gNewWithStr(String, field));
	iKeyFieldValues = gNew(LinkObject);

	iSelect = makeStrObj(sel);
	iWhere = makeStrObj(sw);
	iOrderBy = makeOrderByObj(iKeyFieldNames, 0);
	iOrderByDesc = makeOrderByObj(iKeyFieldNames, 1);

	return obj;
}

cmeth	gNewVirtualListbox(lb, sb, stmt, char *sel, char *sw, char *kf)
{
	return vNew(lb, sb, stmt, sel, sw, kf);
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
	gDeepDispose(iKeyFieldValues);
	gDispose(iSelect);
	gDispose(iWhere);
	gDispose(iOrderBy);
	gDispose(iOrderByDesc);

	if (iTag && iAutoDisposeTag)
		gDeepDispose(iTag);
	
	gDisposePropertyList(self);
	return gDispose(super);
}

private	imeth	void	VirtualListbox_update_listbox_names(int reverse)
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
	gAddOptionAt(iListbox, reverse ? 0 : -1, line);
}

private	imeth	void	VirtualListbox_update_listbox_fun(int reverse)
{
	char	line[500];

	iRFfun(iStatement, line);
	gAddOptionAt(iListbox, reverse ? 0 : -1, line);
}

private	imeth	void	VirtualListbox_update_keylist(int reverse)
{
	object	seq, field, data, list;

	list = gNew(LinkObject);
	for (seq=gSequence(iKeyFieldNames) ; field = gNext(seq) ; ) {
		data = gFldGetValue(iStatement, gStringValue(field));
		gAddLast(list, gCopy(data));
	}
	if (reverse)
		gAddFirst(iKeyFieldValues, list);
	else
		gAddLast(iKeyFieldValues, list);
}

imeth	gReadRecords()
{
	if (iNumbRows)
		return pReadRecs(self, 0);
	return NULL;
}

private	imeth	pReadRecs(object self, int desc)
{
	int	r = gDBSelect(iStatement, pBuildSelect(self, desc));
	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	iTotalRows = 0;
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun)
		return NULL;
	while (iTotalRows < iNumbRows  &&  !gNextRecord(iStatement)) {
		if (iRFfun)
			VirtualListbox_update_listbox_fun(self, desc);
		else
			VirtualListbox_update_listbox_names(self, desc);
		VirtualListbox_update_keylist(self, desc);
		iTotalRows++;
	}
	if (iScrollBar)
		if (iTotalRows != iNumbRows  ||  gNextRecord(iStatement))
			gHide(iScrollBar);
		else {
			gDisplay(iScrollBar);
			gSetShortValue(iScrollBar, desc ? SCROLL_MAX : 0);
		}
	return self;
}

private	imeth	gReadNextGroup()
{
	int	r;

	r = pGroupFun(self, 0, iStatement, gLast(iKeyFieldValues));
	if (!r)
		r = gNextRecord(iStatement);
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun) 
		return NULL;
	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	iTotalRows = 0;
	while (iTotalRows < iNumbRows  &&  !r) {
		if (iRFfun)
			VirtualListbox_update_listbox_fun(self, 0);
		else
			VirtualListbox_update_listbox_names(self, 0);
		VirtualListbox_update_keylist(self, 0);
		iTotalRows++;
		r = gNextRecord(iStatement);
	}
	if (iScrollBar)
		if (iTotalRows != iNumbRows  ||  r)
			gSetShortValue(iScrollBar, SCROLL_MAX);
		else
			gSetShortValue(iScrollBar, SCROLL_MAX / 2);
	return self;
}

private	imeth	gReadPrevGroup()
{
	int	r;

	r = pGroupFun(self, 1, iStatement, gFirst(iKeyFieldValues));
	if (!r)
		r = gNextRecord(iStatement);
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun) 
		return NULL;
	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	iTotalRows = 0;
	while (iTotalRows < iNumbRows  &&  !r) {
		if (iRFfun)
			VirtualListbox_update_listbox_fun(self, 1);
		else
			VirtualListbox_update_listbox_names(self, 1);
		VirtualListbox_update_keylist(self, 1);
		iTotalRows++;
		r = gNextRecord(iStatement);
	}
	if (iTotalRows != iNumbRows) {
		int	n = 0;
		r = gDBSelectOne(iStatement, pBuildSelect(self, 0));
		if (r)
			return NULL;
		while (iTotalRows < iNumbRows  &&  !r) {
			if (++n > iTotalRows) {
				if (iRFfun)
					VirtualListbox_update_listbox_fun(self, 0);
				else
					VirtualListbox_update_listbox_names(self, 0);
				VirtualListbox_update_keylist(self, 0);
				iTotalRows++;
			}
			r = gNextRecord(iStatement);
		}
		if (iScrollBar)
			gSetShortValue(iScrollBar, 0);
		return self;
	}
	if (iScrollBar)
		if (iTotalRows != iNumbRows  ||  r)
			gSetShortValue(iScrollBar, 0);
		else
			gSetShortValue(iScrollBar, SCROLL_MAX / 2);
	return self;
}

private	imeth	nextRecord()
{
	int	r;

	r = pGroupFun(self, 0, iStatement, gLast(iKeyFieldValues));
	if (!r)
		r = gNextRecord(iStatement);
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun) 
		return NULL;
	if (iTotalRows == iNumbRows) {
		gRemoveInt(iListbox, 0);
		gDeepDisposeFirst(iKeyFieldValues);
		iTotalRows--;
	}
	if (iTotalRows < iNumbRows  &&  !r) {
		if (iRFfun)
			VirtualListbox_update_listbox_fun(self, 0);
		else
			VirtualListbox_update_listbox_names(self, 0);
		VirtualListbox_update_keylist(self, 0);
		iTotalRows++;
	}
	if (iScrollBar)
		if (gNextRecord(iStatement))
			gSetShortValue(iScrollBar, SCROLL_MAX);
		else
			gSetShortValue(iScrollBar, SCROLL_MAX / 2);
	return self;
}

private	imeth	prevRecord()
{
	int	r;

	r = pGroupFun(self, 1, iStatement, gFirst(iKeyFieldValues));
	if (!r)
		r = gNextRecord(iStatement);
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun) 
		return NULL;
	if (iTotalRows == iNumbRows) {
		gRemoveInt(iListbox, --iTotalRows);
		gDeepDisposeLast(iKeyFieldValues);
	}
	if (iTotalRows < iNumbRows  &&  !r) {
		if (iRFfun)
			VirtualListbox_update_listbox_fun(self, 1);
		else
			VirtualListbox_update_listbox_names(self, 1);
		VirtualListbox_update_keylist(self, 1);
		iTotalRows++;
	}
	if (iScrollBar)
		if (gNextRecord(iStatement))
			gSetShortValue(iScrollBar, 0);
		else
			gSetShortValue(iScrollBar, SCROLL_MAX / 2);
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

imeth	gReadRecordNumb : VirtualListbox_rrnumb (int i)
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
	for (fseq=gSequence(iKeyFieldNames) ; field = gNext(fseq) ; ) {
		if (and++)
			strcat(where, " AND ");
		strcat(where, gStringValue(field));
		strcat(where, " = ");
		data = gNext(dseq);
		strcat(where, formatDataForSelect(data, buf));
	}
	sprintf(buf, "%s %s", gStringValue(iSelect), where);
	return gDBSelectOne(iStatement, buf) ? NULL : iStatement;
}

imeth	gStatement()
{
	return iStatement;
}

imeth	gReadRecord()
{
	return VirtualListbox_rrnumb(self, gShortValue(iListbox));
}

imeth	gRecordFormatFunction(ifun fun)
{
	iRFfun = fun;
	return self;
}

static	int	process_scrollbar(object sb, unsigned msg, unsigned npos)
{
	object	self = gPropertyGet(sb, "VirtualListbox");
	accessIVs;

	switch ((int)msg)  {
	case SB_TOP:	
//		gReadPrevGroup(self);
		pReadRecs(self, 0);
		break;
	case SB_BOTTOM:
		pReadRecs(self, 1);
//		gReadNextGroup(self);
		break;
	case SB_LINEUP:
		prevRecord(self);
		break;
	case SB_LINEDOWN:
		nextRecord(self);
		break;
	case SB_PAGEUP:
		gReadPrevGroup(self);
		break;
	case SB_PAGEDOWN:
		gReadNextGroup(self);
		break;
	case SB_THUMBTRACK:
		break;
	}
	return 0;
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
	char	*s = str ? str : "";
	object	obj = gNewWithInt(String, strlen(s)+30);

	gChangeStrValue(obj, s);
	return obj;
}

static	object	makeOrderByObj(object kfNames, int desc)
{
	char	*buf = gGetBuf(Application);
	object	obj = gNew(String);
	object	seq, field;
	int	comma = 0;

	if (!gSize(kfNames))
		return obj;

	strcpy(buf, "ORDER BY ");
	
	for (seq=gSequence(kfNames) ; field = gNext(seq) ; ) {
		if (comma++)
			strcat(buf, ",");
		strcat(buf, gStringValue(field));
		if (desc)
			strcat(buf, " desc");
	}

	return gChangeStrValue(obj, buf);
}

private	imeth	char	*pBuildSelect(object self, int desc)
{
	char	*buf = gGetBuf(Application);

	sprintf(buf, "%s %s %s", gStringValue(iSelect), gStringValue(iWhere),
		gStringValue(desc ? iOrderByDesc : iOrderBy));
	return buf;
}

private	imeth	int	pGroupFun(object self, int lessThan, object stmt, object ll)
{
	char	*buf = gGetBuf(Application);
	char	fld[256];
	object	seq, field, dseq, data, obj;
	int	sz = gSize(iKeyFieldNames);
	int	i, fnum;
	int	or, and;
	char	*lsym = lessThan ? " < " : " > ";
	int	r;

	strcpy(buf, "WHERE ");

	if (gSize(iWhere)) {
		object	wo = gCopy(iWhere);
		char	*cp = gStringValue(gToLower(gStripCenter(wo)));
		char	*p = strstr(cp, "where");

		if (p)
			while (!isspace(*p))
				p++;
		else
			p = cp;
		strcat(buf, "(");
		strcat(buf, p);
		strcat(buf, ") AND ");
		gDispose(wo);
	}

	for (i = 0, or = 0; i < sz; i++) {
		if (or)
			strcat(buf, " OR (");
		dseq = gSequence(ll);
		for (and = 0, fnum = 0, seq=gSequence(iKeyFieldNames) ; field = gNext(seq) ; fnum++) {
			data = gNext(dseq);
			if (fnum <= i) {
				if (and++)
					strcat(buf, " AND ");

				strcat(buf, gStringValue(field));
				strcat(buf, fnum < i ? " = " : lsym);
				strcat(buf, formatDataForSelect(data, fld));
			}
		}
		if (or++)
			strcat(buf, ")");
	}
	obj = gNewWithStr(String, buf);
	r = vDBSelect(stmt, "%s %s %s", gStringValue(iSelect), gStringValue(obj),
		      gStringValue(lessThan ? iOrderByDesc : iOrderBy));
	gDispose(obj);
	return r;
}

static	char	*formatDataForSelect(object data, char *buf)
{
	if (ClassOf(data) == Character)
		sprintf(buf, "~%c~", gCharValue(data));
	else if (ClassOf(data) == String)
		sprintf(buf, "~%s~", gStringValue(data));
	else if (ClassOf(data) == ShortInteger)
		sprintf(buf, "%d", (int) gShortValue(data));
	else if (ClassOf(data) == LongInteger)
		sprintf(buf, "%ld", gLongValue(data));
	else if (ClassOf(data) == DoubleFloat)
		sprintf(buf, "%f", gDoubleValue(data));
	else if (ClassOf(data) == Date) {
		object	t = gFormatDate(data, "%n/%d/%Y");
		strcpy(buf, gStringValue(t));
		gDispose(t);
	} else
		*buf = '\0';
	
	return buf;
}

static	long	process_wm_keydown(object	lb, 
				   HWND		hwnd, 
				   UINT		mMsg, 
				   WPARAM	wParam, 
				   LPARAM	lParam)
{
	object	self = gPropertyGet(lb, "VirtualListbox");
	accessIVs;
	int	ctrl = GetKeyState(VK_CONTROL) < 0;

	switch(wParam) {
	case VK_PRIOR:	// Page up
		gReadPrevGroup(self);
		break;
	case VK_NEXT:	// Page down
		gReadNextGroup(self);
		break;
	case VK_END:
		if (ctrl)
			pReadRecs(self, 1);
		break;
	case VK_HOME:
		if (ctrl)
			pReadRecs(self, 0);
		break;
	case VK_UP:
		if (!gShortValue(lb))
			prevRecord(self);
		return 0L;
		break;
	case VK_DOWN:
		if (gShortValue(lb) == iNumbRows - 1)
			nextRecord(self);
		return 0L;
		break;
	}

	return gCallDefaultProc(lb, mMsg, wParam, lParam);
}

imeth	gSetKeyFields(char *kf)
{
	char	field[100];

	gDeepDispose(iKeyFieldNames);
	gDeepDispose(iKeyFieldValues);
	gDispose(iOrderBy);
	gDispose(iOrderByDesc);
	
	iKeyFieldNames = gNew(LinkObject);
	while (kf = getfield(kf, field))
		gAddLast(iKeyFieldNames, gNewWithStr(String, field));
	iKeyFieldValues = gNew(LinkObject);

	iOrderBy = makeOrderByObj(iKeyFieldNames, 0);
	iOrderByDesc = makeOrderByObj(iKeyFieldNames, 1);
	return self;
}

imeth	gGetKeyFields()
{
	return iKeyFieldNames;
}

imeth	gSetSelect(char *sel)
{
	gChangeStrValue(iSelect, sel);

	return self;
}

imeth	char	*gGetSelect()
{
	return gStringValue(iSelect);
}

imeth	gSetWhere(char *sw)
{
	gChangeStrValue(iWhere, sw);

	return self;
}

imeth	char	*gGetWhere()
{
	return gStringValue(iWhere);
}

imeth	int	gVisibleRecords()
{
	return gSize(iListbox);
}

static	char	*removeOrderByString(char *kf)
{
	object	obj = gNewWithStr(String, kf);
	char	*cp = gStringValue(gToLower(obj));
	char	*p = strstr(cp, "order");

	if (p) {
		while (!isspace(*p))
			p++;
		p = strstr(p, "by");
		if (p)
			while (!isspace(*p))
				p++;
		else
			p = cp;
	} else
		p = cp;
	kf += p - cp;

	gDispose(obj);

	while (isspace(*kf))
		kf++;

	return kf;
}

static	int	lbInit(object lb)
{
	object	self = gPropertyGet(lb, "VirtualListbox");
	accessIVs;
	RECT	r;
	LRESULT	h;

	GetClientRect(gHandle(lb), &r);
	h = SendDlgItemMessage(gHandle(gDialog(lb)), gGetCtlID(lb), LB_GETITEMHEIGHT, 0, 0L);

	if (!h)
		vError(Application, "Can't determine virtual listbox number of rows.");

	iNumbRows = r.bottom / h;

	gReadRecords(self);

	if (iInitFun)
		iInitFun(self);
	
	return 0;
}

imeth	gInitFunction(int (*fun)())
{
	iInitFun = fun;
	return self;
}

imeth	gSetTag(object tag)
{
	object	ptag = iTag;
	
	iTag = tag;

	if (ptag  &&  iAutoDisposeTag)
		return gDeepDispose(ptag);

	return ptag;
}

imeth	gGetTag()
{
	return iTag;
}

imeth	gAutoDisposeTag()
{
	iAutoDisposeTag = 1;
	return self;
}








