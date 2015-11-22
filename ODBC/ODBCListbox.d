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

defclass  ODBCListbox : PropertyList {
	iListbox;
	iScrollBar;
	int	iNumbRows;	//  number of rows in the listbox
	int	iTotalRows;	//  rows currently on screen
	int	iRowHeight;

	iStatement;
	iKeyFieldNames;
	iKeyFieldOrders;
	iKeyFieldValues;

	int	iNumFields;	// number of fields explicitly set by gSetKeyFields
	
	iAliasFields;

	iDisplayFieldNames;
	int	(*iRFfun)();
	int	(*iInitFun)();
	int	(*iRCfun)();

	iTag;
	int	iAutoDisposeTag;

	iSelectWA;	// Select statement with aliases added
	iSelect;
	iWhere;
	iOrderBy;
	iOrderByDesc;

	int	iVirtual;
	int	iAtEnd;
};

#include <sql.h>
#include <sqlext.h>
#include <string.h>
#include <ctype.h>


static	char   *Cs(char *s);
static	char	*getfield(char *p, char *buf);
static	int	process_scrollbar(object sb, unsigned msg, unsigned npos);
static	object	makeStrObj(char *str);
static	object	makeOrderByObj(object kfNames, object kfOrders, int desc);
static	char	*formatDataForSelect(object data, char *buf);
static	char	*removeOrderByString(char *kf);
static	int	lbInit(object lb);
static	char	*getFieldName(char *input, char *buf, int *desc);
static	void	addToKeyFields(object kfn, object kfo, object name, int desc);

private	imeth	char	*pBuildSelect(object self, int desc);
private	imeth	int	pGroupFun(object self, int lessThan, object stmt, object ll);
private	imeth	object	pReadRecs(object self, int desc);
private	imeth	char	*pAddAliasToSelect(object self);

static	LRESULT	process_wm_keydown(object	lb, 
				   HWND		hwnd, 
				   UINT		mMsg, 
				   WPARAM	wParam, 
				   LPARAM	lParam);

cvmeth	vNew(lb, sb, stmt, char *sel, char *sw, char *kf, int virtual)
{
	char	field[100];
	object	obj = gNew(super);
	accessIVsOf(obj);
	char	fld[100];
	int	desc;
	int	alct;	// count of aliases
	object	sobj;
	char	buf[100];

	iVirtual = virtual;
	if (lb) {
		iListbox = lb;
		gPropertyPut(lb, "ODBCListbox", 0, obj);
		gInitFunction(lb, lbInit);
		if (iVirtual)
			gAddHandlerAfter(iListbox, (unsigned) WM_KEYDOWN, process_wm_keydown);
	}
	if (sb) {
		iScrollBar = sb;
		gSetFunction(iScrollBar, process_scrollbar);
		gPropertyPut(iScrollBar, "ODBCListbox", 0, obj);
		gScrollBarRange(iScrollBar, 0, SCROLL_MAX, SCROLL_MAX / 10, 1);
	}
	iStatement = stmt;
		
	iKeyFieldNames = gNew(LinkObject);
	iKeyFieldOrders = gNew(LinkObject);
	iAliasFields = gNew(StringDictionary);
	if (kf)
		kf = removeOrderByString(kf);
	else
		kf = "";
	alct = 0;
	while (kf = getfield(kf, field)) {
		getFieldName(field, fld, &desc);
		if (strchr(fld, '.')) {
			sprintf(buf, "ALIASFIELD%d", ++alct);
			if (!gFindValueStr(iAliasFields, fld))
				gAddStr(iAliasFields, fld,gNewWithStr(String, buf));
		}
		sobj = gNewWithStr(String, fld);

		addToKeyFields(iKeyFieldNames, iKeyFieldOrders, sobj, desc);
	}
	iKeyFieldValues = gNew(LinkObject);

	iSelect = makeStrObj(sel);
	iWhere = makeStrObj(sw);
	iOrderBy = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 0);
	iOrderByDesc = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 1);

	return obj;
}

cmeth	gNewODBCListbox(lb, sb, stmt, char *sel, char *sw, char *kf, int virt)
{
	return vNew(self, lb, sb, stmt, sel, sw, kf, virt);
}

private	imeth	int	pNextRecord()
{
	int	r;
	
	do
		r = gNextRecord(iStatement);
	while (!r  &&  iRCfun  &&  !iRCfun(self));
	return r;
}

private	imeth	int	pPrevRecord()
{
	int	r;
	
	do
		r = gPrevRecord(iStatement);
	while (!r  &&  iRCfun  &&  !iRCfun(self));
	return r;
}

private	imeth	int	pFirstRecord()
{
	int	r = gFirstRecord(iStatement);
	
	while (!r  &&  iRCfun  &&  !iRCfun(self))
		r = gNextRecord(iStatement);
	return r;
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
	gDispose(iAliasFields);
	gDeepDispose(iKeyFieldNames);
	gDeepDispose(iKeyFieldOrders);
	gDeepDispose(iKeyFieldValues);
	if (iSelectWA)
		gDispose(iSelectWA);
	gDispose(iSelect);
	gDispose(iWhere);
	gDispose(iOrderBy);
	gDispose(iOrderByDesc);

	if (iTag && iAutoDisposeTag)
		gDeepDispose(iTag);
	
	gDisposePropertyList(self);
	return gDispose(super);
}

private	imeth	void	pCheckHeight(object self)
{
	RECT	r;
	
	if (ClassOf(iListbox) == ListBox)
		GetClientRect(gHandle(iListbox), &r);
	else
		gGetItemHeight(iListbox, &r);

	iNumbRows = r.bottom / iRowHeight;
}

private	imeth	void	update_listbox_names(int reverse)
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
	if (iVirtual && iRowHeight)
		pCheckHeight(self);
}

private	imeth	void	update_listbox_fun(int reverse)
{
	char	line[500];
	int	state;

	state = iRFfun(iStatement, line);
	
	gAddOptionAt(iListbox, reverse ? 0 : -1, line);
	if (ClassOf(iListbox) != ListBox) {
		int	row = reverse ? 0 : gSize(iListbox) - 1;
		
		switch (state) {
		case 0:	// Enabled, not selected
			gEnableRow(iListbox, row, 1);
			gDeselectLine(iListbox, row);
			break;
		case 1:	// Enabled, selected
			gEnableRow(iListbox, row, 1);
			gSelectLine(iListbox, row);
			break;
		case 2:	// Disabled, not selected
			gEnableRow(iListbox, row, 0);
			gDeselectLine(iListbox, row);
			break;
		case 3:	// Disabled, selected
			gEnableRow(iListbox, row, 0);
			gSelectLine(iListbox, row);
			break;
		}
	}
	if (iVirtual && iRowHeight)
		pCheckHeight(self);
}

static	char	*getFieldName(char *input, char *buf, int *desc)
{
	char	*p;

	strcpy(buf, input);
	p = buf;
	
	while (*p && !isspace(*p))
		p++;

	if (desc)
		*desc = 0;
	if (*p) {
		*p = '\0';
		if (desc) {
			char	*t;
			
			p++;
			for (t = p; *t; t++)
				*t = tolower(*t);
			if (strstr(p, "desc"))
				*desc = 1;
		}
	}
	
	return buf;
}

private	imeth	void	update_keylist(int reverse)
{
	object	seq, field, data, list, fld;
	char	*cp;

	list = gNew(LinkObject);
	for (seq=gSequence(iKeyFieldNames) ; field = gNext(seq) ; ) {
		cp = gStringValue(field);
		if (strchr(cp, '.')) {
			if (!(fld = gFindValueStr(iAliasFields, cp)))
				gError(Application, "Can't find alias field for key.");
			else
				cp = gStringValue(fld);
		}
		data = gFldGetValue(iStatement, cp);
		gAddLast(list, gCopy(data));
	}
	if (reverse)
		gAddFirst(iKeyFieldValues, list);
	else
		gAddLast(iKeyFieldValues, list);
}

imeth	gReadRecords()
{
	iAtEnd = 0;
	if (iNumbRows)
		return pReadRecs(self, 0);
	
	return NULL;
}

private	imeth	pReadRecs(object self, int desc)
{
	int	r = gDBSelectDNC(iStatement, pBuildSelect(self, desc));
	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	iTotalRows = 0;
	if (r  ||  !iDisplayFieldNames  &&  !iRFfun) {
		if (iScrollBar)
			gHide(iScrollBar);
		return NULL;
	}
	while (iTotalRows < iNumbRows  &&  !pNextRecord(self)) {
		if (iRFfun)
			update_listbox_fun(self, desc);
		else
			update_listbox_names(self, desc);
		update_keylist(self, desc);
		if (iVirtual)
			iTotalRows++;
	}
	if (iVirtual && iScrollBar)
		if (iTotalRows != iNumbRows  ||  pNextRecord(self))
			gHide(iScrollBar);
		else {
			gDisplay(iScrollBar);
			gSetShortValue(iScrollBar, desc ? SCROLL_MAX : 0);
		}
	if (desc)
		gSetShortValue(iListbox, iTotalRows - 1);
	else
		gSetShortValue(iListbox, 0);
	return self;
}

private	imeth	gReadNextGroup()
{
	int	r = 0;

	if (iAtEnd)
		return NULL;

	gRemoveAll(iListbox);
	gDeepDisposeAllNodes(iKeyFieldValues);
	iTotalRows = 0;
	while (iTotalRows < iNumbRows  &&  !r) {
		if (iRFfun)
			update_listbox_fun(self, 0);
		else
			update_listbox_names(self, 0);
		update_keylist(self, 0);
		iTotalRows++;
		r = pNextRecord(self);
	}
	if (r)
		iAtEnd = 1;
	if (iScrollBar)
		if (iTotalRows != iNumbRows  ||  r)
			gSetShortValue(iScrollBar, SCROLL_MAX);
		else
			if ((gGetCursorPosition(iStatement) - 1) != iTotalRows)
				gSetShortValue(iScrollBar, SCROLL_MAX / 2);
			else
				gSetShortValue(iScrollBar, 0);

	return self;
}

private	imeth	gReadPrevGroup()
{
	if ((gGetCursorPosition(iStatement) - 1) - (iNumbRows + iTotalRows) <= 0)
		pFirstRecord(self);
	else
		gSetRelPos(iStatement, (iNumbRows + iTotalRows) * -1);

	iAtEnd = 0;
	gReadNextGroup(self);
	return self;
}

private	imeth	currentRecord()
{
	if ((gGetCursorPosition(iStatement) - 1) - iTotalRows < 0)
		pFirstRecord(self);
	else
		gSetRelPos(iStatement, iTotalRows * -1);

	gReadNextGroup(self);
	
	return self;
}

private	imeth	nextRecord()
{
	if (iAtEnd)
		return NULL;

	if ((gGetCursorPosition(iStatement) - 1) - iNumbRows < 0)
		pFirstRecord(self);
	else
		gSetRelPos(iStatement, (iNumbRows - 1) * -1);
	
	gReadNextGroup(self);
	return self;
}

private	imeth	prevRecord()
{
	if ((gGetCursorPosition(iStatement) - 1) - (iTotalRows + 1) < 0)
		pFirstRecord(self);
	else
		gSetRelPos(iStatement, (iTotalRows + 1) * -1);
	
	iAtEnd = 0;
	gReadNextGroup(self);
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

imeth	gReloadGroup()
{
	currentRecord(self);
	
	return self;
}

imeth	gReadRecordWithStmt : rrws (object self, int i, object stmt)
{
	char	where[256], *buf;
	object	fseq, field, list, dseq, data;
	int	and = 0;
	int	nflds, j;

	if (i < 0)
		return NULL;

	buf = gGetBuf(Application);

	nflds = iNumFields ? gSize(iKeyFieldNames) - iNumFields : 0;
	
	strcpy(where, " WHERE ");
	list = gNth(iKeyFieldValues, i+1);
	if (list) {
		dseq = gSequence(list);
		for (j = 0, fseq=gSequence(iKeyFieldNames) ; field = gNext(fseq) ; j++) {
			data = gNext(dseq);
			if (j >= nflds) {
				if (and++)
					strcat(where, " AND ");
				strcat(where, gStringValue(field));
				strcat(where, " = ");
				strcat(where, formatDataForSelect(data, buf));
			}
		}
		gDispose(dseq);
	} else
		strcat(where, "1 = 2");
	sprintf(buf, "%s %s", pAddAliasToSelect(self), where);
	return gDBSelectOneDNC(stmt, buf) ? NULL : stmt;
}

imeth	gReadRecordNumb : rrnumb (int i)
{
	return rrws(self, i, iStatement);
}

imeth	gStatement()
{
	return iStatement;
}

imeth	gReadRecord()
{
	return rrnumb(self, gShortValue(iListbox));
}

imeth	gRecordFormatFunction(ifun fun)
{
	iRFfun = fun;
	return self;
}

imeth	gRecordCheckFunction(ifun fun)
{
	iRCfun = fun;
	return self;
}

static	int	process_scrollbar(object sb, unsigned msg, unsigned npos)
{
	object	self = gPropertyGet(sb, "ODBCListbox");
	accessIVs;
	int	nSel = gShortValue(iListbox);

	switch ((int)msg)  {
	case SB_TOP:	// selection position handled in pReadRecs
//		gReadPrevGroup(self);
		pReadRecs(self, 0);
		break;
	case SB_BOTTOM:	// selection position handled in pReadRecs
		pReadRecs(self, 1);
//		gReadNextGroup(self);
		break;
	case SB_LINEUP:
		if (nSel > 0)
			nSel--;
		else
			prevRecord(self);
		gSetShortValue(iListbox, nSel);
		break;
	case SB_LINEDOWN:
		if (nSel < iTotalRows - 1)
			nSel++;
		else
			nextRecord(self);
		gSetShortValue(iListbox, nSel);
		break;
	case SB_PAGEUP:
		gReadPrevGroup(self);
		gSetShortValue(iListbox, 0);
		break;
	case SB_PAGEDOWN:
		gReadNextGroup(self);
		gSetShortValue(iListbox, iTotalRows - 1);
		break;
	case SB_THUMBTRACK:
		break;
	}
	return 0;
}

static	char	*getfield(char *p, char *buf)
{
	char	*b = buf;
	if (!*p)
		return NULL;
	while (*p  &&  *p != ',')
		*buf++ = *p++;
	*buf = '\0';
	Cs(b);
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

static	object	makeOrderByObj(object kfNames, object kfOrders, int reverse)
{
	char	*buf = gGetBuf(Application);
	object	obj = gNew(String);
	object	seq, field, s2, order;
	int	comma = 0;

	if (!gSize(kfNames))
		return obj;

	strcpy(buf, "ORDER BY ");
	
	s2 = gSequence(kfOrders);
	for (seq=gSequence(kfNames) ; field = gNext(seq) ; ) {
		order = gNext(s2);
		if (comma++)
			strcat(buf, ",");
		strcat(buf, gStringValue(field));
		if ((reverse && !gShortValue(order)) || (!reverse && gShortValue(order)))
			strcat(buf, " desc");
	}
	gDispose(s2);
	return gChangeStrValue(obj, buf);
}

private	imeth	char	*pBuildSelect(object self, int desc)
{
	char	*buf = gGetBuf(Application);

	sprintf(buf, "%s %s %s", pAddAliasToSelect(self), gStringValue(iWhere),
		gStringValue(desc ? iOrderByDesc : iOrderBy));
	return buf;
}

private	imeth	int	pGroupFun(object self, int lessThan, object stmt, object ll)
{
	char	*buf = gGetBuf(Application);
	char	fld[256];
	object	seq, field, dseq, data, obj, s2;
	int	sz = gSize(iKeyFieldNames);
	int	i, fnum, desc, szwhere;
	int	or, and;
	char	*lsym;
	int	r;

	strcpy(buf, "WHERE ");

	if (szwhere=gSize(iWhere)) {
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
		strcat(buf, ") AND (");
		gDispose(wo);
	}

	for (i = 0, or = 0; i < sz; i++) {
		if (or)
			strcat(buf, " OR (");
		s2 = gSequence(iKeyFieldOrders);
		dseq = gSequence(ll);
		for (and = 0, fnum = 0, seq=gSequence(iKeyFieldNames) ; field = gNext(seq) ; fnum++) {
			data = gNext(dseq);
			desc = gShortValue(gNext(s2));
			if (fnum <= i) {
				if (and++)
					strcat(buf, " AND ");

				if ((i == (sz - 1) && (fnum == i)))
					strcat(buf, "(");
					
				strcat(buf, gStringValue(field));

				lsym = desc == lessThan ? " > " : " < ";

				strcat(buf, fnum < i ? " = " : lsym);
				strcat(buf, formatDataForSelect(data, fld));
				
				if ((i == (sz - 1) && (fnum == i))) {
					strcat(buf, " OR ");
					strcat(buf, gStringValue(field));

					strcat(buf, " = ");
					strcat(buf, formatDataForSelect(data, fld));
					strcat(buf, ")");
				}
			}
		}
		if (or++)
			strcat(buf, ")");
		gDispose(dseq);
		gDispose(s2);
	}
	if (szwhere)
		strcat(buf, ") ");
	obj = gNewWithStr(String, buf);
	r = vDBSelectDNC(stmt, "%s %s %s", pAddAliasToSelect(self), gStringValue(obj),
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
	else if (ClassOf(data) == Date || ClassOf(data) == DateTime) {
		if (!gDateValue(data))
			strcpy(buf, "~1800-01-01~");
		else {
			object	t = gFormatDate(data, "~%Y-%n-%d~");
			strcpy(buf, gStringValue(t));
			gDispose(t);
		}
	} else
		*buf = '\0';
	
	return buf;
}


static	LRESULT	process_wm_keydown(object	lb, 
				   HWND		hwnd, 
				   UINT		mMsg, 
				   WPARAM	wParam, 
				   LPARAM	lParam)
{
	object	self = gPropertyGet(lb, "ODBCListbox");
	accessIVs;
	int	ctrl = GetKeyState(VK_CONTROL) < 0;
	int	nSel = gShortValue(lb);

	switch(wParam) {
	case VK_PRIOR:	// Page up
		gReadPrevGroup(self);
		gSetShortValue(lb, 0);
		break;
	case VK_NEXT:	// Page down
		gReadNextGroup(self);
		gSetShortValue(lb, iTotalRows - 1);
		break;
	case VK_END:
		if (ctrl) {
			while (!pNextRecord(self));
			if (gGetCursorPosition(iStatement) > iNumbRows / 2)
				gSetRelPos(iStatement, (iNumbRows / 2) * -1);
			gReadNextGroup(self);
			gSetShortValue(lb, iTotalRows - 1);
		}
		break;
	case VK_HOME:
		if (ctrl) {
			pFirstRecord(self);
			iAtEnd = 0;
			gReadNextGroup(self);
			gSetShortValue(lb, 0);
		}
		break;
	case VK_UP:
		if (!nSel)
			prevRecord(self);
		gSetShortValue(lb, nSel);
		return 0L;
		break;
	case VK_DOWN:
		if (nSel == iNumbRows - 1)
			nextRecord(self);
		gSetShortValue(lb, nSel);
		return 0L;
		break;
	}

	return gCallDefaultProc(lb, mMsg, wParam, lParam);
}

imeth	gSetKeyFields(char *kf)
{
	char	field[100];
	char	fld[100];
	int	alct = gSize(iKeyFieldNames);
	char	buf[100];
	object 	sobj;
	int	desc;
	
	gDeepDispose(iKeyFieldValues);
	gDispose(iOrderBy);
	gDispose(iOrderByDesc);

	while (kf = getfield(kf, field)) {
		getFieldName(field, fld, &desc);
		if (strchr(fld, '.')) {
			sprintf(buf, "ALIASFIELD%d", ++alct);
			if (!gFindValueStr(iAliasFields, fld))
				gAddStr(iAliasFields, fld, gNewWithStr(String, buf));
		} 
		sobj = gNewWithStr(String, fld);

		iNumFields++;
		addToKeyFields(iKeyFieldNames, iKeyFieldOrders, sobj, desc);
	}
	iKeyFieldValues = gNew(LinkObject);

	iOrderBy = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 0);
	iOrderByDesc = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 1);

	return self;
}

imeth	gSetOrderBy (char *kf)
{
	char	field[100];
	char	fld[100];
	int	desc;
	int	alct;
	char	buf[100];
	object	sobj;

	iNumFields = 0;
	
	if (iAliasFields)
		gDispose(iAliasFields);
	gDeepDispose(iKeyFieldNames);
	gDeepDispose(iKeyFieldOrders);
	gDeepDispose(iKeyFieldValues);
	gDispose(iOrderBy);
	gDispose(iOrderByDesc);
	
	kf = removeOrderByString(kf);
	iAliasFields = gNew(StringDictionary);
	iKeyFieldNames = gNew(LinkObject);
	iKeyFieldOrders = gNew(LinkObject);
	alct = 0;
	while (kf = getfield(kf, field)) {
		getFieldName(field, fld, &desc);
		if (strchr(fld, '.')) {
			sprintf(buf, "ALIASFIELD%d", ++alct);
			if (!gFindValueStr(iAliasFields, fld))
				gAddStr(iAliasFields, fld, gNewWithStr(String, buf));
		} 
		sobj = gNewWithStr(String, fld);
			
		addToKeyFields(iKeyFieldNames, iKeyFieldOrders, sobj, desc);
	}
	iKeyFieldValues = gNew(LinkObject);

	iOrderBy = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 0);
	iOrderByDesc = makeOrderByObj(iKeyFieldNames, iKeyFieldOrders, 1);
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

imeth	int	gVisibleRecords, gSize ()
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

static	int	resizeColumn(object xtlb, int col, int row)
{
	object	self = gPropertyGet(xtlb, "ODBCListbox");
	accessIVs;
	RECT	r;
	
	iRowHeight = gGetItemHeight(xtlb, &r);
	
	if (!iRowHeight)
		vError(Application, "Can't determine virtual listbox number of rows.");
		
	iNumbRows = r.bottom / iRowHeight;

	currentRecord(self);
//	gReloadGroup(self);

	return 0;
}

static	int	lbInit(object lb)
{
	object	self = gPropertyGet(lb, "ODBCListbox");
	accessIVs;

	if (ClassOf(lb) != ListBox)
		gResizeColumnFunction(lb, resizeColumn);
	
	if (iInitFun)
		iInitFun(self);
	
	if (iVirtual) {
		RECT	r;

		if (ClassOf(lb) == ListBox) {
			GetClientRect(gHandle(lb), &r);
			iRowHeight = SendDlgItemMessage(gHandle(gDialog(lb)), gGetCtlID(lb), LB_GETITEMHEIGHT, 0, 0L);
		} else {
			gEnableVScrollBar(lb, 0);
			iRowHeight = gGetItemHeight(lb, &r);
		}

		if (!iRowHeight)
			vError(Application, "Can't determine virtual listbox number of rows.");
		

		iNumbRows = r.bottom / iRowHeight;
	} else
		iNumbRows = 1;

	gReadRecords(self);

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

static	char   *Cs(char *s)
{
	int i, j, flg;

	if (!s)
		return (s);
	for (j = i = 0, flg = 1; s[i]; i++)
		if (flg) {
			if (isspace(s[i]))
				j++;
			else
				flg = 0;
		}
	i -= j + 1;
	memmove(s, s + j, i + 2);
	for (; i >= 0 && isspace(s[i]); i--);
	s[i + 1] = '\0';
	return (s);
}

imeth	gSetListbox(object lb)
{
	iListbox = lb;

	if (lb) {
		gPropertyPut(lb, "ODBCListbox", 0, self);
		gInitFunction(lb, lbInit);
		if (iVirtual)
			gAddHandlerAfter(iListbox, (unsigned) WM_KEYDOWN, process_wm_keydown);
	}

	return self;
}

imeth	gGetListbox()
{
	return iListbox;
}

imeth	gSetScrollBar(object sb)
{
	iScrollBar = sb;

	if (sb) {
		gSetFunction(iScrollBar, process_scrollbar);
		gPropertyPut(iScrollBar, "ODBCListbox", 0, self);
		gScrollBarRange(iScrollBar, 0, SCROLL_MAX, SCROLL_MAX / 10, 1);
	}

	return self;
}

imeth	gGetScrollBar()
{
	return iScrollBar;
}

private	imeth	char	*pAddAliasToSelect(object self)
{
	char	*buf = gGetBuf(Application);
	char	*p;
	char	*b = buf;
	object	seq, obj;
	char	afld[256];

	if (!gSize(iAliasFields))
		return gStringValue(iSelect);
	
	if (iSelectWA)
		gDispose(iSelectWA);

	p = gStringValue(iSelect);

	while(isspace(*p))	// Check for leading spaces
		*b++ = *p++;
	while (!isspace(*p))	// get first word, (should be "select", any case)
		*b++ = *p++;

	*b++ = ' ';		// Add a whitespace character
	*b = '\0';
	p++;			// prepare to catch the rest of the string
	
	for (seq=gSequence(iAliasFields) ; obj = gNext(seq) ; )	{	// Tack on all of the aliases
		sprintf(afld, "%s %s,", gStringKey(obj), gStringValue(gValue(obj)));
		strcat(b, afld);
	}

	strcat(b, p);		// get the rest of the select statement.
	
	iSelectWA = makeStrObj(buf);

	return gStringValue(iSelectWA);
}

static	void	addToKeyFields(object kfn, object kfo, object name, int desc)
{
	object	seq, field;
	int	found = 0;

	for (seq=gSequence(kfn) ; field = gNext(seq) ; )
		if (!gCompareI(name, field))
			found = 1;

	if (!found) {
		gAddLast(kfn, name);
		gAddLast(kfo, gNewWithInt(ShortInteger, desc));
	} else
		gDispose(name);
}








