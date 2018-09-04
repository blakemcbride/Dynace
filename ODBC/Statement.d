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


#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#include <windows.h>
#include <sql.h>
#include <sqlext.h>
#include <ctype.h>
#include <string.h>
#include <malloc.h>

/*  for gFldSetFile  */
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>

#include "dynsql.h"
#include "../Windows/demo.h"
#include "sqlparse.h"


#define	TYPE_NONE	0
#define	TYPE_SELECT	1
#define	TYPE_INSERT	2


#define	Streq(a,b)	!strcmp((a),(b))
#define	Strne(a,b)	strcmp((a),(b))


#ifndef	WIN32
#define SQL_WCHAR		(-8)
#define SQL_WVARCHAR	 	(-9)
#define SQL_WLONGVARCHAR 	(-10)
#define SQL_GUID		(-11)
#endif

#define	BUF_SIZE	4000

#define	LAZY_SELECT	1
#define	LAZY_SELECTONE	2


#define DB_ERROR_MESSAGE_SIZE	1024


defclass  Statement : PropertyList  {
	iDatabase;
	HSTMT	iStmt;
	iCols;		/*  dictionary of result column names and StatementInfo objects  */
	iColList;	/*  LinkObject of columns (in order)  */
	int	iTType; /*  1=select, 2=insert, etc.                             */
	int	iDBMS;
	int	iDoneFirst;
	UWORD	iRowStatus;
	char	iTable[100];
	iErrors;	/*  error codes to return instead of catching  */
	iDialogs;	//  Attached dialogs
	int	iStmtAlreadyClosed;

	object		iTag;		/*  arbitrary tag object associated
					    with the statement */

	int	iPos;		//  current record number (one origin)
	int	iEof;		//  -1=prior to first record, 0=valid record, 1=past end
	
	int	iAutoDisposeTag;
	int	iSingleTable;
	iTP;			//  transaction processing object
	int	iDataSync;

	/*  Lazy load  */
	int	iLazyLoad;
	iLazyStatement;
	ifun	iLazyFun;
	int	iLazyType;

	iTableList;		// iTable contains the first table, this contains the rest.
	int	iVarTextChanged;
	int	iReadOnly;
	int	iLockEnableWrites;

	iLastSelect;

	int	iIgnoreAllErrors;
	int	iReturnAllErrors;
	int	iErrorState;
	char	iInternalState[10];
	long	iInternalNativeCode;
	
	//  Cursor stuff

	int	iUseCursors;
	iCursorFile;
	int	iRecordsRead;
	int	iRecordLength;
	int	iOracleSQLStatistics;

	ifun	iRecordTestFunction;


	//  Field History stuff
	iFH_Table;		/*  Name of table to store history in  */
	iFH_Tables;		/*  StringDictionary by Table name  */
	long	iUserID;
	iSpecialFH_Tables;
	ifun	iSpecialAuditHandler;

class:
	int	cNumbStatements;
	int	cSaveLastSelect;

	//  Timing stuff
	FILE	*cLFP;
	LARGE_INTEGER	cFreq;
	LARGE_INTEGER	cCount;
	LARGE_INTEGER	cEnd;
	unsigned long	cSeq;

	char	*cBuf;
	int	cBufSiz;

	RETCODE	cRet;
	char	cEmsg[DB_ERROR_MESSAGE_SIZE];
};

char	*lcase(char *v);

extern	char	*fix_sql(char *s);

static	char	*fix_statement(char *s, int dbms);
static	void	begin_log(void);
static	void	end_log(void);
static	void	print_log(char *fun, char *stmt, char *org);
static	char	*query_buffer(int reclen);


private	imeth	int	pTrapError(object self, object es, int err);
private	imeth	char	*pFldGetVarText(object si, char *fld, char *vttbl);
private	imeth	pFldSetVarText(object si, char *fld, char *str, char *vttbl);
private	imeth	char	*pGetVarTextTable(object self, object si, char *fld);

private	imeth	int	pSelectWATCOMPrimaryKeyFields(object self, char *table, char *oname);
private	imeth	int	pSelectWATCOMForeignKeys(object self, char *table, char *oname);
private	imeth	int	pSelectWATCOMReferencedBy(object self, char *table, char *oname);
private	imeth	int	pSelectWATCOMIndexes(object self, char *table, char *oname);
private	imeth	int	pSelectMSSQLIndexes(object self, char *table, char *oname);
private	imeth	int	pSelectWATCOMTriggers(object self, char *table, char type, char *oname);
private	imeth	int	pSelectMSSQLTriggers(object self, char *table, char type, char *oname);
private	imeth	int	pSelectWATCOMColumns(object self, char *table, char *oname, int byname);
private	imeth	int	pSelectMSSQLColumns(object self, char *table, char *oname, int byname);
private	imeth	int	pSelectMYSQLIndexes(object self, char *table, char *oname);
private imeth	int	pSelectORACLEPrimaryKeyFields(object self, char *table, char *oname);


#define	BEGIN_LOG()			if (cLFP)	begin_log()
#define	END_LOG()			if (cLFP)	end_log()
#define	PRINT_LOG(fun, stmt, org)	if (cLFP)	print_log(fun, stmt, org)


cvmeth	vNew(db, fht, fhts, long userID)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	static	char	err[] = "vNew::Statement Error";

	DEMO_CHK_MAX;

#ifndef	_WIN32
	TableInfo;	// to fix some sort of link bug with MSC 16 bit 
#endif

	iDBMS = gDBMS_type(db);
	iFH_Table = fht;
	iFH_Tables = fhts;
	iUserID = userID;
	if (iTP = gGetTP(db))
		iDataSync = 1;
	

	iDatabase = db;
	iStmt = SQL_NULL_HSTMT;
	cRet = SQLAllocStmt(gHDBC(db), &iStmt);
	if (cRet)
		vError(self, err); 

	gAddStatement(db, obj);
	cNumbStatements++;
	return obj;
}

imeth	object	gGCDispose : GCDispose()
{
	if (iStmt != SQL_NULL_HSTMT)
		cRet = SQLFreeStmt(iStmt, SQL_DROP);
	cNumbStatements--;
	return gDispose(super);
}

char	*lcase(char *v)
{
	char	*b = v;
	for ( ; *v ; ++v)
		if (isupper(*v))
			*v = tolower(*v);
	return b;
}

private	imeth	char	*pGetOwnerName(object self, char* oname)
{
	char	*to = (oname && *oname) ? oname : NULL;

	if (!to && iDBMS == DBMS_ORACLE)
		to = gUserName(iDatabase);

	return to;
}

char	*lcname(char *name)
{
	static	char	buf[100];
	strcpy(buf, name);
	return lcase(buf);
}

private	imeth	pUnattachDialog(object self, object dlg, int remove_from_list)
{
	object	ctls;  // A StringDictionary of controls
	object	seq, i, ctl, si;
	char	*cname;

//	if (IsObj(dlg)  &&  (gIsKindOf(dlg, Dialog)  ||  gIsKindOf(dlg, Window)))
//  I do this to disassociate this class with the Window and Dialog classes
	if (IsObj(dlg)  &&  (gIsKindOf(dlg, gFindClass(Class, "Dialog"))  ||  gIsKindOf(dlg, gFindClass(Class, "Window"))))
		if (ctls = gControls(dlg))
			for (seq = gSequence(ctls) ; i = gNext(seq) ; )  {
				ctl = gValue(i);
				cname = gName(ctl);
				if (!cname  ||  !*cname)
					continue;
				si = gFindValueStr(iCols, lcname(cname));
				if (si  &&  gGetSI(ctl) == si  &&  gGetValue(si) == gValue(ctl)) {
					gUnattach(ctl);
					gSetSI(ctl, NULL);
				}
			}
	if (remove_from_list && iDialogs) {
		object	lnkval;
		int	done = 0;

		for (seq = gSequenceLinks(iDialogs); !done && (lnkval = gNext(seq)); )
			if (dlg == gValue(lnkval)) {
				gDispose(lnkval);
				done = 1;
			}
		if (lnkval)
			gDispose(seq);
	}
	return self;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (iDialogs) {
		object	seq, dlg;

		for (seq = gSequence(iDialogs); dlg = gNext(seq); )
			pUnattachDialog(self, dlg, 0);
		iDialogs = gDispose(iDialogs);
	}
	if (iCols)
		gDispose(iCols);
	if (iColList)
		gDeepDispose(iColList);
	if (iTableList)
		gDeepDispose(iTableList);
	if (iErrors)
		gDeepDispose(iErrors);
	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);
	if (iCursorFile)
		gDispose(iCursorFile);
	gDisposePropertyList(self);
	if (iLastSelect)
		gDispose(iLastSelect);
	if (iLazyStatement)
		gDispose(iLazyStatement);
	GCDispose(self);
	gRemoveStatement(iDatabase, self);
	return NULL;
}

cmeth	int	gSize()
{
	return cNumbStatements;
}

imeth	HSTMT	gHSTMT()
{
	return iStmt;
}

imeth	char	*gGetErrorMessage()
{
	char	state[100];
	
	SQLError(gHENV(iDatabase), gHDBC(iDatabase), iStmt, state, NULL,
		 cEmsg, sizeof cEmsg, NULL);
	return cEmsg;
}

imeth	char	*gGetErrorState()
{
	SDWORD	native;

	if (*iInternalState)
		return iInternalState;
	
	SQLError(gHENV(iDatabase), gHDBC(iDatabase), iStmt, iInternalState, &native,
		 cEmsg, sizeof cEmsg, NULL);
		
	iInternalNativeCode=native; 
		 
	return iInternalState;
}

imeth 	long gGetNativeErrorCode()
{
	return iInternalNativeCode;
}

ivmeth	vError(char *fmt, ...)
{
	char	buf1[256];
	MAKE_REST(fmt);

	vsprintf(buf1, fmt, _rest_);

	vError(super, "%s\n%s\n%s\n%ld", buf1, cEmsg, gGetErrorState(self), gGetNativeErrorCode(self));
	return self;
}

cmeth	char	*gGetErrorMessage : c_emsg()
{
	return cEmsg;
}

imeth	int	gGetErrorCode()
{
	return cRet;
}

cmeth	int	gGetErrorCode : c_ecode ()
{
	return cRet;
}

imeth	gRemoveVarText(object self, object flds)
{
	if (flds) {
		object	seq, sa;
		long	id;
		
		for (seq=gSequence(flds) ; sa = gNext(seq) ; )
			if (id = gFldGetLong(self, gStringKey(sa)))
				gDeleteVarText(iDatabase, gStringValue(gValue(sa)), id);
	}
	return self;
}

/*
  Return  0 Doesn't have from keyword
	  1 Has from keyword

  This function assumes that a valid delete statement has been passed in.
*/

static	int	get_delete_table(char *table, char *cmd)
{
	char	*p, *t;

	*table = '\0';
	
	for (p = cmd + 7 ; *p  &&  isspace(*p) ; p++);
	for (t = table ; *p  &&  !isspace(*p) ; p++, t++)
		*t = *p;
	*t = '\0';
	if (stricmp(table, "from"))
		return 0;

	for ( ; *p  &&  isspace(*p) ; p++);
	for (t = table ; *p  &&  !isspace(*p) ; p++, t++)
		*t = *p;
	*t = '\0';
	
	return 1;
}

static	int	get_update_table(char *table, char *cmd)
{
	char	*p, *t;

	*table = '\0';
	
	for (p = cmd + 7 ; *p  &&  isspace(*p) ; p++);
	for (t = table ; *p  &&  !isspace(*p) ; p++, t++)
		*t = *p;
	*t = '\0';
	return 1;
}

static	int	get_insert_table(char *table, char *cmd)
{
	char	*p, *t;

	*table = '\0';
	
	for (p = cmd + 7 ; *p  &&  isspace(*p) ; p++);
	for (t = table ; *p  &&  !isspace(*p) ; p++, t++)
		*t = *p;
	*t = '\0';
	if (stricmp(table, "into"))
		return 0;

	for ( ; *p  &&  isspace(*p) ; p++);
	for (t = table ; *p  &&  !isspace(*p) ; p++, t++)
		*t = *p;
	*t = '\0';
	
	return 1;
}

extern	long	Date_ymd(long date, int *year, int *month, int *day);

#define MAX_PARAMS 300

private imeth int runPreparedStatement(char *cmd)
{
	char	*cmdstart=cmd;
	int	bufsize=strlen(cmdstart)+1;
	char	*buf;		//should be smaller than cmd always
	
	char	*idx;
	int	top=0;
	int	r=0;
	char	*params[MAX_PARAMS];
	int	loop;
	SQLLEN	nts=(SQLLEN)SQL_NTS;

	if (iDBMS == DBMS_ORACLE  ||  strnicmp(cmd, "SELECT", 6)  &&  strnicmp(cmd, "INSERT", 6)  &&  strnicmp(cmd, "UPDATE", 6)  &&  strnicmp(cmd, "DELETE", 6)) {
		char *p = fix_statement(cmd, iDBMS);
		r = SQLExecDirect(iStmt, p, SQL_NTS);
		free(p);
		return r;
	}

	idx = buf = malloc(bufsize*2);		//should be smaller than cmd always

	while (*cmd) {
		
		if (top >= MAX_PARAMS) { //too many parameters, abort
			r=1;
			break;
		}
		
		//if I am in any kind of select, then I need to just copy until I hit the from or the end of the statement
		if (!strnicmp(cmd, "SELECT ", 7)) {
			while (*cmd && strnicmp(cmd, " FROM ", 6))
				*idx++ = *cmd++;
			continue;
		}
		if ((*cmd >= 'a' && *cmd <= 'z') || (*cmd >= 'A' && *cmd <= 'Z')) {
			do {
				*idx++ = *cmd++;
			} while ((*cmd>='a'&&*cmd<='z')||(*cmd>='A'&&*cmd<='Z')||(*cmd=='_')||(*cmd>='0'&&*cmd<='9')||(*cmd=='.'));
			continue;
		} else if (*cmd == '\'') {
			int pos=0;
			params[top] = malloc(bufsize);
			*idx++ = '?';
			*idx++ = ' ';
			do {
				cmd++;
				params[top][pos++] = *cmd;
			} while (*cmd  &&  *cmd != '\'');
			params[top][pos-1] = 0;
			top++;
		} else if (*cmd == '~') {
			int pos=0;
			params[top] = malloc(bufsize);
			*idx++ = '?';
			*idx++ = ' ';
			do {
				cmd++;
				params[top][pos++]=*cmd;
			} while (*cmd  &&  *cmd!='~');
			params[top][pos-1]=0;
			top++;
		} else if (*cmd == '{') {
			do {
				*idx++ = *cmd++;
			} while (*cmd  &&  *cmd!='}');
			*idx++ = *cmd++;
			continue;
		} else if (*cmd >= '0' && *cmd<='9' || *cmd == '-' || *cmd == '.') {
			int pos=0;
			params[top] = malloc(bufsize);

			*idx++ = '?';
			*idx++ = ' ';
			do {
				params[top][pos++] = *cmd++;
			} while (*cmd >= '0' && *cmd <= '9' || *cmd == '.');
			params[top++][pos]=0;
			cmd--;
		} else
			*idx++ = *cmd;
		if (*cmd)
			cmd++;
	}

	*idx = '\0';
	
	if (!r)
		r = SQLPrepare(iStmt, buf, SQL_NTS);


	if (r)  { //If I can't prepare the statement, just run it as it was
		char *p = fix_statement(cmdstart, iDBMS);
		r = SQLExecDirect(iStmt, p, SQL_NTS);

		free(p);
		goto end;
	}
	
	for (loop=0 ; loop<top ; loop++)
		r=SQLBindParameter(iStmt,(SQLUSMALLINT)(loop+1),(SQLSMALLINT)SQL_PARAM_INPUT,(SQLSMALLINT)SQL_C_CHAR,(SQLSMALLINT)SQL_VARCHAR,(SQLUINTEGER)0,(SQLSMALLINT)0,params[loop],
				(SQLINTEGER)(strlen(params[loop])+1),
				&nts);

	r = SQLExecute(iStmt);

	SQLFreeStmt(iStmt, SQL_RESET_PARAMS);

	if (r)  {  //If I can't run the statement, just run it as it was
		char *p = fix_statement(cmdstart, iDBMS);
		//Clean the query up all the way before we try to use it again
		SQLFreeStmt(iStmt, SQL_CLOSE);
		SQLFreeStmt(iStmt, SQL_UNBIND);
		r = SQLExecDirect(iStmt, p, SQL_NTS);

		free(p);
	}
	
 end:
	for (loop=0 ; loop < top ; loop++)
		free(params[loop]);

	free(buf);

	return r;
}

imeth	int	gExecute(char *cmd)
{
	int	r;
	char	tname[100];
	int	tries = 0;

	if (iReadOnly)
		return 0;
	while (*cmd == ' ')
		cmd++;
	if (!strnicmp("delete ", cmd, 7)) {	// Delete VarText first
		int	hasFrom = get_delete_table(tname, cmd);
		object	flds = gGetVarTextFields(iDatabase, tname);
		char	*cp = hasFrom ? "" : "from ";
		
		if (flds) {
			object	stmt = gNewNonCacheStatement(iDatabase);
			char	tbuf[1024];
			gReturnAllErrors(stmt, 1);
			sprintf(tbuf, "select * %s%s", cp, cmd + 7);
			if (!gDBSelect(stmt, tbuf))
				while (!gNextRecord(stmt))
					gRemoveVarText(stmt, flds);
			gDispose(stmt);
		}
		gSetChanged(iDatabase);
	} else if (!strnicmp("update ", cmd, 7)) {
		get_update_table(tname, cmd);
		gSetChanged(iDatabase);
	} else if (!strnicmp("insert ", cmd, 7)) {
		get_insert_table(tname, cmd);
		gSetChanged(iDatabase);
	} else
		*tname = '\0';
//	
again:
	gEndSQL(self);
	BEGIN_LOG();
	// Read this thing and get a prepared style statement from it
	r = runPreparedStatement(self, cmd);

	END_LOG();
	PRINT_LOG("gExecute", cmd, cmd);


	if (r == 1  &&  iDBMS == DBMS_POSTGRES)
		r = 0;
	if (r  &&  pTrapError(self, iErrors, r)) {
		char	state[6], buf[1024];
		
		SQLError(gHENV(iDatabase), gHDBC(iDatabase), iStmt, state, NULL, buf, sizeof buf, NULL);

		// if it is a drop command or it is not any of the below states
		if (!strnicmp("drop ", cmd, 5) ||

		    //  Oracle returns 42S11 or S1000 if you create an index on a field which already has an index
		    //  for example if it was declared as a primary key
		    !strnicmp("create index", cmd, 12) && iDBMS == DBMS_ORACLE &&  Streq(state, "42S11") || 
		    !strnicmp("create index", cmd, 12) && iDBMS == DBMS_ORACLE &&  Streq(state, "S1000") || 

		    Streq(state, "42000") &&  iDBMS == DBMS_WATCOM   //  bug in SqlAnywhere 5.x

		    // S1000 in Oracle indicates inserting NULL into a not null column
//		    Streq(state, "S1000")  &&  iDBMS == DBMS_POSTGRES       I don't remember why this was needed
			)

			r = 0;  //  ignore the error
		else
			if (!strnicmp("insert ", cmd, 7)  &&  Streq(state, "23000")) {
				return 23000; // I know the gTPExecute is being avoided
			} else if (iIgnoreAllErrors) {

				return 0;
			} else if (Streq(state, "40001")  &&  ++tries <= 3)
				goto again;
			else
				vError(super, "gExecute::Statement error %d, %s %s in query %s", r, state, buf, cmd);
	}
	if (iDataSync  &&  iTP  &&  !r)
		gTPExecute(iTP, tname, cmd);


	return r;
}

private	imeth	void	updateOrgValues()
{
	object	si, seq, pk, key;

#if 0
	if (iSingleTable  &&  *iTable)
		if (iDataSync)
			for (seq = gSequence(iCols) ; si = gNext(seq) ; )
				gUpdateOriginalValue(gValue(si));
		else if ((pk = gGetPrimaryKey(iDatabase, iTable))  &&  gSize(pk))
			for (seq=gSequence(pk) ; key = gNext(seq) ; ) {
				si = gFindValueStr(iCols, gStringValue(key));
				if (si)
					gUpdateOriginalValue(si);
			}
#else
	for (seq = gSequence(iCols) ; si = gNext(seq) ; )
		gUpdateOriginalValue(gValue(si));
#endif
	iVarTextChanged = 0;
}

private	imeth	void	cursor_write(int rec)   //  rec is zero origin
{
	object	seq, si;

	if (!iCursorFile)
		if (!iUseCursors)
			return;
		else
			if (!(iCursorFile = gOpenTempFile(BufferedTempFile)))
				gError(Object, "Error opening ODBC cursor file");
	if (!gSeek(iCursorFile, rec * iRecordLength) &&  rec)
		gError(Object, "ODBC Cursor seek error.");
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (!gCursorWrite(si, iCursorFile))
			gError(Object, "ODBC Cursor write error.");
	if (rec == iRecordsRead)
		iRecordsRead++;
}

imeth int gRecordCount()
{
	return iRecordsRead;
}

private	imeth	void	cursor_read(int rec)   //  rec is zero origin
{
	object	seq, si;
	
	if (!iCursorFile)
		return;
	if (!gSeek(iCursorFile, rec * iRecordLength)  &&  rec)
		gError(Object, "ODBC Cursor seek error.");
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (!gCursorRead(si, iCursorFile))
			gError(Object, "ODBC Cursor read error.");
}

private	imeth	int	bindCols(char *err)
{
	RETCODE	r;
	SWORD	n, type, scale, nulls;
	UWORD	i;
	SQLULEN	prec;
	char	cname[100];
	object	si;
	int	size;

	r = SQLNumResultCols(iStmt, &n);
	if (r)
		if (iIgnoreAllErrors) {
			iErrorState = 1;
			return -1;
		} else
			vError(self, err);
	iCols = gNewWithInt(StringDictionary, 101);
	iColList = gNew(LinkObject);
	iRecordLength = 0;
	for (i=1 ; i <= n ; i++)  {
		r = SQLDescribeCol(iStmt, i, cname, sizeof(cname)-1, NULL,
				   &type, &prec, &scale, &nulls);
//  Handle inconsistancies in WATCOM before and after ver 5.5.01
		if (!strcmp(cname, "count(*)")  ||  !strcmp(cname, "\"count\"(*)"))
			strcpy(cname, "count");
		if (!r)
			si = gNewSelect(StatementInfo, self, iStmt, (int) i, lcase(cname), type, prec, scale, nulls, &r,
					iDBMS, &size);
		if (r  ||  !si)
			if (iIgnoreAllErrors) {
				iErrorState = 1;
				return -1;
			} else
				vError(self, err);
		gAddStr(iCols, cname, si);
		gAddLast(iColList, si);
//		if (type != SQL_GUID)
			iRecordLength += size;
	}
	iTType = TYPE_SELECT;
	return r;
}

/*
  Return 1 single table select
         0 multiple table select

  Basically, we assume that if there exists a comma or JOIN between the
  FROM and a WHERE or ORDER then it is a multi-table select.
*/

#define	INC	cmd++, len--

static	int	get_table(char *table, char *cmd, object *tlist)
{
	int	len = strlen(cmd);
	int	rval = 1;
	char	tbl[100];
	char	*tp;

	*table = '\0';
	while (len > 6)
		if ((cmd[0] == ' '  ||  cmd[0] == '\t')  &&
		    (cmd[1] == 'f'  ||  cmd[1] == 'F')  &&
		    (cmd[2] == 'r'  ||  cmd[2] == 'R')  &&
		    (cmd[3] == 'o'  ||  cmd[3] == 'O')  &&
		    (cmd[4] == 'm'  ||  cmd[4] == 'M')  &&
		    (cmd[5] == ' '  ||  cmd[5] == '\t')) {
			cmd += 6;
			while (*cmd == ' '  ||  *cmd == '\t')
				INC;
			for ( ; *cmd  &&  *cmd != ' '  &&  *cmd != '\t'  &&  *cmd != ',' ; INC)
				*table++ = tolower(*cmd);
			*table = '\0';
			break;
		} else
			INC;
	for ( ; *cmd ; INC) {
		while (cmd[0] == ',') {
			*tbl = '\0';
			cmd++;
			while (*cmd == ' '  ||  *cmd == '\t')
				INC;
			for (tp = tbl ; *cmd  &&  *cmd != ' '  &&  *cmd != '\t'  &&  *cmd != ',' ; INC)
				*tp++ = tolower(*cmd);
			*tp = '\0';
			if (!*tlist)
				*tlist = gNew(LinkObject);
			gAddLast(*tlist, gNewWithStr(String, tbl));
			rval = 0;
		}
		if (len > 7) {
			if ((cmd[0] == ' '  ||  cmd[0] == '\t')  &&
			    (cmd[1] == 'w'  ||  cmd[1] == 'W')  &&
			    (cmd[2] == 'h'  ||  cmd[2] == 'H')  &&
			    (cmd[3] == 'e'  ||  cmd[3] == 'E')  &&
			    (cmd[4] == 'r'  ||  cmd[4] == 'R')  &&
			    (cmd[5] == 'e'  ||  cmd[5] == 'E')  &&
			    (cmd[6] == ' '  ||  cmd[6] == '\t'))
				break;
			if ((cmd[0] == ' '  ||  cmd[0] == '\t')  &&
			    (cmd[1] == 'o'  ||  cmd[1] == 'O')  &&
			    (cmd[2] == 'r'  ||  cmd[2] == 'R')  &&
			    (cmd[3] == 'd'  ||  cmd[3] == 'D')  &&
			    (cmd[4] == 'e'  ||  cmd[4] == 'E')  &&
			    (cmd[5] == 'r'  ||  cmd[5] == 'R')  &&
			    (cmd[6] == ' '  ||  cmd[6] == '\t'))
				break;
			if ((cmd[0] == ' '  ||  cmd[0] == '\t')  &&
			    (cmd[1] == 'j'  ||  cmd[1] == 'J')  &&
			    (cmd[2] == 'o'  ||  cmd[2] == 'O')  &&
			    (cmd[3] == 'i'  ||  cmd[3] == 'I')  &&
			    (cmd[4] == 'n'  ||  cmd[4] == 'N')  &&
			    (cmd[5] == ' '  ||  cmd[5] == '\t')) {
				*tbl = '\0';
				cmd += 6;
				while (*cmd == ' '  ||  *cmd == '\t')
					INC;
				for (tp = tbl ; *cmd  &&  *cmd != ' '  &&  *cmd != '\t' ; INC)
					*tp++ = tolower(*cmd);
				*tp = '\0';
				if (!*tlist)
					*tlist = gNew(LinkObject);
				gAddLast(*tlist, gNewWithStr(String, tbl));
				rval = 0;
			}
		}
	}
	return rval;
}

#if 0

//imeth	int	gDBSelect, gExecuteWithBind  (char *cmd)
{
	RETCODE	r;
	static	char	err[] = "gDBSelect::Statement Error";
	char	*p;

	DEMO_CHK_MAX;
	gEndSQL(self);

	BEGIN_LOG();
	r = SQLExecDirect(iStmt, p=fix_statement(cmd, iDBMS), SQL_NTS);
	END_LOG();
	PRINT_LOG("gDBSelect", p, cmd);
	free(p);
	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	iSingleTable = get_table(iTable, cmd, &iTableList);
	return !r || r == SQL_SUCCESS_WITH_INFO ? bindCols(self, err): -1;
}

#else

private	imeth	int	DBSelect (char *cmd, int useCursors)
{
	RETCODE	r;
	static	char	err[] = "gDBSelect::Statement Error";

	int	tries = 0;


	DEMO_CHK_MAX;
	gEndSQL(self);
	gEnterCriticalSection(iDatabase);
	if (cSaveLastSelect)
		if (iLastSelect)
			gChangeStrValue(iLastSelect, cmd);
		else
			iLastSelect = gNewWithStr(String, cmd);
	
	if (iDBMS == DBMS_MSSQL)
		useCursors = 1;
	iUseCursors = useCursors;
//	p = fix_statement(cmd, iDBMS);

 again:
	BEGIN_LOG();
	r=runPreparedStatement(self,cmd);

	END_LOG();
	PRINT_LOG("gDBSelect", cmd, cmd);

	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r)) {
		char	state[10];

		if (iIgnoreAllErrors) {
			iErrorState = 1;

			gLeaveCriticalSection(iDatabase);
			return 0;
		} else  {
			char	state[6], buf[180];
			char *tmpBuf;

			SQLError(gHENV(iDatabase), gHDBC(iDatabase), iStmt, state, NULL, buf, sizeof buf, NULL);
			if (Streq(state, "40001")  &&  ++tries <= 3) {
				gEndSQL(self);
				goto again;
			}
			vError(super, "gDBSelect statement error %d, %s %s in query %s", r, state, buf, cmd);
		}
	}

	iSingleTable = get_table(iTable, cmd, &iTableList);


	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		r = bindCols(self, err);
		if (iDBMS == DBMS_MSSQL) {
			int	r=0;
			while (!r) {
				r = SQLFetch(iStmt);
				if (!r  &&  (!iRecordTestFunction  ||  iRecordTestFunction(self)))
					cursor_write(self, iPos++);
			}
			if (r != SQL_NO_DATA_FOUND  &&  !iIgnoreAllErrors)
				vError(self, "gDBSelect cursor load");
			SQLFreeStmt(iStmt, SQL_CLOSE);
			SQLFreeStmt(iStmt, SQL_UNBIND);
			iStmtAlreadyClosed = 1;
			if (r == SQL_NO_DATA_FOUND)
				iPos = 0;
			else if (iIgnoreAllErrors) {
				gLeaveCriticalSection(iDatabase);
				return 0;
			}
		}
		gLeaveCriticalSection(iDatabase);
		return r;
	} else {
		gLeaveCriticalSection(iDatabase);
		return -1;
	}
}

imeth	int	gDBSelect, gExecuteWithBind, gDBSelectDNC  (char *cmd)
{
	return DBSelect(self, cmd, 1);
}

imeth	int	gDBSelectNC(char *cmd)
{
	return DBSelect(self, cmd, 0);
}

imeth	int	gLazyDBSelect, gLazyExecuteWithBind (ifun fun, char *cmd)
{
	iLazyLoad = 1;
	if (iLazyStatement)
		gDispose(iLazyStatement);
	iLazyStatement = gNewWithStr(String, cmd);
	iLazyFun = fun;
	iLazyType = LAZY_SELECT;
	return 0;
}

imeth	int	gLazyDBSelectOne(ifun fun, char *cmd)
{
	iLazyLoad = 1;
	if (iLazyStatement)
		gDispose(iLazyStatement);
	iLazyStatement = gNewWithStr(String, cmd);
	iLazyFun = fun;
	iLazyType = LAZY_SELECTONE;
	return 0;
}

private	imeth	int	pDoLazyLoad()
{
	object	ss;
	char	*cmd;
	int	r = 0;
	
	if (!iLazyLoad)
		return r;
	ss = iLazyStatement;
	iLazyStatement = NULL;
	iLazyLoad = 0;
	switch (iLazyType) {
	case LAZY_SELECT:
		if ((r=gDBSelect(self, cmd=gStringValue(ss)))  &&  iLazyFun) {
			iLazyFun(self);
			r = gDBSelect(self, cmd);
			if (r == SQL_NO_DATA_FOUND)
				r = 0;
		}
		break;
	case LAZY_SELECTONE:
		if ((r=gDBSelectOne(self, cmd=gStringValue(ss)))  &&  iLazyFun) {
			iLazyFun(self);
			r = gDBSelectOne(self, cmd);
			if (r == SQL_NO_DATA_FOUND)
				r = 0;
		}
		break;
	}
	gDispose(ss);
	return r;
}

#define	LAZY_LOAD	if (iLazyLoad  &&  pDoLazyLoad(self))  vError(self, "LazySelect Error")

#endif

imeth	int	gDoLazyLoad()
{
	return pDoLazyLoad(self);
}	

private	imeth	int	DBSelectOne(char *cmd, int useCursors)
{
	UDWORD	crow;
	int	r = DBSelect(self, cmd, useCursors);
	if (r)
		return r;
	if (iErrorState)
		return -1;
	if (iDBMS == DBMS_MSSQL) {
		if (iRecordsRead) {
			cursor_read(self, 0);
			iPos = 1;
			iEof = 0;
			updateOrgValues(self);
		} else
			return SQL_NO_DATA_FOUND;
	} else {
		do {
			r = SQLFetch(iStmt);
		} while (!r  &&  iRecordTestFunction  &&  !iRecordTestFunction(self));
		if (r  &&  r != SQL_NO_DATA_FOUND  &&  pTrapError(self, iErrors, r))
			if (iIgnoreAllErrors) {
				iErrorState = 1;
				return -1;
			} else
				vError(self, "gDBSelectOne");
		if (!r) {
			cursor_write(self, iPos++);
			iEof = 0;
			updateOrgValues(self);
		}
	}
	return r;
}

imeth	int	gDBSelectOne, gDBSelectOneDNC (char *cmd)
{
	return DBSelectOne(self, cmd, 1);
}

imeth	int	gDBSelectOneNC(char *cmd)
{
	return DBSelectOne(self, cmd, 0);
}

private	imeth	pUpdateVarText(object self)
{
	object	seq, si;

	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		gWriteVarText(si);
	
	return self;
}

private	imeth	pUndoVarText(object self)
{
	object	seq, si;

	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		gUndoVarText(si);
	
	return self;
}

#if 0

//   This insert mechanism depends on the iTables field of the Database class

//imeth	int	gInsert(char *table)
{
	RETCODE	r;
	object	ti;

	gEndSQL(self);
	ti = gGetTable(iDatabase, table);
	if (!ti)
		return -1;
	if (!cBuf)
		cBuf = malloc(cBufSiz=2048);
	if (!cBuf)
		return -1;
	gMakeInsert(ti, cBuf);
	r = SQLPrepare(iStmt, cBuf, SQL_NTS);
	if (r)
		return r;
	iCols = gNewWithInt(StringDictionary, 101);
	iColList = gNew(LinkObject);
	gBindColumns(ti, self, iStmt, iCols);
	iTType = TYPE_INSERT;
	return r;
}

//imeth	int	gAddRecord()
{
	if (iTType != TYPE_INSERT)
		return -1;
	return SQLExecute(iStmt);
}

#else

private	imeth	int	pInsert(char *table, int r)
{
	UDWORD	crow;

	if (iErrorState)
		return 0;
	if (iDBMS == DBMS_MSSQL)
		return r;
//  The following lines _are_ required!
/*	if (!r)
		r = SQLFetch(iStmt);

	if (r  &&  r != SQL_NO_DATA_FOUND  &&  pTrapError(self, iErrors, r))
		if (iIgnoreAllErrors) {
			iErrorState = 1;
			return 0;
		} else
			vError(self, "gInsert: Error %d on table '%s'", r, table);

			*/
	return r;
}

private	imeth	int	makeBlankRow(char *cmd)
{
	object columns,dbTable,cit,seq;
	int colNum=0;
	int i=0;

	if (iCols)
		gDeepDispose(iCols);
	if (iColList)
		gDispose(iColList);

	iCols = gNewWithInt(StringDictionary, 101); //I'm expecting to have a column object
	iColList = gNew(LinkObject);
				
	iRecordLength = 0;
	//get the columns from the database

	iSingleTable = get_table(iTable, cmd, &iTableList);

	dbTable=gGetTable(iDatabase,iTable);
	
	columns=gColumns(dbTable);  //this is a string dictionary of column info objs

	for (seq=gSequence(columns) ; IsObj(seq) && ( cit = gNext(seq)) ; ) {
		object	ci=gValue(cit);
		char	*name=lcase(gName(ci));	
		object	statementinfo;
		RETCODE r;
		int	cize;
		SWORD	scale, nulls;
		int	prec;		
		SWORD	colType=gType(ci);

		i++;

		scale=gColumnScale(ci);

		nulls=gIsNullable(ci);
		prec=gSize(ci);

		statementinfo = gNewSelect(StatementInfo, self, iStmt, (int) gNumber(ci), name, colType, prec, scale, nulls, &r, iDBMS, &cize);
		
		gAddStr(iCols, name, statementinfo);
		gAddLast(iColList, statementinfo);
		iRecordLength+=cize;
	}

	iTType = TYPE_SELECT; //pretend I did this with a select

	return 0;
}

imeth	int	gInsert(char *table)
{
	char	cmd[100];
	int	r=0;
	object	tablelist;

	sprintf(cmd, "select * from %s where 1=2", table);

	makeBlankRow(self,cmd);

	//r = DBSelect(self, cmd, 0);
	return pInsert(self, table, r);
}

static	void	updateFields(ivType *iv)
{
	object	seq, i;

	for (seq = gSequence(iCols) ; i = gNext(seq) ; )
		gUpdate(gValue(i));
}

imeth	gClear()
{
	object	seq, i;

	if (iCols)
		for (seq = gSequence(iCols) ; i = gNext(seq) ; )
			gClear(gValue(i));

	iVarTextChanged = 0;
	return self;
}



#define	GO_END								\
	while (*p) {							\
		p++;							\
		if (++n > QUERY_BUFFER_SIZE + iRecordLength)		\
			vError(self, "QueryBuffer overflow");		\
	}

private	imeth	char	*make_addrec()
{
	char	*buf = query_buffer(iRecordLength*3);
	char	*p;
	object	seq, si, trc = gGetTable(CacheResult, iTable), cc;
	int	add_comma = 0, n=0;
		
	sprintf(p=buf, "INSERT INTO %s (", iTable);

	for (seq=gSequence(iColList) ; si = gNext(seq) ; ) {
		if (add_comma)
			strcat(buf, ", ");
		else
			add_comma = 1;	

		strcat(buf, gName(si));
	}

	add_comma = 0;

	strcat(buf,") VALUES (");
	GO_END;
	
	if (trc  &&  (cc = gFindValueStr(trc, "*"))) {
		gInvalidateResultCache(CacheResult, trc, cc);
		trc = NULL;
	}
	
	for (seq=gSequence(iColList) ; si = gNext(seq) ; ) {
		if (add_comma) {
			*p++ = ',';
			*p++ = ' ';
			n += 2;
		} else
			add_comma = 1;
		gFormatField(si, p);
		if (trc  &&  (cc = gFindValueStr(trc, gName(si))))
			gInvalidateResultCache(CacheResult, trc, cc);
		GO_END;
	}
	strcpy(p, ")");
	GO_END;
	return buf;
}

private	imeth	void	audit_addrec()
{
	char	*fn, *buf=NULL, *buf2=NULL, *p;
	object	seq, si, stmt=NULL, kseq, fld;
	object	auditList = iFH_Tables ? gFindValueStr(iFH_Tables, iTable) : NULL;
	int		usingSpecialAuditList = 0;

	if (!auditList) {
		auditList = iSpecialFH_Tables ? gFindValueStr(iSpecialFH_Tables, iTable) : NULL;
		usingSpecialAuditList = 1;
	}
	
	if (!auditList)
		return;
	for (seq=gSequence(iColList) ; si = gNext(seq) ; ) 
		if (gHasData(si))  {
			fn = gName(si);
			if (gFindStr(auditList, fn)) {
				if (!buf) {
					buf = query_buffer(iRecordLength);
					buf2 = query_buffer(iRecordLength);
					stmt = gNewStatement(iDatabase);

					for (p=buf, kseq=gSequence(gGetPrimaryKey(iDatabase, iTable)) ; fld = gNext(kseq) ; ) {
						gFormatFixedLengthField(gFindValueStr(iCols, gStringValue(fld)), p);
						for (; *p ; p++);
					}
				}
				if (!usingSpecialAuditList) {
					object	now = gNow(iDatabase);
					gInsert(stmt, gStringValue(iFH_Table));
					gFldSetString(stmt, "TableName", iTable);
					gFldSetChar(stmt, "ChangeType", 'A');
					gFldSetString(stmt, "KeyData", buf);
					gChangeValue(gFldGetValue(stmt, "ChangeTime"), now);
					gDispose(now);
					gUpdate(gGetField(stmt, "ChangeTime"));
					gFldSetLong(stmt, "UserID", iUserID);
					gFldSetString(stmt, "FieldName", gName(si));
					gFldSetString(stmt, "FieldValue", gFormatFixedLengthField(si, buf2));
					gAddRecord(stmt);
				}
				else {
					if (iSpecialAuditHandler)
						iSpecialAuditHandler(iTable, 'A', buf, iUserID, gName(si), gFormatFixedLengthField(si, buf2), self);
				}
			}
		}
	if (stmt)
		gDispose(stmt);
	if (buf)
		free(buf);
	if (buf2)
		free(buf2);
}

imeth	int	gAddRecord()
{
	int	r;

	if (iReadOnly  ||  iErrorState)
		return 0;
	if (iTType != TYPE_SELECT)
		return -1;
	if (iDBMS == DBMS_ACCESS)
		updateFields(iv);
	if (iVarTextChanged)
		pUpdateVarText(self);

	if (1  ||  iDBMS == DBMS_SYBASE  ||  iDBMS == DBMS_MYSQL  ||  iDBMS == DBMS_MSSQL) {
		char	*buf = make_addrec(self);
		object	stmt = gStatement(iDatabase);
		int	ds = gEnableTP(stmt, 0);
		int	ro = gIsReadOnly(stmt);
		if (ro)
			gEnableWrites(stmt);
		r = gExecute(stmt, buf);
		free(buf);
		gEnableTP(stmt, ds);
		if (ro)
			gDisableWrites(stmt);
	} else {
		char	*p;
		
		BEGIN_LOG();
		r = SQL_ADD_RECORD(iStmt, 1);  // argument must be 1 to work when file is empty
		END_LOG();
		if (cLFP) {
			char	*t;
			p = make_addrec(self);
			PRINT_LOG("gAddRecord", t=fix_statement(p, iDBMS), p);
			free(p);
			free(t);
		}
	}

	if (r  &&  pTrapError(self, iErrors, r))  {
		char	state[6], buf[128];

		if (iVarTextChanged)
			pUndoVarText(self);
		if ((1  ||  iDBMS == DBMS_SYBASE  ||  iDBMS == DBMS_MYSQL  ||  iDBMS == DBMS_MSSQL)  &&  r == 23000)
			return 1;
		SQLError(gHENV(iDatabase), gHDBC(iDatabase), iStmt, state, NULL, buf, sizeof buf, NULL);
		if (iDBMS != DBMS_ACCESS  &&  strcmp(state, "23000"))
			if (iIgnoreAllErrors)
				return 0;
			else
				vError(super, "gAddRecord error %s %s", state, buf);
		else if (iDBMS == DBMS_ACCESS  &&  strcmp(state, "S1000")  &&  strcmp(state, "01004"))
			if (iIgnoreAllErrors)
				return 0;
			else
				vError(super, "gAddRecord error %s %s", state, buf);
	}
	
	if (!r  &&  iDataSync  &&  iTP)
		gTPAdd(iTP, self);
	if (!r  &&  iFH_Tables)
		audit_addrec(self);
	return r;
}

imeth	int	gAddRecordWithAutoInc(char *table, char *field, char *where, char *order)
{
	object	stmt;
	int	r;
	long	id, seq;
	char	select[256], mutex[256], sequence[256];

	sprintf(mutex, "AutoInc Lock - %s", table);
	lcase(mutex+13);
	if (gCreateMutex(iDatabase, mutex, NULL, 60000, 60000, 250))
		return -1;  // error - can't create mutex lock

	sprintf(sequence, "AutoInc - %s", table);
	lcase(sequence+8);
	seq = gNextNumber(iDatabase, sequence);

	stmt = gNewNonCacheStatement(iDatabase);
	gReturnAllErrors(stmt, 1);
	sprintf(select, "select MAX(%s) as mmax from %s %s", field, table, where ? where : "");
	r = gDBSelectOneNC(stmt, select);
	id = r ? seq : gFldGetLong(stmt, "mmax") + 1L;
	if (id < seq)
		id = seq;
	if (id > seq)
		gSetLastNumber(iDatabase, sequence, id);
	gFldSetLong(self, field, id);
	r = gAddRecord(self);
	gDisposeMutex(iDatabase, mutex);
	gDispose(stmt);
	return r;
}

#endif

imeth	gGetField(char *fld)
{
	return iCols ? gFindValueStr(iCols, lcname(fld)) : NULL;
}

imeth	gFldGetValue(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetValue: No field '%s'", fld);
	return si ? gGetValue(si) : si;
}

imeth	void	*gFldGetPointer(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetPointer: No field '%s'", fld);
	return si ? gPointerValue(si) : NULL;
}


imeth	char	*gFldGetStringNoStrip(char *fld)
{
	char	*cp;
	char	*vttbl;
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetString: No field '%s'", fld);

	if (vttbl = pGetVarTextTable(self, si, fld))
		cp = pFldGetVarText(self, si, fld, vttbl);
	else
		cp = gStringValue(gGetValue(si));

	return cp;
}


imeth	char	*gFldGetString(char *fld)
{
	char	*cp;
	char	*vttbl;
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetString: No field '%s'", fld);

	if (vttbl = pGetVarTextTable(self, si, fld))
		cp = pFldGetVarText(self, si, fld, vttbl);
	else
		cp = gStringValue(gStripRight(gGetValue(si)));

	return cp;
}

imeth	char	*gFldGetStringXML(char *fld)
{
	char	*cp;
	char	*vttbl;
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetString: No field '%s'", fld);

	if (vttbl = pGetVarTextTable(self, si, fld))
		pFldGetVarText(self, si, fld, vttbl);
	cp = gStringValueXML(si);

	return cp;
}

imeth	char	*gFldToFile(char *fld, char *file)
{
	char	*cp;
	int	h;
	object	si;

	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldToFile: No field '%s'", fld);
	h = open(file, O_BINARY | O_CREAT | O_TRUNC | O_WRONLY, _S_IREAD | _S_IWRITE);
	if (gType(si) == SQL_LONGVARBINARY) {
		long	size;
		
		if (!gFldGetBinary(self, fld, &size, &cp)  &&  size) {
			write(h, cp, size);
			free(cp);
		} else
			cp = NULL;
	} else {
		cp = gFldGetString(self, fld);
		write(h, cp, strlen(cp));
	}
	close(h);
	return cp;
}

imeth	char	gFldGetChar(char *fld)
{
	object	val, si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetChar: No field '%s'", fld);
	else
		val = gGetValue(si);
	if (gIsKindOf(val, String))
		if (!gSize(val))
			return '\0';
		else
			return gCharValueAt(val, 0);
	else		
		return gCharValue(val);
}

imeth	short	gFldGetShort(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetShort: No field '%s'", fld);
	return si ? gShortValue(gGetValue(si)) : 0;
}

imeth	unsigned short	gFldGetUnsignedShort(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetUnsignedShort: No field '%s'", fld);
	return si ? gUnsignedShortValue(gGetValue(si)) : 0;
}

imeth	long	gFldGetLong(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetLong: No field '%s'", fld);
	return si ? gLongValue(gGetValue(si)) : 0L;
}

imeth	double	gFldGetDouble(char *fld)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetDouble: No field '%s'", fld);
	return si ? gDoubleValue(gGetValue(si)) : 0;
}

imeth	gFldGetDateTime(char *fld, long *dt, long *tm)
{
	object	si;

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldGetDateTime: No field '%s'", fld);
	else
		gDateTimeValues(gGetValue(si), dt, tm);
	return si ? self : NULL;
}

imeth	gFldSetString(char *fld, char *str)
{
	char	*vttbl;
	object	si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetString: No field '%s'", fld);
	if (vttbl = pGetVarTextTable(self, si, fld))
		pFldSetVarText(self, si, fld, str, vttbl);
	else {
		int	maxlen = gSize(si);
		
		if (strlen(str) > maxlen) {
			char	*buf = (char *) malloc(maxlen + 1);
			int	i;

			if (!buf)
				gError(Object, "Out of memory.");
			
			for (i = 0 ; i < maxlen ; i++)
				buf[i] = str[i];
			buf[i] = '\0';
			gChangeStrValue(gGetValueToPut(si), buf);

			free(buf);
		} else
			gChangeStrValue(gGetValueToPut(si), str);
	}
	gSetNotNull(si);
	return self;
}

imeth	gFldSetFromFile(char *fld, char *file)
{
	char	*vttbl, *str;
	object	si;
	int	h;
	struct	stat	sb;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetFromFile: No field '%s'", fld);

	h = open(file, O_BINARY | O_RDONLY);
	if (h == -1)
		vError(Object, "gFldSetFromFile: file '%s' not found.", file);
	fstat(h, &sb);
	if (gType(si) == SQL_LONGVARBINARY) {
		str = malloc(sb.st_size);
		if (!str)
			gError(Object, "Out of memory.");
		read(h, str, sb.st_size);
		gFldSetBinary(self, fld, sb.st_size, str);
		free(str);
	} else {
		if (vttbl = pGetVarTextTable(self, si, fld)) {
			str = malloc(sb.st_size+1);
			if (!str)
				gError(Object, "Out of memory.");
			read(h, str, sb.st_size);
			str[sb.st_size] = '\0';
			pFldSetVarText(self, si, fld, str, vttbl);
			free(str);
		} else {
			int	maxlen = gSize(si);
			object	val = gGetValueToPut(si);
		
			if (maxlen > MAX_LONG_VARCHAR_LENGTH)
				maxlen = MAX_LONG_VARCHAR_LENGTH;
			if (maxlen < sb.st_size)
				vError(Object, "gFldSetFromFile:  File '%s' exceeds max length of %ld bytes (is %ld bytes).", file, maxlen, (long) sb.st_size);
			str = gStringValue(val);
			read(h, str, sb.st_size);
			str[sb.st_size] = '\0';
			gUpdateLength(val);
		}
	}
	close(h);
	gSetNotNull(si);
	return self;
}

imeth	gFldSetDateTime(char *fld, long dt, long tm)
{
	object	si;

	LAZY_LOAD;
	if (iErrorState)
		return self;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");

	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetDateTime: No field '%s'", fld);
	gChangeDateTimeValues(gGetValueToPut(si), dt, tm);
	gUpdate(si);	//  for dates and times
	gSetNotNull(si);
	return self;
}

imeth	gFldSetChar(char *fld, int val)
{
	object	obj, si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetChar: No field '%s'", fld);
	obj = gGetValueToPut(si);
	if (gIsKindOf(obj, String)) {
		char	buf[2];
		buf[0] = (char) val;
		buf[1] = '\0';
		gChangeStrValue(obj, buf);
	} else
		gChangeCharValue(obj, val);
	gSetNotNull(si);
	return self;
}

imeth	gFldSetShort(char *fld, int val)
{
	object	si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetShort: No field '%s'", fld);
	gChangeShortValue(gGetValueToPut(si), val);
	gSetNotNull(si);
	return self;
}

imeth	gFldSetUnsignedShort(char *fld, unsigned val)
{
	object	si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetUnsignedShort: No field '%s'", fld);
	gChangeUShortValue(gGetValueToPut(si), val);
	gSetNotNull(si);
	return self;
}

imeth	gFldSetLong(char *fld, long val)
{
	object	si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetLong: No field '%s'", fld);
	gChangeLongValue(gGetValueToPut(si), val);
	gUpdate(si);	//  for dates and times
	gSetNotNull(si);
	return self;
}

imeth	gFldSetDouble(char *fld, double val)
{
	object	si;

	if (iErrorState)
		return self;
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetDouble: No field '%s'", fld);
	if (_isnan(val))
		vError(Object, "gFldSetDouble: Field '%s' passed NAN", fld);
	gChangeDoubleValue(gGetValueToPut(si), val);
	gSetNotNull(si);
	return self;
}

imeth	gFldSetNull(char *fld)
{
	object	si;

	if (iErrorState)
		return self;
//	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetNull: No field '%s'", fld);
	gSetNull(si);
	return self;
}

imeth	gFldSetNotNull(char *fld)
{
	object	si;

	if (iErrorState)
		return self;
//	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetNotNull: No field '%s'", fld);
	gSetNotNull(si);
	return self;
}

private	imeth	char	*make_select_field(char *bfld, int *fn)
{
	char	*buf, *p;
	object	seq, si, pk, kseq, fld;
	int	add_comma = 0, n=0;
		
	pk = gGetPrimaryKey(iDatabase, iTable);
	if (!pk  ||  !gSize(pk))
		if (iIgnoreAllErrors)
			return NULL;
		else
			vError(self, "Can't get binary field %s when no primary key declared", iTable);
	*fn = 1;
	strcpy(p=buf=query_buffer(iRecordLength), "SELECT ");
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		if (add_comma) {
			strcpy(p, ", ");
			GO_END;
		} else
			add_comma = 1;
		strcpy(p, gStringValue(fld));
		GO_END;
		(*fn)++;
	}
        sprintf(p, ", %s FROM %s WHERE ", bfld, iTable);
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;

		if (add_comma) {
			strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		strcpy(p, sf=gStringValue(fld));
		strcat(p, " = ");
		GO_END;
		gFormatOrgField(gFindValueStr(iCols, sf), p);
		GO_END;
	}
	return buf;
}
					     
private	imeth	char	*make_update_field(char *bfld)
{
	char	*buf, *p;
	object	seq, si, pk, kseq, fld;
	int	add_comma = 0, n=0;
		
	pk = gGetPrimaryKey(iDatabase, iTable);
	if (!pk  ||  !gSize(pk))
		if (iIgnoreAllErrors)
			return NULL;
		else
			vError(self, "Can't get binary field %s when no primary key declared", iTable);
	sprintf(p=buf=query_buffer(iRecordLength), "UPDATE %s SET %s=? WHERE ", iTable, bfld);
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;

		if (add_comma) {
			strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		strcpy(p, sf=gStringValue(fld));
		strcat(p, " = ");
		GO_END;
		gFormatOrgField(gFindValueStr(iCols, sf), p);
		GO_END;
	}
	return buf;
}

#define	ERR_RTN	 if (iIgnoreAllErrors) {			\
		     iErrorState = 1;				\
		     gDispose(stmt);				\
		     gLeaveCriticalSection(iDatabase);		\
                     return -1;          			\
	         } else 					\
		     vError(stmt, fun)


imeth	int	gFldSetBinary(char *fld, long size, char *val)
{
	object	si, stmt;
	char	*cmd;
	int	r;
	HSTMT	h;
	static	char	fun[] = "gFldSetBinary";
	char * t;
	
	if (iErrorState)
		return iErrorState;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	if (iReadOnly)
		return 0;
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetBinary: No field '%s'", fld);
	stmt = gNewStatement(iDatabase);
	gEnterCriticalSection(iDatabase);
	cmd = make_update_field(self, fld);
	t=fix_statement(cmd, iDBMS);
	r = SQLPrepare(h=gHSTMT(stmt), t, SQL_NTS);
	free(t);
	free(cmd);
	if (!r) {
		SQLLEN	pcbVal = SQL_LEN_DATA_AT_EXEC(size);
		PTR	pToken;
		
		r = SQLBindParameter(h, (UWORD) 1, SQL_PARAM_INPUT, SQL_C_BINARY, SQL_LONGVARBINARY, 0, 0, (PTR) 1, 0, &pcbVal);
		if (r)
			ERR_RTN;
		r = SQLExecute(h);
		if (r != SQL_NEED_DATA)
			ERR_RTN;
		r = SQLParamData(h, &pToken);
		if (r != SQL_NEED_DATA)
			ERR_RTN;
		r = SQLPutData(h, val, size);
		if (r)
			ERR_RTN;
		r = SQLParamData(h, &pToken);
		if (r)
			ERR_RTN;
	}
	gDispose(stmt);
	gLeaveCriticalSection(iDatabase);
	gSetNotNull(si);
	return r;
}

imeth	int	gFldGetBinary(char *fld, long *size, char **val)
{
	object	si, stmt;
	char	*cmd, c;
	int	r, fn;
	HSTMT	h;
	SQLLEN	pcbVal;
	static	char	fun[] = "gFldGetBinary";
	char * t;

	*val = NULL;
	*size = 0;
	if (iErrorState)
		return iErrorState;
	if (iTType != TYPE_SELECT)
		gError(Object, "Get DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetBinary: No field '%s'", fld);
	stmt = gNewStatement(iDatabase);
	gEnterCriticalSection(iDatabase);
	cmd = make_select_field(self, fld, &fn);
	t=fix_statement(cmd, iDBMS);
	r = SQLExecDirect(h=gHSTMT(stmt), t, SQL_NTS);
	free(t);
	free(cmd);
	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		r = SQLFetch(h);
		if (r  &&  r != SQL_NO_DATA_FOUND  &&  pTrapError(self, iErrors, r))
			ERR_RTN;

		r = SQLGetData(h, (UWORD) fn, SQL_C_BINARY, &c, 1, &pcbVal);
		if (r == SQL_SUCCESS_WITH_INFO) {    //  have data bigger than 1 byte
			*size = pcbVal;
			*val = malloc(*size+1);
			**val = c;
			r = SQLGetData(h, (UWORD) fn, SQL_C_BINARY, *val+1, *size-1, &pcbVal);
			(*val)[*size] = '\0';
			if (r)
				ERR_RTN;
		} else if (!r  &&  pcbVal == 1) {   //  only 1 byte long and got it
			*size = pcbVal;
			*val = malloc(*size+1);
			**val = c;
			(*val)[*size] = '\0';
		} else if (!r  &&  pcbVal <= 0) {  //  no data
		} else
			ERR_RTN;
	}  else
		ERR_RTN;
	gDispose(stmt);
	gLeaveCriticalSection(iDatabase);
	return r;
}
	    
private	imeth	void	check_cache()
{
	object	seq, si, trc = gGetTable(CacheResult, iTable), cc;

	if (trc)
		if (cc = gFindValueStr(trc, "*"))
			gInvalidateResultCache(CacheResult, trc, cc);
		else for (seq=gSequence(iColList) ; si = gNext(seq) ; )
			if (cc = gFindValueStr(trc, gName(si)))
				gInvalidateResultCache(CacheResult, trc, cc);
}

private	imeth	char	*make_delete()
{
	char	*buf;
	char	*p;
	object	pk, kseq, fld;
	int	add_comma = 0, n=0;
		
	pk = gGetPrimaryKey(iDatabase, iTable);
	if (!pk  ||  !gSize(pk))
		if (iIgnoreAllErrors)
			return NULL;
		else
			vError(self, "Can't update %s when no primary key declared", iTable);

	sprintf(p=buf=query_buffer(iRecordLength), "DELETE FROM %s WHERE ", iTable);
	GO_END;
	for (kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;

		if (add_comma) {
			strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		strcpy(p, sf=gStringValue(fld));
		strcat(p, " = ");
		GO_END;
		gFormatOrgField(gFindValueStr(iCols, sf), p);
		GO_END;
	}
	return buf;
}

private	imeth	void	audit_delete()
{
	char	*fn, *buf=NULL, *buf2=NULL, *p;
	object	seq, si, stmt=NULL, kseq, fld;
	object	auditList = iFH_Tables ? gFindValueStr(iFH_Tables, iTable) : NULL;
	int		usingSpecialAuditList = 0;
		
	if (!auditList) {
		auditList = iSpecialFH_Tables ? gFindValueStr(iSpecialFH_Tables, iTable) : NULL;
		usingSpecialAuditList = 1;
	}

	if (!auditList)
		return;
	for (seq=gSequence(iColList) ; si = gNext(seq) ; ) 
		if (gHasData(si))  {
			fn = gName(si);
			if (gFindStr(auditList, fn)) {
				if (!buf) {
					buf = query_buffer(iRecordLength);
					buf2 = query_buffer(iRecordLength);
					stmt = gNewStatement(iDatabase);
					
					for (p=buf, kseq=gSequence(gGetPrimaryKey(iDatabase, iTable)) ; fld = gNext(kseq) ; ) {
						gFormatFixedLengthField(gFindValueStr(iCols, gStringValue(fld)), p);
						for (; *p ; p++);
					}

				}
				if (!usingSpecialAuditList) {
					object	now = gNow(iDatabase);
					gInsert(stmt, gStringValue(iFH_Table));
					gFldSetString(stmt, "TableName", iTable);
					gFldSetChar(stmt, "ChangeType", 'D');
					gFldSetString(stmt, "KeyData", buf);
					gChangeValue(gFldGetValue(stmt, "ChangeTime"), now);
					gDispose(now);
					gUpdate(gGetField(stmt, "ChangeTime"));
					gFldSetLong(stmt, "UserID", iUserID);
					gFldSetString(stmt, "FieldName", gName(si));
					gFldSetString(stmt, "FieldValue", gFormatFixedLengthField(si, buf2));
					gAddRecord(stmt);
				}
				else {
					if (iSpecialAuditHandler)
						iSpecialAuditHandler(iTable, 'D', buf, iUserID, gName(si), gFormatFixedLengthField(si, buf2), self);
				}
			}
		}
	if (stmt)
		gDispose(stmt);
	if (buf)
		free(buf);
	if (buf2)
		free(buf2);
}

imeth	int	gDeleteRecord()
{
	int	r;

	if (iReadOnly)
		return 0;
	if (iTType != TYPE_SELECT)
		gError(Object, "gDeleteRecord without Select");
	check_cache(self);
	if (1  ||  iDBMS == DBMS_SYBASE  ||  iDBMS == DBMS_MYSQL  ||  iDBMS == DBMS_MSSQL) {
		char	*buf = make_delete(self);
		object	stmt;
		int	ds, ro, rae;
		if (!buf)
			return 0;
		stmt = gStatement(iDatabase);
		rae = gReturnAllErrors(stmt, 1);
		ds = gEnableTP(stmt, 0);
		ro = gIsReadOnly(stmt);
		if (ro)
			gEnableWrites(stmt);
		r = gExecute(stmt, buf);
		if (r) {
			strcpy(iInternalState, gGetErrorState(stmt));
			iInternalNativeCode = gGetNativeErrorCode(stmt);
		}
		free(buf);
		gEnableTP(stmt, ds);
		if (ro)
			gDisableWrites(stmt);
		gReturnAllErrors(stmt, rae);
	} else {
		char	*p;
		BEGIN_LOG();
		r = SQL_DELETE_RECORD(iStmt, 1);
		END_LOG();
		if (cLFP) {
			char * t;
			p = make_delete(self);
			PRINT_LOG("gDeleteRecord", t=fix_statement(p, iDBMS), p);
			if (p)
				free(p);
			free(t);
		}
	}

	if (r  &&  pTrapError(self, iErrors, r))
		if (iIgnoreAllErrors)
			return 0;
		else
			vError(self, "gDeleteRecord");
	gRemoveVarText(self, gGetVarTextFields(iDatabase, iTable));
	if (iDataSync  &&  iTP)
		gTPDelete(iTP, self);
	if (!r  &&  iFH_Tables)
		audit_delete(self);
	return r;
/*
	char	cursor[30], cmd[128];
	if (iTType != TYPE_SELECT)
		return -1;
	SQL_POSITION_TO(iStmt, 1);
	SQLGetCursorName(iStmt, cursor, (SWORD) sizeof cursor, NULL);
	sprintf(cmd, "delete where current of %s", cursor);
	return SQLExecDirect(iStmt, cmd, SQL_NTS);
*/
}

imeth	int	gFieldsChanged()
{
	object	seq, si, old, newv, trc, cc, cc2;
	int	res = 1;

	if (iTType != TYPE_SELECT)
		return 0;
	trc = gGetTable(CacheResult, iTable);
	cc2 = trc ? gFindValueStr(trc, "*") : NULL;
	for (seq=gSequence(iColList) ; si = gNext(seq) ; ) {
		old = gOriginalValue(si);
		newv = gGetValueToPut(si);
		if (old  &&  gCompare(old, newv)) {
			res = 0;
			if (!trc) {
				gDispose(seq);
				break;
			}
	
			if (trc  &&  cc2) {
				gInvalidateResultCache(CacheResult, trc, cc2);
				trc = NULL;
				gDispose(seq);
				break;
			}
	
			if (cc = gFindValueStr(trc, gName(si)))
				gInvalidateResultCache(CacheResult, trc, cc);
		}
		if (iVarTextChanged && gVarTextChanged(si)) {
			res = 0;
			if (!trc) {
				gDispose(seq);
				break;
			}
	
			if (trc  &&  cc2) {
				gInvalidateResultCache(CacheResult, trc, cc2);
				trc = NULL;
				gDispose(seq);
				break;
			}
	
			if (cc = gFindValueStr(trc, gName(si)))
				gInvalidateResultCache(CacheResult, trc, cc);
		}
	}
	return !res;
}

private	imeth	pRestoreOriginalValues(object self)
{
	object	si, seq;

	for (seq = gSequence(iCols) ; si = gNext(seq) ; )
		gRestoreOriginalValue(gValue(si));

	iVarTextChanged = 0;
	
	return self;
}

private	imeth	char	*make_update(int exit)
{
	char	*buf;
	char	*p;
	object	seq, si, pk, kseq, fld, old, newv;
	int	add_comma = 0, n=0, nfields=0;
		
	pk = gGetPrimaryKey(iDatabase, iTable);
	if (!pk  ||  !gSize(pk))
		if (iIgnoreAllErrors)
			return NULL;
		else
			vError(self, "Can't update %s when no primary key declared", iTable);

	sprintf(p=buf=query_buffer(iRecordLength), "UPDATE %s SET ", iTable);
	GO_END;
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (gType(si) != SQL_LONGVARBINARY) {
			if (exit) {
				old = gOriginalValue(si);
				newv = gGetValueToPut(si);
			} else
				newv = si;  //  any value so following test works
			if (newv  &&  (!exit  ||  !old  ||  gCompare(old, newv))) {
				if (add_comma) {
					*p++ = ',';
					*p++ = ' ';
					n += 2;
				} else
					add_comma = 1;
				strcpy(p, gName(si));
				strcat(p, " = ");
				GO_END;
				gFormatField(si, p);
				GO_END;
				nfields++;
			}
		}
	if (!nfields  &&  exit) {
		free(buf);
		return NULL;
	}
	strcpy(p, " WHERE ");
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;

		if (add_comma) {
			strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		strcpy(p, sf=gStringValue(fld));
		strcat(p, " = ");
		GO_END;
		gFormatOrgField(gFindValueStr(iCols, sf), p);
		GO_END;
	}
	return buf;
}

private	imeth	void	audit_update()
{
	char	*fn, *buf=NULL, *buf2=NULL, *p;
	object	seq, si, stmt=NULL, pk, kseq, fld, old, newv;
	object	auditList = iFH_Tables ? gFindValueStr(iFH_Tables, iTable) : NULL;
	int		exit = 1;
	int		usingSpecialAuditList = 0;
	
	if (!auditList) {
		auditList = iSpecialFH_Tables ? gFindValueStr(iSpecialFH_Tables, iTable) : NULL;
		usingSpecialAuditList = 1;
	}

	if (!auditList)
		return;
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (gType(si) != SQL_LONGVARBINARY) {
			if (exit) {
				old = gOriginalValue(si);
				newv = gGetValueToPut(si);
			} else
				newv = si;  //  any value so following test works
			if (newv  &&  (!exit  ||  !old  ||  gCompare(old, newv))) {
				char	*fn;

				fn = gName(si);
				if (gFindStr(auditList, fn)) {
					if (!buf) {
						buf = query_buffer(iRecordLength);
						buf2 = query_buffer(iRecordLength);
						stmt = gNewStatement(iDatabase);
						
						for (p=buf, kseq=gSequence(gGetPrimaryKey(iDatabase, iTable)) ; fld = gNext(kseq) ; ) {
							gFormatFixedLengthField(gFindValueStr(iCols, gStringValue(fld)), p);
							for (; *p ; p++);
						}
					}
					if (!usingSpecialAuditList) {
						object	now = gNow(iDatabase);
						gInsert(stmt, gStringValue(iFH_Table));
						gFldSetString(stmt, "TableName", iTable);
						gFldSetChar(stmt, "ChangeType", 'C');
						gFldSetString(stmt, "KeyData", buf);
						gChangeValue(gFldGetValue(stmt, "ChangeTime"), now);
						gDispose(now);
						gUpdate(gGetField(stmt, "ChangeTime"));
						gFldSetLong(stmt, "UserID", iUserID);
						gFldSetString(stmt, "FieldName", gName(si));
						gFldSetString(stmt, "FieldValue", gFormatFixedLengthField(si, buf2));
						gAddRecord(stmt);
					}
					else {
						if (iSpecialAuditHandler)
							iSpecialAuditHandler(iTable, 'C', buf, iUserID, gName(si), gFormatFixedLengthField(si, buf2), self);
					}
				}
			}
		}
	if (stmt)
		gDispose(stmt);
	if (buf)
		free(buf);
	if (buf2)
		free(buf2);
}

imeth	int	gUpdateRecord()
{
	int	r;

	if (iLazyLoad)
		return 0;
	if (iReadOnly) {
		pRestoreOriginalValues(self);
		if (iDialogs) {
			object	seq, dlg;
			
			for (seq = gSequence(iDialogs); dlg = gNext(seq); )
				if (IsObj(dlg))
					gUpdate(dlg);
		}
		return 0;
	}
	if (iTType != TYPE_SELECT)
		gError(Object, "gUpdateRecord without Select");
//	if (iDataSync  &&  iTP)
		if (!gFieldsChanged(self))
			return 0;
	if (iDBMS == DBMS_ACCESS)
		updateFields(iv);
	if (iVarTextChanged)
		pUpdateVarText(self);

	if (1  ||  iDBMS == DBMS_SYBASE  ||  iDBMS == DBMS_MYSQL  ||  iDBMS == DBMS_MSSQL  ||
	                                     iDBMS == DBMS_WATCOM  &&  gDBMS_version(iDatabase) > 6.9) {
		char	*buf = make_update(self, 1);
		object	stmt = gStatement(iDatabase);
		int	ds = gEnableTP(stmt, 0);
		int	ro = gIsReadOnly(stmt);
		int	rae = gReturnAllErrors(stmt, 1);
		if (ro)
			gEnableWrites(stmt);
		if (buf) {
			r = gExecute(stmt, buf);
			if (r) {
				strcpy(iInternalState, gGetErrorState(stmt));
				iInternalNativeCode = gGetNativeErrorCode(stmt);
			}
			free(buf);
		} else
			r = 0;
		gEnableTP(stmt, ds);
		if (ro)
			gDisableWrites(stmt);
		gReturnAllErrors(stmt, rae);
	} else {
		char	*p;
		BEGIN_LOG();
		r = SQL_UPDATE_RECORD(iStmt, 1);
		END_LOG();
		if (cLFP) {
			char * t;
			p = make_update(self, 0);
			PRINT_LOG("gUpdateRecord", t=fix_statement(p, iDBMS), p);
			if (p)
				free(p);
			free(t);
		}
	}

	if (r  &&  pTrapError(self, iErrors, r)) {
		if (iVarTextChanged)
			pUndoVarText(self);
		if (iIgnoreAllErrors)
			return 0;
		else
			vError(self, "gUpdateRecord");
	}

	if (iDataSync  &&  iTP)
		gTPChange(iTP, self);
	if (!r) {
		cursor_write(self, iPos - 1);
		if (iFH_Tables)
			audit_update(self);
		updateOrgValues(self);
	}
	return r;
}

#if 0

#define istart(x)	(isalpha(x))
#define irest(x)	(isalnum(x)  ||  (x) == '_')

static	char	*parse_insert(char *cmd, char *field)
{
	if (!strnicmp("insert", cmd, 6))
		while (*cmd  &&  *cmd != '(')
			cmd++;
	while (*cmd  &&  !istart(*cmd)  &&  *cmd != ')')
		cmd++;
	if (!*cmd  ||  *cmd == ')')
		return NULL;
	while (irest(*cmd))
		*field++ = *cmd++;
	*field = '\0';
	return cmd;
}

//imeth	int	gAssociateCol(char *cname, void *field, SWORD type, int sz)
{
	object	f;
	RETCODE	r;
	SDWORD	len;	

	if (iTType == TYPE_NONE  ||  !iCols)
		return -1;
	f = gFindValueStr(iCols, cname);
	if (!f)
		return -2;
	if (iTType == TYPE_SELECT)
		return SQLBindCol(iStmt, (UWORD) gShortValue(f), type, field,
				  (SDWORD) sz, NULL);
	else if (iTType == TYPE_INSERT)  {
		UWORD	i = gShortValue(f);
		SWORD	stype;
		UDWORD	prec;
		SWORD	scale;

		r = SQLDescribeParam(iStmt, i, &stype, &prec, &scale, NULL);
		if (r)
			return r;
		if (type == SQL_C_CHAR)
			len = SQL_NTS;
		else
			len = 0;
#if ODBCVER < 0x0200
		return SQLSetParam(iStmt, i, type, stype, prec, scale, field,
				   &len);
#else
		return SQLBindParameter(iStmt, i, SQL_PARAM_INPUT, type, stype,
					prec, scale, field, 0, &len);
#endif
	}
}

#endif

imeth	int	gEndSQL()
{             
	RETCODE	r;
	
	if (iDialogs) {
		object	seq, dlg;

		for (seq = gSequence(iDialogs); dlg = gNext(seq); )
			pUnattachDialog(self, dlg, 0);
		iDialogs = gDispose(iDialogs);
	}
	if (iCursorFile)
		iCursorFile = gDispose(iCursorFile);
	iRecordsRead = 0;
	iRecordLength = 0;
	iDoneFirst = 0;
	iPos = 0;
	iEof = -1;
	iLazyLoad = 0;
	iOracleSQLStatistics = 0;
	if (iLazyStatement)
		iLazyStatement = gDispose(iLazyStatement);
	if (iTType == TYPE_INSERT) {
		r = SQLTransact(gHENV(iDatabase), gHDBC(iDatabase), SQL_COMMIT);
		SQLFreeStmt(iStmt, SQL_RESET_PARAMS);
	}
	
	if (!iStmtAlreadyClosed) {
		r = SQLFreeStmt(iStmt, SQL_CLOSE);
		if (iTType == TYPE_SELECT)
			SQLFreeStmt(iStmt, SQL_UNBIND);
	} else
		iStmtAlreadyClosed = 0;
	if (iCols)
		iCols = gDispose(iCols);
	if (iColList)
		iColList = gDeepDispose(iColList);
	if (iTableList)
		iTableList = gDeepDispose(iTableList);
	iTType = TYPE_NONE;
	*iTable = '\0';
	iVarTextChanged = 0;
	iErrorState = 0;
	*iInternalState = '\0';
	return r;
}

imeth	int	gNextRecord()
{
	UDWORD	crow;
	int	r=0;

	LAZY_LOAD;
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gNextRecord without Select");	
	*iInternalState = '\0';
	if (iPos >= iRecordsRead) {
		if (iDBMS == DBMS_MSSQL || iOracleSQLStatistics)
			r = SQL_NO_DATA_FOUND;
		else
			do {
				r = SQLFetch(iStmt);
			} while (!r  &&  iRecordTestFunction  &&  !iRecordTestFunction(self));
		if (r  &&  r != SQL_NO_DATA_FOUND  &&  pTrapError(self, iErrors, r))
			if (iIgnoreAllErrors)
				return 1;
			else
				vError(self, "gNextRecord");
		if (!r) {
			cursor_write(self, iPos++);
			iEof = 0;
			updateOrgValues(self);
		} else {
			if (!iEof)
				iPos++;
			iEof = 1;
		}
	} else {
		cursor_read(self, iPos++);
		iEof = 0;
		updateOrgValues(self);
	}
	return r;
}

imeth	int	gPrevRecord()
{
	UDWORD	crow;
	int	r=0;
	
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gPrevRecord without Select");
	if (iPos <= 1)  {
		iPos = 0;
		return SQL_NO_DATA_FOUND;
	}
	cursor_read(self, iPos - 2);
	iPos--;
	updateOrgValues(self);
	iEof = 0;
	return r;
}

imeth	int	gFirstRecord()
{
	LAZY_LOAD;
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gFirstRecord without Select");
	if (iRecordsRead) {
		cursor_read(self, 0);
		iPos = 1;
		updateOrgValues(self);
		return 0;
	} else
		return gNextRecord(self);
}

imeth	int	gLastRecord()
{
	UDWORD	crow;
	int	r=0;
	
	LAZY_LOAD;
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		return -1;
	gError(Object, "gLastRecord is not implemented.");
	return r;
}

imeth	int	gRereadRecord()
{
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gRereadRecord without Select");
	if (!iPos)
		gError(Object, "gReadRecord when no record originally read.");
	cursor_read(self, iPos-1);
	updateOrgValues(self);
	return 0;
}

imeth	int	gSetAbsPos(long pos)   //  pos is one origin
{
	UDWORD	crow;
	int	r=0;
	
	LAZY_LOAD;
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gSetAbsPos without Select");
	if (pos <= iRecordsRead) {
		cursor_read(self, (int) pos - 1);
		iPos = pos;
	} else {
		iPos = iRecordsRead;
		while (pos > iPos  &&  !r) {
			if (iDBMS == DBMS_MSSQL)
				r = SQL_NO_DATA_FOUND;
			else
				do {
					r = SQLFetch(iStmt);
				} while (!r  &&  iRecordTestFunction  &&  !iRecordTestFunction(self));
			if (r  &&  r != SQL_NO_DATA_FOUND  &&  pTrapError(self, iErrors, r))
				if (iIgnoreAllErrors)
					return 1;
				else
					vError(self, "gSetAbsPos");
			if (!r) {
				cursor_write(self, iPos++);
				iEof = 0;
			} else {
				if (!iEof)
					iPos++;
				iEof = 1;
			}
		}
	}
	if (!r)
		updateOrgValues(self);
	return r;
}

imeth	int	gSetRelPos(long pos)
{
	return gSetAbsPos(self, iPos + pos);
}

imeth	gDatabase()
{
	return iDatabase;
}

#define	QUOTE			'\''

#define	NORMAL_STATE		1
#define	COMMENT_STATE		2
#define QUOTE_STATE		3
#define MULTICOMMENT_STATE	4

#define INSERT(x)						\
	{							\
		if (len == mx)  {				\
			mx += 100;				\
			buf = Tnrealloc(char, mx, buf);		\
		}						\
		buf[len++] = x;					\
	}

#define	UN_INSERT						\
	{							\
		if (len)					\
			len--;					\
	}

#define RESET	ps = len = 0

#define GETC	if ((c = getc(fp)) == EOF) goto end; if (c == '\n') linenum++
#define UNGETC	if (c == '\n') linenum--; ungetc(c, fp)

private	imeth	pExecuteWithErrorObj(object self, char *cmd, long linenum)
{
	object	rval = NULL;
	int	r = gExecute(self, cmd);

	if (r) {
		char	buf[25];
		char	*state = gGetErrorState(self);
		char	*dberr = c_emsg(Statement);
		object	sobj;

		if (!strnicmp("insert ", cmd, 7) && atoi(state) == 23000)
			r = 1;
		
		if (linenum > 1)
			sprintf(buf, "Line %ld:\n", linenum - 1);
		sobj = gNewWithStr(String, buf);
		gAppend(sobj, (object) dberr);
		gAppend(sobj, (object) "\n\nSQL Command: ");
		gAppend(sobj, (object) cmd);
		rval = gNewWithIntObj(IntegerAssociation, r, sobj);
	}
	return rval;
}

imeth	int	gExecuteWithError(object self, char *cmd, char *errbuf, int sz, long linenum)
{
	object	rval = pExecuteWithErrorObj(self, cmd, linenum);
	int	r = 0;

	if (rval) {
		object	sobj = gValue(rval);

		r = gIntKey(rval);
		if (gSize(sobj) >= sz)
			gTake(sobj, sz - 1);
		strcpy(errbuf, gStringValue(sobj));
		gDeepDispose(rval);
	}
	return r;
}

// errormode: 0 = error out of system
//            1 = stop processing and return single error data
//            2 = continue processing and return list of errors
private	imeth	object	pExecuteFile(object self, char *file, int errormode)
{
	object	rval = NULL;
	int	pig;
	FILE	*fp;
	char	*buf;
	int	len=0, mx=100, c, state = NORMAL_STATE, res = 0;
	int	ps = 0;	      /*  previous space  */
	int	nesting = 0;  /*  level of comments  */
	char	file2[80];
	object	tmp;
	char	gobuf[3];
	int	gbi = 0;  /*  index into gobuf  */
	long	linenum = 1;

	strcpy(file2, file);
	fp = fopen(file2, "r");
	if (!fp) {
		sprintf(file2, "%s.sql", file);
		fp = fopen(file2, "r");

		if (!fp) {
			switch (errormode) {
			case 2:
				rval = gNew(LinkObject);
				gAddLast(rval, gNewWithIntObj(IntegerAssociation, -1,
							      vSprintf(String, "Can't open \"%s\".", file)));
				break;
			case 1:
				rval = gNewWithIntObj(IntegerAssociation, -1,
						      vSprintf(String, "Can't open \"%s\".", file));
				break;
			default:
				rval = gNewWithLong(LongInteger, -1L);
				break;
			return rval;
			}
		}
	}
	if (errormode)
		pig = gReturnAllErrors(self, 1);
	buf = Tnalloc(char, mx);
	while (!res)  {
		GETC;
		switch (state)  {
		case NORMAL_STATE:
			if (c == ';')  {
//				INSERT(c);
				INSERT('\0');
				switch (errormode) {
				case 2:
					tmp = pExecuteWithErrorObj(self, buf, linenum);
					if (tmp) {
						if (!rval)
							rval = gNew(LinkObject);
						gAddLast(rval, tmp);
					}
					break;
				case 1:
					rval = pExecuteWithErrorObj(self, buf, linenum);
					if (rval)
						res = gIntKey(rval);
					break;
				default:
					res = gExecute(self, buf);
					break;
				}
				RESET;
				gbi = 0;
			} else if (c == QUOTE)  {
				ps = 0;
				INSERT(c);
				state = QUOTE_STATE;
				gbi = 0;
			} else if (c == '-')  {
				GETC;
				gbi = 0;
				if (c == '-')
					state = COMMENT_STATE;
				else  {
					ps = 0;
					UNGETC;
					INSERT('-');
				}
			} else if (c == '{')  {
				gbi = 0;
				nesting = 1;
				state = MULTICOMMENT_STATE;
			} else if (c == ' '  ||  c == '\t'  ||  c == '\r'  ||  c == '\n')  {
				if (gbi == 2  &&  tolower(gobuf[0]) == 'g'  &&  tolower(gobuf[1]) == 'o') {
					UN_INSERT;
					UN_INSERT;
					INSERT('\0');
					switch (errormode) {
					case 2:
						tmp = pExecuteWithErrorObj(self, buf, linenum);
						if (tmp) {
							if (!rval)
								rval = gNew(LinkObject);
							gAddLast(rval, tmp);
						}
						break;
					case 1:
						rval = pExecuteWithErrorObj(self, buf, linenum);
						if (rval)
							res = gIntKey(rval);
						break;
					default:
						res = gExecute(self, buf);
						break;
					}
					RESET;
				} else if (!ps  &&  len)  {
					ps = 1;
					INSERT(' ');
				}
				gbi = 0;
#if 0
			} else  if (isalpha(c)) {
				char	word[80];
				int	i = 0;
				
				do {
					word[i++] = c;
					GETC;
				} while (isalnum(c));
				UNGETC;
				word[i] = '\0';
				if (iDBMS == DBMS_SYBASE) {
					if (!stricmp(word, "date"))
						strcpy(word, "datetime");
				}
				ps = 0;
				for (i=0 ; word[i] ; i++)
					INSERT(word[i]);
#endif
			} else {
				ps = 0;
				if (!isalnum(c))
					gbi = 0;
				else if (gbi < 3)
					gobuf[gbi++] = c;
				INSERT(c);
			}
			break;
		case COMMENT_STATE:
			if (c == '\n')
				state = NORMAL_STATE;
			break;
		case QUOTE_STATE:
			INSERT(c);
			if (c == QUOTE)  {
				GETC;
				if (c != QUOTE)  {
					UNGETC;
					state = NORMAL_STATE;
				} else
					INSERT(c);
			}
			break;
		case MULTICOMMENT_STATE:
			if (c == '}')  {
				if (!--nesting)
					state = NORMAL_STATE;
			} else if (c == '{')
				nesting++;
			break;
		}
	}
 end:
	if (!res  &&  gbi == 2  &&  tolower(gobuf[0]) == 'g'  &&  tolower(gobuf[1]) == 'o') {
		UN_INSERT;
		UN_INSERT;
		INSERT('\0');
		switch (errormode) {
		case 2:
			tmp = pExecuteWithErrorObj(self, buf, linenum);
			if (tmp) {
				if (!rval)
					rval = gNew(LinkObject);
				gAddLast(rval, tmp);
			}
			break;
		case 1:
			rval = pExecuteWithErrorObj(self, buf, linenum);
			if (rval)
				res = gIntKey(rval);
			break;
		default:
			res = gExecute(self, buf);
			break;
		}
	}
	free((void*)buf);
	fclose(fp);
	if (errormode)
		gReturnAllErrors(self, pig);
	gEndSQL(self);
	return rval;
}

imeth	int	gExecuteFile(char *file)
{
	object	tmp = pExecuteFile(self, file, 0);
	int	rval = !!tmp;

	if (tmp)
		gDeepDispose(tmp);
	return rval;
}

imeth	int	gExecuteFileWithError(char *file, char *errbuf, int sz)
{
	object	tmp = pExecuteFile(self, file, 1);
	int	rval = tmp ? gIntKey(tmp) : 0;
	object	sobj = tmp ? gValue(tmp) : NULL;

	if (errbuf) {
		*errbuf = '\0';
		if (sobj) {
			if (gSize(sobj) >= sz)
				gTake(sobj, sz - 1);
			strcpy(errbuf, gStringValue(sobj));
		}
	}
	if (tmp)
		gDeepDispose(tmp);
	
	return rval;
}

imeth	gExecuteFileReturnAllErrors(char *file)
{
	return pExecuteFile(self, file, 2);
}

imeth	gAttachDialog(dlg)
{
	object	ctls;  // A StringDictionary of controls
	object	seq, i, ctl, si;
	char	*cname;

	LAZY_LOAD;
	ctls = gControls(dlg);
	for (seq = gSequence(ctls) ; i = gNext(seq) ; )  {
		ctl = gValue(i);
		if (!gAutoAttach(ctl, -1))
			continue;
		cname = gName(ctl);
		if (!cname  ||  !*cname)
			continue;
		si = gFindValueStr(iCols, lcname(cname));
		if (si) {
			static	object	tc;

			if (!tc)
				tc = gFindClass(Class, "TextControl");
			gAttach(ctl, gGetValue(si));
			if (ClassOf(ctl) == tc) {
				if (!gGetMinLength(ctl)  &&  gAllowNulls(si) == SQL_NO_NULLS)
					gTextRange(ctl, 1, -gSize(si));
				else
					gMaxLength(ctl, -gSize(si));
			}
			gSetSI(ctl, si);
		}
	}
	if (!iDialogs)
		iDialogs = gNew(LinkObject);
	gAddLast(iDialogs, dlg);
	return self;
}

imeth	gUnattachDialog(dlg)
{
	return pUnattachDialog(self, dlg, 1);
}

imeth	gAssociateCtl(char *cname, ctl)
{
	object	si;
	static	object	tc;

	LAZY_LOAD;
	if (!tc)
		tc = gFindClass(Class, "TextControl");
	si = gFindValueStr(iCols, lcname(cname));
	if (!si)
		return NULL;
	gAttach(ctl, gGetValue(si));
	if (ClassOf(ctl) == tc) {
		if (!gGetMinLength(ctl)  &&  gAllowNulls(si) == SQL_NO_NULLS)
			gTextRange(ctl, 1, -gSize(si));
		else
			gMaxLength(ctl, -gSize(si));
	}
	gSetSI(ctl, si);
	return self;
}

imeth	int	gDBMS_type()
{
	return iDBMS;
}

imeth	int	gSelectColumns(char *tname, char *oname)
{
	RETCODE	r;
	static	char	err[] = "gSelectColumns::Statement Error";
	char	*to = pGetOwnerName(self, oname);

	gEndSQL(self);
	gEnterCriticalSection(iDatabase);

	switch (iDBMS) {
	case DBMS_MSSQL:
		return pSelectMSSQLColumns(self, tname, oname, 0);
		break;
	}

	iUseCursors = 1;

	r = SQLColumns(iStmt, NULL, 0, to, (SWORD) (to ? SQL_NTS : 0), tname, SQL_NTS, NULL, 0);
	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	r = !r || r == SQL_SUCCESS_WITH_INFO ? bindCols(self, err): -1;
	gLeaveCriticalSection(iDatabase);
	return r;
}

imeth	int	gSelectColumnsByName(char *tname, char *oname)
{
	RETCODE	r;
	static	char	err[] = "gSelectColumns::Statement Error";
	char	*to = pGetOwnerName(self, oname);

	gEndSQL(self);
	gEnterCriticalSection(iDatabase);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMColumns(self, tname, oname, 1);
		break;
	case DBMS_MSSQL:
		return pSelectMSSQLColumns(self, tname, oname, 1);
		break;
	}

	r = SQLColumns(iStmt, NULL, 0, to, (SWORD) (to ? SQL_NTS : 0), tname, SQL_NTS, NULL, 0);
	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	r = !r || r == SQL_SUCCESS_WITH_INFO ? bindCols(self, err): -1;
	gLeaveCriticalSection(iDatabase);
	return r;
}


/* We have a MSSQL specific select for this here because Microsoft will auto-generate
   tables (ie: dtproperties) that will show up as a user table, even though we didn't
   create it.  The select in pSelectMSSQLTables only brings back tables we created.
*/

private	imeth	int	pSelectMSSQLTables(object self, char *oname)
{
	char *cmd = "select TABLE_QUALIFIER = convert(sysname,db_name()), "
		"TABLE_CAT = convert(sysname,db_name()), "
		"TABLE_OWNER = convert(sysname,user_name(o.uid)), "
		"TABLE_SCHEM = convert(sysname,user_name(o.uid)), "
		"TABLE_NAME = convert(sysname,o.name), "
		"TABLE_TYPE = case when objectproperty(id, 'IsUserTable') = 1 "
		"and objectproperty(id, 'IsMSShipped') = 0 then 'TABLE' "
		"when objectproperty(id, 'IsView') = 1 then 'VIEW' "
		"else 'SYSTEM TABLE' END, "
		"REMARKS = convert(varchar(254),null) "
		"from sysobjects o "
		"where objectproperty(id, 'IsView') = 1 "
		"or objectproperty(id, 'IsTable') = 1 %s"
		"order by TABLE_SCHEM, TABLE_QUALIFIER, TABLE_CAT, TABLE_OWNER";
	char	where[256], ecmd[1024];

	if (oname && *oname)
		sprintf(where, "and user_name(o.uid) = '%s' ", oname);
	else
		*where = '\0';
	sprintf(ecmd, cmd, where);

	return SQLExecDirect(iStmt, ecmd, SQL_NTS);
}

imeth	int	gSelectTables(char *oname)
{
	RETCODE	r;
	static	char	err[] = "gSelectTables::Statement Error";
	char	*to = pGetOwnerName(self, oname);

	gEndSQL(self);
	iUseCursors = 1;
	gEnterCriticalSection(iDatabase);
	if (iDBMS == DBMS_MSSQL)
		r =  pSelectMSSQLTables(self, oname);
	else
		r = SQLTables(iStmt, NULL, 0, to, (SWORD) (to ? SQL_NTS : 0), NULL, 0, "TABLE", SQL_NTS);

	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		r = bindCols(self, err);
		if (iDBMS == DBMS_MSSQL) {
			int	r=0;
			while (!r) {
				r = SQLFetch(iStmt);
				if (!r)
					cursor_write(self, iPos++);
			}
			if (r != SQL_NO_DATA_FOUND  &&  !iIgnoreAllErrors)
				vError(self, "gDBSelect cursor load");
			SQLFreeStmt(iStmt, SQL_CLOSE);
			SQLFreeStmt(iStmt, SQL_UNBIND);
			iStmtAlreadyClosed = 1;
			if (r == SQL_NO_DATA_FOUND)
				iPos = 0;
			else if (iIgnoreAllErrors) {
				gLeaveCriticalSection(iDatabase);
				return 0;
			}
		}
		gLeaveCriticalSection(iDatabase);
		return r;
	} else {
		gLeaveCriticalSection(iDatabase);
		return -1;
	}
}

imeth	gColumns()
{
	return iColList;
}

imeth	gTables()
{
	return iTableList;
}

imeth	gColumnDictionary()
{
	return iCols;
}

private	imeth	int	pTrapError(object self, object es, int err)
{
	object	eo;
	int	r;

	if (iReturnAllErrors)
		return 0;
	if (!es)
		return 1;
	
	eo = gNewWithInt(ShortInteger, err);
	r = !gFind(es, eo);
	gDispose(eo);

	return r;
}

imeth	gReturnError(int err)
{
	object	eo, ret;
	
	if (!iErrors)
		iErrors = gNew(Set);
	eo = gNewWithInt(ShortInteger, err);
	ret = gAdd(iErrors, eo);
	if (!ret)
		gDispose(eo);
	return self;
}

static	void	replace(char *str, char *from, char *to)
{
	int	flen = strlen(from);
	int	tlen = strlen(to);
	int	slen = strlen(str) + 1;
	if (flen != tlen) {
		memmove(str, str+flen, slen = slen - flen);
		memmove(str+tlen, str, slen);
	}
	memcpy(str, to, tlen);
}

#define	CH	'~'


static	char	*fix_statement(char *s, int dbms)
{
	int	inquote = 0, len;
	static	int	mlen = 0;
	static	char	*buf;
	char	*b;
	object tmpString;
	int i;
	char tmpBuf[16];
	
	char *t=malloc(strlen(s) * 2);//Don't change s!
	strcpy(t, s);
	s=t;

	b=s;
	while (*s) {
		while (*s  &&  *s != CH)
			if (inquote) {
				if (*s == QUOTE)
					if (s[1] == QUOTE)
						s += 2;
					else {
						s++;
						inquote = 0;
					}
				else
					s++;
			} else if (*s == QUOTE) {
				s++;
				inquote = 1;
			} else if (isalpha(*s)) {
				char	word[80], *start = s;
				int	i = 0;
				while (isalnum(*s) || *s == '_') {
					*s = tolower(*s);//  Sybase server has mixed case only so we'll downcase everything
					word[i++] = *s++;
				}

				word[i] = '\0';
				if (dbms == DBMS_SYBASE) {
					if (!stricmp(word, "date"))
						replace(start, "date", "datetime");
				}
			} else
				++s;
		if (*s == CH) {
			*s++ = QUOTE;
			for ( ; *s  &&  *s != CH ; s++) 
				if (*s == QUOTE) {
					++s;
					memmove(s+1, s, strlen(s)+1);
					*s = QUOTE;
				}
			if (*s == CH)
				*s++ = QUOTE;
		}
	}

	if (dbms == DBMS_ORACLE)
		return TranslateToOracle(b);

	return b;
}

imeth	gCopyCorresponding(from)
{
	ivType	*iv2;
	object	seq, node, si, si2;
	char	*cname;
	object fromCols;

//	ChkArgTyp(from, 2, CLASS);
	LAZY_LOAD;
	if (gDoLazyLoad(from))
		vError(self, "LazySelect Error");
//	iv2 = ivPtr(from);
//	if (!iv2->iCols)
//		return NULL;

	if (!(fromCols=gColumnDictionary(from)))
		return NULL;

	for (seq=gSequence(iCols) ; node=gNext(seq) ; ) {
		si = gValue(node);
		if (gType(si) != SQL_LONGVARBINARY) {
			cname = gStringKey(node);
	//		si2 = gFindValueStr(iv2->iCols, cname);
			si2 = gFindValueStr(fromCols, cname);
			if (si2) {
				char	*vttbl = pGetVarTextTable(self, si, cname);
				char	*vttbl2 = gGetVarTextTable2(from, si2, cname);
			
				if (vttbl  &&  vttbl2)
					pFldSetVarText(self, si, cname, gFldGetVarText(from, si2, cname, vttbl2), vttbl);
				else if (!vttbl  &&  !vttbl2)
					gCopyCorresponding(si, si2);
			}
		}
	}
	return self;
}

ivmeth	int	vDBSelect, vExecuteWithBind, vDBSelectDNC (char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	r = gExecuteWithBind(self, buf);
	free(buf);
	return r;
}

ivmeth	int	vLazyDBSelect, vLazyExecuteWithBind (ifun fun, char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	r = gLazyDBSelect(self, fun, buf);
	free(buf);
	return r;
}

ivmeth	int	vDBSelectOne, vDBSelectOneDNC (char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	r = gDBSelectOne(self, buf);
	free(buf);
	return r;
}

ivmeth	int	vLazyDBSelectOne(ifun fun, char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	r = gLazyDBSelectOne(self, fun, buf);
	free(buf);
	return r;
}

ivmeth	int	vExecute(char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);

	vsprintf(buf, fmt, _rest_);
	r = gExecute(self, buf);
	free(buf);
	return r;
}

imeth	char	*gName()
{
	return iTable;
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

imeth	int	gSelectPrimaryKeyFields(char *table, char *oname)
{
	char	ostr[256];
	gEndSQL(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMPrimaryKeyFields(self, table, oname);
	case DBMS_SYBASE:
	case DBMS_MSSQL:
		if (oname && *oname)
			sprintf(ostr, ", '%s'", oname);
		else
			*ostr = '\0';
		if (vExecuteWithBind(self, "sp_pkeys '%s'%s", table, ostr))
			return 1;
		if (gNextRecord(self))
			return 1;
		gPrevRecord(self);  // so next gNextRecord reads the first record
		return 0;
	case DBMS_MYSQL:
		return pSelectMYSQLIndexes(self, table, oname);
	case DBMS_ORACLE:
		return pSelectORACLEPrimaryKeyFields(self, table, oname);
	}

	return 1;
}

private	imeth	int	pSelectMYSQLIndexes(object self, char *table, char *oname)
{
	char	buf[128];

	sprintf(buf, "show index from %s", table);
	return gDBSelect(self, buf);
}

private imeth	int pSelectORACLEPrimaryKeyFields(object self, char *table, char *oname)
{
	RETCODE	r;
	static	char	err[] = "gSelectPrimaryKeys::Statement Error";
	char	*to = pGetOwnerName(self, oname);

	gEndSQL(self);
	iUseCursors = 1;

	r = SQLPrimaryKeys(iStmt, NULL, 0, to, (SWORD) (to ? SQL_NTS : 0), table, SQL_NTS);

	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		return bindCols(self, err);
	} else
		return -1;
}

private	imeth	int	pSelectWATCOMPrimaryKeyFields(object self, char *table, char *oname)
{
	char	jstr[256] = "";
	char	ostr[512] = "";
	char	onstr[256] = "";
	char	*cmd;

	if (oname && *oname) {
		object	eo, ret;

		if (!iErrors)
			iErrors = gNew(Set);
		eo = gNewWithInt(ShortInteger, SQL_ERROR);
		ret = gAdd(iErrors, eo);
		if (!ret)
			gDispose(eo);

		if (!gDBSelectOne(self, "select * from sysuserperm")) {
			strcpy(jstr, "join sysuserperm on sysuserperm.user_id = systable.creator ");
			sprintf(ostr, "and sysuserperm.user_name = '%s' ", oname);
		}

		if (ret)
			gDeepDisposeObj(iErrors, eo);
		sprintf(onstr, ", '%s'", oname);
	}
	
	cmd =	"select table_name, column_name from SYSINDEX join SYSIXCOL "
		"on SYSINDEX.table_id = SYSIXCOL.table_id and "
		"SYSINDEX.index_id = SYSIXCOL.index_id join SYSCOLUMN "
		"on SYSCOLUMN.table_id = SYSINDEX.table_id and "
		"SYSCOLUMN.column_id = SYSIXCOL.column_id "
		"join systable on systable.table_id = sysindex.table_id %s"
		"where SYSINDEX.index_Name = 'PK_%s' %s"
		"order by SYSIXCOL.sequence";
	
	if (vDBSelectOne(self, cmd, jstr, table, ostr)) {
		if (vExecuteWithBind(self, "sp_pkeys '%s'%s", table, onstr))
			return 1;
		if (gNextRecord(self))
			return 1;
		gPrevRecord(self);  // so next gNextRecord reads the first record
		return 0;
	}
	
	return vDBSelect(self, cmd, jstr, table, ostr);
}

imeth	int	gSelectReferencedBy(char *table, char *oname)
{
	char	ostr[256];
	gEndSQL(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMReferencedBy(self, table, oname);
		break;
	case DBMS_MSSQL:
		if (oname && *oname)
			sprintf(ostr, ", '%s'", oname);
		else
			*ostr = '\0';
		if (vExecuteWithBind(self, "sp_fkeys '%s'%s", table, ostr))
			return 1;
		if (gNextRecord(self))
			return 1;
		return vExecuteWithBind(self, "sp_fkeys '%s'%s", table, ostr);
		break;
	}

	return 1;
}

private	imeth	int	pSelectWATCOMReferencedBy(object self, char *table, char *oname)
{
	char	ostr[512];
	char	*cmd;

	if (oname  &&  *oname)
		sprintf(ostr, "and sysuserperm.user_name = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select sysforeignkey.role as fk_name, foreigntbl.table_name as fktable_name, "
		"reftbl.table_name as pktable_name, foreignsc.column_name as fkcolumn_name, "
		"primarysc.column_name as pkcolumn_name from sysfkcol join sysforeignkey "
		"on sysfkcol.foreign_table_id = sysforeignkey.foreign_table_id and "
		"sysfkcol.foreign_key_id = sysforeignkey.foreign_key_id "
		"join systable as foreigntbl on foreigntbl.table_id = sysforeignkey.foreign_table_id "
		"join systable as reftbl on reftbl.table_id = sysforeignkey.primary_table_id "
		"join syscolumn as primarysc on primarysc.table_id = sysforeignkey.primary_table_id "
		"and primarysc.column_id = sysfkcol.primary_column_id "
		"join syscolumn as foreignsc on foreignsc.table_id = sysforeignkey.foreign_table_id "
		"and foreignsc.column_id = sysfkcol.foreign_column_id "
		"join sysuserperm on sysuserperm.user_id = foreigntbl.creator "
		"where reftbl.table_name = '%s' %s"
		"order by fk_name, fktable_name, foreignsc.column_id";
	
	if (vDBSelectOne(self, cmd, table, ostr))
		return 1;
	return vDBSelect(self, cmd, table, ostr);
}

imeth	int	gSelectForeignKeys(char *table, char *oname)
{
	char	ostr[256];
	gEndSQL(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMForeignKeys(self, table, oname);
		break;
	case DBMS_MSSQL:
		if (oname && *oname)
			sprintf(ostr, ", '%s'", oname);
		else
			*ostr = '\0';
		if (vExecuteWithBind(self, "sp_fkeys null, null, null, '%s'%s", table, ostr))
			return 1;
		if (gNextRecord(self))
			return 1;
		return vExecuteWithBind(self, "sp_fkeys null, null, null, '%s'%s", table, ostr);
		break;
	}

	return 1;
}

private	imeth	int	pSelectWATCOMForeignKeys(object self, char *table, char *oname)
{
	char	ostr[512];
	char	*cmd;

	if (oname  &&  *oname)
		sprintf(ostr, "and sysuserperm.user_name = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select sysforeignkey.role as fk_name, foreigntbl.table_name as fktable_name, "
		"reftbl.table_name as pktable_name, foreignsc.column_name as fkcolumn_name, "
		"primarysc.column_name as pkcolumn_name from sysfkcol join sysforeignkey "
		"on sysfkcol.foreign_table_id = sysforeignkey.foreign_table_id and "
		"sysfkcol.foreign_key_id = sysforeignkey.foreign_key_id "
		"join systable as foreigntbl on foreigntbl.table_id = sysforeignkey.foreign_table_id "
		"join systable as reftbl on reftbl.table_id = sysforeignkey.primary_table_id "
		"join syscolumn as primarysc on primarysc.table_id = sysforeignkey.primary_table_id "
		"and primarysc.column_id = sysfkcol.primary_column_id "
		"join syscolumn as foreignsc on foreignsc.table_id = sysforeignkey.foreign_table_id "
		"and foreignsc.column_id = sysfkcol.foreign_column_id "
		"join sysuserperm on sysuserperm.user_id = foreigntbl.creator "
		"where foreigntbl.table_name = '%s' %s"
		"order by fk_name, fktable_name, foreignsc.column_id";
	
	if (vDBSelectOne(self, cmd, table, ostr))
		return 1;
	return vDBSelect(self, cmd, table, ostr);
}

imeth	int	gSelectIndexes(char *table, char *oname)
{
	gEndSQL(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMIndexes(self, table, oname);
		break;
	case DBMS_MSSQL:
		return pSelectMSSQLIndexes(self, table, oname);
		break;
	}

	return 1;
}

private	imeth	int	pSelectWATCOMIndexes(object self, char *table, char *oname)
{
	char	ostr[512];
	char	*cmd;

	if (oname && *oname)
		sprintf(ostr, "and ixtable_owner = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select systable.table_name as ixtable_name, "
		"sysuserperm.user_name as ixtable_owner, "
		"sysindex.index_name ix_name, sysixcol.\"order\" ix_order, "
		"substr('NYY', locate('NUY', sysindex.\"unique\"), 1) ix_unique, "
		"syscolumn.column_name as ixcolumn_name from systable "
		"join sysindex on systable.table_id = sysindex.table_id "
		"join sysixcol on sysindex.table_id = sysixcol.table_id and "
		"sysixcol.index_id = sysindex.index_id "
		"join syscolumn on syscolumn.table_id = sysixcol.table_id and "
		"syscolumn.column_id = sysixcol.column_id "
		"join sysuserperm on sysuserperm.user_id = systable.creator "
		"where systable.table_name = '%s' and ix_name <> 'PK_%s' %s"
		"order by ixtable_name, ix_name, sysixcol.sequence";
	
	if (vDBSelectOne(self, cmd, table, table, ostr))
		return 1;
	return vDBSelect(self, cmd, table, table, ostr);
}

private	imeth	int	pSelectMSSQLIndexes(object self, char *table, char *oname)
{
	char	ostr[512];
	char	*cmd;

	if (oname && *oname)
		sprintf(ostr, "and u.name = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select ixtable_name = convert(varchar(32),o.name), "
		"ixcolumn_name = convert(varchar(32),c.name), "
		"ix_sequence = convert(smallint,c1.colid), "
		"ix_unique = SUBSTRING('NNY', (i.status & 2) + 1, 1), "
		"ix_name = convert(varchar(32),i.name), 'A' ix_order, "
		"ixtable_owner = convert(varchar(32), user_name(o.uid)) "
		"from sysindexes i, syscolumns c, sysobjects o, syscolumns c1, sysusers u "
		"where o.id = object_id('%s') and o.id = c.id and o.id = i.id "
		"and c.name = index_col (o.name, i.indid, c1.colid) and u.uid = o.uid "
		"and c1.colid <= i.keycnt and c1.id = object_id('loan_main') "
		"and indexproperty(o.id, i.name, 'IsAutoStatistics') = 0 "
		"and indexproperty(o.id, i.name, 'IsStatistics') = 0 and i.indid > 1 %s"
		"order by ixtable_name, ix_name, ix_sequence";

	if (vDBSelectOne(self, cmd, table, ostr))
		return 1;
	return vDBSelect(self, cmd, table, ostr);
}

imeth	int	gSelectTriggers(char *table, char type, char *oname)
{
	gEndSQL(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		return pSelectWATCOMTriggers(self, table, type, oname);
		break;
	case DBMS_MSSQL:
		return pSelectMSSQLTriggers(self, table, type, oname);
		break;
	}

	return 1;
}

private	imeth	int	pSelectWATCOMTriggers(object self, char *table, char type, char *oname)
{
	char	ostr[512];
	char	*cmd;

	if (oname && *oname)
		sprintf(ostr, "and trtable_owner = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select o.table_name trtable_name, u.user_name trtable_owner, "
		"t.trigger_name tr_name, t.event trevent_type, "
		"t.trigger_defn tr_text, t.trigger_order tr_order "
		"from systable o join systrigger t on o.table_id = t.table_id "
		"join sysuserperm u on o.creator = u.user_id "
		"where o.table_name = '%s' and t.event = '%c' %s"
		"order by o.table_name, t.event";
	
	if (vDBSelectOne(self, cmd, table, type, ostr))
		return 1;
	return vDBSelect(self, cmd, table, type, ostr);
}

private	imeth	int	pSelectMSSQLTriggers(object self, char *table, char type, char *oname)
{
	char	*tfld;
	char	ostr[512];
	char	*cmd;

	if (oname && *oname)
		sprintf(ostr, "and u.name = '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select o.name trtable_name, '%s_%c' tr_name, "
		"'%c' trevent_type, c.colid tr_order, c.text tr_text, "
		"user_name(o.uid) trtable_owner "
		"from sysobjects o join syscomments c on c.id = %s "
		"join sysusers u on o.uid = u.uid "
		"where o.name = '%s' %s"
		"order by c.colid";
	
	switch (type) {
	case 'I':	// Insert
	case 'i':
		tfld = "o.instrig";
		break;
	case 'D':	// Delete
	case 'd':
		tfld = "o.deltrig";
		break;
	case 'U':	// Update
	case 'u':
		tfld = "o.updtrig";
		break;
	default:
		return 1;
		break;
	}
	if (vDBSelectOne(self, cmd, table, type, type, tfld, table, ostr))
		return 1;
	return vDBSelect(self, cmd, table, type, type, tfld, table, ostr);
}

private	imeth	int	pSelectWATCOMColumns(object self, char *table, char *oname, int byname)
{
	char	ostr[512];
	char	*cmd;
	char	*order = byname ? "order by table_owner, table_name, column_name" : "";

	if (oname && *oname)
		sprintf(ostr, "and u.user_name like '%s' ", oname);
	else
		*ostr = '\0';
	
	cmd =	"select current database table_qualifier, "
		"u.user_name table_owner, "
		"table_name, "
		"column_name, "
		"d.type_id data_type, "
		"ifnull(c.user_type,d.domain_name, "
		"(select type_name from SYS.SYSUSERTYPE "
		"where type_id=c.user_type)) type_name, "
		"isnull(d.\"precision\",width) \"precision\", "
		"width length, "
		"scale, "
		"if locate(d.domain_name,'char')=0 "
		"and locate(d.domain_name,'binary')=0 "
		"and locate(d.domain_name,'time')=0 "
		"and locate(d.domain_name,'date')=0 then "
		"10 else null endif radix, "
		"if nulls='Y' then 1 else 0 endif nullable, "
		"null remarks, "
		"c.domain_id ss_data_type, "
		"column_id colid "
		"from SYS.SYSCOLUMN as c,SYS.SYSTABLE as t,SYS.SYSDOMAIN as d "
		",SYS.SYSUSERPERMS as u "
		"where c.table_id=t.table_id "
		"and t.table_name like '%s' "
		"and t.creator=u.user_id %s"
		"and c.domain_id=d.domain_id %s";
		
	if (vDBSelectOne(self, cmd, table, ostr, order))
		return 1;
	return vDBSelect(self, cmd, table, ostr, order);
}

private	imeth	int	pSelectMSSQLColumns(object self, char *table, char *oname, int byname)
{
	long	tid;
	char	ostr[512];
	char	*cmd;
	char	*order = byname ? "order by table_owner, table_name, column_name" : "order by 17";
	float	ver = gDBMS_version(iDatabase);

	if (oname && *oname)
		sprintf(ostr, "and u.name = '%s' ", oname);
	else
		*ostr = '\0';

	if (ver >= 7.00)
		cmd =	"SELECT TABLE_QUALIFIER = convert(sysname,DB_NAME()), "
			"TABLE_OWNER = convert(sysname,USER_NAME(o.uid)), "
			"TABLE_NAME = convert(sysname,o.name), "
			"COLUMN_NAME = convert(sysname,c.name), "
			"d.DATA_TYPE, "
			"rtrim(substring('integer smallintchar    varchar datetimedouble  double  ', "
			"charindex(convert (sysname,case "
			"when t.xusertype > 255 then t.name "
			"else d.TYPE_NAME end) ,'int     smallintchar    varchar datetimereal    float   '), 8)) "
			"TYPE_NAME, "
			"convert(int,case "
			"when d.DATA_TYPE in (6,7) then d.data_precision "
			"else OdbcPrec(c.xtype,c.length,c.xprec) "
			"end) \"PRECISION\", convert(int,case "
			"when type_name(d.ss_dtype) IN ('numeric','decimal') then "
			"OdbcPrec(c.xtype,c.length,c.xprec)+2 else isnull(d.length, c.length) end) LENGTH, "
			"SCALE = convert(smallint, OdbcScale(c.xtype,c.xscale)), d.RADIX, "
			"NULLABLE = convert(smallint, ColumnProperty (c.id, c.name, 'AllowsNull')), "
			"REMARKS = convert(varchar(254),null), "
			"COLUMN_DEF = text, "
			"d.SQL_DATA_TYPE, d.SQL_DATETIME_SUB, "
			"CHAR_OCTET_LENGTH = isnull(d.length, c.length)+d.charbin, "
			"ORDINAL_POSITION = convert(int,c.colid), "
			"IS_NULLABLE = convert(varchar(254), "
			"substring('NO YES',(ColumnProperty (c.id, c.name, 'AllowsNull')*3)+1,3)), "
			"SS_DATA_TYPE = c.type "
			"FROM sysobjects o, master.dbo.spt_datatype_info d, systypes t, sysusers u, syscolumns c "
			"LEFT OUTER JOIN syscomments m on c.cdefault = m.id AND m.colid = 1 "
			"WHERE o.id = %ld AND c.id = o.id AND t.xtype = d.ss_dtype "
			"AND c.length = isnull(d.fixlen, c.length) AND (d.ODBCVer is null or d.ODBCVer = 2) "
			"AND o.type <> 'P' "
			"AND isnull(d.AUTO_INCREMENT,0) = isnull(ColumnProperty (c.id, c.name, 'IsIdentity'),0) "
			"AND c.xusertype = t.xusertype %s%s";
	else
		cmd =	"SELECT TABLE_QUALIFIER = convert(varchar(32),DB_NAME()), "
			"TABLE_OWNER = convert(varchar(32),USER_NAME(o.uid)), "
			"TABLE_NAME = convert(varchar(32),o.name), "
			"COLUMN_NAME = convert(varchar(32),c.name), "
			"d.DATA_TYPE, "
			"rtrim(substring('integer smallintchar    varchar datetimedouble  double  ', "
			"charindex(convert (varchar(30),case "
			"when t.usertype > 100 or t.usertype in (18,80) then t.name "
			"else d.TYPE_NAME end) ,'int     smallintchar    varchar datetimereal    float   '), 8)) "
			"TYPE_NAME, "
			"convert(int,case "
			"when d.DATA_TYPE in (6,7) then d.data_precision "
			"else isnull(convert(int,c.prec), 2147483647) "
			"end) \"PRECISION\", convert(int,case "
			"when d.ss_dtype IN (106, 108, 55, 63) then "
			"c.prec+2 else isnull(d.length, c.length) end) LENGTH, "
			"SCALE = convert(smallint, c.scale), d.RADIX, NULLABLE = "
			"convert(smallint, convert(bit, c.status&8)), "
			"REMARKS = convert(varchar(254),null),	/* Remarks are NULL */ "
			"COLUMN_DEF = convert(varchar(254),substring(text,2,datalength(text)-2)), "
			"d.SQL_DATA_TYPE, d.SQL_DATETIME_SUB, "
			"CHAR_OCTET_LENGTH = isnull(convert(int,c.prec), 2147483647)+d.charbin, "
			"ORDINAL_POSITION = convert(int,c.colid), "
			"IS_NULLABLE = convert(varchar(254),rtrim(substring('NO      YES',(c.status&8)+1,3))), "
			"SS_DATA_TYPE = c.type "
			"FROM syscolumns c, sysobjects o, syscomments m, sysusers u, "
			"master.dbo.spt_datatype_info d, systypes t "
			"WHERE o.id = %ld AND c.id = o.id AND t.type = d.ss_dtype and u.uid = o.uid "
			"AND c.length = isnull(d.fixlen, c.length) AND (d.ODBCVer is null or d.ODBCVer = 2) "
			"AND o.type <> 'P' AND isnull(d.AUTO_INCREMENT,0) = (c.status&128)/128 "
			"AND c.usertype = t.usertype AND c.cdefault *= m.id AND m.colid = 1 %s%s";

	if (vDBSelectOne(self, "select id from sysobjects where name = '%s'", table))
		return 1;
	tid = gFldGetLong(self, "id");

	if (vDBSelectOne(self, cmd, tid, ostr, order))
		return 1;
	return vDBSelect(self, cmd, tid, ostr, order);
}

imeth	gNow()
{
	gEndSQL(self);
	return gNow(iDatabase);
}

private	imeth	char	*pFldGetVarText(object si, char *fld, char *vttbl)
{
	object	vtobj;

	gSetColVarText(si, NULL, vttbl);
	
	vtobj = gGetColVarText(si);
	return vtobj ? gStringValue(vtobj) : "";
}

private	imeth	pFldSetVarText(object si, char *fld, char *str, char *vttbl)
{
	char	*cp = str ? str : "";

	iVarTextChanged = 1;
	
	gSetColVarText(si, cp, vttbl);
	gSetNotNull(si);
	return self;
}

imeth	int	gEnableTP(int flg)
{
	int	old = iDataSync;
	iDataSync = flg;
	return old;
}

private	imeth	char	*pGetVarTextTable(object self, object si, char *fld)
{
	object	cls = gClass(si);
	char	*cp = NULL;
	
	if ((cls == LongInteger  ||  cls == ShortInteger))
		if (!(cp = gGetVarTextTable(iDatabase, iTable, fld))  &&  iTableList) {
			object	seq, tn;
		
			for (seq=gSequence(iTableList) ; !cp  &&  (tn = gNext(seq)) ; )
				if (cp = gGetVarTextTable(iDatabase, gStringValue(tn), fld))
					gDispose(seq);
		}
	return cp;
}

imeth	char	*gFldGetVarText(object si, char *fld, char *vttbl)
{
	return pFldGetVarText(self,si,fld,vttbl);
}

imeth	gFldSetVarText(object si, char *fld, char *str, char *vttbl)
{
	return pFldSetVarText(self,si,fld,str,vttbl);
}

imeth	char	*gGetVarTextTable2(object self, object si, char *fld)
{
	return pGetVarTextTable(self,si,fld);
}

imeth	gEnableWrites()
{
	if (!iLockEnableWrites)
		iReadOnly = 0;
	
	return self;
}

imeth	int	gIsReadOnly()
{
	return iReadOnly;
}

imeth	gDisableWrites()
{
	if (!iLockEnableWrites)
		iReadOnly = 1;
	
	return self;
}

imeth	gLockEnableWrites()
{
	iLockEnableWrites = 1;
	return self;
}

imeth	int	gIgnoreAllErrors(int v)
{
	int	r = iIgnoreAllErrors;
	iIgnoreAllErrors = v;
	return r;
}

imeth	int	gReturnAllErrors(int v)
{
	int	r = iReturnAllErrors;
	iReturnAllErrors = v;
	return r;
}

imeth	char	*gLastSelect()
{
	return iLastSelect ? gStringValue(iLastSelect) : NULL;
}

imeth	int	gGetCursorPosition()
{
	return iPos;
}

cmeth	int	gSaveLastSelect(int mode)
{
	int	pmode = cSaveLastSelect;
	cSaveLastSelect = mode;
	return pmode;
}

static	char	*query_buffer(int reclen)
{
	char	*p = malloc(QUERY_BUFFER_SIZE+reclen);
	if (!p)
		gError(Object, "Out of memory.");
	return p;
}

cmeth	gLogODBC(char *file)
{
	if (cLFP) {
		fclose(cLFP);
		cLFP = NULL;
	}
	if (file)
		cLFP = fopen(file, "w");
	cSeq = 0UL;
#ifdef	_WIN32
	if (!cFreq.QuadPart)
		QueryPerformanceFrequency(&cFreq);
#endif
	return self;
}

static	void	begin_log(void)
{
	QueryPerformanceCounter(&cCount);
}

static	void	end_log(void)
{
	QueryPerformanceCounter(&cEnd);
}

#if 1

static	void	print_log(char *fun, char *stmt, char *org)
{
#ifdef	_WIN32
	if (stmt ==  org  ||  !strcmp(stmt, org))
		fprintf(cLFP, "%6.3f\t%9lu\t%s\t%s    |    SAME\n",
			(double)(cEnd.QuadPart - cCount.QuadPart) / (double) cFreq.QuadPart,
			++cSeq, fun, stmt);
	else
		fprintf(cLFP, "%6.3f\t%9lu\t%s\t%s    |    %s\n",
			(double)(cEnd.QuadPart - cCount.QuadPart) / (double) cFreq.QuadPart,
			++cSeq, fun, stmt, org);
//	fflush(cLFP);
#endif
}

#else

static	void	print_log(char *fun, char *stmt, char *org)
{
#ifdef	_WIN32
	if (stmt ==  org  ||  !strcmp(stmt, org))
		fprintf(cLFP, "%s\n",
			stmt);
	else
		fprintf(cLFP, "%s\n",
			stmt);
//	fflush(cLFP);
#endif
}

#endif

cmeth	gPrintODBCLog(char *fun, char *msg)
{
	BEGIN_LOG();
	END_LOG();
	PRINT_LOG(fun, msg, msg);
	return self;
}

static	char   *centerStrip(char *s)
{
	int i, j, flg;

	if (!s)
		return s;
	for (j = i = 0, flg = 1; s[i]; i++)
		if (flg)
			if (isspace(s[i]))
				j++;
			else
				flg = 0;
	i -= j + 1;
	if (j)
		memmove(s, s + j, i + 2);
	for (; i >= 0 && isspace(s[i]); i--);
	s[i + 1] = '\0';
	return s;
}

static	void	addMSSQLCols(object idxobj, char *keys)
{
	char	*p = keys;
	char	col[100];
	char	*cp, *tp;
	int	desc;

	while (*p) {
		cp = col;
		while (*p && *p != ',')
			*cp++ = *p++;
		*cp = '\0';
		if (*col) {
			if (tp = strstr(col, "(-)")) {
				desc = 1;
				*tp = '\0';
			} else
				desc = 0;
			gAddStr(idxobj, lcase(centerStrip(col)), gNewWithInt(ShortInteger, desc));
		}
		if (*p)
			p++;
	}
}

static	object	loadMSSQLColOrders(object stmt, char *tbl)
{
	object	rval = gNew(StringDictionary);
	object	idxobj;
	char	buf[256];
	int	prev = gReturnAllErrors(stmt, 1);
	int	r;

	sprintf(buf, "sp_helpindex %s", tbl);
	r = gExecute(stmt, buf);
	gReturnAllErrors(stmt, prev);
	if (!r) {
		gExecuteWithBind(stmt, buf);
		while (!gNextRecord(stmt)) {
			idxobj = gNew(StringDictionary);
			strcpy(buf, gFldGetString(stmt, "index_name"));
			gAddStr(rval, lcase(buf), idxobj);
			addMSSQLCols(idxobj, gFldGetString(stmt, "index_keys"));
		}
	}

	return rval;
}

static	object	loadOracleColOrders(object stmt, char *tbl)
{
	object	rval = gNew(StringDictionary);
	object	idxobj, tobj;
	char	buf[256], *p, buf2[256];
	int	len;

	vDBSelect(stmt, "SELECT C.INDEX_NAME, COLUMN_NAME, COLUMN_EXPRESSION FROM USER_IND_COLUMNS C "
		  "JOIN USER_IND_EXPRESSIONS E "
		  "ON E.INDEX_NAME = C.INDEX_NAME "
		  "AND E.COLUMN_POSITION = C.COLUMN_POSITION "
		  "WHERE C.TABLE_NAME = ~%s~", tbl);
	while (!gNextRecord(stmt)) {
		strcpy(buf, gFldGetString(stmt, "index_name"));
		if (!(idxobj = gFindValueStr(rval, lcase(buf)))) {
			idxobj = gNew(StringDictionary);
			gAddStr(rval, buf, idxobj);
		}
		strcpy(buf, gFldGetString(stmt, "column_expression"));
		len = strlen(buf);
		if (len > 1) {
			buf[strlen(buf) - 1] = '\0';
			p = buf + 1;
		} else
			p = buf;
		strcpy(buf2, gFldGetString(stmt, "column_name"));
		gAddStr(idxobj, lcase(buf2), gNewWithStr(String, lcase(centerStrip(p))));
	}

	return rval;
}

imeth	int	gSQLStatistics(char *tname, char *oname)
{
	RETCODE	r;
	static	char	err[] = "gSQLStatistics::Statement Error";
	char	*schema = pGetOwnerName(self, oname);
	int	dbtype = gDBMS_type(iDatabase);
	object	colorders = NULL;
	char	idxname[100], colname[100];

 	if (dbtype == DBMS_MSSQL)
		colorders = loadMSSQLColOrders(self, tname);
	else if (dbtype == DBMS_ORACLE)
		colorders = loadOracleColOrders(self, tname);

	gEndSQL(self);
	gEnterCriticalSection(iDatabase);

	iUseCursors = 1;

	if (dbtype == DBMS_ORACLE) {
		char	cmd[2048], *p;
		
		sprintf(cmd, "SELECT '' AS TABLE_CAT, "
			"T1.TABLE_OWNER AS TABLE_SCHEM, "
			"T1.TABLE_NAME, "
			"CASE WHEN T1.UNIQUENESS = 'UNIQUE' THEN 0 ELSE 1 END AS NON_UNIQUE, "
			"T1.TABLE_OWNER AS INDEX_QUALIFIER, "
			"T1.INDEX_NAME, "
			"3 AS TYPE, "
			"T2.COLUMN_POSITION AS ORDINAL_POSITION, "
			"T2.COLUMN_NAME, "
			"CASE WHEN T2.DESCEND = 'DESC' THEN 'D' ELSE 'A' END AS ASC_OR_DESC, "
			"NULL AS CARDINALITY, "
			"NULL AS PAGES, "
			"NULL AS FILTER_CONDITION "
			"FROM USER_INDEXES T1 "
			"JOIN USER_IND_COLUMNS T2 "
			"ON T2.INDEX_NAME = T1.INDEX_NAME "
			"WHERE T1.TABLE_OWNER = ~%s~ AND T1.TABLE_NAME = ~%s~ "
			"ORDER BY T1.INDEX_NAME, T1.TABLE_OWNER, T1.TABLE_NAME, T2.COLUMN_POSITION",
			schema, tname);
		p = fix_statement(cmd, iDBMS);
		r = SQLExecDirect(iStmt, p, SQL_NTS);
		free(p);
	} else
		r = SQLStatistics(iStmt, NULL, 0, schema, (SWORD) (schema ? SQL_NTS : 0),
				  tname, SQL_NTS, SQL_INDEX_ALL, SQL_ENSURE);
	
	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		SWORD	n, type, scale, nulls;
		UWORD	i;
		SQLLEN	prec;
		char	cname[100];
		object	si;
		int	size;
		float	odbc_ver = gODBC_version(iDatabase);
		object	namecol, colcol, adcol;

		r = SQLNumResultCols(iStmt, &n);
		if (r)
			if (iIgnoreAllErrors) {
				iErrorState = 1;
				r = -1;
			} else
				vError(self, err);
		iCols = gNewWithInt(StringDictionary, 101);
		iColList = gNew(LinkObject);
		iRecordLength = 0;
		for (i=1 ; i <= n ; i++)  {
			r = SQLDescribeCol(iStmt, i, cname, sizeof(cname)-1, NULL,
					   &type, &prec, &scale, &nulls);
                        // Convert ODBC 2.0 column names to ODBC 3.0 names for consistency
			if (!stricmp(cname, "table_qualifier"))
				strcpy(cname, "table_cat");
			else if (!stricmp(cname, "table_owner"))
				strcpy(cname, "table_schem");
			else if (!stricmp(cname, "seq_in_index"))
				strcpy(cname, "ordinal_position");
			else if (!stricmp(cname, "collation"))
				strcpy(cname, "asc_or_desc");
			if (!r)
				si = gNewSelect(StatementInfo, self, iStmt, (int) i, lcase(cname), type, prec, scale, nulls, &r,
						iDBMS, &size);
			if (r  ||  !si)
				if (iIgnoreAllErrors) {
					iErrorState = 1;
					if (colorders)
						gDeepDispose(colorders);
					return -1;
				} else
					vError(self, err);
			if (iDBMS == DBMS_MSSQL  ||  iDBMS == DBMS_ORACLE) {
				if (!stricmp(cname, "asc_or_desc"))
					adcol = si;
				else if (!stricmp(cname, "index_name"))
					namecol = si;
				else if (!stricmp(cname, "column_name"))
					colcol = si;
			}
			gAddStr(iCols, cname, si);
			gAddLast(iColList, si);
//		        if (type != SQL_GUID)
			iRecordLength += size;
		}
		iTType = TYPE_SELECT;
		
//		r = bindCols(self, err);
		if (iDBMS == DBMS_MSSQL  ||  iDBMS == DBMS_ORACLE) {
			int	r=0;
			object	idxobj, colobj;
			
			iOracleSQLStatistics = (iDBMS == DBMS_ORACLE);
			
			while (!r) {
				r = SQLFetch(iStmt);
				if (!r) {
					gToLower(gGetValue(namecol));
					gToLower(gGetValue(colcol));
					strcpy(idxname, gStringValue(gStripRight(gGetValue(namecol))));
					strcpy(colname, gStringValue(gStripRight(gGetValue(colcol))));
					if (iDBMS == DBMS_MSSQL &&
					    namecol  &&  colcol  &&  adcol  &&
					    (idxobj = gFindValueStr(colorders, lcase(idxname))) &&
					    (colobj = gFindValueStr(idxobj, lcase(colname))))
						gChangeStrValue(gGetValueToPut(adcol), gShortValue(colobj) ? "D" : "A");
					else if (iDBMS == DBMS_ORACLE && namecol  &&  colcol  &&  adcol  &&
						 (idxobj = gFindValueStr(colorders, lcase(idxname))) &&
						 (colobj = gFindValueStr(idxobj, lcase(colname)))) {
						gChangeStrValue(gGetValueToPut(colcol), gStringValue(colobj));
						gChangeStrValue(gGetValueToPut(adcol), "D");
					}
					cursor_write(self, iPos++);
				}
			}
			if (r != SQL_NO_DATA_FOUND  &&  !iIgnoreAllErrors)
				vError(self, "gSQLStatistics cursor load");
			SQLFreeStmt(iStmt, SQL_CLOSE);
			SQLFreeStmt(iStmt, SQL_UNBIND);
			iStmtAlreadyClosed = 1;
			if (r == SQL_NO_DATA_FOUND)
				iPos = 0;
			else if (iIgnoreAllErrors) {
				gLeaveCriticalSection(iDatabase);
				if (colorders)
					gDeepDispose(colorders);
				return 0;
			}
		}
	} else
		r = -1;
	gLeaveCriticalSection(iDatabase);
	if (colorders)
		gDeepDispose(colorders);
	return r;
}

imeth	int	gSQLForeignKeys(char *tname, char *oname, int refby)
{
	RETCODE	r;
	static	char	err[] = "gSQLForeignKeys::Statement Error";
	char	*schema = pGetOwnerName(self, oname);
	int	dbtype = gDBMS_type(iDatabase);

	if (dbtype == DBMS_ORACLE) {
		if (!refby)
			return vDBSelect(self, "SELECT '' AS PKTABLE_CAT, "
					 "F1.R_OWNER AS PKTABLE_SCHEM, "
					 "P1.TABLE_NAME AS PKTABLE_NAME, "
					 "P2.COLUMN_NAME AS PKCOLUMN_NAME, "
					 "'' AS FKTABLE_CAT, "
					 "F1.OWNER AS FKTABLE_SCHEM, "
					 "F1.TABLE_NAME AS FKTABLE_NAME, "
					 "F2.COLUMN_NAME AS FKCOLUMN_NAME, "
					 "F2.POSITION AS KEY_SEQ, "
					 "0 AS UPDATE_RULE, "
					 "CASE WHEN F1.DELETE_RULE = 'NO ACTION' THEN 3 ELSE 0 END AS DELETE_RULE, "
					 "F1.CONSTRAINT_NAME AS FK_NAME, "
					 "F1.R_CONSTRAINT_NAME AS PK_NAME "
					 "FROM USER_CONSTRAINTS F1 "
					 "JOIN USER_CONSTRAINTS P1 "
					 "ON P1.CONSTRAINT_NAME = F1.R_CONSTRAINT_NAME "
					 "JOIN USER_CONS_COLUMNS F2 "
					 "ON F2.CONSTRAINT_NAME = F1.CONSTRAINT_NAME "
					 "JOIN USER_CONS_COLUMNS P2 "
					 "ON P2.CONSTRAINT_NAME = F1.R_CONSTRAINT_NAME "
					 "AND P2.POSITION = F2.POSITION "
					 "WHERE F1.OWNER = ~%s~ AND F1.TABLE_NAME = ~%s~ "
					 "ORDER BY F1.CONSTRAINT_NAME, F1.OWNER, F1.TABLE_NAME, F2.POSITION",
					 schema, tname);
		
// This is commented out because it is slower than SQLForeignKeys
// The problem is with getting the FKCOLUMN_NAME field.  That join is slow.
// I'm leaving it here for future reference.
		
// 		return vDBSelect(self, "SELECT '' AS PKTABLE_CAT, "
// 				 "P1.OWNER AS PKTABLE_SCHEM, "
// 				 "P1.TABLE_NAME AS PKTABLE_NAME, "
// 				 "P2.COLUMN_NAME AS PKCOLUMN_NAME, "
// 				 "'' AS FKTABLE_CAT, "
// 				 "F1.OWNER AS FKTABLE_SCHEM, "
// 				 "F1.TABLE_NAME AS FKTABLE_NAME, "
// 				 "F2.COLUMN_NAME AS FKCOLUMN_NAME, "
// 				 "P2.POSITION AS KEY_SEQ, "
// 				 "0 AS UPDATE_RULE, "
// 				 "CASE WHEN F1.DELETE_RULE = 'NO ACTION' THEN 3 ELSE 0 END AS DELETE_RULE, "
// 				 "F1.CONSTRAINT_NAME AS FK_NAME, "
// 				 "F1.R_CONSTRAINT_NAME AS PK_NAME "
// 				 "FROM USER_CONSTRAINTS P1 "
// 				 "JOIN USER_CONSTRAINTS F1 "
// 				 "ON F1.R_CONSTRAINT_NAME = P1.CONSTRAINT_NAME "
// 				 "JOIN USER_CONS_COLUMNS P2 "
// 				 "ON P2.CONSTRAINT_NAME = P1.CONSTRAINT_NAME "
// 				 "JOIN USER_CONS_COLUMNS F2 "
// 				 "ON F2.CONSTRAINT_NAME = F1.CONSTRAINT_NAME "
// 				 "AND F2.POSITION = P2.POSITION "
// 				 "WHERE P1.OWNER = ~%s~ AND P1.TABLE_NAME = ~%s~ "
// 				 "ORDER BY F1.CONSTRAINT_NAME, P1.OWNER, P1.TABLE_NAME, P2.POSITION",
// 				 schema, tname);
	}
	
	gEndSQL(self);
	gEnterCriticalSection(iDatabase);

	iUseCursors = 1;

	if (refby) {
// 		if (dbtype == DBMS_MSSQL) {
// 			char	cmd[256];
		
// 			if (schema)
// 				sprintf(cmd, "sp_fkeys %s, %s", tname, schema);
// 			else
// 				sprintf(cmd, "sp_fkeys %s", tname);
// 			r = SQLExecDirect(iStmt, cmd, SQL_NTS);
// 		} else
		r = SQLForeignKeys(iStmt, NULL, 0, schema, (SWORD) (schema ? SQL_NTS : 0),
				   tname, SQL_NTS, NULL, 0, NULL, 0, NULL, 0);
	} else {
// 		if (dbtype == DBMS_MSSQL) {
// 			char	cmd[256];
		
// 			if (schema)
// 				sprintf(cmd, "sp_fkeys NULL, NULL, NULL, %s, %s", tname, schema);
// 			else
// 				sprintf(cmd, "sp_fkeys NULL, NULL, NULL, %s", tname);
// 			r = SQLExecDirect(iStmt, cmd, SQL_NTS);
// 		} else
		r = SQLForeignKeys(iStmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
				   schema, (SWORD) (schema ? SQL_NTS : 0), tname, SQL_NTS);
	}
	if (r  &&  r != SQL_SUCCESS_WITH_INFO  &&  pTrapError(self, iErrors, r))
		vError(self, err);
	if (!r  ||  r == SQL_SUCCESS_WITH_INFO) {
		SWORD	n, type, scale, nulls;
		UWORD	i;
		SQLLEN	prec;
		char	cname[100];
		object	si;
		int	size;
		float	odbc_ver = gODBC_version(iDatabase);

		r = SQLNumResultCols(iStmt, &n);
		if (r)
			if (iIgnoreAllErrors) {
				iErrorState = 1;
				r = -1;
			} else
				vError(self, err);
		iCols = gNewWithInt(StringDictionary, 101);
		iColList = gNew(LinkObject);
		iRecordLength = 0;
		for (i=1 ; i <= n ; i++)  {
			r = SQLDescribeCol(iStmt, i, cname, sizeof(cname)-1, NULL,
					   &type, &prec, &scale, &nulls);
                        // Convert ODBC 2.0 column names to ODBC 3.0 names for consistency
			if (!stricmp(cname, "pktable_qualifier"))
				strcpy(cname, "pktable_cat");
			else if (!stricmp(cname, "pktable_owner"))
				strcpy(cname, "pktable_schem");
			else if (!stricmp(cname, "fktable_qualifier"))
				strcpy(cname, "fktable_cat");
			else if (!stricmp(cname, "fktable_owner"))
				strcpy(cname, "fktable_schem");
			if (!r)
				si = gNewSelect(StatementInfo, self, iStmt, (int) i, lcase(cname), type, prec, scale, nulls, &r,
						iDBMS, &size);
			if (r  ||  !si)
				if (iIgnoreAllErrors) {
					iErrorState = 1;
					return -1;
				} else
					vError(self, err);
			gAddStr(iCols, cname, si);
			gAddLast(iColList, si);
//		        if (type != SQL_GUID)
			iRecordLength += size;
		}
		iTType = TYPE_SELECT;
		
//		r = bindCols(self, err);
		if (iDBMS == DBMS_MSSQL) {
			int	r=0;
			while (!r) {
				r = SQLFetch(iStmt);
				if (!r)
					cursor_write(self, iPos++);
			}
			if (r != SQL_NO_DATA_FOUND  &&  !iIgnoreAllErrors)
				vError(self, "gSQLForeignKeys cursor load");
			SQLFreeStmt(iStmt, SQL_CLOSE);
			SQLFreeStmt(iStmt, SQL_UNBIND);
			iStmtAlreadyClosed = 1;
			if (r == SQL_NO_DATA_FOUND)
				iPos = 0;
			else if (iIgnoreAllErrors) {
				gLeaveCriticalSection(iDatabase);
				return 0;
			}
		}
	} else
		r = -1;
	gLeaveCriticalSection(iDatabase);
	return r;
}

imeth	ifun	gSetRecordTestFunction(ifun fun)
{
	ifun	old = iRecordTestFunction;
	iRecordTestFunction = fun;
	return old;
}

imeth	gGetStatement()
{
	return self;
}

imeth	gSetSpecialAuditInfo(ifun fun, object dict)
{
	iSpecialFH_Tables = dict;
	iSpecialAuditHandler = fun;
	
	return self;
}




