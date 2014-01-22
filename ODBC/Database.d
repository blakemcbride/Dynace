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
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "dynsql.h"

#define DB_ERROR_MESSAGE_SIZE	1024
#define	HASH_SIZE		499

defclass  Database  {
	HDBC	iDbc;
	iTables;		/*  StringDictionary of all tables in a data source  */
	iStatements;		/*  StringDictionary of all statements associated with DB         */
	iPrimaryKeys;		/*  StringDictionary of primary keys for each file   */
	iVarTextFields;		/*  StringDictionary of VarText fields for each file */
	int	iDBMS;
	float	iDBMS_VER;
	float	iODBC_VER;
	short	iODBC_LEVEL;
	iStatement;		/*  single statement for internal commands  */
	iVarTextStmt;		/*  vartext internal statemnt  */
	iTP;			/*  transaction processing object  */
	int	iReadOnly;
	int	iIgnoreAllErrors;
	int	iReturnAllErrors;
	int	iDataChanged;
	time_t	iDBTime;
	time_t	iSysTime;
	object	iUserName;	/* User name for the current connection */
	CRITICALSECTION	iCS;
	int	iDisposing;
	
	//  Field History stuff
	iFH_Table;		/*  Name of table to store history in  */
	iFH_Tables;		/*  StringDictionary by Table name  */
	long	iUserID;
	ifun	iSpecialAuditHandler;	/* handling of special auditable tables */
	iSpecialFH_Tables;				/* list of tables needing special handling */
class:
	HENV	cEnv;
	int	cNDB;		/*  number of DBs opened  */
	RETCODE	cRet;
	char	cEmsg[DB_ERROR_MESSAGE_SIZE];
	int	cIgnoreNAN;	/*  Used to ignore NAN's in the statinfo class	*/
};



#define	streq(a, b)	!strcmp(a, b)
#define	VARTEXTMAX	255

extern	char	*lcname(char *name);
extern	char	*lcase(char *v);
extern	int	_ReadPrimaryKeys(object db, char *file);

static	object	read_tables(object db, HDBC dbc, object tbls);
static	object	open_database(object self, int query, char *source, char *id, char *pw, object wind, int child);


cmeth	gOpenDatabase(char *source, char *id, char *pw)
{
	return open_database(self, 0, source, id, pw, NULL, 0);
}

cmeth	gQueryDatabase(wind)
{
	return open_database(self, 1, NULL, NULL, NULL, wind, 0);
}

cmeth	gGetDataSources()
{
	HENV	env;
	RETCODE	ret = SQLAllocEnv(&env);
	char	dsn[SQL_MAX_DSN_LENGTH + 1];
	SWORD	ncdsn;
	char	dsd[255];
	SWORD	ncdsd;
	object	ll;

	if (ret)
		return NULL;

	ll = gNew(LinkObject);
	
	while ((ret = SQLDataSources(env, SQL_FETCH_NEXT, dsn, SQL_MAX_DSN_LENGTH + 1, &ncdsn,
				     dsd, 255, &ncdsd)) == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
		gAddLast(ll, gNewWithObjObj(ObjectAssociation, gNewWithStr(String, dsn), gNewWithStr(String, dsd)));

	SQLFreeEnv(env);

	return ll;
}

static	int	database_type(object self, char *source, char *id, char *pw)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	char	buf[255];
	int	type = 0;

	iDbc  = SQL_NULL_HDBC;
	cNDB++;
	if (!cEnv)  {
		cRet = SQLAllocEnv(&cEnv);
		if (cRet) {
			cEnv = (HENV) 0;
			goto er1;
		}
	}
	cRet = SQLAllocConnect(cEnv, &iDbc);
	if (cRet)
		goto er1;

	cRet = SQLConnect(iDbc, source, SQL_NTS, id, SQL_NTS, pw, SQL_NTS);
	if (cRet  &&  cRet != SQL_SUCCESS_WITH_INFO)
		goto er1;

	SQLGetInfo(iDbc, SQL_DBMS_NAME, buf, sizeof(buf), NULL);
	if (!strnicmp(buf, "WATCOM", 6)  ||
	    !strnicmp(buf, "Sybase SQL Anywhere", 19)  ||
	    !strnicmp(buf, "Adaptive Server Anywhere", 24))
		iDBMS = DBMS_WATCOM;
	else if (!strnicmp(buf, "ACCESS", 6))
		iDBMS = DBMS_ACCESS;
	else if (!strnicmp(buf, "SQL Server", 6))
		iDBMS = DBMS_SYBASE;
	else if (!strnicmp(buf, "Microsoft SQL Server", 13))
		iDBMS = DBMS_MSSQL;
	else if (!strnicmp(buf, "MySQL", 5))
		iDBMS = DBMS_MYSQL;
	else if (!strnicmp(buf, "EXCEL", 5))
		iDBMS = DBMS_EXCEL;
	else if (!strnicmp(buf, "TEXT", 4))
		iDBMS = DBMS_TEXT;
	else if (!strnicmp(buf, "PostgreSQL", 10))
		iDBMS = DBMS_POSTGRES;
	else if (!strnicmp(buf, "Oracle", 6))
		iDBMS = DBMS_ORACLE;

	type = iDBMS;
 er1:
	gDispose(obj);
	return type;
}

static	object	open_database(object self, int query, char *source, char *id, char *pw, object wind, int child)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	char	buf[255];

	INITIALIZECRITICALSECTION(iCS);
	iDbc  = SQL_NULL_HDBC;
	cNDB++;
	if (!cEnv)  {
		cRet = SQLAllocEnv(&cEnv);
		if (cRet) {
			cEnv = (HENV) 0;
			goto er1;
		}
	}
	cRet = SQLAllocConnect(cEnv, &iDbc);
	if (cRet)
		goto er1;

	if (query) {
#if 0
		// Doesn't work on older versions of SQL Anywhere
		cRet = SQLDriverConnect(iDbc, wind ? gHandle(wind) : GetDesktopWindow(),
					NULL, 0, buf, sizeof(buf), NULL, SQL_DRIVER_COMPLETE);
#else
		cRet = SQLDriverConnect(iDbc, wind ? gHandle(wind) : GetDesktopWindow(),
					NULL, 0, buf, sizeof(buf), NULL, SQL_DRIVER_PROMPT);
#endif
		if (cRet == SQL_NO_DATA_FOUND)
			goto er2;
		else if (cRet  &&  cRet != SQL_SUCCESS_WITH_INFO) {
			char	state[10], error[128];
			SQLError(cEnv, iDbc, NULL, state, NULL, error, sizeof error, NULL);
			vError(super, "%s %s", state, error);
			cRet = 1;
		}
	} else {
		char	cstr[256];

		sprintf(cstr, "DSN=%s", source);
		if (id)
			sprintf(cstr+strlen(cstr), ";UID=%s", id);
		if (pw)
			sprintf(cstr+strlen(cstr), ";PWD=%s", pw);
		cRet = SQLDriverConnect(iDbc, wind ? gHandle(wind) : GetDesktopWindow(),
					cstr, SQL_NTS, buf, sizeof(buf), NULL, SQL_DRIVER_COMPLETE);
//		cRet = SQLConnect(iDbc, source, SQL_NTS, id, SQL_NTS, pw, SQL_NTS);
	}
	if (cRet  &&  cRet != SQL_SUCCESS_WITH_INFO)
		goto er1;

	iStatements = gNew(StringDictionary);

	iTables = gNewWithInt(StringDictionary, HASH_SIZE);

	SQLGetInfo(iDbc, SQL_DBMS_NAME, buf, sizeof(buf), NULL);
	if (!strnicmp(buf, "WATCOM", 6)  ||
	    !strnicmp(buf, "Sybase SQL Anywhere", 19)  ||
	    !strnicmp(buf, "Adaptive Server Anywhere", 24))
		iDBMS = DBMS_WATCOM;
	else if (!strnicmp(buf, "ACCESS", 6))
		iDBMS = DBMS_ACCESS;
	else if (!strnicmp(buf, "SQL Server", 6))
		iDBMS = DBMS_SYBASE;
	else if (!strnicmp(buf, "Microsoft SQL Server", 13))
		iDBMS = DBMS_MSSQL;
	else if (!strnicmp(buf, "MySQL", 5))
		iDBMS = DBMS_MYSQL;
	else if (!strnicmp(buf, "EXCEL", 5))
		iDBMS = DBMS_EXCEL;
	else if (!strnicmp(buf, "TEXT", 4))
		iDBMS = DBMS_TEXT;
	else if (!strnicmp(buf, "PostgreSQL", 10))
		iDBMS = DBMS_POSTGRES;
	else if (!strnicmp(buf, "Oracle", 6))
		iDBMS = DBMS_ORACLE;
	
        //Set the connection to treat string arguments to catalog
        //functions as case-insensitve identifiers (ODBC 3.0).  This is very
        //important on databases that are case-sensitive, like Oracle.
	//This command kills SQLStatistics under Microsoft server
	if (iDBMS == DBMS_ORACLE)
		SQLSetConnectOption(iDbc, SQL_ATTR_METADATA_ID, SQL_TRUE);

	if (iDBMS) {
		SQLGetInfo(iDbc, SQL_DBMS_VER, buf, sizeof(buf), NULL);
		if (iDBMS == DBMS_WATCOM  ||  iDBMS == DBMS_MSSQL  ||  iDBMS == DBMS_EXCEL  ||  iDBMS == DBMS_TEXT || iDBMS == DBMS_ORACLE) {
			buf[5] = '\0';
			iDBMS_VER = atof(buf);
		} else if (iDBMS == DBMS_ACCESS)
			iDBMS_VER = atof(buf);
		else if (iDBMS == DBMS_SYBASE) {
			char	*p;
			for (p=buf ; *p  &&  *p != '/' ; ++p);
			if (*p) {
				char	*e;
				int	n = 0;
				for (e=++p ; *e  &&  *e != '/'  &&  (!n || *e != '.') ; e++)
					if (*e == '.')
						n++;
				*e = '\0';
				iDBMS_VER = atof(p);
			}
		} else if (iDBMS == DBMS_MYSQL) {
			buf[4] = '\0';
			iDBMS_VER = atof(buf);
		} else
			iDBMS_VER = atof(buf);
	}
	SQLGetInfo(iDbc, SQL_ODBC_VER, buf, sizeof(buf), NULL);
	iODBC_VER = atof(buf);
	SQLGetInfo(iDbc, SQL_ODBC_API_CONFORMANCE, &iODBC_LEVEL, sizeof(short), NULL);

	if (SQL_ERROR == SQLGetInfo(iDbc, SQL_USER_NAME, buf, sizeof(buf), NULL))
		goto er1;
	iUserName = gNewWithStr(String, buf);
	
	iPrimaryKeys = gNew(StringDictionary);
	iVarTextFields = gNew(StringDictionary);

	iStatement = gNewStatement(obj);
	iVarTextStmt = gNewStatement(obj);

	//For Oracle, change the default date format to YYYY-MM-DD HH24:MI:SS.
	//This accepts almost any format that puts the fields in the correct order,
	//including yyyy-mm-dd, yyyy.mm.dd, yyyy/mm/dd, and yyyymmdd (no delimiters).  
	//Trailing fields can be omitted, so it is not necessary to include the time fields.
	if (DBMS_ORACLE == iDBMS)
		if (gExecute(obj, "alter session set nls_date_format = 'YYYY-MM-DD HH24:MI:SS'"))
			goto er1;
	
	if (!child)
		gDisposeAtFatalExit(Application, obj);
	return obj;
 er1:
	gGetErrorMessage(obj);
 er2:
	gDispose(obj);
	return NULL;
}

imeth	object	gGCDispose : Database_GCDispose ()
{
	gRemoveAtFatalExit(Application, self);
	if (iDbc != SQL_NULL_HDBC)  {
		cRet = SQLDisconnect(iDbc);
		cRet = SQLFreeConnect(iDbc);
	}
	if (--cNDB  &&  cEnv) {
		cRet = SQLFreeEnv(cEnv);
		cEnv = (HENV) 0;
	}
	DELETECRITICALSECTION(iCS);
	return gDispose(super self);
}

imeth	object	gDispose, gDeepDispose ()
{
	object	seq, stmt;

	iDisposing = 1;
	if (iStatements)
		gDeepDispose(iStatements);
	if (iTables)
		gDeepDispose(iTables);
	if (iPrimaryKeys)
		gDeepDispose(iPrimaryKeys);
	if (iVarTextFields)
		gDeepDispose(iVarTextFields);
	if (iTP)
		gDispose(iTP);
	if (iFH_Table)
		gDispose(iFH_Table);
	if (iFH_Tables)
		gDeepDispose(iFH_Tables);
	if (iSpecialFH_Tables)
		gDeepDispose(iSpecialFH_Tables);
	return Database_GCDispose(self);
}

cmeth	gSetErrorMessage(char *emsg)
{
	char	*p;
	int	i;
	int	sz = DB_ERROR_MESSAGE_SIZE - 1;

	for (i = 0, p = emsg; *p && i < sz; i++, p++)
		cEmsg[i] = *p;
	cEmsg[i] = '\0';
	return self;
}

imeth	char	*gGetErrorMessage()
{
	SQLError(cEnv, iDbc, NULL, NULL, NULL, cEmsg, sizeof cEmsg, NULL);
	return cEmsg;
}

cmeth	char	*gGetErrorMessage()
{
	return cEmsg;
}

imeth	int	gGetErrorCode()
{
	return cRet;
}

cmeth	int	gGetErrorCode()
{
	return cRet;
}

imeth	HDBC	gHDBC()
{
	return iDbc;
}

imeth	HENV	gHENV()
{
	return cEnv;
}

imeth	gAddStatement(stmt)
{
	char	buf[25];
	object	wp = gNewWithObj(WeakPointer, stmt);

	sprintf(buf, "%ld", stmt);
	gDeepDisposeStr(iStatements, buf);
	gAddStr(iStatements, buf, wp);
	return self;
}

imeth	gRemoveStatement(stmt)
{
	char	buf[25];
	object	wp;

	if (!iDisposing) {
		sprintf(buf, "%ld", stmt);
		wp = gFindValueStr(iStatements, buf);
		if (wp) {
			gDispose(wp);
			gRemoveStr(iStatements, buf);
		}
	}
	return self;
}

imeth	gNewStatement, gNewNonCacheStatement()
{
	object	obj = vNew(Statement, self, iFH_Table, iFH_Tables, iUserID);
	gSetSpecialAuditInfo(obj, iSpecialAuditHandler, iSpecialFH_Tables);
	if (iReadOnly)
		gDisableWrites(obj);
	if (iIgnoreAllErrors)
		gIgnoreAllErrors(obj, 1);
	if (iReturnAllErrors)
		gReturnAllErrors(obj, 1);
	return obj;
}

imeth	gStatement()
{
	return iStatement;
}

imeth	object	gExecuteFileReturnAllErrors(char *file)
{
	return gExecuteFileReturnAllErrors(iStatement, file);
}

imeth	int	gExecuteFileWithError(char *file, char *errbuf, int sz)
{
	return gExecuteFileWithError(iStatement, file, errbuf, sz);
}
	
imeth	int	gExecuteFile(char *file)
{
	return gExecuteFileWithError(iStatement, file, NULL, 0);
}

imeth	int	gExecute(char *cmd)
{
	int	r = gExecute(iStatement, cmd);
	gEndSQL(iStatement);
	return r;
}

imeth	int	gDropTable(char *table)
{
	char	cmd[100];

	sprintf(cmd, "drop table %s", table);
	return gExecute(iStatement, cmd);
}

imeth	gGetTable(char *table)
{
	char	name[130];
	object	r;

	strcpy(name, table);
	r = gFindValueStr(iTables, lcase(name));
	if (!r) {
		r = vNew(TableInfo, cEnv, self, table, TT_TABLE, 0);
		if (r)
			gAddStr(iTables, name, r);
	}
	return r;
}

imeth	gResetTableInfo()
{
	if (iTables)
		gDeepDispose(iTables);
	iTables = gNewWithInt(StringDictionary, HASH_SIZE);
	return self;
}

imeth	gGetAllTables()
{
	read_tables(self, iDbc, iTables);
	return iTables;
}

private	imeth	object	pReadAllTables(object self)
{
	HSTMT	stmt;
	char	name[130], type[130], lname[130];
	RETCODE	r;
	int	itype;
	object	ti, nl, seq, tbl;
	char*	schema = iDBMS == DBMS_ORACLE ? gStringValue(iUserName) : 0;
	
	gEnterCriticalSection(self);
	r = SQLAllocStmt(iDbc, &stmt);
	r = SQLTables(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), NULL, 0, "TABLE", SQL_NTS);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, name, (sizeof(name)-1), NULL);
	r = SQLBindCol(stmt, 4, SQL_C_CHAR, type, (sizeof(type)-1), NULL);
	nl = gNew(LinkObject);
	while (!(r = SQLFetch(stmt))) {
		if (streq(type, "TABLE"))
			itype = TT_TABLE;
		else if (streq(type, "VIEW"))
			itype = TT_VIEW;
		else
			itype = 0;
		if (itype)
			gAddLast(nl, gNewWithObjObj(ObjectAssociation,
						    gNewWithStr(String, name),
						    gNewWithLong(LongInteger, (long) itype)));
	}
	r = SQLFreeStmt(stmt, SQL_UNBIND);
	for (seq = gSequence(nl) ; tbl = gNext(seq) ; ) {
		strcpy(name, gStringValue(gKey(tbl)));
		itype = gShortValue(gValue(tbl));
		strcpy(lname, name);
		if (!gFindValueStr(iTables, lcase(lname))) {
			ti = vNew(TableInfo, cEnv, self, name, itype, 1);
			if (ti)
				gAddStr(iTables, lname, ti);
		}
	}
	gDeepDispose(nl);
	
	r = SQLColumns(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), NULL, 0, NULL, 0);

	if (r == SQL_SUCCESS) {
		char	tname[130];
		char	name[130];
		char	owner[130];
		char	*powner;
		char	typebuf[256];
		SWORD	type;
		SDWORD	len;
		SWORD	scale;
		SWORD	nullable;
		long	pcbValueOwner;
		long	pcbValueTypeName;
		long	pcbValueLength;
		long	pcbValueScale;
		long	pcbValueNullable;
		object	tobj;

		r = SQLBindCol(stmt,  2, SQL_C_CHAR, owner, sizeof(owner) - 1, &pcbValueOwner);
		r = SQLBindCol(stmt,  3, SQL_C_CHAR, tname, 129, NULL);
		r = SQLBindCol(stmt,  4, SQL_C_CHAR, name, 129, NULL);
		r = SQLBindCol(stmt,  5, SQL_C_SHORT, &type, 0, NULL);
		r = SQLBindCol(stmt,  6, SQL_C_CHAR, typebuf, sizeof(typebuf) - 1, &pcbValueTypeName);
		r = SQLBindCol(stmt,  7, SQL_C_LONG, &len, 0, &pcbValueLength);
		r = SQLBindCol(stmt,  9, SQL_C_SHORT, &scale, 0, &pcbValueScale);
		r = SQLBindCol(stmt, 11, SQL_C_SHORT, &nullable, 0, &pcbValueNullable);

		while (!(r = SQLFetch(stmt))) {
			if (pcbValueScale == SQL_NULL_DATA)
				scale = 0;
			if (type == SQL_DECIMAL || type == SQL_NUMERIC)
				if (scale != 0)
					type = SQL_DOUBLE;
				else 
					if (len < 5  ||  len == 5  &&  iDBMS == DBMS_ORACLE)
						type = SQL_SMALLINT;
					else 
						type = SQL_INTEGER;

			if (tobj = gFindValueStr(iTables, lcase(tname))) {
				if (pcbValueOwner != SQL_NULL_DATA)
					powner = owner;
				else
					powner = NULL;
					
				gAddColumn(tobj, powner, name, type, typebuf, len, scale, nullable);
			}
		}
	}
	r = SQLFreeStmt(stmt, SQL_DROP);
	
	gLeaveCriticalSection(self);
	return iTables;
}

imeth	gGetAllTablesAndColumns()
{
	pReadAllTables(self);
	return iTables;
}

imeth	gGetColumn(char *table, char *col)
{
	object	t = gGetTable(self, table);
	if (t)
		t = gFindStr(t, col);
	return t;
}

#if 0

//  This code fails in Microsoft SQL Server 6.5 because it can't get field info while in the middle
//  of getting table names.

static	object	read_tables(object db, HDBC dbc, object tbls)
{
	HSTMT	stmt;
	char	name[130], type[130], lname[130];
	RETCODE	r;
	int	itype;
	object	ti;
	char*	schema = gDBMS_type(db) == DBMS_ORACLE ? gUserName(db) : 0;
	
	gEnterCriticalSection(db);
	r = SQLAllocStmt(dbc, &stmt);
	r = SQLTables(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), NULL, 0, NULL, 0);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, name, (sizeof(name)-1), NULL);
	r = SQLBindCol(stmt, 4, SQL_C_CHAR, type, (sizeof(type)-1), NULL);
	while (!(r = SQLFetch(stmt))) {
		if (streq(type, "TABLE"))
			itype = TT_TABLE;
		else if (streq(type, "VIEW"))
			itype = TT_VIEW;
		else
			itype = 0;
		strcpy(lname, name);
		if (itype  &&  !gFindValueStr(tbls, lcase(lname))) {
			ti = vNew(TableInfo, cEnv, db, name, itype, 1);
			if (ti)
				gAddStr(tbls, lname, ti);
		}
	}
	r = SQLFreeStmt(stmt, SQL_UNBIND);
	r = SQLFreeStmt(stmt, SQL_DROP);
	gLeaveCriticalSection(db);
	return tbls;
}

#else

static	object	read_tables(object db, HDBC dbc, object tbls)
{
	HSTMT	stmt;
	char	name[130], type[130], lname[130];
	RETCODE	r;
	int	itype;
	object	ti, nl, seq, tbl;
	char*	schema = gDBMS_type(db) == DBMS_ORACLE ? gUserName(db) : 0;
	
	gEnterCriticalSection(db);
	r = SQLAllocStmt(dbc, &stmt);
	r = SQLTables(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), NULL, 0, "TABLE", SQL_NTS);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, name, (sizeof(name)-1), NULL);
	r = SQLBindCol(stmt, 4, SQL_C_CHAR, type, (sizeof(type)-1), NULL);
	nl = gNew(LinkObject);
	while (!(r = SQLFetch(stmt))) {
		if (streq(type, "TABLE"))
			itype = TT_TABLE;
		else if (streq(type, "VIEW"))
			itype = TT_VIEW;
		else
			itype = 0;
		if (itype)
			gAddLast(nl, gNewWithObjObj(ObjectAssociation,
						    gNewWithStr(String, name),
						    gNewWithLong(LongInteger, (long) itype)));
	}
	r = SQLFreeStmt(stmt, SQL_UNBIND);
	r = SQLFreeStmt(stmt, SQL_DROP);
	for (seq = gSequence(nl) ; tbl = gNext(seq) ; ) {
		strcpy(name, gStringValue(gKey(tbl)));
		itype = gShortValue(gValue(tbl));
		strcpy(lname, name);
		if (!gFindValueStr(tbls, lcase(lname))) {
			ti = vNew(TableInfo, cEnv, db, name, itype, 1);
			if (ti)
				gAddStr(tbls, lname, ti);
		}
	}
	gDeepDispose(nl);
	gLeaveCriticalSection(db);
	return tbls;
}

#endif

imeth	int	gDBMS_type()
{
	return iDBMS;
}

imeth	float	gDBMS_version()
{
	return iDBMS_VER;
}

imeth	float	gODBC_version()
{
	return iODBC_VER;
}

imeth	int	gUserHasPrivilege(char *userName, char *tableName, char *priv)
{
	HSTMT	stmt;
	char	user[130], table[130], privilege[130], grant[5], ubuf[128];
	RETCODE	r;
	int	rval = -1;

	if (!userName)
		userName = gGetUserName(self, ubuf);
	ENTERCRITICALSECTION(iCS);
	r = SQLAllocStmt(iDbc, &stmt);
	r = SQLTablePrivileges(stmt, NULL, 0, NULL, 0, tableName, SQL_NTS);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, table, (sizeof(table)-1), NULL);
	r = SQLBindCol(stmt, 5, SQL_C_CHAR, user, (sizeof(user)-1), NULL);
	r = SQLBindCol(stmt, 6, SQL_C_CHAR, privilege, (sizeof(privilege)-1), NULL);
	r = SQLBindCol(stmt, 7, SQL_C_CHAR, grant, (sizeof(grant)-1), NULL);
	while (rval < 0  &&  !(r = SQLFetch(stmt)))
		if (!stricmp(table, tableName)  &&  !stricmp(userName, user)  &&  !stricmp(privilege, priv))
			if (streq(grant, "YES"))
				rval = 1;
			else
				rval = 0;

	r = SQLFreeStmt(stmt, SQL_UNBIND);
	r = SQLFreeStmt(stmt, SQL_DROP);
	LEAVECRITICALSECTION(iCS);
	
	return rval;
}

imeth	char	*gGetUserName(char *buf)
{
	SQLGetInfo(iDbc, SQL_USER_NAME, buf, 127, NULL);
	return buf;
}

imeth	char	*gGetPassword(char *buf)
{
	SQLGetInfo(iDbc, SQL_USER_NAME, buf, 127, NULL);
	return buf;
}

imeth	char	*gGetCurrentDataSource(char *buf)
{
	SQLGetInfo(iDbc, SQL_DATA_SOURCE_NAME, buf, 127, NULL);
	return buf;
}

imeth	char	*gGetCurrentDatabaseName(char *buf)
{
	SQLGetInfo(iDbc, SQL_DATABASE_NAME, buf, 127, NULL);
	return buf;
}

ivmeth	vVarTextFields(char *table, ...)
{
	object	flds = gNew(StringDictionary), ret;
	char	*fld, *vttbl, tbl[100], lcfld[100];
	MAKE_REST(table);
	while (fld = GetArg(char *)) {
		strcpy(lcfld, fld);
		if (!(vttbl = GetArg(char *)))
			gError(self, "vVarTextFields : Field not matched with VarText table.");
		gAddStr(flds, lcase(lcfld), gNewWithStr(String, lcname(vttbl)));
	}
	strcpy(tbl, table);
	ret = gAddStr(iVarTextFields, lcase(tbl), flds);
	if (!ret) {
		//  in case it was previously there
		gDeepDisposeStr(iVarTextFields, tbl);
		gAddStr(iVarTextFields, tbl, flds);
	}
	return self;
}

imeth	gVarTextFields(char *table, flds)
{
	object	ret;
	char	tbl[100];
	
	strcpy(tbl, table);
	ret = gAddStr(iVarTextFields, lcase(tbl), flds);
	if (!ret) {
		//  in case it was previously there
		gDeepDisposeStr(iVarTextFields, tbl);
		gAddStr(iVarTextFields, tbl, flds);
	}
	return self;
}

imeth	gVarTextInit(char *intbl)
{
	char	*sutbl = intbl  &&  *intbl ? intbl : "VariableTextInit";

	if (gTableExists(self, sutbl)) {
		char	ptbl[100], tbl[100], fname[100];
		object	flds;
		
		vDBSelect(iStatement, "select * from %s order by TableName, FieldName", sutbl);
		*ptbl = '\0';
		while (!gNextRecord(iStatement)) {
			strcpy(tbl, gFldGetString(iStatement, "TableName"));
			if (stricmp(ptbl, lcase(tbl))) {
				if (*ptbl)
					gVarTextFields(self, ptbl, flds);
				strcpy(ptbl, tbl);
				flds = gNew(StringDictionary);
			}
			strcpy(fname, gFldGetString(iStatement, "FieldName"));
			gAddStr(flds, lcase(fname), gNewWithStr(String, gFldGetString(iStatement, "VarTextTableName")));
		}
		if (*ptbl)
			gVarTextFields(self, ptbl, flds);
	}
	return self;
}

imeth	gGetVarTextFields(char *table)
{
	return gFindValueStr(iVarTextFields, lcname(table));
}

imeth	char	*gGetVarTextTable(char *table, char *fld)
{
	object	flds = gFindValueStr(iVarTextFields, lcname(table));
	object	tbl = flds ? gFindValueStr(flds, lcname(fld)) : NULL;

	return tbl ? gStringValue(tbl) : NULL;
}

ivmeth	vPrimaryKey(char *table, ...)
{
	object	flds = gNew(LinkObject), ret;
	char	*fld;
	MAKE_REST(table);
	while (fld = GetArg(char *))
		gAddLast(flds, gNewWithStr(String, lcname(fld)));
	ret = gAddStr(iPrimaryKeys, lcname(table), flds);
	if (!ret) {
		//  in case it was previously there
		gDeepDisposeStr(iPrimaryKeys, lcname(table));
		gAddStr(iPrimaryKeys, lcname(table), flds);
	}
	return self;
}

imeth	gPrimaryKey(char *table, flds)
{
	object	ret;

	ret = gAddStr(iPrimaryKeys, lcname(table), flds);
	if (!ret) {
		//  in case it was previously there
		gDeepDisposeStr(iPrimaryKeys, lcname(table));
		gAddStr(iPrimaryKeys, lcname(table), flds);
	}
	return self;
}

static	void	getTablePrimaryKey(object db, char *table, int dbms)
{
	object	cols = gNewStatement(db);
	object	flds = gNew(LinkObject);
	char	tt[256];
	char	fname[256];

	strcpy(tt, table);
	gSelectPrimaryKeyFields(cols, table, NULL);

	while (!gNextRecord(cols))
		if (dbms == DBMS_MYSQL) {
			if (!strcmp("PRIMARY", gFldGetString(cols, "key_name"))) {
				strcpy(fname, gFldGetString(cols, "column_name"));
				gAddLast(flds, gNewWithStr(String, lcase(fname)));
			}
		} else {
			strcpy(fname, gFldGetString(cols, "column_name"));
			gAddLast(flds, gNewWithStr(String, lcase(fname)));
		}

	gPrimaryKey(db, lcase(tt), flds);

	gDispose(cols);
}

static	void	getPostgresTablePrimaryKey(object db, char *table)
{
	object	flds, stmt = gNewStatement(db);
	char	tt[256], fname[256], buf[256];
	long	r, nidx, indexrelid, relfilenode;

	strcpy(tt, table);
	sprintf(buf, "select indexrelid, relfilenode from pg_index, pg_class where indrelid = relfilenode and "
		"indisprimary = TRUE and indisunique = TRUE and relname = '%s'", table);
	r = gDBSelectOne(stmt, buf);
	if (r) {
		sprintf(buf, "select indexrelid, relfilenode from pg_index, pg_class where indrelid = relfilenode "
			"and indisunique = TRUE and relname = '%s'", table);
		r = gDBSelectOne(stmt, buf);
	}
	if (r) {
		gDispose(stmt);
		return;
	}
	flds = gNew(LinkObject);
	relfilenode = gFldGetLong(stmt, "relfilenode");
	indexrelid = gFldGetLong(stmt, "indexrelid");
	for (r=nidx=0 ; !r ; nidx++) {
		sprintf(buf, "select attname from pg_attribute, pg_index where indexrelid = %ld and "
			"attrelid = %ld and attnum = indkey[%ld]", indexrelid, relfilenode, nidx);
		r = gDBSelectOne(stmt, buf);
		if (!r) {
			strcpy(fname, gFldGetString(stmt, "attname"));
			gAddLast(flds, gNewWithStr(String, lcase(fname)));
		}
	}
	gPrimaryKey(db, lcase(tt), flds);
	gDispose(stmt);
}

imeth	gGetPrimaryKey(char *table)
{
	object	pk = gFindValueStr(iPrimaryKeys, lcname(table));

	if (!pk  &&  (iDBMS == DBMS_WATCOM  ||  iDBMS == DBMS_MSSQL  ||  iDBMS == DBMS_SYBASE  ||  iDBMS == DBMS_MYSQL  ||
		iDBMS == DBMS_POSTGRES || iDBMS == DBMS_ORACLE)) {
		if (iDBMS == DBMS_POSTGRES)
			getPostgresTablePrimaryKey(self, table);
		else
			getTablePrimaryKey(self, table, iDBMS);
		pk = gFindValueStr(iPrimaryKeys, lcname(table));
	}
	return pk;
}

imeth	long	gAddVarText(char *tbl, char *text)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	long	id = 0L, count = 0L;
	int	len, tp;
	object	stmt;
	char	buf[VARTEXTMAX + 1];
	char	*p = text;
	char	cmd[256];

	if (!*text)
		return 0L;
	
	stmt = iVarTextStmt;

	gInsert(stmt, table);

	tp = gEnableTP(stmt, 0);

	while (len=strlen(p)) {
		len = len > VARTEXTMAX ? VARTEXTMAX : len;
		strncpy(buf, p, len);
		buf[len] = '\0';

		gFldSetLong(stmt, "VarTextCount", ++count);
		gFldSetString(stmt, "VarText", buf);

		if (id) {
			gFldSetLong(stmt, "VarTextID", id);
			gAddRecord(stmt);
		} else {
			gAddRecordWithAutoInc(stmt, tbl, "VarTextID", NULL, NULL);
			id = gFldGetLong(stmt, "VarTextID");
		}
		
		p += len;
	}
	gEnableTP(stmt, tp);
	return id;
}

imeth	gGetVarText(char *tbl, long id)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	object	text = gNew(String);
	char	cmd[256];
	
	sprintf(cmd, "select * from %s where VarTextID = %ld order by VarTextCount", table, id);
	gDBSelect(iStatement, cmd);
	while (!gNextRecord(iStatement))
		vBuild(text, NULL, gFldGetString(iStatement, "VarText"), END);

	return text;
}

imeth	long	gAppendVarText(char *tbl, long id, char *text)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	object	stmt;
	long	count = 0L;
	int	len, tp;
	char	*p = text;
	char	buf[VARTEXTMAX + 1];
	char	cmd[256];

	if (!id)
		return gAddVarText(self, tbl, text);
	
	stmt = iVarTextStmt;
	sprintf(cmd, "select VarTextCount from %s "
		"where VarTextID = %ld "
		"order by VarTextCount desc",
		table, id);
	if (!gDBSelectOne(stmt, cmd))
		count = gFldGetLong(stmt, "VarTextCount");

	gInsert(stmt, table);

	tp = gEnableTP(stmt, 0);

	if (!*text  &&  !count) {
		gFldSetLong(stmt, "VarTextID", id);
		gFldSetLong(stmt, "VarTextCount", ++count);
		gAddRecord(stmt);
	} else
		while (len=strlen(p)) {
			len = len > VARTEXTMAX ? VARTEXTMAX : len;
			strncpy(buf, p, len);
			buf[len] = '\0';

			gFldSetLong(stmt, "VarTextID", id);
			gFldSetLong(stmt, "VarTextCount", ++count);
			gFldSetString(stmt, "VarText", buf);
			gAddRecord(stmt);
		
			p += len;
		}
	gEnableTP(stmt, tp);
	return id;
}

imeth	gDeleteVarText(char *tbl, long id)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	char	cmd[256];
	int	tp = gEnableTP(iStatement, 0);

	sprintf(cmd, "delete from %s where VarTextID = %ld", table, id);
	gExecute(iStatement, cmd);
	gEnableTP(iStatement, tp);
	return self;
}

imeth	long	gUpdateVarText(char *tbl, long id, char *text)
{
	if (id)
		gDeleteVarText(self, tbl, id);

	return gAppendVarText(self, tbl, id, text);
}

imeth	gCopyRecords(char *tTbl, fDb, char *fTbl, char *where, int delflag)
{
	object	tStmt = gNewStatement(self);
	object	fStmt = gNewStatement(fDb);
	char	cmd[50];

	sprintf(cmd, "select * from %s", fTbl);
	if (where  &&  strlen(where)) {
		strcat(cmd, " where ");
		strcat(cmd, where);
	}
	
	gDBSelect(fStmt, cmd);
	gInsert(tStmt, tTbl);

	while (!gNextRecord(fStmt)) {
		gClear(tStmt);
		
		gCopyCorresponding(tStmt, fStmt);
		gAddRecord(tStmt);
		
		if(delflag)
			gDeleteRecord(fStmt);
	}
	
	gDispose(tStmt);
	gDispose(fStmt);

	return self;
}

imeth	gCopyCorresponding(fDb)
{
	HSTMT	stmt;
	char	name[130], tbltype[130];
	RETCODE	r;
	HDBC	fDbc = gHDBC(fDb);
	object	tSet = gNew(Set);
	object	fSet = gNew(Set);
	object	seq;
	object	str;
	char*	schema = gDBMS_type(self) == DBMS_ORACLE ? gUserName(self) : 0;
	char*	fromSchema = gDBMS_type(fDb) == DBMS_ORACLE ? gUserName(fDb) : 0;
	
	ENTERCRITICALSECTION(iCS);
	r = SQLAllocStmt(iDbc, &stmt);
	r = SQLTables(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), NULL, 0, NULL, 0);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, name, (sizeof(name)-1), NULL);
	r = SQLBindCol(stmt, 4, SQL_C_CHAR, tbltype, (sizeof(tbltype)-1), NULL);
	while (!SQLFetch(stmt))
		if (!strcmp(tbltype, "TABLE"))
			gAdd(tSet, gNewWithStr(String, name));

	r = SQLFreeStmt(stmt, SQL_UNBIND);
	r = SQLFreeStmt(stmt, SQL_DROP);
	
	r = SQLAllocStmt(fDbc, &stmt);
	r = SQLTables(stmt, NULL, 0, fromSchema, (SQLSMALLINT)(fromSchema ? SQL_NTS : 0), NULL, 0, NULL, 0);
	r = SQLBindCol(stmt, 3, SQL_C_CHAR, name, (sizeof(name)-1), NULL);
	r = SQLBindCol(stmt, 4, SQL_C_CHAR, tbltype, (sizeof(tbltype)-1), NULL);
	while (!SQLFetch(stmt))
		if (!strcmp(tbltype, "TABLE"))
			gAdd(fSet, gNewWithStr(String, name));

	r = SQLFreeStmt(stmt, SQL_UNBIND);
	r = SQLFreeStmt(stmt, SQL_DROP);

	for (seq = gSequence(fSet); str = gNext(seq); )
		if (gFind(tSet, str)) {
			char	*cp = gStringValue(str);

			gCopyRecords(self, cp, fDb, cp, NULL, 0);
		}
	
	gDeepDispose(fSet);
	gDeepDispose(tSet);
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	int	gTableExists(char *tblname)
{
	return !!gGetTable(self, tblname);
}

imeth	gReadPrimaryKeys(char *file)
{
	return _ReadPrimaryKeys(self, file) ? NULL : self;
}

imeth	gNewTP(char type, char *file, long station, long user)
{
	if (iTP)
		gDispose(iTP);

	gDispose(iStatement);
	gDispose(iVarTextStmt);
	iTP = gNewTP(TransactionProcessing, type, file, station, user);
	iStatement = gNewStatement(self);
	iVarTextStmt = gNewStatement(self);

	return iTP;
}

imeth	gSetRouteFunction(long *(*fun)(char *tab))
{
	return iTP ? gSetRouteFunction(iTP, fun) : NULL;
}

imeth	gGetTP()
{
	return iTP;
}

imeth	gEnableWrites()
{
	object	seq, sa, stmt;
	
	iReadOnly = 0;
	
	if (iStatements)
		for (seq=gSequence(iStatements) ; sa = gNext(seq) ; ) {
			stmt = gValue(gValue(sa));
			if (stmt)
				gEnableWrites(stmt);
		}
	return self;
}

imeth	gDumpObjects(char *file, int type)
{
	object	seq, sa, stmt;
	char	*select;
	FILE	*fp;
	int	n = 0;
	
	fp = fopen(file, "w");
	if (!fp)
		return NULL;
	if (iStatements)
		for (seq=gSequence(iStatements) ; sa = gNext(seq) ; n++) {
			stmt = gValue(gValue(sa));
			if (stmt)
				if (select = gLastSelect(stmt))
					fprintf(fp, "%s\n", select);
				else
					fprintf(fp, "No select\n");
		}
	fprintf(fp, "\nTotal statements for that database = %d\n", n);
	fprintf(fp, "\nTotal statements for all databases = %d\n\n", gSize(Statement));
	fclose(fp);
	return self;
}

imeth	gDisableWrites()
{
	object	seq, sa, stmt;

	iReadOnly = 1;
	
	if (iStatements)
		for (seq=gSequence(iStatements) ; sa = gNext(seq) ; ) {
			stmt = gValue(gValue(sa));
			if (stmt)
//				if (stmt != iStatement)
					gDisableWrites(stmt);
		}
	return self;
}

cmeth	int	gIgnoreNAN(int mode)
{
	int	pm = cIgnoreNAN;

	if (mode >= 0)
		cIgnoreNAN = mode;

	return pm;
}

imeth	int	gSize()
{
	return gSize(iStatements);
}

imeth	int	gIgnoreAllErrors(int v)
{
	int	r = iIgnoreAllErrors;
	iIgnoreAllErrors = v;
	if (iStatement)
		gIgnoreAllErrors(iStatement, v);
	if (iVarTextStmt)
		gIgnoreAllErrors(iVarTextStmt, v);
	return r;
}

imeth	int	gReturnAllErrors(int v)
{
	int	r = iReturnAllErrors;
	iReturnAllErrors = v;
	if (iStatement)
		gReturnAllErrors(iStatement, v);
	if (iVarTextStmt)
		gReturnAllErrors(iVarTextStmt, v);
	return r;
}

imeth	gEnterCriticalSection()
{
	ENTERCRITICALSECTION(iCS);
	return self;
}

imeth	gLeaveCriticalSection()
{
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gSetChanged()
{
	iDataChanged = 1;
	return self;
}

imeth	gResetChanged()
{
	iDataChanged = 0;
	return self;
}

imeth	int	gDataChanged()
{
	return iDataChanged;
}

private	imeth	pNow()
{
	long	dt, tm;	
	char	*cmd = NULL;
	object	stmt = gNewStatement(self);

	switch (iDBMS) {
	case DBMS_WATCOM:
		cmd = "select NOW(*) as CurrentTime";
		break;
	case DBMS_POSTGRES:
		cmd = "select LOCALTIMESTAMP as CurrentTime";
		break;
	case DBMS_SYBASE:
	case DBMS_MSSQL:
	default:
		cmd = "select GETDATE() as CurrentTime";
		break;
	case DBMS_ORACLE:
		cmd = "select sysdate as CurrentTime from dual";
		break;
	}
	if (cmd  &&  !gDBSelectOneNC(stmt, cmd)) {
		gFldGetDateTime(stmt, "CurrentTime", &dt, &tm);
		gDispose(stmt);
		return gNewDateTime(DateTime, dt, tm);
	}
	return gDispose(stmt);
}

static	long	Jul(long y, long m, long d)		/*  converts cal yyyymmdd to julian day	 */
{
	d += (long) (.5 + (m - 1L) * 30.57);
	if (m >	2L)  {
		d--;
/*  Had to convert to the following lines because of bug in MSVC 1.0
		if (0L != y % 400L  &&  (0L != y % 4L  ||  0L == y % 100L))
			d--;
*/
		if (0L != y % 400L  &&  0L != y % 4L)
			d--;
		else if (0L != y % 400L  &&  0L == y % 100L)
			d--;
	}
	d += (long) (365.25 * --y);
	d += y / 400L;
	d -= y / 100L;
	return(d);
}

static	void	Cal(long d, WORD *year, WORD *month, WORD *day)		/*  converts julian date to calander yyyymmdd */
{
	long	y, m, t;

	if (d <= 0L)		
		return;
	y = (long)(1.0 + d / 365.2425);
	t = y -	1L;
	d -= (long) (t * 365.25);
	d -= t / 400L;
	d += t / 100L;
	if (d >	59L  &&	 0L != y % 400L	 &&  (0L != y %	4  ||  0L == y % 100L))
		d++;
	if (d >	60L)	
		d++;
	m = (long)((d + 30L) / 30.57);
	d -= (long) floor(.5 + (m - 1L)	* 30.57);
	if (m == 13)  {
		m = 1;
		++y;
	}  else  if (!m)  {
		m = 12;
		--y;
	}
	*year = y;
	*month = m;
	*day = d;
}

static	long	toSeconds(SYSTEMTIME *st)
{
	return (Jul(st->wYear, st->wMonth, st->wDay) - Jul(1980, 1, 1)) * (24 * 60 * 60) + st->wHour * (60 * 60) + st->wMinute * 60 + st->wSecond;
}

static	void	toSystemTime(SYSTEMTIME *st, long s)
{
	long	days, hours, minutes;

	days = s / (24 * 60 * 60);
	s -= days * (24 * 60 * 60);
	days += Jul(1980, 1, 1);
	Cal(days, &st->wYear, &st->wMonth, &st->wDay);
	
	hours = s / (60 * 60);
	s -= hours * (60 * 60);
	st->wHour = hours;
	
	minutes = s / 60;
	s -= minutes * 60;
	st->wMinute = minutes;
	st->wSecond = s;
	st->wMilliseconds = 0;
	
	st->wDayOfWeek = days % 7;
	
	return;
}


#define	GET_HOURS(t)		(t / 10000000L)
#define	GET_MINUTES(t)		((t / 100000L) % 100L)
#define	GET_SECONDS(t)		((t / 1000L) % 100L)
#define	GET_MILLI(t)		(t % 1000)
#define	GET_YEAR(d)		(d / 10000L)
#define	GET_MONTH(d)		((d % 10000L) / 100)
#define	GET_DAY(d)		((d % 10000L) % 100)
#define	BUILD_TIME(h, m, s, l)	(((long) h * 10000000L) + ((long) m * 100000L) + ((long) s * 1000) + (long) l)
#define	BUILD_DATE(y,m,d)	((long) (y) * 10000L + (long) (m) * 100L + (long) d)

imeth	gNow()
{
	SYSTEMTIME	st;
	long	now, dt, tv;
	object	res;
	
	GetLocalTime(&st);
	now = toSeconds(&st);
	if (!iSysTime  ||  now < iSysTime  ||  now-iSysTime > 3500L) {
		iSysTime = now;
		res = pNow(self);
		gDateTimeValues(res, &dt, &tv);
		st.wSecond = GET_SECONDS(tv);
		st.wMinute = GET_MINUTES(tv);
		st.wHour = GET_HOURS(tv);
		st.wDay = GET_DAY(dt);
		st.wMonth = GET_MONTH(dt);
		st.wYear = GET_YEAR(dt);
		iDBTime = toSeconds(&st);
		return res;
	}
	now += iDBTime - iSysTime;
	toSystemTime(&st, now);
	dt = BUILD_DATE(st.wYear, st.wMonth, st.wDay);
	tv = BUILD_TIME(st.wHour, st.wMinute, st.wSecond, 0);
	return gNewDateTime(DateTime, dt, tv);
}

/*
	expiretime is amount of milliseconds after which we delete the previous mutex holder's hold
	maxwait is maximum wait time in milliseconds for getting a hold before giving up
	sleeptime is amount of time to wait between each check for getting a lock

	returns 0 on success, 1 on time out
*/


imeth	int	gCreateMutex(char *tag, char *data, long expiretime, long maxwait, long sleeptime)
{
	char	buf[1024];
	object	begtime=gNow(self);
	object stmt = gNewNonCacheStatement(self);

	gEnableWrites(stmt);
	gIgnoreAllErrors(stmt, 0);
	gEnableTP(stmt, 0);
	gReturnAllErrors(stmt, 0);
	gInsert(stmt, "dynace_mutex");
	while (1) {
		object	now, then;
		long	dv, tv, dv2, tv2;

		gFldSetString(stmt, "tag", tag);
		gFldSetString(stmt, "data", data ? data : "");
		now = gNow(self);
		gDateTimeValues(now, &dv, &tv);
		gFldSetDateTime(stmt, "lastupdate", dv, tv);
		if (!gAddRecord(stmt)) {
			gDispose(stmt);
			gDispose(begtime);
			gDispose(now);
			return 0;
		}

		//  mutex previously existed

		sprintf(buf, "select * from dynace_mutex where tag=~%s~", tag);
		if (gDBSelectOneNC(stmt, buf))
			continue;
		then = gFldGetValue(stmt, "lastupdate");
		gDateTimeDifference(now, then, &dv, &tv);
		if (dv  ||  tv > expiretime) {
			gDeleteRecord(stmt);
			continue;
		}

		gDateTimeDifference(now, begtime, &dv, &tv);
		if (dv  ||  tv > maxwait) {
			gDispose(stmt);
			gDispose(begtime);
			gDispose(now);
			return 1;  // timed out
		}

		Sleep(sleeptime);	
	}
}

imeth	int	gUpdateMutex(char *tag)
{
	char	buf[1024];
	object	stmt = gNewNonCacheStatement(self);
	int	r;

	gEnableWrites(stmt);
	gIgnoreAllErrors(stmt, 0);
	gEnableTP(stmt, 0);
	gReturnAllErrors(stmt, 0);
	sprintf(buf, "select * from dynace_mutex where tag = ~%s~", tag);
	if (!gDBSelectOneNC(stmt, buf)) {
		object	now = gNow(self);
		long	dv, tv;

		gDateTimeValues(now, &dv, &tv);
		gFldSetDateTime(stmt, "lastupdate", dv, tv);
		r = gUpdateRecord(stmt);
		gDispose(now);
	} else
		r = 100;  //  mutex not found
	gDispose(stmt);
	return r;
}

imeth	int	gDisposeMutex(char *tag)
{
	char	buf[1024];
	object	stmt = gNewNonCacheStatement(self);
	int	r;

	gEnableWrites(stmt);
	gIgnoreAllErrors(stmt, 0);
	gEnableTP(stmt, 0);
	gReturnAllErrors(stmt, 0);
	sprintf(buf, "delete from dynace_mutex where tag = ~%s~", tag);
	r = gExecute(stmt, buf);
	gDispose(stmt);
	return r;
}

imeth	long	gNextNumber(char *tag)
{
	char	mutex[256], select[256];
	long	num;
	object	stmt;

	sprintf(mutex, "Sequence - %s", tag);
	if (gCreateMutex(self, mutex, NULL, 60000, 60000, 250))
		return -1;  // error - can't create mutex lock
	stmt = gNewNonCacheStatement(self);
	sprintf(select, "select * from dynace_sequence where tag = ~%s~", tag);
	if (!gDBSelectOneNC(stmt, select)) {
		num = 1 + gFldGetLong(stmt, "lastvalue");
		gFldSetLong(stmt, "lastvalue", num);
		if (gUpdateRecord(stmt))
			num = -2;
	} else {
		num = 1;
		gInsert(stmt, "dynace_sequence");
		gFldSetString(stmt, "tag", tag);
		gFldSetLong(stmt, "lastvalue", num);
		if (gAddRecord(stmt))
			num = -3;
	}
	gDispose(stmt);
	gDisposeMutex(self, mutex);
	return num;
}

imeth	long	gSetLastNumber(char *tag, long num)
{
	char	mutex[256], select[256];
	object	stmt;

	sprintf(mutex, "Sequence - %s", tag);
	if (gCreateMutex(self, mutex, NULL, 60000, 60000, 250))
		return -1;  // error - can't create mutex lock
	stmt = gNewNonCacheStatement(self);
	sprintf(select, "select * from dynace_sequence where tag = ~%s~", tag);
	if (!gDBSelectOneNC(stmt, select)) {
		gFldSetLong(stmt, "lastvalue", num);
		if (gUpdateRecord(stmt))
			num = -2;
	} else {
		gInsert(stmt, "dynace_sequence");
		gFldSetString(stmt, "tag", tag);
		gFldSetLong(stmt, "lastvalue", num);
		if (gAddRecord(stmt))
			num = -3;
	}
	gDispose(stmt);
	gDisposeMutex(self, mutex);
	return num;
}

imeth	gSetHistoryTable(char *tbl)
{
	if (iFH_Table)
		iFH_Table = gDispose(iFH_Table);
	if (iFH_Tables)
		iFH_Tables = gDeepDispose(iFH_Tables);
	if (iSpecialFH_Tables)
		iSpecialFH_Tables = gDeepDispose(iSpecialFH_Tables);
	if (tbl) {
		iFH_Table = gNewWithStr(String, tbl);
		iFH_Tables = gNewWithInt(StringDictionary, 101);
	}
	/* special audit fields are not dependent on just one fixed table for storage of info */
	iSpecialFH_Tables = gNewWithInt(StringDictionary, 101);
	return self;
}

imeth	char	*gHistoryTable()
{
	return iFH_Table ? gStringValue(iFH_Table) : "";
}

imeth	gAuditField(char *tbl, char *fld, char *alias)
{
	object	to;
	char	buf[128];
	
	if (!iFH_Tables)
		return NULL;
	strcpy(buf, tbl);
	to = gFindValueStr(iFH_Tables, lcase(buf));
	if (!to) {
		to = gNew(StringDictionary);
		gAddStr(iFH_Tables, lcase(buf), to);
	}
	strcpy(buf, fld);
	gAddStr(to, lcase(buf), gNewWithStr(String, alias));
	return self;
}

imeth	gSetUserID(long userID)
{
	iUserID = userID;
	return self;
}

/* Returns the user name */
imeth	char*	gUserName()
{
	return gStringValue(iUserName);
}


imeth int gPreCacheGroup(int group,char *queryPart)
{
	return 0;
}

imeth int gClearCacheGroupQueries(int group)
{
	return 0;
} 

imeth ifun gSetSpecialAuditHandler(ifun fun)
{
	ifun org = iSpecialAuditHandler;
	iSpecialAuditHandler = fun;
	return org;
}

imeth	gSpecialAuditField(char *tbl, char *fld, char *alias)
{
	object	to;
	char	buf[128];
	
	if (!iSpecialFH_Tables)
		return NULL;
	strcpy(buf, tbl);
	to = gFindValueStr(iSpecialFH_Tables, lcase(buf));
	if (!to) {
		to = gNew(StringDictionary);
		gAddStr(iSpecialFH_Tables, lcase(buf), to);
	}
	strcpy(buf, fld);
	gAddStr(to, lcase(buf), gNewWithStr(String, alias));
	return self;
}





