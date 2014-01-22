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
#include <string.h>


defclass  TableInfo {
	iName;
	char	*iPName;
	int	iType;
	int	iNumCols;
	iColumns;		//  StringDictionary of column names
	iLLColumns;		//  Linked list of column names
	iOwner;			//  String containing the table owner

	iDatabase;
};

private	imeth	pLoadTableInfo(object self);
	
extern	char	*lcase(char *v);

private	imeth	pInitInstance(object self, object db, char *tname, int ttype, int lazyload)
{
	iDatabase = db;
	iName = gNewWithStr(String, tname);
	iPName = gStringValue(iName);
	iType = ttype;

	if (!lazyload)
		if (!pLoadTableInfo(self))
			return gDispose(self);
	
	return self;
}

cvmeth	vNew(HENV env, object db, char *tname, int ttype, int lazyload)
{
	return pInitInstance(gNew(super), db, tname, ttype, lazyload);
}

private	imeth	pLoadTableInfo(object self)
{
	object	rval = self;
	HDBC	dbc = gHDBC(iDatabase);
	HSTMT	stmt;
	RETCODE	r;
	char	name[130];
	char	owner[130];
	char	typebuf[256];
	SWORD	type;
	SDWORD	len;
	SWORD	scale;
	SWORD	nullable;
	object	ci;
	int	nflds = 0;
	int	dbtype = gDBMS_type(iDatabase);
	char*	schema = dbtype == DBMS_ORACLE ? gUserName(iDatabase) : 0;
	
	gEnterCriticalSection(iDatabase);
	r = SQLAllocStmt(dbc, &stmt);
	r = SQLColumns(stmt, NULL, 0, schema, (SQLSMALLINT)(schema ? SQL_NTS : 0), iPName, SQL_NTS, NULL, 0);

	if (r == SQL_SUCCESS) {
		long	pcbValueOwner;
		long	pcbValueTypeName;
		long	pcbValueLength;
		long	pcbValueScale;
		long	pcbValueNullable;

		iColumns = gNewWithInt(StringDictionary, 101);
		iLLColumns = gNew(LinkObject);
		r = SQLBindCol(stmt,  2, SQL_C_CHAR, owner, sizeof(owner) - 1, &pcbValueOwner);
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
					if (len < 5  ||  len == 5  &&  gDBMS_type(iDatabase) == DBMS_ORACLE)
						type = SQL_SMALLINT;
					else 
						type = SQL_INTEGER;

			if (!iOwner && pcbValueOwner != SQL_NULL_DATA)
				iOwner = gNewWithStr(String, owner);

			if (dbtype = DBMS_POSTGRES  &&  type == -1  &&  !strcmp(typebuf, "varchar"))
				type = SQL_VARCHAR;

			ci = vNew(ColumnInfo, name, type, typebuf, len, nullable, ++iNumCols, scale);
			gAddStr(iColumns, lcase(name), ci);
			gAddLast(iLLColumns, ci);
			nflds++;
		}

		if (!iOwner)
			iOwner = gNew(String);
		r = SQLFreeStmt(stmt, SQL_UNBIND);
	}
	if (!nflds)
		rval = NULL;
	r = SQLFreeStmt(stmt, SQL_DROP);

	gLeaveCriticalSection(iDatabase);
	return rval;
}

imeth	gDispose, gDeepDispose ()
{
	gDispose(iName);
	if (iColumns) {
		gDeepDispose(iColumns);
		gDispose(iLLColumns);
	}
	if (iOwner)
		gDispose(iOwner);
	return gDispose(super);
}

imeth	char	*gName()
{
	return iPName;
}

imeth object gGetOwner()
{
	if (!iColumns)
		pLoadTableInfo(self);
	return iOwner;
}

imeth	int	gType()
{
	return iType;
}

imeth	int	gSize()
{
	if (!iColumns)
		pLoadTableInfo(self);
	return iNumCols;
}

imeth	gFindStr(char *field)
{
	char	name[130];

	if (!iColumns)
		pLoadTableInfo(self);
	strcpy(name, field);
	return gFindValueStr(iColumns, lcase(name));
}

#define	GO_END	while (*buf)	buf++
#define	ADD(x)	{  strcpy(buf, x); GO_END;  }

imeth	gMakeInsert(char *buf)
{
	int	n;
	object	seq, ci;

	if (!iColumns)
		pLoadTableInfo(self);
	sprintf(buf, "insert into %s (", iPName);
	GO_END;
	for (n=0, seq=gSequence(iLLColumns) ; ci=gNext(seq) ; ) {
		if (n++) {
			*buf++ = ',';
			*buf++ = ' ';
		}
		ADD(gName(ci));
	}
	ADD(") values (");
	while (n-- > 0) {
		*buf++ = '?';
		*buf++ = ',';
	}
	*(buf-1) = ')';
	*buf = '\0';
	return self;
}

imeth	gBindColumns(stmt, HSTMT hstmt, cols)
{
	object	si, seq, ci;
	char	*cname;
	int	n, type, len;
	RETCODE	r;

	if (!iColumns)
		pLoadTableInfo(self);
	for (seq=gSequence(iLLColumns) ; ci=gNext(seq) ; ) {
		gGetColValues(ci, &cname, &n, &type, &len);
		si = gNewInsert(StatementInfo, stmt, hstmt, n, cname, type, len, &r, 0);
		if (si)
			gAddStr(cols, cname, si);
	}
	return self;
}

imeth	gColumns()
{
	if (!iColumns)
		pLoadTableInfo(self);
	return iColumns;
}

imeth	gAddColumn(char *owner, char *name, SWORD type, char *typebuf, SDWORD len, SWORD scale, SWORD nullable)
{
	object	col = NULL;
	char	lcname[50];

	strcpy(lcname, name);
	lcase(lcname);

	if (!iColumns) {
		iColumns = gNewWithInt(StringDictionary, 101);
		iLLColumns = gNew(LinkObject);
	}
	if (type == -1  &&  gDBMS_type(iDatabase) == DBMS_POSTGRES  &&  !strcmp(typebuf, "varchar"))
		type = SQL_VARCHAR;

	if (!gFindStr(iColumns, lcname)) {
		col = vNew(ColumnInfo, name, type, typebuf, len, nullable, ++iNumCols, scale);
		gAddStr(iColumns, lcname, col);
		gAddLast(iLLColumns, col);
	
		if (!iOwner && owner)
			iOwner = gNewWithStr(String, owner);
	}
	
	return col;
}






