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
#include <ctype.h>
#include "dynsql.h"

#if	!defined(_WIN32)  ||  defined(__WINE__)
#define	_int64	long
#endif

typedef	union {
	DATE_STRUCT		date;
	TIME_STRUCT		time;
	TIMESTAMP_STRUCT	ts;
	_int64			bigint;
}	DateTime_struct;


defclass  StatementInfo  {
	iStatement;
	int	iNumber;		//  column number
	iName;				//  field name
	char	*iPName;
	iValue;				//  actual field value
	void	*iPValue;		//  pointer to the value
	iOrgValue;			//  original field value
	int	iSize;			//  size of data item
	DateTime_struct	iDT;
	SWORD	iType;
	UDWORD	iPrecision;
	SWORD	iScale;
	SWORD	iNulls;
	SDWORD	iPCBValue;
	int	iDBMS;

	int	iSetToNull;

	char	*iXMLString;
	int	iXMLStringLen;

	iVarTextTable;
	long	iOrgVarTextID;
	iOrgVarText;
	iVarText;
 class:
	long	cMaxLongVarChar;
};



#ifndef	WIN32
#define SQL_WCHAR		(-8)
#define SQL_WVARCHAR	 	(-9)
#define SQL_WLONGVARCHAR 	(-10)
#define SQL_GUID		(-11)
#endif

private	imeth	pFormatField(object self, char *buf, int org, int fixDouble, int fixedlen);
private	imeth	SWORD	internal_type(object self);

cmeth	long	gSetMaxText(long val)
{
	long	org = cMaxLongVarChar;
	cMaxLongVarChar = val;
	return org;
}

cmeth	gNewSelect(stmt, HSTMT hstmt, int num, char *name, SWORD type,
		   UDWORD prec, SWORD scale, SWORD nulls, RETCODE *r, int dbms, int *size)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	if (!cMaxLongVarChar)
		cMaxLongVarChar = MAX_LONG_VARCHAR_LENGTH;
	iStatement = stmt;
	iDBMS = dbms;
	iNumber = num;
	iName = gNewWithStr(String, name);
	iPName = gStringValue(iName);
	iType = type;
	iPrecision = prec;
	iScale = scale;
	iNulls = nulls;
	type = internal_type(obj);
	switch (type) {
	case SQL_WLONGVARCHAR:
	case SQL_LONGVARCHAR:
		//  On Microsoft SQL Server this field is of type "TEXT"
		if (prec > cMaxLongVarChar  ||  prec < 1)
			prec = cMaxLongVarChar;
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
		iValue = gNewWithInt(String, (int) (prec+1));
		*size = iSize = prec + 1;
		iPValue = (void *) gStringValue(iValue);
//		iPCBValue = prec;
		iPCBValue = SQL_NTS;
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_CHAR, (PTR) iPValue, (SDWORD) (prec+1), &iPCBValue);
		break;
	case SQL_SMALLINT:
		iValue = gNew(ShortInteger);
		iPValue = gPointerValue(iValue);
		*size = iPCBValue = iSize = sizeof(short);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_SHORT, (PTR) iPValue, (SDWORD) sizeof(short), &iPCBValue);
		break;
	case SQL_INTEGER:
		iValue = gNew(LongInteger);
		iPValue = gPointerValue(iValue);
		*size = iPCBValue = iSize = sizeof(long);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_LONG, (PTR) iPValue, (SDWORD) sizeof(long), &iPCBValue);
		break;
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
		iValue = gNew(DoubleFloat);
		iPValue = gPointerValue(iValue);
		*size = iPCBValue = iSize = sizeof(double);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_DOUBLE, (PTR) iPValue, (SDWORD) sizeof(double), &iPCBValue);
		break;
	case SQL_BIT:
	case SQL_TINYINT:
		iValue = gNew(Character);
		iPValue = gPointerValue(iValue);
		*size = iPCBValue = iSize = sizeof(char);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_TINYINT, (PTR) iPValue, (SDWORD) sizeof(char), &iPCBValue);
		break;
	case SQL_DATE:
		iValue = gNew(Date);
		if (iDBMS != DBMS_ACCESS) {
			iDT.date.year = 1800;
//  minimum on Sybase SQL ANYWHERE is 100-01-01
//  minimum on Sybase SQL Server 1753-01-01
			iDT.date.month = 1;
			iDT.date.day = 1;
		}
		iPValue = (void *) &iDT;
		*size = iSize = sizeof(DATE_STRUCT);
//		iPCBValue = SQL_NULL_DATA;  //  no value yet
		iPCBValue = 0;
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_DATE, (PTR) iPValue, (SDWORD) sizeof(DATE_STRUCT), &iPCBValue);
		break;
	case SQL_TIME:
		iValue = gNew(Time);
		iPValue = (void *) &iDT;
		*size = iSize = sizeof(TIME_STRUCT);
//		iPCBValue = SQL_NULL_DATA;  //  no value yet
		iPCBValue = 0;
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_TIME, (PTR) iPValue, (SDWORD) sizeof(TIME_STRUCT), &iPCBValue);
		break;
	case SQL_TIMESTAMP:
		iValue = gNew(DateTime);
		if (iDBMS != DBMS_ACCESS) {
			iDT.ts.year = 1800;
			iDT.ts.month = 1;
			iDT.ts.day = 1;
		}
		iPValue = (void *) &iDT;
		*size = iSize = sizeof(TIMESTAMP_STRUCT);
//		iPCBValue = SQL_NULL_DATA;  //  no value yet
		iPCBValue = 0;
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_TIMESTAMP, (PTR) iPValue, (SDWORD) sizeof(TIMESTAMP_STRUCT), &iPCBValue);
		break;
	case SQL_BIGINT:
		iValue = gNew(LongInteger);
		iPValue = (void *) &iDT;
		*size = iSize = sizeof(_int64);
//		iPCBValue = SQL_NULL_DATA;  //  no value yet
		iPCBValue = 0;
//		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_SBIGINT, (PTR) iPValue, (SDWORD) sizeof(_int64), &iPCBValue);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_DEFAULT, (PTR) iPValue, (SDWORD) sizeof(_int64), &iPCBValue);
		break;
	case SQL_GUID:
		iValue = gNew(UniqueIdentifier);
		iPValue = gPointerValue(iValue);
		*size = iPCBValue = iSize = sizeof(GUID);
		*r = SQLBindCol(hstmt, (UWORD) num, SQL_C_GUID, (PTR) iPValue, (SDWORD) sizeof(GUID), &iPCBValue);
		break;
	case SQL_LONGVARBINARY:
		//  On Microsoft SQL Server this field is of type "IMAGE"
		//  Don't actually bind this column right now
		*r = SQL_SUCCESS;
		*size = 0;
		break;
	}
	if (*r != SQL_SUCCESS)
		obj = gDispose(obj);
		
	return obj;
}

cmeth	gNewInsert(stmt, HSTMT hstmt, int num, char *name, int type, int prec, RETCODE *r, int dbms)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	SWORD	scale = 0;

	iStatement = stmt;
	iDBMS = dbms;
	iNumber = num;
	iName = gNewWithStr(String, name);
	iPName = gStringValue(iName);
	iType = type;
	iPrecision = prec;
	type = internal_type(obj);
	switch (type) {
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
		iValue = gNewWithInt(String, (int) (iPrecision+1));
		iPValue = (void *) gStringValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_CHAR, iType, (UDWORD)(prec+1), scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_CHAR, iType,
				      (UDWORD) (prec+1), scale, (PTR) iPValue, (SDWORD) (prec+1), NULL);
#endif
		break;
	case SQL_SMALLINT:
		iValue = gNew(ShortInteger);
		iPValue = gPointerValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_SHORT, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_SHORT, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_INTEGER:
		iValue = gNew(LongInteger);
		iPValue = gPointerValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_LONG, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_LONG, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
		iValue = gNew(DoubleFloat);
		iPValue = gPointerValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_DOUBLE, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_DOUBLE, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_BIT:
	case SQL_TINYINT:
		iValue = gNew(Character);
		iPValue = gPointerValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_TINYINT, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_TINYINT, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_DATE:
		iValue = gNew(Date);
		if (iDBMS != DBMS_ACCESS) {
			iDT.date.year = 1800;
			iDT.date.month = 1;
			iDT.date.day = 1;
		}
		iPValue = (void *) &iDT;
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_DATE, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_DATE, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_TIME:
		iValue = gNew(Time);
		iPValue = (void *) &iDT;
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_TIME, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_TIME, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_TIMESTAMP:
		iValue = gNew(DateTime);
		iPValue = (void *) &iDT;
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_TIMESTAMP, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_TIMESTAMP, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_GUID:
		iValue = gNew(UniqueIdentifier);
		iPValue = gPointerValue(iValue);
#if ODBCVER < 0x0200
		*r = SQLSetParam(hstmt, (UWORD) num, SQL_C_GUID, iType, (UDWORD) prec, scale, (PTR) iPValue, NULL);
#else
		*r = SQLBindParameter(hstmt, (UWORD) num, SQL_PARAM_INPUT, SQL_C_GUID, iType,
				      (UDWORD) prec, scale, (PTR) iPValue, (SDWORD) 0, NULL);
#endif
		break;
	case SQL_LONGVARBINARY:
		//  Don't actually bind this column right now
		*r = SQL_SUCCESS;
		break;
	}
	if (*r != SQL_SUCCESS)
		obj = gDispose(obj);
		
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (iName)
		gDispose(iName);
	if (iValue)
		gDispose(iValue);
	if (iOrgValue)
		gDispose(iOrgValue);
	if (iXMLString)
		free(iXMLString);
	if (iVarText)
		gDispose(iVarText);
	if (iVarTextTable)
		gDispose(iVarTextTable);
	if (iOrgVarText)
		gDispose(iOrgVarText);
	return gDispose(super);
}

imeth	gClear : clear ()
{
	switch (internal_type(self)) {
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
		*(char *)iPValue = '\0';
		break;
	case SQL_SMALLINT:
		*(short *)iPValue = 0;
		break;
	case SQL_INTEGER:
		*(long *)iPValue = 0L;
		break;
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
		*(double *)iPValue = 0.0;
		break;
	case SQL_BIT:
	case SQL_TINYINT:
		*(char *)iPValue = '\0';
		break;
	case SQL_DATE:
		gChangeLongValue(iValue, 0L);
//		iPCBValue = SQL_NULL_DATA;  //  no value
		iPCBValue = 0;
		if (iDBMS != DBMS_ACCESS) {
			iDT.date.year = 1800;
			iDT.date.month = 1;
			iDT.date.day = 1;
		} else {
			iDT.date.year = 0;
			iDT.date.month = 0;
			iDT.date.day = 0;
		}
		break;
	case SQL_TIMESTAMP:
		gChangeDateTimeValues(iValue, 0L, 0L);
//		iPCBValue = SQL_NULL_DATA;  //  no value
		iPCBValue = 0;
		memset(&iDT, 0, sizeof iDT);
		if (iDBMS != DBMS_ACCESS) {
			iDT.ts.year = 1800;
			iDT.ts.month = 1;
			iDT.ts.day = 1;
		}
		break;
	case SQL_TIME:
		gChangeLongValue(iValue, 0L);
//		iPCBValue = SQL_NULL_DATA;  //  no value
		iPCBValue = 0;
		memset(&iDT, 0, sizeof iDT);
		break;
	case SQL_BIGINT:
		gChangeLongValue(iValue, 0L);
//		iPCBValue = SQL_NULL_DATA;  //  no value
		iPCBValue = 0;
		memset(&iDT, 0, sizeof iDT);
		break;
	case SQL_GUID:
//		memset(iPValue, 0, sizeof(GUID));
		break;
	}

	if (iOrgVarText)
		iOrgVarText = gDispose(iOrgVarText);
	if (iVarText)
		iVarText = gDispose(iVarText);
	if (iVarTextTable)
		iVarTextTable = gDispose(iVarTextTable);
	return iValue;
}

static	object	checkDoubleValue(char *name, object val, object stmt, int orig)
{
	if (_isnan(gDoubleValue(val))) {
		if (!gIgnoreNAN(Database, -1)) {
			FILE	*fp = fopen("math.err", "wt");
			char	buf[260];

			if (fp) {
				object	si, seq, obj;

				fprintf(fp, "NAN value found in one or more of the "
					"following %sfields:\n",
					orig ? "original " : "");
				for (seq = gSequence(gColumnDictionary(stmt)) ; obj = gNext(seq) ; ) {
					si = gValue(obj);
					pFormatField(si, buf, orig, 0, 0);
					fprintf(fp, "%-35s%s\n", gName(si), buf);
				}
				fclose(fp);
			}
			sprintf(buf, "Found NAN in field '%s'", name);
			gMessage(Application, buf);
		}
		gChangeDoubleValue(val, 0.00);
	}
	return val;
}

imeth	gGetInternalValue()
{
	return iValue;
}

imeth	gGetValue : getValue ()
{
	if (iPCBValue == SQL_NULL_DATA)
		clear(self);
	else if (iType == SQL_DATE) {
		if (iDT.date.year <= 1800)
			gChangeLongValue(iValue, 0L);
		else
			gChangeLongValue(iValue,
					 (long) iDT.date.year * 10000L + 
					 (long) iDT.date.month * 100L + 
					 (long) iDT.date.day); 
	} else if (iType == SQL_TIME)
		gChangeLongValue(iValue,
				 (long) iDT.time.hour * 10000000L + 
				 (long) iDT.time.minute * 100000L + 
				 (long) iDT.time.second * 1000L); 
	else if (iType == SQL_TIMESTAMP)
		if (iDT.ts.year <= 1800)
			gChangeDateTimeValues(iValue, 0L, 0L);
		else {
			gChangeDateValue(iValue,
					 (long) iDT.ts.year * 10000L + 
					 (long) iDT.ts.month * 100L + 
					 (long) iDT.ts.day);
			gChangeTimeValue(iValue,
					 (long) iDT.ts.hour * 10000000L +
					 (long) iDT.ts.minute * 100000L +
					 (long) iDT.ts.second * 1000L +
					 (long) iDT.ts.fraction / 1000000L);  // fraction is stored to 6 places
		}
	else if (iType == SQL_BIGINT)
		gChangeLongValue(iValue, (long) iDT.bigint);
	else if (internal_type(self) == SQL_REAL  ||  internal_type(self) == SQL_FLOAT  ||  internal_type(self) == SQL_DOUBLE)
		checkDoubleValue(gStringValue(iName), iValue, iStatement, 0);

	return iValue;
}

imeth	gGetValueToPut : valueToPut ()
{
	if (iType == SQL_CHAR  ||  iType == SQL_VARCHAR  ||  iType == SQL_LONGVARCHAR  ||
	    iType == SQL_WCHAR  ||  iType == SQL_WVARCHAR  ||  iType == SQL_WLONGVARCHAR)
		iPCBValue = SQL_NTS;
	else
		iPCBValue = iSize;
	return iValue;
}

imeth	gUpdate : update ()
{
	long	val, val2;
	int	tmp;

	if (iType == SQL_DATE) {
		val = gLongValue(iValue);
		if (iDBMS != DBMS_ACCESS) {
			if (val < 18000101L)
				val = 18000101L;  //  use a substitute to a zero date
		}
		iDT.date.year = (SWORD) (val / 10000L);
		tmp = (int) (val % 10000L);
		iDT.date.month = (UWORD) (tmp / 100);
		iDT.date.day = (UWORD) (tmp % 100);
		iPCBValue = val ? 0 : SQL_NULL_DATA;
	} else if (iType == SQL_TIME) {
		val = gLongValue(iValue);
		iDT.time.hour = (SWORD) (val / 10000000L);
		tmp = (int) (val % 10000000L);
		iDT.time.minute = (UWORD) (tmp / 100000L);
		tmp = (int) (tmp % 100000L);
		iDT.time.second = (UWORD) (tmp / 1000L);
		iPCBValue = val ? 0 : SQL_NULL_DATA;
	} else if (iType == SQL_TIMESTAMP) {
		gDateTimeValues(iValue, &val, &val2);
		if (iDBMS != DBMS_ACCESS) {
			if (val < 18000101L)
				val = 18000101L;  //  use a substitute to a zero date
		}
		iDT.ts.year = (SWORD) (val / 10000L);
		tmp = (int) (val % 10000L);
		iDT.ts.month = (UWORD) (tmp / 100);
		iDT.ts.day = (UWORD) (tmp % 100);
		iDT.ts.hour = (SWORD) (val2 / 10000000L);
		tmp = (int) (val2 % 10000000L);
		iDT.ts.minute = (UWORD) (tmp / 100000L);
		tmp = (int) (tmp % 100000L);
		iDT.ts.second = (UWORD) (tmp / 1000L);
		iDT.ts.fraction = (UDWORD) ((tmp % 1000L) * 1000000L);
		iPCBValue = val  ||  val2 ? 0 : SQL_NULL_DATA;
	} else if (iType == SQL_CHAR  ||  iType == SQL_VARCHAR  ||  iType == SQL_LONGVARCHAR  ||
		   iType == SQL_WCHAR  ||  iType == SQL_WVARCHAR  ||  iType == SQL_WLONGVARCHAR) {
		if (iDBMS == DBMS_ACCESS) {
			char	*p = (char *) iPValue;
			if (p  &&  !*p) {
				p[0] = ' ';
				p[1] = '\0';
			}
		}
		iPCBValue = SQL_NTS;
	} else if (iType == SQL_BIGINT) {
		iDT.bigint = (_int64) gLongValue(iValue);
		iPCBValue = 0;
	} else
		iPCBValue = iSize;
	return self;
}

imeth	void	*gPointerValue()
{
	return iPValue;
}

imeth	int	gSize()
{
	return (int) iPrecision;
}

imeth	int	gSQLType()
{
	return (int) internal_type(self);
}

imeth	int	gAllowNulls()
{
	return (int) iNulls;
}

imeth	char	*gName()
{
	return iName ? gStringValue(iName) : "";
}

imeth	char	*gStringValue()
{
	return gStringValue(getValue(self));
}

private	imeth	char	*pExtend(char *p)
{
	int	off = p - iXMLString;
	iXMLStringLen += iXMLStringLen / 4;
	iXMLString = realloc(iXMLString, iXMLStringLen);
	return iXMLString + off;
}

imeth	char	*gStringValueXML()
{
	char	*p, *from;
	int	len;

	getValue(self);
	if (iValue  &&  (ClassOf(iValue) == LongInteger  ||  ClassOf(iValue) == ShortInteger)) {
		if (!iVarText)
			return NULL;
		from = gStringValue(iVarText);
		len = strlen(from) + 20;
	} else {
		from = (char *) iPValue;
		len = iSize;
	}
	if (!iXMLString) {
		iXMLStringLen = len + len / 4;
		if (iXMLStringLen < 50)
			iXMLStringLen = 50;
		iXMLString = malloc(iXMLStringLen);
	}
	
	for (p=iXMLString ; *from ; from++) {
		if (p-iXMLString+7 >= iXMLStringLen)
			p = pExtend(self, p);
		switch (*from) {
		case '\'':
			strcpy(p, "&apos;");
			p += 6;
			break;
		case '"':
			strcpy(p, "&quot;");
			p += 6;
			break;
		case '&':
			strcpy(p, "&amp;");
			p += 5;
			break;
		case '<':
			strcpy(p, "&lt;");
			p += 4;
			break;
		case '>':
			strcpy(p, "&gt;");
			p += 4;
			break;
		case '\t':
		case '\r':
		case '\n':
			*p++ = ' ';
			break;
		default:
			*p++ = *from >= ' '  &&  *from <= 126 ? *from : ' ';
			break;
		}
	}
	*p = '\0';
	return iXMLString;
}

imeth	char	gCharValue()
{
	return gCharValue(getValue(self));
}

imeth	short	gShortValue()
{
	return gShortValue(getValue(self));
}

imeth	unsigned short	gUnsignedShortValue()
{
	return gUnsignedShortValue(getValue(self));
}

imeth	long	gLongValue()
{
	return gLongValue(getValue(self));
}

imeth	double	gDoubleValue()
{
	return gDoubleValue(getValue(self));
}

imeth	gDateTimeValues(long *dt, long *tm)
{
	gDateTimeValues(getValue(self), dt, tm);
	return self;
}

imeth	gCopyCorresponding(from)
{
//	ChkArgTyp(from, 2, CLASS);
	if (iType != SQL_GUID)
		if (gIsKindOf(iValue, Number) == gIsKindOf(gGetInternalValue(from), Number)) {
			gChangeValue(valueToPut(self), gGetValue(from));
			update(self);
		}
	return self;
}


imeth	gCopy, gDeepCopy ()
{
	object	obj = gCopy(super);
	ivType	*iv2 = ivPtr(obj);
	if (iName)
		iv2->iName= gCopy(iName);
	if (iValue)
		iv2->iValue = gCopy(iValue);
	switch (internal_type(self)) {
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
		iPValue = (void *) gStringValue(iv2->iValue);
		break;
	case SQL_DATE:
	case SQL_TIME:
	case SQL_TIMESTAMP:
	case SQL_BIGINT:
		iPValue = (void *) &iv2->iDT;
		break;
	default:
		if (iv2->iValue)
			iPValue = gPointerValue(iv2->iValue);
		break;
	}
	return obj;
}

imeth	gSetNull()
{
	iSetToNull = 1;
	return self;
}

imeth	gSetNotNull()
{
	iSetToNull = 0;
	return self;
}

imeth	int	gIsNull()
{
	return iSetToNull;
}

private	imeth	pFormatField(object self, char *buf, int org, int fixDouble, int fixedlen)
{
	object	fval = org ? iOrgValue : iValue;
	if (!fval  &&  internal_type(self) != SQL_GUID  &&  iType != SQL_LONGVARBINARY)
		vError(self, "No original value for field %s", iPName ? iPName : "");
	if (iSetToNull) {
		strcpy(buf, "NULL");
		return self;
	}
	switch (internal_type(self)) {
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
		if (fixedlen)
			sprintf(buf, "%-*.*s", iPrecision, iPrecision, gStringValue(fval));
		else
			sprintf(buf, "~%s~", gStringValue(fval));
		break;
	case SQL_SMALLINT:
		if (fixedlen)
			sprintf(buf, "%06d", (int) gShortValue(fval));
		else
			sprintf(buf, "%d", (int) gShortValue(fval));
		break;
	case SQL_INTEGER:
		if (fixedlen)
			sprintf(buf, "%011ld", gLongValue(fval));
		else
			sprintf(buf, "%ld", gLongValue(fval));
		break;
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
		if (fixDouble)
			sprintf(buf, "%f", gDoubleValue(checkDoubleValue(gStringValue(iName), fval, iStatement, org)));
		else if (_isnan(gDoubleValue(fval)))
			strcpy(buf, "#NAN");
		else
			sprintf(buf, "%f", gDoubleValue(fval));
		break;
	case SQL_TIME: {
		object	obj, val, obj2;
		long	tm;
		
		if (org)
			tm = gLongValue(val=fval);
		else {
			val = getValue(self);
			tm = gLongValue(val);
		}
		obj2 = tm ? val : gNewWithLong(Time, 0L);
		if (fixedlen)
			sprintf(buf, "%s", gStringValue(obj=gFormatTime(obj2, "%G%M%S%L")));
		else
			sprintf(buf, "~%s~", gStringValue(obj=gFormatTime(obj2, "%G:%M:%S.%L")));
		gDispose(obj);
		if (!tm)
			gDispose(obj2);
		break;
	}
	case SQL_DATE: {
		object	obj, val, obj2;
		long	dt;
		
		if (org)
			dt = gLongValue(val=fval);
		else {
			val = getValue(self);
			dt = gLongValue(val);
		}
		if (iDBMS != DBMS_ACCESS)
			obj2 = dt ? val : gNewWithLong(Date, 18000101L);
		else
			obj2 = dt ? val : gNewWithLong(Date, 0L);
		
		if (fixedlen)
			sprintf(buf, "%s", gStringValue(obj=gFormatDate(obj2, "%Y%N%D")));
		else if (iDBMS == DBMS_ORACLE)
			sprintf(buf, "TO_DATE(~%s~, 'YYYY-MM-DD')", gStringValue(obj=gFormatDate(obj2, "%Y-%N-%D")));
		else
			sprintf(buf, "~%s~", gStringValue(obj=gFormatDate(obj2, "%Y-%N-%D")));
		
		gDispose(obj);
		if (!dt)
			gDispose(obj2);
		break;
	}
	case SQL_TIMESTAMP: {
		object	dobj, tobj, val, dobj2, tobj2;
		long	dt, tm;
		
		if (org)
			gDateTimeValues(val=fval, &dt, &tm);
		else {
			val = getValue(self);
			gDateTimeValues(val, &dt, &tm);
		}
		if (iDBMS != DBMS_ACCESS)
			dobj2 = dt ? val : gNewWithLong(Date, 18000101L);
		else
			dobj2 = dt ? val : gNewWithLong(Date, 0L);

		tobj2 = tm ? val : gNewWithLong(Time, 0L);

		if (fixedlen) {
			dobj=gFormatDate(dobj2, "%Y%N%D");
			tobj=gFormatTime(tobj2, "%G%M%S%L");
			sprintf(buf, "%s %s", gStringValue(dobj), gStringValue(tobj));
		} else {
			dobj=gFormatDate(dobj2, "%Y-%N-%D");
	
			if (iDBMS == DBMS_ORACLE)
				tobj=gFormatTime(tobj2, "%G:%M:%S");
			else
				tobj=gFormatTime(tobj2, "%G:%M:%S.%L");
	
			// we need this to avoid a space after the date if the time value is 0
			if (gLongValue(tobj2) == 0) 
				sprintf(buf, "~%s~", gStringValue(dobj));
			else
				sprintf(buf, "~%s %s~", gStringValue(dobj), gStringValue(tobj));
		}
		gDispose(dobj);
		gDispose(tobj);
		if (!dt)
			gDispose(dobj2);
		if (!tm)
			gDispose(tobj2);
		break;
	}
	case SQL_BIGINT: {
		object	val, obj2;
		long	dt;
		
		if (org)
			dt = gLongValue(val=fval);
		else {
			val = getValue(self);
			dt = gLongValue(val);
		}
		obj2 = dt ? val : gNewWithLong(LongInteger, 0L);
		sprintf(buf, "%ld", gLongValue(obj2));
		if (!dt)
			gDispose(obj2);
		break;
	}
	case SQL_GUID:
		strcpy(buf, "newID()");
		break;
	case SQL_LONGVARBINARY:
		strcpy(buf, "NULL");
		break;
	default:
		*buf = '\0';
		break;
	}
	return self;
}

imeth	gFormatField(char *buf)
{
	return pFormatField(self, buf, 0, 1, 0);
}

imeth	char	*gFormatFixedLengthField(char *buf)
{
	pFormatField(self, buf, 0, 1, 1);
	return buf;
}

imeth	gFormatOrgField(char *buf)
{
	return pFormatField(self, buf, 1, 1, 0);
}

imeth	int	gHasData()
{
	object	fval = iValue;
	int	chg = 0;

	switch (internal_type(self)) {
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:  {
		char	*p = gStringValue(fval);
		for (; *p ; p++)
			if (!isspace(*p)) {
				chg = 1;
				break;
			}
		break;
	}
	case SQL_SMALLINT:
		chg = !!gShortValue(fval);
		break;
	case SQL_INTEGER:
		chg = !!gLongValue(fval);
		break;
	case SQL_TIME:
	case SQL_DATE:
		chg = !!gLongValue(getValue(self));
		break;
	case SQL_TIMESTAMP: {
		long	dt, tm;

		gDateTimeValues(getValue(self), &dt, &tm);
		chg = !!dt  ||  !!tm;
		break;
	}
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:	
	case SQL_BIGINT:
	case SQL_GUID:
	case SQL_LONGVARBINARY:
		break;
	}
	return chg;
}

imeth	gUpdateOriginalValue()
{
	if (iType == SQL_DATE  ||  iType == SQL_TIME  ||  iType == SQL_TIMESTAMP  ||  iType == SQL_BIGINT)
		getValue(self);

	if (!iOrgValue && iValue)
		iOrgValue = gCopy(iValue);
	else if (iValue)
		gChangeValue(iOrgValue, iValue);
	else if (iOrgValue)
		iOrgValue = gDispose(iOrgValue);
	
	if (iOrgVarText)
		iOrgVarText = gDispose(iOrgVarText);
	if (iVarText)
		iVarText = gDispose(iVarText);
	if (iVarTextTable)
		iVarTextTable = gDispose(iVarTextTable);
	return self;
}

imeth	gRestoreOriginalValue()
{
	if (!iOrgValue) {
		clear(self);
		return self;
	}
	
	if (!iValue)
		iValue = gCopy(iOrgValue);
	else
		gChangeValue(iValue, iOrgValue);
	if (iOrgVarText)
		iOrgVarText = gDispose(iOrgVarText);
	if (iVarText)
		iVarText = gDispose(iVarText);
	if (iVarTextTable)
		iVarTextTable = gDispose(iVarTextTable);
	return self;
}

imeth	gOriginalValue()
{
	return iOrgValue;
}

imeth	gStatement()
{
	return iStatement;
}

imeth	gClass()
{
	if (iValue)
		return ClassOf(iValue);
	return NULL;
}

imeth	int	gVarTextChanged : varTextChanged ()
{
	if (!iOrgVarText || !iVarText)
		return 0;

	return gCompare(iOrgVarText, iVarText);
}

imeth	gGetColVarText()
{
	return iVarText;
}

imeth	gGetOrgVarText()
{
	return iOrgVarText;
}

imeth	gSetColVarText(char *str, char *vttbl)
{
	long	id = gLongValue(iValue);
	object	vtobj = NULL;

	if (iVarTextTable)
		gChangeStrValue(iVarTextTable, vttbl);
	else
		iVarTextTable = gNewWithStr(String, vttbl);

	if (str) {
		if (iVarText)
			gChangeStrValue(iVarText, str);
		else
			iVarText = gNewWithStr(String, str);
	} else {
		vtobj = gGetVarText(gDatabase(iStatement), vttbl, id);
		if (iVarText)
			gChangeStrValue(iVarText, gStringValue(vtobj));
		else
			iVarText = gCopy(vtobj);
	}

	if (!iOrgVarText && id > 0L) {
		if (vtobj)
			iOrgVarText = gCopy(vtobj);
		else
			iOrgVarText = gGetVarText(gDatabase(iStatement), vttbl, id);
	} else if (!iOrgVarText)
		iOrgVarText = gNew(String);

	if (vtobj)
		gDispose(vtobj);
	
	return self;
}

imeth	gWriteVarText()
{
	if (varTextChanged(self)) {
		long	len = gLength(iVarText);
		object	db = gDatabase(iStatement);
		char	*vtTable = gStringValue(iVarTextTable);

		iOrgVarTextID = gLongValue(iValue);
		
		if (iOrgVarTextID <= 0L  &&  len)
			gChangeLongValue(valueToPut(self), gAddVarText(db, vtTable, gStringValue(iVarText)));
		else if (iOrgVarTextID > 0L  &&  len)
			gUpdateVarText(db, vtTable, iOrgVarTextID, gStringValue(iVarText));
		else if (iOrgVarTextID > 0L  && !len) {
			gDeleteVarText(db, vtTable, iOrgVarTextID);
			gChangeLongValue(valueToPut(self), 0L);
		}
	}
	
	return self;
}

imeth	gUndoVarText()
{
	if (varTextChanged(self)) {
		long	id = gLongValue(iValue);
		object	db = gDatabase(iStatement);
		char	*vtTable = gStringValue(iVarTextTable);

		if (iOrgVarTextID <= 0L  &&  id > 0L)
			gDeleteVarText(db, vtTable, id);
		else if (iOrgVarTextID > 0L)
			gUpdateVarText(db, vtTable, iOrgVarTextID, gStringValue(iOrgVarText));
	}

	return self;
}

imeth	int	gCursorWrite(file)
{
	if (iType == SQL_LONGVARBINARY)
		return 1;
	getValue(self);  //  make sure iValue is correct
	return iValue ? iSize == gWrite(file, (char *) iPValue, iSize) : 1;
}

imeth	int	gCursorRead(file)
{
	if (iType == SQL_LONGVARBINARY)
		return 1;
	valueToPut(self);  //  update iPCBValue
	return iValue ? iSize == gRead(file, (char *) iPValue, iSize) : 1;
}

private	imeth	SWORD	internal_type()
{
	if (iType == SQL_DECIMAL  ||  iType == SQL_NUMERIC)
		if (iScale == 0)
			if (iPrecision < 5  ||  iPrecision == 5  &&  iDBMS == DBMS_ORACLE)
				return SQL_SMALLINT;
			else
				return SQL_INTEGER;
		else
			return SQL_DOUBLE;
	else
		return iType;
}

imeth	int	gType()
{
	return iType;
}

imeth	int	gDecimalPlaces()
{
	return iScale;
}

imeth	int	gNumber()
{
	return iNumber;
}





