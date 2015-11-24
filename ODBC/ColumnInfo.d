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



#include <windows.h>
#include <sql.h>
#include <sqlext.h>


defclass  ColumnInfo {
	int	iNum;
	iName;
	char	*iPName;
	iTypeName;
	int	iType;
	int	iLen;
	int	iNullable;
	int	iScale;
};


cvmeth	vNew(char *name, int type, char *typebuf, SDWORD len, int nullable, int n, int scale)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iNum  = n;
	iName = gNewWithStr(String, name);
	iPName = gStringValue(iName);
	iTypeName = gNewWithStr(String, typebuf);
	iType = type;
	iLen  = len;
	iNullable = nullable;
	iScale = scale;
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	gDispose(iName);
	gDispose(iTypeName);
	return gDispose(super);
}

imeth	char	*gName()
{
	return iPName;
}

imeth	char	*gTypeName()
{
	return gStringValue(iTypeName);
}

imeth	int	gType()
{
	return iType;
}

#ifndef	WIN32
#define SQL_WCHAR		(-8)
#define SQL_WVARCHAR	 	(-9)
#define SQL_WLONGVARCHAR 	(-10)
#endif

static	object	getDynaceType(int type)
{
	object	rval = NULL;
	
	switch (type) {
	case SQL_WLONGVARCHAR:
	case SQL_LONGVARCHAR:
	case SQL_WCHAR:
	case SQL_WVARCHAR:
	case SQL_CHAR:
	case SQL_VARCHAR:
		rval = String;
		break;
	case SQL_SMALLINT:
		rval = ShortInteger;
		break;
	case SQL_INTEGER:
		rval = LongInteger;
		break;
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
		rval = DoubleFloat;
		break;
	case SQL_BIT:
	case SQL_TINYINT:
		rval = Character;
		break;
	case SQL_DATE:
		rval = Date;
		break;
	case SQL_TIME:
		rval = Time;
		break;
	case SQL_TIMESTAMP:
		rval = DateTime;
		break;
	}
	return rval;
}

imeth	gClass()
{
	return getDynaceType(iType);
}

imeth	int	gSize()
{
	return iLen;
}

imeth	int	gNumber()
{
	return iNum;
}

imeth int gColumnScale()
{
	return iScale;
}

imeth	gGetColValues(char **cname, int *n, int *type, int *len)
{
	*cname = iPName;
	*n = iNum;
	*type = iType;
	*len = iLen;
	return self;
}

imeth	int	gIsNullable()
{
	return iNullable;
}






