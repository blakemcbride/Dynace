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
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef unix
#include <unistd.h>
#define	O_BINARY	0
#else
#include <io.h>
#endif
#include <sys/locking.h>
#include <stddef.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <fcntl.h>
#include <winsock.h>

#include "TransactionProcessing.h"

#define	LOCK_BYTES	100000000L


defclass TransactionProcessing {
	char	iMode;		/*  R or W	*/
	int	iFlushMode;	/*  1, 2 or 3	*/
	object	iTFile;		/*  transaction file name  */
	FILE	*iFP;		/*  transaction file handle  */
	object	iSFile;		/*  StringFile object  */

	int	iBuflen;
	int	iBufused;
	char	*iBuf;

	long	*(*iRouteFun)(char *tableName);

	Head	iH;

	/*  selection criteria  */

	long	iDT;		/*  date YYYYMMDD  	*/
	int	iTime;		/*  time HHMM		*/
	
	iOutputStreamObjects;
};


#define	TP_VERSION	2
#define	VERSION		1

static	char	AddTransCmd[] = "addtransaction";
static	int		AddTransCmdLen;

static	char	what_type(object cls, object si);
static	long	what_size(object cls, void *p, int varText);
static	int	is_zero(object val, void *p);
static	char	*make_date(long t);
static	void	write_value(ivType *iv, object val, void *p, int flg, int varText, object stream);
static	int	display_value(ivType *iv, FILE *fp, Field f, int n, char *RE, int change, object stream);
static	int	set_field(ivType *iv, Field f, char *field, char *RE, int change, object stmt, int exists);
static	object	make_object(ivType *iv, Field f, char *RE, int change, int exists);
static	void	formatField(int type, char *buf, object fval);

extern	char	*Getenv(char *);

private imeth	int	pCommitOutputStreams(object self, object dataBuffStream);
private	imeth	int	pWriteHostInt(int val, object stream);
private	imeth	void pCheckServer();
private	imeth	pConnectToServer(char *server);

typedef	unsigned long  int uint32_t;

private	imeth	int	pWriteHostInt(int val, object stream)
{
	uint32_t network_val;

	network_val = htonl(val);
	return gWrite(stream, (char *)&network_val, sizeof(network_val));
}

private	imeth	pConnectToServer(char *server)
{
		int listenPort = 9966;
		char *colonpos;		
		object socketstream;
		
		if (!server || *server == '\0')
			gError(Object, "gNewTP: Invalid DataSync Server Name");
		
		colonpos = strchr(server, ':');
		if (colonpos) {
			*colonpos = '\0';
			listenPort = atoi(colonpos + 1);
		}		
		
		socketstream =  gSocketConnect(Socket, server, listenPort, 0);
		if (!socketstream)
			gError(Object, "gNewTP: Unable to connect to Datasync Server");
		
		return socketstream;
}

private imeth	int	pCommitOutputStreams(object dataBuffStream)
{
	object s;
	object obj;
	int writebuflen;
	
	if (!gLength(dataBuffStream))
		return 1;
	
	writebuflen = gLength(dataBuffStream);
	for (s=gSequence(iOutputStreamObjects) ; obj = gNext(s) ; ) {
		if (ClassOf(obj) == File) {
			gTPLock(CLASS, gPointerValue(obj));
			if (gWrite(obj, gStringValue(dataBuffStream), writebuflen) != writebuflen)
			{
				gTPUnlock(CLASS, gPointerValue(obj));
				return 0;
			}
			gFlush(obj);
			_commit(_fileno((FILE *)gPointerValue(obj)));
			gTPUnlock(CLASS, gPointerValue(obj));
		}
		if (ClassOf(obj) == Socket)	{
			int readlen;
			char donebuf[80];
			object outbuf;

			*donebuf = '\0';
			//build buffer with write command, buffer len, buffer data
			outbuf = gNew(String);
			pWriteHostInt(self, AddTransCmdLen, outbuf);
			gWrite(outbuf, AddTransCmd, AddTransCmdLen);
			pWriteHostInt(self, writebuflen, outbuf);
			gWrite(outbuf, gStringValue(dataBuffStream), writebuflen);
			
			//write buffer to socket
			if (!gWrite(obj, gStringValue(outbuf), gLength(outbuf)) == gLength(outbuf))
				gError(Object, "Write Error in DataSync server");
			
			//read response and check
			if (!gRead(obj, (char *)&readlen, sizeof(readlen)) == sizeof(readlen))
				gError(Object, "Read Error in DataSync server");
			
			readlen = ntohl(readlen);
			if (readlen > 100 || readlen < 0) 
				gError(Object, "Read Length Error in DataSync server");

			if (!gRead(obj, donebuf, readlen) == readlen)	
				gError(Object, "Read Error in DataSync server");
			
			donebuf[readlen] = '\0';
			if (strcmp(donebuf, "DataSync Write Ok"))
				gError(Object, "Error writing transaction to DataSync server");
			gDispose(outbuf);
		}
	}
	return 1;
}

cmeth	gNewTP(char type, char *file, long station, long user)
{
	char	buf[256], envBuf[512], *envptr;
	int		useTranFile;
	object	obj = gNew(super);
	accessIVsOf(obj);
	useTranFile = 1;
	iOutputStreamObjects = gNew(LinkObject);
	
	AddTransCmdLen = strlen(AddTransCmd);
	if (type == 'W'  ||  type == 'w')
		iMode = 'W';
	else
		iMode = 'R';
	
	envptr = Getenv("DATASYNC_SERVER_HOST");
	
	if (envptr && iMode == 'W') {
		object socketstream;
		char	*envAndTran = Getenv("DATASYNC_SERVER_AND_TRANFILE");
		if (!(envAndTran && (*envAndTran == 'Y' || *envAndTran == 'y')))
			useTranFile = 0;
		
		socketstream = pConnectToServer(obj, envptr);
		if (!socketstream)
			gError(Object, "gNewTP: Unable to connect to Datasync Server");
		
		gAddLast(iOutputStreamObjects, socketstream);
		iSFile = gNewWithServer(StringFile, socketstream);
	}
	if (useTranFile) {
		object filestream;
		if (!iSFile)
		{
			sprintf(buf, "%s.sf", file);
			iSFile = gNewWithStr(StringFile, buf);
		}
	
		sprintf(buf, "%s.tf", file);
		iTFile = gNewWithStr(String, buf);

	//	iFlushMode = ??
		if (iMode == 'W')
			filestream = gOpenFile(File, buf, "a+b");  //  I think I need the + for the rewind to work
		else 
			filestream = gOpenFile(File, buf, "rb");
		
		if (!filestream)
			return gDispose(obj);
		
		iFP = gPointerValue(filestream);
		gAddLast(iOutputStreamObjects, filestream);
	}
	iH.station = station;
	iH.user = user;
	iH.version = (unsigned char) TP_VERSION;	//  version 1 had 2 byte string lengths
							//  version 2 has 4 byte string lengths
	return obj;
}

imeth	gDispose, gDeepDispose ()
{
	gDeepDispose(iOutputStreamObjects);	
	gDispose(iSFile);
	
	if (iTFile)
		iTFile = gDispose(iTFile);
	
	return gDispose(super);
}


imeth	gTPAdd(stmt)
{
	static	char	WE[] = "TPAdd:  Error writing to transaction file.";
	char	*table;
	object	seq, si;
	Field	f;
	object	db = gDatabase(stmt);
	long	*rv;
	int	ri=1;
	object	dataBuffStream;
	int		status;

	if (!iRouteFun)
		return NULL;

	if (iMode != 'W')
		gError(Object, "TPAdd: transaction file not opened for writing");
	iH.time = time(NULL);
	iH.type = 'A';
	table = gName(stmt);
	if (!table)
		gError(Object, "TPAdd:  no table name");

	if (!stricmp(table, gHistoryTable(gDatabase(stmt))))
		rv = iRouteFun(gFldGetString(stmt, "tablename"));
	else
		rv = iRouteFun(table);
	if (!rv  &&  *rv <= 0)
		return NULL;

	dataBuffStream = gNew(String);
	
	iH.table = gGetStringIndex(iSFile, table);
	
	while (*rv >= ri) {
		if (!(iH.route = rv[ri++]))
			continue;
		
		gWrite(dataBuffStream, (char *)&iH, sizeof(iH));
			
		for (seq=gSequence(gColumns(stmt)) ; si = gNext(seq) ; ) {
			long	size;
			object	val;
			void	*p, *field;
		
			val = gGetValueToPut(si);
			if (!val  ||  ClassOf(val) == UniqueIdentifier)     //  SQL_GUID types
				continue;
			p = ClassOf(val) == String ? (void *) gStringValue(val) : gPointerValue(val);
			if (is_zero(val, p))
				continue;
			f.type  = what_type(ClassOf(val), si);
			if (f.type == 'V')
				p = (void *) gStringValue(gGetColVarText(si));
			size = what_size(ClassOf(val), p, f.type == 'V');
			
			f.field = gGetStringIndex(iSFile, field=gName(si));
			if (!f.type) 
				gError(Object, "TPAdd: invalid field type");
			
			if (ClassOf(val) == Character)
				f.cval = *(char *)p;
			
			gWrite(dataBuffStream, (char *)&f, sizeof(f));
			write_value(iv, val, p, 0, f.type == 'V', dataBuffStream);
			
		}
		
		f.field = 0L;
		f.type = 'Z';
		gWrite(dataBuffStream, (char *)&f, sizeof(f));
	}
	
	status = pCommitOutputStreams(self, dataBuffStream);
	
	gDispose(dataBuffStream);
	if (!status)
		gError(Object, WE);
	
	return self;
}

imeth	gTPChange(stmt)
{
	static	char	WE[] = "TPChange:  Error writing to transaction file.";
	char	*table, *fname;
	object	pk, seq, si, colDict, fld, old, new, db;
	Field	f;
	void	*p;
	int	nkf;		//  number of key fields
	char	*pks[40];
	long	*rv;
	int	ri=1;
	object	dataBuffStream;
	int		status;

	if (!iRouteFun)
		return NULL;

	if (iMode != 'W')
		gError(Object, "TPChange: transaction file not opened for writing");
	iH.time = time(NULL);
	iH.type = 'C';
	table = gName(stmt);
	if (!table)
		gError(Object, "TPChange:  no table name");

	if (!stricmp(table, gHistoryTable(gDatabase(stmt))))
		rv = iRouteFun(gFldGetString(stmt, "tablename"));
	else
		rv = iRouteFun(table);
	if (!rv  &&  *rv <= 0)
		return NULL;

	dataBuffStream = gNew(String);
	iH.table = gGetStringIndex(iSFile, table);
	
	while (*rv >= ri) {
		if (!(iH.route = rv[ri++]))
			continue;

		gWrite(dataBuffStream, (char *)&iH, sizeof(iH));

		pk = gGetPrimaryKey(db=gDatabase(stmt), table);
		if (!pk)
			vError(self, "Can't update %s when no primary key declared", table);
		colDict = gColumnDictionary(stmt);
		for (nkf=0, seq=gSequence(pk) ; fld=gNext(seq) ; ) {
			int	diff;

			si = gFindValueStr(colDict, fname=gStringValue(fld));
			pks[nkf++] = fname;
			old = gOriginalValue(si);
			new = gGetValueToPut(si);
			if (!old  ||  !new  ||  ClassOf(old) == UniqueIdentifier)   //  SQL_GUID
				continue;
			p = ClassOf(old) == String ? (void *) gStringValue(old) : gPointerValue(old);
			f.field = gGetStringIndex(iSFile, fname);
			f.type  = what_type(ClassOf(old), NULL);
			diff = old  &&  gCompare(old, new);
			f.cval = diff ? '2' : '1';
			if (!f.type)
				gError(Object, "TPChange: invalid field type");
			
			gWrite(dataBuffStream, (char *)&f, sizeof(f));
			write_value(iv, old, p, 1, 0, dataBuffStream);
			if (diff) {
				p = ClassOf(new) == String ? (void *) gStringValue(new) : gPointerValue(new);
				write_value(iv, new, p, 1, 0, dataBuffStream);
			}
		}
		for (seq=gSequence(gColumns(stmt)) ; si=gNext(seq) ; ) {
			int	i;

			//  Don't write out primary key fields again!
			fname = gName(si);
			for (i=0 ; i < nkf ; i++)
				if (!strcmpi(fname, pks[i]))
					break;
			if (i != nkf)
				continue;


			old = gOriginalValue(si);
			new = gGetValueToPut(si);
			if (!old  ||  !new  ||  ClassOf(old) == UniqueIdentifier)  //  SQL_GUID
				continue;
			f.type  = what_type(ClassOf(old), si);
			if (f.type == 'V') {
				old = gGetOrgVarText(si);
				new = gGetColVarText(si);
			}
			if (!old  ||  !gCompare(old, new))
				continue;
			p = ClassOf(old) == String ? (void *) gStringValue(old) : gPointerValue(old);
			f.field = gGetStringIndex(iSFile, fname);
			f.cval = '2';
			if (!f.type) 
				gError(Object, "TPChange: invalid field type");
			
			gWrite(dataBuffStream, (char *)&f, sizeof(f));
			write_value(iv, old, p, 1, f.type == 'V', dataBuffStream);
			p = ClassOf(new) == String ? (void *) gStringValue(new) : gPointerValue(new);
			write_value(iv, new, p, 1, f.type == 'V', dataBuffStream);
		}
		f.field = 0L;
		f.type = 'Z';
		gWrite(dataBuffStream, (char *)&f, sizeof(f));
	}

	status = pCommitOutputStreams(self, dataBuffStream);
	
	gDispose(dataBuffStream);
	if (!status)
		gError(Object, WE);

	return self;
}

imeth	gTPDelete(stmt)
{
	static	char	WE[] = "TPDelete:  Error writing to transaction file.";
	char	*table, *fname;
	object	pk, seq, si, colDict, fld, old;
	Field	f;
	void	*p;
	long	*rv;
	int	ri=1;
	object	dataBuffStream;
	int	status;

	if (!iRouteFun)
		return NULL;

	if (iMode != 'W')
		gError(Object, "TPDelete: transaction file not opened for writing");
	iH.time = time(NULL);
	iH.type = 'D';
	table = gName(stmt);
	if (!table)
		gError(Object, "TPDelete:  no table name");

	if (!stricmp(table, gHistoryTable(gDatabase(stmt))))
		rv = iRouteFun(gFldGetString(stmt, "tablename"));
	else
		rv = iRouteFun(table);
	rv = iRouteFun(table);
	if (!rv  &&  *rv <= 0)
		return NULL;

	dataBuffStream = gNew(String);
	iH.table = gGetStringIndex(iSFile, table);

	while (*rv >= ri) {
		if (!(iH.route = rv[ri++]))
			continue;

		gWrite(dataBuffStream, (char *)&iH, sizeof(iH));
		
		pk = gGetPrimaryKey(gDatabase(stmt), table);
		if (!pk)
			vError(self, "Can't update %s when no primary key declared", table);
		colDict = gColumnDictionary(stmt);
		for (seq=gSequence(pk) ; fld=gNext(seq) ; ) {
			si = gFindValueStr(colDict, fname=gStringValue(fld));
			old = gOriginalValue(si);
			if (!old  ||  ClassOf(old) == UniqueIdentifier)  //  SQL_GUID
				continue;
			p = ClassOf(old) == String ? (void *) gStringValue(old) : gPointerValue(old);
			f.type  = what_type(ClassOf(old), NULL);
			f.field = gGetStringIndex(iSFile, fname);
			if (ClassOf(old) == Character)
				f.cval = *(char *)p;
			if (!f.type) {
				gTPUnlock(CLASS, iFP);
				gError(Object, "TPDelete: invalid field type");
			}
			gWrite(dataBuffStream, (char *)&f, sizeof(f));
			write_value(iv, old, p, 0, 0, dataBuffStream);
		}
		f.field = 0L;
		f.type = 'Z';
		gWrite(dataBuffStream, (char *)&f, sizeof(f));
	}
	
	status = pCommitOutputStreams(self, dataBuffStream);
	
	gDispose(dataBuffStream);
	if (!status)
		gError(Object, WE);
	
	return self;
}

imeth	gTPExecute(char *table, char *cmd)
{
	static	char	WE[] = "TPExecute:  Error writing to transaction file.";
	long	*rv;
	int	ri=1;
	object	dataBuffStream;
	int	status;

	if (!iRouteFun)
		return NULL;

	rv = iRouteFun(table);
	if (!rv  &&  *rv <= 0)
		return NULL;

	dataBuffStream = gNew(String);

	if (iMode != 'W')
		gError(Object, "TPExecute: transaction file not opened for writing");
	iH.time = time(NULL);
	iH.type = 'E';
	iH.table = strlen(cmd)+1;

	while (*rv >= ri) {
		if (!(iH.route = rv[ri++]))
			continue;

		gWrite(dataBuffStream, (char *)&iH, sizeof(iH));
		gWrite(dataBuffStream, cmd, (int) iH.table);
	}
	status = pCommitOutputStreams(self, dataBuffStream);
	
	gDispose(dataBuffStream);
	if (!status)
		gError(Object, WE);
	return self;
}

#if 0
static	int	is_vartext(object stmt, char *table)
{
	static	char	*v[3] = {"vartextdatabaseid", "vartextid", "vartextcount"};
	int	i;
	object	kseq, fld, pk = gGetPrimaryKey(gDatabase(stmt), table);
	if (!pk  ||  gSize(pk) != 3)
		return 0;
	for (kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		for (i=0 ; i < 3 ; i++)
			if (!strcmp(v[i], gStringValue(fld)))
				break;
		if (i == 3) {
			gDispose(kseq);
			return 0;
		}
	}
	return 1;
}
#endif

static	void	update_add(ivType *iv, object stmt, char *table, char *RE)
{
	int	r, texists;
	Field	f;
	object	cols;

	r = gInsert(stmt, table);
	texists = !r  ||  r == SQL_NO_DATA_FOUND;
	if (texists)
		cols = gColumnDictionary(stmt);

	for (f.type = ' ' ; f.type != 'Z' ; ) {
		char	*field;
		int	exists;

		if (sizeof(f) != fread(&f, 1, sizeof(f), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (f.type != 'Z') {
			field = gStringFromIndex(iSFile, f.field);
			if (!field)
				gError(Object, "TPUpdate: Missing field name in string table.");
			exists = texists  &&  !!gFindStr(cols, field);
			set_field(iv, f, field, RE, iH.type == 'C', stmt, exists);
		}
	}
	if (texists)
		gAddRecord(stmt);
}

#define	BUFFER_SIZE	8192

#define	GO_END								\
	while (*p) {							\
		p++;							\
		if (++n > BUFFER_SIZE)					\
			vError(Object, "QueryBuffer overflow");		\
	}

static	void	update_change(ivType *iv, object stmt, char *table, char *RE, char *buf)
{
	int	r, texists, exists, exec=0, type = gDBMS_type(stmt), nvt=0;
	Field	f;
	object	cols, vals, val1, val2;
	long	vt[100];

	r = gInsert(stmt, table);			//  get a list of valid columns
	texists = !r  ||  r == SQL_NO_DATA_FOUND;
	if (texists)
		cols = gColumnDictionary(stmt);

	vals = gNewWithInt(StringDictionary, 101);
	for (f.type = ' ' ; f.type != 'Z' ; ) {
		char	*field;

		if (sizeof(f) != fread(&f, 1, sizeof(f), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (f.type != 'Z') {
			field = gStringFromIndex(iSFile, f.field);
			if (!field)
				gError(Object, "TPUpdate: Missing field name in string table.");
			exists = texists  &&  !!gFindStr(cols, field);
			val1 = make_object(iv, f, RE, iH.type == 'C', exists);
			if (f.cval == '2') {
				val2 = make_object(iv, f, RE, iH.type == 'C', exists);
				if (exists)
					gAddStr(vals, field, gNewWithObjObj(ObjectAssociation, val1, val2));
			} else if (exists)
				gAddStr(vals, field, val1);
		}
	}
	
	if (texists) {
		char	*p=buf, *name;
		object	seq, si, pk, kseq, fld;
		int	add_comma = 0, n=0;
		
		pk = gGetPrimaryKey(gDatabase(stmt), table);
		if (!pk)
			vError(Object, "Can't update %s when no primary key declared", table);

		sprintf(p, "select * from %s ", table);
		GO_END;
		strcpy(p, " WHERE ");
		GO_END;
		for (exec=1, add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
			val1 = gFindValueStr(vals, name=gStringValue(fld));
			if (val1) {
				if (add_comma) {
					strcpy(p, " AND ");
					GO_END;
				} else
					add_comma = 1;
				strcpy(p, name);
				strcat(p, " = ");
				GO_END;
				formatField(type, p, ClassOf(val1) == ObjectAssociation ? gKey(val1) : val1);
				GO_END;
			} else
				exec = 0;
		}
	}
	if (texists  &&  exec)
		if (!gDBSelectOne(stmt, buf)) {
			object	seq, sa, oa;
			for (seq = gSequence(vals) ; sa = gNext(seq) ; )
				if (ClassOf(oa=gValue(sa)) == ObjectAssociation) {
//					val1 = gKey(oa);
					val2 = gValue(oa);
					if (ClassOf(val2) == Character)
						gFldSetChar(stmt, gStringKey(sa), (int) gCharValue(val2));
					else if (ClassOf(val2) == String) {
						char	*str = gStringValue(val2);
						if (*str == (char) 1)    //  VarText
							gFldSetString(stmt, gStringKey(sa), str+1);
						else
							gFldSetString(stmt, gStringKey(sa), str);
					} else if (ClassOf(val2) == ShortInteger)
						gFldSetShort(stmt, gStringKey(sa), gShortValue(val2));
					else if (ClassOf(val2) == LongInteger  ||
						 ClassOf(val2) == Date ||
						 ClassOf(val2) == Time)
						gFldSetLong(stmt, gStringKey(sa), gLongValue(val2));
					else if (ClassOf(val2) == DoubleFloat)
						gFldSetDouble(stmt, gStringKey(sa), gDoubleValue(val2));
					else if (ClassOf(val2) == DateTime) {
						long	dt, tm;
						gDateTimeValues(val2, &dt, &tm);
						gFldSetDateTime(stmt, gStringKey(sa), dt, tm);
					}
				}
			gUpdateRecord(stmt);
		}
	gDeepDispose(vals);
}

#if 0

static	void	update_change(ivType *iv, object stmt, char *table, char *RE, char *buf)
{
	int	r, texists, exists, exec=0, type = gDBMS_type(stmt), nvt=0;
	Field	f;
	object	cols, vals, val1, val2;
	long	vt[100];

	r = gInsert(stmt, table);			//  get a list of valid columns
	texists = !r  ||  r == SQL_NO_DATA_FOUND;
	if (texists)
		cols = gColumnDictionary(stmt);

	vals = gNewWithInt(StringDictionary, 101);
	for (f.type = ' ' ; f.type != 'Z' ; ) {
		char	*field;

		if (sizeof(f) != fread(&f, 1, sizeof(f), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (f.type != 'Z') {
			field = gStringFromIndex(iSFile, f.field);
			if (!field)
				gError(Object, "TPUpdate: Missing field name in string table.");
			exists = texists  &&  !!gFindStr(cols, field);
			val1 = make_object(iv, f, RE, iH.type == 'C', exists);
			if (f.cval == '2') {
				val2 = make_object(iv, f, RE, iH.type == 'C', exists);
				if (exists)
					gAddStr(vals, field, gNewWithObjObj(ObjectAssociation, val1, val2));
			} else if (exists)
				gAddStr(vals, field, val1);
		}
	}
	
	if (texists) {
		char	*p=buf, *name;
		object	seq, si, pk, kseq, fld;
		int	add_comma = 0, n=0;
		
		pk = gGetPrimaryKey(gDatabase(stmt), table);
		if (!pk)
			vError(Object, "Can't update %s when no primary key declared", table);

		sprintf(p, "UPDATE %s SET ", table);
		GO_END;
		for (seq=gSequence(gColumns(stmt)) ; si = gNext(seq) ; ) {
			val1 = gFindValueStr(vals, name=gName(si));
			if (val1  &&  ClassOf(val1) == ObjectAssociation) {
				if (add_comma) {
					*p++ = ',';
					*p++ = ' ';
					n += 2;
				} else
					add_comma = 1;
				strcpy(p, name);
				strcat(p, " = ");
				GO_END;
				formatField(type, p, gValue(val1));
				GO_END;
			}
		}
		strcpy(p, " WHERE ");
		GO_END;
		for (exec=1, add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
			val1 = gFindValueStr(vals, name=gStringValue(fld));
			if (val1) {
				if (add_comma) {
					strcpy(p, " AND ");
					GO_END;
				} else
					add_comma = 1;
				strcpy(p, name);
				strcat(p, " = ");
				GO_END;
				formatField(type, p, ClassOf(val1) == ObjectAssociation ? gKey(val1) : val1);
				GO_END;
			} else
				exec = 0;
		}
	}
	if (texists  &&  exec)
		gExecute(stmt, buf);
	gDeepDispose(vals);
}

#endif	

static	void	update_delete(ivType *iv, object stmt, char *table, char *RE, char *buf)
{
	int	r, texists, exists, exec=0, type = gDBMS_type(stmt);
	Field	f;
	object	cols, vals, val1;

	r = gInsert(stmt, table);			//  get a list of valid columns
	texists = !r  ||  r == SQL_NO_DATA_FOUND;
	if (texists)
		cols = gColumnDictionary(stmt);

	vals = gNewWithInt(StringDictionary, 101);
	for (f.type = ' ' ; f.type != 'Z' ; ) {
		char	*field;

		if (sizeof(f) != fread(&f, 1, sizeof(f), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (f.type != 'Z') {
			field = gStringFromIndex(iSFile, f.field);
			if (!field)
				gError(Object, "TPUpdate: Missing field name in string table.");
			exists = texists  &&  !!gFindStr(cols, field);
			val1 = make_object(iv, f, RE, iH.type == 'C', exists);
			if (exists)
				gAddStr(vals, field, val1);
		}
	}
	
	if (texists) {
		char	*p=buf, *name;
		object	seq, si, pk, kseq, fld;
		int	add_comma = 0, n=0;
		
		pk = gGetPrimaryKey(gDatabase(stmt), table);
		if (!pk)
			vError(Object, "Can't update %s when no primary key declared", table);

		sprintf(p, "DELETE FROM %s WHERE ", table);
		GO_END;
		for (exec=1, add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
			val1 = gFindValueStr(vals, name=gStringValue(fld));
			if (val1) {
				if (add_comma) {
					strcpy(p, " AND ");
					GO_END;
				} else
					add_comma = 1;
				strcpy(p, name);
				strcat(p, " = ");
				GO_END;
				formatField(type, p, val1);
				GO_END;
			} else
				exec = 0;
		}
	}
	if (texists  &&  exec)
		gExecute(stmt, buf);
	gDeepDispose(vals);
}

static	void	update_execute(ivType *iv, object stmt, long len, char *RE, char *buf)
{
	if ((int) len != fread(buf, 1, (int) len, iFP)) {
		gTPUnlock(CLASS, iFP);
		gError(Object, RE);
	}
	gExecute(stmt, buf);
}

imeth	gTPUpdate(db)
{
	static	char	RE[] = "TPUpdate:  Error reading from transaction file.";
	object	stmt = gNewStatement(db);
	char	*buf;

	if (iMode == 'W')
		gError(Object, "TPUpdate: transaction file not opened for reading");
	gTPLock(CLASS, iFP);
	buf = malloc(BUFFER_SIZE);
	if (!buf)
		gError(Object, "Out of memory.");
	gReturnError(stmt, SQL_ERROR);
//	gLogODBC(Statement, "odbc.log");
	rewind(iFP);
	while (sizeof(iH) == fread(&iH, 1, sizeof(iH), iFP)) {
		char	*table;

		if (iH.version > TP_VERSION)
			vError(Object, "Transaction file wrong version (%d/%d)", iH.version, TP_VERSION);
		if (iH.type != 'E') {
			table = gStringFromIndex(iSFile, iH.table);
			if (!table)
				gError(Object, "TPUpdate: Missing table name in string table.");
		}
		
		if (iH.type == 'A') {	
			update_add(iv, stmt, table, RE);
		} else if (iH.type == 'C') {
			update_change(iv, stmt, table, RE, buf);
		} else if (iH.type == 'D') {
			update_delete(iv, stmt, table, RE, buf);
		} else if (iH.type == 'E') {
			update_execute(iv, stmt, iH.table, RE, buf);
		} else
			gError(Object, "TPUpdate: Invalid record type.");
	}
	gTPUnlock(CLASS, iFP);
	gDispose(stmt);
	free(buf);
//	gLogODBC(Statement, NULL);
	return self;
}

imeth	gTPDump(char *file)
{
	static	char	RE[] = "TPDump:  Error reading from transaction file.";
	FILE	*fp = fopen(file, "w");
	char	*buf;
	object s;
	object obj, stream = NULL;
	long	bytesReadAtStart;
	long		dataLen;

	if (!fp)
		gError(Object, "Can't open TPDump file");
	if (iMode == 'W')
		gError(Object, "TPDump: transaction file not opened for reading");
	buf = malloc(BUFFER_SIZE);
	if (!buf)
		gError(Object, "Out of memory.");
	
	//find out what kind of stream we're using
	for (s=gSequence(iOutputStreamObjects) ; obj = gNext(s) ; ) {
		if (ClassOf(obj) == Socket)
			stream = obj;
		else if (ClassOf(obj) == File && !stream)
			stream = obj;
	}
	
	if (ClassOf(stream) == File) {
		gTPLock(CLASS, iFP);
		rewind(iFP);
	} else {
		char *cmd = "sendtranfile";
		int cmdlen = strlen(cmd);
		
		//send cmd to get file
		gWriteInt32(stream, cmdlen);
		gWrite(stream, cmd, cmdlen);
		//read length
		if (!gReadInt32(stream, &dataLen))
			gError(Object, "DataSync Server socket read error.");
		bytesReadAtStart = gGetTotalBytesRead(stream);
	}
	while (1) {
		int	end, n;
		Field	f;

		if (ClassOf(stream) == File) {
			if (sizeof(iH) != gRead(stream, (char *)&iH, sizeof(iH)))
				break;
		} else {
			if (gGetTotalBytesRead(stream) - bytesReadAtStart >= dataLen)
				break;
			gRead(stream, (char *)&iH, sizeof(iH));
		}
		if (iH.version > TP_VERSION)
			vError(Object, "Transaction file wrong version (%d/%d)", iH.version, TP_VERSION);
		fprintf(fp, "%s - %hd - %hd - %c - %s (%ld)\n",
			make_date(iH.time), iH.station, iH.route, iH.type,
			iH.type != 'E' ? gStringFromIndex(iSFile, iH.table) : "",
			iH.table);
		if (iH.type == 'E') {
			if ((int) iH.table != gRead(stream, buf, (int) iH.table)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
			fprintf(fp, "\t%s\n", buf);
		} else
			for (n=1, end=0 ; !end ; n++) {
				if (sizeof(f) != gRead(stream, (char *)&f, sizeof(f))) {
					gTPUnlock(CLASS, iFP);
					gError(Object, RE);
				}
				end = display_value(iv, fp, f, n, RE, iH.type == 'C', stream);
				if (!end  &&  iH.type == 'C'  &&  f.cval == '2')
					display_value(iv, fp, f, n, RE, 1, stream);
			}
		fprintf(fp, "\n");
		fflush(fp);
	}
	if (ClassOf(stream) == File) {
		gTPUnlock(CLASS, iFP);
		fclose(fp);
	}
	free(buf);
	return self;
}


static	long	copy_file(char *ffile, FILE *tfp, char *buf, int trunc)
{
	int	len=1;
	long	tlen=0L;
	FILE	*ffp;

	ffp = fopen(ffile, "rb");
	if (!ffp)
		vError(Object, "Can't open %s", ffile);
	gTPLock(CLASS, ffp);
	while (len > 0) {
		len = fread(buf, 1, BUFFER_SIZE, ffp);
		if (len > 0)
			if (len != fwrite(buf, 1, len, tfp)) {
				gTPUnlock(CLASS, ffp);
				gError(Object, "write error");
			} else
				tlen += len;
		else if (ferror(ffp)) {
			gTPUnlock(CLASS, ffp);
			gError(Object, "read error");
		}
	}
	if (trunc) {
		_chsize(_fileno(ffp), 0L);
		_commit(_fileno(ffp));
	}
	gTPUnlock(CLASS, ffp);
	fclose(ffp);
	return tlen;
}

static	void	copy_file2(char *tfile, FILE *ffp, char *buf, long mlen)
{
	int	len;
	FILE	*tfp;

	tfp = fopen(tfile, "wb");
	if (!tfp)
		vError(Object, "Can't create %s", tfile);
	while (mlen) {
		len = BUFFER_SIZE > mlen ? mlen : BUFFER_SIZE;
		len = fread(buf, 1, len, ffp);
		if (len > 0)
			if (len != fwrite(buf, 1, len, tfp))
				gError(Object, "write error");
			else
				mlen -= len;
		else if (ferror(ffp))
			gError(Object, "read error");
	}
	fclose(tfp);
}

cmeth	gTPCombine(char *sfile, char *tfile, char *cfile)
{
	static	char	WE[] = "gTPCombine: write error";
	FILE	*tfp;
	char	*buf;
	CHead	h;

	buf = malloc(BUFFER_SIZE);
	if (!buf)
		gError(self, "out of memory");
	tfp = fopen(cfile, "wb");
	if (!tfp)
		vError(self, "Can't create %s", cfile);

	strcpy(h.sig, "CTF");
	h.version = VERSION;
	h.time = time(NULL);
	h.tlen = h.slen = 0L;
	if (1 != fwrite(&h, sizeof(h), 1, tfp))
		gError(self, WE);

	h.tlen = copy_file(tfile, tfp, buf, 1);
	h.slen = copy_file(sfile, tfp, buf, 0);

	rewind(tfp);
	if (1 != fwrite(&h, sizeof(h), 1, tfp))
		gError(self, WE);
	
	fclose(tfp);
	free(buf);
	return self;
}

cmeth	gTPSplit(char *sfile, char *tfile, char *cfile)
{
	static	char	RE[] = "gTPSplit: read error";
	FILE	*fp;
	char	*buf;
	CHead	h;

	buf = malloc(BUFFER_SIZE);
	if (!buf)
		gError(self, "out of memory");
	fp = fopen(cfile, "rb");
	if (!fp)
		vError(self, "Can't open %s", cfile);

	if (1 != fread(&h, sizeof(h), 1, fp))
		gError(self, RE);

	if (strcmp("CTF", h.sig))
		vError(self, "%s is not a combined transaction file.", cfile);
	if (h.version != VERSION)
		gError(self, "Incorrect CTF file version number.");
	if (sizeof(h) + h.tlen + h.slen != _filelength(_fileno(fp)))
		gError(self, "CTF file incorrect size.");

	copy_file2(tfile, fp, buf, h.tlen);
	copy_file2(sfile, fp, buf, h.slen);

	fclose(fp);
	free(buf);
	return self;
}

static	object	make_object(ivType *iv, Field f, char *RE, int change, int exists)
{
	object	rval = NULL;

	switch (f.type) {
	case '1':	//  Character
	{
		char	val;

		if (!change)
			val = f.cval;
		else if (1 != fread(&val, 1, 1, iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithChar(Character, (int) val);
	}
	break;
	case '2':	//  String
	{
		long	size;
		char	*buf, buffer[256];

		if (iH.version == 1) {
			unsigned short	ssize;
			if (sizeof(ssize) != fread(&ssize, 1, sizeof(ssize), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
			size = ssize;
		} else {
			if (sizeof(size) != fread(&size, 1, sizeof(size), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
		}
		if (size > sizeof(buffer)) {
			buf = malloc((unsigned)size);
			if (!buf) {
				gTPUnlock(CLASS, iFP);
				gError(Object, "Out of memory.");
			}
		} else
			buf = buffer;
		if (size != fread(buf, 1, (int) size, iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithStr(String, buf);
		if (size > sizeof(buffer))
			free(buf);
	}
	break;
	case 'V':	//  VarText
	{
		long	size;
		char	*buf, buffer[256];

		if (iH.version == 1)  {
			unsigned short	ssize;
			if (sizeof(ssize) != fread(&ssize, 1, sizeof(ssize), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
			size = ssize;
		} else {
			if (sizeof(size) != fread(&size, 1, sizeof(size), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
		}
		if (size+1 > sizeof(buffer)) {
			buf = malloc((unsigned)size+1);
			if (!buf) {
				gTPUnlock(CLASS, iFP);
				gError(Object, "Out of memory.");
			}
		} else
			buf = buffer;
		*buf = (char) 1;  //  signify VarText
		if (size != fread(buf+1, 1, (int) size, iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithStr(String, buf);
		if (size+1 > sizeof(buffer))
			free(buf);
	}
	break;
	case '3':	//  ShortInteger
	{
		short	val;

		if (sizeof(short) != fread(&val, 1, sizeof(short), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithInt(ShortInteger, (int) val);
	}
	break;
	case '4':	//  LongInteger
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithLong(LongInteger, val);
	}
	break;
	case '6':	//  DoubleFloat
	{
		double	val;

		if (sizeof(double) != fread(&val, 1, sizeof(double), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithDouble(DoubleFloat, val);
	}
	break;
	case '7':	//  Date
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithLong(Date, val);
	}
	break;
	case '8':	//  Time
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewWithLong(Time, val);
	}
	break;
	case '9':	//  DateTime
	{
		long	d, t;

		if (sizeof(long) != fread(&d, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (sizeof(long) != fread(&t, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			rval = gNewDateTime(DateTime, d, t);
	}
	break;
	case 'Z':
		break;
	}
	return rval;
}

static	int	set_field(ivType *iv, Field f, char *field, char *RE, int change, object stmt, int exists)
{
	int	end = 0;

	switch (f.type) {
	case '1':	//  Character
	{
		char	val;

		if (!change)
			val = f.cval;
		else if (1 != fread(&val, 1, 1, iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetChar(stmt, field, (int) val);
	}
	break;
	case '2':	//  String
	case 'V':	//  VarText
	{
		long	size;
		char	*buf, buffer[256];

		if (iH.version == 1) {
			unsigned short ssize;
			if (sizeof(ssize) != fread(&ssize, 1, sizeof(ssize), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
			size = ssize;
		} else {
			if (sizeof(size) != fread(&size, 1, sizeof(size), iFP)) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
		}
		if (size > sizeof(buffer)) {
			buf = malloc((unsigned)size);
			if (!buf) {
				gTPUnlock(CLASS, iFP);
				gError(Object, "Out of memory.");
			}
		} else
			buf = buffer;
		if (size != fread(buf, 1, (int) size, iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			if (f.type == 'V')
				gFldSetString(stmt, field, buf);
			else
				gFldSetString(stmt, field, buf);
		if (size > sizeof(buffer))
			free(buf);
	}
	break;
	case '3':	//  ShortInteger
	{
		short	val;

		if (sizeof(short) != fread(&val, 1, sizeof(short), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetShort(stmt, field, (int) val);
	}
	break;
	case '4':	//  LongInteger
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetLong(stmt, field, val);
	}
	break;
	case '6':	//  DoubleFloat
	{
		double	val;

		if (sizeof(double) != fread(&val, 1, sizeof(double), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetDouble(stmt, field, val);
	}
	break;
	case '7':	//  Date
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetLong(stmt, field, val);
	}
	break;
	case '8':	//  Time
	{
		long	val;

		if (sizeof(long) != fread(&val, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetLong(stmt, field, val);
	}
	break;
	case '9':	//  DateTime
	{
		long	d, t;

		if (sizeof(long) != fread(&d, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (sizeof(long) != fread(&t, 1, sizeof(long), iFP)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (exists)
			gFldSetDateTime(stmt, field, d, t);
	}
	break;
	case 'Z':
		end = 1;
		break;
	}
	return end;
}

static	int	display_value(ivType *iv, FILE *fp, Field f, int n, char *RE, int change, object stream)
{
	int	end = 0;

	switch (f.type) {
	case '1':	//  Character
	{
		char	val;

		if (!change)
			val = f.cval;
		else if (1 != gRead(stream, (char *)&val, 1)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Character - '%c'\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '2':	//  String
	case 'V':	//  VarText
	{
		long	size;
		char	*buf, buffer[256];

		if (iH.version == 1) {
			unsigned short	ssize;
			if (sizeof(short) != gRead(stream, (char *)&ssize, sizeof(short))) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
			size = ssize;
		} else {
			if (sizeof(size) != gRead(stream, (char *)&size, sizeof(size))) {
				gTPUnlock(CLASS, iFP);
				gError(Object, RE);
			}
		}
		if (size > sizeof(buffer)) {
			buf = malloc((unsigned)size);
			if (!buf) {
				gTPUnlock(CLASS, iFP);
				gError(Object, "Out of memory.");
			}
		} else
			buf = buffer;
		if (size != gRead(stream, buf, (int) size)) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - %s - \"%s\"\n", 
			n, gStringFromIndex(iSFile, f.field), f.field,
			f.type == '2' ? "String" : "VarText", buf);
		if (size > sizeof(buffer))
			free(buf);
	}
	break;
	case '3':	//  ShortInteger
	{
		short	val;

		if (sizeof(short) != gRead(stream, (char *)&val, sizeof(short))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Short - %hd\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '4':	//  LongInteger
	{
		long	val;

		if (sizeof(long) != gRead(stream, (char *)&val, sizeof(long))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Long - %ld\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '6':	//  DoubleFloat
	{
		double	val;

		if (sizeof(double) != gRead(stream, (char *)&val, sizeof(double))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Double - %f\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '7':	//  Date
	{
		long	val;

		if (sizeof(long) != gRead(stream, (char *)&val, sizeof(long))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Date - %ld\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '8':	//  Time
	{
		long	val;

		if (sizeof(long) != gRead(stream, (char *)&val, sizeof(long))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - Time - %ld\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, val);
	}
	break;
	case '9':	//  DateTime
	{
		long	d, t;

		if (sizeof(long) != gRead(stream, (char *)&d, sizeof(long))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		if (sizeof(long) != gRead(stream, (char *)&t, sizeof(long))) {
			gTPUnlock(CLASS, iFP);
			gError(Object, RE);
		}
		fprintf(fp, "\t%3d - %s (%ld) - DateTime - %ld, %ld\n", 
			n, gStringFromIndex(iSFile, f.field), f.field, d, t);
	}
	break;
	case 'Z':
		end = 1;
		fprintf(fp, "\tEOF - %d fields\n", n-1);
		break;
	}
	return end;
}

static	void	write_value(ivType *iv, object val, void *p, int flg, int varText, object stream)
{
	static	char	WE[] = "Error writing value to transaction file.";
	long	size = what_size(ClassOf(val), p, varText);

	if (ClassOf(val) == String  ||  varText)
		gWrite(stream, (char *)&size, sizeof(size));
	if (ClassOf(val) == DateTime) {
		long	d, t;
		gDateTimeValues(val, &d, &t);
		gWrite(stream, (char *)&d, sizeof(long));
		gWrite(stream, (char *)&t, sizeof(long));
	} else if (ClassOf(val) != Character  ||  flg) {
		gWrite(stream, (char *)p, size);
	}
}

static	char	*make_date(long t)
{
	char	*p = asctime(localtime((time_t*)&t));
	char	*b = p;
	
	for ( ; *p ; p++)
		if (*p == '\r'  ||  *p == '\n') {
			*p = '\0';
			break;
		}
	return b;
}

static	int	is_zero(object val, void *p)
{
	object	cls = ClassOf(val);

	if (cls == Character  ||  cls == String)
		return !*(char *)p;
	if (cls == ShortInteger)
		return !*(short *)p;
	if (cls == LongInteger  ||  cls == Date  ||  cls == Time)
		return !*(long *)p;
	if (cls == DoubleFloat)
		return *(double *)p == 0.0;
	if (cls == DateTime) {
		long	d, t;
		gDateTimeValues(val, &d, &t);
		if (!d  &&  !t)
			return 1;
	}
	return 0;
}

#define	TY(x, y)	if (cls == x) return y

static	char	what_type(object cls, object si)
{
	if ((cls == LongInteger  ||  cls == ShortInteger)  &&  si  &&  gVarTextChanged(si))
		return 'V';	//  VarText
	TY(Character, '1');
	TY(String, '2');
	TY(ShortInteger, '3');
	TY(LongInteger, '4');
	TY(DoubleFloat, '6');
	TY(Date, '7');
	TY(Time, '8');
	TY(DateTime, '9');
	TY(UniqueIdentifier, 'U');
	return '\0';
}	

static	long	what_size(object cls, void *p, int varText)
{
	if (varText)
		return (unsigned) strlen((char *)p) + 1U;
	TY(Character, sizeof(char));
	if (cls == String)
		return (unsigned) strlen((char *)p) + 1U;
	TY(ShortInteger, sizeof(short));
	TY(LongInteger, sizeof(long));
	TY(DoubleFloat, sizeof(double));
	TY(Date, sizeof(long));
	TY(Time, sizeof(long));
	TY(DateTime, 2*sizeof(long));
	TY(UniqueIdentifier, sizeof(GUID));
	return 0;
}	

cmeth	gTPLock(FILE *fp)
{
	int	n = 0;

	rewind(fp);
	while (_locking(_fileno(fp), _LK_LOCK, LOCK_BYTES))
		if (errno != EDEADLOCK  ||  ++n == 40)
			gError(Object, "Can't get lock on file.");
	return self;
}

cmeth	gTPUnlock(FILE *fp)
{
	if (!fp)
		return self;
	rewind(fp);
	_locking(_fileno(fp), _LK_UNLCK, LOCK_BYTES);
	return self;
}

static	void	formatField(int type, char *buf, object fval)
{
	if (ClassOf(fval) == String)
		sprintf(buf, "~%s~", gStringValue(fval));
	else if (ClassOf(fval) == ShortInteger)
		sprintf(buf, "%d", (int) gShortValue(fval));
	else if (ClassOf(fval) == LongInteger)
		sprintf(buf, "%ld", gLongValue(fval));
	else if (ClassOf(fval) == DoubleFloat)
		sprintf(buf, "%f", gDoubleValue(fval));
	else if (ClassOf(fval) == Time) {
		object	obj, obj2;
		long	tm;
		
		tm = gLongValue(fval);
		obj2 = tm ? fval : gNewWithLong(Time, 0L);
		sprintf(buf, "~%s~", gStringValue(obj=gFormatTime(obj2, "%H:%M:%S.%L")));
		gDispose(obj);
		if (!tm)
			gDispose(obj2);
	} else if (ClassOf(fval) == Date) {
		object	obj, obj2;
		long	dt;
		
		dt = gLongValue(fval);
		if (type != DBMS_ACCESS)
			obj2 = dt ? fval : gNewWithLong(Date, 18000101L);
		else
			obj2 = dt ? fval : gNewWithLong(Date, 0L);
		sprintf(buf, "~%s~", gStringValue(obj=gFormatDate(obj2, "%Y-%N-%D")));
		gDispose(obj);
		if (!dt)
			gDispose(obj2);
	} else if (ClassOf(fval) == DateTime) {
		object	dobj, tobj, dobj2, tobj2;
		long	dt, tm;
		
		gDateTimeValues(fval, &dt, &tm);
		if (type != DBMS_ACCESS)
			dobj2 = dt ? fval : gNewWithLong(Date, 18000101L);
		else
			dobj2 = dt ? fval : gNewWithLong(Date, 0L);
		tobj2 = tm ? fval : gNewWithLong(Time, 0L);
		sprintf(buf, "~%s %s~", gStringValue(dobj=gFormatDate(dobj2, "%Y-%N-%D")),
			gStringValue(tobj=gFormatTime(tobj2, "%H:%M:%S.%L%p")));
		gDispose(dobj);
		gDispose(tobj);
		if (!dt)
			gDispose(dobj2);
		if (!tm)
			gDispose(tobj2);
	} else
		*buf = '\0';
}

imeth	gSetRouteFunction(long *(*fun)(char *tab))
{
	iRouteFun = fun;
	return self;
}








