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
#include "sqlite3.h"




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



#define	LAZY_SELECT	1
#define	LAZY_SELECTONE	2


#define DB_ERROR_MESSAGE_SIZE	1024

#define	BUF_SIZE	4000

/* this class mimics Statement and attempts to use the database cache
	it is filled with fallbacks that pass through to the Statement class
	if a query can't use the cache
*/

defclass  CacheStatement : PropertyList {
	object iStatement;
	object iDatabase;

	short iUsingCache;

	short iLoadedCurrentRow;

	char *iSqlCmd;

	sqlite3 *iDB;


	iCols;		/*  dictionary of result column names and StatementInfo objects  */
	iColList;	/*  LinkObject of columns (in order)  */
	int	iTType; /*  1=select, 2=insert, etc.                             */

	int	iDoneFirst;
	UWORD	iRowStatus;
	char	iTable[100];
	iErrors;	/*  error codes to return instead of catching  */
	iDialogs;	//  Attached dialogs
	int	iStmtAlreadyClosed;

	object		iTag;		/*  arbitrary tag object associated
					    with the iStatement */

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

	//  Cursor stuff


	iCursorFile;
	int	iRecordsRead;
	int	iRecordLength;


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

extern void * getStringMemory(char *str);



private	imeth	void	cursor_write(int rec);
private imeth	getCurrentWhereClause(char * tableName);
private imeth int resetStmt();
private	imeth	int	bindCols(sqlite3_stmt *sqlLiteStmt);


extern	char	*fix_sql(char *s);
extern	char	*Getenv(char *);


static	void	begin_log(void);
static	void	end_log(void);
static	void	print_log(char *fun, char *stmt, char *org);
static	char	*query_buffer(int reclen);
extern char	*lcase(char *v);

extern char	*lcname(char *name);
static	char	*fix_statement(char *s, int dbms);
static	int	get_table(char *table, char *cmd, object *tlist);


private	imeth	int	pTrapError(object self, object es, int err);
private	imeth	char	*pFldGetVarText(object si, char *fld, char *vttbl);
private	imeth	pFldSetVarText(object si, char *fld, char *str, char *vttbl);
private	imeth	char	*pGetVarTextTable(object self, object si, char *fld);




#define	BEGIN_LOG()			if (cLFP)	begin_log()
#define	END_LOG()			if (cLFP)	end_log()
#define	PRINT_LOG(fun, stmt, org)	if (cLFP)	print_log(fun, stmt, org)


/*constructor
*/
cvmeth	gNewCacheStatement(object dyndb, object stmt, void * sqllitedb)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	char	*en = Getenv("INTEGRA_DATABASE_CACHE_VALIDATE"); 

	
	iDatabase=dyndb;
	iStatement=stmt;
	
	iDB=(sqlite3 *)sqllitedb;

	iSqlCmd=NULL;


	resetStmt(obj);

	
	
	
	if (en&&(stricmp(en,"Y")==0)) 
	{
		if (!cLFP)
		{
			cLFP=fopen("CacheErrorLog.xml","w");
			fprintf(cLFP,"<LOG>");
		}
	}

	
	return obj;
}


/* reset the sqlite statement for reuse
*/
private imeth int resetStmt()
{

	iUsingCache=0;

	iRecordsRead=0;
	
	if (iSqlCmd)
	{
		free(iSqlCmd);
		iSqlCmd=NULL;
	}	

	

	return 0;
}

static	int	get_update_table(char *table, char *cmd);


static	int	get_insert_table(char *table, char *cmd);

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

/*This function is called when something may need to result in clearing the cache
	it will try to keep from having to clear it by sending the command to both 
	databases.  If that fails, or it doesn't know what the command is, it will clear
	the appropriate tables.

	cmd - sql command being executed
*/
private imeth int conditionalClearCache(char *cmd)
{
	object buf=gNewWithStr(String,cmd);

	gToUpper(buf);

	//SQLite REQUIRES the FROM in a DELETE statement, but we don't always use them

	if (!strnicmp(cmd,"DELETE",6)&& !strstr(gStringValue(buf)," FROM "))
	{
		char *t=cmd+6;
		gDispose(buf);
		buf=gNewWithStr(String, "DELETE FROM ");
		gWrite(buf,t,strlen(t));
		cmd=gStringValue(buf);
	}
	


	if (!strnicmp(cmd,"DELETE",5)||!strnicmp(cmd,"UPDATE",6)||!strnicmp(cmd,"INSERT",6))  //I can only pass on DELETE, UPDATE, and INSERT's
	{
		char * msg;
		int rc=0;
		//I don't really need to clear the cache, I need to run this command
		cmd=fix_statement(cmd, DBMS_MSSQL);

		//this may be an insert or update and I haven't made the table ready so...

		if (!strnicmp(cmd,"UPDATE",6))
		{
			object nonCacheStmt=gNewNonCacheStatement(iDatabase);
			object cachetable;
			char  table[256];
			get_update_table(table,cmd);
		
			cachetable=gGetCacheTable(iDatabase,table);
	
			gMakeReady(cachetable,nonCacheStmt,iDB);

			gDispose(nonCacheStmt);

		}

		if (!strnicmp(cmd,"INSERT",6))
		{
			object nonCacheStmt=gNewNonCacheStatement(iDatabase);
			object cachetable;
			char  table[256];
			get_insert_table(table,cmd);
		
			cachetable=gGetCacheTable(iDatabase,table);
	
			gMakeReady(cachetable,nonCacheStmt,iDB);

			gDispose(nonCacheStmt);

		}

		if (!strnicmp(cmd,"DELETE",6))
		{
			object nonCacheStmt=gNewNonCacheStatement(iDatabase);
			object cachetable;
			char  table[256];
			get_delete_table(table,cmd);
		
			cachetable=gGetCacheTable(iDatabase,table);
	
			gMakeReady(cachetable,nonCacheStmt,iDB);

			gDispose(nonCacheStmt);

		}
		
		if (cLFP)
		{
			fprintf(cLFP, "<SQL> <![CDATA[%s]]> \n",cmd);
			fprintf(cLFP, " </SQL> \n");
		}
		if ((rc=sqlite3_exec(iDB,cmd,NULL,NULL,&msg))&&strnicmp(cmd,"DELETE",5)) //If a delete fails, I must not have it, so I'll go on
		{
			//doing my own failed, so I'll clear my cache
			char table[256];
			if (!strnicmp(cmd,"UPDATE",6))
				get_update_table(table,cmd);
			if (!strnicmp(cmd,"INSERT",6))
				get_insert_table(table,cmd);
			if (!strnicmp(cmd,"DELETE",6))
				get_delete_table(table,cmd);

			gClearCache(gGetCacheTable(iDatabase,table));
		}	
		
			
		if (rc)
		{
			if (strnicmp(msg,"no such table",13))
			{
				if (cLFP)
				{
					fprintf(cLFP, "<ERROR> <![CDATA[%s]]> </ERROR>\n",msg);
				}
			}

		}

		free (cmd);
	}
	else //If the command was some kind of select, don't clear the cache
		if (strnicmp(cmd,"SELECT",6))
			if (IsObj(iv->iDatabase))
				gClearCache(iDatabase);

	gDispose(buf);

	return 0;
}



imeth	int	gExecute (char *cmd)
{
	//first make sure it worked
	int ret=gExecute(iStatement, cmd);

	if (!ret)
	{
		resetStmt(self);


		conditionalClearCache(self,cmd);

		iUsingCache=0;
	}
	 
	return ret;
}


/* This function makes a blank row of CacheStatementInfo objects
	It's needed for inserts or queries that returned no rows
	cmd - sql statement being executed
*/
private imeth int makeBlankRow(char * cmd)
{
	object columns,dbTable,sit,seq;
	int colNum=0;


	if (iCols)
		gDeepDispose(iCols);
	if (iColList)
		gDispose(iColList);

	iCols = gNewWithInt(StringDictionary, 101); //I'm expecting to have a column object
	iColList = gNew(LinkObject);
				
	iRecordLength = 0;
	//get the columns from the iDatabase
	dbTable=gGetTable(iDatabase,iTable);

	
	columns=gColumns(dbTable);  //this is a string dictionary of column info objs

				
				
	//spin through each column in the table
	for (seq=gSequence(columns) ; IsObj(seq) && ( sit = gNext(seq)) ; )
	{
		
		object si=gValue(sit);
		
		char * name=lcase(gName(si));	

		object statementinfo;

		RETCODE r;
		int size;

		SWORD colType=gType(si);

		int nulls=gIsNullable(si);

		if (cmd&&(colType==SQL_DATE||colType==SQL_TIME||colType==SQL_TIMESTAMP))
		{
			//if any of the column types are date, time, or datetime, fall back in case the query
			//was using date functions that aren't supported by sqllite

			char * pos=strstr(lcase(cmd),name);

			//only do fallback if this column was in the query
			if (strstr(lcase(cmd),name))
			{
 
				gDispose(seq);
				return 1;
			}

		}
	
		
		statementinfo=gNewCacheStatementInfo(CacheStatementInfo, self, NULL, colNum++, name, colType,gSize(si), &r, &size,(SWORD)nulls);
		
		gAddStr(iCols, name, statementinfo);
		gAddLast(iColList, statementinfo);
		iRecordLength+=size;
	}

	return 0;

}


/* this function does the actual selects against sqlite
	cmd - sql statement to execute
*/
private imeth int internalSelect(char *cmd)
{

	//first get the tables

	object seq;
	object tbl;


	int ready=1;
	object nonCacheStmt;
	object cachetable;

	sqlite3_stmt *sqlLiteStmt;
	

	//Tilde is being sent in as an apostrophe substitute, need to change it

	char * cmdx=fix_statement(cmd, DBMS_MSSQL);

	char *upperCmd=malloc(strlen(cmdx)+1);
	char *upperCmdStart=upperCmd;
	strcpy(upperCmd,cmdx);
	upperCmd=strupr(upperCmd);

	iSingleTable = get_table(iTable, cmdx, &iTableList);

	
	resetStmt(self);
	
	//see if we can use the cache - ready value > 0
	//this was conditional, but then I discovered return value is table and then list of tables that doesn't have first one
	
	nonCacheStmt=gNewNonCacheStatement(iDatabase);
		
	cachetable=gGetCacheTable(iDatabase,iTable);
	
	ready=(gMakeReady(cachetable,nonCacheStmt,iDB)>0);
	
	gDispose(nonCacheStmt);
	
	if (!iSingleTable&&ready) //we have multiple tables, make sure they are all ready
	{
		for (seq=gSequence(iTableList) ; tbl = gNext(seq) ; )//check each table
		{
			int tready=0;

			nonCacheStmt=gNewNonCacheStatement(iDatabase);
			
			cachetable=gGetCacheTable(iDatabase,gStringValue(tbl));
		
			tready=gMakeReady(cachetable,nonCacheStmt,iDB);
		
			gDispose(nonCacheStmt);
			
			if (tready<1) //if this table isn't ready, the whole query isn't ready
				ready=0;
			
			
		}

	}

	//see if I have any subqueries that would acuase me to want to skip the cache
	upperCmd+=6; //skip this select for next iteration

	while ((upperCmd=strstr(upperCmd,"SELECT "))&&ready)
	{
		char table[100];
		object tableList=NULL;

		get_table(table, upperCmd, &tableList);


		nonCacheStmt=gNewNonCacheStatement(iDatabase);
		
		cachetable=gGetCacheTable(iDatabase,table);
		
		ready=(gMakeReady(cachetable,nonCacheStmt,iDB)>0);
		
		gDispose(nonCacheStmt);

		if (tableList)
			for (seq=gSequence(tableList) ; tbl = gNext(seq) ; )//check each table
			{
				int tready=0;

				nonCacheStmt=gNewNonCacheStatement(iDatabase);
				
				cachetable=gGetCacheTable(iDatabase,gStringValue(tbl));
			
				tready=gMakeReady(cachetable,nonCacheStmt,iDB);
			
				gDispose(nonCacheStmt);
				
				if (tready<1) //if this table isn't ready, the whole query isn't ready
					ready=0;
				
				
			}

		if (tableList)
			gDispose(tableList);

		upperCmd+=6;  //skip this select for next iteration
	}

	free(upperCmdStart);
	
	//save the command in case I need it later

	iSqlCmd=getStringMemory(cmdx);
	
	Strcpy(iSqlCmd,cmdx);
	
	//if all the tables are ready, use the cache
	if (ready>0)
	{
		

		
		int rc= sqlite3_prepare(iDB,            /* Database handle */
								cmdx,       /* SQL iStatement, UTF-8 encoded */
								-1,             /* Length of zSql in bytes.  -1 means to first terminator*/
								&sqlLiteStmt,  /* OUT: Statement handle */
								NULL     /* OUT: Pointer to unused portion of zSql */
								);


		if (rc!=SQLITE_OK)
		{
			iUsingCache=0;
			sqlite3_finalize(sqlLiteStmt);
			sqlLiteStmt=NULL;
			//otherwise hit the normal iDatabase
			return 1;
		}

		iRecordsRead=0;
		iPos=0;

		rc=sqlite3_step(sqlLiteStmt); 



		if ((rc!=SQLITE_ROW)) //This query syntax failed or returned no rows, try regular iDatabase
		{
			if (!iSingleTable&&rc!=SQLITE_DONE)  //this may be too complex for sql lite, so fall back
			{
				iUsingCache=0;
				sqlite3_finalize(sqlLiteStmt);
				sqlLiteStmt=NULL;
				//otherwise hit the normal iDatabase
				return 1;
			}
			else  //got a good select, but no rows returned
			{
				
				iUsingCache=1;
				iTType=TYPE_SELECT;
	

				if (makeBlankRow(self,cmdx)) //need a blank row for some features
				{
					iUsingCache=0;  //blake row create failed, fall back to normal statement
					sqlite3_finalize(sqlLiteStmt);
					sqlLiteStmt=NULL;
					free(cmdx);
					return 1;
				}
				free(cmdx);
				sqlite3_finalize(sqlLiteStmt);
				sqlLiteStmt=NULL;
				return 0;
			}
			
		}
		
		free(cmdx);

		rc = bindCols(self,sqlLiteStmt);

		//check the return code and if there was an error, fall back to normal iStatement
		if ((rc==SQLITE_ERROR)||(rc==SQLITE_MISUSE)||(rc==SQLITE_BUSY))
		{
			sqlite3_finalize(sqlLiteStmt);
			sqlLiteStmt=NULL;
			return 1;
		}

		
		//store results in cursor file	
		do {
			object si;
	
		
			cursor_write(self, iPos);
			iPos++;
			rc = sqlite3_step(sqlLiteStmt);
			if (rc==SQLITE_ROW)  //only do it if on a good row
				for (seq=gSequence(iColList) ; si = gNext(seq) ; )
					gLoadColumnData(si);
		} while (rc==SQLITE_ROW);


	
		//check the return code and if there was an error, fall back to normal iStatement
		if ((rc==SQLITE_ERROR)||(rc==SQLITE_MISUSE)||(rc==SQLITE_BUSY))
		{
			sqlite3_finalize(sqlLiteStmt);
			sqlLiteStmt=NULL;
			return 1;
		}


		iUsingCache=1;

		iPos=0;

		sqlite3_finalize(sqlLiteStmt);
		sqlLiteStmt=NULL;
		
		return 0;
	}	
	else  //tables weren't ready
	{
		free(cmdx);
		//otherwise hit the normal iDatabase
		return 1;
	}

}


/* This function selects the current row from a sqlite query
   as the current row in the real database.  That way I can do
   updates and deletes.
   tableName-the current table in the query
*/
private imeth int selectCurrentRow(char * tableName)
{

	int ret;

	object where=getCurrentWhereClause(self,tableName);
	
	char *buf=malloc(35+strlen(tableName)+gSize(where));
	
	sprintf(buf,"SELECT * FROM %s %s",tableName,gStringValue(where));

	ret= gDBSelectOne(iStatement,buf);

	if (!ret)
		iLoadedCurrentRow=1;

	free (buf);
	gDispose(where);

	return ret;

}

extern	char	*Getenv(char *);


private imeth int dumpRows(object stmt)
{
	object	si, seq;

	gFirstRecord(stmt);
	gPrevRecord(stmt);

	while (!gNextRecord(stmt))
	{
		fprintf(cLFP,"<ROW>\n");
		for (seq = gSequence(iCols) ; si = gNext(seq) ; )
		{
			char b1[512];
			char b2[512];
			object sqlite;
			object sqlserver;
			object val=gValue(si);
			char * nam=gName(val);

			sqlserver=gFindValueStr(gColumnDictionary(stmt),nam);

			if (IsObj(sqlserver))
			{

				gFormatOrgField(sqlserver, b2);

				fprintf(cLFP,"<%s><![CDATA[%s]]></%s>\n",nam,b2,nam);
			}
			else
			{
				fprintf(cLFP,"<NONOBJECT/>");
			}

		}	
		fprintf(cLFP,"</ROW>\n");
	}

	gFirstRecord(stmt);
	gPrevRecord(stmt);

	return 0;

}


private imeth int testResults(char * cmd)
{

	char	*en = Getenv("INTEGRA_DATABASE_CACHE_VALIDATE"); 
	int checkCount=0;

		/* Compatibility tester */
	
	
	if (en&&(stricmp(en,"Y")==0)) //check to see if I'm using cache
	{
		

		if (!gDBSelect(iStatement, cmd))
		{
			

			char colname[256];
			char value[256];
			char ovalue[256];
			char found=1;


			//check for same number of result rows
			while (!gNextRecord(iStatement))
			{
				checkCount++;
			}


			if (checkCount!=iRecordsRead)
			{
				if (cLFP)
				{

					fprintf(cLFP,"<ERROR>\n");
					fprintf(cLFP, "<![CDATA[Different result count (SS %d, SL %d) returned for %s ]]>\n",checkCount,iRecordsRead,cmd);

					fprintf(cLFP, "<SQLSERVER>\n");
					if (checkCount)
						dumpRows(self,iStatement);
					fprintf(cLFP, "</SQLSERVER>\n");
					fprintf(cLFP, "<SQLITE>\n");
					if (iRecordsRead)
						dumpRows(self,self);
					fprintf(cLFP, "</SQLITE>\n");
		//			fprintf(cLFP, "<STACKDUMP> <![CDATA[%s]]></STACKDUMP>  \n",gStringValue(gGetStackDumpText(gGetTracer(StackTracer))));
					fprintf(cLFP,"</ERROR>\n");
				
				}
			}

			gFirstRecord(iStatement);
			gPrevRecord(iStatement);

			//see if all columns for every row have the same thing

			while (!gNextRecord(iStatement))
			{
				object	si, seq;
				

				gFirstRecord(self);
				gPrevRecord(self);

				while (!gNextRecord(self))
				{
					found=1;
					for (seq = gSequence(iCols) ; si = gNext(seq) ; )
					{
						char b1[512];
						char b2[512];
						object sqlite;
						object sqlserver;
						object val=gValue(si);
						char * nam=gName(val);

						sqlite=gFindValueStr(iCols,nam);
						sqlserver=gFindValueStr(gColumnDictionary(iStatement),nam);

						if (!sqlserver)
							continue;

						gFormatOrgField(sqlite, b1);
						gFormatOrgField(sqlserver, b2);
						if (strcmp(b1,b2))
						{
							char *comp1=&(b1[1]);
							char *comp2=&(b2[1]);
							//ok, this may just be different length empty strings
							//if so, I really don't care  
							
							while (*comp1&&(*comp1==' '||*comp1=='~'))
								comp1++;
							
							while (*comp2&&(*comp2==' '||*comp2=='~'))
								comp2++;

							if (strcmp(comp1,comp2))  
							{
								//they don't match
								found=0;
								Strcpy(colname,nam);
								Strcpy(value,b1);
								Strcpy(ovalue,b2);
								break;
							}
						} 

					}

					if (found)
					{
						//I got a match
						break;
					}
				
				
				}

				if (found)
				{
					//I got a match
					break;
				}

			}

			if (!found)
			{
				//I didn't find a row that matched 
				
					char msg[4096];

					Strcpy(msg,"No match found in query ");
					Strcat(msg,cmd);
					Strcat(msg,"| column is ");
					Strcat(msg,colname);
					Strcat(msg,"| value is ");
					Strcat(msg,value);
					Strcat(msg,"| last compared value is ");
					Strcat(msg,ovalue);
					//nothing left and nothing matched
					
					if (cLFP)
					{
						fprintf(cLFP, "<ERROR><![CDATA[%s]]> \n",msg);
			//			fprintf(cLFP, "<STACKDUMP> <![CDATA[%s]]></STACKDUMP>  \n",gStringValue(gGetStackDumpText(gGetTracer(StackTracer))));
						fprintf(cLFP, "</ERROR> \n",msg);
					}
			}

			

		}
		gFirstRecord(iStatement);
		gFirstRecord(self);
		gPrevRecord(self);

	}

	return 0;

}


imeth	int	gDBSelect, gExecuteWithBind (char *cmd)
{	

	if (internalSelect(self,cmd))   //if internal fails, pass to regular iStatement
		return gDBSelect(iStatement, cmd);

	testResults(self,cmd);
	

	return 0;

}


imeth	int	gDBSelectOneDNC(char *cmd)
{

	resetStmt(self);
	iUsingCache=0;
	return gDBSelectOne(iStatement, cmd);
}

imeth	int	gDBSelectDNC(char *cmd)
{

	resetStmt(self);
	iUsingCache=0;
	return gDBSelect(iStatement, cmd);
}

imeth	int	gDBSelectNC(char *cmd)
{
	if (internalSelect(self,cmd))   //if internal fails, pass to regular iStatement
		return gDBSelectNC(iStatement, cmd);

	testResults(self,cmd);
	

	return 0;

}

imeth	int	gLazyDBSelect, gLazyExecuteWithBind (ifun fun, char *cmd)
{
	conditionalClearCache(self,cmd);
	resetStmt(self);
	return gLazyDBSelect(iStatement,fun,cmd);
}

imeth	int	gLazyDBSelectOne(ifun fun, char *cmd)
{
	resetStmt(self);
	return gLazyDBSelectOne(iStatement,fun,cmd);
}


imeth	int	gDBSelectOne(char *cmd)
{
	
	if (internalSelect(self,cmd)) //there isn't an equivilent to this in our cache iDatabase	
		return gDBSelectOne(iStatement, cmd);

	testResults(self,cmd);

	
	return gNextRecord(self);
	
}

imeth	int	gDBSelectOneNC(char *cmd)
{
	if (internalSelect(self,cmd))   //if internal fails, pass to regular iStatement
		return gDBSelectOneNC(iStatement, cmd);

	testResults(self,cmd);

	return 0;
}


ivmeth	int	vDBSelectDNC(char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);
	iUsingCache=0;

	vsprintf(buf, fmt, _rest_);
	r = gDBSelect(iStatement, buf);
	free(buf);

	return r;
}

imeth int gRecordCount()
{
	return iRecordsRead;
}


ivmeth	int	vDBSelectOneDNC(char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	int	r;
	MAKE_REST(fmt);
	iUsingCache=0;

	vsprintf(buf, fmt, _rest_);
	r = gDBSelectOne(iStatement, buf);
	free(buf);

	return r;
}

#define	GO_END								\
	while (*p) {							\
		p++;							\
		if (++n > QUERY_BUFFER_SIZE + iRecordLength)		\
			vError(self, "QueryBuffer overflow");		\
	}


/* This function builds a where clause based on the current sqlite row
	tableName - name of the current query table
*/
private imeth getCurrentWhereClause(char * tableName) 
{
	int ret=0;

	object where;

	char	*p, *w;
	char	buf[1024];
	object	pk, kseq, fld,valStr;
	int	add_comma = 0, n=0;
		
	pk = gGetPrimaryKey(iDatabase, tableName);
	if (!pk  ||  !gSize(pk))
		if (iIgnoreAllErrors)
			return NULL;
		else
			vError(self, "Can't update %s when no primary key declared", tableName);


	where = gNewWithStr(String, " WHERE ");

	p=buf;
	
	GO_END;
	//spin all the keys
	for (kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;
		p=buf;

		buf[0]=0;

		if (add_comma) {
			gWrite(where, " AND ",strlen(" AND "));
			GO_END;
		} else
			add_comma = 1;

		sf=gStringValue(fld);

		gWrite(where, sf,strlen(sf));
		gWrite(where, " = ",strlen( " = "));

		GO_END;

		valStr=gFindValueStr(iCols, sf);
		if (valStr)  
		{
			gFormatOrgField(valStr, p);

			gWrite(where,p,strlen(p));
		}
		else
			gWrite(where, " NULL ",strlen(" NULL "));
		GO_END;
	}
	

	return where;
}





cvmeth	vNew(dbx, fht, fhts, long userID)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	static	char	err[] = "vNew::Statement Error";

	DEMO_CHK_MAX;

#ifndef	_WIN32
	TableInfo;	// to fix some sort of link bug with MSC 16 bit 
#endif


	iFH_Table = fht;
	iFH_Tables = fhts;
	iUserID = userID;
	if (iTP = gGetTP(dbx))
		iDataSync = 1;
	



	cNumbStatements++;
	return obj;
}

imeth	object	gGCDispose : GCDispose()
{
	resetStmt(self);
	cNumbStatements--;

	if (IsObj(iCols))
		gDeepDispose(iCols);

	if (IsObj(iColList))
		gDeepDispose(iColList);

	if (IsObj(iStatement))
		gDispose(iStatement);

	return gDispose(super);
}




private	imeth	pUnattachDialog(object self, object dlg, int remove_from_list)
{
	
/*
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
	*/
	return self;
	
}

imeth	object	gDispose, gDeepDispose ()
{
	resetStmt(self);

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

	

	return NULL;
}

cmeth	int	gSize()
{
	return gSize(Statement);
}

imeth	HSTMT	gHSTMT()
{
	return gHSTMT(iStatement);
}


imeth	char	*gGetErrorMessage()
{
	if (!iUsingCache)
		return gGetErrorMessage(iStatement);

	return (char *)(sqlite3_errmsg(iDB));

}

imeth	char	*gGetErrorState()
{

	if (!iUsingCache)
		return gGetErrorState(iStatement);
	
	return (char *)(sqlite3_errmsg(iDB));
}

ivmeth	vError(char *fmt, ...)
{
	char	*buf = Tnalloc(char, BUF_SIZE);
	object	r;
	MAKE_REST(fmt);
	iUsingCache=0;

	vsprintf(buf, fmt, _rest_);
	r = gError(Object, buf);
	free(buf);

	return r;
}

cmeth	char	*gGetErrorMessage : c_emsg()
{
	return gGetErrorMessage(Statement);
}

imeth	int	gGetErrorCode()
{
	if (!iUsingCache)
		return gGetErrorCode(iStatement);

	return sqlite3_errcode(iDB);
}

cmeth	int	gGetErrorCode : c_ecode ()
{
	return gGetErrorCode(Statement);

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


private	imeth	void	updateOrgValues()
{
	object	si, seq, pk, key;


	for (seq = gSequence(iCols) ; si = gNext(seq) ; )
		gUpdateOriginalValue(gValue(si));

	iVarTextChanged = 0;
}

private	imeth	void	cursor_write(int rec)   //  rec is zero origin
{
	object	seq, si;

	if (!iCursorFile)
		if (!(iCursorFile = gOpenTempFile(BufferedTempFile)))
			gError(Object, "Error opening ODBC cursor file");
	if (!gSeek(iCursorFile, rec * iRecordLength) &&  rec)
		gError(Object, "ODBC Cursor seek error.");
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (!gCursorWrite(si, iCursorFile))
			gError(Object, "ODBC Cursor write error.");
	if (rec == iRecordsRead)
		iRecordsRead++;


	iLoadedCurrentRow=0;
}

private	imeth	void	cursor_read(int rec)   //  rec is zero origin
{
	object	seq, si;
	
	if (!iCursorFile)
		return;

		iLoadedCurrentRow=0;

	if (!gSeek(iCursorFile, rec * iRecordLength)  &&  rec)
		gError(Object, "ODBC Cursor seek error.");
	for (seq=gSequence(iColList) ; si = gNext(seq) ; )
		if (!gCursorRead(si, iCursorFile))
			gError(Object, "ODBC Cursor read error.");
}


/* this function is used to parse sql statements to find "real" column names
*/
char * backUpAWord(char *startpos,char *stringStart)
{
	//eat spaces backwards
	char *p=startpos;

	do
	{
		--p;
	}
	while ((*p==' '||*p==')')&&p!=stringStart);

	if (p==stringStart)
		return startpos;


	//ok p should be pointing to a non comma
	if (*p!=',')
	{
		//mark this end spot and back up until I get a space or comma
		char * end=p;
		

		do
		{
			p--;
		}
		while ((*p!=','&&*p!=' '&&*p!='.')&&p!=stringStart);
		

		//now I'm pointing one character before where I want to be
		p++;

	

	}
	return p;
}


/* this function is used to parse sql statements to find "real" column names
*/
char * getThisWord(char * p)
{
	char * buf=getStringMemory(p);
	int index=0;
	char * retVal;
	
	//p is now pointing at start of word and end is pointing at end
		
	while (*p&&*p!=' '&&*p!='.'&&*p!=')'&&*p!='(')
	{
		buf[index++]=*p;
		p++;
	}

	//null terminate
	buf[index++]=0;

	return buf;
}

/* this function is used to parse sql statements to find "real" column names
*/
private imeth char * findWordBefore(char *col)
{

	
	char * retVal=NULL;
	char * startpos=NULL;
	char * temp=getStringMemory(iSqlCmd);
	Strcpy(temp,iSqlCmd);
	temp=lcase(temp);
	startpos=strstr(temp,col);
		

	if (startpos)
	{
		startpos=backUpAWord(startpos,temp);
		
		if (!startpos)
			return NULL;

		retVal=getThisWord(startpos);

		if (!stricmp(retVal,"as")) //I got back as, so go back some more
		{	
			free(retVal);
			startpos=backUpAWord(startpos,temp);
			retVal=getThisWord(startpos);
		}

	}
	return retVal;
}

private	imeth	int	bindCols(sqlite3_stmt *sqlLiteStmt)
{
	RETCODE r=0;
	SWORD	n, type, scale, nulls;
	UWORD	i;
	UDWORD	prec=0;

	object	si;
	int	size;


	n=sqlite3_column_count(sqlLiteStmt);

	iCols = gNewWithInt(StringDictionary, 101);
	iColList = gNew(LinkObject);
	iRecordLength = 0;
	for (i=0 ; i < n ; i++)  
	{


		object table,seq;

		char * cname=(char *)(sqlite3_column_name(sqlLiteStmt,i));


		//get the columns from the iDatabase
		object dbTable=gGetTable(iDatabase,iTable);

			
		object columns=gColumns(dbTable);  //this is a string dictionary of column info objs

		object ci=gFindValueStr(columns,lcase(cname));


		if (iTableList&&!ci)
		{
			for (seq=gSequence(iTableList) ; !ci  &&  (table = gNext(seq)) ; )
			{

				//get the columns from the iDatabase
				object dbTable=gGetTable(iDatabase,gStringValue(table));

					
				object columns=gColumns(dbTable);  //this is a string dictionary of column info objs

				ci=gFindValueStr(columns,lcase(cname));


			}
			if (IsObj(seq))
				gDispose(seq);
		}


		if (ci)
		{
			type=gType(ci);
			prec=gSize(ci);	
			nulls=gIsNullable(ci);
		}
		else
		{
			//I don't know what this is because they probably renamed the column in the select
			//parse the query and find the real column name
								
			short found=0;		
			char *colName=findWordBefore(self,cname);

			if (colName)
			{
				//see if I can find this column name

				ci=gFindValueStr(columns,lcase(colName));

				if (iTableList&&!ci)
				{
					for (seq=gSequence(iTableList) ; !ci  &&  (table = gNext(seq)) ; )
					{

						//get the columns from the iDatabase
						object dbTable=gGetTable(iDatabase,gStringValue(table));

							
						object columns=gColumns(dbTable);  //this is a string dictionary of column info objs

						ci=gFindValueStr(columns,lcase(colName));


					}
					if (IsObj(seq))
						gDispose(seq);
				}


				free(colName);
				
				if (ci)
				{
					type=gType(ci);
					prec=gSize(ci);	
					nulls=gIsNullable(ci);
					found=1;
				}
	

			}
			
		
			if (!found)
			{

				//Ok, try just using the sqllite type

				int sqllitetype=sqlite3_column_type(sqlLiteStmt, i);
				nulls=1;

				//this should return an object of the appropriate type for the data

				switch(sqllitetype)
				{


					case SQLITE_TEXT:
						type=SQL_VARCHAR;
						prec=512;  //512 is just a guess, but so far, this never gets hit
						break;
					case SQLITE_INTEGER:
						type=SQL_INTEGER;
						break;
					case SQLITE_FLOAT:
						type=SQL_FLOAT;
						break;
					default:
						return SQLITE_ERROR;
				

				}
			}
		}		
	

		////////////////////////////////////////////////
	
		si = gNewCacheStatementInfo(CacheStatementInfo, self, sqlLiteStmt, (int) i+1, lcase(cname), type, prec, &r, &size,(SWORD)nulls);

		if (r  ||  !si)
			if (iIgnoreAllErrors) {
				iErrorState = 1;
				free (cname);
				return -1;
			} else
				vError(self, "ERROR in Cache");
		gAddStr(iCols, cname, si);
		gAddLast(iColList, si);


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







	

private	imeth	int	DBSelectOne(char *cmd, int useCursors)
{
	UDWORD	crow;
	int	r = DBSelect(self, cmd, useCursors);
	if (r)
		return r;
	if (iErrorState)
		return -1;

	if (iRecordsRead) {
		cursor_read(self, 0);
		iPos = 1;
		iEof = 0;
		updateOrgValues(self);
	} else
		return SQL_NO_DATA_FOUND;
	

	return r;
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





imeth	int	gInsert(char *table)
{
	//The actual insert into cache db will happen via the Execute method

	
	iUsingCache=0;


	return gInsert(iStatement,table);

}

static	void	updateFields(ivType *iv)
{
	object	seq, i;

	for (seq = gSequence(iCols) ; i = gNext(seq) ; )
		gUpdate(gValue(i));
}

imeth	gClear()
{
	if (!iUsingCache)
		return gClear(iStatement);
	else
	{

		object	seq, i;
		
		//expressing an empty row is equivelent to loading a row
		iLoadedCurrentRow=1;
		selectCurrentRow(self,iTable);

		gClear(iStatement);//always clear statement too
		
		if (iCols)
			for (seq = gSequence(iCols) ; i = gNext(seq) ; )
				if (IsObj(gValue(i)))
					gClear(gValue(i));

		iVarTextChanged = 0;
		return self;
	}
}



imeth	int	gAddRecord()
{
/*	The gAddRecord from Statement will call my gExecute which will do this
	so I don't have to do anything here but pass it up the line
*/
	iUsingCache=0;

	/*
		// read all my values and set them in statement
		//I have to do this because some things like controls
		//update the info objects without going through this class

		if (iCols)
		{
			object	seq, i;
	
			for (seq = gSequence(iCols) ; i = gNext(seq) ; )
			{
				//find this value from iStatement
				char *fldName=gStringKey(i);
				gChangeValue(gFldGetValue(iStatement,fldName), gFldGetValue(self,fldName));
			}
		}
	*/

	return gAddRecord(iStatement);
	
}

imeth	int	gAddRecordWithAutoInc(char *table, char *field, char *where, char *order)
{
	iUsingCache=0;
	return gAddRecordWithAutoInc(iStatement, table, field, where, order);
	
}



imeth	gGetField(char *fld)
{

	if (!iUsingCache)
		return gGetField(iStatement,fld);

	return iCols ? gFindValueStr(iCols, lcname(fld)) : NULL;
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


imeth	int	gDoLazyLoad()
{
	return pDoLazyLoad(self);
}


imeth	gFldGetValue(char *fld)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetValue(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	return si ? gGetValue(si) : si;
}

imeth	void	*gFldGetPointer(char *fld)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetPointer(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	return si ? gPointerValue(si) : NULL;
}

imeth	char	*gFldGetString(char *fld)
{
	char	*cp;
	char	*vttbl;
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetString(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	if (!si)
		return "";

	if (vttbl = pGetVarTextTable(self, si, fld))
		cp = pFldGetVarText(self, si, fld, vttbl);
	else
		cp = gStringValue(gStripRight(gNewWithStr(String,gStringValue(si))));

	return cp;
}

imeth	char	*gFldGetStringXML(char *fld)
{
	char	*cp;
	char	*vttbl;
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetStringXML(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	if (vttbl = pGetVarTextTable(self, si, fld))
		pFldGetVarText(self, si, fld, vttbl);
	cp = gStringValueXML(si);

	return cp;
}

imeth	char	*gFldToFile(char *fld, char *file)
{
	char	*cp;
	int	h;
	object	si=NULL;

	if (!iUsingCache)
		return gFldToFile(iStatement,fld,file);

	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

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
	object	val, si=NULL;

	if (!iUsingCache)
		return gFldGetChar(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));
	if (si)
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
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetShort(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");

	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	return si ? gShortValue(gGetValue(si)) : 0;
}

imeth	unsigned short	gFldGetUnsignedShort(char *fld)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetUnsignedShort(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	return si ? gUnsignedShortValue(gGetValue(si)) : 0;
}

imeth	long	gFldGetLong(char *fld)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetLong(iStatement,fld);


	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");

	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));


	return si ? gLongValue(gGetValue(si)) : 0L;
}

imeth	double	gFldGetDouble(char *fld)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetDouble(iStatement,fld);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");


	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));

	return si ? gDoubleValue(gGetValue(si)) : 0;
}

imeth	gFldGetDateTime(char *fld, long *dt, long *tm)
{
	object	si=NULL;

	if (!iUsingCache)
		return gFldGetDateTime(iStatement,fld,dt,tm);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "DB field access without select");

	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));
	if (si)
		gDateTimeValues(gGetValue(si), dt, tm);
	return si ? self : NULL;
}

imeth	gFldSetString(char *fld, char *str)
{
	

	char	*vttbl;
	object	si;
	int status=0;

	if (!iUsingCache)
		return gFldSetString(iStatement,fld,str);
	
	if (iErrorState)
		return self;

	if (!strcmp(gFldGetString(self,fld),str))
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetString(iStatement,fld,str);
	

	
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
	return self;
}

imeth	gFldSetFromFile(char *fld, char *file)
{
	char	*vttbl, *str;
	object	si;
	int	h, status=0;
	struct	stat	sb;

	if (!iUsingCache)
		return gFldSetFromFile(iStatement,fld,file);

	if (iErrorState)
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetFromFile(iStatement,fld,file);

	
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
	return self;
}

imeth	gFldSetDateTime(char *fld, long dt, long tm)
{
	object	si=NULL;
	int oldDt,oldTm,status=0;

	if (!iUsingCache)
		return gFldSetDateTime(iStatement,fld,dt,tm);

	if (iErrorState)
		return self;

	gFldGetDateTime(self,fld,&oldDt, &oldTm);

	if (dt==oldDt&&tm==oldTm)  //if the set didn't change anything just return
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetDateTime(iStatement,fld,dt,tm);


	LAZY_LOAD;
	
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");

	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetDateTime: No field '%s'", fld);
	gChangeDateTimeValues(gGetValueToPut(si), dt, tm);
	gUpdate(si);	//  for dates and times
	return self;
}

imeth	gFldSetChar(char *fld, int val)
{
	object	obj, si=NULL;
	int status=0;

	if (!iUsingCache)
		return gFldSetChar(iStatement,fld,val);

	if (iErrorState)
		return self;

	if (val==gFldGetChar(self,fld))
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetChar(iStatement,fld,val);

	
	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	if (iCols)
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
	return self;
}

imeth	gFldSetShort(char *fld, int val)
{
	object	si;
	int status=0;

	if (!iUsingCache)
		return gFldSetShort(iStatement,fld,val);


	if (iErrorState)
		return self;

	if (val==gFldGetShort(self,fld))
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetShort(iStatement,fld,val);


	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetShort: No field '%s'", fld);
	gChangeShortValue(gGetValueToPut(si), val);
	return self;
}

imeth	gFldSetUnsignedShort(char *fld, unsigned val)
{
	object	si;
	int status=0;

	if (!iUsingCache)
		return gFldSetUnsignedShort(iStatement,fld,val);

	if (iErrorState)
		return self;

	if (val==gFldGetUnsignedShort(self,fld))
		return self;
	
	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetUnsignedShort(iStatement,fld,val);


	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetUnsignedShort: No field '%s'", fld);
	gChangeUShortValue(gGetValueToPut(si), val);
	return self;
}

imeth	gFldSetLong(char *fld, long val)
{
	object	si=NULL;
	int status=0;
	
	if (!iUsingCache)
		return gFldSetLong(iStatement,fld,val);

	if (iErrorState)
		return self;

	//if this is the same value I already have, then just return
	if (val==gFldGetLong(self,fld))
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetLong(iStatement,fld,val);

	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetLong: No field '%s'", fld);
	gChangeLongValue(gGetValueToPut(si), val);
	gUpdate(si);	//  for dates and times
	return self;
}

imeth	gFldSetDouble(char *fld, double val)
{
	object	si=NULL;
	int status=0;

	if (!iUsingCache)
		return gFldSetDouble(iStatement,fld,val);

	if (iErrorState)
		return self;

	//if this is the same value I already have, then just return
	if (val==gFldGetDouble(self,fld))
		return self;

	if (!iLoadedCurrentRow)
		status=selectCurrentRow(self,iTable);

	if (!status)
		gFldSetDouble(iStatement,fld,val);


	LAZY_LOAD;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	if (iCols)
		si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetDouble: No field '%s'", fld);
	if (_isnan(val))
		vError(Object, "gFldSetDouble: Field '%s' passed NAN", fld);
	gChangeDoubleValue(gGetValueToPut(si), val);
	return self;
}
	
#define	GO_END								\
	while (*p) {							\
		p++;							\
		if (++n > QUERY_BUFFER_SIZE + iRecordLength)		\
			vError(self, "QueryBuffer overflow");		\
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
	Strcpy(p=buf=query_buffer(iRecordLength), "SELECT ");
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		if (add_comma) {
			Strcpy(p, ", ");
			GO_END;
		} else
			add_comma = 1;
		Strcpy(p, gStringValue(fld));
		GO_END;
		(*fn)++;
	}
        sprintf(p, ", %s FROM %s WHERE ", bfld, iTable);
	GO_END;
	for (add_comma=0, kseq=gSequence(pk) ; fld = gNext(kseq) ; ) {
		char	*sf;

		if (add_comma) {
			Strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		Strcpy(p, sf=gStringValue(fld));
		Strcat(p, " = ");
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
			Strcpy(p, " AND ");
			GO_END;
		} else
			add_comma = 1;
		Strcpy(p, sf=gStringValue(fld));
		Strcat(p, " = ");
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
	char *t;
	
	if (iErrorState)
		return iErrorState;
	if (iTType != TYPE_SELECT)
		gError(Object, "Set DB field without select");
	if (iReadOnly)
		return 0;
	si = gFindValueStr(iCols, lcname(fld));
	if (!si)
		vError(Object, "gFldSetBinary: No field '%s'", fld);
	stmt = gNewNonCacheStatement(iDatabase);
	gEnterCriticalSection(iDatabase);
	cmd = make_update_field(self, fld);
	t=fix_statement(cmd, gDBMS_type(iStatement));
	r = SQLPrepare(h=gHSTMT(stmt), t, SQL_NTS);
	free(cmd);
	free(t);
	if (!r) {
		SDWORD	pcbVal = SQL_LEN_DATA_AT_EXEC(size);
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
	return r;
}




imeth	int	gFldGetBinary(char *fld, long *size, char **val)
{
	object	si, stmt;
	char	*cmd, c;
	int	r, fn;
	HSTMT	h;
	SDWORD	pcbVal;
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
	stmt = gNewNonCacheStatement(iDatabase);
	gEnterCriticalSection(iDatabase);
	cmd = make_select_field(self, fld, &fn);
	t=fix_statement(cmd, gDBMS_type(iStatement));
	r = SQLExecDirect(h=gHSTMT(stmt), t, SQL_NTS);
	free(cmd);
	free(t);
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


imeth	int	gDeleteRecord()
{

	if (!iUsingCache)
		return gDeleteRecord(iStatement);
	else
	{
		char * cmd;
		object seq, node;
		int ret;

		
		iSingleTable = get_table(iTable, iSqlCmd, &iTableList);



		ret=selectCurrentRow(self,iTable);

		if (!ret)
			return gDeleteRecord(iStatement);
		else
			return ret;
	}
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

private imeth void moveColumn(object fromStmt,object toStmt, object columnInfo, char * name)
{
	long dt,tm;

	switch (gType(columnInfo))
	{
		case SQL_WLONGVARCHAR:
		case SQL_LONGVARCHAR:
		case SQL_WCHAR:
		case SQL_WVARCHAR:
		case SQL_CHAR:
		case SQL_VARCHAR:
			gFldSetString(toStmt,name,gFldGetString(fromStmt,name));
			break;
		case SQL_BIT:
		case SQL_TINYINT:
		case SQL_SMALLINT:
			gFldSetShort(toStmt,name,gFldGetShort(fromStmt,name));
			break;
		case SQL_INTEGER:
			gFldSetLong(toStmt,name,gFldGetLong(fromStmt,name));
			break;
		case SQL_REAL:
		case SQL_FLOAT:
		case SQL_DOUBLE:
		case SQL_NUMERIC:
			gFldSetDouble(toStmt,name,gFldGetDouble(fromStmt,name));
			break;
		case SQL_DATE:
		case SQL_TIME:
		case SQL_TIMESTAMP:
			gFldGetDateTime(fromStmt,name,&dt,&tm); 
			gFldSetDateTime(toStmt,name,dt,tm);				
			break;
	}


}

imeth	int	gUpdateRecord()
{
	char *update;
	char *x;
	int 	ret=0;
	int 	retval=0;
	
	if (!iUsingCache)
		return gUpdateRecord(iStatement);
	else
	{

		object seq, node;

	//  It's possible to change the values via control bindings
	//  this was keeping those changes from getting saved if 
	//  nothing else triggered the loaded flag
	//	if (!iLoadedCurrentRow)
	//		return 0; //nothing was set, so nothing was changed

		
		iSingleTable = get_table(iTable, iSqlCmd, &iTableList);

	
		if (!iLoadedCurrentRow)
			ret=selectCurrentRow(self,iTable);
			
		
		if (ret) //this is not an updateable row!
			return ret;


		//read all my values and set them in statement
		//I have to do this because some things like controls
		//update the info objects directly without going through this class
		if (iCols)
		{

			object dbTable=gGetTable(iDatabase,iTable);
			object seq, cit, columns;

			if (IsObj(dbTable))  //do I have info on this table from main db?
				columns=gColumns(dbTable);
			else //if not in main db, can't cache it, bail out
			{
				return gUpdateRecord(iStatement);
			}



			//get column definitions
			//for (seq=gSequence(columns) ; IsObj(seq) && ( cit = gNext(seq)) ; )

			//I was originally going through all the columns here, but that caused
			//a bug when not all columns were selected by the query - it blanked out
			//data in the columns that were not selected.
			//now I just go through the columns selected by the query which should
			//both be faster and correct
			for (seq=gSequence(iColList) ; IsObj(seq) && ( cit = gNext(seq)) ; )
			{
				object colobj=gFindStr(columns, gName(cit));
				
				if (colobj)
				{

					object ci=gValue(colobj);
					
					char * name=lcase(gName(ci));	
					char * vttbl;

					object statementInfo=gFindValueStr(iCols,name);

					object old = gOriginalValue(statementInfo);
					object newv = gGetValueToPut(statementInfo);
					if (!old  ||  !gCompare(old, newv)) 
						continue;

					//if this is a vartext field, set string, not int
					if (gGetColVarText(statementInfo))
					{
						gFldSetString(iStatement,name,gStringValue(gGetColVarText(statementInfo)));
						continue;
					}
					
					moveColumn(self,self,iStatement, ci, name);
					
				}
				else  //didn't find the column in the table definition
				{		//I have seen this happen when an update is done on a select that 
					//was for multiple tables.  I don't think that's legal, so we'll 
					//skip missing columns
		
					int x=0;  //this code is to aid in debugging
					char* missingColName=gName(cit);
					x++;
					
				}
				
			}
			
		}


		retval= gUpdateRecord(iStatement);

		//saving something can result in the statement values 
		//changing, especially with vartext


		if (iCols)
		{
			object seq, cit, columns;
			object dbTable=gGetTable(iDatabase,iTable);

			if (IsObj(dbTable))  //do I have info on this table from main db?
				columns=gColumns(dbTable);

			for (seq=gSequence(iColList) ; IsObj(seq) && ( cit = gNext(seq)) ; )
			{
				object colobj=gFindStr(columns, gName(cit));
				
				if (colobj)
				{
					object ci=gValue(colobj);
					
					char * name=lcase(gName(ci));	
					moveColumn(self,iStatement,self, ci, name);
				}
				else  //didn't find the column in the table definition
				{	//I have seen this happen when an update is done on a select that 
					//was for multiple tables.  I don't think that's legal, so we'll 
					//skip missing columns
		
					int x=0;  //this code is to aid in debugging
					char* missingColName=gName(cit);
					x++;
					
				}
				
			}
		}




		return retval;
	}
}



imeth	int	gEndSQL()
{      

	if (!iUsingCache)
		return gEndSQL(iStatement);
		
	resetStmt(self);	       
	
	return 0;
}

imeth	int	gNextRecord()
{


	UDWORD	crow;
	int	r=0;

	if (!iUsingCache)
		return gNextRecord(iStatement);


	LAZY_LOAD;
	if (iErrorState)
		return 1;
	if (iTType != TYPE_SELECT)
		gError(Object, "gNextRecord without Select");
	if (iPos >= iRecordsRead) {
	
		r = SQL_NO_DATA_FOUND;
	
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

	if (!iUsingCache)
		return gPrevRecord(iStatement);	

	
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
	if (!iUsingCache)
		return gFirstRecord(iStatement);	

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

	if (!iUsingCache)
		return gLastRecord(iStatement);
	
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
	if (!iUsingCache)
		return gRereadRecord(iStatement);

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

	if (!iUsingCache)
		return gSetAbsPos(iStatement,pos);
	
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
		r = SQL_NO_DATA_FOUND;
	}
	if (!r)
		updateOrgValues(self);
	return r;
}

imeth	int	gSetRelPos(long pos)
{
	if (!iUsingCache)
		return gSetRelPos(iStatement,pos);

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

imeth	int	gExecuteWithError  (object self, char *cmd, char *errbuf, int sz, long linenum)
{
	object	rval = pExecuteWithErrorObj(self, cmd, linenum);
	int	r = 0;

	if (rval) {
		object	sobj = gValue(rval);

		r = gIntKey(rval);
		if (gSize(sobj) >= sz)
			gTake(sobj, sz - 1);
		Strcpy(errbuf, gStringValue(sobj));
		gDeepDispose(rval);
	}
	return r;
}


imeth	int	gExecuteFile(char *file)
{
	//The safest thing to do for now is not use the cache and reset it
	gClearCache(iDatabase);
	resetStmt(self);
	return gExecuteFile(iStatement, file);
}

imeth	int	gExecuteFileWithError(char *file, char *errbuf, int sz)
{
	//The safest thing to do for now is not use the cache and reset it
	gClearCache(iDatabase);
	resetStmt(self);
	return gExecuteFileWithError(iStatement, file, errbuf, sz);
}

imeth	gExecuteFileReturnAllErrors(char *file)
{
	//The safest thing to do for now is not use the cache and reset it
	gClearCache(iDatabase);
	resetStmt(self);
	return gExecuteFileReturnAllErrors(iStatement, file);
}


imeth	gAttachDialog(dlg)
{
	if (!iUsingCache)
		return gAttachDialog(iStatement,dlg);
	else
	{

		object	ctls;  // A StringDictionary of controls
		object	seq, i, ctl, si=NULL;
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
			if (iCols)
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
}

imeth	gUnattachDialog(dlg)
{
	if (!iUsingCache)
		return 	gUnattachDialog(iStatement,dlg);
	else
		return pUnattachDialog(self, dlg, 1);
}

imeth	gAssociateCtl(char *cname, ctl)
{
	if (!iUsingCache)
		return gAssociateCtl(iStatement,cname,ctl);
	else
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

}

imeth	int	gDBMS_type()
{
	return gDBMS_type(iStatement);
}

imeth	int	gSelectColumns(char *tname, char *oname)
{
	return gSelectColumns(iStatement,tname,oname);
}

imeth	int	gSelectColumnsByName(char *tname, char *oname)
{
	return gSelectColumnsByName(iStatement,tname,oname);
}



imeth	int	gSelectTables(char *oname)
{
	return gSelectTables(iStatement,oname);
}

imeth	gColumns()
{
	if (!iUsingCache)
		return gColumns(iStatement);
	return iColList;
}

imeth	gTables()
{
	if (!iUsingCache)
		return gTables(iStatement);
	return iTableList;
}

imeth	gColumnDictionary()
{
	if (!iUsingCache)
		return gColumnDictionary(iStatement);
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
	if (!iUsingCache)
		return gReturnError(iStatement,err);
	else
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
	int	inquote = 0;
	
	char *t=getStringMemory(s);//Don't change s!
	Strcpy(t,s);
	s=t;

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
		return TranslateToOracle(t);

	return t;
}


imeth gGetStatement()
{
	if (!iUsingCache)
		return iStatement;
	else
		return self;
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

imeth	gCopyCorresponding(from)
{
	if (!iUsingCache)
		return gCopyCorresponding(iStatement,gGetStatement(from));
	else
	{
		ivType	*iv2;
		object	seq, node, si, si2;
		char	*cname;
		object fromCols;

//		ChkArgTyp(from, 2, CLASS);
		LAZY_LOAD;
		if (gDoLazyLoad(from))
			vError(self, "LazySelect Error");
//		iv2 = ivPtr(from);
//		if (!iv2->iCols)
//			return NULL;

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
}

ivmeth	int	vDBSelect, vExecuteWithBind (char *fmt, ...)
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

ivmeth	int	vDBSelectOne(char *fmt, ...)
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
	if (!iUsingCache)
		return gName(iStatement);

	return iTable;
}

imeth	gSetTag(tag)
{
	if (!iUsingCache)
		return gSetTag(iStatement,tag);
	else
	{
		object	ptag = iTag;
		iTag = tag;
		if (ptag  &&  iAutoDisposeTag)
			return gDeepDispose(ptag);
		else
			return ptag;
	}
}

imeth	gGetTag()
{
	if (!iUsingCache)
		return gGetTag(iStatement);

	return iTag;
}

imeth	int	gSelectPrimaryKeyFields(char *table, char *oname)
{
	return gSelectPrimaryKeyFields(iStatement,table,oname);
}

imeth	int	gSelectReferencedBy(char *table, char *oname)
{
	return gSelectReferencedBy(iStatement,table,oname);
}


imeth	int	gSelectForeignKeys(char *table, char *oname)
{
	return gSelectForeignKeys(iStatement,table,oname);
}



imeth	int	gSelectIndexes(char *table, char *oname)
{
	return gSelectIndexes(iStatement,table,oname);
}

imeth	int	gSelectTriggers(char *table, char type, char *oname)
{
	return gSelectTriggers(iStatement,table,type,oname);
}

imeth	gNow()
{
	if (!iUsingCache)
		return gNow(iStatement);

	gEndSQL(self);
	return gNow(iDatabase);
}

private imeth	char	*pFldGetVarText(object si, char *fld, char *vttbl)
{
	object	vtobj;

	gSetColVarText(si, NULL, vttbl);
	
	vtobj = gGetColVarText(si);
	return vtobj ? gStringValue(vtobj) : "";
}

private imeth	pFldSetVarText(object si, char *fld, char *str, char *vttbl)
{
	char	*cp = str ? str : "";

	iVarTextChanged = 1;
	
	gSetColVarText(si, cp, vttbl);
	return self;
}

imeth	int	gEnableTP(int flg)
{
	int	old = iDataSync;
	iDataSync = flg;
	return old;
}

private imeth	char	*pGetVarTextTable(object self, object si, char *fld)
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

imeth	gEnableWrites()
{

	return gEnableWrites(iStatement);
}

imeth	int	gIsReadOnly()
{
	return gIsReadOnly(iStatement);
}

imeth	gDisableWrites()
{
	return gDisableWrites(iStatement);
}

imeth	gLockEnableWrites()
{
	return gLockEnableWrites(iStatement);
}

imeth	int	gIgnoreAllErrors(int v)
{
	int	r = iIgnoreAllErrors;
	iIgnoreAllErrors = v;
	return gIgnoreAllErrors(iStatement,v);
}

imeth	int	gReturnAllErrors(int v)
{
	int	r = iReturnAllErrors;
	iReturnAllErrors = v;
	return gReturnAllErrors(iStatement,v);
}

imeth	char	*gLastSelect()
{
	if (!iUsingCache)
		return gLastSelect(iStatement);

	return iLastSelect ? gStringValue(iLastSelect) : NULL;
}

imeth	int	gGetCursorPosition()
{
	if (!iUsingCache)
		return gGetCursorPosition(iStatement);

	return iPos;
}

cmeth	int	gSaveLastSelect(int mode)
{
	int	pmode = cSaveLastSelect;
	cSaveLastSelect = mode;
	return gSaveLastSelect(Statement,mode);
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
			Strcpy(buf, gFldGetString(stmt, "index_name"));
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
		Strcpy(buf, gFldGetString(stmt, "index_name"));
		if (!(idxobj = gFindValueStr(rval, lcase(buf)))) {
			idxobj = gNew(StringDictionary);
			gAddStr(rval, buf, idxobj);
		}
		Strcpy(buf, gFldGetString(stmt, "column_expression"));
		len = strlen(buf);
		if (len > 1) {
			buf[strlen(buf) - 1] = '\0';
			p = buf + 1;
		} else
			p = buf;
		Strcpy(buf2, gFldGetString(stmt, "column_name"));
		gAddStr(idxobj, lcase(buf2), gNewWithStr(String, lcase(centerStrip(p))));
	}

	return rval;
}

imeth	int	gSQLStatistics(char *tname, char *oname)
{
	return gSQLStatistics(iStatement,tname,oname);
}

imeth	int	gSQLForeignKeys(char *tname, char *oname, int refby)
{
	return gSQLForeignKeys(iStatement,tname,oname,refby);
}

imeth	ifun	gSetRecordTestFunction(ifun fun)
{
	ifun	old = iRecordTestFunction;
	iRecordTestFunction = fun;
	return gSetRecordTestFunction(iStatement,fun);
}

imeth 	long gGetNativeErrorCode()
{
	return gGetNativeErrorCode(iStatement);
}

imeth	gSetSpecialAuditInfo(ifun fun, object dict)
{
	iSpecialFH_Tables = dict;
	iSpecialAuditHandler = fun;
	
	return self;
}




