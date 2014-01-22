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
#include "sqlite3.h"


#ifndef	WIN32
#define SQL_WCHAR		(-8)
#define SQL_WVARCHAR	 	(-9)
#define SQL_WLONGVARCHAR 	(-10)
#define SQL_GUID		(-11)
#endif


/* this class stores the cache state of a table
	It will also do the load into the table from the 
	main database.
*/

defclass  CachedTable {
	object iDatabase;
	short iCached; //0 not iCached yet, 1 iCached, -1 never cash
	int iLoadThreshold;
	char * iTableName;
	char * iLoadQuery;
	int iCacheGroup;
	object iQueries;
	sqlite3 *iDB; 
	int iRefreshMinutes;
	object iNextLoadTime;
	int iOriginalCacheFlag;
class:
	FILE	*cLFP;
	
};
extern	char	*Getenv(char *);
extern void * getStringMemory(char *str);
private	imeth int createTableIfNeeded(sqlite3 *db);

#define AUTO_REFRESH_MINUTES 60

/* Constructor
	dyndb - the dynace database
	cache - inital value for cache table, so it can be marked never cache, etc.
	threshold - maximum rows to allow before aborting load
	name - the table name
	query - the load query
	group - precache group id, used to load groups of tables with the same key
*/
cvmeth	gNewCachedTable(object dyndb, short cache, int threshold, char *name, char *query, int group, void *sqllite)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);
	
	char	*en = Getenv("INTEGRA_DATABASE_CACHE_VALIDATE");
	
	iDatabase=dyndb;

	
	iLoadThreshold=threshold;
	iTableName=getStringMemory(name);
	Strcpy(iTableName,name);

	if (query) //Query is optional
	{
		iLoadQuery=getStringMemory(query);
		Strcpy(iLoadQuery,query);
	}

	iCacheGroup=group;

	iQueries=gNewWithInt(StringDictionary, 101); 

	iDB=sqllite;
	if (cache>0)
	{
		createTableIfNeeded(obj,iDB);
				

	}

	if (iCached!=-1)
		iCached=cache;//this must come after the table create

	if (en&&(stricmp(en,"Y")==0)) 
	{
		if (!cLFP)
			cLFP=fopen("CacheTableErrorLog.txt","w");
	}

	iRefreshMinutes=AUTO_REFRESH_MINUTES;

	iOriginalCacheFlag=iCached;

	iNextLoadTime=gNow(Time);
	gAddMinutes(iNextLoadTime,iRefreshMinutes);

	return obj;
}


//retuns the group this table is associated with
imeth int gGetGroup()
{
	return iCacheGroup;
}


//destructor
imeth	object	gGCDispose : GCDispose()
{

	gDeepDispose(iQueries);
	free(iTableName);
	free(iLoadQuery);
	
	return gDispose(super);
}

//destructor
imeth	object	gDispose, gDeepDispose ()
{
	GCDispose(self);
	return NULL;
}


/*load this table
	db - the sqllite database
	stmt - a statement on the main database
*/
private	imeth int runLoadQuery(sqlite3 *db, object stmt)
{

		object buf=gNewWithStr(String,"INSERT INTO ");
	
		object seq;
		int loop;
		object cols;
		int colCount=0;
		object si;
		sqlite3_stmt *sqlLiteStmt;
		int rc;
		int rowcount=0;

	
		gWrite(buf,iTableName,strlen(iTableName));
		gWrite(buf," (",2);
	
		gIgnoreAllErrors(stmt,1);

		if (gDBSelectNC(stmt, iLoadQuery))
		{
			//could not run load query for some reason
			if (cLFP)
				fprintf(cLFP,"WARNING Load Query could not execute - %s \n",iLoadQuery);

			iCached=-1;

			return iCached;
		}


		if ((gRecordCount(stmt)>iLoadThreshold)&&(iLoadThreshold!=-1))
		{
			iCached=-1;

			return iCached;
		}

		cols=gColumns(stmt); //LinkObject

		if (!cols)
		{
			//could not run load query for some reason
			if (cLFP)
				fprintf(cLFP,"WARNING Load Query could not execute - %s \n",iLoadQuery);

			iCached=-1;

			return iCached;
		}
		
		
		//spin the columns adding to the insert statement
		for (seq=gSequence(cols) ; si = gNext(seq) ; ) {

			if (colCount!=0) //conditionally add comma
			{
				gWrite(buf,",",1);
			}
			gWrite(buf,gName(si),strlen(gName(si)));
			gWrite(buf," ",1);
			colCount++;
		}
		
		gWrite(buf,") VALUES (",10);
		
		//add a ? for each column in value segment of query
		for (loop=0;loop<colCount;loop++)
		{
			if (loop)  //conditionally add comma
				gWrite(buf,",",1);
				
			gWrite(buf,"?",1);
			
		}
	
		gWrite(buf,")",1);	
		
		
		
		//if (cLFP)
				//fprintf(cLFP,"Built insert statement - %s \n",gStringValue(buf));
		
		rc= sqlite3_prepare(db,            /* iDatabase handle */
								gStringValue(buf),       /* SQL statement, UTF-8 encoded */
								-1,             /* Length of zSql in bytes.  -1 means to first terminator*/
								&sqlLiteStmt,  /* OUT: Statement handle */
								NULL    /* OUT: Pointer to unused portion of zSql */
								);


		gDispose(buf);


		if (rc)//Could not create the table for some reason
		{
			//set to 0 so it will try again
			iCached=0;
			return 1;
		}

		
		//process all loaded rows
		while (!gNextRecord(stmt))
		{

			int paramCount=0;

			//if this goes past the threshold, abort
			if ((iLoadThreshold>-1)&&rowcount++>iLoadThreshold)
			{
				iCached=-1;
				sqlite3_finalize(sqlLiteStmt);

				return iCached;

			}

			
			//spin each column setting value
			for (seq=gSequence(cols) ; si = gNext(seq) ; ) 
			{
		
				char * nam=gName(si);

				switch (gType(si))  //values are set by type
				{
					case SQL_WLONGVARCHAR:
					case SQL_LONGVARCHAR:
					case SQL_WCHAR:
					case SQL_WVARCHAR:
					case SQL_CHAR:
					case SQL_VARCHAR:
					{
						char * val=gFldGetStringNoStrip(stmt, nam);
						if (!stricmp(val,"NULL"))
							val=" ";
						sqlite3_bind_text(sqlLiteStmt,++paramCount,val,-1,SQLITE_TRANSIENT);
						break;
					}
					case SQL_BIT:
					case SQL_TINYINT:
					case SQL_SMALLINT:
						sqlite3_bind_int(sqlLiteStmt, ++paramCount, gFldGetShort(stmt, nam));
						break;
					case SQL_INTEGER:
						sqlite3_bind_int(sqlLiteStmt, ++paramCount, gFldGetLong(stmt, nam));
						break;
					case SQL_NUMERIC:
					case SQL_DECIMAL:
					case SQL_REAL:
					case SQL_FLOAT:
					case SQL_DOUBLE:
						sqlite3_bind_double(sqlLiteStmt, ++paramCount, gFldGetDouble(stmt, nam));
						break;
					case SQL_DATE:
						{
							object tempDate;
							long val=gFldGetLong(stmt, nam);
							object dt=gNewWithLong(Date,val);

							char * dat=gStringValue(tempDate=gFormatDate(dt,"%Y-%N-%D"));
							sqlite3_bind_text(sqlLiteStmt,++paramCount,dat,-1,SQLITE_TRANSIENT);
							gDispose(tempDate);
						}
						break;
					case SQL_TIME:
						{
							object tempTime;
							long val=gFldGetLong(stmt, nam);
							object dt=gNewWithLong(Time,val);

							char * dat=gStringValue(tempTime=gFormatDate(dt,"%G:%M:%S.%L"));
							sqlite3_bind_text(sqlLiteStmt,++paramCount,dat,-1,SQLITE_TRANSIENT);
							gDispose(dt);
							gDispose(tempTime);
						}
						break;
					case SQL_TIMESTAMP:	
						{

							char buffer[80];

							long val=0;
							long tval=0;
							object tempDate;
							
							
							object o=gFldGetDateTime(stmt, nam,&val,&tval);
							

							object dt=gNewWithLong(Date,val);

							char * dat=gStringValue(tempDate=gFormatDate(dt,"%Y-%N-%D"));  

							

							char * tdat="";

							gDispose(dt);
							
							if (tval)
							{
								object tempTime;
								object tdt=gNewWithLong(Time,tval);
								tdat=gStringValue(tempTime=gFormatTime(tdt,"%G:%M:%S.%L")); 
								gDispose(tempTime);
								gDispose(tdt);
							}
							sprintf(buffer,"%s %s",dat,tdat);

							gDispose(tempDate);
							

							sqlite3_bind_text(sqlLiteStmt,++paramCount,buffer,-1,SQLITE_TRANSIENT);
						}
						break;
						default:
								{
									if (cLFP)
										fprintf(cLFP,"ERROR - Unknown datatype encountered, can not cache %s:%s - datatype is %d \n",iTableName,nam,gType(si));
									
									sqlite3_finalize(sqlLiteStmt);
								
									iCached=-1;

									return iCached;
								}
				}
			
				
			}
			
			rc=sqlite3_step(sqlLiteStmt); //run the statement	
		
			
			sqlite3_reset(sqlLiteStmt);
			
			
		}

		sqlite3_finalize(sqlLiteStmt);
		iCached=1;


		if (!rowcount&&cLFP)
			fprintf(cLFP,"WARNING Load Query had no rows - %s \n",iLoadQuery);
				
		return 0;

}


/*
	create table in cache db, if not present
	db - sqlite database
*/
private	imeth int createTableIfNeeded(sqlite3 *db)
{
	object columns;
	object cit;
	object ctstmt=gNewWithStr(String,"CREATE TABLE ");
	short first=1;

	int rc=0;	
	object seq;
	char *zErrMsg = 0;
	object dbTable;
	object pk;

	iDB=db;

	gClearCache(self);
	
	gWrite(ctstmt,iTableName,strlen(iTableName));
	gWrite(ctstmt," ( ",3);
	
	
	//determine if table already in cache iDatabase
	
	//create table	

	dbTable=gGetTable(iDatabase,iTableName);

	if (IsObj(dbTable))  //do I have info on this table from main db?
		columns=gColumns(dbTable);
	else //if not in main db, can't cache it, bail out
	{
		iCached=-1;
		return 1;
	}

	pk = gGetPrimaryKey(iDatabase, iTableName);

	//get column definitions
	for (seq=gSequence(columns) ; IsObj(seq) && ( cit = gNext(seq)) ; )
	{
		char buf[1024];
		
		object ci=gValue(cit);
		
		char * name=gName(ci);	

		int nullable=gIsNullable(ci);

	
		
		if (!first) //add comma when needed
			gWrite(ctstmt,", ",2);

			
		first=0;
		
		//this iDatabase can't handle constraints anyway, so everything will allow nulls
		//inserts will be tried on the real database first anyway

		switch (gType(ci))
		{
			case SQL_WLONGVARCHAR:
			case SQL_LONGVARCHAR:
			case SQL_WCHAR:
			case SQL_WVARCHAR:
			case SQL_CHAR:
			case SQL_VARCHAR:
				sprintf(buf,"[%s] varchar (%d) %s ",gName(ci),gSize(ci),(nullable)?"NULL":"NOT NULL");
				break;
			case SQL_BIT:
			case SQL_TINYINT:
			case SQL_SMALLINT:
				sprintf(buf,"[%s] smallint %s ",gName(ci),(nullable)?"NULL":"NOT NULL");
				break;
			case SQL_INTEGER:
				sprintf(buf,"[%s] int %s ",gName(ci),(nullable)?"NULL":"NOT NULL");
				break;
			case SQL_REAL:
			case SQL_FLOAT:
			case SQL_DOUBLE:
			case SQL_NUMERIC:
				sprintf(buf,"[%s] float %s ",gName(ci),(nullable)?"NULL":"NOT NULL");
				break;
			case SQL_DATE:
			case SQL_TIME:
			case SQL_TIMESTAMP:
				sprintf(buf,"[%s] date %s ",gName(ci),(nullable)?"NULL":"NOT NULL");				
				break;
		}
		gWrite(ctstmt,buf,strlen(buf));
		
	}

	if (pk)
	{
		object kseq,fld;
		int first=1;

		gWrite(ctstmt,", PRIMARY KEY (",15);

		for (kseq=gSequence(pk) ; fld = gNext(kseq) ; ) 
		{
			char	*sf=gStringValue(fld);

			if (!first)
			{
				gWrite(ctstmt,", ",2);
			}
			first=0;

			gWrite(ctstmt,sf,strlen(sf));
		}
		gWrite(ctstmt,")",1);
	}
	gWrite(ctstmt," ) ",3);


//	MessageBox(NULL,gStringValue(ctstmt),gStringValue(ctstmt),MB_OK);
	//most of these params were optional	
	rc=sqlite3_exec(
		db,                     /* An open iDatabase */
		gStringValue(ctstmt),              /* SQL to be executed */
		NULL,             /* Callback function */
		NULL,                       /* 1st argument to callback function */
		NULL                 /* Error msg written here */
	);



	gDispose(ctstmt);
		
	return rc;
}

private imeth int doPreCache(char * queryPart, object stmt)
{
/*
	if (gFindValueStr(iQueries, queryPart)) //see if I have already done this one
	{
		return iCached; //I've done this already

	}
*/
	if (!iCached)  //any non zero is either iCached, or not to be iCached
	{
		if (createTableIfNeeded(self,iDB)) //try to make the table
		{
			iCached=-1;  //could not make it, bail out
			return iCached;
		}

	}
	
	if (!iLoadQuery)  //no load query was initally set
	{
		iLoadQuery=queryPart;  //query part must be an entire query
		if (!runLoadQuery(self,iDB,stmt)) //try to run it
			iCached=1;  


		iLoadQuery=NULL;
	}
	else  //query was initially set, try adding query part to it
	{
		char * oldlq=iLoadQuery;
		char * lq=malloc(strlen(iLoadQuery)+strlen(queryPart)+120);

		sprintf(lq,"%s             %s",iLoadQuery,queryPart);

		iLoadQuery=lq;

		runLoadQuery(self,iDB,stmt);  //try to load



		free(lq);

		iLoadQuery=oldlq;
	}
	
	//save the query part  if I'm asked to do same query part again, I'll just skip it and go on
	gAddStr(iQueries, queryPart,gNewWithStr(String,queryPart));  //not sure if I want to save something 
													//with it, so for now just setting value to key
	return iCached;

}


/* pre load the table based on some where clause
	stmt - main db statement to use
	sqllitedb - sqlite database
	querypart - where portion of select statement
*/
imeth	int	gPreCache : preCache (object stmt, void * sqllitedb, char * queryPart)
{

	int ret=0;

	if (iCached==-1)  //check if flagged for no caching
		return iCached;

	if (gFindValueStr(iQueries, queryPart)) //see if I have already done this one
	{
		return iCached; //I've done this already

	}
	
	iDB=sqllitedb;

	//if I have already cached this table, go ahead do this load
	//otherwise, id have to clear it and that would result in just as many queries
	if (iCached==1)
		doPreCache(self,queryPart, stmt);


	//save the query part  if I'm asked to do same query part again, I'll just skip it and go on
	gAddStr(iQueries, queryPart,gNewWithStr(String,queryPart));  //not sure if I want to save something 
													//with it, so for now just setting value to key
	return iCached;
}
 

/* loads the table if needed
	stmt - main db statement
	sqllitedb - sqlite database
*/
imeth	int	gMakeReady : makeReady (object stmt, void * sqllitedb)
{
	object seq,q;
	short didPreCache=0;
	long timeDif;

	//get current time
	object now=gNow(Time);

	//set last done time if not set
	if (!iNextLoadTime)
		iNextLoadTime=gNow(Time);
	

	//compare current time to last time
	timeDif=gDifference(now,iNextLoadTime);
	
	if (timeDif>0)	
	{	
		iCached=5; //dump the table
		gClearCache(self); //dump the table contents

		if (createTableIfNeeded(self,iDB)) //try to make the table
		{
			iCached=-1;  //could not make it, bail out
			return iCached;
		}

		iCached=iOriginalCacheFlag;//reset cache if older than target
		
		gDispose(iNextLoadTime);
		iNextLoadTime=gNow(Time);
		gAddMinutes(iNextLoadTime,iRefreshMinutes);

	}
	//set last done time
	gDispose(now);
	
	

	if (iCached==2)
	{
		//try any precache queries that may have accumulated
		for (seq=gSequence(iQueries) ; q = gNext(seq) ; )
		{
			doPreCache(self,gStringValue(gValue(q)),stmt);  

			if (iCached<1)  //failed abort out
				return iCached;

		}

	}

	if (iCached)  //any non zero is either iCached, or not to be iCached
		return iCached;


	if (!iLoadQuery||createTableIfNeeded(self,sqllitedb))
	{
		iCached=-1;
		return iCached;
	}


	//look to see if I have any queries to run from precache
	//if so, run the precache queries

	
	for (seq=gSequence(iQueries) ; q = gNext(seq) ; )
	{
		doPreCache(self,gStringValue(gValue(q)),stmt);  

		if (iCached<1)  //failed abort out
			return iCached;

		didPreCache=1;
	}


	//no precache queries, so run the normal load
	if (!didPreCache)
		runLoadQuery(self,sqllitedb,stmt);

	return iCached;
}


/* clear the cache on this table
*/
imeth	int	gClearCache()
{
	if (iCached>=0&&iCached!=2)  //If set to not cache, don't set back to cacheable
	{
		char cmd[1024];

		sprintf (cmd,"DROP TABLE %s",iTableName);

		//I need to delete everything from the table - best way is to drop the table
		sqlite3_exec(
			iDB,                     /* An open iDatabase */
			cmd,              /* SQL to be executed */
			NULL,             /* Callback function */
			NULL,                       /* 1st argument to callback function */
			NULL                 /* Error msg written here */
		);
	
		iCached=0;
	}
	return 0;
}
 

imeth void gDumpPrecacheQueries()
{
	gDeepDispose(iQueries);
	iQueries=gNewWithInt(StringDictionary, 101); 
}
