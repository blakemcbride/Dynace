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


extern	char	*Getenv(char *);





/* caching version of database, uses sqlite to store data
   activated by environment variable
*/

defclass  CachedDatabase : Database
{
	
		object tempFile;
	class:
			object	cTableDictionary;  //tables in database with cache state (CacheTable objects)
			int cInitFlag;			//flag for if database has been initialized
			sqlite3 *cDB;			//sqlite database
						
			object cTableGroupData;
			object iStatement;
			
};


typedef struct {  

	int id;

	object keyColumns;
	object savedPreCacheRequestList;


} GroupData;

private imeth int runPreCache(int group);
private imeth int addCacheTable(char *name,int group);


#define	VARTEXTMAX	255

void * getStringMemory(char *str)
{
	return malloc(2*(strlen(str)+1));
}


/* method to skip caching system and get normal statement
*/
imeth gNewNonCacheStatement()
{
	return gNewStatement(super self);
}


/* return statement, or cache statement depeding on environment setting
*/
imeth	gNewStatement()
{
	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	object stmt= gNewStatement(super self);
	
	if ((cDB==NULL)||!en||(strcmp(en,"Y")!=0)) // If I failed to open the cache cDB, just use normal statement
	{
		return stmt;
	}

	return gNewCacheStatement(CacheStatement, self, stmt, cDB);
	
	
}

private cmeth GroupData * getGroupData(int id)
{
	GroupData * groupData;
	object data;

	data=gFindInt(cTableGroupData,id);

	
	if (!data)
	{
		groupData=malloc(sizeof(GroupData));
		ZeroMemory(groupData,sizeof(GroupData));
		data=gNewWithPtr(Pointer,groupData);
		gAddInt(cTableGroupData,id,data);
		groupData->id=id;
		groupData->keyColumns=gNew(LinkList);
		groupData->savedPreCacheRequestList=gNew(LinkList);

	}
	else
	{
		groupData=gPointerValue(gValue(data));
	}

	return groupData;
}


/* opens and readies sqlite database
*/
private imeth prepDatabase()
{
	char *zErrMsg = 0;
	int rc;
	object table;
	object xml=NULL;
	object head;
	char * dbPath;

	

	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); //Does not need to be freed
	
	if (!en||(strcmp(en,"Y")!=0)) //check to see if I'm using cache
	{
		return NULL;

	}



	tempFile=gOpenTempFile(File);
	dbPath=gName(tempFile);

	rc = sqlite3_open(dbPath, &cDB); 

	if( rc ){  //if I can't make db file, forget it
 
		sqlite3_close(cDB);
		cDB=NULL;
		
		return NULL;
	}


	sqlite3_exec(cDB,"PRAGMA synchronous = OFF;",NULL,NULL,NULL);

	cTableDictionary = gNewWithInt(StringDictionary, 101);
	cTableGroupData = gNewWithInt(IntegerDictionary, 20);

	//load table information
	head=gParseFile(XMLNode,"cacheconfig.xml",NULL,NULL,NULL);

	if (head)  //dig down a level if I can
		head=gFirst(head);

	if (head)//dig down a level if I can
		xml=gFirst(head);

	while (xml)  //spin all nodes
	{
		if (gName(xml)&&!stricmp(gName(xml),"group"))  //handle group entries
		{
			char * idStr=gGetAttributeValue(xml,"id");
			if (idStr)
			{
				int id=atoi(idStr);

				object child;

				GroupData * groupData;

				if (id<0) //out of bounds
					continue; 
			
				groupData=getGroupData(self,id);

				child=gFirst(xml);

				while (child)
				{
					char * col=gGetAttributeValue(child,"name");

					if (col)
					{
						//I never free this because I'll need the info until shut down
						gAddLast(groupData->keyColumns,gNewWithObj(LinkValue, gNewWithStr(String,col)));

					}

					child=gNext(child);
				}
			}
			
		}
		if (gName(xml)&&!stricmp(gName(xml),"table"))  //handle table entries
		{
			char *tableName=gGetAttributeValue(xml,"tablename");
			char *query=gGetAttributeValue(xml,"query");
			char *groupstr=gGetAttributeValue(xml,"group");
			char *thresholdstr=gGetAttributeValue(xml,"threshold");
			char *cacheflagstr=gGetAttributeValue(xml,"cacheflag");

			if (tableName) //must have a table name to work with it
			{
				int group=0;
				int threshold=100;
				int cacheflag=0;
				char buf[512];


				if (!query) //build query if one not provided
				{
					//loadtable thing is so I can describe groups of tables with same where clause
					sprintf(buf,"SELECT DISTINCT loadTable.* FROM %s as loadTable",tableName);
					query=buf;

				}

				if (groupstr)  //get group if provided
					group=atoi(groupstr);
				if (thresholdstr)// get threshold if provided
					threshold=atoi(thresholdstr);
				if (cacheflagstr)//get flag if provided
					cacheflag=atoi(cacheflagstr);

				gAddStr(cTableDictionary, strupr(tableName), gNewCachedTable(CachedTable,self, cacheflag, threshold, strupr(tableName), query,group,cDB));
			}

		}

	
		xml=gNext(xml);  //next node
	}

	


	gDispose(head);

	//just in case I get them out of order, I'll spin for precache commands second

	runPreCache(self,-1);

	

	return NULL;
	

}

/* pre caches tables
	group - group id of tables to pre cache
*/
private imeth int runPreCache(int group)
{

	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	if (!en||(strcmp(en,"Y")!=0))   //don't do this if environment not set to cache
	{
		return 0;

	}
	else  //using cache, so proceed
	{

		object head=gParseFile(XMLNode,"cacheconfig.xml",NULL,NULL,NULL);
		object xml=NULL;

		if (head) //skip down to nodes I care about
			head=gFirst(head);

		if (head)
			xml=gFirst(head);

		if (head)
			xml=gFirst(head);

		while (xml) //spin all nodes at this level
		{

			if (gName(xml)&&!stricmp(gName(xml),"precache")) //handle precache requests
			{
				char *groupstr=gGetAttributeValue(xml,"group");  //It looks like I don't need to free this
				char *where=gGetAttributeValue(xml,"whereclause");

				//if required attributes are there, pre cache the table
				if (where&&groupstr&&((atoi(groupstr)==group)||(group=-1)))  
				{

					gPreCacheGroup(self,atoi(groupstr),where);
				}
			}

			xml=gNext(xml);
		}

		gDispose(head);

		return 0;

	}

}


/* shortcut adder to put tables in group 0
	name - table name
*/
private imeth int addStandardCacheTable(char *name)
{

	return addCacheTable(self,name,0);
}

/* adder to put table in a particular group
	name - table name
	group - id of table group
*/
private imeth int addCacheTable(char *name,int group)
{
	char buf[512];

	//make sure name is in upper case

	name=strupr(name);


	if (!gFindValueStr(cTableDictionary, name)) //make sure it's not already there
	{

		sprintf(buf,"select loadTable.* from %s as loadTable ",name);

		gAddStr(cTableDictionary, strupr(name), gNewCachedTable(CachedTable,self, 0, 100, name, buf,group,cDB));
	}

	return 0;
}


/* loads table(s) based on passed in where clause
	group - table group to cache
	queryPart - where clause of SQL select  
	*/
imeth int gPreCacheGroup(int group,char *queryPart)
{
	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	if (!en||(strcmp(en,"Y")!=0)) //abort if not caching
	{
		return 0;

	}
	else //caching, so proceed
	{
		object seq;
		object tbl;
		object tblval;
		GroupData * groupData;

		object stmt=gNewNonCacheStatement(self);
		
		//look through all tables defined
		for (seq=gSequence(cTableDictionary) ; tbl = gNext(seq) ; )
		{
			
			object tblval=gValue(tbl);

			if (gGetGroup(tblval)==group)  //if this table is in the group, do it
				gPreCache(tblval,stmt,cDB,queryPart);
		}

		
		groupData=getGroupData(self,group);


		//save this precache request in case a table gets defined later
		gAddLast(groupData->savedPreCacheRequestList,gNewWithObj(LinkValue, gNewWithStr(String,queryPart)));

		gDispose(stmt);

		return 0;
	}
}



/* clears a table group
	group - table group id
*/
imeth int gClearCacheGroup(int group)
{
	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	if (!en||(strcmp(en,"Y")!=0))  //abort if not caching
	{
		return 0;

	}
	else //caching, so continue
	{

		//wipe out my tables
		object seq;
		object tbl;
		object tblval;
		
		//check all tables
		for (seq=gSequence(cTableDictionary) ; tbl = gNext(seq) ; )
		{
			
			object tblval=gValue(tbl);

			if (gGetGroup(tblval)==group)  //if its in the group, do it
				gClearCache(tblval);

		}

		runPreCache(self,group);

		return 0;
	}
}

/* clears a table group
	group - table group id
*/
imeth int gClearCacheGroupQueries(int group)
{
	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	if (!en||(strcmp(en,"Y")!=0))  //abort if not caching
	{
		return 0;

	}
	else //caching, so continue
	{

		//wipe out my tables
		object seq;
		object tbl;
		object tblval;
		
		//check all tables
		for (seq=gSequence(cTableDictionary) ; tbl = gNext(seq) ; )
		{
			
			object tblval=gValue(tbl);

			if (gGetGroup(tblval)==group)  //if its in the group, do it
			{
				gDumpPrecacheQueries(tblval);
				gClearCache(tblval);
			}

		}

		return 0;
	}
}

/* clears the whole cache
*/
imeth int gClearCache()
{
	char	*en = Getenv("USE_INTEGRA_DATABASE_CACHE"); 
	
	if (!en||(strcmp(en,"Y")!=0)) //skip if not caching
	{
		return 0;

	}
	else // doing caching
	{

		//wipe out my tables
		object seq;
		object tbl;
		object tblval;
		
		//spin all tables
		for (seq=gSequence(cTableDictionary) ; tbl = gNext(seq) ; )
		{
			
			object tblval=gValue(tbl);
			gClearCache(tblval);
		}

		runPreCache(self,-1);

		return 0;
	}
}

/* returns a CacheTable object
	tableName - name of the table to retun
*/
imeth gGetCacheTable(char *tableName)
{

	//make sure table name is in upper case


	object ret= NULL;

	if (!tableName) //gotta give me something to look up
		return ret;

	tableName=strupr(tableName);


	ret=gFindValueStr(cTableDictionary, tableName);

	
	if (ret==NULL)// it's not in the cache already
	{
		//get the table definition
		object dbTable=gGetTable(self,(char *)lcase(tableName));


		if (IsObj(dbTable))  //do I have info on this table from main db?
		{
			object columns=gColumns(dbTable);
			object seq;
			object ptr;

			//loop through all my groups
			for (seq=gSequence(cTableGroupData) ; ptr = gNext(seq) ; )
			{
				GroupData * groupData=gPointerValue(gValue(ptr));

				char allThere=1;
				//if I have a group defined, see if the keys are in this table
				object seq;
				object str;

				for (seq=gSequence(groupData->keyColumns);str=gNext(seq);)
				{
					if (!gFindStr(columns,(char *)lcase(gStringValue(gValue(str)))))
						allThere=0;
				}

				//if keys are in the table, it's in the group
				if (allThere)
				{
					char buf[512];
					int precacheloop;
					object seq,str;

					sprintf(buf,"SELECT DISTINCT loadTable.* FROM %s as loadTable",tableName);

					ret=gNewCachedTable(CachedTable,self, 2, 100, tableName, buf,groupData->id,cDB);
				
					//tell it all the precache information for its group
					for (seq=gSequence(groupData->savedPreCacheRequestList);str=gNext(seq);)
						gPreCache(ret,gNewNonCacheStatement(self), cDB, gStringValue(gValue(str)));
					
					gAddStr(cTableDictionary, strupr(tableName), ret);

					break; //I'm done	
					
				}
				
			}

		}

		if (ret==NULL)
		{
			//Ok, lets try just adding it to load the whole table, if under 100 records
			ret=gNewCachedTable(CachedTable,self, 0, 100, tableName, NULL,0,cDB);
			gAddStr(cTableDictionary, strupr(tableName), ret);
		}
	}
	
	return ret;
}

/* opens database via parent and cache if not initialized
*/
cmeth	gOpenDatabase(char *source, char *id, char *pw)
{
	object tDB=gOpenDatabase(super self, source, id, pw);
	char	*en = Getenv("INTEGRA_DATABASE_CACHE_VALIDATE"); 

	if (!cInitFlag&&tDB)  //init cache db if needed
	{
		cInitFlag=1;	
		prepDatabase(tDB);
	}

	if (en&&(stricmp(en,"Y")==0)) 
	{
		gLogODBC(Statement,"sqllog.txt" );
	}


	if (tDB)
		iStatement=gNewStatement(tDB);

	return tDB;
}

/* opens database via parent and cache if not initialized
*/
cmeth	gQueryDatabase(wind)
{
	object tDB=gQueryDatabase(super self, wind);
	char	*en = Getenv("INTEGRA_DATABASE_CACHE_VALIDATE"); 

	if (!cInitFlag&&tDB)//init cache db if needed
	{
		cInitFlag=1;	
		prepDatabase(tDB);
	}

	if (en&&(stricmp(en,"Y")==0)) 
	{
		gLogODBC(Statement,"sqllog.txt" );
	}

	iStatement=gNewStatement(tDB);

	return tDB;

}


//destructor
imeth	object	gDispose, gDeepDispose ()
{
	sqlite3_close(cDB);  //put a close here to be sure file isn't locked open

	gDispose(tempFile);
	return NULL;
}


imeth	gGetVarText(char *tbl, long id)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	object	text = gNew(String);
	char	cmd[256];
	object stmt=gNewStatement(self);
	
	sprintf(cmd, "select * from %s where VarTextID = %ld order by VarTextCount", table, id);
	gDBSelectDNC(stmt, cmd);
	while (!gNextRecord(stmt))
		vBuild(text, NULL, gFldGetString(stmt, "VarText"), END);

	gDispose(stmt);

	return text;
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
	
	stmt = gNewStatement(self);

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

	gDispose(stmt);

	return id;
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
	
	stmt = gNewStatement(self);
	sprintf(cmd, "select VarTextCount from %s "
		"where VarTextID = %ld "
		"order by VarTextCount desc",
		table, id);
	if (!gDBSelectOneDNC(stmt, cmd))
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

	gDispose(stmt);

	return id;
}

imeth	gDeleteVarText(char *tbl, long id)
{
	char	*table = tbl && *tbl ? tbl : "VariableText";
	char	cmd[256];
	object stmt=gNewStatement(self);
	int	tp = gEnableTP(stmt, 0);

	sprintf(cmd, "delete from %s where VarTextID = %ld", table, id);
	gExecute(stmt, cmd);
	gEnableTP(stmt, tp);

	gDispose(stmt);

	return self;
}

imeth	long	gUpdateVarText(char *tbl, long id, char *text)
{
	if (id)
		gDeleteVarText(self, tbl, id);

	return gAppendVarText(self, tbl, id, text);
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

	if (!iStatement)
	{
		return gExecute(super self,cmd);
	}
	else
	{
		int	r = gExecute(iStatement, cmd);
		gEndSQL(iStatement);
		return r;
	}
}



imeth	gStatement()
{
	return iStatement;
}
