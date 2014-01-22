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
#include "generics.h"
#include "resource.h"


static	long	add_record(object wind);
static	long	list_records(object wind);
static	long	file_exit(object wind);
static	long	create_file(object wind);
static	long	drop_file(object wind);
static	long	delete_one(object wind);
static	long	change_one(object wind);
static	long	dialog(object wind);

static	object	DB;

int	start()
{
	object	win;
	char	title[80];
	int	r;

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);
	gSetMaxLines(win, 200);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, ID_MAIN_MENU);
	mAssociate(win, ID_TAB_CREATE, create_file);
	mAssociate(win, ID_TAB_DELETE, drop_file);
	mAssociate(win, ID_REC_ADD, add_record);
	mAssociate(win, ID_REC_LIST, list_records);
	mAssociate(win, ID_FILE_EXIT, file_exit);
	mAssociate(win, ID_REC_DELONE, delete_one);
	mAssociate(win, ID_REC_CHANGE, change_one);
	mAssociate(win, ID_REC_DIALOG, dialog);

	
#if 0
 #if 0
	DB = gOpenDatabase(Database, "WSQL 4.0 Sample", "DBA", "SQL");
 #else
	DB = gOpenDatabase(Database, "Access Test", "DBA", "SQL");
 #endif
#else
	DB = gQueryDatabase(Database, NULL);
#endif
	if (!DB)
		gError(Application, "Can't open datbase");
	

	r = gProcessMessages(win);

	gDispose(DB);

	return r;
}

static	long	create_file(object wind)
{
	gExecuteFile(DB, "phone");
	return 0L;
}

static	long	drop_file(object wind)
{
	gDropTable(DB, "phone");
//	gDropTable(DB, "vendors");
	return 0L;
}

static	long	add_record(object wind)
{
	object	stmt;

	stmt =  gNewStatement(DB);

	gInsert(stmt, "phone");
	gFldSetString(stmt, "lname", "McBride");
	gFldSetString(stmt, "fname", "Blake");
	gFldSetShort(stmt, "age", 37);
	gFldSetString(stmt, "street", "3020 liberty");
//	gFldSetShort(stmt, "id", 57);
	gAddRecord(stmt);

	gDispose(stmt);

	return 0L;
}

#if 0

static	long	list_records(object wind)
{
	object	stmt;

	stmt =  gNewStatement(DB);
	gDBSelect(stmt, "select * from phone order by lname, fname");
	while (!gNextRecord(stmt))
		vPrintf(wind, "%s %s %d\n",
			gFldGetString(stmt, "lname"),
			gFldGetString(stmt, "fname"),
			gFldGetShort(stmt, "age"));
	vPrintf(wind, "\n");

	gDispose(stmt);

	return 0L;
}

#endif

static	long	list_records(object wind)
{
	object	stmt;

	stmt =  gNewStatement(DB);
	gDBSelect(stmt, "select lname, fname, street, age from phone order by lname");
	while (!gNextRecord(stmt))
		vPrintf(wind, "%s %s %s %d\n",
			gFldGetString(stmt, "lname"),
			gFldGetString(stmt, "fname"),
			gFldGetString(stmt, "street"),
			gFldGetShort(stmt, "age"));
	vPrintf(wind, "\n");

	gDispose(stmt);

	return 0L;
}

static	long	change_one(object wind)
{
	object	stmt;

	stmt =  gNewStatement(DB);

	gDBSelect(stmt, "select * from phone order by lname, fname");
	gFirstRecord(stmt);
	gFldSetString(stmt, "fname", "----");
	gUpdateRecord(stmt);

	gDispose(stmt);

	return 0L;
}

static	long	delete_one(object wind)
{
	object	stmt;

	stmt =  gNewStatement(DB);

	gDBSelect(stmt, "select * from phone order by lname, fname");
	gFirstRecord(stmt);
	gDeleteRecord(stmt);

	gDispose(stmt);

	return 0L;
}

static	void	initControls(object dlg, object stmt)
{
	object	ctl;

	ctl = mAddControlStr(dlg, TextControl, IDC_FNAME, "fname");

	ctl = mAddControlStr(dlg, TextControl, IDC_LNAME, "lname");

	ctl = mAddControlStr(dlg, NumericControl, IDC_AGE, "age");
	gNumericRange(ctl, 1.0, 99.0, 0);

	ctl = mAddControlStr(dlg, TextControl, IDC_STREET, "street");

	gAttachDialog(stmt, dlg);
}

static	long	dialog(object wind)
{
	object	stmt, dlg;
	int	r;

	stmt =  gNewStatement(DB);
	gInsert(stmt, "phone");

	dlg = mNewDialog(ModalDialog, IDD_DIALOG, wind);
	initControls(dlg, stmt);

	r = gPerform(dlg);

	if (r)
		if (gAddRecord(stmt))
			gMessage(wind, "Duplicate record");

	gDispose(stmt);
	gDispose(dlg);

	return 0L;
}

static	long	file_exit(object wind)
{
	gQuitApplication(Application, 0);
	return 0L;
}
