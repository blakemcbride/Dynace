


#include <windows.h>
#include <sql.h>
#include <sqlext.h>
#include "generics.h"
#include "resource.h"


static	long	add_record(object wind, unsigned id);
static	long	list_records(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);
static	long	create_file(object wind, unsigned id);
static	long	drop_file(object wind, unsigned id);
static	long	delete_one(object wind, unsigned id);
static	long	change_one(object wind, unsigned id);
static	long	dialog(object wind, unsigned id);
static	long	list_tables(object wind, unsigned id);

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
	mAssociate(win, ID_LIST_TABLES, list_tables);
	mAssociate(win, ID_REC_ADD, add_record);
	mAssociate(win, ID_REC_LIST, list_records);
	mAssociate(win, ID_FILE_EXIT, file_exit);
	mAssociate(win, ID_REC_DELONE, delete_one);
	mAssociate(win, ID_REC_CHANGE, change_one);
	mAssociate(win, ID_REC_DIALOG, dialog);

	
#if 0
	DB = gOpenDatabase(Database, "Dynace Example", "DBA", "SQL");
#else
	DB = gQueryDatabase(Database, NULL);
#endif
	if (!DB)
		gError(Application, "Can't open datbase");
	

	r = gProcessMessages(win);

//	gDispose(DB);

	return r;
}

static	long	create_file(object wind, unsigned id)
{
	gExecuteFile(DB, "phone");
	return 0L;
}

static	long	drop_file(object wind, unsigned id)
{
	gDropTable(DB, "phone");
	return 0L;
}

static	long	add_record(object wind, unsigned id)
{
	object	stmt;

	stmt =  gNewStatement(DB);

	gInsert(stmt, "phone");
	gFldSetString(stmt, "lname", "McBride");
	gFldSetString(stmt, "fname", "Blake");
	gFldSetShort(stmt, "age", 37);
	gFldSetString(stmt, "street", "3020 liberty");
	gAddRecord(stmt);

	gDispose(stmt);

	return 0L;
}

static	long	list_records(object wind, unsigned id)
{
	object	stmt;

	stmt =  gNewStatement(DB);
	gDBSelect(stmt, "select * from phone order by lname");
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

static	long	change_one(object wind, unsigned id)
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

static	long	delete_one(object wind, unsigned id)
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

static	long	dialog(object wind, unsigned id)
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

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}

static	long	list_tables(object wind, unsigned id)
{
	object	tbls, tseq, tbl, cols, cseq, col;

	tbls = gGetAllTables(DB);
	for (tseq = gSequence(tbls) ; tbl = gNext(tseq) ; ) {
		tbl = gValue(tbl);
		vPrintf(wind, "%s\n", gName(tbl));
		cols = gColumns(tbl);
		for (cseq = gSequence(cols) ; col = gNext(cseq) ; ) {
			col = gValue(col);
			vPrintf(wind, "         %s  %d  %d\n", gName(col), gType(col), gSize(col));
		}
	}

	return 0L;
}
