

#include "generics.h"
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_dialog(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*(int)sizeof(char *));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_MESSAGE, file_message);
	mAssociate(win, ID_FILE_DIALOG, file_dialog);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	long	file_message(object wind, unsigned id)
{
	gMessage(wind, "File Message");
	return 0L;
}

static	void	init_controls(object dlg)
{
	object	ctl;
	
	ctl = mAddControl(dlg, NumericControl, FIELD_AGE);
	gNumericRange(ctl, 1.0, 99.0, 0);

	ctl = mAddControl(dlg, DateControl, FIELD_BIRTH_DATE);
	gDateRange(ctl, 19000101L, 19951231L, 0);

	ctl = mAddControl(dlg, NumericControl, FIELD_MONEY);
	gNumericRange(ctl, 0.0, 1000000.0, 2);
}

static	void	displayValues(object wind, object dlg)
{
	int	ival;
	long	lval;
	double	dval;
	
	ival = mCtlShortValue(dlg, FIELD_AGE);
	vPrintf(wind, "Age = %d\n", ival);

	lval = mCtlLongValue(dlg, FIELD_BIRTH_DATE);
	vPrintf(wind, "Birth Date = %ld\n", lval);

	dval = mCtlDoubleValue(dlg, FIELD_MONEY);
	vPrintf(wind, "Money = %.2f\n\n", dval);
}

static	long	file_dialog(object wind, unsigned id)
{
	object	dlg;
	int	r;
	
	dlg = mNewDialog(ModalDialog, DL1, wind);

	init_controls(dlg);

	r = gPerform(dlg);

	if (r == TRUE)
		displayValues(wind, dlg);

	gDispose(dlg);

	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
