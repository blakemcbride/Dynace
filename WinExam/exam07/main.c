

#include "generics.h"
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_dialog(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
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
	
	ctl = mAddControl(dlg, CheckBox, IDC_CHECK1);
	gSetShortValue(ctl, 1);

	ctl = mAddControl(dlg, RadioButton, IDC_RADIO1);
	ctl = mAddControl(dlg, RadioButton, IDC_RADIO2);
	gSetShortValue(ctl, 1);
}

static	void	displayValues(object wind, object dlg)
{
	int	ival;
	
	ival = mCtlShortValue(dlg, IDC_CHECK1);
	vPrintf(wind, "Check Box Checked = %s\n", ival ? "Yes" : "No");

	ival = mCtlShortValue(dlg, IDC_RADIO1);
	vPrintf(wind, "Radio Box 1 Checked = %s\n", ival ? "Yes" : "No");

	ival = mCtlShortValue(dlg, IDC_RADIO2);
	vPrintf(wind, "Radio Box 2 Checked = %s\n\n", ival ? "Yes" : "No");
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
