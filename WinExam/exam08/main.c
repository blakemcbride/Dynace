

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
	
	ctl = mAddControl(dlg, ListBox, IDC_LIST1);
	gAddOption(ctl, "One");
	gAddOption(ctl, "Two");
	gAddOption(ctl, "Three");
	gAddOption(ctl, "Four");
	gAddOption(ctl, "Five");
	gAddOption(ctl, "Six");
	gAddOption(ctl, "Seven");

	ctl = mAddControl(dlg, ComboBox, IDC_COMBO2);
	gAddOption(ctl, "Blue");
	gAddOption(ctl, "Green");
	gAddOption(ctl, "Red");
	gAddOption(ctl, "Black");
	gAddOption(ctl, "White");
	gAddOption(ctl, "Purple");
	gSetShortValue(ctl, 2);

	ctl = mAddControl(dlg, ComboBox, IDC_COMBO1);
	gAddOption(ctl, "First");
	gAddOption(ctl, "Second");
	gAddOption(ctl, "Third");
	gAddOption(ctl, "Fourth");
	gAddOption(ctl, "Fifth");
	gAddOption(ctl, "Sixth");
	gAddOption(ctl, "Seventh");
	gSetShortValue(ctl, 2);
}

static	void	displayValues(object wind, object dlg)
{
	int	ival;
	
	ival = mCtlShortValue(dlg, IDC_LIST1);
	vPrintf(wind, "List Box = %d\n", ival);

	ival = mCtlShortValue(dlg, IDC_COMBO2);
	vPrintf(wind, "Combo Box = %d\n", ival);

	ival = mCtlShortValue(dlg, IDC_COMBO1);
	vPrintf(wind, "Drop Down List Box = %d\n\n", ival);
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
