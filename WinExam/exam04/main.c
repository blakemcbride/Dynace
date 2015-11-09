

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
	
	ctl = mAddControl(dlg, TextControl, FIELD_FIRST_NAME);
	gTextRange(ctl, 0, 10);

	ctl = mAddControl(dlg, TextControl, FIELD_LAST_NAME);
	gTextRange(ctl, 1, 30);
	gCapitalize(ctl);
}

static	void	displayValues(object wind, object dlg)
{
	char	*val;
	
	val = mCtlStringValue(dlg, FIELD_FIRST_NAME);
	vPrintf(wind, "First Name = %s\n", val);

	val = mCtlStringValue(dlg, FIELD_LAST_NAME);
	vPrintf(wind, "Last Name = %s\n\n", val);
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
