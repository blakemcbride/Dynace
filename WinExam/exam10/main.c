

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

static	int	push_button(object ctl, object dlg)
{
	gMessage(dlg, "Push Button Hit");
	return 0;
}

static	void	init_controls(object dlg)
{
	object	ctl;
	
	ctl = mAddControl(dlg, TextControl, FIELD_FIRST_NAME);
	gTextRange(ctl, 0, 10);

	ctl = mAddControl(dlg, TextControl, FIELD_LAST_NAME);
	gTextRange(ctl, 1, 30);
	gCapitalize(ctl);

	ctl = mAddControl(dlg, NumericControl, FIELD_AGE);
	gNumericRange(ctl, 1.0, 99.0, 0);

	ctl = mAddControl(dlg, DateControl, FIELD_BIRTH_DATE);
	gDateRange(ctl, 19000101L, 19951231L, 0);

	ctl = mAddControl(dlg, NumericControl, FIELD_MONEY);
	gNumericRange(ctl, 0.0, 1000000.0, 2);

	ctl = mAddControl(dlg, PushButton, IDC_BUTTON);
	gSetFunction(ctl, push_button);

	ctl = mAddControl(dlg, CheckBox, IDC_CHECK1);
	gSetShortValue(ctl, 1);

	ctl = mAddControl(dlg, RadioButton, IDC_RADIO1);
	ctl = mAddControl(dlg, RadioButton, IDC_RADIO2);
	gSetShortValue(ctl, 1);

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

	ctl = mAddControl(dlg, ScrollBar, IDC_SCROLLBAR1);
}

static	void	displayValues(object wind, object dlg)
{
	char	*pval;
	int	ival;
	long	lval;
	double	dval;
	
	pval = mCtlStringValue(dlg, FIELD_FIRST_NAME);
	vPrintf(wind, "First Name = %s\n", pval);

	pval = mCtlStringValue(dlg, FIELD_LAST_NAME);
	vPrintf(wind, "Last Name = %s\n", pval);

	ival = mCtlShortValue(dlg, FIELD_AGE);
	vPrintf(wind, "Age = %d\n", ival);

	lval = mCtlLongValue(dlg, FIELD_BIRTH_DATE);
	vPrintf(wind, "Birth Date = %ld\n", lval);

	dval = mCtlDoubleValue(dlg, FIELD_MONEY);
	vPrintf(wind, "Money = %.2f\n", dval);

	ival = mCtlShortValue(dlg, IDC_CHECK1);
	vPrintf(wind, "Check Box Checked = %s\n", ival ? "Yes" : "No");

	ival = mCtlShortValue(dlg, IDC_RADIO1);
	vPrintf(wind, "Radio Box 1 Checked = %s\n", ival ? "Yes" : "No");

	ival = mCtlShortValue(dlg, IDC_RADIO2);
	vPrintf(wind, "Radio Box 2 Checked = %s\n", ival ? "Yes" : "No");

	ival = mCtlShortValue(dlg, IDC_LIST1);
	vPrintf(wind, "List Box = %d\n", ival);

	ival = mCtlShortValue(dlg, IDC_COMBO2);
	vPrintf(wind, "Combo Box = %d\n", ival);

	ival = mCtlShortValue(dlg, IDC_COMBO1);
	vPrintf(wind, "Drop Down List Box = %d\n", ival);

	ival = mCtlShortValue(dlg, IDC_SCROLLBAR1);
	vPrintf(wind, "Scroll Bar = %d\n\n", ival);
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
