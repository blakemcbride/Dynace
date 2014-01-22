

#include "generics.h"
#include "resource.h"

static	long	file_dialog(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);
static	int	recalc(object actl, object dlg);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_DIALOG, file_dialog);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	void	init_controls(object dlg)
{
	object	ctl;

	ctl = mAddControl(dlg, NumericControl, IDC_VALUE1);
	gNumericRange(ctl, 0.0, 1000000.0, 2);
	gSetFunction(ctl, recalc);

	ctl = mAddControl(dlg, NumericControl, IDC_VALUE2);
	gNumericRange(ctl, 0.0, 1000000.0, 2);
	gSetFunction(ctl, recalc);

	ctl = mAddControl(dlg, NumericControl, IDC_TOTAL);
	gNumericRange(ctl, 0.0, 1000000.0, 2);
	gDisable(ctl);
}

static	void	displayValues(object wind, object dlg)
{
	double	dval;

	dval = mCtlDoubleValue(dlg, IDC_VALUE1);
	vPrintf(wind, "Value 1 = %.2f\n", dval);

	dval = mCtlDoubleValue(dlg, IDC_VALUE2);
	vPrintf(wind, "Value 2 = %.2f\n", dval);

	dval = mCtlDoubleValue(dlg, IDC_TOTAL);
	vPrintf(wind, "Total = %.2f\n\n", dval);
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

static	int	recalc(object actl, object dlg)
{
	object	ctl;
	double	tot;

	ctl = mGetControl(dlg, IDC_VALUE1);
	tot = gDoubleValue(ctl);

	ctl = mGetControl(dlg, IDC_VALUE2);
	tot += gDoubleValue(ctl);

	ctl = mGetControl(dlg, IDC_TOTAL);
	gSetDoubleValue(ctl, tot);

	return 0;
}
