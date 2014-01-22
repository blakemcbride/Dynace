

#include "generics.h"
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

#define	ROW(x)		((x) * lh)
#define COL(x)		((x) * acw)
#define	WIDTH(x)	((x) * acw)
#define	NEXTROW(x)	x += lh

#define	START_WINDOW_DIALOG						\
	int	mode = gSetScalingMode(Application, SM_PIXELS);		\
	int	lh = line_height();					\
	int	acw = ave_char_width()

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_MESSAGE, file_message);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	gSetPosition(win, 0, 0);

	return gProcessMessages(win);
}

static	int	fun(object ctl, object wnd)
{
	object	tctl = gGetControlStr(wnd, "FName");

	vPrintf(wnd, "%s\n", gStringValue(tctl));
	gDisposeAllControls(wnd);
	return 0L;
}

static	int	line_height()
{
	object	font = gGetFont(Application);
	int	height = gLineHeight(font) + 6;
	gDispose(font);
	return height;
}

static	int	ave_char_width()
{
	object	font = gGetFont(Application);
	int	space = gAveCharWidth(font);
	gDispose(font);
	return space;
}

static	long	file_message(object wind, unsigned id)
{
	START_WINDOW_DIALOG;
	int	col, row;
	object	ctl;

//   The following line allows the user to move the controls on the window like
//   a resource editor
	gSetModifyChildren(wind, 1);

	gAddStaticControl(wind, ROW(1), COL(5), &col, NULL, "This is the first line of my script.  Please enter the following:");

	row = ROW(2);
	gAddStaticControl(wind, row, COL(5), &col, NULL, "First name");
	gAddTextControl(wind, row, col, WIDTH(10), &col, "FName");

	gAddStaticControl(wind, row, col, &col, NULL, ",            Last name");

	ctl = gAddTextControl(wind, row, col, WIDTH(20), &col, "LName");
//	gHide(ctl);

//	gAddCheckBox(wind, row, col, WIDTH(15), &col, "CheckBox", "A Checkbox");
	

	NEXTROW(row);
	gAddNumericControl(wind, row, COL(5), WIDTH(10), &col, "Number");
	gAddDateControl(wind, row, col, WIDTH(12), &col, "Date");
/*
	NEXTROW(row);
	gAddRadioButton(wind, row, COL(5), WIDTH(10), &col, "RB1", "RB1", "RB2");
	gAddRadioButton(wind, row, col, WIDTH(10), &col, "RB2", "RB2", "RB1");

	NEXTROW(row);
	ctl = gAddListBox(wind, row, col, ROW(5), COL(20), &col, "Listbox");
	gAddOption(ctl, "One");
	gAddOption(ctl, "Two");
	gAddOption(ctl, "Three");
*/
	gAddButton(wind, ROW(5), COL(10), WIDTH(10), NULL, fun, "Button", "OK");

	gPerform(wind);	

	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
