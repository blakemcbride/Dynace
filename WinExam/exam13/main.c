

#include "generics.h"
#include "resource.h"

static	long	file_report(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_REPORT, file_report);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	void	report_output(object pntr)
{
	if (!gTextOut(pntr, 0, 0, "Line 0"))
		return;
	if (!gTextOut(pntr, 1, 0, "Line 1"))
		return;
	if (!gTextOut(pntr, 2, 0, "Line 2"))
		return;
	if (!gTextOut(pntr, 64, 40, "Line 64"))
		return;
	if (!gTextOut(pntr, 66, 40, "Line 66"))
		return;
	if (!gTextOut(pntr, 65, 40, "Line 65"))
		return;
	if (-1 == gPuts(pntr, "\n\n\n\n\n\nThis is a gPuts line of output.\n"))
		return;
	if (-1 == vPrintf(pntr, "\nColby is %d years old.\n", 8))
		return;

	if (!gNewPage(pntr))
		return;

	if (!gTextOut(pntr, 33, 40, "Second Page of Output!"))
		return;
}

static	long	file_report(object wind, unsigned id)
{
	object	pntr;

	pntr = gOpenDefault(Printer, wind, "My Test Output");
	if (!pntr)
		return 0L;

	report_output(pntr);

	gDispose(pntr);

	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
