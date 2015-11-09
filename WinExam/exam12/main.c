

#include "generics.h"
#include "resource.h"

static	long	file_report(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*(int)sizeof(char *));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_REPORT, file_report);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	long	file_report(object wind, unsigned id)
{
	object	pntr;

	pntr = gQueryPrinter(Printer, wind, "My Test Output");
	if (!pntr)
		return 0L;

	if (-1 == gPuts(pntr, "This is line one of my output.\n"))
		goto end;

 end:
	gDispose(pntr);

	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
