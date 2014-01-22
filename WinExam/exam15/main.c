

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
	if (-1 == gPuts(pntr, "\nGraphics Output!"))
		return;
	gSetScale(pntr, 1100, 850);
	if (!gEllipse(pntr, 100, 100, 200, 200))
		return;
	gUse(pntr, vNew(CustomPen, 0, 0, 0, PS_DASHDOT, 1));
	if (!gEllipse(pntr, 600, 100, 800, 500))
		return;
	gUse(pntr, vNew(CustomPen, 0, 0, 0, PS_SOLID, 10));
	gUse(pntr, vNew(HatchBrush, 0, 0, 0, HS_DIAGCROSS));
	if (!gRectangle(pntr, 400, 400, 500, 600))
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
