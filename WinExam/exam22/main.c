

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

	pntr = gOpenDefault(Printer, wind, "My Test Output");
	if (!pntr)
		return 0L;
	gLoadFont(pntr, "Times New Roman", 10);

	//  Each line of test.pc contains row, col, data drop name
	//  Coordinates may be found by printing a grid
	gUseForm(pntr, "form.wmf", "test.pc");

//	The following line will cause a grid to be printed
//	gGrid(pntr);


#if 0   //  print the data drop names
	gPrintDataDrops(pntr);
#else  //  print the real data

	//  Associate data to data drop locations
	if (!gDropData(pntr, "Nm", "Blake McBride"))
		goto end;
	if (!gDropData(pntr, "Addrs", "3020 Liberty Hills Drive"))
		goto end;
#endif

 end:
	gDispose(pntr);
	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
