

/*  This program is fully documented in the file main.txt   */


#include "generics.h"
#include "resource.h"

static	long	file_dialog(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);


int	start()
{
	object	win;
	char	title[80];
	object	obj1, obj2;

	sprintf(title, "My Test Application - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_DIALOGS, file_dialog);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

static	long	file_dialog(object wind, unsigned id)
{
	gPerformDialogs(Dialogs, wind, 0);
	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}

