

#include "generics.h"
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);
static	long	tool_file_new(object wind, unsigned bm);

#define	FIRST_SECTION	1

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

	gAddToolBitmap(win, IDB_NEW, 0, 1, tool_file_new, "New File");
	gAddToolBitmap(win, IDB_NEW, 0, 1, tool_file_new, "Save File");

	gAddSection(win, FIRST_SECTION, 40, DT_CENTER);
	gSetSectionText(win, FIRST_SECTION, "Text in section");


	return gProcessMessages(win);
}

static	long	tool_file_new(object wind, unsigned bm)
{
	gMessage(wind, "New bitmap hit");
	return 0L;
}

static	long	file_message(object wind, unsigned id)
{
	gMessage(wind, "File Message");
	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
