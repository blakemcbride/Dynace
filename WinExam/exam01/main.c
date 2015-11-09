
/*  This program is fully documented in the file main.txt   */

#include "generics.h"
#include "resource.h"

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*(int)sizeof(char *));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	vPrintf(win, "Hello, World!\n");

	return gProcessMessages(win);
}
