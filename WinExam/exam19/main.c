

/*  This program is fully documented in the file main.txt   */


#include "generics.h"
#include "resource.h"

int	start()
{
	object	win;
	char	title[80];
	object	obj1, obj2;

	sprintf(title, "My Test Application - %d", 8*(int)sizeof(char *));
	win = vNew(MainWindow, title);

	mLoadIcon(win, ALGOCORP_ICON);

	obj1 = vNew(Class1, 45);
	obj2 = vNew(Class1, 36);

	vPrintf(win, "obj1's code = %d\n", gGetCode(obj1));
	vPrintf(win, "obj2's code = %d\n", gGetCode(obj2));

	gDispose(obj1);
	gDispose(obj2);

	return gProcessMessages(win);
}






