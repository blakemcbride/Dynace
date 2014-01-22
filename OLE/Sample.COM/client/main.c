

/*  CLIENT code  */

#include <windows.h>
#include <sql.h>
#include <sqlext.h>


#include "generics.h"
#include "resource.h"


static	long	file_exit(object wind);


static	object	iblake;

static	long	create_object(object wind)
{
	iblake = gNew(IBlakeClass);
	if (iblake)
		gPuts(wind, "Object creation succeeded\n");
	else
		gPuts(wind, "Object creation failed\n");
	return 0L;
}

static	long	loop_object(object wind)
{
	char	buf[100];
	short	n;
	
	memset(buf, 0, 10);
	iSetValue(iblake, (short) 6, "Hello");
	iGetValue(iblake, (short) 90, buf, &n);
	vPrintf(wind, "value = \"%s\" (%d)\n", buf, (int) n);
	return 0L;
}

static	long	destroy_object(object wind)
{
	gDispose(iblake);
	gPuts(wind, "Object distroyed\n");
	return 0L;
}

object	mainWind;

int	start()
{
	object	win;
	char	title[80];
	int	r;

	sprintf(title, "Test OLE Client - %d", 8*sizeof(int));
	mainWind = win = vNew(MainWindow, title);
	gSetMaxLines(win, 400);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_CREATE, create_object);
	mAssociate(win, ID_LOOP, loop_object);
	mAssociate(win, ID_DESTROY, destroy_object);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	r = gProcessMessages(win);

	return r;
}

static	long	file_exit(object wind)
{
	gQuitApplication(Application, 0);
	return 0L;
}
