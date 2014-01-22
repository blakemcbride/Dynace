



/*
 *
 *	Copyright (c) 1996 Algorithms Corporation
 *	3020 Liberty Hills Drive
 *	Franklin, TN  37067
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */




/*  CLIENT code  */

#include <windows.h>
#include <sql.h>
#include <sqlext.h>


#include "generics.h"
#include "resource.h"


static	long	file_exit(object wind);
static	long	create_object(object wind);


int	start()
{
	object	win;
	char	title[80];
	int	r;

	sprintf(title, "Test OLE Client - %d", 8*sizeof(int));
	win = vNew(MainWindow, title);
	gSetMaxLines(win, 400);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_CREATE, create_object);

	r = gProcessMessages(win);

	return r;
}

static	long	create_object(object wind)
{
	object	obj;

	obj = gNew(TestClient);
	if (obj)
		gPuts(wind, "Object creation succeeded\n");
	else {
		gPuts(wind, "Object creation failed\n");
		return 0L;
	}

	gSetValues(obj, "Hello there", 38);

	vPrintf(wind, "Return = \"%s\"\n", gGetString(obj));

//	gDispose(obj);
	
	return 0L;
}

static	long	file_exit(object wind)
{
	gQuitApplication(Application, 0);
	return 0L;
}




/*
 *
 *	Copyright (c) 1996 Algorithms Corporation
 *	3020 Liberty Hills Drive
 *	Franklin, TN  37067
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */


