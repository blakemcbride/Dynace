    
/*   SERVER code  */

#include <windows.h>
#include <sql.h>
#include <sqlext.h>


#include "generics.h"
#include "resource.h"


#include "../idl/iblake.h"


#include <ole2ver.h>
#include <initguid.h>  //  only in one source file!


DEFINE_GUID(CLSID_Test, 0xccbe60a0,0x01a9,0x11d0, 0xbc,0x41,0x00,0x40,0x05,0x18,0x52,0x08);


object	mainWind;


static	long	file_exit(object wind);


int	start()
{
	object	win;
	char	title[80];
	int	r;
	object	server;

	sprintf(title, "Test OLE Server - %d", 8*sizeof(int));
	mainWind = win = vNew(MainWindow, title);
	gSetMaxLines(win, 400);
//	gSetStyle(win, WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL /* | WS_VISIBLE */);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	server = gNewComServer(ComServer, &CLSID_Test);

	gAddInterface(server, &IID_IBlake, IBlakeClass);
	
	gRegister(server);

	r = gProcessMessages(win);

	gDispose(server);

	return r;
}

static	long	file_exit(object wind)
{
	gQuitApplication(Application, 0);
	return 0L;
}
