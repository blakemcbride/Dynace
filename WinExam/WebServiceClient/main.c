
#include "generics.h"
#include "resource.h"

static	long	file_message(object wind, unsigned id);
static	long	file_exit(object wind, unsigned id);

int	start()
{
	object	win;
	char	title[80];

	sprintf(title, "My Test Application - %d", 8*(int)sizeof(char *));
	win = vNew(MainWindow, title);
	gSetMaxLines(win, 400);

	mLoadIcon(win, ALGOCORP_ICON);

	mLoadMenu(win, IDR_MENU1);
	mAssociate(win, ID_FILE_MESSAGE, file_message);
	mAssociate(win, ID_FILE_EXIT, file_exit);

	return gProcessMessages(win);
}

#include <sys/types.h>
#include <sys/stat.h>

static	long	file_message(object wind, unsigned id)
{
	object	ws, dom, file, dom2;
	DWORD	ret;
	char	method[] = "login";
	char	path[80];
#if 1
	ws = gNewServiceRequest(WebService, "http://192.168.202.116:8454/destiny/soap/com.integra.destinylos.dataHandler.URLA1Data.iws?wsdl");
	gSetNamespaceURL(ws, "http://integra-online.com/soap/");
	ret = gSendRequest(ws, 0);
	vPrintf(wind, "ret = %d\n%s\n", ret, gGetErrorMessage(ws));
	dom = gGetXMLResponse(ws);
	gPrint(dom, wind);
	gPuts(wind, "\n");
#else
	ws = gNewServiceRequest(WebService, "http://192.168.202.114:9080/destiny/SecurityService.iws");
	gSetNamespaceURL(ws, "http://integra-online.com/soap/");
	gSetMethod(ws, method);
	gAddArgument(ws, "username", "supervisor");
	gAddArgument(ws, "password", "integra");
	gAddArgument(ws, "language", "en");

	ret = gSendRequest(ws, 0);
	vPrintf(wind, "ret = %d\n%s\n", ret, gGetErrorMessage(ws));

	vPrintf(wind, "Set-Cookie = %s\n", gGetResponseHeader(ws, "Set-Cookie"));

	dom = gGetXMLResponse(ws);
	gPrint(dom, wind);
	gPuts(wind, "\n");

	file = gOpenFile(File, "output.xml", "w");
	gPrint(dom, file);
	gPuts(file, "\n\n");

	sprintf(path, "/env:Envelope/env:Body/iss:%sResponse/%sResult", method, method);
	dom2 = gXPath(dom, path);
	if (dom2) {
		gPrint(gFirst(dom2), file);
		gPuts(file, "\n\n");
	}
	gDispose(file);

	gDispose(dom);


	gSetMethod(ws, "getCurrentUser");

	ret = gSendRequest(ws, 0);
	vPrintf(wind, "ret = %d\n%s\n", ret, gGetErrorMessage(ws));

	vPrintf(wind, "Set-Cookie = %s\n", gGetResponseHeader(ws, "Set-Cookie"));

	dom = gGetXMLResponse(ws);
	gPrint(dom, wind);
	gPuts(wind, "\n");
#endif



	gDispose(ws);

	return 0L;
}

static	long	file_exit(object wind, unsigned id)
{
	gQuitApplication(Application, 0);
	return 0L;
}
