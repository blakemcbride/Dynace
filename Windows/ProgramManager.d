/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include "logfile.h"
#include "hdlcache.h"



defclass  ProgramManager {
	HWND	iDDEClient;
	HWND	iProgMan;
	HWND	iServer;
	char	iApplication[30];
	char	iTopic[30];
};

#include <dde.h>
#include <string.h>

static	char ClassName[] = "ProgmanDDEClient";

#define WM_DDECLIENT_EXECUTE  (WM_USER + 0)


static LRESULT CALLBACK DDEClientWndProc(HWND hWnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT		lResult = 0;
	BOOL		fCallDefProc = FALSE;
	ATOM		aApp, aTopic;
	MSG 		Msg;
	HC_VARS;

	if (wMsg == WM_NCCREATE) {
		HC_NEW(WINDOW_HANDLE_CACHE, hWnd, ((CREATESTRUCT *) lParam)->lpCreateParams);
		HC_UPDATE(WINDOW_HANDLE_CACHE, hWnd);
		iDDEClient = hWnd;
	} else
		HC_UPDATE(WINDOW_HANDLE_CACHE, hWnd);
	if (self)
		switch (wMsg) {
		case WM_CREATE:
			aApp = GlobalAddAtom(iApplication);
			aTopic = GlobalAddAtom(iTopic);
			SendMessage((HWND)-1, WM_DDE_INITIATE, (WPARAM) hWnd, MAKELPARAM(aApp, aTopic));

			if (iServer) {
				GlobalDeleteAtom(aApp);
				GlobalDeleteAtom(aTopic);
				break;
			}

			// A conversation was not able to be established. 
			// Attempt to execute the desired application.
			WinExec(iApplication, SW_RESTORE);

			SendMessage((HWND)-1, WM_DDE_INITIATE, (WPARAM)hWnd, MAKELPARAM(aApp, aTopic));

			if (!iServer)
				lResult = -1;

			GlobalDeleteAtom(aApp);
			GlobalDeleteAtom(aTopic);

			break;

		case WM_DESTROY:
			PostMessage(iServer, WM_DDE_TERMINATE, (WPARAM) hWnd, 0);

			// From now on, do not send a WM_DDE_ACK message to the 
			// server in response to any messages sent from the 
			// Server.
			HC_DELETE(WINDOW_HANDLE_CACHE, iDDEClient);
			break;

		case WM_DDE_DATA:
#ifdef	WIN32
			if (iServer != (HWND) wParam) {
				UINT_PTR	low, high;

				UnpackDDElParam(wMsg, lParam, &low, &high);
				// Conversation not initiated with this Server or
				// Server sent after we have terminated the 
				// conversation.
				if (high != 0) {
					// Data handle is not.  If it were NULL, a link 
					// was set using the WM_DDE_ADVISE message.
					GlobalFree((HANDLE)high);
				}
				GlobalDeleteAtom((ATOM)low);
			}
			FreeDDElParam(wMsg, lParam);
#else	     
			if (iServer != (HWND) wParam) {
				// Conversation not initiated with this Server or
				// Server sent after we have terminated the 
				// conversation.
				if (HIWORD(lParam) != 0) {
					// Data handle is not.  If it were NULL, a link 
					// was set using the WM_DDE_ADVISE message.
					GlobalFree(HIWORD(lParam));
				}
				GlobalDeleteAtom(LOWORD(lParam));
			}
#endif
			break;

		case WM_DDECLIENT_EXECUTE:
			// This message was sent to this window from the 
			// Setup Application.  The lParam parameter contains 
			// the handle of the memory containing the commands 
			// to be executed by the Server.

			// Verify that a conversation was started and 
			// hasn't been terminated.
			if (!iServer)
				break;

			PostMessage(iServer, WM_DDE_EXECUTE, (WPARAM)hWnd, lParam);

			// Wait for response from the Server.
			GetMessage(&Msg, hWnd, WM_DDE_ACK, WM_DDE_ACK);

			// Return whether the command was 
			// acknowledged successfully.
			wParam = LOWORD(Msg.lParam);
			lResult = ((DDEACK *) &wParam)->fAck;
			break;

		case WM_DDE_TERMINATE:
			if (!iServer)
				break;
			// The Server has terminated the conversation with us.
			// We must post the WM_DDE_TERMINATE message back to 
			// the server.
			PostMessage(iServer, WM_DDE_TERMINATE, (WPARAM)hWnd, 0);
			break;

		case WM_DDE_ACK:
			if (!iServer) {
				// No conversation initiated, WM_DDE_ACK must be from 
				// a potential server that just received my 
				// WM_DDE_INITIATE message.
				iServer = (HWND) wParam;
				break;
			}

			// WM_DDE_ACK message received from a potential Server 
			// but we have already established a conversation with 
			// another Server.  Tell the Server that we do not wish 
			// to continue our conversation with it. 
			PostMessage((HWND) wParam, WM_DDE_TERMINATE, (WPARAM)hWnd, 0);
			break;

		default:
			fCallDefProc = TRUE;
			break;
		}

	if (fCallDefProc)
		lResult = DefWindowProc(hWnd, wMsg, wParam, lParam);

	return(lResult);
}

cmeth	gNew()
{
	BOOL		r=FALSE;
	static	char	ProgMan[] = "PROGMAN";
	WNDCLASS	wc;
	HINSTANCE 	hInstance = gInstance(Application);
	object		obj = gNew(super);
	ivType		*iv = ivPtr(obj);
	
	wc.style = 0;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.lpfnWndProc = DDEClientWndProc;
	wc.hInstance = hInstance;
	wc.hIcon = 0;
	wc.hCursor = 0;
	wc.hbrBackground = 0;
	wc.lpszMenuName = NULL;
	wc.lpszClassName = ClassName;
	RegisterClass(&wc);

	strcpy(iApplication, ProgMan);
	strcpy(iTopic, ProgMan);
	iDDEClient = CreateWindow(ClassName, "", (DWORD) 0, 0, 0, 0, 0,
				  (HWND) 0, (HMENU) 0, hInstance, obj);

	if (iDDEClient == 0)
		return gDispose(obj);
	iProgMan = FindWindow(ProgMan, NULL);
	if (!IsWindow(iProgMan))  // Program Manager cannot be found
		return gDispose(obj);
	ShowWindow(iProgMan, SW_RESTORE);
	EnableWindow(iProgMan, FALSE);
	return obj;
}

imeth	gDispose, gDeepDispose, gGCDispose ()
{
	if (iDDEClient)
		DestroyWindow(iDDEClient);
	UnregisterClass(ClassName, gInstance(Application));
	if (iProgMan)
		EnableWindow(iProgMan, TRUE);
	return gDispose(super);
}

imeth	gAddGroup(char *group)
{
	char	cmd[256], *p;
	HGLOBAL	h;
	int	r;

	sprintf(cmd, "[CreateGroup(%s)]", group);
	h = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, strlen(cmd) + 1);
	p = GlobalLock(h);
	strcpy(p, cmd);
	GlobalUnlock(h);
#ifdef	WIN32
	r = (BOOL) SendMessage(iDDEClient, WM_DDECLIENT_EXECUTE, 0, (LPARAM) h);
#else
	r = (BOOL) SendMessage(iDDEClient, WM_DDECLIENT_EXECUTE, 0, MAKELONG(0, h));
#endif
	GlobalFree(h);
	return self;
}

imeth	gAddProgItem(char *path, char *title)
{
	char	cmd[256], *p;
	HGLOBAL	h;
	int	r;

	sprintf(cmd, "[AddItem(%s,%s)]", path, title);
	h = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, strlen(cmd) + 1);
	p = GlobalLock(h);
	strcpy(p, cmd);
	GlobalUnlock(h);
#ifdef	WIN32
	r = (BOOL) SendMessage(iDDEClient, WM_DDECLIENT_EXECUTE, 0, (LPARAM) h);
#else
	r = (BOOL) SendMessage(iDDEClient, WM_DDECLIENT_EXECUTE, 0, MAKELONG(0, h));
#endif
	GlobalFree(h);
	return self;
}









